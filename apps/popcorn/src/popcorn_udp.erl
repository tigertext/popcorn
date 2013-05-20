-module(popcorn_udp).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(to_int(Value), list_to_integer(binary_to_list(Value))).

-include("include/popcorn.hrl").

-include_lib("popcorn_proto/include/popcorn_pb.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3, get_tags/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-record(state, {socket            :: pid(),
                known_nodes       :: list(),
                workers = []      :: list(),
                retention_policy  :: list()}).

init(_) ->
    {ok, Udp_Listen_Port} = application:get_env(popcorn, udp_listen_port),
    {ok, Socket} = gen_udp:open(Udp_Listen_Port, [binary, {active, once}, {recbuf, 524288}]),

    pubsub:subscribe(storage, ?MODULE, self()),

    {ok, Retention_Policy} = application:get_env(popcorn, log_retention),

    {ok, #state{socket            = Socket,
                retention_policy  = lists:map(fun({Severity, Retention_Interval}) ->
                                        {popcorn_util:severity_to_number(Severity), popcorn_util:retention_time_to_microsec(Retention_Interval)}
                                      end, Retention_Policy),
                known_nodes       = []}}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({broadcast, {new_storage_workers, Workers}}, State) ->
    ?POPCORN_INFO_MSG("~p accepted new list of workers ~p", [?MODULE, Workers]),
    {noreply, State#state{workers = Workers}};

handle_info({udp, Socket, _Host, _Port, Bin}, #state{workers = Workers} = State) ->
    ?RPS_INCREMENT(udp_received),
    {Popcorn_Node, Log_Message} = decode_protobuffs_message(State#state.retention_policy, Bin),
    New_State = ingest_packet(State, Popcorn_Node, Log_Message, Workers),
    inet:setopts(Socket, [{active, once}]),
    {noreply, New_State};

handle_info(timeout, State) ->
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_tags(Message) ->
    {lists:filter(fun(Identity) ->
         string:substr(Identity, 1, 5) =/= "Port<"
       end, tokenize(Message, "#")),
     tokenize(Message, "@")}.

tokenize(Message, Character) ->
    [string:substr(Word, 2, length(Word) -1) || Word <- string:tokens(Message, " ,;-"), string:substr(Word, 1, 1) =:= Character].

-spec decode_protobuffs_message(list(), binary()) -> {#popcorn_node{}, #log_message{}} | error.
decode_protobuffs_message(Retention_Policy, Encoded_Message) ->

    LogMessageProto = popcorn_pb:decode_log_message_proto(Encoded_Message),
    
    io:fwrite("~p ~n", [LogMessageProto]),

    %% I'm pretty sure the message is a string on the other side. 
    {Topics, Identities} = get_tags(LogMessageProto#log_message_proto.message),

    Popcorn_Node = #popcorn_node{node_name = check_undefined(list_to_binary(LogMessageProto#log_message_proto.node)),
                                 role      = check_undefined(list_to_binary(LogMessageProto#log_message_proto.node_role)),
                                 version   = check_undefined(list_to_binary(LogMessageProto#log_message_proto.node_version))},

    Popcorn_Severity = case check_undefined(LogMessageProto#log_message_proto.severity) of
                          undefined -> popcorn_util:severity_to_number(none);
                          _         -> LogMessageProto#log_message_proto.severity
                       end,

    Time_To_Expire = case proplists:get_value(Popcorn_Severity, Retention_Policy) of
                         undefined -> 7200000000;
                         TTL       -> TTL
                     end,

    Log_Message  = #log_message{message_id   = ?PU:unique_id(),
                                timestamp    = ?NOW,     %% this should be part of the protobuffs packet?
                                expire_at    = ?NOW + Time_To_Expire,
                                severity     = Popcorn_Severity,
                                message      = check_undefined(LogMessageProto#log_message_proto.message),
                                topics       = Topics,
                                identities   = Identities,
                                log_nodename = list_to_binary(Popcorn_Node#popcorn_node.node_name),
                                log_product  = list_to_binary(Popcorn_Node#popcorn_node.role),
                                log_version  = Popcorn_Node#popcorn_node.version,
                                log_module   = check_undefined(list_to_binary(LogMessageProto#log_message_proto.module)),
                                log_function = check_undefined(list_to_binary(LogMessageProto#log_message_proto.function)),
                                log_line     = check_undefined(list_to_binary(LogMessageProto#log_message_proto.line)),
                                log_pid      = check_undefined(list_to_binary(LogMessageProto#log_message_proto.pid))},


    {Popcorn_Node, Log_Message}.


check_undefined(<<>>) -> undefined;
check_undefined(Value) -> Value.

%% If no module/line numbers are passed generate one on the fly
location_check(<<>>, _, Message) ->
    Message2 = re:replace(Message, "\"[^\"]*\"", "", [{return,binary}, global]),
    Message3 = re:replace(Message2, "'[^']*'", "", [{return,binary}, global]),
    Message4 = re:replace(Message3, "<[^>]*>'", "", [{return,binary}, global]),
    Message5 = re:replace(Message4, "[0-9]*", "", [{return,binary}, global]),
    Alt_Module = list_to_binary(popcorn_util:hexstring(erlang:md5(Message5))),
    {Alt_Module, <<"0">>};
location_check(Module, Line, _) -> {Module, Line}.

-spec ingest_packet(#state{}, #popcorn_node{}, #log_message{}, list()) -> #state{}.  %% return value is whether is a new node
ingest_packet(#state{known_nodes = Known_Nodes} = State, #popcorn_node{node_name = Node_Name} = Popcorn_Node, Log_Message, Workers) ->
    %% create the node fsm, if necessary
    Node_Added =
      case lists:member(Node_Name, Known_Nodes) of
          false ->
              %% suspect this is a new node, but check the database just to be safe
              case gen_server:call(?CACHED_STORAGE_PID(Workers), {is_known_node, Node_Name}) of
                  false ->
                    {ok, Pid} = node_sup:add_child(Node_Name),
                    ok = gen_fsm:sync_send_event(Pid, {set_popcorn_node, Popcorn_Node}),
                    ets:insert(current_nodes, {Node_Name, Pid}),
                    gen_server:cast(?CACHED_STORAGE_PID(Workers), {add_node, Popcorn_Node}),
                    true;
                  _ ->
                    false
              end,
              true;  %% return true so that this node is added to the known_nodes state variable
          true  -> false
      end,

    %% let the fsm create the log and do the full ingest
    case ets:lookup(current_nodes, Node_Name) of
        [] ->
            ?POPCORN_WARN_MSG("unable to find fsm for node ~p", [Node_Name]),
            ok;
        [{_, Running_Pid}] ->
            gen_fsm:send_event(Running_Pid, {log_message, Popcorn_Node, Log_Message, Node_Added})
    end,

    case Node_Added of
        false ->
            State;
        true ->
            State#state{known_nodes = lists:append(Known_Nodes, [Node_Name])}
    end.
