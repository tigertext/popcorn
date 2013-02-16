-module(popcorn_udp).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(to_int(Value), list_to_integer(binary_to_list(Value))).

-include("include/popcorn.hrl").

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
    {tokenize(Message, "#"), tokenize(Message, "@")}.

tokenize(Message, Character) ->
    [string:substr(Word, 2, length(Word) -1) || Word <- string:tokens(Message, " ,;-"), string:substr(Word, 1, 1) =:= Character].

-spec decode_protobuffs_message(list(), binary()) -> {#popcorn_node{}, #log_message{}} | error.
decode_protobuffs_message(Retention_Policy, Encoded_Message) ->
    {{1, Packet_Version},  Rest} = protobuffs:decode(Encoded_Message, bytes),

    case Packet_Version of
        1 -> 
            decode_protobuffs_message(Retention_Policy, 1, Rest);
        No_Version when is_binary(No_Version) ->
            decode_protobuffs_message(Retention_Policy, 0, Encoded_Message);
        Other ->
            ?POPCORN_ERROR_MSG("#unknown packet #version: ~p", [Other]),
            error
    end.

decode_protobuffs_message(Retention_Policy, 0, Encoded_Message) ->
    {{1, Node},           Rest1} = protobuffs:decode(Encoded_Message, bytes),
    {{2, Node_Role},      Rest2} = protobuffs:decode(Rest1, bytes),
    {{3, Node_Version},   Rest3} = protobuffs:decode(Rest2, bytes),
    {{4, Severity},       Rest4} = protobuffs:decode(Rest3, bytes),
    {{5, Message},        Rest5} = protobuffs:decode(Rest4, bytes),
    {{6, Module},         Rest6} = protobuffs:decode(Rest5, bytes),
    {{7, Function},       Rest7} = protobuffs:decode(Rest6, bytes),
    {{8, Line},           Rest8} = protobuffs:decode(Rest7, bytes),
    {{9, Pid},            <<>>}  = protobuffs:decode(Rest8, bytes),

    {Topics, Identities} = get_tags(binary_to_list(Message)),

    Popcorn_Node = #popcorn_node{node_name = check_undefined(Node),
                                 role      = check_undefined(Node_Role),
                                 version   = check_undefined(Node_Version)},

    Popcorn_Severity = case check_undefined(Severity) of
                          undefined -> popcorn_util:severity_to_num(none);
                          _         -> Severity
                       end,

    Time_To_Expire = case proplists:get_value(Popcorn_Severity, Retention_Policy) of
                         undefined -> 7200000000;
                         TTL       -> TTL
                     end,

    Log_Message  = #log_message{message_id   = ?PU:unique_id(),
                                timestamp    = ?NOW,     %% this should be part of the protobuffs packet?
                                expire_at    = ?NOW + Time_To_Expire,
                                severity     = Popcorn_Severity,
                                message      = check_undefined(Message),
                                topics       = Topics,
                                identities   = Identities,
                                log_nodename = Popcorn_Node#popcorn_node.node_name,
                                log_product  = Popcorn_Node#popcorn_node.role,
                                log_version  = Popcorn_Node#popcorn_node.version,
                                log_module   = check_undefined(Module),
                                log_function = check_undefined(Function),
                                log_line     = check_undefined(Line),
                                log_pid      = check_undefined(Pid)},

    {Popcorn_Node, Log_Message};

decode_protobuffs_message(Retention_Policy, 1, Rest) ->
    {{2, Node},           Rest1} = protobuffs:decode(Rest, bytes),
    {{3, Node_Role},      Rest2} = protobuffs:decode(Rest1, bytes),
    {{4, Node_Version},   Rest3} = protobuffs:decode(Rest2, bytes),
    {{5, Severity},       Rest4} = protobuffs:decode(Rest3, bytes),
    {{6, Message},        Rest5} = protobuffs:decode(Rest4, bytes),
    {{7, Module},         Rest6} = protobuffs:decode(Rest5, bytes),
    {{8, Function},       Rest7} = protobuffs:decode(Rest6, bytes),
    {{9, Line},           Rest8} = protobuffs:decode(Rest7, bytes),
    {{10, Pid},           <<>>}  = protobuffs:decode(Rest8, bytes),

    {Topics, Identities} = get_tags(binary_to_list(Message)),

    Popcorn_Severity = case check_undefined(Severity) of
                           undefined -> popcorn_util:severity_to_number(none);
                           _         -> Severity
                       end,

    Time_To_Expire = case proplists:get_value(Popcorn_Severity, Retention_Policy) of
                         undefined -> 7200000000;
                         TTL       -> TTL
                     end,

    Popcorn_Node = #popcorn_node{node_name = check_undefined(Node),
                                 role      = check_undefined(Node_Role),
                                 version   = check_undefined(Node_Version)},

    %% Ensure we have a module and line so we can perform rollup
    {Module2, Line2} = location_check(Module, Line, Message),

    Log_Message  = #log_message{message_id   = ?PU:unique_id(),
                                timestamp    = ?NOW,     %% this should be part of the protobuffs packet?
                                expire_at    = ?NOW + Time_To_Expire,
                                severity     = Popcorn_Severity,
                                message      = check_undefined(Message),
                                topics       = Topics,
                                identities   = Identities,
                                log_nodename = Popcorn_Node#popcorn_node.node_name,
                                log_product  = Popcorn_Node#popcorn_node.role,
                                log_version  = Popcorn_Node#popcorn_node.version,
                                log_module   = check_undefined(Module2),
                                log_function = check_undefined(Function),
                                log_line     = check_undefined(Line2),
                                log_pid      = check_undefined(Pid)},

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
    {Alt_Module, <<"1">>};
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

    %% let the fsm create the log
    Node_Pid =
        case ets:lookup(current_nodes, Node_Name) of
            []                 ->
                ?POPCORN_WARN_MSG("unable to find fsm for node ~p", [Node_Name]),
                undefined;
            [{_, Running_Pid}] ->
                gen_fsm:send_event(Running_Pid, {log_message, Popcorn_Node, Log_Message}),
                Running_Pid
        end,

    triage_handler:safe_notify(Popcorn_Node, Node_Pid, Log_Message, Node_Added),

    case Node_Added of
        false ->
            State;
        true ->
            State#state{known_nodes = lists:append(Known_Nodes, [Node_Name])}
    end.
