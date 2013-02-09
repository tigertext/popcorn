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
-record(state, {socket      :: pid(),
                known_nodes :: list()}).

init(_) ->
    {ok, Udp_Listen_Port} = application:get_env(popcorn, udp_listen_port),
    {ok, Socket} = gen_udp:open(Udp_Listen_Port, [binary, {active, once}, {recbuf, 524288}]),

    {ok, #state{socket      = Socket,
                known_nodes = []}}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({udp, Socket, Host, _Port, Bin}, State) ->
    ?RPS_INCREMENT(udp_received),
    try decode_protobuffs_message(Bin) of
        {Popcorn_Node, Log_Message} ->
            Node_Added = ingest_packet(State#state.known_nodes, Popcorn_Node, Log_Message),
            inet:setopts(Socket, [{active, once}]),
            case Node_Added of
                false ->
                    {noreply, State};
                true ->
                    {noreply, State#state{known_nodes = lists:append(State#state.known_nodes, [Popcorn_Node#popcorn_node.node_name])}}
            end
    catch
        error:Reason -> error_logger:error_msg("Error ingesting packet from ~p reason:~p", [Host, Reason]),
        {noreply, State}
    end;

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

-spec decode_protobuffs_message(binary()) -> {#popcorn_node{}, #log_message{}} | error.
decode_protobuffs_message(Encoded_Message) ->
    {{1, Packet_Version},  Rest} = protobuffs:decode(Encoded_Message, bytes),

    case Packet_Version of
        1 -> 
            decode_protobuffs_message(1, Rest);
        No_Version when is_binary(No_Version) ->
            decode_protobuffs_message(0, Encoded_Message);
        Other ->
            ?POPCORN_ERROR_MSG("#unknown packet #version: ~p", [Other]),
            error
    end.

decode_protobuffs_message(0, Encoded_Message) ->
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

    Log_Message  = #log_message{message_id   = ?PU:unique_id(),
                                timestamp    = ?NOW,     %% this should be part of the protobuffs packet?
                                severity     = check_undefined(Severity),
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

decode_protobuffs_message(1, Rest) ->
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

    Popcorn_Node = #popcorn_node{node_name = check_undefined(Node),
                                 role      = check_undefined(Node_Role),
                                 version   = check_undefined(Node_Version)},

    %% Ensure we have a module and line so we can perform rollup
    {Module2, Line2} = location_check(Module, Line, Message),

    Log_Message  = #log_message{message_id   = ?PU:unique_id(),
                                timestamp    = ?NOW,     %% this should be part of the protobuffs packet?
                                severity     = check_undefined(Severity),
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

-spec ingest_packet(list(), #popcorn_node{}, #log_message{}) -> boolean().  %% return value is whether is a new node
ingest_packet(Known_Nodes, Popcorn_Node, Log_Message) ->
    ?RPS_INCREMENT(udp),

    %% create the node fsm, if necessary
    Node_Added =
      case lists:member(Popcorn_Node#popcorn_node.node_name, Known_Nodes) of
          false ->
              %% suspect this is a new node, but check the database just to be safe
              case gen_server:call(?STORAGE_PID, {is_known_node, Popcorn_Node#popcorn_node.node_name}) of
                  false -> {ok, Pid} = supervisor:start_child(node_sup, []),
                           ok = gen_fsm:sync_send_event(Pid, {set_popcorn_node, Popcorn_Node}),
                           ets:insert(current_nodes, {Popcorn_Node#popcorn_node.node_name, Pid}),
                           true;
                  _     -> false
              end,
              true;  %% return true so that this node is added to the known_nodes state variable
          true  -> false
      end,
%
%    %% let the fsm create the log
%    Node_Pid =
%        case ets:lookup(current_nodes, Popcorn_Node#popcorn_node.node_name) of
%            []                 ->
%                ?POPCORN_WARN_MSG("unable to find fsm for node ~p", [Popcorn_Node#popcorn_node.node_name]),
%                undefined;
%            [{_, Running_Pid}] ->
%                gen_fsm:send_event(Running_Pid, {log_message, Popcorn_Node, Log_Message}),
%                Running_Pid
%        end,
%
%    triage_handler:safe_notify(Popcorn_Node, Node_Pid, Log_Message, Node_Added),
    Node_Added.
