-module(node_fsm).
-author('marc.e.campbell@gmail.com').
-behavior(gen_fsm).

-include("include/popcorn.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([start_link/0]).

-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-export([
    'LOGGING'/2,
    'LOGGING'/3]).

-export([get_message_counts/1]).

-record(state, {most_recent_version   :: string(),
                popcorn_node          :: #popcorn_node{}}).

get_message_counts(Node_Pid) ->
    case erlang:is_process_alive(Node_Pid) of
        false -> [{total, 0}];
        true -> gen_fsm:sync_send_event(Node_Pid, get_message_counts)
    end.

start_link() -> gen_fsm:start_link(?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),

    {ok, 'LOGGING', #state{}}.

'LOGGING'({log_message, Popcorn_Node, Log_Message}, State) ->
    try
        %% log the message
        case mnesia:dirty_write(popcorn_history, Log_Message) of
            ok -> ok;
            O  -> ?POPCORN_WARN_MSG("failed to write log entry because: ~p", [O])
        end,

        %% increment the total event counter
        mnesia:dirty_update_counter(popcorn_counters, ?TOTAL_EVENT_COUNTER, 1),

        %% Notify any streams connected
        Log_Streams = ets:tab2list(current_log_streams),
        lists:foreach(fun(Log_Stream) ->
            gen_fsm:send_all_state_event(Log_Stream#stream.stream_pid, {new_message, newer, Log_Message})
          end, Log_Streams)
    catch
        _:Error ->
            io:format("Couldn't log message:~nMessage: ~p~nNode: ~p~nError: ~p~nStack: ~p~n",
                        [Log_Message, Popcorn_Node, Error, erlang:get_stacktrace()])
    end,
    {next_state, 'LOGGING', State}.

'LOGGING'({deserialize_popcorn_node, Popcorn_Node}, _From, State) ->
    Node_Name        = Popcorn_Node#popcorn_node.node_name,
    Prefix           = <<"raw_logs__">>,

    ets:insert(current_roles, {Popcorn_Node#popcorn_node.role, self()}),

    {reply, ok, 'LOGGING', State#state{popcorn_node          = Popcorn_Node}};

'LOGGING'({set_popcorn_node, Popcorn_Node}, _From, State) ->
    mnesia:dirty_write(known_nodes, Popcorn_Node),

    Node_Name        = Popcorn_Node#popcorn_node.node_name,
    Prefix           = <<"raw_logs__">>,

    %% add this node to the "roles" tets table
    ets:insert(current_roles, {Popcorn_Node#popcorn_node.role, self()}),

    {reply, ok, 'LOGGING', State#state{popcorn_node          = Popcorn_Node}};

'LOGGING'(get_message_counts, _From, State) ->
    Severity_Counts = lists:map(fun({Severity, Metric_Name}) ->
                          {lager_util:num_to_level(Severity), mnesia:dirty_update_counter(popcorn_counters, Metric_Name, 0)}
                        end, State#state.severity_metric_names),
    Total_Count     = lists:foldl(fun({_, Count}, Total) -> Total + Count end, 0, Severity_Counts),

    {reply, Severity_Counts ++ [{total, Total_Count}], 'LOGGING', State}.

handle_event(Event, StateName, State)                 -> {stop, {StateName, undefined_event, Event}, State}.
handle_sync_event(Event, _From, StateName, State)     -> {stop, {StateName, undefined_event, Event}, State}.
handle_info(_Info, StateName, State)                  -> {next_state, StateName, State}.
terminate(_Reason, _StateName, State)                 -> ok.
code_change(_OldVsn, StateName, StateData, _Extra)    -> {ok, StateName, StateData}.
