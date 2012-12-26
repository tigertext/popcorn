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

-record(state, {severity_metric_names :: list(),
                most_recent_version   :: string(),
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

        %% increment the severity counter for this node
        mnesia:dirty_update_counter(popcorn_counters, proplists:get_value(Log_Message#log_message.severity, State#state.severity_metric_names), 1),

        %% increment the total event counter
        mnesia:dirty_update_counter(popcorn_counters, ?TOTAL_EVENT_COUNTER, 1),

        %% ensure the metric exists for this hour, severity combination and increment
        Prefix    = <<"_popcorn__">>,
        Hour      = list_to_binary(popcorn_util:hour()),
        SeverityB = list_to_binary(integer_to_list(Log_Message#log_message.severity)),
        Sep       = <<"_">>,
        Node_Name = Popcorn_Node#popcorn_node.node_name,

        Node_Severity_History_Counter = binary_to_atom(<<Prefix/binary, Sep/binary, Node_Name/binary, Sep/binary, SeverityB/binary, Sep/binary, Hour/binary>>, latin1),
        Total_Severity_History_Counter = binary_to_atom(<<Prefix/binary, Sep/binary, SeverityB/binary, Sep/binary, Hour/binary>>, latin1),

        mnesia:dirty_update_counter(popcorn_counters, Node_Severity_History_Counter, 1),
        mnesia:dirty_update_counter(popcorn_counters, Total_Severity_History_Counter, 1),

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

    %% 0 = emergency -> 7 = debug
    Separator_Binary = <<30>>,
    Severity_Metric_Names = lists:map(fun(Level) ->
                                Level_Bin = list_to_binary(integer_to_list(Level)),
                                Severity_Counter_Name = binary_to_atom(<<Prefix/binary, Node_Name/binary, Separator_Binary/binary, Level_Bin/binary>>, latin1),
                                {Level, Severity_Counter_Name}
                              end, lists:seq(0, 7)),

    {reply, ok, 'LOGGING', State#state{severity_metric_names = Severity_Metric_Names,
                                       popcorn_node          = Popcorn_Node}};

'LOGGING'({set_popcorn_node, Popcorn_Node}, _From, State) ->
    mnesia:dirty_write(known_nodes, Popcorn_Node),

    Node_Name        = Popcorn_Node#popcorn_node.node_name,
    Prefix           = <<"raw_logs__">>,

    %% add this node to the "roles" tets table
    ets:insert(current_roles, {Popcorn_Node#popcorn_node.role, self()}),

    %% 0 = emergency -> 7 = debug
    Separator_Binary = <<30>>,
    Severity_Metric_Names = lists:map(fun(Level) ->
                                Level_Bin = list_to_binary(integer_to_list(Level)),
                                Severity_Counter_Name = binary_to_atom(<<Prefix/binary, Node_Name/binary, Separator_Binary/binary, Level_Bin/binary>>, latin1),
                                {Level, Severity_Counter_Name}
                              end, lists:seq(0, 7)),

    %% create the metrics
    lists:foreach(fun({_, N}) -> mnesia:dirty_update_counter(popcorn_counters, N, 0) end, Severity_Metric_Names),

    {reply, ok, 'LOGGING', State#state{severity_metric_names = Severity_Metric_Names,
                                       popcorn_node          = Popcorn_Node}};

'LOGGING'(get_message_counts, _From, State) ->
    Severity_Counts = lists:map(fun({Severity, Metric_Name}) ->
                          {lager_util:num_to_level(Severity), mnesia:dirty_update_counter(popcorn_counters, Metric_Name, 0)}
                        end, State#state.severity_metric_names),
    Total_Count     = lists:foldl(fun({_, Count}, Total) -> Total + Count end, 0, Severity_Counts),

    {reply, Severity_Counts ++ [{total, Total_Count}], 'LOGGING', State};
'LOGGING'({severity_count_history, Severity}, _From, State) ->
    Last_24_Hours = popcorn_util:last_24_hours(),

    Prefix    = <<"_popcorn__">>,
    SeverityB = list_to_binary(integer_to_list(Severity)),
    Sep       = <<"_">>,
    Node_Name = (State#state.popcorn_node)#popcorn_node.node_name,

		Hours_Ago     = lists:seq(0, 23),
    Metric_Names  = lists:map(fun(Hour) -> HourB = list_to_binary(Hour), binary_to_atom(<<Prefix/binary, Sep/binary, Node_Name/binary, Sep/binary, SeverityB/binary, Sep/binary, HourB/binary>>, latin1) end, popcorn_util:last_24_hours()),
		Time_And_Name = lists:zip(Hours_Ago, Metric_Names),

    Values = lists:map(fun({Hour_Ago, Metric_Name}) ->
                 %% this is just to simplify the code, instead of a read, check if exists, this returns a quick value...
                 Value = mnesia:dirty_update_counter(popcorn_counters, Metric_Name, 0),
								 [{'hours_ago', 0 - Hour_Ago},
									{'count',     Value}]
               end, Time_And_Name),

    {reply, Values, 'LOGGING', State}.

handle_event(Event, StateName, State)                 -> {stop, {StateName, undefined_event, Event}, State}.
handle_sync_event(Event, _From, StateName, State)     -> {stop, {StateName, undefined_event, Event}, State}.
handle_info(_Info, StateName, State)                  -> {next_state, StateName, State}.
terminate(_Reason, _StateName, State)                 -> ok.
code_change(_OldVsn, StateName, StateData, _Extra)    -> {ok, StateName, StateData}.
