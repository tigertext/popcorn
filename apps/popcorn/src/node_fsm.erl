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

-define(EXPIRE_TIMER, 15000).

-record(state, {history_name          :: atom(),
                severity_metric_names :: list(),
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

    gen_fsm:start_timer(?EXPIRE_TIMER, expire_log_messages),

    {ok, 'LOGGING', #state{}}.

'LOGGING'({timeout, _From, expire_log_messages}, State) ->
    {ok, Retentions} = application:get_env(popcorn, log_retention),
    lists:foreach(fun({Severity, Retention_Interval}) ->
        Microseconds = popcorn_util:retention_time_to_microsec(Retention_Interval),
        Oldest_TS    = ?NOW - Microseconds,
        Severity_Num = popcorn_util:severity_to_number(Severity)
        %I = mnesia:select(State#state.history_name, ets:fun2ms(fun(#log_message{timestamp = TS, severity = S} = Log_Message) when TS < Oldest_TS andalso S =:= Severity_Num -> Log_Message end)),
        %?POPCORN_DEBUG_MSG("I = ~p", [I])
      end, Retentions),

    gen_fsm:start_timer(?EXPIRE_TIMER, expire_log_messages),

    {next_state, 'LOGGING', State};

'LOGGING'({log_message, Popcorn_Node, Log_Message}, State) ->
    try
        %% log the message
        mnesia:dirty_write(State#state.history_name, Log_Message),

        %% increment the severity counter for this node
        folsom_metrics:notify({proplists:get_value(Log_Message#log_message.severity, State#state.severity_metric_names), {inc, 1}}),

        %% increment the total event counter
        folsom_metrics:notify({?TOTAL_EVENT_COUNTER, {inc, 1}}),

        %% ensure the metric exists for this hour, severity combination and increment
        Prefix    = <<"_popcorn__">>,
        Hour      = list_to_binary(popcorn_util:hour()),
        SeverityB = list_to_binary(integer_to_list(Log_Message#log_message.severity)),
        Sep       = <<"_">>,
        Node_Name = Popcorn_Node#popcorn_node.node_name,

        Node_Severity_History_Counter = binary_to_atom(<<Prefix/binary, Sep/binary, Node_Name/binary, Sep/binary, SeverityB/binary, Sep/binary, Hour/binary>>, latin1),
        Total_Severity_History_Counter = binary_to_atom(<<Prefix/binary, Sep/binary, SeverityB/binary, Sep/binary, Hour/binary>>, latin1),

        case folsom_metrics:metric_exists(Node_Severity_History_Counter) of
            false -> folsom_metrics:new_counter(Node_Severity_History_Counter);
            true  -> ok
        end,

        case folsom_metrics:metric_exists(Total_Severity_History_Counter) of
            false -> folsom_metrics:new_counter(Total_Severity_History_Counter);
            true  -> ok
        end,

        folsom_metrics:notify({Node_Severity_History_Counter, {inc, 1}}),
        folsom_metrics:notify({Total_Severity_History_Counter, {inc, 1}}),

        %% Notify any streams connected
        Log_Streams = ets:tab2list(current_log_streams),
        lists:foreach(fun(Log_Stream) ->
            gen_fsm:send_all_state_event(Log_Stream#stream.stream_pid, {new_message, Log_Message})
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
    History_Name     = binary_to_atom(<<Prefix/binary, Node_Name/binary>>, latin1),

    ets:insert(current_roles, {Popcorn_Node#popcorn_node.role, self()}),

    %% 0 = emergency -> 7 = debug
    Separator_Binary = <<30>>,
    Severity_Metric_Names = lists:map(fun(Level) ->
                                Level_Bin = list_to_binary(integer_to_list(Level)),
                                Severity_Counter_Name = binary_to_atom(<<Prefix/binary, Node_Name/binary, Separator_Binary/binary, Level_Bin/binary>>, latin1),
                                {Level, Severity_Counter_Name}
                              end, lists:seq(0, 7)),

    %% create the metrics
    lists:foreach(fun({_, N}) -> folsom_metrics:new_counter(N) end, Severity_Metric_Names),

    {reply, ok, 'LOGGING', State#state{history_name          = History_Name,
                                       severity_metric_names = Severity_Metric_Names,
                                       popcorn_node          = Popcorn_Node}};

'LOGGING'({set_popcorn_node, Popcorn_Node}, _From, State) ->
    mnesia:dirty_write(known_nodes, Popcorn_Node),

    Node_Name        = Popcorn_Node#popcorn_node.node_name,
    Prefix           = <<"raw_logs__">>,
    History_Name     = binary_to_atom(<<Prefix/binary, Node_Name/binary>>, latin1),

    %% create the mnesia table to stpre the raw logs for this node
    {atomic, ok} = mnesia:create_table(History_Name, [{disc_copies, [node()]},
                                                      {record_name, log_message},
                                                      {index,       [#log_message.severity]},     %% this can't be the first element in the record
                                                      {attributes,  record_info(fields, log_message)}]),

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
    lists:foreach(fun({_, N}) -> folsom_metrics:new_counter(N) end, Severity_Metric_Names),

    {reply, ok, 'LOGGING', State#state{history_name          = History_Name,
                                       severity_metric_names = Severity_Metric_Names,
                                       popcorn_node          = Popcorn_Node}};

'LOGGING'(get_message_counts, _From, State) ->
    Severity_Counts = lists:map(fun({Severity, Metric_Name}) ->
                          {lager_util:num_to_level(Severity), folsom_metrics:get_metric_value(Metric_Name)}
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
                 Value = case folsom_metrics:metric_exists(Metric_Name) of
                             false -> 0;
                      			 true  -> folsom_metrics:get_metric_value(Metric_Name)
                 				 end,
								 [{'hours_ago', 0 - Hour_Ago},
									{'count',     Value}]
               end, Time_And_Name),

    {reply, Values, 'LOGGING', State}.

handle_event(Event, StateName, State)                 -> {stop, {StateName, undefined_event, Event}, State}.
handle_sync_event(Event, _From, StateName, State)     -> {stop, {StateName, undefined_event, Event}, State}.
handle_info(_Info, StateName, State)                  -> {next_state, StateName, State}.
terminate(_Reason, _StateName, State)                 -> ok.

code_change(_OldVsn, StateName, StateData, _Extra)    -> {ok, StateName, StateData}.



