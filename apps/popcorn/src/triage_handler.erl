-module(triage_handler).
-author('martin@tigertext.com').
-author('marc.e.campbell@gamil.com').
-author('elbrujohalcon@inaka.net').

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(UPDATE_INTERVAL, 10000).

-export([counter_data/1, all_alerts/1, alerts/2, recent_alerts/1,
         alert_count_today/0, alert_count/0, clear_alert/1,
         safe_notify/4, log_messages/3, alert_properties/1]).

-include_lib("lager/include/lager.hrl").
-include("include/popcorn.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {
        incident = 1 :: integer(),
        timer :: reference()
}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

safe_notify(Popcorn_Node, Node_Pid, Log_Message, Is_New_Node) ->
    gen_server:cast(?MODULE, {triage_event, Popcorn_Node, Node_Pid, Log_Message, Is_New_Node}).

alert_properties(Alert) ->
    location_as_strings(base64:decode(re:replace(Alert, "_", "=", [{return, binary}, global]))).

location_as_strings(Counter) ->
    lists:zipwith(
        fun(K, V) -> {K, binary_to_list(V)} end,
        [severity, product, version, name, line], split_location(Counter)).

split_location(Counter) -> re:split(Counter, <<":">>, [{return, binary}]).

log_messages(Alert, Starting_Timestamp, Page_Size) ->
    gen_server:call(?MODULE, {messages, base64:decode(re:replace(Alert, "_", "=", [{return, binary}, global])), Starting_Timestamp, Page_Size}).

counter_data(Counter) -> gen_server:call(?MODULE, {data, Counter}).

alert_count() -> gen_server:call(?MODULE, total_alerts).

alert_count_today() -> gen_server:call(?MODULE, alerts_for_today).

all_alerts(true = _Include_Cleared) -> gen_server:call(?MODULE, {alerts, all, all});
all_alerts(_) -> gen_server:call(?MODULE, {alerts, recent, all}).

alerts(true = _Include_Cleared, Severities) -> gen_server:call(?MODULE, {alerts, all, Severities});
alerts(_, Severities) -> gen_server:call(?MODULE, {alerts, recent, Severities}).

recent_alerts(Count) -> gen_server:call(?MODULE, {alerts, Count, all}).

clear_alert(Alert) ->
    Counter = base64:decode(re:replace(Alert, "_", "=", [{return, binary}, global])),
    gen_server:call(?MODULE, {clear, binary_to_list(Counter)}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_) ->
    Timer = erlang:send_after(?UPDATE_INTERVAL, self(), update_counters),
    {ok, #state{timer = Timer}}.

handle_call({data, Counter}, _From, State) ->
    V = case gen_server:call(pg2:get_closest_pid('storage'), {get_alert, Counter}) of
            #alert{} = Alert -> Alert;
            _ -> #alert{}
        end,
    {reply, data(V), State};
handle_call(total_alerts, _From, State) ->
    Alert_Count = ?COUNTER_VALUE(?TOTAL_ALERT_COUNTER),
    {reply, Alert_Count, State};
handle_call(alerts_for_today, _From, State) ->
    Day_Key = day_key(),
    Day_Alerts = ?COUNTER_VALUE(Day_Key),
    {reply, Day_Alerts, State};
handle_call({alerts, Count, Severities}, _From, State) ->
    %% storage_mnesia
    %% popcorn_udp
    Alerts = gen_server:call(pg2:get_closest_pid('storage'), {get_alerts, Severities}),
    {Small_List,_} = lists:split(erlang:min(Count, length(Alerts)), Alerts),
    FinalList = [data(Alert) || Alert <- Small_List],
    {reply, FinalList, State};
handle_call({clear, Counter}, _From, State) ->
    Key = recent_key(Counter),

    gen_server:cast(?STORAGE_PID, {delete_counter, Key}),
    ?DECREMENT_COUNTER(?TOTAL_ALERT_COUNTER),
    Total_Alert_Count = ?COUNTER_VALUE(?TOTAL_ALERT_COUNTER),
    NewCounters =
        [{counter,      Counter},
         {alert_count,  Total_Alert_Count}],
    dashboard_stream_fsm:broadcast({update_counters, NewCounters}),
    {reply, ok, reset_timer(State)};
handle_call({messages, Alert, Starting_Timestamp, Page_Size}, _From, State) ->
    [S, P, V, M, L] = split_location(Alert),
    Messages = gen_server:call(?STORAGE_PID, {search_messages, {popcorn_util:severity_to_number(S), P, V, M, L, Page_Size, Starting_Timestamp}}),
    {reply, Messages, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({triage_event, #popcorn_node{} = Node, Node_Pid,
              #log_message{log_product=Product, log_version=Version,
                           log_module=Module, log_line=Line, severity=Severity} = Log_Entry,
              Is_New_Node}, #state{incident=Incident} = State)
        when Severity =< 16, Severity =/= 0, is_binary(Product), is_binary(Version), is_binary(Module), is_binary(Line) ->
    gen_server:cast(pg2:get_closest_pid('storage'), {new_alert, key(Severity,Product,Version,Module,Line), #alert{log=Log_Entry, incident=Incident}}),
    case Is_New_Node of
        true ->
            outbound_notifier:notify(new_node, as_proplist(Node)),
            dashboard_stream_fsm:broadcast({new_node, Node});
        false -> ok
    end,
    update_counter(Node,Node_Pid,Severity,Product,Version,Module,Line),
    {noreply, reset_timer(State)};
handle_cast({triage_event, #popcorn_node{} = Node, _Node_Pid, _Log_Message, true}, State) ->
    outbound_notifier:notify(new_node, as_proplist(Node)),
    dashboard_stream_fsm:broadcast({new_node, Node}),
    {noreply, State};
handle_cast({triage_event, #popcorn_node{}, _Node_Pid, _Log_Message, false}, State) ->
    {noreply, State};
handle_cast(Event, State) ->
    io:format("Unexpected event: ~p~n", [Event]),
    {noreply, State}.

handle_info(update_counters, State) ->
    lists:foreach(
        fun({_, undefined}) -> ok;
           ({Node_Name, _Node_Pid}) ->
            NodeCounters =
                [{node_hash,  re:replace(base64:encode(Node_Name), "=", "_", [{return, binary}, global])},
                  {node_count, ?COUNTER_VALUE(?NODE_EVENT_COUNTER(Node_Name))}],
            dashboard_stream_fsm:broadcast({update_counters, NodeCounters})
        end, ets:tab2list(current_nodes)),

    Day_Key = day_key(),

    %% TODO, perhaps this should be optimized
    gen_server:cast(pg2:get_closest_pid('storage'), {new_alert_key, day, Day_Key}),

    Day_Count   = ?COUNTER_VALUE(Day_Key),
    Event_Count = ?COUNTER_VALUE(?TOTAL_EVENT_COUNTER),
    Alert_Count = ?COUNTER_VALUE(?TOTAL_ALERT_COUNTER),

    NewCounters =
        [{event_count,       Event_Count},
         {alert_count_today, Day_Count},
         {alert_count,       Alert_Count}],

    dashboard_stream_fsm:broadcast({update_counters, NewCounters}),
    {noreply, reset_timer(State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_counter(Node, _Node_Pid, Severity, Product, Version, Module, Line) ->
    Count_Key           = key(Severity,Product,Version,Module,Line),
    Recent_Counter_Key  = recent_key(Count_Key),
    Day_Key             = day_key(),

    %% TODO perhaps these next 2 lines should be optimized, store in state if we have added to the ets table?
    gen_server:cast(pg2:get_closest_pid('storage'), {new_alert_key, alert, {Severity, Product, Version, Module, Line}}),
    gen_server:cast(pg2:get_closest_pid('storage'), {new_alert_key, day, Day_Key}),
    %% storage_mnesia

    case ?COUNTER_VALUE(Count_Key) of
        0 -> ?INCREMENT_COUNTER(Day_Key);
        _ -> ok
    end,

    ?INCREMENT_COUNTER(Count_Key),
    ?INCREMENT_COUNTER(Recent_Counter_Key),

    case ?COUNTER_VALUE(Recent_Counter_Key) of
        1 ->
            outbound_notifier:notify(new_alert, location_as_strings(Count_Key)),
            ?INCREMENT_COUNTER(?TOTAL_ALERT_COUNTER);
        _ -> ok
    end,

    Day_Count         = ?COUNTER_VALUE(Day_Key),
    Event_Count       = ?COUNTER_VALUE(?TOTAL_EVENT_COUNTER),
    Alert_Count       = ?COUNTER_VALUE(?TOTAL_ALERT_COUNTER),
    Node_Event_Count  = ?COUNTER_VALUE(?NODE_EVENT_COUNTER(Node#popcorn_node.node_name)),

    NewCounters =
        [   {node_hash,         re:replace(base64:encode(Node#popcorn_node.node_name), "=", "_", [{return, binary}, global])},
            {node_count,        Node_Event_Count},
            {counter,           Count_Key},
            {event_count,       Event_Count},
            {alert_count_today, Day_Count},
            {alert_count,       Alert_Count}],

    outbound_notifier:notify(new_event, [{location, Count_Key} | location_as_strings(Count_Key)]),
    dashboard_stream_fsm:broadcast({update_counters, NewCounters}).

key(Severity,Product,Version,Module,Line) ->
    SeverityName = list_to_binary(popcorn_util:number_to_severity(Severity)),
    binary_to_list(<<SeverityName/binary, ":", Product/binary, ":", Version/binary, ":", Module/binary, ":", Line/binary>>).

recent_key(Counter) -> "recent:" ++ Counter.

%% TODO: use the timestamp from the log
day_key() ->
        {{Year,Month,Day},_} = calendar:now_to_universal_time(erlang:now()),
        integer_to_list(Year) ++ "-" ++ integer_to_list(Month) ++ "-" ++ integer_to_list(Day).

data(#alert{location = undefined}) -> [];
data(Alert) ->
    Basic_Properties =
      case Alert of
        #alert{log = #log_message{message = Message, severity = SeverityNumber}} ->
          [{'severity_num', SeverityNumber}, {'message', list(Message)}];
        #alert{} ->
          []
      end,

    All_Properties = location_as_strings(Alert#alert.location) ++ Basic_Properties,

    Location_Count        = ?COUNTER_VALUE(Alert#alert.location),
    Recent_Location_Count = ?COUNTER_VALUE(recent_key(Alert#alert.location)),

    [{location, re:replace(base64:encode(Alert#alert.location), "=", "_", [{return, list}, global])},
     {count,    Location_Count},
     {recent,   Recent_Location_Count}
     | All_Properties].

list(B) when is_binary(B) -> binary_to_list(B);
list(_) -> "".

reverse_limit_and_filter(Alerts, all) -> [data(Alert) || Alert <- lists:reverse(Alerts)];
reverse_limit_and_filter(Alerts, Count) ->
    reverse_limit_and_filter(lists:reverse(Alerts), Count, []).
reverse_limit_and_filter([], _Count, Acc) -> lists:reverse(Acc);
reverse_limit_and_filter(_Alerts, Count, Acc) when length(Acc) == Count -> lists:reverse(Acc);
reverse_limit_and_filter([Alert | Alerts], Count, Acc) ->
    Location_Count = ?COUNTER_VALUE(recent_key(Alert#alert.location)),

    reverse_limit_and_filter(
        Alerts, Count,
        case Location_Count of
            0 -> Acc;
            _ -> [data(Alert) | Acc]
        end).

reset_timer(#state{incident=Incident} = State) ->
    erlang:cancel_timer(State#state.timer),
    Timer = erlang:send_after(?UPDATE_INTERVAL, self(), update_counters),
    State#state{timer = Timer, incident=Incident+1}.

as_proplist(Node) when is_record(Node, popcorn_node) ->
    [popcorn_node | Props] = tuple_to_list(Node),
    lists:zip(record_info(fields, popcorn_node), Props).