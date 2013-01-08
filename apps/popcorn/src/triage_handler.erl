-module(triage_handler).
-author('martin@tigertext.com').
-author('marc.e.campbell@gamil.com').
-author('elbrujohalcon@inaka.net').

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(UPDATE_INTERVAL, 10000).

-record(alert, {location, log, timestamp = erlang:now()}).

-export([counter_data/1, all_alerts/1, recent_alerts/1,
         alert_count_today/0, alert_count/0, clear_alert/1,
         safe_notify/4, log_messages/6, decode_location/1]).

-include_lib("lager/include/lager.hrl").
-include("include/popcorn.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {timer :: reference()}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

safe_notify(Popcorn_Node, Node_Pid, Log_Message, Is_New_Node) ->
    gen_server:cast(?MODULE, {triage_event, Popcorn_Node, Node_Pid, Log_Message, Is_New_Node}).

decode_location(Alert) ->
    Counter = base64:decode(re:replace(Alert, "_", "=", [{return, binary}, global])),
    do_decode_location(Counter).

do_decode_location(Counter) ->
    Parts = re:split(Counter, <<":-:">>, [{return, list}]),
    lists:zip([product, version, name, line], Parts).

log_messages(Product, Version, Name, Line, Starting_Timestamp, Page_Size) ->
    gen_server:call(?MODULE, {messages, Product, Version, Name, Line, Starting_Timestamp, Page_Size}).

counter_data(Counter) -> gen_server:call(?MODULE, {data, Counter}).

alert_count() -> gen_server:call(?MODULE, total_alerts).

alert_count_today() -> gen_server:call(?MODULE, alerts_for_today).

all_alerts(true = _Include_Cleared) -> gen_server:call(?MODULE, {alerts, all});
all_alerts(_) -> gen_server:call(?MODULE, {alerts, recent}).

recent_alerts(Count) -> gen_server:call(?MODULE, {alerts, Count}).

clear_alert(Alert) ->
    Counter = base64:decode(re:replace(Alert, "_", "=", [{return, binary}, global])),
    gen_server:call(?MODULE, {clear, binary_to_list(Counter)}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_) ->
    try ets:new(triage_error_keys, [named_table, set, public, {keypos, 2}]) of
        triage_error_keys -> ok
    catch
        _:badarg -> ok
    end,
    try ets:new(triage_error_data, [named_table, set, public, {keypos, #alert.location}]) of
        triage_error_data -> ok
    catch
        _:badarg -> ok
    end,
    Timer = erlang:send_after(?UPDATE_INTERVAL, self(), update_counters),
    {ok, #state{timer = Timer}}.

handle_call({data, Counter}, _From, State) ->
    V = case ets:lookup(triage_error_data, Counter) of
            [#alert{} = Alert] -> Alert;
            _ -> #alert{}
        end,
    {reply, data(V), State};
handle_call(total_alerts, _From, State) ->
    [{popcorn_counters, _, Alert_Count}] = mnesia:dirty_read(popcorn_counters, ?TOTAL_ALERT_COUNTER),
    {reply, Alert_Count, State};
handle_call(alerts_for_today, _From, State) ->
    Day_Key = day_key(),
    Day_Alerts = case mnesia:dirty_read(popcorn_counters, Day_Key) of
                     None when length(None) =:= 0 -> 0;
                     [{popcorn_counters, _, V}]   -> V
                 end,

    {reply, Day_Alerts, State};
handle_call({alerts, Count}, _From, State) ->
    Alerts =
        [   case ets:lookup(triage_error_data, Counter) of
                [#alert{} = Alert] -> Alert;
                _ -> #alert{}
            end || {key, Counter} <- ets:tab2list(triage_error_keys), string:str(Counter, ":") =/= 0 ],
    Sorted = lists:keysort(#alert.timestamp, Alerts),
    FinalList = reverse_limit_and_filter(Sorted, Count),
    {reply, FinalList, State};
handle_call({clear, Counter}, _From, State) ->
    Key = recent_key(Counter),

    mnesia:dirty_delete(popcorn_counters, Key),
    mnesia:dirty_update_counter(popcorn_counters, Key, 0),

    Total_Alert_Count = mnesia:dirty_update_counter(popcorn_counters, ?TOTAL_ALERT_COUNTER, -1),
    NewCounters =
        [{counter,      Counter},
         {alert_count,  Total_Alert_Count}],
    dashboard_stream_fsm:broadcast({update_counters, NewCounters}),
    {reply, ok, reset_timer(State)};
handle_call({messages, Product, Version, Module, Line, Starting_Timestamp, Page_Size}, _From, State) ->
    P = list_to_binary(Product),
    V = list_to_binary(Version),
    M = list_to_binary(Module),
    L = list_to_binary(Line),
    Messages =
        case mnesia:transaction(
                fun() ->
                    mnesia:select(
                        popcorn_history,
                        ets:fun2ms(
                            fun(#log_message{timestamp = TS, log_product = LP, log_version = LV, log_module = LM, log_line = LL} = Log_Message)
                                when LP == P, LV == V, LM == M, LL == L, (Starting_Timestamp == undefined orelse TS > Starting_Timestamp) -> Log_Message end),
                        Page_Size,
                        read)
                end) of
            {atomic, {Ms, _}} -> Ms;
            {atomic, '$end_of_table'} -> []
        end,
    {reply, Messages, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({triage_event, #popcorn_node{} = Node, Node_Pid,
              #log_message{log_product=Product, log_version=Version,
                           log_module=Module, log_line=Line, severity=Severity} = Log_Entry,
              Is_New_Node}, State)
        when Severity < 4, is_binary(Product), is_binary(Version), is_binary(Module), is_binary(Line) ->
    true = ets:insert(triage_error_data, #alert{location=key(Product,Version,Module,Line), log=Log_Entry}),
    case Is_New_Node of
        true ->
            outbound_notifier:notify(new_node, as_proplist(Node)),
            dashboard_stream_fsm:broadcast({new_node, Node});
        false -> ok
    end,
    update_counter(Node,Node_Pid,Product,Version,Module,Line),
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
           ({Node_Name, Node_Pid}) ->
            NodeCounters =
                [{node_hash,  re:replace(base64:encode(Node_Name), "=", "_", [{return, binary}, global])},
                 {node_count, proplists:get_value(total, node_fsm:get_message_counts(Node_Pid), 0)}],
            dashboard_stream_fsm:broadcast({update_counters, NodeCounters})
        end, ets:tab2list(current_nodes)),

    Day_Key = day_key(),

    %% TODO, perhaps this should be optimized
    true = ets:insert(triage_error_keys, {key, Day_Key}),

    Day_Count = mnesia:dirty_update_counter(popcorn_counters, Day_Key, 0),

    [{popcorn_counters, _, Event_Count}] = mnesia:dirty_read(popcorn_counters, ?TOTAL_EVENT_COUNTER),
    [{popcorn_counters, _, Alert_Count}] = mnesia:dirty_read(popcorn_counters, ?TOTAL_ALERT_COUNTER),

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

update_counter(Node, Node_Pid, Product, Version, Module, Line) ->
    Count_Key           = key(Product,Version,Module,Line),
    Recent_Counter_Key  = recent_key(Count_Key),
    Day_Key             = day_key(),

    %% TODO perhaps these next 2 lines should be optimized, store in state if we have added to the ets table?
    true = ets:insert(triage_error_keys, {key, Count_Key}),
    true = ets:insert(triage_error_keys, {key, Day_Key}),

    case mnesia:dirty_update_counter(popcorn_counters, Count_Key, 0) of
        0 -> mnesia:dirty_update_counter(popcorn_counters, Day_Key, 1);
        _ -> ok
    end,

    mnesia:dirty_update_counter(popcorn_counters, Count_Key, 1),
    Recent_Value = mnesia:dirty_update_counter(popcorn_counters, Recent_Counter_Key, 1),

    case Recent_Value of
        1 ->
            outbound_notifier:notify(new_alert, do_decode_location(Count_Key)),
            mnesia:dirty_update_counter(popcorn_counters, ?TOTAL_ALERT_COUNTER, 1);
        _ -> ok
    end,

    [{popcorn_counters, _, Event_Count}] = mnesia:dirty_read(popcorn_counters, ?TOTAL_EVENT_COUNTER),
    [{popcorn_counters, _, Alert_Count}] = mnesia:dirty_read(popcorn_counters, ?TOTAL_ALERT_COUNTER),
    [{popcorn_counters, _, Day_Count}]   = mnesia:dirty_read(popcorn_counters, Day_Key),
    NewCounters =
        [   {node_hash,         re:replace(base64:encode(Node#popcorn_node.node_name), "=", "_", [{return, binary}, global])},
            {node_count,        case Node_Pid of
                                    undefined -> 0;
                                    Node_Pid ->
                                        proplists:get_value(total, node_fsm:get_message_counts(Node_Pid), 0)
                                end},
            {counter,           Count_Key},
            {event_count,       Event_Count},
            {alert_count_today, Day_Count},
            {alert_count,       Alert_Count}],

    outbound_notifier:notify(new_event, [{location, Count_Key} | do_decode_location(Count_Key)]),
    dashboard_stream_fsm:broadcast({update_counters, NewCounters}).

key(Product,Version,Module,Line) ->
    binary_to_list(<<Product/binary, ":-:", Version/binary, ":-:", Module/binary, ":-:", Line/binary>>).

recent_key(Counter) -> "recent:" ++ Counter.

%% TODO: use the timestamp from the log
day_key() ->
        {{Year,Month,Day},_} = calendar:now_to_universal_time(erlang:now()),
        integer_to_list(Year) ++ "-" ++ integer_to_list(Month) ++ "-" ++ integer_to_list(Day).

data(#alert{location = undefined}) -> [];
data(Alert) ->
    Basic_Properties =
      case Alert of
        #alert{log = #log_message{message = Message}} ->
          [ {'message', list(Message)}];
        #alert{} ->
          []
      end,
    All_Properties =
        case string:tokens(Alert#alert.location, ":-:") of
            [Product,Version,Module,Line|_] ->
                [{product,  Product},
                 {version,  Version},
                 {name,     Module},
                 {line,     Line}
                 | Basic_Properties];
            _ -> Basic_Properties
        end,

    [{popcorn_counters, _, Location_Count}] = mnesia:dirty_read(popcorn_counters, Alert#alert.location),
    [{popcorn_counters, _, Recent_Location_Count}] = mnesia:dirty_read(popcorn_counters, recent_key(Alert#alert.location)),

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
    Location_Count = mnesia:dirty_update_counter(popcorn_counters, recent_key(Alert#alert.location), 0),

    reverse_limit_and_filter(
        Alerts, Count,
        case Location_Count of
            0 -> Acc;
            _ -> [data(Alert) | Acc]
        end).

reset_timer(State) ->
    erlang:cancel_timer(State#state.timer),
    Timer = erlang:send_after(?UPDATE_INTERVAL, self(), update_counters),
    State#state{timer = Timer}.

as_proplist(Node) when is_record(Node, popcorn_node) ->
    [popcorn_node | Props] = tuple_to_list(Node),
    lists:zip(record_info(fields, popcorn_node), Props).