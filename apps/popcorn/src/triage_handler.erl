-module(triage_handler).
-author('martin@tigertext.com').
-behaviour(gen_event).

-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         day_key/0,
         code_change/3
]).

-export([counter_data/1, all_alerts/1, recent_alerts/1, alert_count_today/0, alert_count/0, clear_alert/1]).

-include_lib("lager/include/lager.hrl").
-include("include/popcorn.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {}).

counter_data(Counter) -> gen_event:call(?MODULE, ?MODULE, {data, Counter}).

alert_count() -> gen_event:call(?MODULE, ?MODULE, total_alerts).

alert_count_today() -> gen_event:call(?MODULE, ?MODULE, alerts_for_today).

all_alerts(true = _Include_Cleared) -> gen_event:call(?MODULE, ?MODULE, {alerts, all});
all_alerts(_) -> gen_event:call(?MODULE, ?MODULE, {alerts, recent}).

recent_alerts(Count) -> gen_event:call(?MODULE, ?MODULE, {alerts, Count}).

clear_alert(Counter) -> gen_event:call(?MODULE, ?MODULE, {clear, Counter}).

init(_) ->
    ets:new(triage_error_keys, [named_table, set, public, {keypos, 2}]),
    ets:new(triage_error_data, [named_table, set, public, {keypos, #alert.location}]),
    folsom_metrics:new_counter("total_alerts"),
    {ok, #state{}}.

handle_call({data, Counter}, State) ->
    V = case ets:lookup(triage_error_data, Counter) of
            [#alert{} = Alert] -> Alert;
            _ -> #alert{}
        end,
    {ok, data(V), State};
handle_call(total_alerts, State) ->
    {ok, folsom_metrics:get_metric_value("total_alerts"), State};
handle_call(alerts_for_today, State) ->
    Day = day_key(),
    Value = case folsom_metrics:metric_exists(Day) of
                false -> folsom_metrics:new_counter(Day), 0;
                true  -> folsom_metrics:get_metric_value(Day)
            end,
    {ok, Value, State};
handle_call({alerts, Count}, State) ->
    Alerts =
        [   case ets:lookup(triage_error_data, Counter) of
                [#alert{} = Alert] -> Alert;
                _ -> #alert{}
            end || {key, Counter} <- ets:tab2list(triage_error_keys), string:str(Counter, ":") =/= 0 ],
    Sorted = lists:keysort(#alert.timestamp, Alerts),
    FinalList = reverse_limit_and_filter(Sorted, Count),
    {ok, FinalList, State};
handle_call({clear, Counter}, State) ->
    Key = "recent:" ++ Counter,
    folsom_metrics:delete_metric(Key),
    folsom_metrics:new_counter(Key),
    dashboard_stream_fsm:broadcast({update_counters, [{counter, Counter}]}),
    {ok, ok, State};
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({triage_event, #popcorn_node{} = Node, Node_Pid,
              #log_message{log_module=Module, log_line=Line, severity=Severity} = Log_Entry,
              Is_New_Node}, State)
        when Severity < 4, is_binary(Module), is_binary(Line) ->
    true = ets:insert(triage_error_data, #alert{location=key(Module,Line), node=Node, log=Log_Entry}),
    case Is_New_Node of
        true -> dashboard_stream_fsm:broadcast({new_node, Node});
        false -> ok
    end,
    update_counter(Node,Node_Pid,Module,Line),
    {ok, State};

handle_event({triage_event, #popcorn_node{} = Node, _Node_Pid, _Log_Message, true}, State) ->
    dashboard_stream_fsm:broadcast({new_node, Node}),
    {ok, State};

handle_event({triage_event, #popcorn_node{}, _Node_Pid, _Log_Message, false}, State) ->
    {ok, State};

handle_event(Event, State) ->
    io:format("Unexpected event: ~p~n", [Event]),
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% TODO version number should be read here, or else we don't support upgrades
    {ok, State}.

update_counter(_, _, undefined, _) -> ok;
update_counter(_, _, _, undefined) -> ok;
update_counter(Node, Node_Pid, Module, Line) ->
    folsom_metrics:notify({"total_alerts", {inc, 1}}),
    Counter = key(Module,Line),
    Day = day_key(),
    case folsom_metrics:metric_exists(Day) of
        false -> new_metric(Day);
        true  -> ok
    end,
    case folsom_metrics:metric_exists(Counter) of
        false -> new_metric(Counter);
        true  -> ok
    end,
    folsom_metrics:notify({Day, {inc, 1}}),
    folsom_metrics:notify({Counter, {inc, 1}}),
    folsom_metrics:notify({"recent:" ++ Counter, {inc, 1}}),

    NewCounters =
        [   {node_hash,         re:replace(base64:encode(Node#popcorn_node.node_name), "=", "_", [{return, binary}, global])},
            {node_count,        case Node_Pid of
                                    undefined -> 0;
                                    Node_Pid ->
                                        proplists:get_value(total, gen_fsm:sync_send_event(Node_Pid, get_message_counts), 0)
                                end},
            {counter,           Counter},
            {event_count,       folsom_metrics:get_metric_value(?TOTAL_EVENT_COUNTER)},
            {alert_count_today, folsom_metrics:get_metric_value(Day)},
            {alert_count,       folsom_metrics:get_metric_value("total_alerts")}],

    dashboard_stream_fsm:broadcast({update_counters, NewCounters}).

new_metric(Counter) ->
    true = ets:insert(triage_error_keys, {key, Counter}),
    folsom_metrics:new_counter(Counter),
    folsom_metrics:new_counter("recent:" ++ Counter).

key(Module,Line) -> binary_to_list(Module) ++ ":" ++ binary_to_list(Line).

%% TODO: use the timestamp from the log
day_key() ->
        {{Year,Month,Day},_} = calendar:now_to_universal_time(erlang:now()),
        integer_to_list(Year) ++ "-" ++ integer_to_list(Month) ++ "-" ++ integer_to_list(Day).

data(#alert{location = undefined}) -> [];
data(Alert) ->
    Basic_Properties =
      case Alert of
        #alert{node = #popcorn_node{role = Role, version = Version},
               log  = #log_message{message = Message}} ->
          [ {'message', list(Message)}, {'product', list(Role)}, {'version', list(Version)}];
        #alert{node = #popcorn_node{version = Version}} ->
          [ {'version', list(Version)}];
        #alert{log  = #log_message{message = Message}} ->
          [ {'message', list(Message)}];
        #alert{} ->
          []
      end,
    All_Properties =
        case string:tokens(Alert#alert.location, ":") of
            [Counter_Name,Line|_] -> [{'name', Counter_Name}, {'line', Line} | Basic_Properties];
            _ -> Basic_Properties
        end,
    [{count, folsom_metrics:get_metric_value(Alert#alert.location)},
     {recent, folsom_metrics:get_metric_value("recent:" ++ Alert#alert.location)}
     | All_Properties].

list(B) when is_binary(B) -> binary_to_list(B);
list(_) -> "".

reverse_limit_and_filter(Alerts, all) -> [data(Alert) || Alert <- lists:reverse(Alerts)];
reverse_limit_and_filter(Alerts, Count) ->
    reverse_limit_and_filter(lists:reverse(Alerts), Count, []).
reverse_limit_and_filter([], _Count, Acc) -> lists:reverse(Acc);
reverse_limit_and_filter(_Alerts, Count, Acc) when length(Acc) == Count -> lists:reverse(Acc);
reverse_limit_and_filter([Alert | Alerts], Count, Acc) ->
    reverse_limit_and_filter(
        Alerts, Count,
        case folsom_metrics:get_metric_value("recent:" ++ Alert#alert.location) of
            0 -> Acc;
            _ -> [data(Alert) | Acc]
        end).