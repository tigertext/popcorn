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

-export([counter_data/1]).

-include_lib("lager/include/lager.hrl").
-include("include/popcorn.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {}).

counter_data(Counter) ->
    Basic_Properties =
      case gen_event:call(triage_handler, triage_handler, {data, Counter}) of
        #alert{node = #popcorn_node{version = Version},
               log  = #log_message{message = Message}} ->
          [ {'message', list(Message)}, {'version', list(Version)}];
        #alert{node = #popcorn_node{version = Version}} ->
          [ {'version', list(Version)}];
        #alert{log  = #log_message{message = Message}} ->
          [ {'message', list(Message)}];
        #alert{} ->
          []
      end,
    case string:tokens(Counter, ":") of
        [Counter_Name,Line|_] -> [{'name', Counter_Name}, {'line', Line} | Basic_Properties];
        _ -> Basic_Properties
    end.

list(B) when is_binary(B) -> binary_to_list(B);
list(_) -> "".

init(_) ->
    ets:new(triage_error_keys, [named_table, set, public, {keypos, 2}]),
    ets:new(triage_error_data, [named_table, set, public, {keypos, #alert.location}]),
    folsom_metrics:new_counter("total_alerts"),
    {ok, #state{}}.

%% gen_event:call(triage_handler, triage_handler, {count, "tts_sup:42"}).
handle_call({count, Counter}, State) ->
    {ok, folsom_metrics:get_metric_value(Counter), State};

%% gen_event:call(triage_handler, triage_handler, {total_alerts}).
handle_call({total_alerts}, State) ->
    {ok, folsom_metrics:get_metric_value("total_alerts"), State};

%% gen_event:call(triage_handler, triage_handler, {alerts_for_today}).
handle_call({alerts_for_today}, State) ->
    Day = day_key(),
    Value = case folsom_metrics:metric_exists(Day) of
        false -> folsom_metrics:new_counter(Day), 0;
        true  -> folsom_metrics:get_metric_value(Day)
    end,
    {ok, Value, State};

handle_call({data, Counter}, State) ->
    Match = ets:fun2ms(fun(#alert{location=Location} = Alert) when Location =:= Counter -> Alert end),
    V = case ets:select(triage_error_data, Match) of
        [#alert{} = Alert] -> Alert;
        _ -> #alert{}
    end,
    {ok, V, State};

handle_call({alerts}, State) ->
    Alerts = [begin
                {Counter, folsom_metrics:get_metric_value(Counter)}
              end || {key, Counter} <- ets:tab2list(triage_error_keys)],
    {ok, lists:sort(fun({_,A}, {_,B}) -> A > B end, Alerts), State};

handle_call(_Request, State) ->
    {ok, ok, State}.

%% popcorn_udp:handle_info
%% node_fsm
handle_event({triage_event, #popcorn_node{} = Node,
              #log_message{log_module=Module, log_line=Line, severity=Severity},
              Is_New_Node} = Log_Entry, State)
        when Severity < 4, is_binary(Module), is_binary(Line) ->
    true = ets:insert(triage_error_data, #alert{location=key(Module,Line), node=Node, log=Log_Entry}),
    case Is_New_Node of
        true -> dashboard_stream_fsm:broadcast({new_node, Node});
        false -> ok
    end,
    update_counter(Module,Line),
    {ok, State};

handle_event({triage_event, #popcorn_node{} = Node, true}, State) ->
    dashboard_stream_fsm:broadcast({new_node, Node}),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% TODO version number should be read here, or else we don't support upgrades
    {ok, State}.

update_counter(undefined, _) -> ok;
update_counter(_, undefined) -> ok;
update_counter(Module,Line) ->
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

    NewCounters =
        [   {counter,           list_to_binary(Counter)},
            {counter_count,     folsom_metrics:get_metric_value(Counter)},
            {event_count,       folsom_metrics:get_metric_value(?TOTAL_EVENT_COUNTER)},
            {alert_count_today, folsom_metrics:get_metric_value(Day)},
            {alert_count,       folsom_metrics:get_metric_value("total_alerts")}],

    dashboard_stream_fsm:broadcast({update_counters, NewCounters}).

new_metric(Counter) ->
    true = ets:insert(triage_error_keys, {key, Counter}),
    folsom_metrics:new_counter(Counter),
    dashboard_stream_fsm:broadcast({new_metric, Counter}).

key(Module,Line) -> binary_to_list(Module) ++ ":" ++ binary_to_list(Line).

%% TODO: use the timestamp from the log
day_key() ->
        {{Year,Month,Day},_} = calendar:now_to_universal_time(erlang:now()),
        integer_to_list(Year) ++ "-" ++ integer_to_list(Month) ++ "-" ++ integer_to_list(Day).
