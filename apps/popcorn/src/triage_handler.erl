-module(triage_handler).
-author('martin@tigertext.com').
-behaviour(gen_event).

-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3
]).

-include_lib("lager/include/lager.hrl").
-include("include/popcorn.hrl").

-record(state, {}).

init(_) ->
    ets:new(triage_error_keys, [named_table, set, public, {keypos, #log_stream.stream_id}]),
    {ok, #state{}}.

%% gen_event:call(triage_handler, triage_handler, {count, "tts_sup:42"}).
handle_call({count, Counter}, State) ->
    {ok, folsom_metrics:get_metric_value(Counter), State};

%% gen_event:call(triage_handler, triage_handler, {alerts}).
handle_call({alerts}, State) ->
    Alerts = [begin 
                {Counter, folsom_metrics:get_metric_value(Counter)}
              end || {key, Counter} <- ets:tab2list(triage_error_keys)],
    {ok, lists:sort(fun({_,A}, {_,B}) -> A > B end, Alerts), State};

handle_call(_Request, State) ->
    {ok, ok, State}.

%% popcorn_udp:handle_info
%% node_fsm
handle_event({triage_event, #popcorn_node{}, #log_message{log_module=Module, log_line=Line}}, State) ->
    update_counter(Module,Line),
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
    Counter = key(Module,Line),
    case folsom_metrics:metric_exists(Counter) of
        false -> new_metric(Counter);
        true  -> ok
    end,
    folsom_metrics:notify({Counter, {inc, 1}}).

new_metric(Counter) ->
    true = ets:insert(triage_error_keys, {key, Counter}),
    folsom_metrics:new_counter(Counter).

key(Module,Line) -> binary_to_list(Module) ++ ":" ++ binary_to_list(Line).
