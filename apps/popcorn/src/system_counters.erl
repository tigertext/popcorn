%%%
%%% Copyright 2012
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%


%%%-------------------------------------------------------------------
%%% File:      system_counters.erl
%%% @author    Marc Campbell <marc.e.campbell@gmail.com>
%%% @doc
%%% @end
%%%-----------------------------------------------------------------

-module(system_counters).
-author('marc.e.campbell@gmail.com').
-behavior(gen_server).

-include("include/popcorn.hrl").

-define(COUNTER_WRITE_INTERVAL, 1000).

-export([start_link/0,
         increment/2,
         decrement/2,
         has_entries_for_severity/1,
         get_severity_counters/0,
         set_severity_counters/1,
         increment_severity_counter/1,
         counter_value/1,
         reset_interval/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {cached_counters     :: list(),
                severity_counters   :: list(),
                dirty_counters      :: list(),
                rps_enabled         :: boolean()}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
increment(Which_Counter, Increment_By) -> gen_server:cast(?MODULE, {increment, Which_Counter, Increment_By}).
decrement(Which_Counter, Decrement_By) -> gen_server:cast(?MODULE, {decrement, Which_Counter, Decrement_By}).
has_entries_for_severity(Severity_Num) -> gen_server:call(?MODULE, {has_entries_for_severity, Severity_Num}).
get_severity_counters() -> gen_server:call(?MODULE, get_severity_counters).
set_severity_counters(Severity_Counters) -> gen_server:cast(?MODULE, {set_severity_counters, Severity_Counters}).
increment_severity_counter(Severity_Num) -> gen_server:cast(?MODULE, {increment_severity_counter, Severity_Num}).
decrement_severity_counter(Severity_Num) -> gen_server:cast(?MODULE, {decrement_severity_counter, Severity_Num}).
counter_value(Counter) -> gen_server:call(?MODULE, {read_through_cache_counter_value, Counter}).
reset_interval() -> gen_server:cast(?MODULE, reset_interval).

init([]) ->
    process_flag(trap_exit, true),

    ?POPCORN_DEBUG_MSG("#system_counters starting"),

    %erlang:send_after(?COUNTER_WRITE_INTERVAL, self(), write_counter),

    {ok, #state{cached_counters   = [],
                dirty_counters    = [],
                severity_counters = []}}.  %% severity counters is set in the deserialize start_phase

handle_call({has_entries_for_severity, Severity_Num}, _From, State) ->
    case proplists:lookup(Severity_Num, State#state.severity_counters) of
        undefined -> {reply, false, State};
        0         -> {reply, false, State};
        _         -> {reply, true,  State}
    end;

handle_call(get_severity_counters, _From, State) ->
    {reply, State#state.severity_counters, State};

handle_call({read_through_cache_counter_value, Counter}, _From, State) ->
    case proplists:get_value(Counter, State#state.cached_counters) of
        undefined ->
            Counter_Value = gen_server:call(?STORAGE_PID, {counter_value, Counter}),
            {reply, Counter_Value, State#state{cached_counters = State#state.cached_counters ++ [{Counter, Counter_Value}]}};
        Counter_Value ->
            {reply, Counter_Value, State}
    end;

handle_call(Request, _From, State)  ->
    {stop, {unknown_call, Request}, State}.

handle_cast({set_severity_counters, Severity_Counters}, State) ->
    {noreply, State#state{severity_counters = Severity_Counters}};

handle_cast({increment, Counter, V}, State) ->
    Current_Dirty_Value  = proplists:get_value(Counter, State#state.dirty_counters, 0),
    Current_Cached_Value = proplists:get_value(Counter, State#state.cached_counters, 0),

    Dirty_Counters  = proplists:delete(Counter, State#state.dirty_counters) ++ [{Counter, Current_Dirty_Value + V}],
    Cached_Counters = proplists:delete(Counter, State#state.cached_counters) ++ [{Counter, Current_Cached_Value + V}],

    {noreply, State#state{dirty_counters  = Dirty_Counters,
                          cached_counters = Cached_Counters}};
handle_cast({decrement, Counter, V}, State) ->
    Current_Dirty_Value  = proplists:get_value(Counter, State#state.dirty_counters, 0),
    Current_Cached_Value = proplists:get_value(Counter, State#state.cached_counters, 0),

    Dirty_Counters  = proplists:delete(Counter, State#state.dirty_counters) ++ [{Counter, Current_Dirty_Value - V}],
    Cached_Counters = proplists:delete(Counter, State#state.cached_counters) ++ [{Counter, Current_Cached_Value - V}],

    {noreply, State#state{dirty_counters  = Dirty_Counters,
                          cached_counters = Cached_Counters}};

handle_cast({increment_severity_counter, Severity_Num}, State) ->
    Count = proplists:get_value(Severity_Num, State#state.severity_counters, 0),
    {noreply, State#state{severity_counters = proplists:delete(Severity_Num, State#state.severity_counters) ++ [{Severity_Num, Count + 1}]}};

handle_cast({decrement_severity_counter, Severity_Num}, State) ->
    Count = proplists:get_value(Severity_Num, State#state.severity_counters, 0),
    {noreply, State#state{severity_counters = proplists:delete(Severity_Num, State#state.severity_counters) ++ [{Severity_Num, Count - 1}]}};

handle_cast(reset_interval, State) ->
    erlang:send_after(?COUNTER_WRITE_INTERVAL, self(), write_counter),
    {noreply, State};

handle_cast(_Msg, State)            -> {noreply, State}.

handle_info(write_counter, State) ->
    gen_server:cast(?STORAGE_PID, {increment_counters, State#state.dirty_counters}),
    {noreply, State#state{dirty_counters = []}};

handle_info(_Msg, State)            -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


