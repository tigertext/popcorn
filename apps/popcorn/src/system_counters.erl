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
         decrement/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {counters :: list()}).


start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
increment(Which_Counter, Increment_By) -> gen_server:cast(?MODULE, {increment, Which_Counter, Increment_By}).
decrement(Which_Counter, Decrement_By) -> gen_server:cast(?MODULE, {decrement, Which_Counter, Decrement_By}).

init([]) ->
    process_flag(trap_exit, true),

    ?POPCORN_DEBUG_MSG("#system_counters starting"),

    erlang:send_after(?COUNTER_WRITE_INTERVAL, self(), write_counter),

    {ok, #state{counters = []}}.

handle_call(Request, _From, State)  -> {stop, {unknown_call, Request}, State}.

handle_cast({increment, Counter, V}, State) ->
    Current_Value = proplists:get_value(Counter, State#state.counters, 0),
    Counters = proplists:delete(Counter, State#state.counters) ++ [{Counter, Current_Value + V}],
    {noreply, State#state{counters = Counters}};
handle_cast({decrement, Counter, V}, State) ->
    Current_Value = proplists:get_value(Counter, State#state.counters, 0),
    Counters = proplists:delete(Counter, State#state.counters) ++ [{Counter, Current_Value - V}],
    {noreply, State#state{counters = Counters}};

handle_cast(_Msg, State)            -> {noreply, State}.

handle_info(write_counter, State) ->
    [gen_server:cast(?STORAGE_PID, {increment_counter, Counter, Value}) || {Counter, Value} <- State#state.counters],

    erlang:send_after(?COUNTER_WRITE_INTERVAL, self(), write_counter),

    {noreply, State#state{counters = []}};

handle_info(_Msg, State)            -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


