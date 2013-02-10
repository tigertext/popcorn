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
%%% File:      storage_monitor.erl
%%% @author    Marc Campbell <marc.e.campbell@gmail.com>
%%% @doc
%%% @end
%%%-----------------------------------------------------------------

-module(storage_monitor).
-author('marc.e.campbell@gmail.com').
-behavior(gen_server).

-include("include/popcorn.hrl").

-define(WORKER_HEALTH_INTERVAL, 10000).

-export([start_link/0,
         start_workers/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
start_workers() -> gen_server:cast(?MODULE, start_workers).

init([]) ->
    process_flag(trap_exit, true),

    ?POPCORN_DEBUG_MSG("#storage_monitor starting"),
    pg2:create('storage'),

    erlang:send_after(?WORKER_HEALTH_INTERVAL, self(), check_worker_health),

    {ok, 'not_ready'}.

handle_call(Request, _From, State)  -> {stop, {unknown_call, Request}, State}.

handle_cast(start_workers, 'not_ready') ->
    ?POPCORN_DEBUG_MSG("Starting storage workers..."),

    case popcorn_util:optional_env(track_rps, false) of
        false -> ok;
        true  -> gen_info:start_link(),
                 rps_sup:start_link([ [{name, storage}, {module, gen_info}, {time, 5000}, {send, stats}] ])
    end,

    %% pick one of the started workers and have it from the init phase
    ok = gen_server:call(pg2:get_closest_pid('storage'), start_phase),

    {noreply, 'ready'};

handle_cast(_Msg, State)            -> {noreply, State}.

handle_info(check_worker_health, State) ->
    _Num_Workers = length(pg2:get_local_members('storage')),
    %% TODO if works change use ets pubsub to let listeners know about the change
    erlang:send_after(?WORKER_HEALTH_INTERVAL, self(), check_worker_health),
    {noreply, State};

handle_info(_Msg, State)            -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


