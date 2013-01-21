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
%%% File:      rps_statsd.erl
%%% @author    Marc Campbell <marc.e.campbell@gmail.com>
%%% @doc
%%% @end
%%%-----------------------------------------------------------------

-module(rps_statsd).
-author('marc.e.campbell@gmail.com').
-behavior(gen_server).

-include("include/popcorn.hrl").

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link(Params) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).

init(Params) ->
    ?POPCORN_DEBUG_MSG("#rps_statsd starting"),

    process_flag(trap_exit, true),

    {ok, 'not_ready'}.

handle_call(Request, _From, State)  -> {stop, {unknown_call, Request}, State}.

handle_cast({raw_stats, Name, Stats}, State) ->
    io:format("#rps_statsd (~p) ~p~n", [Name, Stats]),
    {noreply, State};

handle_cast({stats, Name, Stats}, State) ->
    %% popcorn_statsd:increment("popcorn." ++ atom_to_list(Name)),
    %% TODO send to statsd, but whats the value i should send... i think i need rps to make a counter
    %% or something.  
    {noreply, State};

handle_cast(_, State) -> {noreply, State}.

handle_info(_Msg, State)            -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


