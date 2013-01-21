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
    lists:foreach(fun({Count, Date}) ->
        Date_Tokens = string:tokens(binary_to_list(Date), " "),
        Time_Tokens = string:tokens(lists:nth(5, Date_Tokens), ":"),
        Y  = lists:nth(4, Date_Tokens),
        Mo = month_to_int(lists:nth(3, Date_Tokens)),
        D  = lists:nth(2, Date_Tokens),
        H  = lists:nth(1, Time_Tokens),
        M  = lists:nth(2, Time_Tokens),
        S  = lists:nth(3, Time_Tokens),

        %% TODO extend the statsd module so that we can send "past" seconds of data
        %% until then, we ignore the date, so it's commented out, but parsed!
        popcorn_statsd:gauge("popcorn." ++ atom_to_list(Name), Count)
      end, Stats),
    {noreply, State};

handle_cast({stats, Name, Stats}, State) ->
    %%popcorn_statsd:increment("popcorn." ++ atom_to_list(Name)),
    {noreply, State};

handle_cast(_, State) -> {noreply, State}.

handle_info(_Msg, State)            -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

-spec month_to_int(string()) -> number().
month_to_int("Jan") -> 1;
month_to_int("Feb") -> 2;
month_to_int("Mar") -> 3;
month_to_int("Apr") -> 4;
month_to_int("May") -> 5;
month_to_int("Jun") -> 6;
month_to_int("Jul") -> 7;
month_to_int("Aug") -> 8;
month_to_int("Sep") -> 9;
month_to_int("Oct") -> 10;
month_to_int("Nov") -> 11;
month_to_int("Dec") -> 12.
