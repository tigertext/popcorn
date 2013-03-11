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
%%% File:      log_stream_manager.erl
%%% @author    Marc Campbell <marc.e.campbell@gmail.com>
%%% @doc
%%% @end
%%%-----------------------------------------------------------------

-module(log_stream_manager).
-author('marc.e.campbell@gmail.com').
-behavior(gen_server).

-include("include/popcorn.hrl").

-export([start_link/0,
         add_stream_pid/1,
         del_stream_pid/1,
         new_log_message/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {current_stream_pids :: list()}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add_stream_pid(pid()) -> ok.
add_stream_pid(Pid) ->
    gen_server:cast(?MODULE, {add_stream_pid, Pid}),
    ok.

-spec del_stream_pid(pid()) -> ok.
del_stream_pid(Pid) ->
    gen_server:cast(?MODULE, {del_stream_pid, Pid}),
    ok.

-spec new_log_message(#log_message{}, #popcorn_node{}) -> ok.
new_log_message(Log_Message, Popcorn_Node) ->
    gen_server:cast(?MODULE, {new_log_message, Log_Message, Popcorn_Node}),
    ok.

init([]) ->
    process_flag(trap_exit, true),

    ?POPCORN_DEBUG_MSG("#log_stream_manager starting"),

    {ok, #state{current_stream_pids = []}}.

handle_call(Request, _From, State)  -> {stop, {unknown_call, Request}, State}.

handle_cast({add_stream_pid, Pid}, State) ->
    case lists:member(Pid, State#state.current_stream_pids) of
        false ->
            {noreply, State#state{current_stream_pids = lists:append(State#state.current_stream_pids, [Pid])}};
        true ->
            {noreply, State}
    end;

handle_cast({del_stream_pid, Pid}, State) ->
    Stream_Pids  = State#state.current_stream_pids,
    Updated_Pids = lists:filter(fun(Stream_Pid) -> Stream_Pid =/= Pid end, Stream_Pids),

    {noreply, State#state{current_stream_pids = Updated_Pids}};

handle_cast({new_log_message, Log_Message, Popcorn_Node}, State) ->
    [gen_fsm:send_all_state_event(Pid, {message, Log_Message, Popcorn_Node}) || Pid <- State#state.current_stream_pids],
    {noreply, State};

handle_cast(_Msg, State)            -> {noreply, State}.
handle_info(_Msg, State)            -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


