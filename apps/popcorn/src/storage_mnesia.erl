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
%%% File:      storage_mnesia.erl
%%% @author    Marc Campbell <marc.e.campbell@gmail.com>
%%% @doc
%%% @end
%%%-----------------------------------------------------------------

-module(storage_mnesia).
-author('marc.e.campbell@gmail.com').
-behavior(gen_server).

-include("include/popcorn.hrl").

-export([start_link/0,
         pre_init/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link() -> gen_server:start_link(?MODULE, [], []).
pre_init() ->
    stopped = mnesia:stop(),
    case mnesia:create_schema([node()]) of
      ok -> io:format(" initializing schema...");
      {error, {_Node, {already_exists,_Node}}} -> io:format(" recovering schema...")
    end,
    ok = mnesia:start(),

    io:format("Ensuring required mnesia tables exist..."),
    io:format("\n\t[popcorn_history: ~p]",
       [mnesia:create_table(known_nodes,  [{disc_copies, [node()]},
                                           {record_name, popcorn_node},
                                           {attributes,  record_info(fields, popcorn_node)}])]),
    io:format("\n\t[popcorn_history: ~p]",
       [mnesia:create_table(popcorn_history, [{disc_copies, [node()]},
                                              {record_name, log_message},
                                              {type,        ordered_set},
                                              {index,       [#log_message.log_product,
                                                             #log_message.log_version,
                                                             #log_message.log_module,
                                                             #log_message.log_line,
                                                             #log_message.timestamp]},
                                              {attributes,  record_info(fields, log_message)}])]),
    io:format("\n\t[popcorn_counters: ~p]",
       [mnesia:create_table(popcorn_counters, [{disc_copies, [node()]}])]),
    io:format("\n... done!\n").

init([]) ->
    process_flag(trap_exit, true),

    pg2:join('storage', self()),
    {ok, undefined}.

handle_call(start_phase, _From, State) ->
    io:format("Reloading previously known nodes...\n"),
    lists:foreach(fun(Known_Node) ->
        io:format("Node: ~s\n", [binary_to_list(Known_Node)]),
        Popcorn_Node = lists:nth(1, mnesia:dirty_read(known_nodes, Known_Node)),
        {ok, Pid} = supervisor:start_child(node_sup, []),
        ok = gen_fsm:sync_send_event(Pid, {deserialize_popcorn_node, Popcorn_Node}),
        ets:insert(current_nodes, {Popcorn_Node#popcorn_node.node_name, Pid})
      end, mnesia:dirty_all_keys(known_nodes)),
    io:format(" done!\n"),

    io:format("Ensuring counters have a default value...\n"),
      io:format("\n\t[TOTAL_EVENT_COUNTER: ~p]",
        [mnesia:dirty_update_counter(popcorn_counters, ?TOTAL_EVENT_COUNTER, 0)]),
      io:format("\n\t[TOTAL_ALERT_COUNTER: ~p]",
        [mnesia:dirty_update_counter(popcorn_counters, ?TOTAL_ALERT_COUNTER, 0)]),
    io:format("\n done!\n"),

    {reply, ok, State};

handle_call(Request, _From, State)  -> {stop, {unknown_call, Request}, State}.

handle_cast({new_log_message, Log_Message}, State) ->
    mnesia:dirty_write(popcorn_history, Log_Message),
    {noreply, State};

handle_cast({increment_counter, Counter, Increment_By}, State) ->
    mnesia:dirty_update_counter(popcorn_counters, Counter, Increment_By),
    {noreply, State};

handle_cast(_Msg, State)            -> {noreply, State}.
handle_info(_Msg, State)            -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
