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
-include_lib("stdlib/include/ms_transform.hrl").

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
                                              {index,       [#log_message.severity,
                                                             #log_message.log_product,
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

handle_call({counter_value, Counter}, _From, State) ->
    Counter_Value =
      case mnesia:dirty_read(popcorn_counters, Counter) of
          [{popcorn_counters, _, V}] -> V;
          _ -> 0
      end,

    {reply, Counter_Value, State};

handle_call({is_known_node, Node_Name}, _From, State) ->
    ?RPS_INCREMENT(storage_total),
    {reply,
     mnesia:dirty_read(known_nodes, Node_Name) =/= [],
     State};

handle_call({search_messages, {P, V, M, L, Page_Size, Starting_Timestamp}}, _From, State) ->
    Messages =
      case mnesia:transaction(
                fun() ->
                    ?RPS_INCREMENT(storage_log_read),
                    ?RPS_INCREMENT(storage_total),
                    mnesia:select(
                        popcorn_history,
                        ets:fun2ms(
                            fun(#log_message{timestamp = TS, log_product = LP, log_version = LV, log_module = LM, log_line = LL} = Log_Message)
                                when LP == P, LV == V, LM == M, LL == L, (Starting_Timestamp == undefined orelse TS > Starting_Timestamp) -> Log_Message end),
                        Page_Size,
                        read)
                end) of
            {atomic, {Ms, _}} -> Ms;
            {atomic, '$end_of_table'} -> []
        end,

    %% sort the messages by date, newest first
    Sorted =
      lists:sort(fun(Message1, Message2) ->
          Message1#log_message.timestamp >= Message2#log_message.timestamp
        end, Messages),

    {reply, Sorted, State};

handle_call(Request, _From, State)  -> {stop, {unknown_call, Request}, State}.

handle_cast({expire_logs_matching, Severity, Timestamp}, State) ->
    delete_recent_log_line(Severity, Timestamp, undefined),
    {noreply, State};

handle_cast({send_recent_matching_log_lines, Pid, Count, Filters}, State) ->
    send_recent_log_line(Pid, Count, undefined, Filters),
    {noreply, State};

handle_cast({new_log_message, Log_Message}, State) ->
    ?RPS_INCREMENT(storage_log_write),
    ?RPS_INCREMENT(storage_total),
    mnesia:dirty_write(popcorn_history, Log_Message),
    {noreply, State};

handle_cast({delete_counter, Counter}, State) ->
    ?RPS_INCREMENT(storage_total),
    mnesia:dirty_delete(popcorn_counters, Counter),
    {noreply, State};

handle_cast({increment_counter, Counter, Increment_By}, State) ->
    ?RPS_INCREMENT(storage_counter_write),
    ?RPS_INCREMENT(storage_total),
    mnesia:dirty_update_counter(popcorn_counters, Counter, Increment_By),
    {noreply, State};

handle_cast({add_node, Popcorn_Node}, State) ->
    ?RPS_INCREMENT(storage_total),
    mnesia:dirty_write(known_nodes, Popcorn_Node),
    {noreply, State};

handle_cast(_Msg, State)            -> {noreply, State}.
handle_info(_Msg, State)            -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

send_recent_log_line(_, 0, _, _) -> ok;
send_recent_log_line(_, _, '$end_of_table', _) -> ok;
send_recent_log_line(Pid, Count, Last_Key_Checked, Filters) ->
    Key = case Last_Key_Checked of
              undefined -> ?RPS_INCREMENT(storage_total),
                           ?RPS_INCREMENT(storage_index_read),
                           mnesia:dirty_last(popcorn_history);
              _         -> ?RPS_INCREMENT(storage_total),
                           ?RPS_INCREMENT(storage_index_read),
                           mnesia:dirty_prev(popcorn_history, Last_Key_Checked)
          end,

    case Key of
        '$end_of_table' -> ok;
        _               -> ?RPS_INCREMENT(storage_log_read),
                           ?RPS_INCREMENT(stoage_total),
                           Log_Message = lists:nth(1, mnesia:dirty_read(popcorn_history, Key)),
                           case is_filtered_out(Log_Message, Filters) of
                               false -> gen_fsm:send_all_state_event(Pid, {new_message, older, Log_Message}),
                                        send_recent_log_line(Pid, Count - 1, Key, Filters);
                               true  -> send_recent_log_line(Pid, Count, Key, Filters)
                           end
    end.

delete_recent_log_line(_, '$end_of_table', _) -> ok;
delete_recent_log_line(Severity_Num, Oldest_Ts, Last_Key_Checked) ->
    Key = case Last_Key_Checked of
              undefined -> ?RPS_INCREMENT(storage_total),
                           ?RPS_INCREMENT(storage_index_read),
                           mnesia:dirty_last(popcorn_history);
              _         -> ?RPS_INCREMENT(storage_total),
                           ?RPS_INCREMENT(storage_index_read),
                           mnesia:dirty_prev(popcorn_history, Last_Key_Checked)
          end,

    case Key of
        '$end_of_table' -> ok;
        _               -> ?RPS_INCREMENT(storage_total),
                           ?RPS_INCREMENT(storage_log_read),
                           Log_Message = lists:nth(1, mnesia:dirty_read(popcorn_history, Key)),
                           case {Log_Message#log_message.severity, Log_Message#log_message.timestamp} of
                                {Severity_Num, TS} when TS < Oldest_Ts ->
                                    %% purge this and iterate
                                    ?RPS_INCREMENT(storage_total),
                                    mnesia:dirty_delete(popcorn_history, Log_Message#log_message.message_id),
                                    case ets:lookup(current_nodes, Log_Message#log_message.log_nodename) of
                                        Node_Pids when length(Node_Pids) =:= 1 ->
                                            {_, Node_Pid} = lists:nth(1, Node_Pids),
                                            gen_fsm:send_all_state_event(Node_Pid, decrement_counter);
                                        _ ->
                                            ok
                                    end,
                                    system_counters:decrement(total_event_counter, 1),
                                    delete_recent_log_line(Severity_Num, Oldest_Ts, Key);
                                {Severity_Num, _} ->
                                    %% stop iterating
                                    ok;
                                _ ->
                                    %% keep iterating
                                    delete_recent_log_line(Severity_Num, Oldest_Ts, Key)
                           end
    end.

%%
%% TODO this is duplicated from log_stream_fsm for now
%% we need to expose this so that other storage backend developers aren't required to implement
is_filtered_out(Log_Message, Filters) ->
    Severity_Restricted = not lists:member(Log_Message#log_message.severity, proplists:get_value('severities', Filters, [])),
    Time_Restricted = case proplists:get_value('max_timestamp', Filters) of
                          undefined       -> false;
                          Max_Timestamp   -> Log_Message#log_message.timestamp > Max_Timestamp
                      end,

    Severity_Restricted orelse Time_Restricted.


