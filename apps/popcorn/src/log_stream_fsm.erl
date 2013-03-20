-module(log_stream_fsm).
-author('marc.e.campbell@gmail.com').
-behavior(gen_fsm).

-include("include/popcorn.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(IDLE_DISCONNECT_TIMER,      5000).

-export([start_link/0]).

-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).


-export([
    'STARTING'/2,
    'STARTING'/3,
    'STREAMING'/2,
    'STREAMING'/3]).

-record(state, {stream                  :: #double_stream{},
                idle_loops_disconnected :: integer()}).

start_link() -> gen_fsm:start_link(?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),

    gen_fsm:start_timer(?IDLE_DISCONNECT_TIMER, idle_disconnect),

    {ok, 'STARTING', #state{idle_loops_disconnected = 0}}.

'STARTING'({connect, Stream}, State) ->
    %% add to the ets table
    ets:insert(current_log_streams, Stream),
    {next_state, 'STARTING', State#state{stream = Stream}};
'STARTING'({set_client_summary_pid, Pid}, State) ->
    Stream  = State#state.stream,
    Stream2 = lists:nth(1, ets:select(current_log_streams, ets:fun2ms(fun(Stream3) when Stream3#double_stream.stream_id =:= Stream#double_stream.stream_id -> Stream3 end))),
    Stream3 = Stream2#double_stream{client_summary_pid = Pid},
    ets:insert(current_log_streams, Stream3),

    %% let the log_stream_manager know of this pid
    log_stream_manager:add_stream_pid(self()),

    case {Stream3#double_stream.client_summary_pid, Stream3#double_stream.client_messages_pid} of
        {A, B} when is_pid(A) andalso is_pid(B) ->
            %% when the client pid changes, it's a reconnect or initial connection, so 
            %% we send some log lines down
            Stream3#double_stream.client_messages_pid ! clear_log,
            gen_fsm:send_event_after(0, init_log_lines),
            {next_state, 'STREAMING', State#state{stream = Stream3}};
        _ ->
            {next_state, 'STARTING', State#state{stream = Stream3}}
    end;

'STARTING'({set_client_messages_pid, Pid}, State) ->
    Stream  = State#state.stream,
    Stream2 = lists:nth(1, ets:select(current_log_streams, ets:fun2ms(fun(Stream3) when Stream3#double_stream.stream_id =:= Stream#double_stream.stream_id -> Stream3 end))),
    Stream3 = Stream2#double_stream{client_messages_pid = Pid},
    ets:insert(current_log_streams, Stream3),

    %% let the log_stream_manager know of this pid
    log_stream_manager:add_stream_pid(self()),

    case {Stream3#double_stream.client_summary_pid, Stream3#double_stream.client_messages_pid} of
        {A, B} when is_pid(A) andalso is_pid(B) ->
            %% when the client pid changes, it's a reconnect or initial connection, so 
            %% we send some log lines down
            Stream3#double_stream.client_messages_pid ! clear_log,
            gen_fsm:send_event_after(0, init_log_lines),
            {next_state, 'STREAMING', State#state{stream = Stream3}};
        _ ->
            {next_state, 'STARTING', State#state{stream = Stream3}}
    end;

'STARTING'({timeout, _From, idle_disconnect}, State) ->
    gen_fsm:start_timer(?IDLE_DISCONNECT_TIMER, idle_disconnect),
    {next_state, 'STREAMING', State#state{idle_loops_disconnected = 0}}.

'STARTING'(Other, _From, State) ->
    {noreply, undefined, 'STARTING', State}.

'STREAMING'({timeout, _From, idle_disconnect}, State) ->
    Is_Connected = (State#state.stream)#double_stream.client_summary_pid =/= undefined andalso
                   erlang:is_process_alive((State#state.stream)#double_stream.client_summary_pid),
    case Is_Connected of
        true ->  {next_state, 'STREAMING', State#state{idle_loops_disconnected = 0}};
        false -> case State#state.idle_loops_disconnected of
                     4 -> {stop, normal, State};
                     O -> gen_fsm:start_timer(?IDLE_DISCONNECT_TIMER, idle_disconnect),
                          {next_state, 'STREAMING', State#state{idle_loops_disconnected = O + 1}}
                 end
    end;

'STREAMING'({update_severities, New_Severities}, State) ->
    Severity_Filter = lists:map(fun(S) -> list_to_integer(S) end, string:tokens(binary_to_list(New_Severities), ",")),
    Stream          = State#state.stream,
    Applied_Filters = Stream#double_stream.applied_filters,

    Applied_Filters2 = case proplists:get_value('severities', Applied_Filters) of
                           undefined -> Applied_Filters ++ [{'severities', Severity_Filter}];
                           _         -> proplists:delete('severities', Applied_Filters) ++ [{'severities', Severity_Filter}]
                       end,

    update_filters(Applied_Filters2, Stream#double_stream.stream_id),

    Stream2 = Stream#double_stream{applied_filters = Applied_Filters2},

    Stream#double_stream.client_messages_pid ! clear_log,
    gen_fsm:send_event_after(0, init_log_lines),

    {next_state, 'STREAMING', State#state{stream = Stream2}};

'STREAMING'(init_log_lines, State) ->
    %% send some log lines, based on the current state,
    %% and update the state so we can use timestamps instead of select/4 in a transaction
    Stream = State#state.stream,
    gen_server:cast(?STORAGE_PID, {send_recent_matching_log_lines, self(), 500, lists:filter(fun({_, V}) -> V =/= undefined end, [])}),
    {next_state, 'STREAMING', State};

'STREAMING'(Other, State) ->
    {next_state, 'STREAMING', State}.

'STREAMING'(Other, _From, State) ->
    {noreply, undefined, 'STREAMING', State}.

handle_event({message, Log_Message, Popcorn_Node}, State_Name, State) ->
    Stream = State#state.stream,
    Should_Stream =
      Stream#double_stream.paused =:= false andalso
      is_pid(Stream#double_stream.client_messages_pid) andalso
      is_pid(Stream#double_stream.client_summary_pid) andalso
      is_filtered_out(Log_Message, Stream#double_stream.max_timestamp, Stream#double_stream.applied_filters) =:= false,

    case Should_Stream of
        false ->
            ok;
        true  ->
            Stream#double_stream.client_messages_pid ! {message, Log_Message, Popcorn_Node},
            Stream#double_stream.client_summary_pid ! {summary, Log_Message, Popcorn_Node}
    end,

    {next_state, State_Name, State};

handle_event(toggle_pause, State_Name, State) ->
    Stream = State#state.stream,
    New_Stream = Stream#double_stream{paused = not Stream#double_stream.paused},
    {next_state, State_Name, State#state{stream = New_Stream}};

handle_event(Event, StateName, State)                 -> {stop, {StateName, undefined_event, Event}, State}.

handle_sync_event(is_paused, _From, State_Name, State) ->
    Stream = State#state.stream,
    {reply, Stream#double_stream.paused, State_Name, State};

handle_sync_event(Event, _From, StateName, State)     -> {stop, {StateName, undefined_event, Event}, State}.
handle_info(_Info, StateName, State)                  -> {next_state, StateName, State}.
terminate(_Reason, _StateName, State)                 ->
    %% let the log_stream_manager remove this pid
    log_stream_manager:del_stream_pid(self()),

    Stream_Id = (State#state.stream)#double_stream.stream_id,
    1 = ets:select_delete(current_log_streams, ets:fun2ms(fun(#double_stream{stream_id = SID}) when SID =:= Stream_Id -> true end)),
    ok.
code_change(_OldVsn, StateName, StateData, _Extra)    -> {ok, StateName, StateData}.

update_filters(Applied_Filters, Stream_Id) ->
    Stream = lists:nth(1, ets:select(current_log_streams, ets:fun2ms(fun(Stream2) when Stream2#double_stream.stream_id =:= Stream_Id -> Stream2 end))),
    Stream2 = Stream#double_stream{applied_filters = Applied_Filters},
    ets:insert(current_log_streams, Stream2).

is_filtered_out(#log_message{log_nodename = Node_Name, severity = Severity, timestamp = Timestamp, log_product = Role} = Log_Message, Max_Timestamp, Filters) ->
    is_severity_restricted(Severity, proplists:get_value('severities', Filters, [])).

is_severity_restricted(Severity, Allowed_Severities) ->
    not lists:member(Severity, Allowed_Severities).
