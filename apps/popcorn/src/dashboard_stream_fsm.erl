-module(dashboard_stream_fsm).
-author('marc.e.campbell@gmail.com').
-author('elbrujohalcon@inaka.net').

-behavior(gen_fsm).

-include("include/popcorn.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(IDLE_DISCONNECT_TIMER,      5000).

-export([start_link/0]).
-export([broadcast/1]).

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

-record(state, {stream                  :: #stream{},
                idle_loops_disconnected :: integer()}).

broadcast(Event) ->
    lists:foreach(
        fun(Dashboard_Stream) ->
            gen_fsm:send_all_state_event(Dashboard_Stream#stream.stream_pid, Event)
        end, ets:tab2list(current_dashboard_streams)).

start_link() -> gen_fsm:start_link(?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),

    gen_fsm:start_timer(?IDLE_DISCONNECT_TIMER, idle_disconnect),

    {ok, 'STARTING', #state{idle_loops_disconnected = 0}}.

'STARTING'({timeout, _From, idle_disconnect}, State) ->
    gen_fsm:start_timer(?IDLE_DISCONNECT_TIMER, idle_disconnect),
    case State#state.idle_loops_disconnected of
        4 ->
            error_logger:error_msg("Dashboard FSM disconnecting before streaming"),
            {stop, normal, State};
        O -> {next_state, 'STARTING', State#state{idle_loops_disconnected = O + 1}}
    end;
'STARTING'({connect, Stream}, State) ->
    %% add to the ets table
    ets:insert(current_dashboard_streams, Stream),

    {next_state, 'STARTING', State#state{stream = Stream}};
'STARTING'({set_client_pid, Pid}, State) ->
    Stream  = State#state.stream,
    Stream2 = lists:nth(
                1, ets:select(
                    current_dashboard_streams,
                    ets:fun2ms(
                        fun(Stream3) when Stream3#stream.stream_id =:= Stream#stream.stream_id -> Stream3 end))),
    Stream3 = Stream2#stream{client_pid = Pid},
    ets:insert(current_dashboard_streams, Stream3),

    {next_state, 'STREAMING', State#state{stream = Stream3}}.
'STARTING'(Other, _From, State) ->
    {noreply, undefined, 'STARTING', State}.

'STREAMING'({timeout, _From, idle_disconnect}, State) ->
    gen_fsm:start_timer(?IDLE_DISCONNECT_TIMER, idle_disconnect),
    Is_Connected = (State#state.stream)#stream.client_pid =/= undefined andalso
                   erlang:is_process_alive((State#state.stream)#stream.client_pid),
    case Is_Connected of
        true ->  {next_state, 'STREAMING', State#state{idle_loops_disconnected = 0}};
        false -> case State#state.idle_loops_disconnected of
                     4 -> {stop, normal, State};
                     O -> {next_state, 'STREAMING', State#state{idle_loops_disconnected = O + 1}}
                 end
    end;
'STREAMING'(Other, State) ->
    {next_state, 'STREAMING', State}.

'STREAMING'(Other, _From, State) ->
    {noreply, undefined, 'STREAMING', State}.

handle_event({_Kind, _Data} = Event, State_Name, State) ->
    Stream      = State#state.stream,

    case is_pid(Stream#stream.client_pid) of
        false -> ok;
        true  -> Stream#stream.client_pid ! Event
    end,

    {next_state, State_Name, State};
handle_event(Event, StateName, State)                 -> {stop, {StateName, undefined_event, Event}, State}.

handle_sync_event(Event, _From, StateName, State)     -> {stop, {StateName, undefined_event, Event}, State}.
handle_info(_Info, StateName, State)                  -> {next_state, StateName, State}.
terminate(_Reason, _StateName, State)                 ->
    Stream_Id = (State#state.stream)#stream.stream_id,
    1 = ets:select_delete(current_dashboard_streams, ets:fun2ms(fun(#stream{stream_id = SID}) when SID =:= Stream_Id -> true end)),
    ok.
code_change(_OldVsn, StateName, StateData, _Extra)    -> {ok, StateName, StateData}.