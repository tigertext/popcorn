-module(outbound_notifier_handler).
-author('elbrujohalcon@inaka.net').

-behaviour(gen_server).

-export([supervisor_spec/3, start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {trigger     :: atom(),
                mod         :: atom(),
                mod_state   :: term()}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-callback handler_name(InitArgs::term()) -> atom().
-callback init(InitArgs::term()) -> {ok, State::term()} | ignore | {stop, Reason::term()}.
-callback handle_event(Event::atom(), Data::term(), State::term()) -> {ok, State::term()} | {stop, Reason::term(), State::term()}.
-callback handle_call(Call::term(), State::term()) -> {ok, Reply::term(), State::term()} | {stop, Reason::term(), Reply::term(), State::term()}.
-callback handle_info(Info::term(), State::term()) -> {ok, State::term()} | {stop, Reason::term(), State::term()}.
-callback terminate(Reason::term(), State::term()) -> _.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec supervisor_spec(atom(), atom(), term()) -> supervisor:child_spec().
supervisor_spec(Trigger, Module, InitArgs) ->
    {process_name(Trigger, Module, InitArgs),
     {outbound_notifier_handler, start_link, [Trigger, Module, InitArgs]},
     transient, brutal_kill, worker, [outbound_notifier_handler, Module]}.

-spec start_link(atom(), atom(), term()) -> {ok, pid()}.
start_link(Trigger, Module, InitArgs) ->
    gen_server:start_link({local, process_name(Trigger, Module, InitArgs)}, ?MODULE, {Trigger, Module, InitArgs}, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Trigger, Module, InitArgs}) ->
    case Module:init(InitArgs) of
        {ok, State} ->
            ok = gen_event_caster:start(outbound_notifier, self()),
            {ok, #state{trigger = Trigger, mod = Module, mod_state = State}};
        ignore -> ignore;
        {stop, Reason} -> {stop, Reason}
    end.

%% trigger = '_' means 'any event'
handle_cast({Event, Data}, State = #state{trigger = '_'}) ->
    io:format("~p handled by ~p: ~p~n", [Event, State#state.mod, Data]),
    try (State#state.mod):handle_event(Event, Data, State#state.mod_state) of
        {ok, NewState} ->
            {noreply, State#state{mod_state = NewState}};
        {stop, Reason, NewState} ->
            {stop, Reason, State#state{mod_state = NewState}}
    catch
        _:{stop, Reason, NewState} ->
            {stop, Reason, State#state{mod_state = NewState}}
    end;
%% trigger = '[Event]' means 'that event'
handle_cast({Event, Data}, State = #state{trigger = Event}) ->
    io:format("~p handled by ~p: ~p~n", [Event, State#state.mod, Data]),
    try (State#state.mod):handle_event(Event, Data, State#state.mod_state) of
        {ok, NewState} ->
            {noreply, State#state{mod_state = NewState}};
        {stop, Reason, NewState} ->
            {stop, Reason, State#state{mod_state = NewState}}
    catch
        _:{stop, Reason, NewState} ->
            {stop, Reason, State#state{mod_state = NewState}}
    end;
handle_cast(_OtherEvent, State) ->
    {noreply, State}.

handle_call(Call, _From, State) ->
    try (State#state.mod):handle_event(Call, State#state.mod_state) of
        {ok, Reply, NewState} ->
            {reply, Reply, State#state{mod_state = NewState}};
        {stop, Reason, Reply, NewState} ->
            {stop, Reason, Reply, State#state{mod_state = NewState}}
    catch
        _:{stop, Reason, Reply, NewState} ->
            {stop, Reason, Reply, State#state{mod_state = NewState}}
    end.

handle_info(Info, State) ->
    try (State#state.mod):handle_info(Info, State#state.mod_state) of
        {ok, NewState} ->
            {noreply, State#state{mod_state = NewState}};
        {stop, Reason, NewState} ->
            {stop, Reason, State#state{mod_state = NewState}}
    catch
        _:{stop, Reason, NewState} ->
            {stop, Reason, State#state{mod_state = NewState}}
    end.

terminate(Reason, State) -> (State#state.mod):terminate(Reason, State#state.mod_state).

code_change(_, _, State) -> {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec process_name(atom(), atom(), term()) -> atom().
process_name(Trigger, Module, InitArgs) ->
    Name = Module:handler_name(InitArgs),
    list_to_atom(?MODULE_STRING ++ [$- | atom_to_list(Trigger)] ++ [$- | atom_to_list(Name)]).