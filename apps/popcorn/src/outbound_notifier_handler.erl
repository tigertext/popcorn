-module(outbound_notifier_handler).
-author('elbrujohalcon@inaka.net').

-include("popcorn.hrl").

-behaviour(gen_server).

-export([supervisor_spec/3, start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {trigger     :: atom(),
                mod         :: atom(),
                mod_state   :: term(),
                trigger_data:: term()}).

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
            TriggerData =
                case Trigger of
                    Atom when is_atom(Atom) -> undefined;
                    {repeat, _Count, Lapse} ->
                        erlang:send_after(Lapse * 1000, self(), reset_counters),
                        ets:new(trigger_data, [set, private, {keypos, 1}, {write_concurrency, true}])
                end,
            {ok, #state{trigger = Trigger, mod = Module, mod_state = State, trigger_data = TriggerData}};
        ignore -> ignore;
        {stop, Reason} -> {stop, Reason}
    end.

%% trigger = '_' means 'any event'
handle_cast({Event, Data}, State = #state{trigger = '_'}) ->
    handle_event(Event, Data, State);
%% trigger = '{repeat, [Count], [Lapse]}' means 'more than Count events for the same alert in Lapse seconds'
handle_cast({new_event, Data}, State = #state{trigger = {repeat, Count, _Lapse}}) ->
    Alert = proplists:get_value(location, Data, ""),
    try ets:update_counter(State#state.trigger_data, Alert, {2,1,Count,1}) of
        1 -> handle_event(new_event, Data, State); %%NOTE: Threshold exceeded
        _ -> {noreply, State}
    catch
        _:badarg -> %%NOTE: Not yet registered
            ets:insert(State#state.trigger_data, {Alert, 1}),
            {noreply, State}
    end;
%% trigger = '[Event]' means 'that event'
handle_cast({Event, Data}, State = #state{trigger = Event}) ->
    handle_event(Event, Data, State);
handle_cast(_OtherEvent, State) ->
    {noreply, State}.

handle_event(Event, Data, State) ->
    ?POPCORN_DEBUG_MSG("~p handled by ~p: ~p~n", [Event, State#state.mod, Data]),
    try (State#state.mod):handle_event(Event, Data, State#state.mod_state) of
        {ok, NewState} ->
            {noreply, State#state{mod_state = NewState}};
        {stop, Reason, NewState} ->
            {stop, Reason, State#state{mod_state = NewState}}
    catch
        _:{stop, Reason, NewState} ->
            {stop, Reason, State#state{mod_state = NewState}}
    end.


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

handle_info(reset_counters, State = #state{trigger = {repeat, _Count, Lapse}}) ->
    true = ets:delete_all_objects(State#state.trigger_data),
    erlang:send_after(Lapse * 1000, self(), reset_counters),
    {noreply, State};
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
process_name({repeat,_,_}, Module, InitArgs) ->
    Name = Module:handler_name(InitArgs),
    list_to_atom(?MODULE_STRING ++ "-repeat-" ++ atom_to_list(Name));
process_name(Trigger, Module, InitArgs) ->
    Name = Module:handler_name(InitArgs),
    list_to_atom(?MODULE_STRING ++ [$- | atom_to_list(Trigger)] ++ [$- | atom_to_list(Name)]).
