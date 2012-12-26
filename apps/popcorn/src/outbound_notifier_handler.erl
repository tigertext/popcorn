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

handle_cast({Trigger, Data}, State = #state{trigger = Trigger}) ->
    io:format("~p handled by ~p: ~p~n", [Trigger, State#state.mod, Data]),
    {noreply, State};
handle_cast(_OtherEvent, State) ->
    {noreply, State}.

handle_call(_,_,State)      -> {reply, ok, State}.

handle_info(_Msg, State)    -> {noreply, State}.

code_change(_, _, State)    -> {noreply, State}.

terminate(_, _)             -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec process_name(atom(), atom(), term()) -> atom().
process_name(Trigger, Module, InitArgs) ->
    Name = Module:handler_name(InitArgs),
    list_to_atom(?MODULE_STRING ++ [$- | atom_to_list(Trigger)] ++ [$- | atom_to_list(Name)]).