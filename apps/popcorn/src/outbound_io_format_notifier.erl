-module(outbound_io_format_notifier).
-author('elbrujohalcon@inaka.net').

-behaviour(outbound_notifier_handler).

-record(state, {format :: string()}).
-opaque state() :: #state{}.

-export([handler_name/1, init/1, handle_event/3, handle_call/2, handle_info/2, terminate/2]).

-spec handler_name(InitArgs::string()) -> atom().
-spec init(InitArgs::string()) -> {ok, State::state()}.
-spec handle_event(Trigger::atom(), Data::term(), State::state()) -> {ok, State::state()}.
-spec handle_call(Call::term(), State::state()) -> {ok, ok, State::state()}.
-spec handle_info(Info::term(), State::state()) -> {ok, State::state()}.
-spec terminate(Reason::term(), State::state()) -> _.

handler_name(Format) -> list_to_atom(?MODULE_STRING ++ [$:|Format]).

init(Format) -> {ok, #state{format = Format}}.

handle_event(Trigger, Data, State) -> {io:format(State#state.format, [Trigger, Data]), State}.

handle_call(Call, State) -> {ok, io:format(State#state.format, ['CALL', Call]), State}.

handle_info(Info, State) -> {io:format(State#state.format, ['INFO', Info]), State}.

terminate(Reason, State) -> io:format(State#state.format, ['TERMINATE', Reason]).