-module(gen_event_caster).
-author('Fernando Benavides <elbrujohalcon@inaka.net>').

-behaviour(gen_event).

-export([start/2]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-include("utils.hrl").
-include("records.hrl").

-type receiver() :: atom() | pid().

-record(state, {receiver :: receiver()}).
-type state() :: #state{}.

%% ====================================================================
%% External functions
%% ====================================================================
-spec start(atom(), receiver()) -> ok.
start(Dispatcher, Receiver) ->
  try gen_event:add_handler(Dispatcher, {?MODULE, Receiver}, Receiver)
  catch
    _:Error ->
      io:format("gen_event:add_handler error:~n~p~n~p~n", [Error, erlang:get_stacktrace()]),
      throw(Error)
  end.

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init(receiver()) -> {ok, state()}.
init(Receiver) -> {ok, #state{receiver = Receiver}}.

%% @hidden
-spec handle_event(term(), state()) -> {ok, state()}.
handle_event(Event, State) ->
  ok = gen_server:cast(State#state.receiver, Event),
  {ok, State}.

%% @hidden
-spec handle_call(term(), state()) -> {ok, term(), state()}.
handle_call(Call, State) ->
  {ok, gen_server:call(State#state.receiver, Call), State}.

%% @hidden
-spec handle_info(term(), state()) -> {ok, state()} | remove_handler.
handle_info(Info, State) ->
  State#state.receiver ! Info,
  {ok, State}.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(Reason, State) ->
  io:format("Source dispatcher exited (~p). exiting ~p, who was listening...~n", [State#state.receiver]),
  exit(State#state.receiver, Reason).

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.