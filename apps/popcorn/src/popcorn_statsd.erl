-module(popcorn_statsd).
-author("Dominik Liebler").

%% this is based heavily on https://github.com/domnikl/statsd-erlang

%%
%% we have a 2 fundamentally different types of stats collections:
%%  1. statsd.  calling increment/decrement/count/timing will
%%     send the message to statsd
%%  2. counters.  calling increment_counter will increment the counter
%%     and also increment statsd
%%

-behavior(gen_server).

-include("include/popcorn.hrl").

-export([start_link/1]).

-export([increment/1,
         increment/2,
         decrement/1,
         decrement/2,
         count/2,
         count/3,
         timing/2,
         timing/3,
			 	 increment_counter/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(STATSD_DEFAULT_PORT, 8125).
-define(STATSD_DEFAULT_HOST, "localhost").

%% holds all the relevant state that must be passed to all functions
-record(state, { 
	port = ?STATSD_DEFAULT_PORT,
	host = ?STATSD_DEFAULT_HOST,
	socket
}).

-spec increment(string()) 									-> no_return().
-spec increment(string(), number()) 				-> no_return().
-spec decrement(string()) 									-> no_return().
-spec decrement(string(), number()) 				-> no_return().
-spec count(string(), number()) 						-> no_return().
-spec count(string(), number(), number()) 	-> no_return().
-spec timing(string(), number()) 						-> no_return().
-spec timing(string(), number(), number()) 	-> no_return().
-spec increment_counter(string()) 				  -> no_return().

start_link(Params) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).


%% Public: increments a counter by 1
%% 
%% returns ok or {error, Reason}
increment(Key, Samplerate) -> count(Key, 1, Samplerate).
increment(Key)             -> count(Key, 1).
	
%% Public: decrements a counter by 1
%% 
%% returns ok or {error, Reason}
decrement(Key, Samplerate) -> count(Key, -1, Samplerate).
decrement(Key)             -> count(Key, -1).

%% Public: increments a counter by an arbitrary integer value
%%
%% returns: ok or {error, Reason}
count(Key, Value)             -> gen_server:cast(?MODULE, {send, build_message({message, Key, Value, c})}).
count(Key, Value, Samplerate) -> gen_server:cast(?MODULE, {send, build_message({message, Key, Value, c, Samplerate})}).

%% Public: sends a timing in ms
%%
%% returns: ok or {error, Reason}
timing(Key, Value) when is_float(Value) -> gen_server:cast(?MODULE, {send, build_message({message, Key, integer_to_list(round(Value)), ms})});
timing(Key, Value)                      -> gen_server:cast(?MODULE, {send, build_message({message, Key, integer_to_list(Value), ms})}).
timing(Key, Value, Samplerate)          -> gen_server:cast(?MODULE, {send, build_message({message, Key, integer_to_list(Value), ms, Samplerate})}).

%% Public: increments a counter by 1 (see note above)
%%
%% returns: ok or {error, Reason}
increment_counter(Key) -> gen_server:cast(?MODULE, {send, build_message({message, Key, 1, c})}),
													gen_server:cast(?MODULE, {increment_counter, Key}).
%% Internal: builds the message string to be sent
%% 
%% returns: a String	
build_message({message, Key, Value, Type})             -> [Key, ":", Value, "|", atom_to_list(Type)];
build_message({message, Key, Value, Type, Samplerate}) -> [build_message({message, Key, Value, atom_to_list(Type)}), "@", io:format("~.2f", 1.0 / Samplerate)].
		
init(Params) ->
    process_flag(trap_exit, true),

    Host = proplists:get_value(statsd_host, Params),
    Port = proplists:get_value(statsd_port, Params),

    State = #state{port = Port,
                   host = Host},
    {ok, Socket} = gen_udp:open(0, [list]),
    {ok, State#state{socket = Socket}}.

handle_call(Request, _From, State)  -> {stop, {unknown_call, Request}, State}.

handle_cast({send, Message}, State) ->
    gen_udp:send(State#state.socket, State#state.host, State#state.port, lists:flatten(Message)),
    {noreply, State};
handle_cast({increment_counter, Key}, State) ->
		%% we need to send this to humbledb
		%humbledb:increment_counter(Key),
		{noreply, State};

handle_cast(_Msg, State)            -> {noreply, State}.
handle_info(_Msg, State)            -> {noreply, State}.
terminate(_Reason, State)           -> gen_udp:close(State#state.socket).
code_change(_OldVsn, State, _Extra) -> {ok, State}.




