%%
%% run this from the console with the following command:
%%
%% eunit:test({timeout, 60, mnesia_performance_tests}).
%%

-module(mnesia_performance_tests).

-author('marc.e.campbell@gamil.com').

-export([insert_logmessages/1]).

-include("../include/popcorn.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {}).

mnesia_performance_test_() ->
    {foreach,
      fun setup/0,
      fun teardown/1,
       [
         fun insert_logmessages/1
       ]}.

setup() ->
    #state{}.

teardown(_Pid) ->
    ok.

insert_logmessages(State) ->
    fun() ->
        Number_Of_Messages = 100,

        lists:foreach(fun(Iteration) ->
              Log_Message = #log_message{message_id   = ?PU:unique_id(),
                                         timestamp    = ?NOW,
                                         severity     = 128,
                                         message      = <<"Test message">>,
                                         topics       = [<<"topic1">>, <<"topic2">>],
                                         identities   = [<<"user1">>, <<"session1">>],
                                         log_nodename = <<"mnesiatest@localhost">>,
                                         log_product  = <<"popcorn">>,
                                         log_version  = <<"1.0.32">>,
                                         log_module   = <<"mnesia_performance_tests">>,
                                         log_function = <<"insert_logmessages">>,
                                         log_line     = 99,
                                         log_pid      = self()},
             {T, _} = timer:tc(fun() -> mnesia:dirty_write(popcorn_history, Log_Message) end, []),
             ?POPCORN_DEBUG_MSG("~p, ~p", [Iteration, T])
          end, lists:seq(1, Number_Of_Messages))
    end.

