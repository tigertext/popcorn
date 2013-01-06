-module(popcorn_udp_tests).

-author('marc.e.campbell@gmail.com').

-include_lib("eunit/include/eunit.hrl").

popcorn_udp_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
          [
            fun receive_message/1
          ]}.

setup() -> ok.

teardown(_) -> ok.

receive_message(_) ->
    fun() ->
        ok
    end.
