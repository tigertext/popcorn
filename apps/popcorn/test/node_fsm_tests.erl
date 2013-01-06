-module(node_fsm_tests).

-author('marc.e.campbell@gamil.com').

-export([set_node/1,
         log_message/1]).

-include("../include/popcorn.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {popcorn_node      :: #popcorn_node{},
                fsm_pid           :: pid()}).

visit_fsm_test_() ->
    {foreach,
      fun setup/0,
      fun teardown/1,
       [
         fun set_node/1,
         fun log_message/1
       ]}.

setup() ->
    %% Create a new node, with a random name, we can assume this will not be present
    AllowedChars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
    random:seed(now()),
    Random_Node_Name = lists:foldl(fun(_, Acc) ->
        [lists:nth(random:uniform(length(AllowedChars)), AllowedChars)] ++ Acc
       end, [], lists:seq(1, 12)),

    Popcorn_Node = #popcorn_node{node_name = list_to_binary(Random_Node_Name),
                                 role      = <<"Test">>,
                                 version   = <<"1.0.0">>},

    {ok, Pid} = supervisor:start_child(node_sup, []),

    #state{popcorn_node = Popcorn_Node,
           fsm_pid      = Pid}.

teardown(_Pid) ->
    ok.

set_node(State) ->
    fun() ->
        Popcorn_Node = State#state.popcorn_node,

        %% There should not be an entry for this yet
        Existing_Node = mnesia:dirty_read(known_nodes, Popcorn_Node#popcorn_node.node_name),
        ?assertEqual([], Existing_Node),

        %% Set the node name
        ok = gen_fsm:sync_send_event(State#state.fsm_pid, {set_popcorn_node, State#state.popcorn_node}),

        %% There should be an entry now
        Created_Nodes = mnesia:dirty_read(known_nodes, Popcorn_Node#popcorn_node.node_name),
        ?assertEqual(1, length(Created_Nodes)),
        Created_Node = lists:nth(1, Created_Nodes),
        ?assertEqual(Popcorn_Node#popcorn_node.node_name, Created_Node#popcorn_node.node_name)
    end.

log_message(Pid) ->
    fun() ->
        ok
    end.


