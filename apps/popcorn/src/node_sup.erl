%%%
%% Copyright 2012
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

%%%-------------------------------------------------------------------
%%% @author Martin Hald
%%% @copyright (C) 2013
%%% @doc
%%%
%%% @end
%%% Created : Sun Feb 10 2013
%%%-------------------------------------------------------------------
-module(node_sup).

-behaviour(supervisor).

-include("popcorn.hrl").

%% API
-export([start_link/0, add_child/1, monitor_node_process/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, []}}.

add_child(Node_Name) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    ?POPCORN_INFO_MSG("Launching node_fsm child ~p~n", [Node_Name]),
    {ok, Pid} = Child = supervisor:start_child(node_sup, 
                                               {list_to_atom(binary_to_list(Node_Name)), {node_fsm, start_link, []}, Restart, Shutdown, Type, [node_sup]}),
    spawn(?MODULE, monitor_node_process, [Pid]),
    Child.

%%%===================================================================
%%% Internal functions
%%%===================================================================

monitor_node_process(Proc) ->
    erlang:monitor(process,Proc),
    receive
       {'DOWN', _Ref, process, _Pid,  normal} -> ok;
       {'DOWN', Ref, process, Pid,  Reason} ->
           % TODO: alert gen_servers that have cached which nodes are launched to flush their cache
           ?POPCORN_ERROR_MSG("~p said that ~p died by unnatural causes~n~p",[Ref,Pid,Reason])
    end.

