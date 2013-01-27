%%
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
-module(http_api_handler).
-author('mhald@mac.com').

-behavior(cowboy_http_handler).

-include("include/popcorn.hrl").

-export([init/3,
         handle/2,
         terminate/2]).

init({_Any, http}, Req, _) -> {ok, Req, undefined_state}.

handle(Req, State) ->
    {Path, Req1}   = cowboy_req:path(Req),
    Path_Parts     = lists:filter(fun(<<>>) -> false; (_) -> true end, binary:split(Path, <<"/">>, [global])),
    {Method, Req2} = cowboy_req:method(Req1),
    handle_path(Method, Path_Parts, Req2, State).

terminate(_Req, _State) -> ok.

handle_path(<<"POST">>, [<<"api">>, <<"mapping">>], Req, State) ->
    {ok, Post, Req2} = cowboy_req:body(Req),
    JSON = jiffy:decode(Post),
    Role = json_util:get_path(JSON, [<<"sender">>, <<"role">>]),
    Version = json_util:get_path(JSON, [<<"sender">>, <<"version">>]),
    {Map} = json_util:get_path(JSON, [<<"mapping">>], []),
    Storage = pg2:get_closest_pid('storage'),
    [begin
        Record_Key = iolist_to_binary([Role, $:, Version, $:, Module]),
        gen_server:cast(Storage, {new_release_scm_mapping, #release_scm_mapping{key=Record_Key, role=Role, version=Version, module_name=Module, url=Url}}),
        io:format("K ~p ~p ~p ~p=~p~n", [Record_Key, Role, Version, Module, Url])
     end || {Module, Url} <- Map],
    {ok, Reply} = cowboy_req:reply(200, [], [], Req2),
    {ok, Reply, State};

handle_path(_, _, Req, State) ->
    {ok, Reply} = cowboy_req:reply(401, [], [], Req),
    {ok, Reply, State}.
