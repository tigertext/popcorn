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
-module(http_popupmenu).
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

handle_path(<<"GET">>, [<<"popupmenu">>, <<"alert">>, Alert_Id], Req, State) ->
    Body = iolist_to_binary([<<"<html><body>">>,
                <<"<ul><li><a href=\"javascript:$.post('/clear_alert', {'alert': '">>, Alert_Id, <<"'});\">Clear Alert</a></li>">>,
                <<"<li><a href=\"/alert/">>, Alert_Id, <<"\">View Details</a></li></ul>">>,
                <<"</body></html>">>]),
    {ok, Reply} = cowboy_req:reply(200, [{<<"Content-type">>, <<"text/plain">>}], Body, Req),
    {ok, Reply, State};

handle_path(_, _, Req, State) ->
    {ok, Reply} = cowboy_req:reply(401, [], [], Req),
    {ok, Reply, State}.
