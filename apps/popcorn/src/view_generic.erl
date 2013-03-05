-module(view_generic).
-author('marc.e.campbell@gmail.com').

-include("include/popcorn.hrl").

-export([username/1,
         avatar_path/1]).

-spec username(dict()) -> string().
username(Context) ->
    mustache:get(username, Context).

-spec avatar_path(dict()) -> string().
avatar_path(Context) ->
    "http://www.gravatar.com/avatar/" ++ popcorn_util:md5_hex(mustache:get(username, Context)).

