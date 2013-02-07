-module(view_generic).
-author('marc.e.campbell@gmail.com').

-include("include/popcorn.hrl").

-export([username/1]).

-spec username(dict()) -> string().
username(Context) ->
    mustache:get(username, Context).
