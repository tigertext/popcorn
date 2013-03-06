-module(view_node).
-author('marc.e.campbell@gmail.com').
-behaviour(view_generic).

-include("include/popcorn.hrl").

-export([username/1,
         avatar_path/1,
         head_includes/1,
         current_node_name/1]).

-spec username(dict()) -> string().
username(Context) ->
  mustache:get(username, Context).

-spec avatar_path(dict()) -> string().
avatar_path(Context) ->
  "http://www.gravatar.com/avatar/" ++ popcorn_util:md5_hex(mustache:get(username, Context)).

-spec head_includes(dict()) -> list().
head_includes(_) ->
  popcorn_util:head_includes().

-spec current_node_name(dict()) -> string().
current_node_name(Context) -> binary_to_list(mustache:get(node_name, Context)).
