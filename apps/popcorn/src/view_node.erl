-module(view_node).
-author('marc.e.campbell@gmail.com').

-include("include/popcorn.hrl").

-export([head_includes/0,
         username/0,
         current_node_name/1]).

-spec head_includes() -> list().
head_includes() -> popcorn_util:head_includes().

-spec username() -> string().
username() -> "admin".

-spec current_node_name(dict()) -> string().
current_node_name(Context) -> binary_to_list(mustache:get(node_name, Context)).
