-module(view_nodes).
-author('marc.e.campbell@gmail.com').

-include("include/popcorn.hrl").

-export([head_includes/0,
         username/1]).

-spec head_includes() -> list().
head_includes() -> popcorn_util:head_includes().

-spec username(dict()) -> string().
username(Context) -> view_generic:username(Context).

