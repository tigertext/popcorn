-module(view_alerts).
-author('elbrujohalcon@inaka.net').

-include("include/popcorn.hrl").

-export([head_includes/0, alerts/0, username/0]).

-spec head_includes() -> list().
head_includes() -> popcorn_util:head_includes().

alerts() -> [dict:from_list(Alert) || Alert <- triage_handler:all_alerts()].

-spec username() -> string().
username() -> "admin".