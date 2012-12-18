-module(view_alerts).
-author('elbrujohalcon@inaka.net').

-include("include/popcorn.hrl").

-export([head_includes/0, alerts/0, username/0]).

-spec head_includes() -> list().
head_includes() -> popcorn_util:head_includes().

alerts() ->
    [dict:from_list([{count, Num} | triage_handler:counter_data(Counter)])
     || {Counter, Num} <- gen_event:call(triage_handler, triage_handler, {alerts})
    ].

-spec username() -> string().
username() -> "marc".