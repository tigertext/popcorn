-module(view_alert).
-author('elbrujohalcon@inaka.net').

-include("include/popcorn.hrl").

-export([head_includes/0, username/0, logs/1]).

-spec head_includes() -> list().
head_includes() -> popcorn_util:head_includes().

-spec logs(dict()) -> [dict()].
logs(Context) ->
	[dict:from_list(popcorn_util:format_log_message(Log_Message))
	 || Log_Message <- triage_handler:log_messages(mustache:get(counter, Context))].

-spec username() -> string().
username() -> "admin".