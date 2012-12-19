-module(view_alert).
-author('elbrujohalcon@inaka.net').

-include("include/popcorn.hrl").

-export([head_includes/0, username/0, logs/1]).

-spec head_includes() -> list().
head_includes() -> popcorn_util:head_includes().

-spec logs(dict()) -> [dict()].
logs(Context) ->
	[dict:from_list(popcorn_util:format_log_message(Log_Message))
	 || Log_Message <- [#log_message{message = <<"one">>, timestamp = ?NOW, severity = 1},
  						#log_message{message = <<"two">>, timestamp = ?NOW, severity = 2}]].

-spec username() -> string().
username() -> "admin".