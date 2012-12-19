-module(view_alerts).
-author('elbrujohalcon@inaka.net').

-include("include/popcorn.hrl").

-export([head_includes/0, alerts/1, username/0, header_button/1]).

-spec head_includes() -> list().
head_includes() -> popcorn_util:head_includes().

alerts(Context) -> [dict:from_list(Alert) || Alert <- triage_handler:all_alerts("true" == mustache:get(all, Context))].

header_button(Context) ->
	[dict:from_list(
		case mustache:get(all, Context) of
			"true" -> [{href, "/alerts"}, {label, "Recent"}];
			_ -> [{href, "/alerts?all"}, {label, "All"}]
		end)].

-spec username() -> string().
username() -> "admin".