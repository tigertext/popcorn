-module(view_alerts).
-author('elbrujohalcon@inaka.net').

-include("include/popcorn.hrl").

-export([head_includes/0, alerts/1, username/0, known_severities/1, header_button/1]).

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

-spec known_severities(dict()) -> list().
known_severities(Ctx) ->
	Checked =
		case mustache:get(severities, Ctx) of
			all -> [N || {_, N} <- popcorn_util:all_severities()];
			Ss -> Ss
		end,
    lists:map(fun({Severity_Name, Severity_Number}) ->
        Params = [{'label',        	Severity_Name},
                  {'severity_num', 	integer_to_list(Severity_Number)},
                  {'checked',		case lists:member(Severity_Number, Checked) of
                  						true -> "checked";
                  						false -> ""
                  					end}],
        dict:from_list(Params)
      end, lists:reverse(popcorn_util:all_severities())).