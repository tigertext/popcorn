-module(view_alerts).
-author('elbrujohalcon@inaka.net').

-include("include/popcorn.hrl").

-export([head_includes/0,
         alerts/1,
         username/1,
         known_severities/1,
         header_button/1,
         sort_button/1]).

-spec head_includes() -> list().
head_includes() -> popcorn_util:head_includes().

alerts(Context) ->
	Alerts =
		case mustache:get(severities, Context) of
            "all" -> triage_handler:all_alerts("true" == mustache:get(all, Context), as_atom(mustache:get(sort, Context)));
            Ss -> triage_handler:alerts("true" == mustache:get(all, Context), Ss, as_atom(mustache:get(sort, Context)))
		end,
	[dict:from_list(Alert) || Alert <- Alerts].

as_atom(List) -> list_to_atom(List).

header_button(Context) ->
	[dict:from_list(
		case mustache:get(all, Context) of
			"true" -> [{href, "/alerts"}, {label, "Recent"}];
			_ -> [{href, "/alerts?all"}, {label, "All"}]
		end)].

sort_button(Context) ->
	[dict:from_list(
		case mustache:get(sort, Context) of
			"time"  -> [{href, "/alerts?sort=count"}, {label, "Count"}];
			"count" -> [{href, "/alerts?sort=time"}, {label, "Time"}]
		end)].

-spec username(dict()) -> string().
username(Context) -> view_generic:username(Context).

-spec known_severities(dict()) -> list().
known_severities(Context) ->
	Checked =
		case mustache:get(severities, Context) of
			"all" -> [N || {_, N} <- popcorn_util:all_severities()];
			Ss -> Ss
		end,
	erlang:display(Checked),
    lists:map(fun({Severity_Name, Severity_Number}) ->
        Params = [{'label',        	Severity_Name},
                  {'severity_num', 	integer_to_list(Severity_Number)},
                  {'checked',		case lists:member(Severity_Number, Checked) of
                  						true -> "checked";
                  						false -> ""
                  					end}],
        dict:from_list(Params)
      end, lists:reverse(popcorn_util:alert_severities())). %% TODO: use a list comprehension and add a filter to remove severities that have not been seen
