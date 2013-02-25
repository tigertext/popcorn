-module(view_log).
-author('marc.e.campbell@gmail.com').

-include_lib("stdlib/include/ms_transform.hrl").
-include("include/popcorn.hrl").

-export([head_includes/0,
         known_severities/1,
         applied_filters/1,
         username/1,
         streaming_url/1]).

-spec head_includes() -> list().
head_includes() -> popcorn_util:head_includes().

-spec known_severities(dict()) -> list().
known_severities(_) ->
    lists:map(fun({Severity_Name, Severity_Number}) ->
        Params = [{'label',        Severity_Name},
                  {'severity_num', integer_to_list(Severity_Number)}],
        dict:from_list(Params)
      end, popcorn_util:all_severities()).

-spec applied_filters(dict()) -> string().
applied_filters(Context) ->
    Default_Filters = dict:to_list(proplists:get_value(default_filters, dict:to_list(Context))),
    Json = {struct, lists:map(fun({Name, Value}) ->
                        {atom_to_list(Name), {array, Value}}
                      end, Default_Filters)},
    lists:flatten(mochijson:encode(Json)).

-spec username(dict()) -> string().
username(Context) -> view_generic:username(Context).

-spec streaming_url(dict()) -> string().
streaming_url(Context) ->
    "/log/stream/" ++ mustache:get(stream_id, Context).

get_opt_env(Mod, Var, Default) ->
    case application:get_env(Mod, Var) of
        {ok, Val} -> {ok, Val};
        _         -> {ok, Default}
    end.
