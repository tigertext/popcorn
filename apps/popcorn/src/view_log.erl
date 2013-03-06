-module(view_log).
-author('marc.e.campbell@gmail.com').
-behaviour(view_generic).

-include_lib("stdlib/include/ms_transform.hrl").
-include("include/popcorn.hrl").

-export([username/1,
         avatar_path/1,
         head_includes/1,
         known_severities/1,
         applied_filters/1,
         streaming_url/1]).

-spec username(dict()) -> string().
username(Context) ->
  mustache:get(username, Context).

-spec avatar_path(dict()) -> string().
avatar_path(Context) ->
  "http://www.gravatar.com/avatar/" ++ popcorn_util:md5_hex(mustache:get(username, Context)).

-spec head_includes(dict()) -> list().
head_includes(_) ->
  popcorn_util:head_includes().

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
    Json = {lists:map(fun({Name, Value}) ->
                          {Name, popcorn_util:jiffy_safe_array(Value)}
            end, Default_Filters)},
    binary_to_list(jiffy:encode(Json)).

-spec streaming_url(dict()) -> string().
streaming_url(Context) ->
    "/log/stream/" ++ mustache:get(stream_id, Context).

get_opt_env(Mod, Var, Default) ->
    case application:get_env(Mod, Var) of
        {ok, Val} -> {ok, Val};
        _         -> {ok, Default}
    end.
