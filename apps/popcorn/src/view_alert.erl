-module(view_alert).
-author('elbrujohalcon@inaka.net').
-behaviour(view_generic).

-include("include/popcorn.hrl").

-export([username/1,
         avatar_path/1,
         head_includes/1,
         logs/1,
         next_page/1]).

-spec username(dict()) -> string().
username(Context) ->
  mustache:get(username, Context).

-spec avatar_path(dict()) -> string().
avatar_path(Context) ->
  "http://www.gravatar.com/avatar/" ++ popcorn_util:md5_hex(mustache:get(username, Context)).

-spec head_includes(dict()) -> list().
head_includes(_) ->
  popcorn_util:head_includes().

-spec logs(dict()) -> [dict()].
logs(Context) ->
  [dict:from_list(popcorn_util:format_log_message(Log_Message, undefined))
   || Log_Message <- mustache:get(log_messages, Context)].

-spec next_page(dict()) -> list().
next_page(Context) ->
  case lists:reverse(mustache:get(log_messages, Context)) of
    [] -> "";
    [#log_message{timestamp = TS}|_] ->
        NextPageUrl = "/alert/" ++ mustache:get(location, Context) ++ "?since=" ++ integer_to_list(TS),
        "<tr><td class='load-more' colspan='3'><a href='" ++ NextPageUrl ++ "'>Load More...</a></td></tr>"
  end.
