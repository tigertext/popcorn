-module(view_alert).
-author('elbrujohalcon@inaka.net').

-include("include/popcorn.hrl").

-export([head_includes/0,
         username/1,
         logs/1,
         next_page/1]).

-spec head_includes() -> list().
head_includes() -> popcorn_util:head_includes().

-spec logs(dict()) -> [dict()].
logs(Context) ->
    [dict:from_list(popcorn_util:format_log_message(Log_Message))
     || Log_Message <- mustache:get(log_messages, Context)].

-spec username(dict()) -> string().
username(Context) -> view_generic:username(Context).

-spec next_page(dict()) -> list().
next_page(Context) ->
    case lists:reverse(mustache:get(log_messages, Context)) of
        [] -> "";
        [#log_message{timestamp = TS}|_] ->
            NextPageUrl = "/alert/" ++ mustache:get(location, Context) ++ "?since=" ++ integer_to_list(TS),
            "<tr><td class='load-more' colspan='3'><a href='" ++ NextPageUrl ++ "'>Load More...</a></td></tr>"
    end.
