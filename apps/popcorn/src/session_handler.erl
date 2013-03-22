-module(session_handler).
-author('marc.e.campbell@gmail.com').

-include("include/popcorn.hrl").

-export([try_start_authed_session/3,
         is_session_authed_and_valid/1,
         current_username/1,
         return_404/2,
         delete_session/1]).

-spec try_start_authed_session(string(), string(), string()) -> {error, string()} | {ok, any()}.
-spec is_session_authed_and_valid(any()) -> true | false.
-spec current_username(any()) -> string().
-spec return_404(any(), any()) -> any().
-spec delete_session(any()) -> any().

try_start_authed_session(IP_Address, Username, Password) ->
    {ok, Pid} = supervisor:start_child(connected_user_sup, []),
    case connected_user_fsm:try_auth_visit(Pid, IP_Address, Username, Password) of
        false -> gen_fsm:send_all_state_event(Pid, destroy),
                 {error, "Invalid Login"};
        true  -> Session_Key = gen_fsm:sync_send_event(Pid, get_session_key),
                 ets:insert(current_connected_users, {Session_Key, Pid}),
                 {ok, Session_Key}
    end.

is_session_authed_and_valid(Req) ->
    is_session_authed_and_valid(Req, default_auth(application:get_env(popcorn, http_auth))).

default_auth(undefined) -> enabled;
default_auth({ok, Value}) -> Value.

is_session_authed_and_valid(_, disabled) -> true;
is_session_authed_and_valid(Req, enabled) ->
    try cowboy_req:cookie(<<"popcorn-session-key">>, Req) of
        {Session_Key, _} ->
            case Session_Key of
                undefined -> false;
                _         -> case ets:lookup(current_connected_users, Session_Key) of
                                 [] -> false;
                                 _  -> true
                             end
            end
    catch
        A:B -> ?POPCORN_WARN_MSG("#exception in #session_handler #is_session_authed_and_valid ~p:~p", [A, B]),
               false
    end.

current_username(Req) -> current_username(Req, default_auth(application:get_env(popcorn, http_auth))).
current_username(_, disabled) -> <<"admin">>;
current_username(Req, enabled) ->
    try cowboy_req:cookie(<<"popcorn-session-key">>, Req) of
        {Session_Key, _} ->
            case Session_Key of
                undefined -> "";
                _         -> [{_, Pid}] = ets:lookup(current_connected_users, Session_Key),
                             gen_fsm:sync_send_event(Pid, get_username)
            end
    catch
        A:B -> ?POPCORN_WARN_MSG("#exception in #session_handler #current_username ~p:~p", [A, B]),
               ""
    end.

return_404(Req, State) ->
    {ok, Reply} = cowboy_req:reply(404, [], <<>>, Req),
    {ok, Reply, State}.

delete_session(Req) ->
    {Session_Key, Req1} =
      case cowboy_req:cookie(<<"popcorn-session-key">>, Req) of
          {SK, R1} when SK =:= undefined ->
              {undefined, R1};
          {SK, R1} ->
              {SK, R1}
      end,

    Req2 = cowboy_req:set_resp_cookie(<<"popcorn-session-key">>, <<>>, [{path, <<"/">>}], Req1),
    ets:delete(current_connected_users, Session_Key),
    Req2.
