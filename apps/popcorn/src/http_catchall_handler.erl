-module(http_catchall_handler).
-author('marc.e.campbell@gmail.com').

-behavior(cowboy_http_handler).

-include("include/popcorn.hrl").

-export([init/3,
         handle/2,
         terminate/2]).

init({_Any, http}, Req, _) -> {ok, Req, undefined_state}.

handle(Req, State) ->
    case {cowboy_req:method(Req), cowboy_req:path(Req)} of
        {{<<"GET">>, _}, {<<"/logout">>, _}} ->
            ?POPCORN_DEBUG_MSG("http request for logout page"),
            Output = mustache:render(popcorn, ?MUSTACHE("logout.mustache"), dict:from_list([])),
            Req1 = cowboy_req:set_resp_cookie(<<"popcorn-session-key">>, <<>>, [{path, <<"/">>}], Req),
            {ok, Reply} = cowboy_req:reply(200, [], Output, Req1),
            {ok, Reply, State};

        {{<<"GET">>, _}, {<<"/login">>, _}} ->
            ?POPCORN_DEBUG_MSG("http request for login page"),
            Output = mustache:render(view_login),
            {ok, Reply} = cowboy_req:reply(200, [], Output, Req),
            {ok, Reply, State};

        {{<<"POST">>, _}, {<<"/api/v1/login">>, _}} ->
            ?POPCORN_DEBUG_MSG("http api login"),
            {ok, Post_Vals, _Req2} = cowboy_req:body_qs(Req),
            Username = proplists:get_value(<<"username">>, Post_Vals),
            Password = proplists:get_value(<<"password">>, Post_Vals),

            case session_handler:try_start_authed_session("", Username, Password) of
                {error, Message}  -> {ok, Reply} = cowboy_req:reply(401, [], Message, Req),
                                     {ok, Reply, State};
                {ok, Session_Key} -> Req1 = cowboy_req:set_resp_cookie(<<"popcorn-session-key">>, Session_Key, [{path, <<"/">>}], Req),
                                     {ok, Reply} = cowboy_req:reply(200, [], <<>>, Req1),
                                     {ok, Reply, State}
            end;

        {{<<"POST">>, _}, {<<"/clear_alert">>, _}} ->
            {ok, Post, _Req2} = cowboy_req:body_qs(Req),
            Alert = proplists:get_value(<<"alert">>, Post, <<>>),
            triage_handler:clear_alert(Alert),
            {ok, Reply} = cowboy_req:reply(200, Req),
            {ok, Reply, State};

        {{<<"POST">>, _}, {<<"/alert/", Alert/binary>>, _}} ->
            {ok, Post_Vals, _Req2} = cowboy_req:body_qs(Req),
            case session_handler:is_session_authed_and_valid(Req) of
                false -> Req1 = cowboy_req:set_resp_cookie(<<"popcorn-session-key">>, <<>>, [{path, <<"/">>}], Req),
                         {ok, Reply} = cowboy_req:reply(301, [{"Location", "/login"}], [], Req1),
                         {ok, Reply, State};
                true  ->
                    Since =
                        case proplists:get_value(<<"since">>, Post_Vals) of
                            undefined -> undefined;
                            BinSince -> list_to_integer(binary_to_list(BinSince))
                        end,
                    ?POPCORN_DEBUG_MSG("http request for alert ~s (since ~p)", [Alert, Since]),
                    Log_Messages=
                        triage_handler:log_messages(
                            Alert, Since,
                            case application:get_env(popcorn, alert_page_size) of
                                {ok, Val} -> Val;
                                _         -> 10
                            end),
                    Jsons =
                        [jsonify(popcorn_util:format_log_message(Log_Message))
                         || Log_Message <- Log_Messages],
                    Output = lists:flatten(mochijson:encode({array, Jsons})),
                    {ok, Reply} = cowboy_req:reply(200, [], Output, Req),
                    {ok, Reply, State}
            end;

        {{<<"GET">>, _}, {<<"/alert/", Alert/binary>>, _}} ->
            ?POPCORN_DEBUG_MSG("http request for alert ~s", [Alert]),
            case session_handler:is_session_authed_and_valid(Req) of
                false -> Req1 = cowboy_req:set_resp_cookie(<<"popcorn-session-key">>, <<>>, [{path, <<"/">>}], Req),
                         {ok, Reply} = cowboy_req:reply(301, [{"Location", "/login"}], [], Req1),
                         {ok, Reply, State};
                true  ->
                    Since =
                        case cowboy_req:qs_val(<<"since">>, Req) of
                            {undefined, _} -> undefined;
                            {BinSince, _} -> list_to_integer(binary_to_list(BinSince))
                        end,
                    Log_Messages=
                        triage_handler:log_messages(
                            Alert, Since,
                            case application:get_env(popcorn, alert_page_size) of
                                {ok, Val} -> Val;
                                _         -> 10
                            end),
                    Location    = base64:decode(re:replace(Alert, "_", "=", [{return, binary}, global])),
                    Context     = dict:from_list([{location, binary_to_list(Alert)}, {log_messages, Log_Messages}
                                                    | triage_handler:alert_properties(Alert)]),
                    TFun        = pcache:get(rendered_templates, view_alert),
                    Output      = mustache:render(view_alert, TFun, Context),
                    {ok, Reply} = cowboy_req:reply(200, [], Output, Req),
                    {ok, Reply, State}
            end;

        {{<<"GET">>, _}, {<<"/alerts">>, _}} ->
            {All, _} = cowboy_req:qs_val(<<"all">>, Req),
            ?POPCORN_DEBUG_MSG("http request for alerts (~p)", [All]),
            case session_handler:is_session_authed_and_valid(Req) of
                false -> Req1 = cowboy_req:set_resp_cookie(<<"popcorn-session-key">>, <<>>, [{path, <<"/">>}], Req),
                         {ok, Reply} = cowboy_req:reply(301, [{"Location", "/login"}], [], Req1),
                         {ok, Reply, State};
                true  ->
                        %% spawn the stream fsm
                        {ok, Stream_Pid} = supervisor:start_child(dashboard_stream_sup, []),

                        %% create the stream object
                        Stream_Id = popcorn_util:random_id(),
                        Stream = #stream{stream_id        = Stream_Id,
                                         stream_pid       = Stream_Pid,
                                         client_pid       = undefined},

                        %% assign to the fsm
                        gen_fsm:send_event(Stream_Pid, {connect, Stream}),

                        Context = dict:from_list([{stream_id, binary_to_list(Stream_Id)},
                                                  {all,       All}]),

                        TFun        = pcache:get(rendered_templates, view_alerts),
                        Output      = mustache:render(view_alerts, TFun, Context),
                        {ok, Reply} = cowboy_req:reply(200, [], Output, Req),
                        {ok, Reply, State}
            end;

        {{<<"GET">>, _}, {<<"/">>, _}} ->
            ?POPCORN_DEBUG_MSG("http request for dashboard"),
            case session_handler:is_session_authed_and_valid(Req) of
                false -> Req1 = cowboy_req:set_resp_cookie(<<"popcorn-session-key">>, <<>>, [{path, <<"/">>}], Req),
                         {ok, Reply} = cowboy_req:reply(301, [{"Location", "/login"}], [], Req1),
                         {ok, Reply, State};
                true  ->
                        %% spawn the stream fsm
                        {ok, Stream_Pid} = supervisor:start_child(dashboard_stream_sup, []),

                        %% create the stream object
                        Stream_Id = popcorn_util:random_id(),
                        Stream = #stream{stream_id        = Stream_Id,
                                         stream_pid       = Stream_Pid,
                                         client_pid       = undefined},

                        %% assign to the fsm
                        gen_fsm:send_event(Stream_Pid, {connect, Stream}),

                        Context = dict:from_list([{stream_id, binary_to_list(Stream_Id)}]),

                        TFun        = pcache:get(rendered_templates, view_dashboard),
                        Output      = mustache:render(view_dashboard, TFun, Context),
                        {ok, Reply} = cowboy_req:reply(200, [], Output, Req),
                        {ok, Reply, State}
            end;

        {{Method, _}, {Path, _}} ->
            ?POPCORN_DEBUG_MSG("not implemented in catchall http request: ~p, ~p", [Method, Path]),
            {ok, Reply} = cowboy_req:reply(405, [], <<>>, Req),
            {ok, Reply, State}
    end.

terminate(_Req, _State) -> ok.

jsonify(Vals) -> {struct, [do_jsonify(Val) || Val <- Vals]}.
do_jsonify({Key, String}) when is_list(String) -> {atom_to_binary(Key, latin1), list_to_binary(String)};
do_jsonify({Key, Atom}) when is_atom(Atom) -> {atom_to_binary(Key, latin1), atom_to_binary(Atom, latin1)};
do_jsonify({Key, Other}) -> {atom_to_binary(Key, latin1), Other}.