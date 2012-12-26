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

        {{<<"GET">>, _}, {<<"/alert/", Alert/binary>>, _}} ->
            ?POPCORN_DEBUG_MSG("http request for alert ~s", [Alert]),
            case session_handler:is_session_authed_and_valid(Req) of
                false -> Req1 = cowboy_req:set_resp_cookie(<<"popcorn-session-key">>, <<>>, [{path, <<"/">>}], Req),
                         {ok, Reply} = cowboy_req:reply(301, [{"Location", "/login"}], [], Req1),
                         {ok, Reply, State};
                true  ->
                    Location    = triage_handler:decode_location(Alert),
                    Log_Messages=
                        triage_handler:log_messages(
                            proplists:get_value(product,   Location),
                            proplists:get_value(version,   Location),
                            proplists:get_value(name,      Location),
                            proplists:get_value(line,      Location),
                            undefined,
                            case application:get_env(popcorn, alert_page_size) of
                                {ok, Val} -> Val;
                                _         -> 10
                            end),
                    Context     = dict:from_list([{log_messages, Log_Messages} | Location]),
                    TFun        = mustache:compile(view_alert),
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

                        TFun        = mustache:compile(view_alerts),
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

                        TFun        = mustache:compile(view_dashboard),
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
