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
            Req1 = session_handler:delete_session(Req),
            Output = mustache:render(view_login),
            {ok, Reply} = cowboy_req:reply(301, [{"Location", "/login"}], [], Req1),
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

            ?POPCORN_DEBUG_MSG("Username = ~p, Password = ~p", [Username, Password]),
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
                      [jsonify(popcorn_util:format_log_message(Log_Message, undefined))
                         || Log_Message <- Log_Messages],
                    Output = binary_to_list(jiffy:encode({Jsons})),
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
                    Context     = dict:from_list([{username, binary_to_list(session_handler:current_username(Req))},
                                                  {location, binary_to_list(Alert)},
                                                  {log_messages, Log_Messages}
                                                  | triage_handler:alert_properties(Alert)]),
                    TFun        = pcache:get(rendered_templates, view_alert),
                    Output      = mustache:render(view_alert, TFun, Context),
                    {ok, Reply} = cowboy_req:reply(200, [], Output, Req),
                    {ok, Reply, State}
            end;

        {{<<"GET">>, _}, {<<"/data/alert_timestamps">>, _}} ->
            %% TODO: parse params for timezone
            %% TODO: parse params for severities chosen
            %% TODO: update the graph live as new data comes in
            ?POPCORN_DEBUG_MSG("#http_request for #alert_timestamps"),
            Timestamps = gen_server:call(pg2:get_closest_pid('storage'), {get_alert_timestamps, [8,128]}),
            Dict = lists:foldl(fun(Timestamp, Dict) -> 
                Date_String = format_timestamp(date_util:epoch_to_gregorian_seconds(Timestamp)),
                dict:update(Date_String, fun(C) -> C + 1 end, 1, Dict) 
            end, dict:new(), Timestamps),
            Now_TS = date_util:now_to_gregorian_seconds(),
            Range = [begin
                Last_Hour = Now_TS - 60 * 60 * (Hour-1),
                format_timestamp(Last_Hour)
             end || Hour <- lists:seq(1,24)],
            Range_List = dict:to_list(Dict),
            Output = [<<"date\tcount\n">>] ++
                     [begin
                        io_lib:format("~p\t~p\n", [Hour, proplists:get_value(Hour, Range_List, 0)])
                     end || Hour <- lists:reverse(Range)],
            {ok, Reply} = cowboy_req:reply(200, [], Output, Req),
            {ok, Reply, State};

        {{<<"GET">>, _}, {<<"/nodes">>, _}} ->
            ?POPCORN_DEBUG_MSG("#http_request for #nodes"),
            case session_handler:is_session_authed_and_valid(Req) of
                false ->
                    Req1 = cowboy_req:set_resp_cookie(<<"popcorn-session-key">>, <<>>, [{path, <<"/">>}], Req),
                    {ok, Reply} = cowboy_req:reply(301, [{"Location", "/login"}], [], Req1),
                    {ok, Reply, State};
                true  ->
                    %% this one isn't a stream, but maybe it should be!
                    Context = dict:from_list([{username, binary_to_list(session_handler:current_username(Req))}]),
                    TFun    = pcache:get(rendered_templates, view_nodes),
                    Output  = mustache:render(view_nodes, TFun, Context),
                    {ok, Reply} = cowboy_req:reply(200, [], Output, Req),
                    {ok, Reply, State}
            end;

        {{<<"GET">>, _}, {<<"/alerts">>, _}} ->
            {All, _} = cowboy_req:qs_val(<<"all">>, Req),
            {Sort, _} = cowboy_req:qs_val(<<"sort">>, Req, <<"time">>),
            Severities =
                case cowboy_req:qs_val(<<"severities">>, Req) of
                    {undefined, _} -> all;
                    {AsBinary, _} -> [list_to_integer(binary_to_list(S)) || S <- binary:split(AsBinary, <<",">>, [global,trim])]
                end,
            ?POPCORN_DEBUG_MSG("#http_request for alerts (~p, ~p)", [All, Severities]),
            case session_handler:is_session_authed_and_valid(Req) of
                false ->
                    Req1 = cowboy_req:set_resp_cookie(<<"popcorn-session-key">>, <<>>, [{path, <<"/">>}], Req),
                    {ok, Reply} = cowboy_req:reply(301, [{"Location", "/login"}], [], Req1),
                    {ok, Reply, State};
                true ->
                    %% spawn the stream fsm
                    {ok, Stream_Pid} = supervisor:start_child(dashboard_stream_sup, []),

                    %% create the stream object
                    Stream_Id = popcorn_util:random_id(),
                    Stream = #stream{stream_id          = Stream_Id,
                                     stream_pid         = Stream_Pid,
                                     client_pid         = undefined},

                    %% assign to the fsm
                    gen_fsm:send_event(Stream_Pid, {connect, Stream}),

                    Context = dict:from_list([{username,    binary_to_list(session_handler:current_username(Req))},
                                              {stream_id,   binary_to_list(Stream_Id)},
                                              {all,         All},
                                              {sort,        list_to_atom(binary_to_list(Sort))},
                                              {severities,  Severities}]),

                    TFun        = pcache:get(rendered_templates, view_alerts),
                    Output      = mustache:render(view_alerts, TFun, Context),
                    {ok, Reply} = cowboy_req:reply(200, [], Output, Req),
                    {ok, Reply, State}
            end;

        {{<<"GET">>, _}, {<<"/settings">>, _}} ->
            ?POPCORN_DEBUG_MSG("#http_request for settings"),
            case session_handler:is_session_authed_and_valid(Req) of
                false ->
                    Req1 = cowboy_req:set_resp_cookie(<<"popcorn-session-key">>, <<>>, [{path, <<"/">>}], Req),
                    {ok, Reply} = cowboy_req:reply(301, [{"Location", "/login"}], [], Req1),
                    {ok, Reply, State};
                true ->
                    Context = dict:from_list([{username, binary_to_list(session_handler:current_username(Req))}]),
                    TFun = pcache:get(rendered_templates, view_settings),
                    Output = mustache:render(view_settings, TFun, Context),
                    {ok, Reply} = cowboy_req:reply(200, [], Output, Req),
                    {ok, Reply, State}
            end;

        {{<<"GET">>, _}, {<<"/">>, _}} ->
            ?POPCORN_DEBUG_MSG("#http_request for dashboard"),
            case session_handler:is_session_authed_and_valid(Req) of
                false ->
                    Req1 = cowboy_req:set_resp_cookie(<<"popcorn-session-key">>, <<>>, [{path, <<"/">>}], Req),
                    {ok, Reply} = cowboy_req:reply(301, [{"Location", "/login"}], [], Req1),
                    {ok, Reply, State};
                true  ->
                    %% spawn the stream fsm
                    {ok, Stream_Pid} = supervisor:start_child(dashboard_stream_sup, []),

                    %% create the stream object
                    Stream_Id = popcorn_util:random_id(),
                    Stream = #stream{stream_id = Stream_Id,
                                     stream_pid = Stream_Pid,
                                     client_pid = undefined},

                    %% assign to the fsm
                    gen_fsm:send_event(Stream_Pid, {connect, Stream}),

                    Context = dict:from_list([{username,  binary_to_list(session_handler:current_username(Req))},
                                              {stream_id, binary_to_list(Stream_Id)}]),

                    TFun = pcache:get(rendered_templates, view_dashboard),
                    Output = mustache:render(view_dashboard, TFun, Context),
                    {ok, Reply} = cowboy_req:reply(200, [], Output, Req),
                    {ok, Reply, State}
            end;

        {{Method, _}, {Path, _}} ->
            ?POPCORN_DEBUG_MSG("not implemented in catchall http request: ~p, ~p", [Method, Path]),
            {ok, Reply} = cowboy_req:reply(405, [], <<>>, Req),
            {ok, Reply, State}
    end.

terminate(_Req, _State) -> ok.

jsonify(Vals) -> {[do_jsonify(Val) || Val <- Vals]}.
do_jsonify({Key, String}) when is_list(String) -> {atom_to_binary(Key, latin1), list_to_binary(String)};
do_jsonify({Key, Atom}) when is_atom(Atom) -> {atom_to_binary(Key, latin1), atom_to_binary(Atom, latin1)};
do_jsonify({Key, Other}) -> {atom_to_binary(Key, latin1), Other}.

format_timestamp(Timestamp) ->
    {{Year, Month, Day}, {Hour_GMT, Min, Sec}} = calendar:gregorian_seconds_to_datetime(Timestamp),
    Hour = Hour_GMT - 7,
    %io_lib:format("~B/~B/~4..0B~2B", [Month, Day, Year, Hour]).
    normalize_hour(Hour).

normalize_hour(0) -> "12pm";
normalize_hour(Hour) when Hour > 12 ->
    integer_to_list(Hour - 12) ++ "pm";
normalize_hour(Hour) ->
    integer_to_list(Hour) ++ "am".

