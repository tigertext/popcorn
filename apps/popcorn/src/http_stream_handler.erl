-module(http_stream_handler).
-author('marc.e.campbell@gmail.com').
-author('elbrujohalcon@inaka.net').

-behavior(cowboy_http_handler).

-include("include/popcorn.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, _) -> {ok, Req, undefined_state}.

handle(Req, State) ->
    {Path, Req1}   = cowboy_req:path(Req),
    Path_Parts     = lists:filter(fun(<<>>) -> false; (_) -> true end, binary:split(Path, <<"/">>, [global])),
    {Method, Req2} = cowboy_req:method(Req1),
    handle_path(Method, Path_Parts, Req2, State).

terminate(_Req, _State) -> ok.

handle_path(<<"GET">>, [<<"dashboard">>, <<"stream">>, Stream_Id], Req, State) ->
    case ets:select(
            current_dashboard_streams,
            ets:fun2ms(
                fun(#stream{stream_id  = SID, stream_pid = SPID}) when SID =:= Stream_Id -> SPID end)) of
        [] ->
            Req1 = cowboy_req:set_resp_cookie(<<"popcorn-session-key">>, <<>>, [{path, <<"/">>}], Req),
            {ok, Reply} = cowboy_req:reply(301, [{"Location", "/login"}], [], Req1),
            {ok, Reply, State};
        Streams ->
            Headers      = [{"Content-Type", <<"text/event-stream">>}],
            {ok, Reply}  = cowboy_req:chunked_reply(200, Headers, Req),

            Stream_Pid    = lists:nth(1, Streams),

            gen_fsm:send_event(Stream_Pid, {set_client_pid, self()}),

            handle_loop(Reply, State)
    end.

handle_loop(Req, State) ->
    receive
        logout ->
            {ok, Req, State};
        {cowboy_req, resp_sent} ->
            handle_loop(Req, State);
        {new_node, Node} ->
            Total_Message_Count = ?COUNTER_VALUE(?TOTAL_EVENT_COUNTER),
            Data = [{node_count, ets:info(current_nodes, size)},
                    {node_name, Node#popcorn_node.node_name},
                    {node_hash, re:replace(base64:encode(Node#popcorn_node.node_name), "=", "_", [{return, binary}, global])},
                    {percent_of_all_events, ?PERCENT(1 / Total_Message_Count)},
                    {total_messages, 1},
                    {alert_count,   0},
                    {hashtag_count, 0},
                    {mention_count, 0}],
            Event = lists:flatten(mochijson:encode({struct, Data})),
            chunk_event("new_node", Event, Req, State);
        {update_counters, NewCounters} ->
            Event =
                lists:flatten(mochijson:encode(
                    case lists:keytake(counter, 1, NewCounters) of
                        false ->
                            {struct, NewCounters};
                        {value, {counter, Counter}, Rest} ->
                            CounterData = triage_handler:counter_data(Counter),
                            {struct, [{counter, {struct, CounterData}} | Rest]}
                    end)),
            chunk_event("update_counters", Event, Req, State);
        Other ->
            ?POPCORN_DEBUG_MSG("streaming handler received unknown message: ~p", [Other]),
            handle_loop(Req, State)
    end.

chunk_event(EventName, Event, Req, State) ->
    case cowboy_req:chunk(lists:flatten(["event: " ++ EventName ++ "\n"]), Req) of
        ok ->
            case cowboy_req:chunk(lists:flatten(["data: ", Event, "\n\n"]), Req) of
                ok -> handle_loop(Req, State);
                {error, closed} -> {ok, Req, State}
            end;
        {error, closed} -> {ok, Req, State}
    end.
