-module(outbound_tt_notifier).
-author('elbrujohalcon@inaka.net').

-behaviour(outbound_notifier_handler).

-include_lib("ibrowse/include/ibrowse.hrl").

-record(state, {url         :: string(),
                basic_auth  :: {basic_auth, {string(), string()}},
                recipients  :: [string()],
                message     :: string()
                }).
-opaque state() :: #state{}.

-export([handler_name/1, init/1, handle_event/3, handle_call/2, handle_info/2, terminate/2]).

-spec handler_name(InitArgs::[proplists:property()]) -> atom().
-spec init(InitArgs::string()) -> {ok, State::state()} | {stop, Reason::string()}.
-spec handle_event(Trigger::atom(), Data::term(), State::state()) -> {ok, State::state()} | {stop, Reason::term(), State::state()}.
-spec handle_call(Call::term(), State::state()) -> {ok, ok, State::state()}.
-spec handle_info(Info::term(), State::state()) -> {ok, State::state()}.
-spec terminate(Reason::term(), State::state()) -> _.

handler_name(Params) ->
    list_to_atom(
        ?MODULE_STRING ++
        [$:|proplists:get_value(api_key, Params, "")] ++
        [$:|proplists:get_value(api_secret, Params, "")]).

init(Params) ->
    application:start(ibrowse),
    Url         = proplists:get_value(base_url,     Params, "https://api.tigertext.me/v1/") ++ "message",
    ApiKey      = proplists:get_value(api_key,      Params, ""),
    ApiSecret   = proplists:get_value(api_secret,   Params, ""),
    Recipients  = proplists:get_value(recipients,   Params, []),
    Message     = proplists:get_value(message, Params, ""),
    case {ApiKey, ApiSecret, Recipients, Message, ibrowse_lib:parse_url(Url)} of
        {"", _, _, _, _} -> {stop, "Missing parameter: api_key"};
        {_, "", _, _, _} -> {stop, "Missing parameter: api_secret"};
        {_, _, [], _, _} -> {stop, "No recipients listed"};
        {_, _, _, "", _} -> {stop, "Missing parameter: message"};
        {_, _, _, _, {error, invalid_uri}} -> {stop, "Bad parameter: base_url"};
        {_, _, _, _, UrlRec = #url{}} ->
            ibrowse:set_max_sessions(UrlRec#url.host, UrlRec#url.port, 100),
            ibrowse:set_max_pipeline_size(UrlRec#url.host, UrlRec#url.port, 100),
            {ok, #state{url         = Url,
                        basic_auth  = {basic_auth, {ApiKey, ApiSecret}},
                        recipients  = [ibrowse_lib:url_encode(Recipient) || Recipient <- Recipients],
                        message     = Message}}
    end.

handle_event(Trigger, Data, State) ->
    Ctx     = dict:from_list([{trigger, Trigger} | Data]),
    Body    = mustache:render(State#state.message, Ctx),

    lists:foreach(
        fun(Recipient) ->
            try ibrowse:send_req(
                    State#state.url,
                    [{"Content-Type", "application/x-www-form-urlencoded"}], post,
                    ["body=", Body, "&recipient=", Recipient], [State#state.basic_auth]) of
                {ok, "204", _, _} -> ok;
                {ok, "429", Headers, _} ->
                    io:format("We have been rate-limited, we have to wait"),
                    case proplists:get_value("Retry-After", Headers) of
                        undefined -> undefined;
                        S -> timer:sleep(list_to_integer(S) * 1000)
                    end;
                Error ->
                    io:format("Could not notify ~p: ~p", [Recipient, Error]),
                    throw({stop, Error, State})
            catch
                _:Error ->
                    io:format("Could not notify ~p: ~p", [Recipient, Error]),
                    throw({stop, Error, State})
            end
        end, State#state.recipients),

    {ok, State}.

handle_call(Call, State) -> {ok, io:format("~p:~p Call: ~p~n", [?MODULE, ?LINE, Call]), State}.

handle_info(Info, State) -> {io:format("~p:~p Info: ~p~n", [?MODULE, ?LINE, Info]), State}.

terminate(Reason, _State) -> io:format("~p:~p Terminate: ~p~n", [?MODULE, ?LINE, Reason]).