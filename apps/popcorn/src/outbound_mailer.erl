-module(outbound_mailer).
-author('elbrujohalcon@inaka.net').

-behaviour(outbound_notifier_handler).

-record(state, {from        :: {string() | binary(), string() | binary()},
                recipients  :: {string() | binary(), [string() | binary()]},
                subject     :: string(),
                body        :: string(),
                options     :: [proplists:property()]
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
        [$:|proplists:get_value(from, Params, "")] ++
        [$:|proplists:get_value(to, Params, "")]).

init(Params) ->
    From =
        case proplists:get_value(from, Params) of
            undefined -> {"PopCorn Server", "server@popcorn.net"};
            {FN, FV} -> {FN, FV};
            FV -> {FV, FV}
        end,
    Recipients =
        case proplists:get_value(recipients, Params) of
            undefined -> {"Alert Recipients", []};
            {RN, Rs} -> {RN, Rs};
            Rs -> {"Alert Recipients", Rs}
        end,
    Subject     = proplists:get_value(subject,      Params, ""),
    Body        = proplists:get_value(body,         Params, ""),
    Options     = proplists:get_value(options,      Params, []),
    case {Recipients, Subject, Body} of
        {{_, []}, _, _} -> {stop, "Missing parameter: recipients"};
        {_, "", _} -> {stop, "Missing parameter: subject"};
        {_, _, ""} -> {stop, "Missing parameter: body"};
        {_, _, _} ->
            {ok, #state{from = From, recipients = Recipients, subject = Subject, body = Body, options = Options}}
    end.

handle_event(Trigger, Data, State) ->
    Ctx     = dict:from_list([{trigger, Trigger} | Data]),
    Body    = mustache:render(State#state.body, Ctx),
    Subject = mustache:render(State#state.subject, Ctx),
    {FromName, From} = State#state.from,
    {RecsName, Recs} = State#state.recipients,
    SmtpData = ["Subject: ", Subject, "\r\n"
                "From: ", FromName, "\r\n"
                "To: ", RecsName, "\r\n"
                "\r\n", Body],

    {ok, _Pid} = gen_smtp_client:send({From, Recs, SmtpData}, State#state.options),

    {ok, State}.

handle_call(Call, State) -> {ok, io:format("~p:~p Call: ~p~n", [?MODULE, ?LINE, Call]), State}.

handle_info(Info, State) -> {io:format("~p:~p Info: ~p~n", [?MODULE, ?LINE, Info]), State}.

terminate(Reason, _State) -> io:format("~p:~p Terminate: ~p~n", [?MODULE, ?LINE, Reason]).