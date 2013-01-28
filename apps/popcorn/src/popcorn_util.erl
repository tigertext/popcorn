-module(popcorn_util).
-author('marc.e.campbell@gmail.com').

-include("include/popcorn.hrl").

-export([node_event_counter/1,
         unique_id/0,
         hour/0,
         retention_time_to_microsec/1,
         last_24_hours/0,
         severity_to_number/1,
         number_to_severity/1,
         all_severity_numbers/0,
         random_id/0,
         format_log_message/1,
         opt/2,
         head_includes/0,
         optional_env/2,
         hexstring/1,
         read/1,
         rps_enabled/0]).

node_event_counter(Node_Name) ->
    Prefix = <<"node_events__">>,
    <<Prefix/binary, Node_Name/binary>>.

unique_id() -> {_, S} = flake_server:id(62),
               list_to_binary(S).

hour() -> integer_to_list(erlang:trunc(folsom_utils:now_epoch() / 3600)).

seconds_to_microseconds(Seconds) -> Seconds * 1000000.

retention_time_to_microsec({minutes, Minutes}) -> seconds_to_microseconds(Minutes * 60);
retention_time_to_microsec({hours, Hours})     -> retention_time_to_microsec({minutes, Hours * 60});
retention_time_to_microsec({days, Days})       -> retention_time_to_microsec({hours, Days * 24});
retention_time_to_microsec({weeks, Weeks})     -> retention_time_to_microsec({days, Weeks * 7});
retention_time_to_microsec({months, Months})   -> retention_time_to_microsec({weeks, Months * 4}).

last_24_hours() ->
    lists:map(fun(Hours_Ago) ->
        integer_to_list(erlang:trunc(folsom_utils:now_epoch() / 3600) - Hours_Ago)
      end, lists:seq(0, 23)).

severity_to_number(<<"all">>) -> -1;
severity_to_number(<<"debug">>) -> 128;
severity_to_number(<<"info">>) -> 64;
severity_to_number(<<"notice">>) -> 32;
severity_to_number(<<"warn">>) -> 16;
severity_to_number(<<"error">>) -> 8;
severity_to_number(<<"critical">>) -> 4;
severity_to_number(<<"alert">>) -> 2;
severity_to_number(<<"emergency">>) -> 1;
severity_to_number(<<"none">>) -> 0;
severity_to_number(_) -> undefined.

number_to_severity(128) -> <<"debug">>;
number_to_severity(64) -> <<"info">>;
number_to_severity(32) -> <<"notice">>;
number_to_severity(16) -> <<"warn">>;
number_to_severity(8) -> <<"error">>;
number_to_severity(4) -> <<"critical">>;
number_to_severity(2) -> <<"alert">>;
number_to_severity(1) -> <<"emergency">>;
number_to_severity(0) -> <<"none">>;
number_to_severity(_) -> <<"?">>.

all_severity_numbers() -> [0, 1, 2, 4, 8, 16, 32, 64, 128].

random_id() ->
    Length = 64,
    AllowedChars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
    random:seed(now()),
    New_Key = lists:foldl(fun(_, Acc) ->
                  [lists:nth(random:uniform(length(AllowedChars)), AllowedChars)] ++ Acc
                end, [], lists:seq(1, Length)),
    list_to_binary(New_Key).

optional_env(Key, Default) ->
    case application:get_env(popcorn, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

opt(<<>>, Default)      -> Default;
opt(undefined, Default) -> Default;
opt(Value, _)           -> Value.

apply_links([], In) -> In;
apply_links(Identities, In) ->
    Identity = lists:nth(1, Identities),
    Out = re:replace(binary_to_list(In), "@" ++ Identity, "<a href=\"#\">@" ++ Identity ++ "</a>", [global, {return, list}]),
    apply_links(lists:nthtail(1, Identities), list_to_binary(Out)).

format_log_message(#log_message{timestamp=Timestamp, log_module=Module, log_function=Function, log_line=Line, log_pid=Pid,
                                severity=Severity, message=Message, topics=Topics, identities=Identities, log_product=Product,
                                log_version=Version}) ->
  UTC_Timestamp = calendar:now_to_universal_time({Timestamp div 1000000000000, 
                                                  Timestamp div 1000000 rem 1000000,
                                                  Timestamp rem 1000000}),
  {{Year, Month, Day}, {Hour, Minute, Second}} = UTC_Timestamp,
  Formatted_DateTime = lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Minute, Second])),
  Formatted_Time     = lists:flatten(io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B", [Hour, Minute, Second])),

  Find_More_Html     = "<strong>Filter current list to show only messages with matching:</strong><br /><br />" ++
                       "<label class='checkbox popover-label'><input type='checkbox'>Severity: " ++ binary_to_list(number_to_severity(Severity)) ++ "</label>" ++
                       "<label class='checkbox popover-label'><input type='checkbox'>Module: " ++ binary_to_list(opt(Module, <<"Not set">>)) ++ "</label>" ++
                       "<label class='checkbox popover-label'><input type='checkbox'>Function: " ++ binary_to_list(opt(Function, <<"Not set">>)) ++ "</label>" ++
                       "<label class='checkbox popover-label'><input type='checkbox'>Line: " ++ binary_to_list(opt(Line, <<"?">>)) ++ " in " ++ binary_to_list(opt(Module, <<"not set">>)) ++ "</label>" ++
                       "<label class='checkbox popover-label'><input type='checkbox'>Pid: " ++ binary_to_list(opt(Pid, <<"Not set">>)) ++ "</label>" ++
                       lists:append(["<label class='checkbox popover-label'><input type='checkbox'>@" ++ Identity ++ "</label>" || Identity <- Identities]) ++
                       lists:append(["<label class='checkbox popover-label'><input type='checkbox'>#" ++ Topic ++ "</label>" || Topic <- Topics]) ++ 
                       "<br /><button class='btn btn-mini' type='button'>Apply Filter</button>",

  Linked_Message = apply_links(Identities, Message),

  [{'timestamp',        Timestamp},
   {'topics',           {array, Topics}},
   {'identities',       {array, Identities}},
   {'time',             Formatted_Time},
   {'datetime',         Formatted_DateTime},
   {'find_more_html',   Find_More_Html},
   {'log_product',      binary_to_list(opt(Product, <<"Unknown">>))},
   {'log_version',      binary_to_list(opt(Version, <<"Unknown">>))},
   {'log_module',       binary_to_list(opt(Module, <<"Unknown">>))},
   {'log_function',     binary_to_list(opt(Function, <<"Unknown">>))},
   {'log_line',         binary_to_list(opt(Line, <<"??">>))},
   {'log_pid',          binary_to_list(opt(Pid, <<"?">>))},
   {'message_severity', binary_to_list(number_to_severity(Severity))},
   {'message',          binary_to_list(Linked_Message)}].

css_file() ->
    case file:read_file_info(code:priv_dir(popcorn) ++ "/css/popcorn.css") of
        {error,enoent} -> "popcorn.less";
        {ok, _} -> "popcorn.css"
    end.

head_includes() ->
    Head_Includes = ["<link rel='stylesheet/less' href=\"/css/"++css_file()++"\" type=\"text/css\">",
                     "<script src=\"/js/less.js\" type=\"text/javascript\"></script>"],

    lists:map(fun(Include) ->
        dict:from_list([{'tag', Include}])
      end, Head_Includes).

%% For testing
read(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} -> Binary;
        {error, _}   -> <<"">>
    end.

hexstring(<<X:128/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [X])).

rps_enabled() ->
    {ok, Rps_Options} = case application:get_env(popcorn, rps_tracking) of
                            undefined ->  {ok, [{enabled, false}]};
                            Rps_Config -> Rps_Config
                        end,
    proplists:get_value(enabled, Rps_Options).
