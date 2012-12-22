-module(popcorn_util).
-author('marc.e.campbell@gmail.com').

-include("include/popcorn.hrl").

-export([hour/0,
         retention_time_to_microsec/1,
         last_24_hours/0,
         severity_to_number/1,
         number_to_severity/1,
         all_severity_numbers/0,
         random_id/0,
         format_log_message/1,
         opt/2,
         head_includes/0]).

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
severity_to_number(<<"debug">>) -> 7;
severity_to_number(<<"info">>) -> 6;
severity_to_number(<<"notice">>) -> 5;
severity_to_number(<<"warn">>) -> 4;
severity_to_number(<<"error">>) -> 3;
severity_to_number(<<"critical">>) -> 2;
severity_to_number(<<"alert">>) -> 1;
severity_to_number(<<"emergency">>) -> 0;
severity_to_number(_) -> undefined.

number_to_severity(7) -> <<"debug">>;
number_to_severity(6) -> <<"info">>;
number_to_severity(5) -> <<"notice">>;
number_to_severity(4) -> <<"warn">>;
number_to_severity(3) -> <<"error">>;
number_to_severity(2) -> <<"critical">>;
number_to_severity(1) -> <<"alert">>;
number_to_severity(0) -> <<"emergency">>;
number_to_severity(_) -> <<"?">>.

all_severity_numbers() -> lists:seq(0, 7).

random_id() ->
    Length = 64,
    AllowedChars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
    random:seed(now()),
    New_Key = lists:foldl(fun(_, Acc) ->
                  [lists:nth(random:uniform(length(AllowedChars)), AllowedChars)] ++ Acc
                end, [], lists:seq(1, Length)),
    list_to_binary(New_Key).

opt(<<>>, Default)      -> Default;
opt(undefined, Default) -> Default;
opt(Value, _)           -> Value.

format_log_message(#log_message{timestamp=Timestamp, log_module=Module, log_function=Function, log_line=Line, log_pid=Pid,
                                severity=Severity, message=Message, hashtags=Hashtags, mentions=Mentions, log_product=Product,
                                log_version=Version}) ->
  UTC_Timestamp = calendar:now_to_universal_time({Timestamp div 1000000000000, 
                                                  Timestamp div 1000000 rem 1000000,
                                                  Timestamp rem 1000000}),
  {{Year, Month, Day}, {Hour, Minute, Second}} = UTC_Timestamp,
  Formatted_DateTime = lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Minute, Second])),
  Formatted_Time     = lists:flatten(io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B", [Hour, Minute, Second])),

  Find_More_Html     = "<strong>Filter current list to show only messages with matching:</strong><br /><br />" ++
                       "<label class='checkbox popover-label'><input type='checkbox'>Severity: " ++ binary_to_list(popcorn_util:number_to_severity(Severity)) ++ "</label>" ++
                       "<label class='checkbox popover-label'><input type='checkbox'>Module: " ++ binary_to_list(opt(Module, <<"Not set">>)) ++ "</label>" ++
                       "<label class='checkbox popover-label'><input type='checkbox'>Function: " ++ binary_to_list(opt(Function, <<"Not set">>)) ++ "</label>" ++
                       "<label class='checkbox popover-label'><input type='checkbox'>Line: " ++ binary_to_list(opt(Line, <<"?">>)) ++ " in " ++ binary_to_list(opt(Module, <<"not set">>)) ++ "</label>" ++
                       "<label class='checkbox popover-label'><input type='checkbox'>Pid: " ++ binary_to_list(opt(Pid, <<"Not set">>)) ++ "</label>" ++
                       lists:append(["<label class='checkbox popover-label'><input type='checkbox'>@" ++ Mention ++ "</label>" || Mention <- Mentions]) ++
                       lists:append(["<label class='checkbox popover-label'><input type='checkbox'>#" ++ Hashtag ++ "</label>" || Hashtag <- Hashtags]) ++ 
                       "<br /><button class='btn btn-mini' type='button'>Apply Filter</button>",

  [{'time',             Formatted_Time},
   {'datetime',         Formatted_DateTime},
   {'find_more_html',   Find_More_Html},
   {'log_product',      binary_to_list(opt(Product, <<"Unknown">>))},
   {'log_version',      binary_to_list(opt(Version, <<"Unknown">>))},
   {'log_module',       binary_to_list(opt(Module, <<"Unknown">>))},
   {'log_function',     binary_to_list(opt(Function, <<"Unknown">>))},
   {'log_line',         binary_to_list(opt(Line, <<"??">>))},
   {'log_pid',          binary_to_list(opt(Pid, <<"?">>))},
   {'message_severity', binary_to_list(popcorn_util:number_to_severity(Severity))},
   {'message',          binary_to_list(Message)}].

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


