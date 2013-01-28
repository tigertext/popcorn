-module(json_util).
-author('mhald@mac.com').

-export([get_path/2, get_path/3]).

% @doc utility method to recurse and get values out of JSON structures 
%
% Called like:
%    json_util:get_path(jiffy:decode(<<"{\"foo\": {\"bar\": \"baz\"}}">>), [<<"foo">>, <<"bar">>]).
%    <<"baz">>
%
%    json_util:get_path(jiffy:decode(<<"{\"foo\": {\"bar\": \"baz\"}}">>), [<<"foo">>, <<"qux">>]).
%    undefined
%
%    json_util:get_path(jiffy:decode(<<"{\"foo\": [\"bar\", \"baz\"]}">>), [<<"foo">>]).
%    [<<"bar">>,<<"baz">>]
%
get_path(Decoded_Json, Path) ->
    get_path(Decoded_Json, Path, undefined).

get_path(Decoded_Json, Path, Default) ->
    {List} = Decoded_Json,
    Fields = recurse(List, Path, []),
    {_,Found_Value} = lists:nth(length(Fields), Fields),
    case Found_Value of
        undefined -> Default;
        _ -> Found_Value
    end.

recurse(_, [], Acc) -> Acc;
recurse(List, [H|T], Acc) ->
    Entry = proplists:get_value(H, List, undefined),
    Next_Entry2 = case Entry of
        [] -> [];
        {L} -> L;
        Bin -> Bin
    end,
    recurse(Next_Entry2, T, Acc ++ [{H, Entry}]).
