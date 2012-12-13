-module(view_dashboard).
-author('marc.e.campbell@gmail.com').

-include("include/popcorn.hrl").

-export([head_includes/0,
         node_count/0,
         event_count/0,
         alerts/0,
         hashtag_count/0,
         mention_count/0,
         alert_count_today/0,
         alert_count/0,
         known_nodes/0,
         username/0]).

-spec head_includes() -> list().
head_includes() -> popcorn_util:head_includes().

-spec node_count() -> integer().
node_count() -> length(ets:tab2list(current_nodes)).

-spec event_count() -> integer().
event_count() -> folsom_metrics:get_metric_value(?TOTAL_EVENT_COUNTER).

-spec hashtag_count() -> integer().
hashtag_count() -> 0.

-spec mention_count() -> integer().
mention_count() -> 0.

-spec alert_count_today() -> integer().
alert_count_today() -> 0.

-spec alert_count() -> integer().
alert_count() -> 0.

alerts() ->
    [begin
        [Counter_Name,Line] = string:tokens(Counter, ":"),
        #alert{node=#popcorn_node{version=Version}} = gen_event:call(triage_handler, triage_handler, {data, "tt_support:220"}),
        Node_Properties = [{'name',  Counter_Name},
                           {'line',  Line},
                           {'count', Num},
                           {'node_name', list(Version)},
                           {'node',  Num}],
        dict:from_list(Node_Properties)
     end || {Counter, Num} <- gen_event:call(triage_handler, triage_handler, {alerts})
    ].

list(B) when is_binary(B) -> binary_to_list(B);
list(_) -> "".

-spec known_nodes() -> list().
known_nodes() ->
    Total_Message_Count = folsom_metrics:get_metric_value(?TOTAL_EVENT_COUNTER),
    lists:map(fun({Node, Pid}) ->
        Message_Counts  = gen_fsm:sync_send_event(Pid, get_message_counts),
        Node_List       = binary_to_list(Node),
        Node_Properties = [{'node_name',             Node_List},
                           {'total_messages',        proplists:get_value(total, Message_Counts, 0)},
                           {'percent_of_all_events', ?PERCENT(proplists:get_value(total, Message_Counts, 0) / Total_Message_Count)},
                           {'alert_count',           0},
                           {'hashtag_count',         0},
                           {'mention_count',         0}],
        dict:from_list(Node_Properties)
      end, ets:tab2list(current_nodes)).

-spec username() -> string().
username() -> "marc".
