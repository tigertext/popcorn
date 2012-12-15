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
alert_count_today() -> gen_event:call(triage_handler, triage_handler, {alerts_for_today}).

-spec alert_count() -> integer().
alert_count() -> gen_event:call(triage_handler, triage_handler, {total_alerts}).

alerts() ->
    [dict:from_list([{count, Num} | triage_handler:counter_data(Counter)])
     || {Counter, Num} <- gen_event:call(triage_handler, triage_handler, {alerts})
    ].

-spec known_nodes() -> list().
known_nodes() ->
    Total_Message_Count = folsom_metrics:get_metric_value(?TOTAL_EVENT_COUNTER),
    lists:map(fun({Node, Pid}) ->
        Message_Counts  = gen_fsm:sync_send_event(Pid, get_message_counts),
        Node_List       = binary_to_list(Node),
        Node_Properties = [{'node_name',             Node_List},
                           {'node_hash',             re:replace(base64:encode(Node), "=", "_", [{return, list}, global])},
                           {'total_messages',        proplists:get_value(total, Message_Counts, 0)},
                           {'percent_of_all_events', ?PERCENT(proplists:get_value(total, Message_Counts, 0) / Total_Message_Count)},
                           {'alert_count',           0},
                           {'hashtag_count',         0},
                           {'mention_count',         0}],
        dict:from_list(Node_Properties)
      end, ets:tab2list(current_nodes)).

-spec username() -> string().
username() -> "marc".
