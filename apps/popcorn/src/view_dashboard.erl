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
         alert_lines/0,
         known_nodes/0,
         username/0]).

-spec alert_lines() -> pos_integer().
alert_lines() ->
  case application:get_env(popcorn, dashboard_alert_lines) of
    {ok, Value} -> Value;
    _ -> 3
  end.

-spec head_includes() -> list().
head_includes() -> popcorn_util:head_includes().

-spec node_count() -> integer().
node_count() -> ets:info(current_nodes, size).

-spec event_count() -> integer().
event_count() -> folsom_metrics:get_metric_value(?TOTAL_EVENT_COUNTER).

-spec hashtag_count() -> integer().
hashtag_count() -> 0.

-spec mention_count() -> integer().
mention_count() -> 0.

-spec alert_count_today() -> integer().
alert_count_today() -> triage_handler:alert_count_today().

-spec alert_count() -> integer().
alert_count() -> triage_handler:alert_count().

alerts() -> [dict:from_list(Alert) || Alert <- triage_handler:recent_alerts(alert_lines())].

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