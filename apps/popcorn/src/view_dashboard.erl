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
event_count() ->
    Event_Count = 
      case mnesia:dirty_read(popcorn_counters, ?TOTAL_EVENT_COUNTER) of
          [{popcorn_counters, _, Total_Message_Count}] -> Total_Message_Count;
          [] -> 0
      end,
    Event_Count.

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
    Total_Message_Count = event_count(),
    lists:map(fun({Node, Pid}) ->
        Node_Event_Count = 
          case mnesia:dirty_read(popcorn_counters, ?NODE_EVENT_COUNTER(Node)) of
              [{popcorn_counters, _, Node_Message_Count}] -> Node_Message_Count;
              [] -> 0
          end,
        Node_List       = binary_to_list(Node),
        Node_Properties = [{'node_name',             Node_List},
                           {'node_hash',             re:replace(base64:encode(Node), "=", "_", [{return, list}, global])},
                           {'total_messages',        Node_Event_Count},
                           {'percent_of_all_events', case Total_Message_Count of
                                                        0 -> 0;
                                                        _ -> ?PERCENT(Node_Event_Count / Total_Message_Count)
                                                     end},
                           {'alert_count',           0},
                           {'hashtag_count',         0},
                           {'mention_count',         0}],
        dict:from_list(Node_Properties)
      end, ets:tab2list(current_nodes)).

-spec username() -> string().
username() -> "admin".
