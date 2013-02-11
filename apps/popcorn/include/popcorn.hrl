
-define(MUSTACHE(Filename),          code:priv_dir(popcorn) ++ "/templates/" ++ Filename).

-define(STORAGE_PID,                 pg2:get_closest_pid('storage')).
-define(COUNTER_VALUE(Counter),      system_counters:counter_value(Counter)).

-define(DECREMENT_COUNTER(Counter),  system_counters:decrement(Counter, 1)).
-define(INCREMENT_COUNTER(Counter),  system_counters:increment(Counter, 1)).

-define(RPS_INCREMENT(Metric),   gen_server:cast(rps_manager, {incr, Metric})).

-define(TOTAL_EVENT_COUNTER,         binary_to_atom(<<"total_events">>, latin1)).
-define(TOTAL_ALERT_COUNTER,         binary_to_atom(<<"total_alerts">>, latin1)).
-define(NODE_EVENT_COUNTER(Node),    popcorn_util:node_event_counter(Node)).

-define(PERCENT(Value),              try round(Value * 100 * math:pow(10, 2)) / math:pow(10, 2) catch _:badarith -> 0 end).
-define(NOW,                         folsom_utils:now_epoch_micro()).
-define(POPCORN_DEBUG_MSG(Msg),      io:format("~s\n", [Msg]), lager:debug(Msg)).
-define(POPCORN_INFO_MSG(Msg),       io:format("~s\n", [Msg]), lager:info(Msg)).
-define(POPCORN_NOTICE_MSG(Msg),     io:format("~s\n", [Msg]), lager:notice(Msg)).
-define(POPCORN_WARN_MSG(Msg),       io:format("~s\n", [Msg]), lager:warning(Msg)).
-define(POPCORN_ERROR_MSG(Msg),      io:format("~s\n", [Msg]), lager:error(Msg)).
-define(POPCORN_CRITICAL_MSG(Msg),   io:format("~s\n", [Msg]), lager:critical(Msg)).
-define(POPCORN_ALERT_MSG(Msg),      io:format("~s\n", [Msg]), lager:alert(Msg)).
-define(POPCORN_EMERGENCY_MSG(Msg),  io:format("~s\n", [Msg]), lager:emergency(Msg)).

-define(POPCORN_DEBUG_MSG(Msg, Args),      io:format("~s\n", [lists:flatten(io_lib:format(Msg, Args))]), lager:debug(Msg, Args)).
-define(POPCORN_INFO_MSG(Msg, Args),       io:format("~s\n", [lists:flatten(io_lib:format(Msg, Args))]), lager:info(Msg, Args)).
-define(POPCORN_NOTICE_MSG(Msg, Args),     io:format("~s\n", [lists:flatten(io_lib:format(Msg, Args))]), lager:notice(Msg, Args)).
-define(POPCORN_WARN_MSG(Msg, Args),       io:format("~s\n", [lists:flatten(io_lib:format(Msg, Args))]), lager:warning(Msg, Args)).
-define(POPCORN_ERROR_MSG(Msg, Args),      io:format("~s\n", [lists:flatten(io_lib:format(Msg, Args))]), lager:error(Msg, Args)).
-define(POPCORN_CRITICAL_MSG(Msg, Args),   io:format("~s\n", [lists:flatten(io_lib:format(Msg, Args))]), lager:critical(Msg, Args)).
-define(POPCORN_ALERT_MSG(Msg, Args),      io:format("~s\n", [lists:flatten(io_lib:format(Msg, Args))]), lager:alert(Msg, Args)).
-define(POPCORN_EMERGENCY_MSG(Msg, Args),  io:format("~s\n", [lists:flatten(io_lib:format(Msg, Args))]), lager:emergency(Msg, Args)).

-define(PU, popcorn_util).

-record(stream,  {stream_id       :: string(),
                  max_timestamp   :: number() | undefined,
                  stream_pid      :: pid(),
                  client_pid      :: pid(),
                  applied_filters :: list(),
                  paused          :: boolean()}).

-record(popcorn_node, {node_name :: binary(),
                       role      :: binary(),
                       version   :: binary()}).

-record(log_message, {message_id      :: binary(),
                      severity        :: 0 | 1 | 2 | 4 | 8 | 16 | 32 | 64 | 128,
                      message         :: binary(),
                      timestamp       :: number(),
                      expire_at       :: number(),
                      log_nodename    :: binary(),
                      log_product     :: binary(),
                      log_version     :: binary(),
                      log_module      :: binary(),      %% underscore in the name is to prevent confusion with BIF and types
                      topics     = [] :: list(),
                      identities = [] :: list(),
                      log_function    :: binary(),
                      log_line        :: integer(),
                      log_pid         :: binary()}).

-record(release_scm, {
        key :: binary(),
        role :: binary(),
        version :: binary(),
        checksum :: binary()
        }).

-record(release_scm_mapping, {
        key :: binary(),
        role :: binary(),
        version :: binary(),
        module_name :: binary(),
        url :: binary()
        }).

-record(alert, {
        location,
        log,
        timestamp = erlang:now(),
        incident
}).

-record(alert_key, {
        type,
        key
}).

-record(alert_counter, {
        key,
        value
}).
