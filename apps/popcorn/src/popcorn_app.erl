-module(popcorn_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         start_phase/3,
         stop/1,
         init/1]).

-include("include/popcorn.hrl").

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
stop(_State) -> ok.

start_phase(deserialize_storage, _Start_Type, _Phase_Args) ->
    ?POPCORN_DEBUG_MSG("#deserialize_storage starting"),
    storage_monitor:start_workers(),
    ?POPCORN_DEBUG_MSG("#deserialize_storage finished"),
    ok.

init([]) ->
    io:format("CWD: ~p\n", [filename:absname("")]),

    io:format("Creating ets tables..."),
    current_connected_users =   ets:new(current_connected_users,  [named_table, set, public]),
    current_nodes =             ets:new(current_nodes,            [named_table, set, public]),
    current_log_streams =       ets:new(current_log_streams,      [named_table, set, public, {keypos, #stream.stream_id}]),
    current_dashboard_streams = ets:new(current_dashboard_streams,[named_table, set, public, {keypos, #stream.stream_id}]),
    current_roles =             ets:new(current_roles,            [named_table, bag, public]),
    io:format(" done!\n"),

    Cache_Ttl = get_cache_ttl(application:get_env(popcorn, template_cache_ttl)),
    pcache_server:start_link(rendered_templates, mustache, compile, 16, Cache_Ttl), 
    io:format("Template cache expire policy is ~p milliseconds~n", [Cache_Ttl]),

    io:format("Starting storage..."),
    {ok, Storage_Options} = application:get_env(popcorn, storage),
    Storage_Module        = list_to_atom("storage_" ++ proplists:get_value(engine, Storage_Options)),
    Storage_Module:pre_init(),
    io:format(" done!\n"),

    io:format("Starting http listener..."),
    {ok, Http_Listen_Port} = application:get_env(popcorn, http_listen_port),
    Http_Dispatch = [{'_', [
                            {[<<"favicon.ico">>],       http_static_handler, []},
                            {[<<"js">>, '...'],         http_static_handler, []},
                            {[<<"css">>, '...'],        http_static_handler, []},
                            {[<<"images">>, '...'],     http_static_handler, []},
                            {[<<"bootstrap">>, '...'],  http_static_handler, []},
                            {[<<"log">>, '...'],        http_log_handler, []},
                            {[<<"dashboard">>, <<"stream">>, '...'],
                                                        http_stream_handler, []},
                            {[<<"node">>, '_'],         http_node_handler, []},
                            {'_',                       http_catchall_handler, []}
                           ]}],

    cowboy:start_http(http_handler, 100, [{port, Http_Listen_Port}], [{dispatch, Http_Dispatch}]),
    io:format(" done!\n"),

    Children = [
                  {popcorn_server,    {popcorn_server,    start_link, []}, permanent, 5000, worker, [popcorn_server]},
                  {popcorn_udp,       {popcorn_udp,       start_link, []}, permanent, 5000, worker, [popcorn_udp]},
                  {triage_handler,    {triage_handler,    start_link, []}, permanent, 5000, worker, [triage_handler]},

                  {outbound_notifier, {outbound_notifier, start_sup,  []}, permanent, 5000, worker, [outbound_notifier]},

                  {history_optimizer,   {history_optimizer,   start_link, []}, permanent, 5000, worker, []},
                  {log_stream_manager,  {log_stream_manager,  start_link, []}, permanent, 5000, worker, []},
                  {system_counters,     {system_counters,     start_link, []}, permanent, 5000, worker, []},
                  {storage_monitor,     {storage_monitor,     start_link, []}, permanent, 5000, worker, []},

                  {storage_sup,          {supervisor, start_link, [{local, storage_sup},          ?MODULE, [Storage_Module]]},       permanent, infinity, supervisor, []},
                  {connected_user_sup,   {supervisor, start_link, [{local, connected_user_sup},   ?MODULE, [connected_user_fsm]]},   permanent, infinity, supervisor, []},
                  {node_sup,             {supervisor, start_link, [{local, node_sup},             ?MODULE, [node_fsm]]},             permanent, infinity, supervisor, []},
                  {log_stream_sup,       {supervisor, start_link, [{local, log_stream_sup},       ?MODULE, [log_stream_fsm]]},       permanent, infinity, supervisor, []},
                  {dashboard_stream_sup, {supervisor, start_link, [{local, dashboard_stream_sup}, ?MODULE, [dashboard_stream_fsm]]}, permanent, infinity, supervisor, []}
               ],

    {ok, { {one_for_one, 10000, 10}, Children} };

init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              {   undefined,
                  {Module,start_link,[]},
                  temporary,
                  2000,
                  worker,
                  []
              }
            ]
        }
    }.

get_cache_ttl({ok, Val}) -> Val;
get_cache_ttl(undefined) -> 43200000. % 12 hours
