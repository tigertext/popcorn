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

start_phase(deserialize_mnesia, _Start_Type, _Phase_Args) ->
    io:format("Reloading previously known nodes...\n"),
    lists:foreach(fun(Known_Node) ->
        io:format("Node: ~s\n", [binary_to_list(Known_Node)]),
        Popcorn_Node = lists:nth(1, mnesia:dirty_read(known_nodes, Known_Node)),
        {ok, Pid} = supervisor:start_child(node_sup, []),
        ok = gen_fsm:sync_send_event(Pid, {deserialize_popcorn_node, Popcorn_Node}),
        ets:insert(current_nodes, {Popcorn_Node#popcorn_node.node_name, Pid})
      end, mnesia:dirty_all_keys(known_nodes)),
    io:format(" done!\n").

init([]) ->
    io:format("CWD: ~p\n", [filename:absname("")]),

    {ok, _} = gen_event:start({local, triage_handler}),
    gen_event:add_handler(triage_handler, triage_handler, []),

    io:format("Creating ets tables..."),
    current_connected_users =   ets:new(current_connected_users,  [named_table, set, public]),
    current_nodes =             ets:new(current_nodes,            [named_table, set, public]),
    current_log_streams =       ets:new(current_log_streams,      [named_table, set, public, {keypos, #stream.stream_id}]),
    current_dashboard_streams = ets:new(current_dashboard_streams,[named_table, set, public, {keypos, #stream.stream_id}]),
    current_roles =             ets:new(current_roles,            [named_table, bag, public]),
    io:format(" done!\n"),

    %% ensure we have a mnesia schema created
    io:format("Starting mnesia..."),
    stopped = mnesia:stop(),
    case mnesia:create_schema([node()]) of
      ok -> io:format(" initializing schema...");
      {error, {_Node, {already_exists,_Node}}} -> io:format(" recovering schema...")
    end,
    ok = mnesia:start(),
    io:format(" done!\n"),

    io:format("Ensuring required mnesia tables exist..."),
    mnesia:create_table(known_nodes,  [{disc_copies, [node()]},
                                       {record_name, popcorn_node},
                                       {attributes,  record_info(fields, popcorn_node)}]),
    io:format(" done!\n"),

    io:format("Creating global metrics..."),
    folsom_metrics:new_counter(?TOTAL_EVENT_COUNTER),
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
                  {popcorn_server, {popcorn_server, start_link, []}, permanent, 5000, worker, [popcorn_server]},
                  {popcorn_udp,    {popcorn_udp,    start_link, []}, permanent, 5000, worker, [popcorn_udp]},

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




