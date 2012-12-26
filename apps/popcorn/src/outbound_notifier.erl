-module(outbound_notifier).
-author('elbrujohalcon@inaka.net').

-behavior(supervisor).

-export([start_sup/0, start_handlers_sup/0, init/1]).
-export([start_link/0, notify/2]).

-spec start_sup() -> {ok, pid()}.
start_sup() -> supervisor:start_link({local, outbound_notifier_sup}, ?MODULE, main).

-spec start_handlers_sup() -> {ok, pid()}.
start_handlers_sup() -> supervisor:start_link({local, outbound_notifier_handler_sup}, ?MODULE, handlers).

-spec init(main) -> _.
init(main) ->
    {ok,
     {  {one_for_all, 5, 10},
        [
            {?MODULE, {?MODULE, start_link, []}, permanent, 5000, worker, [?MODULE]},
            {handler_sup, {?MODULE, start_handlers_sup, []}, permanent, 5000, supervisor, []}
        ]}};
init(handlers) ->
    Handlers =
        case application:get_env(popcorn, outbound_notifiers) of
            {ok, Hs} -> Hs;
            _ -> []
        end,
    {ok,
     {{one_for_one, 5, 10},
      [ outbound_notifier_handler:supervisor_spec(Trigger, Module, InitArgs)
        || {Trigger, Module, InitArgs} <- Handlers]}}.

-spec start_link() -> {ok, pid()}.
start_link() -> gen_event:start_link({local, ?MODULE}).

-spec notify(atom(), term()) -> ok.
notify(Event, Data) -> gen_event:notify(?MODULE, {Event, Data}).