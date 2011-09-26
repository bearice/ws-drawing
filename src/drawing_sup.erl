-module(drawing_sup).
-author("Bearice Ren <bearice@gmail.com>").

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Web = http_specs(get_config(http,[])),
    Room = room_specs(),
    Processes = [Web,Room],
    Strategy = {one_for_one, 10, 10},
    {ok,{Strategy, lists:flatten(Processes)}}.

get_config(Key,Default)->
    case application:get_key(Key) of
        undefined ->
            Default;
        Any ->
            Any
    end.

http_specs(Config) ->
    HttpDefaults = lists:keysort(1,[
        {module,drawing_ws},
        {port,8080}
    ]),
    HttpConfig = lists:keysort(1,Config),
    RunningConfig = lists:keymerge(1,HttpConfig,HttpDefaults),
    {http_server,
     {http_server, start, [RunningConfig]},
     permanent, 5000, worker, dynamic}.

room_specs() ->
    {drawing_room,
        {drawing_room, start_link, [[]]},
        permanent, 5000, worker, [drawing_room]}.
