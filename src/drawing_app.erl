-module(drawing_app).
-author("Bearice Ren <bearice@gmail.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for ffstreaming.
start(_Type, _StartArgs) ->
    drawing_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for ffstreaming.
stop(_State) ->
    ok.
