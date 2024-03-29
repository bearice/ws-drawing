-module(drawing).
-author("Bearice Ren <bearice@gmail.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the server.
start() ->
    ensure_started(crypto),
    ensure_started(inets),
    application:start(drawing).


%% @spec stop() -> ok
%% @doc Stop the server.
stop() ->
    application:stop(drawing).
