-module(erws).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.

start() ->
    ensure_started(crypto),
    ensure_started(cowboy),
    ensure_started(gproc),
    application:start(erws).

stop() ->
    application:stop(erws).
