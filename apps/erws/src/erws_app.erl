-module(erws_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% {Host, list({Path, Handler, Opts})}
    %% Dispatch the requests (whatever the host is) to
    %% erws_handler, without any additional options.
    Dispatch = [{'_', [
        {'_', erws_handler, []}
    ]}],
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    %% Listen in 10100/tcp for http connections.
    cowboy:start_listener(erws_websocket, 100,
        cowboy_tcp_transport, [{port, 10100}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ),
    erws_sup:start_link().

stop(_State) ->
    ok.

