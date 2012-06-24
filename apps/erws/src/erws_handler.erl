-module(erws_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).

% Behaviour cowboy_http_handler
-export([init/3, handle/2, terminate/2]).

% Behaviour cowboy_http_websocket_handler
-export([
    websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3
]).

-export([broadcast/0]).

% Called to know how to dispatch a new connection.
init({tcp, http}, Req, _Opts) ->
    lager:debug("Request: ~p", [Req]),
    % "upgrade" every request to websocket,
    % we're not interested in serving any other content.
    {upgrade, protocol, cowboy_http_websocket}.

% Should never get here.
handle(Req, State) ->
    lager:debug("Unexpected request: ~p", [Req]),
    {ok, Req2} = cowboy_http_req:reply(404, [
        {'Content-Type', <<"text/html">>}
    ]),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

-record(ws, {tref}).

-define(BC, bc).

% Called for every new websocket connection.
websocket_init(_Any, Req, []) ->
    lager:debug("New client"),
    gproc:reg({p, l,{?MODULE, ?BC}}),
    TRef = timer:apply_interval(1000, ?MODULE, broadcast, []),
    State = #ws{tref=TRef},
    Req2 = cowboy_http_req:compact(Req),
    {ok, Req2, State, hibernate}.

% Called when a text message arrives.
websocket_handle({text, Msg}, Req, State) ->
    lager:debug("Received: ~p", [Msg]),
    {reply,
        {text, << "Responding to ", Msg/binary >>},
        Req, State, hibernate
    };
websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({_Pid, {_Module, ?BC}, Msg}, Req, State) ->
    % catch gproc:send and forward Msg to client
    {reply, {text, Msg}, Req, State, hibernate};

% Other messages from the system are handled here.
websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, State) ->
    {ok, cancel} = timer:cancel(State#ws.tref),
    ok.

js_timestamp() ->
    % format of javascript's Date.getTime()
    {Megasec, Sec, Microsec} = now(),
    Milliseconds_since_1970 = (((Megasec * 1000000) + Sec) * 1000) + round(Microsec / 10),
    Milliseconds_since_1970.

broadcast() ->
    Ts = js_timestamp(),
    N = crypto:rand_uniform(0, 10000) / 10000,
    Json = lists:flatten(io_lib:format(
        "{\"ts\":~B,\"n\":~.2f}\n",
        [Ts, N])),
    gproc:send({p, l, {?MODULE, ?BC}},
        {self(), {?MODULE, ?BC}, list_to_binary(Json)}).

