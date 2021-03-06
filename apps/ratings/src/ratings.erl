-module(ratings).

-export([start/0, stop/0]).

applications() ->
    [gproc, mavg, crypto, ranch, cowlib, cowboy, erlcron, ratings].

start() ->
    ok = lager:start(),
    ok = sync:go(),
    [ok = application:ensure_started(App) || App <- applications()].

stop() ->
    [ok = application:stop(App) || App <- applications()].
