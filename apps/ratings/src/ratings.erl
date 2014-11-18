-module(ratings).

-export([set_user_score/2, get_rating/1, get_user_stat/2]).
-export([start/0, stop/0, restart/0]).

%%%% Api
set_user_score(UserId, Score) -> ratings_manager:set_user_score(UserId, Score).
get_rating(Type) -> ratings_manager:get_rating(Type).
get_user_stat(UserId, Type) -> ratings_manager:get_user_stat(UserId, Type).


%%%% OTP Callbacks

applications() ->
    [gproc, mavg, crypto, ranch, cowlib, cowboy, ratings].

start() ->
    ok = lager:start(),
    ok = sync:go(),
    [ok = application:ensure_started(App) || App <- applications()].

stop() ->
    [ok = application:stop(App) || App <- applications()].

restart() ->
    stop(),
    start().
