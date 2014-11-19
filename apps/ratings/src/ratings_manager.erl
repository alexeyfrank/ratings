-module(ratings_manager).
-include("include/gproc_macros.hrl").

-define(RATING_SERVERS, { ratings }).
-define(RATING_SERVER_NAME(Name), { rating_server, Name }).
-define(RATING_STAT_SERVER_NAME(Name), { rating_stat_server, Name }).

-export([set_user_score/2, get_rating/1, get_stat/1 ]).
-export([ reg_rating_server/1, reg_rating_stat_server/1, flush/1 ]).

% vvvv API vvvv
get_rating(Name) ->
    call(?GET_PID(?RATING_SERVER_NAME(Name)), { get_rating }).
set_user_score(User, Score) ->
    ?PUBLISH(?RATING_SERVERS, { set_user_score, { User, Score }}).
flush(Name) ->
    cast(?GET_PID(?RATING_SERVER_NAME(Name)), { flush }),
    cast(?GET_PID(?RATING_STAT_SERVER_NAME(Name)), { flush }).
get_stat(Name) ->
    call(?GET_PID(?RATING_STAT_SERVER_NAME(Name)), { get_stat }).
reg_rating_server(Name) ->
    ?REG_PID(?RATING_SERVER_NAME(Name)),
    ?SUBSCRIBE(?RATING_SERVERS).
reg_rating_stat_server(Name) ->
    ?REG_PID(?RATING_STAT_SERVER_NAME(Name)),
    ?SUBSCRIBE(?RATING_SERVERS).

call(Pids, Msg) when is_list(Pids) -> lists:map(fun(Pid) -> call(Pid, Msg) end, Pids);
call(Pid, Msg) when is_pid(Pid) -> gen_server:call(Pid, Msg).
cast(Pids, Msg) when is_list(Pids) -> lists:foreach(fun(Pid) -> cast(Pid, Msg) end, Pids);
cast(Pid, Msg) when is_pid(Pid) -> gen_server:cast(Pid, Msg).
