-module(ratings_manager).

-define(RATING_SERVER_NAME(Type), { rating_server, Type }).
-define(RATING_STAT_SERVER_NAME(Type), { rating_stat_server, Type }).

-export([set_user_score/2, get_rating/1, get_user_stat/2 ]).

-export([get_rating_servers/0, get_rating_server/1, reg_rating_server/1]).
-export([get_rating_stat_servers/0, get_rating_stat_server/1, reg_rating_stat_server/1]).

% vvvv API vvvv
get_rating(Type) -> call(get_rating_server(Type), { get_rating }).
set_user_score(User, Score) -> cast(get_rating_servers(), { set_user_score, { User, Score }}).

get_user_stat(User, Type) -> call(get_rating_stat_server(Type), { get_user_stat, User }).

get_rating_servers() -> select_pids(gproc:select(get_rating_server_filter())).
get_rating_server(Type) -> gproc:lookup_local_name(?RATING_SERVER_NAME(Type)).
reg_rating_server(Type) -> gproc:add_local_name(?RATING_SERVER_NAME(Type)).

get_rating_stat_servers() -> select_pids(gproc:select(get_rating_stat_server_filter())).
get_rating_stat_server(Type) -> gproc:lookup_local_name(?RATING_STAT_SERVER_NAME(Type)).
reg_rating_stat_server(Type) -> gproc:add_local_name(?RATING_STAT_SERVER_NAME(Type)).


%%% Private
get_rating_server_filter() ->
    GProcKey = {'_', '_', {rating_server, '_'}},
    MatchHead = {GProcKey, '_', '_'},
    [{MatchHead, [], ['$$']}].

get_rating_stat_server_filter() ->
    GProcKey = {'_', '_', {rating_stat_server, '_'}},
    MatchHead = {GProcKey, '_', '_'},
    [{MatchHead, [], ['$$']}].

select_pids(GProcResult) ->
    lists:map(fun([_, Pid, _]) -> Pid end, GProcResult).

call(Pids, Msg) when is_list(Pids) -> lists:map(fun(Pid) -> call(Pid, Msg) end, Pids);
call(Pid, Msg) when is_pid(Pid) -> gen_server:call(Pid, Msg).
cast(Pids, Msg) when is_list(Pids) -> lists:foreach(fun(Pid) -> cast(Pid, Msg) end, Pids);
cast(Pid, Msg) when is_pid(Pid) -> gen_server:cast(Pid, Msg).
