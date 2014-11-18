-module(ratings_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================
all() ->
    [set_user_score_test].

init_per_testcase(_TestCase, Config) ->
    application:set_env(ratings, threshold, 20),
    application:set_env(ratings, port, 8008),
    ratings:start(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ratings:stop(),
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================
set_user_score_test(_Config) ->
    { UserId, Score } = { user1, 100 },
    ok = ratings:set_user_score(UserId, Score),
    ok = ratings:set_user_score(UserId, Score),
    [{UserId, Score}] = ratings:get_rating(daily),
    [{UserId, Score}] = ratings:get_rating(weekly),
    [{UserId, Score}] = ratings:get_rating(total),
    ok.
