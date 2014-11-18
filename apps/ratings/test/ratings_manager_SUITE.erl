-module(ratings_manager_SUITE).
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
    application:set_env(ratings, ratings, [{ daily, {daily, {6, 0, am}}},
                                           { weekly, {weekly, {6, 0, am}}},
                                           { monthly, {monthly, 1, {6, 0, am}}}]),
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
    ok = ratings_manager:set_user_score(UserId, Score),
    ok = ratings_manager:set_user_score(UserId, Score),
    ok = ratings_manager:set_user_score(UserId, Score),
    [{UserId, Score}] = ratings_manager:get_rating(daily),
    [{UserId, Score}] = ratings_manager:get_rating(weekly),
    [{UserId, Score}] = ratings_manager:get_rating(total),
    ok.
