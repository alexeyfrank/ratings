-module(ratings_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/users/:user_id/:score", user_handler, []},
            {"/api/ratings/:type", rating_handler, []},
            {"/api/stat/:type", rating_stat_handler, []}
        ]}
    ]),
    {ok, Port } = application:get_env(port),
    {ok, _} = cowboy:start_http(http_listener, 5,
        [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    ratings_sup:start_link().

stop(_State) ->
    ok.
