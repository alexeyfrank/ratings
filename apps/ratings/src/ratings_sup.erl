-module(ratings_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Threshold } = application:get_env(threshold),

    Workers = get_ratings_server(total, Threshold) ++
              get_ratings_server(weekly, Threshold) ++
              get_ratings_server(daily, Threshold),

    {ok,{{ one_for_one, 5, 10}, Workers}}.


get_ratings_server(Type, Threshold) ->
    ServerName = list_to_atom(atom_to_list(Type) ++ "_ratings_server"),
    StatServerName = list_to_atom(atom_to_list(Type) ++ "_ratings_stat_server"),
    RatingServer = {ServerName,
                    {ratings_server, start_link, [#{ type => Type, threshold => Threshold }]},
                    permanent, 2000, worker, [ratings_server]},
    RatingStatServer = {StatServerName,
                        {ratings_stat_server, start_link, [#{ type => Type }]},
                        permanent, 2000, worker, [ratings_stat_server]},
    [RatingServer, RatingStatServer].
