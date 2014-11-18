-module(rating_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link(Name, RestartSpec, Threshold) ->
    supervisor:start_link(?MODULE, [Name, RestartSpec, Threshold]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init(Args) ->
    {ok, {{one_for_one, 5, 10}, [
                                 ?CHILD(rating_server, rating_server, worker, Args),
                                 ?CHILD(rating_stat_server, rating_stat_server, worker, Args)
                                ]}}.
