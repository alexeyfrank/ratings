-module(ratings_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).
%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    { ok, Threshold } = application:get_env(threshold),
    { ok, Ratings } = application:get_env(ratings),
    Children = get_children(Ratings, Threshold),
    {ok,{{ one_for_one, 5, 10}, Children}}.

get_children(Ratings, Threshold) ->
    lists:map(fun({ Name, RestartSpec }) ->
                    ?CHILD(Name, rating_sup, supervisor, [Name, RestartSpec, Threshold])
              end, Ratings).
