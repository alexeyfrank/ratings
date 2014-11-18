-module(rating_stat_handler).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).

% content funs
-export([display/2]).

%%
%% Cowboy REST callbacks
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, Context) ->
    {[<<"GET">>], Req, Context}.

content_types_provided(Req, Context) ->
    CTA = [{{<<"application">>, <<"json">>, '*'}, display}],
    {CTA, Req, Context}.

display(Req0, Context) ->
    Type = binary_to_atom(cowboy_req:binding(type, Req0), utf8),
    Stat = ratings_manager:get_stat(Type),
    {stat_to_json(Stat), Req0, Context}.

stat_to_json(Stat) ->
    StatHash = lists:map(fun({User, Events}) ->
                                 {[{user, User},
                                   {events, events_to_json(Events)}]}
                         end, Stat),
    jiffy:encode(StatHash).

events_to_json(Events) ->
    lists:map(fun({Score, Time}) ->
                      {[{score, Score},
                        {time, Time}]}
              end, Events).


