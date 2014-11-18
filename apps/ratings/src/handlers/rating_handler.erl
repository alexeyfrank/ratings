-module(rating_handler).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
% -export([content_types_accepted/2]).

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
    Rating = ratings_manager:get_rating(Type),
    {to_json(Rating), Req0, Context}.


to_json(Rating) ->
    RatingHash = lists:map(fun({User, Score}) ->
                            {[{user_id, User},
                              {score, Score}]}
                           end, Rating),
    jiffy:encode(RatingHash).

