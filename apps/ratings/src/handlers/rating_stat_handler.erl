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
    UserId = cowboy_req:binding(user_id, Req0),
    Stat = ratings:get_user_stat(UserId, Type),
    {to_json(Stat), Req0, Context}.

to_json(Stat) ->
    StatHash = lists:map(fun({Score, Time}) ->
                            {[
                                {score, Score},
                                {time, Time}
                             ]}
                           end, Stat),
    jiffy:encode(StatHash).

