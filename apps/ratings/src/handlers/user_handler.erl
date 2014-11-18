-module(user_handler).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

% content funs
-export([add_user_score/2]).

%%
%% Cowboy REST callbacks
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, Context) ->
    {[<<"PUT">>], Req, Context}.

content_types_provided(Req, Context) ->
    CTA = [{{<<"application">>, <<"json">>, '*'}, undef}],
    {CTA, Req, Context}.

content_types_accepted(Req, Context) ->
    CTA = [{{<<"application">>, <<"json">>, '*'}, add_user_score}],
    {CTA, Req, Context}.

add_user_score(Req0, Context) ->
    UserId = cowboy_req:binding(user_id, Req0),
    Score = binary_to_integer(cowboy_req:binding(score, Req0)),

    ratings_manager:set_user_score(UserId, Score),
    {true, Req0, Context}.
