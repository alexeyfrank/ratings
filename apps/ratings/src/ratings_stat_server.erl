-module(ratings_stat_server).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-define(YEAR_IN_SECONDS, 86400).

% vvvv API vvvv
start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

% vvvv Gen Server Implementation vvvv
init(#{ type := Type } = Args) ->
    ratings_manager:reg_rating_stat_server(Type),
    State = Args#{
                users => dict:new(),
                requests_counter => jn_mavg:new_mavg(300)
             },
    {ok, State}.

handle_call({ get_requests_count }, _From, State) ->
    { reply, get_total_requests_count(?YEAR_IN_SECONDS, State), State };
handle_call({ get_requests_count, Period }, _From, State) ->
    { reply, get_total_requests_count(Period, State), State };
handle_call({ get_user_stat, UserId }, _From, State) ->
    { reply, get_user_stat(UserId, State), State };
handle_call(_Message, _From, State) ->
    { reply, invalid_command, State }.

handle_cast({ on_set_user_score, User }, State) ->
    State2 = find_or_create_user_event(User, State),
    State3 = bump_total_requests_counter(State2),
    {noreply, State3}.

handle_info(_Message, State) -> { noreply, State }.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> { ok, State }.

%%% private
get_total_requests_count(Period, #{ requests_counter := RequestCounter }) ->
    jn_mavg:getEventsPer(RequestCounter, Period).

get_user_stat(UserId, #{ users := Users }) ->
    case dict:find(UserId, Users) of
        error -> [];
        { ok, Value } -> Value
    end.

find_or_create_user_event({UserId, Score}, #{ users := Users } = State) ->
    NewEvent = [{Score, unixtime()}],
    NewUsers = case dict:find(UserId, Users) of
        error -> dict:store(UserId, NewEvent, Users);
        { ok, EventsList } -> dict:store(UserId, EventsList ++ NewEvent, Users)
    end,
    State#{ users => NewUsers }.

bump_total_requests_counter(#{ requests_counter := RequestCounter } = State) ->
    State#{ requests_counter := jn_mavg:bump_mavg(RequestCounter, 1) }.

unixtime() ->
    { Mega, Secs, _ } = now(),
     Mega * 1000000 + Secs.
