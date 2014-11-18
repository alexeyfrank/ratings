-module(rating_stat_server).

-behaviour(gen_server).

-export([start_link/3]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-define(YEAR_IN_SECONDS, 86400).

% vvvv API vvvv
start_link(Name, RestartSpec, Threshold) ->
    gen_server:start_link(?MODULE, [Name, RestartSpec, Threshold], []).

% vvvv Gen Server Implementation vvvv
init([Name, RestartSpec, Threshold]) ->
    ratings_manager:reg_rating_stat_server(Name),
    State = #{ name => Name,
               threshold => Threshold,
               restart_spec => RestartSpec,
               users => dict:new(),
               requests_counter => jn_mavg:new_mavg(300)},
    {ok, State}.

handle_call({ get_stat }, _From, State) ->
    { reply, get_stat(State), State };
handle_call(_Message, _From, State) ->
    { reply, invalid_command, State }.

handle_cast(_Msg, State) ->
    { noreply, State }.

handle_info({ set_user_score, User }, State) ->
    State2 = find_or_create_user_event(User, State),
    State3 = bump_total_requests_counter(State2),
    {noreply, State3};
handle_info(_Message, State) -> { noreply, State }.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> { ok, State }.

%%% private
get_total_requests_count(Period, #{ requests_counter := RequestCounter }) ->
    jn_mavg:getEventsPer(RequestCounter, Period).

get_stat(State) ->
    #{ users := Users } = State,
    dict:to_list(Users).

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
