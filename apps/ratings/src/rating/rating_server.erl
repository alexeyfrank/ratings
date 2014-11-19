-module(rating_server).

-behaviour(gen_server).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

% vvvv API vvvv
start_link(Name, RestartSpec, Threshold) ->
    gen_server:start_link(?MODULE, [Name, RestartSpec, Threshold], []).

% vvvv Gen Server Implementation vvvv
init([Name, RestartSpec, Threshold]) ->
    ratings_manager:reg_rating_server(Name),
    State = #{ users => dict:new(),
               name => Name,
               threshold => Threshold,
               restart_spec => RestartSpec },
    {ok, State}.

handle_call({ get_rating }, _From, State) ->
    { reply, get_rating(State), State };
handle_call(_Message, _From, State) ->
    { reply, invalid_command, State }.

handle_cast({ flush }, State) ->
    { noreply, flush_state(State) };
handle_cast(_Msg, State) ->
    { noreply, State }.

handle_info({ set_user_score, User}, State) ->
    {_IsJoin, NewState} = join_to_rating(User, State),
    {noreply, NewState};
handle_info(_Message, State) ->
    { noreply, State }.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> { ok, State }.

%%% Private
flush_state(OldState) ->
    OldState#{ users => dict:new() }.

get_user_score(UserId, #{users := Users}) ->
    case dict:find(UserId, Users) of
        error -> undefined;
        { ok, Value } -> Value
    end.

join_to_rating({UserId, Score}, #{ users := Users, threshold := Threshold } = State) ->
    {IsJoin, NewUsers} = case Score >= Threshold of
                             true -> {true, dict:store(UserId, Score, Users)};
                             _ -> {false, Users}
                         end,
    {IsJoin, State#{ users => NewUsers }}.

get_rating(#{ users := Users } = State) ->
    List = dict:to_list(Users),
    lists:sort(fun({_, ScoreA}, {_, ScoreB}) -> ScoreA >= ScoreB end, List).
