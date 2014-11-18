-module(ratings_server).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

% vvvv API vvvv
start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

% vvvv Gen Server Implementation vvvv
init(#{ type := Type } = Args) ->
    ratings_manager:reg_rating_server(Type),
    State = Args#{ users => dict:new() },
    {ok, State}.

handle_call({ get_rating }, _From, #{ users := Users } = State) ->
    { reply, get_rating(State), State };
handle_call(_Message, _From, State) ->
    { reply, invalid_command, State }.

handle_cast({ set_user_score, User}, State) ->
    send_statistic_event({set_user_score, User}, State),
    {_IsJoin, NewState} = join_to_rating(User, State),
    {noreply, NewState}.

handle_info(_Message, State) -> { noreply, State }.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> { ok, State }.



%%% Private
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

send_statistic_event({set_user_score, Msg }, #{ type := Type }) ->
    StatPid = ratings_manager:get_rating_stat_server(Type),
    gen_server:cast(StatPid, { on_set_user_score, Msg });
send_statistic_event({user_join_in_rating, User}, #{ type := Type }) ->
    StatPid = ratings_manager:get_rating_stat_server(Type),
    gen_server:cast(StatPid, { on_user_join_in_rating, User });
send_statistic_event({user_not_join_in_rating, User}, #{ type := Type }) ->
    StatPid = ratings_manager:get_rating_stat_server(Type),
    gen_server:cast(StatPid, { on_user_not_join_in_rating, User }).
