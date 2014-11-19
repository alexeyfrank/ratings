-module(rating_timer).
-behaviour(gen_server).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Name, RestartSpec, Threshold) ->
    gen_server:start_link(?MODULE, [Name, RestartSpec, Threshold], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Name, RestartSpec, Threshold]) ->
    JobRef = start_job(Name, RestartSpec),
    {ok, #{ job_ref => JobRef }}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%% Private
start_job(Name, RestartSpec) ->
    erlcron:cron({ RestartSpec, fun(_, _) -> ratings_manager:flush(Name) end }).

stop_job(Ref) ->
    erlcron:cancel(Ref).

