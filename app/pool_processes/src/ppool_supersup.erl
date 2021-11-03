-module(ppool_supersup).
-behavior(supervisor).
%% API
-export([start_link/0, stop/0, start_pool/3, stop_pool/1]).
-export([init/1]).


start_link() ->
  io:format("ppool_supersup:: start_link~n"),
  supervisor:start_link({local, ppool}, ?MODULE, []).

stop() ->
  io:format("ppool_supersup:: stop pid ~p~n", [whereis(ppool)]),
  case whereis(ppool) of
    P when is_pid(P) ->
      exit(P, kill);
    _ -> ok
  end.

start_pool(Name, WorkersLimit, MFA) ->
  ChildSpec = {Name,
              {ppool_sup, start_link, [Name, WorkersLimit, MFA]}, %% How to start the Child (MFA)
              permanent,
              10500, %% shutdown time
              supervisor,
              [ppool_sup]},
  supervisor:start_child(ppool,ChildSpec).

stop_pool(Name) ->
  supervisor:terminate_child(ppool, Name),
  supervisor:delete_child(ppool, Name).

init([]) ->
  MaxRestartAttempts = 6,
  MaxTime = 3600,
  io:format("ppool_supersup:: init with parameters MaxRestartAttempts: ~p MaxTime: ~p~n", [MaxRestartAttempts, MaxTime]),
  {ok, { {one_for_one, MaxRestartAttempts, MaxTime}, []} }.