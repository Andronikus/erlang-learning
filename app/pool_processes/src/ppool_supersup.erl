-module(ppool_supersup).
-behavior(supervisor).
%% API
-export([start_link/0, stop/0, start_pool/3, stop_pool/1]).
-export([init/1]).


start_link() ->
  supervisor:start_link({local, ppool}, ?MODULE, []).

stop() ->
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
  {ok, { {one_for_one, MaxRestartAttempts, MaxTime}, []} }.