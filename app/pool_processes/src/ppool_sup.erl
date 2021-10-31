-module(ppool_sup).
-behavior(supervisor).

%% API
-export([start_link/3, init/1]).

start_link(Name, WorkersLimit, MFA) ->
  io:format("ppool_sup:: start "),
  supervisor:start_link(?MODULE, {Name, WorkersLimit, MFA}).

init({Name, WorkerLimit, MFA}) ->
  io:format("ppool_sup:: init "),
  MaxRestartAttempts = 1,
  MaxTime = 3600,
  {ok, { one_for_all, MaxRestartAttempts, MaxTime},
    %% Child Specs
    [
      {serv,
        {ppool_serv, start_link, [Name, WorkerLimit, self(), MFA]},
        permanent,
        5000, %% Shutdown time
        worker,
        [ppool_serv]
      }
    ]}.
