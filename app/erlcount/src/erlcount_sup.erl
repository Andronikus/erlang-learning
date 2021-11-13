-module(erlcount_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE,[]).

init([]) ->
    MaxRestart = 5,
    MaxTime = 120,
    {ok, 
        {one_for_one, MaxRestart, MaxTime}, 
        [dispatcher, {erlcount_dispatcher, start_link, []}, transient, 60000, worker, [erlcount_dispatcher]]
    }.

