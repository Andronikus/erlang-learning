-module(bank_sup).

-export([start/0, stop/0]).
-export([init/0]).

start() ->
    Pid = spawn(?MODULE, init, []),
    register(?MODULE, Pid).

stop() -> 
    ?MODULE ! stop.

init() ->
    process_flag(trap_exit, true),
    Pid = bank_atm:start_link(),
    main_loop(Pid).

main_loop(CallerPid) ->
    receive
        {'EXIT', CallerPid, _} ->
            logger:error("Process ~p died! Spawning again", [CallerPid]),
            NewPid = bank_atm:start_link(),
            main_loop(NewPid);
        stop ->
            bank_atm:stop()
    end.