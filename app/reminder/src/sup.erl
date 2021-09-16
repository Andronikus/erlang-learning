-module(sup).

-export([start/2, start_link/2, init/1, loop/1]).

start(Module, Args) ->
    spawn(?MODULE, init, [{Module, Args}]).

start_link(Module, Args) -> 
    spawn_link(?MODULE, init, [{Module, Args}]).

init({Module, Args}) ->
    process_flag(trap_exit, true),
    loop({Module, start_link, Args}).

loop({Module, Function, Args}) ->
    Pid = apply(Module, Function, Args),
    receive
        {'EXIT', _From, shutdown} ->
            %% Kill the supervisor as the child too
            exit(shutdown);
        {'EXIT', Pid, Reason } ->
            io:format("Process ~p exited with reason ~p~n",[Pid, Reason]),
            loop({Module,Function,Args})
    end.




