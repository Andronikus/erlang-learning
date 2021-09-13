-module(linkmon).

-compile(export_all).

myProc() -> 
    timer:sleep(5000),
    exit(reason).

chain(0) ->
    receive
        _ -> ok
    after 4000 ->
        exit("chain dies")
    end;

chain(N) ->
    Pid = spawn(fun() -> chain(N-1) end),
    link(Pid),
    receive
        _ -> ok
    end.

%% -----------------------------
start_critic() -> 
    spawn(?MODULE, restarter, []).

judge(Album, Song) ->
    %% to identify the message
    Ref = make_ref(),
    critic ! {self(), Ref, {Album, Song}},
    receive
        {Ref, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.

restarter() ->
    %% trap exists
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic, []),
    register(critic, Pid),
    Pid = whereis(critic),
    receive
        {'EXIT', Pid, 'normal'} ->
            ok;
        {'EXIT', Pid, 'shutdown'} ->
            shutdown;
        {'EXIT', Pid, _} -> 
            %% Something bad happened.
            io:format("~p was dead ~n", [Pid]),
            restarter()
    end.
     

critic() ->
    receive
        {From, Ref,{"Rage Against the Turing Machine", "Unit Testify"}} ->
            From ! {Ref, "They are greate!"};
        {From, Ref,{"System of down time", "Memoize"}} ->
            From ! {Ref, "They're not Johnny Crash but they're good!"};
        {From, Ref,{"Johnny Crash", "The Token ring of fire"}} ->
            From ! {Ref, "Simply incredible!"};
        {From, Ref,{_Album, _Song}} ->
            From ! {Ref, "They are terrible!"}
    end,
    critic().