-module(event).

-compile(export_all).

-define(MAX_DELAY, 49*24*60*60). %% 49 days in seconds

-record(state, {server, name="", to_go=0}).

start(EventName, Delay) ->
    spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
    spawn_link(?MODULE, init, [self(), EventName, Delay]).

init(Server, EventName, DateTime) ->
    loop(#state{server=Server, name=EventName, to_go=time_to_go(DateTime)}).

loop(S = #state{server = Server, to_go=[Delay|OtherDelays]}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after Delay * 1000 ->
        if OtherDelays =:= [] ->
            Server ! {done, S#state.name};
           OtherDelays =/= [] ->
               loop(#state{to_go=OtherDelays})
        end

    end.

cancel(Pid) ->
    %% create a monitor to check it the Pid event still alive or not
    Ref = erlang:monitor(process,Pid),
    Pid ! {self(), Ref, cancel},
    receive
        {Ref, ok} -> 
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, _Reason} -> 
            ok
    end.

%% internal functions
normalize(Seconds) ->
    [Seconds rem (?MAX_DELAY) | lists:duplicate(Seconds div (?MAX_DELAY), (?MAX_DELAY))].

time_to_go(Timeout = {{_,_,_},{_,_,_}}) -> 
    Now = calendar:local_time(),
    ToGo = calendar:datetime_to_gregorian_seconds(Timeout) - calendar:datetime_to_gregorian_seconds(Now),
    Seconds = if ToGo > 0 -> ToGo; 
                 ToGo =< 0 -> 0 
              end,
    normalize(Seconds).  
