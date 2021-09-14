-module(event_server).

-record(state, {clients, events}).
-record(event, {name="", description="",pid, timeout={{1970,1,1},{0,0,0}}}).

-compile(export_all).

init() ->
    %% This place could be used to load info from a file for example.
    loop(#state{clients=orddict:new(), events=orddict:new()}).

loop(S = #state{}) ->
    receive
        {Pid, MsgRef, {subscribe, Client}} ->
            %% Add Client to the Client's list
            Ref = erlang:monitor(process, Client),
            NewClients = orddict:store(Ref, Client, S#state.clients),
            Pid ! {MsgRef, ok},
            loop(S#state{clients = NewClients});
        {Pid, MsgRef, {add, Name, Description, Timeout}} ->
            %% Add event
            case is_valid_datetime(Timeout) of
                true ->
                    % Create a new event and link with the server
                    EventPid = event:start_link(Name, Timeout),
                    % Save event in dictionary
                    NewEvents = orddict:store(Name, #event{name=Name, 
                                                           description=Description, 
                                                           pid=EventPid,
                                                           timeout=Timeout}, S#state.events),
                    
                    Pid ! {MsgRef, ok},
                    loop(S#state{events=NewEvents});
                false ->
                    %% Not a valid date and time format
                    %% Send bad timeout message to the caller
                    Pid ! {MsgRef, {error, bad_timeout}},
                    %% loop again
                    loop(S)
            end;
        {Pid, MsgRef, {cancel, Name}} ->
            %% Cancel the event Name
            NewEvents = case orddict:find(Name) of
                            %% Event exists in events dictionary
                            {ok, E} ->
                                %% cancel the event
                                event:cancel(E#event.pid),
                                %% remove from dictionary (returns a new events state)
                                orddict:erase(Name, S#state.events);
                            error -> 
                                %% not exists already... return the events state
                                S#state.events
                        end,
            %% Send response to caller
            Pid ! {MsgRef, ok},
            loop(S#state{events=NewEvents});
        {done, Name} ->
            %% event timeout elapsed
            case orddict:find(Name) of
                {ok, E} ->
                    %% Report all clients subcribed
                    report_clients({done, E#event.name, E#event.description}, S#state.clients),
                    %% Remove event from list
                    NewClients = orddict:erase(Name),
                    loop(S#state{events=NewClients});
                error ->
                    loop(S)
            end;
        shutdown -> 
            ok;
        {'DOWN', Ref, process, _Pid, _Reason} ->
            ok;
        code_change ->
            ok;
        Unknown -> 
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(S)
    end.

%% Private functions
is_valid_datetime({Date,Time}) ->
    try
        calendar:valid_date(Date) andalso valid_time(Time)
    catch
        error:function_clause -> false
    end.

valid_time({H,M,S}) -> valid_time(H,M,S).

valid_time(H, M, S) when H >=0, H <24, M >=0, M <60, S >=0, S <60 ->
    true;
valid_time(_,_,_) -> false.

%% For each client subcribed in the server, notify them with Msg
report_clients(Msg, ClientsDict) ->
    orddict:map(fun({_Ref, Client}) -> Client ! Msg end, ClientsDict).
