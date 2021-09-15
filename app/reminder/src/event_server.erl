-module(event_server).

-record(state, {clients, events}).
-record(event, {name="", description="",pid, timeout={{1970,1,1},{0,0,0}}}).

-define(ANSWER_TIMEOUT, 5000).

-compile(export_all).

start() ->
    register(?MODULE, Pid=spawn(?MODULE, init, [])),
    Pid.

start_link() -> 
    register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
    Pid.

subscribe(Pid) ->
    %% Monitoring the event server loop process
    Ref = erlang:monitor(process, ?MODULE),
    %% Send subcribe message to the server process
    ?MODULE ! {self(), Ref, {subscribe, Pid}},
    receive
        {Ref, ok} -> 
            %% if the subscription was ok, return ok message
            {ok, Ref};
        {'DOWN', Ref, process, _Pid, Reason} -> 
            %% Something went wrong with the event server process. Return the reason
            {error, Reason}
    after ?ANSWER_TIMEOUT ->
        {error, timeout}
    end.

add_event(Name, Description, Timeout) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, Timeout}},
    receive
        %% Forward all the messages to the client even the bad_timeout!
        {Ref, Msg} -> Msg
    after ?ANSWER_TIMEOUT ->
        {error, timeout}
    end.

cance_event(Name) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {cancel, Name}},
    receive
        {Ref, ok} -> ok
    after ?ANSWER_TIMEOUT ->
        {error, timeout}
    end.

terminate() ->
    ?MODULE ! shutdown.


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
            %% shutdown the current process
            exit(shutdown);
        {'DOWN', Ref, process, _Pid, _Reason} ->
            %% As we are monitoring clients... mean a client dies
            %% lets remove it from Clients list
            NewClients = orddict:erase(Ref, S#state.clients),
            loop(S#state{clients=NewClients});
        code_change ->
            %% With this external call to module (?MODULE) code 
            %% will be hot reload in case on a new version available
            ?MODULE:loop(S);
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
