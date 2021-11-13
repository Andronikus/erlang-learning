-module(erlcount_dispatcher).
-behaviour(gen_fsm).

-record(data, {regex=[], refs=[]}).
-define(POOL, erlcount).

-export([start_link/0, complete/4]).
-export([init/1, handle_info/3, dispatching/2]).


%% Public API
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

complete(Pid, Regex, Ref, Count) ->
    gen_fsm:send_all_state_event(Pid, {complete, Regex, Ref, Count}).

%% Handlers
handle_info({start, Dir}, State, Data) ->
    gen_fsm:send_event(self(), erlcount_lib:find_erl(Dir)),
    {next_state, State, Data}.

handle_event({complete, R, Ref, Matches}, State, Data=#data{regex=Regex, refs=Refs}) ->
    {R, Count} = lists:keyfind(R,1,Regex),
    NewRegex = lists:keyreplace(R, 1, Regex, {R, Count+Matches}),
    NewData = Data#data{regex=NewRegex, refs = Refs--[Ref]},
    case State of
        dispatching -> 
            {next_state, dispatching, Data};
        listening ->
            listening(done,Data)
    end.

dispatching({continue, File, Continue}, Data=#data{regex = Regex, refs=Refs}) ->
    F = fun({R, _Count}, AccRefs) ->
        Ref = make_ref(),
        ppool:asyn_queue(?POOL, [self(), Ref, File, R]),
        [Ref|AccRefs]
    end,
    NewRefs = lists:foldl(F, Refs, Regex),
    gen_fsm:send_event(self(), Continue()),
    {next_state, dispatching, Data#data{refs = NewRefs}},

dispatching(done, Data) ->
    %% when received a done message means that the files look up finished... but no assumption
    %% can be made about the processing response from workers. It means we cannot go to the next state
    %% blind... if all files response already arrives before moving to the new state we will be stuck
    %% in listening state forever because no message will arrive for the new state
    listening(done, Data).

listening(done, Data=#state{refs=[], regex = Regex}) ->
    %% In the case we move to the listening state and all responses from processing files already arrives
    %% (refs is empty) we reach the end of the fsm states
    [io:format("Regex ~p matched ~p times~n",[R,C]) || {R,C} <- Regex],
    {stop, normal, Data}.

listening(done, Data) ->
    %% we still wait for responses from workers... we can move to listening state and responses will be handle
    %% in listening callback
    {next_state, listening, Data}.

init([]) ->
    {ok, Regex} = application:get_env(regex),
    {ok, Dir} = application:get_env(directory),
    {ok, MaxFiles} = application:get_env(maxfiles),
    ppool:start_pool(?POOL, MaxFiles, {erlang_counter, start_link, []}),
    case lists:all(fun valid_regex/1, Regex) of
        true -> 
            self() ! {start, Dir},
            {ok, dispatching, #state{regex = [ {R,0} || R <- Regex]}}
        false ->
            {stop, invalid_regex}
    end.

valid_regex(Reg) ->
    %% run will first compile the Reg and if its malformed or invalid
    %% an error:badarg will be thrown
    try re:run("", Reg) of
        %% Here we are not interested if its a matched or nomatch result
        _ -> true
    catch
        error:badarg -> false   
    end.
