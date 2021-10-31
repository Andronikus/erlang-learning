-module(ppool_serv).
-behavior(gen_server).

-define(SPEC(MFA), {worker_sup, {ppool_worker_sup, start_link, [MFA]}, temporary, 10000, supervisor, [ppool_worker_sup]}).
-record(state, {limit = 0, sup, refs, queue = queue:new()}).


%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([start/4, start_link/4]).
-export([run/2, sync_queue/2, async_queue/2, stop/1]).

start(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
  gen_server:start({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

start_link(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
  gen_server:start_link({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

run(Name, Args) ->
  gen_server:call(Name, {run, Args}).

sync_queue(Name, Args) ->
  gen_server:call(Name, {sync, Args}, infinity).

async_queue(Name, Args) ->
  gen_server:cast(Name, {async, Args}).

stop(Name) ->
  gen_server:call(Name, stop).

init({Limit, MFA, Sup}) ->
  %% Cannot call from here the supervisor:add_child to create the workers supervisor...
  %% pool supervisor in init method (that blocks executions) is creating the pool server process
  %% and wait for its completion... so from pool server we cannot call the supervisor because it will not
  %% treat our call because is waiting for the init method to end its execution. So all the processes will
  %% be waiting for each other (deadlock)

  %% send a message for the server itself that will be executed after the init method ends
  %% (init method will return and the pool_supervisor will be able to receive calls)
  self() ! {start_worker_supervisor, Sup, MFA},
  {ok, #state{limit = Limit, refs = gb_sets:empty()}}.

handle_call({run, Args}, _From, S = #state{limit = N, sup = Sup, refs = Refs}) when N > 0 ->
  %% There is free space for the worker in the pool
  %% Add the child to the workers sup
  {ok, Pid} = supervisor:start_child(Sup,Args),
  Ref = erlang:monitor(process, Pid),
  {reply, {ok, Pid}, S#state{limit = N-1, refs = gb_sets:add(Ref, Refs)}};

handle_call({run, Args}, _From, S = #state{limit = N}) when N =< 0 ->
  %% workers exhausted... reply with a noalloc message!
  {reply, noalloc, S};

handle_call({sync, Args}, _From, S = #state{limit = N, sup = Sup, refs = Refs}) when N > 0 ->
  {ok, Pid} = supervisor:start_child(Sup, Args),
  Ref = erlang:monitor(process, Pid),
  {reply, {ok, Pid}, S#state{limit = N-1, refs = gb_sets:add(Ref,Refs)}};

handle_call({sync, Args}, From, S = #state{limit = N, queue = Q}) when N =< 0 ->
  %% no workers available to process the task... time to enqueue the task
  %% the reply will be sent to the caller after the task dequeue and process.
  %% We need in this case to save the From reference... so we can reply later to the caller
  {noreply, S#state{queue = queue:in({From, Args},Q)}};

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast({async, Args}, S = #state{limit = N, refs = Refs, sup = Sup}) when N > 0 ->
  {ok, Pid} = supervisor:start_child(Sup, Args),
  Ref = erlang:monitor(process, Pid),
  {noreply, S#state{limit = N-1, refs = gb_sets:add(Ref, Refs)}};

handle_cast({async, Args}, S = #state{limit = N, queue = Q}) when N =< 0 ->
  {noreply, S#state{queue = queue:in(Args,Q)}};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, _}, S = #state{refs = Refs}) ->
  io:format("Receives a DOWN message"),
  case gb_sets:is_element(Ref, Refs) of
    true ->
      %% When the DOWN message we receive is from a process we are tracking we
      %% need to process it
      handle_down_worker(Ref, S);
    false ->
      %% The DOWN message we received is not from a process we are tracking
      %% lets move on...
      {noreply, S}
  end;

handle_info({start_worker_supervisor, Sup, MFA}, S=#state{}) ->
  {ok, Pid} = supervisor:start_child(Sup,?SPEC(MFA)),
  link(Pid),
  {no_reply, S#state{sup = Pid}};

handle_info(Msg, State) ->
  io:format("Unknown message: ~p~n", [Msg]),
  {no_reply, State}.

handle_down_worker(Ref, S = #state{limit = Limit, sup = Sup, queue = Q, refs = Refs}) ->
  case queue:out(Q) of
    {{value, {From, Args}}, NewQueue} ->
      %% pop out a new sync task to execute.
      %% Add worker to the supervisor and start the child
      {ok, Pid} = supervisor:start_child(Sup, Args),
      %% Add a monitor to the worker
      NewRef = erlang:monitor(process, Pid),
      %% Remove the Ref of the worker that finish from the Refs controller structure
      %% and add the new one
      NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref, Refs)),
      %% Reply to the caller (Its waiting a reply since we made the call)
      gen_server:reply(From, {ok, Pid}),
      {noreply, S#state{refs = NewRefs, queue = NewQueue}};
    {{value, Args}, NewQueue} ->
      %% pop out a new async task from queue
      {ok, Pid} = supervisor:add_child(Sup, Args),
      NewRef = erlang:monitor(process, Pid),
      NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref, Refs)),
      {noreply, S#state{refs = NewRefs, queue = NewQueue}};
    {empty, _} ->
      %% There is no more tasks in the queue... so nothing to do! Just let know the system
      %% has one more space for a worker
      io:format("Not found more task in the queue! Available workers in the pool: ~p~n",[Limit+1]),
      {noreply, S#state{limit = Limit +1, refs = gb_sets:delete(Ref, Refs)}}
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.