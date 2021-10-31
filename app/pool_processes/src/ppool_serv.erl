-module(ppool_serv).
-behavior(gen_server).

-define(SPEC(MFA), {worker_sup, {ppool_worker_sup, start_link, [MFA]}, temporary, 10000, supervisor, [ppool_worker_sup]}).
-record(state, {limit = 0, sup, refs, queue = queue:new()}).


%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
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
  {reply, noalloc, S}.

handle_cast(Request, State) ->
  erlang:error(not_implemented).

handle_info({start_worker_supervisor, Sup, MFA}, S=#state{}) ->
  {ok, Pid} = supervisor:start_child(Sup,?SPEC(MFA)),
  link(Pid),
  {no_reply, S#state{sup = Pid}};

handle_info(Msg, State) ->
  io:format("Unknown message: ~p~n", [Msg]),
  {no_reply, State}.