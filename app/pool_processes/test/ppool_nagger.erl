-module(ppool_nagger).
-behavior(gen_server).

%% API
-export([start_link/0, start_link/4, stop/1, init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link() ->
  io:format("ppool_nagger:: start link with empty~n"),
  gen_server:start_link(?MODULE, [], []).

start_link(Task, Delay, Max, SendTo) ->
  io:format("ppool_nagger:: start link~n"),
  gen_server:start_link(?MODULE, {Task, Delay, Max, SendTo}, []).

stop(Pid) ->
  gen_server:call(Pid, stop).

init({Task, Delay, Max, SendTo}) ->
  io:format("ppool_nagger:: init"),
  {ok, {Task, Delay, Max, SendTo}, Delay}.

%% handlers
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, {Task, Delay, Max, SendTo}) ->
  SendTo ! {self(), Task},
  io:format("ppool_nagger:: handle_info Max: ~p~n", [Max]),
  if Max =:= infinity ->
       {noreply, {Task, Delay, Max, SendTo}, Delay};
     Max >= 1 ->
       {noreply, {Task, Delay, Max-1, SendTo}, Delay};
     Max =< 0 ->
       {stop, normal, {Task, Delay, Max, SendTo}}
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) -> ok.