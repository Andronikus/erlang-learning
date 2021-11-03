-module(ppool_worker_sup).
-export([start_link/1, init/1]).
-behaviour(supervisor).

start_link(MFA = {_,_,_}) ->
  io:format("ppool_worker_sup:: start_link~n"),
  supervisor:start_link(?MODULE, MFA).

%init({M,F,A}) ->
%  io:format("ppool_worker_sup:: init with params M:~p, F:~p, A:~p~n", [M,F,A]),
%  {ok, {{simple_one_for_one, 5, 3600},
%    [{ppool_worker,
%      {M,F,A},
%      temporary, 5000, worker, [M]}]}}.

init({M,F,A}) ->
  io:format("ppool_worker_sup:: new~n"),
  {ok, {{simple_one_for_one, 5, 3600}, [{ppool_worker, {M,F,A}, temporary, 5000, worker, [M]}]}}.