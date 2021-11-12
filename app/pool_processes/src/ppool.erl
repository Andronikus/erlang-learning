-module(ppool).

%% API
-export([start/2, stop/1, start_pool/3, stop_pool/1]).
-export([run/2, sync_queue/2, async_queue/2]).

start(normal, _Args) ->
  ppool_supersup:start_link().

stop(_State) ->
  ok.

start_pool(Name, Limit, MFA) ->
  ppool_supersup:start_pool(Name, Limit, MFA).

stop_pool(Name) ->
  ppool_supersup:stop_pool(Name).

run(Name, Args) ->
  ppool_serv:run(Name,Args).

sync_queue(Name, Args) ->
  ppool_serv:sync_queue(Name,Args).

async_queue(Name, Args) ->
  ppool_serv:async_queue(Name, Args).

% ppool:start_link().
% ppool:start_pool(nagger, 2, {ppool_nagger, start_link, []}).
% ppool:run(nagger, ["Watch a good movie", 10000, 10, self()]).