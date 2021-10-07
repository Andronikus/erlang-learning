-module(curling_accumulator).
-behaviour(gen_event).
%% API
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2]).

-record(state, {teams = orddict:new(), round = 0}).

init([]) ->
  {ok, #state{}}.

handle_event({set_teams, TeamA, TeamB}, S) ->
  Teams = orddict:store(TeamA,0,orddict:store(TeamB, 0,S#state.teams)),
  {ok, S#state{teams = Teams}};

handle_event({add_points, Team, Points}, S = #state{teams = T}) ->
  Teams = orddict:update_counter(Team,Points,T),
  {ok, S#state{teams = Teams}};

handle_event(next_round, S = #state{round = R}) ->
  {ok, S#state{round = R + 1}};

handle_event(_Event, State) ->
  {ok, State}.

handle_call(game_info, S) ->
  %%% {ok, Reply, NewState}
  {ok, {orddict:to_list(S#state.teams), {round, S#state.round}}, S};

handle_call(_Request, State) ->
  {ok, {}, State}.

handle_info(_Event, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
