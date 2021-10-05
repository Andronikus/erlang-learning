-module(curling_scoreboard_hw).

%% API
-export([set_teams/2, next_round/0, add_point/1, reset_board/0, shutdown/0]).

set_teams(TeamA, TeamB) ->
  io:format("Scoreboard:: Team ~p Vs Team ~p~n", [TeamA, TeamB]).

next_round() ->
  io:format("Scoreboard:: Round over~n").

add_point(Team) ->
  io:format("Scoreboard:: increase score of team ~p by one~n", [Team]).

reset_board() ->
  io:format("Scoreboard:: All teams are undefined and all scores are 0~n").

shutdown() ->
  io:format("Shutdown in progress...~n").