-module(musician).
-behaviour(gen_server).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {name="", role, skill=good}).
-define(DELAY, 750).

start_link(Role, Skill) ->
  gen_server:start_link({local,Role}, ?MODULE, [Role, Skill], []).

stop(Role) ->
  gen_server:call(Role, stop).

init([Role,Skill]) ->
  process_flag(trap_exit, true),
  rand:seed(exs1024s),
  TimeToPlay = rand:uniform(3000),
  Name = pick_name(),
  RoleAsStr = atom_to_list(Role),
  {ok, #state{name = Name, role = RoleAsStr, skill = Skill}, TimeToPlay}.

%%% When received Message = stop terminate the process
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Message, _From, State) ->
  {noreply, State, ?DELAY}.

handle_cast(_Message, State) ->
  {noreply, State, ?DELAY}.

handle_info(timeout, S = #state{name= Name, skill = good}) ->
  io:format("~s produced a sound!~n", [Name]),
  {noreply, S, ?DELAY};

handle_info(timeout, S = #state{name = Name, skill = bad}) ->
  case rand:uniform(5) of
    1 ->
      io:format("~s played a false note! Huuu ~n", [Name]),
      {stop, bad_note, S};
    _ ->
      io:format("~s produced a sound!~n", [Name]),
      {noreply, S, ?DELAY}
  end;

handle_info(_Message, State) ->
  {noreply, State, ?DELAY}.

code_change(_OldVersion, State, _Extra) ->
  {ok,State}.

%%% Terminate when stop() is fired
terminate(normal, S) ->
  io:format("~s left the room (~s)~n", [S#state.name, S#state.role]);
terminate(bad_note, S) ->
  io:format("~s sucks! kicked out that member out of the band (~s)~n", [S#state.name, S#state.role]);
terminate(shutdown, S) ->
  io:format("The manager fired all the band! ~s just got back to playing in the subway! ~n", [S#state.name]);
terminate(_Reason, S) ->
  io:format("~s was been kicked out (~s)~n", [S#state.name, S#state.role]).


%%% Private APIs
pick_name() ->
  lists:nth(rand:uniform(10), first_names()) ++ " " ++ lists:nth(rand:uniform(10), last_names()).


first_names() ->
  ["Valerie", "Arnold", "Carlos", "Dorothy", "Keesha", "Phoebe", "Ralphie", "Tim", "Wanda", "Janet"].

last_names() ->
  ["Frizzle", "Perlstein", "Ramon", "Ann", "Franklin", "Terese", "Tennelli", "Jamal", "Li", "Perlstein"].