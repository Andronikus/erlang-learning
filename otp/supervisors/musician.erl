-module(musician).
-behaviour(gen_server).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {name="", role, skill=good}).

start_link(Role, Skill) ->
  gen_server:start_link({local,Role}, ?MODULE, [Role, Skill], []).

stop(Role) ->
  gen_server:terminate(Role, stop).

init([Role,Skill]) ->
  process_flag(trap_exit, true),
  rand:seed(now()),
  TimeToPlay = rand:uniform(3000),
  Name = pick_name(),
  RoleAsStr = atom_to_list(Role),
  {ok, #state{name = Name, role = RoleAsStr, skill = Skill}, TimeToPlay}.

handle_call(_Message, _From, _State) ->
  erlang:error("Not implemented").

handle_cast(_Message, _State) ->
  erlang:error("Not implemented").


%%% Private APIs
pick_name() ->
  lists:nth(rand:uniform(10), first_names()) ++ " " ++ lists:nth(rand:uniform(10), last_names()).


first_names() ->
  ["Valerie", "Arnold", "Carlos", "Dorothy", "Keesha", "Phoebe", "Ralphie", "Tim", "Wanda", "Janet"].

last_names() ->
  ["Frizzle", "Perlstein", "Ramon", "Ann", "Franklin", "Terese", "Tennelli", "Jamal", "Li", "Perlstein"].