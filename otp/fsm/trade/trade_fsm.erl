-module(trade_fsm).
-behaviour(gen_statem).

-record(state, {name="", other, ownitems=[], otheritems=[], monitor, from}).

-export([start/1, start_link/1]).
-export([trade/2, accept_trade/1, make_offer/2, retract_offer/2, ready/1, cancel/1]).
-export([ask_negotiate/2, accept_negotiate/2, do_offer/2, undo_offer/2]).
-export([are_you_ready/1, am_ready/1, ack_trans/1, ask_commit/1, do_commit/1]).
-export([notify_cancel/1]).
-export([init/1, idle/2, idle/3, idle_wait/2, idle_wait/3, negotiate/2, negotiate/3, wait/2, ready/2, ready/3]).
-export([handle_event/3, handle_sync_event/4, handle_info/3, code_change/4, terminate/3 ]).

%% Public API
start(Name) ->
    gen_statem:start(?MODULE, [Name], []).

start_link(Name) -> 
    gen_statem:start_link(?MODULE, [Name], []).

% ask for a begin session. Returns when or if other accepts
trade(OwnPid, OtherPid) ->
    gen_statem:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

% accept someone's trade offer
accept_trade(OwnPid) -> 
    gen_statem:sync_send_event(OwnPid, {accept_negotiate}).

% make an offer
make_offer(OwnPid, Item) ->
    gen_statem:send_event(OwnPid, {make_offer, Item}).

% cancel trade offer
retract_offer(OwnPid, Item) -> 
    gen_statem:send_event(OwnPid, {retract_offer, Item}).

% when you are ready to trade.
ready(OwnPid) ->
    gen_statem:sync_send_event(OwnPid, ready, infinity).

cancel(OwnPid) ->
    gen_statem:sync_send_all_state_event(OwnPid, cancel).

%% FSM-FSM communication
% ask for other FSM for a trade session
ask_negotiate(OtherPid, OwnPid) ->
    gen_statem:send_event(OtherPid, {ask_negotiate, OwnPid}).

% Forward the client message accepting the transaction
accept_negotiate(OtherPid, OwnPid) ->
    gen_statem:send_event(OtherPid, {accept_negotiate, OwnPid}).

% forward a client's offer
do_offer(OtherPid, Item) ->
    gen_statem:send_event(OtherPid, {do_offer, Item}).

% forward the client's offer cancellation
undo_offer(OtherPid, Item) ->
    gen_statem:send_event(OtherPid, {undo_offer, Item}).

% ask other side if it's ready to trade
are_you_ready(OtherPid) ->
    gen_statem:send_event(OtherPid, are_you_ready).

% reply that the side is not ready to trade
not_yet(OtherPid) ->
    gen_statem:send_event(OtherPid, not_yet).

% tell the others that the user is currently waiting for ready state
am_ready(OtherPid) ->
    gen_statem:send_event(OtherPid, 'ready').

% acknowledge that the fsm is in ready state
ack_trans(OtherPid) ->
    gen_statem:send_event(OtherPid, ack).

% ask if ready to commit
ask_commit(OtherPid) ->
    gen_statem:sync_send_event(OtherPid, ask_commit).

do_commit(OtherPid) ->
    gen_statem:sync_send_event(OtherPid, do_commit).

notify_cancel(OtherPid) ->
    gen_statem:send_all_state_event(OtherPid, cancel).


%% Callbacks
init(Name) -> 
    {ok, idle, #state{name=Name}}.

% fsm handlers (StateName(Event, State))
idle({ask_negotiation, OtherPid}, S=#state{}) ->
    Ref = monitor(process, OtherPid),
    notice(S, "~p asked for a trade negociation", [OtherPid]),
    {next_state, idle_wait, S#state{other = OtherPid, monitor = Ref}};

idle(Event, Data) ->
    unexpected(Event,idle),
    {next_state, idle, Data}.

idle({negotiate, OtherPid}, From, S=#state{}) ->
    ask_negotiate(OtherPid, self()),
    notice(S, "asking user ~p for a trade", [OtherPid]),
    Ref = monitor(process, OtherPid),
    {next_state, idle_state, S#state{other = OtherPid, monitor = Ref, from = From}};

idle(Event, _From, Data) ->
    unexpected(Event, idle),
    {next_state, idle, Data}.

idle_wait({ask_negotiate, OtherPid}, S=#state{}) ->
    gen_statem:reply(S#state.from, ok),
    notice(S, "starting negotiation", []),
    {next_state, negotiate, S};
% the other side accepts our offer. Move no negotiate state
idle_wait({accept_negotiate, OtherPid}, S=#state{}) ->
    gen_statem:reply(S#state.from, ok),
    notice(S, "starting negotiation", []),
    {next_state, negotiate, S};
idle_wait(Event, Data) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, Data}.

idle_wait(accept_negotiate, _From, S= #state{other=OtherPid}) ->
    accept_negotiate(OtherPid, self()),
    notice(S, "accepting negotiation",[]),
    {reply, ok, negotiate, S};
idle_wait(Event, _From, Data) ->
    unexpected(Event,Data),
    {next_state, idle_wait, Data}.

% player to FSM
negotiate({make_offer, Item}, S = #state{ownitems = OwnItems}) ->
    do_offer(S#state.other, Item),
    notice(S, "offering ~p", [Item]),
    {next_state, negotiate, S#state{ownitems = add(Item, OwnItems)}};
% Own side retracting an Item
negotiate({retract_offer, Item}, S = #state{ownitems = OwnItems}) ->
    undo_offer(S#state.other, Item),
    notice(S, "cancelling offer on ~p", [Item]),
    {next_state, negotiate, S#state{ownitems = remove(Item,OwnItems)}};

% FSM to FSM communication
negotiate({do_offer, Item}, S = #state{otheritems = OtherItems}) ->
    notice(S, "other player offering ~p", [Item]),
    {next_state, negotiate, S#state{otheritems = add(Item,OtherItems)}};
% Other player is cancelling the offer
negotiate({undo_offer,Item}, S=#state{otheritems = OtherItems}) ->
    notice(S, "other player is retracting ~p", [Item]),
    {next_state, negotiate, S#state{otheritems = remove(Item,OtherItems)}};

negotiate(are_you_ready, S=#state{other = OtherPid}) ->
    io:format("other user is ready to trade.~n"),
    notice(S, "Other user is ready to transfer goods. ~n You get ~p, The other side gets ~p~n", [S#state.otheritems,S#state.ownitems]),
    not_yet(OtherPid),
    {next_state, negotiate, S};
negotiate(Event, Data) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, Data}.

negotiate(ready, From, S = #state{other = OtherPid}) ->
    are_you_ready(OtherPid),
    notice(S, "is asking if ready! Waiting...", []),
    {next_state, wait, S#state{from = From}};

negotiate(Event, _From, S) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, S}.

wait({do_offer, Item}, S = #state{otheritems = OtherItems}) ->
    gen_statem:reply(S#state.from, offer_changed),
    notice(S, "other side offering ~p", [Item]),
    {next_state, negotiate, S#state{otheritems = add(Item,OtherItems)}};
wait({undo_offer, Item}, S = #state{otheritems = OtherItems}) ->
    gen_statem:reply(S#state.from, offer_changed),
    notice(S, "other side retract item ~p", [Item]),
    {next_state, negotiate, S#state{otheritems = remove(Item,OtherItems)}};
wait(are_you_ready, S = #state{}) ->
    am_ready(S#state.other),
    notice(S, "asked if ready and I am! Waiting for same reply!", []),
    {next_state, wait, S};

wait(not_yet, S = #state{}) ->
    notice(S, "other is not ready yet!",[]),
    {next_state, wait,S};

wait("ready!", S = #state{}) ->
    am_ready(S#state.other),
    % trigger to start the "two phase commit"
    ack_trans(S#state.other),
    gen_statem:reply(S#state.from,ok),
    {next_state, ready, S};
wait(Event, Data) ->
    unexpected(Event,wait),
    {next_state, wait, Data}.

ready(ack, S = #state{}) ->
    case priority(self(), S#state.other) of
        true ->
            try
                notice(S, "asking for commit", []),
                ready_commit = ask_commit(S#state.other),
                notice(S, "ordering commit", []),
                ok = do_commit(S#state.other),
                notice(S, "Commiting...",[]),
                {stop, normal, S}
            catch
                Class:Reason  ->
                    notice(S, "commit failed", []),
                    {stop, {Class,Reason}, S}
            end;
        false ->
            {next_state, ready, S}
    end;

ready(Event, Data) ->
    unexpected(Event, ready),
    {next_state, ready,Data}.

ready(ask_commit, _From, S=#state{}) ->
    notice(S, "reply to ask commit",[]),
    {reply, ready_commit, ready, S};

ready(do_commit, _From, S=#state{}) ->
    notice(S, "do commit", []),
    commit(S),
    {stop, normal, ok, S};

ready(Event, _From, Data) ->
    unexpected(Event, ready),
    {next_state, ready, Data}.

% Someone cancel the event! Time to stop everything
handle_event(cancel, _StateName, S = #state{}) ->
    notice(S, "received a cancel event", []),
    {stop, other_cancelled, S};

handle_event(Event, StateName, Data) ->
    unexpected(Event,Data),
    {next_state, StateName, Data}.

handle_sync_event(cancel, _From, _StateName, S = #state{}) ->
    notify_cancel(S#state.other),
    notice(S, " cancelling trade! Send cancel event right now...", []),
    {stop, cancelled, ok, S};

handle_sync_event(Event, _From, StateName, Data) ->
    unexpected(Event,Data),
    {next_state, StateName, Data}.

%% Other FSM died!
handle_info({'DOWN', Ref, process, Pid, Reason}, _StateName , S = #state{monitor = Ref, other = Pid}) ->
    notice(S, "other side died!", []),
    {stop, {other_down, Reason}, S};

handle_info(Event, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

code_change(_OldVersion, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

% Transaction completed
terminate(normal, ready, S = #state{}) ->
    notice(S, "FSM Leaving", []);
terminate(_Reason, _Event, _Data) ->
    ok.

%%%%
%% private functions
%%%%
notice(#state{name=Name}, Str, Args) ->
    io:format("~s "++Str++"~n", [Name|Args]).

unexpected(Msg, State) ->
    io:format("~p received unknown event ~p while in state ~p~n", [self(), Msg, State]).

% add an item to a list
add(Item, Items) ->
    [Item|Items].
% remove item from a list
remove(Item, Items) ->
    Items -- [Item].

priority(OwnPid, OtherPid) when OwnPid > OtherPid -> true;
priority(OwnPid, OtherPid) when OwnPid < OtherPid -> false.

commit(S = #state{}) ->
    io:format("Transaction completed for ~s.
               Items sent: ~n~p, ~n received: ~n~p. ~n
               This operation should have some atomic save in database ",
               [S#state.name, S#state.ownitems, S#state.otheritems]).