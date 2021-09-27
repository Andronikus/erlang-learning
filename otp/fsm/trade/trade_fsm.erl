-module(trade_fsm).
-behaviour(gen_fsm).

-export([start/1, start_link/1]).
-export([trade/2, accept_trade/1, make_offer/2, retract_offer/2, ready/1, cancel/1]).
-export([ask_negotiation/2, accept_negotiate/2, do_offer/2, undo_offer/2]).
-export([are_you_ready/1, am_ready/1, ack_trans/1, ask_commit/1, do_commit/1]).
-export([notify_cancel/1]).

%% Public API
start(Name) ->
    gen_fsm:start(?MODULE, [Name], []).

start_link(Name) -> 
    gen_fsm:start_link(?MODULE, [Name], []).

% ask for a begin session. Returns when or if other accepts
trade(OwnPid, OtherPid) ->
    gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

% accept someone's trade offer
accept_trade(OwnPid) -> 
    gen_fsm:sync_send_event(OwnPid, {accept_negotiate}).

% make an offer
make_offer(OwnPid, Item) ->
    gen_fsm:send_event(OwnPid, {make_offer, Item}).

% cancel trade offer
retract_offer(OwnPid, Item) -> 
    gen_fsm:send_event(OwnPid, {retract_offer, Item}).

% when you are ready to trade.
ready(OwnPid) ->
    gen_fsm:sync_send_event(OwnPid, ready, infinity).

cancel(OwnPid) ->
    gen_fsm:sync_send_all_state_event(OwnPid, cancel).

%% FSM-FSM communication
% ask for other FSM for a trade session
ask_negotiation(OtherPid, OwnPid) ->
    gen_fsm:send_event(OtherPid, {ask_negotiation, OwnPid}).

% Forward the client message accepting the transaction
accept_negotiate(OtherPid, OwnPid) ->
    gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

% forward a client's offer
do_offer(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {do_offer, Item}).

% forward the client's offer cancelation
undo_offer(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {undo_offer, Item}).

% ask other side if it's ready to trade
are_you_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, are_you_ready).

% reply that the side is not ready to trade
not_yet(OtherPid) ->
    gen_fsm:send_event(OtherPid, not_yet).

% tell the others that the user is currently waiting for ready state
am_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, 'ready').

% acknowledge that the fsm is in ready state
ack_trans(OtherPid) ->
    gen_fsm:send_event(OtherPid, ack).

% ask if ready to commmit
ask_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, ask_commit).

do_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, do_commit).

notify_cancel(OtherPid) ->
    gen_fsm:send_all_state_event(OtherPid, cancel).


