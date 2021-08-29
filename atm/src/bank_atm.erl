-module(bank_atm).

-export([start_link/0, stop/0]).
-export([deposit/2, balance/1, withdraw/2]).
-export([init/0]).

-define(TIMEOUT, 5000).

-record(state, {accounts}).

start_link() ->
    io:format("Opening bank~n"),
    Pid = spawn_link(?MODULE, init, []),
    register(bank_atm,Pid),
    Pid.

stop() -> 
    ?MODULE ! terminate.

deposit(AccountId, Amount) ->
    ?MODULE ! {deposit, AccountId, Amount}.

withdraw(AccountId, Amount) ->
    ?MODULE ! {withdraw, self(), AccountId, Amount},
    receive
        Reply -> Reply
    after ?TIMEOUT ->
        {error, time_out}
    end.

balance(AccountId) ->
    ?MODULE ! {balance, self(),AccountId},
    receive
        Reply -> Reply
    after ?TIMEOUT -> 
        {error, time_out}
    end.

init() ->
    State = #state{accounts = dict:new()},
    main_loop(State).

main_loop(#state{accounts = Accounts} = State) ->
    receive
        {deposit, AccountId, Amount} when Amount > 0 ->
            NewAccounts = dict:update_counter(AccountId, Amount, Accounts),
            main_loop(#state{accounts=NewAccounts});
        
        {withdraw, CallerPid, AccountId, Amount} when Amount > 0 ->
            case get_balance(AccountId, Accounts) of
                Balance when Amount =< Balance ->
                    NewAccounts = dict:update_counter(AccountId, -1*Amount, Accounts),
                    CallerPid ! ok,
                    main_loop(#state{accounts=NewAccounts});
                _ ->
                    CallerPid ! {error, balance_not_enough},
                    main_loop(State)
            end;
        {balance, CallerPid, AccountId} -> 
            Balance = get_balance(AccountId, Accounts),
            CallerPid ! {ok, Balance},
            main_loop(State);
        terminate -> 
            io:format("Closing Bank~n")
    end.

get_balance(AccountId, Accounts) ->
    case dict:find(AccountId, Accounts) of 
        error -> 0;
        {ok, Value} -> Value
    end.