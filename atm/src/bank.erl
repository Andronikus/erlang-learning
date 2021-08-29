-module(bank).

-export([open/0, close/0]).
-export([deposit/2, balance/1,withdraw/2]).


open() ->
    bank_sup:start().

close() -> 
    bank_sup:stop().

deposit(AccountId, Amount) ->
    bank_atm:deposit(AccountId, Amount).

withdraw(AccountId, Amount) ->
    bank_atm:withdraw(AccountId, Amount).

balance(AccountId) ->
    bank_atm:balance(AccountId).