-module(hhfuns).

-compile(export_all).

one() -> 1.
two() -> 2.

add(X,Y) -> X() + Y().

% Increment and decrement by one in a list
increment([]) -> [];
increment([H|T]) -> [H+1|increment(T)].

decrement([]) -> [];
decrement([H|T]) -> [H-1|decrement(T)].

% Same behaviour... cycle through a list and apply a behaviour (+1 or -1)

% Behaviour Abstraction
map(_,[]) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

addOne(X) -> X + 1.
subtractOne(X) -> X - 1.

%filter
even(List) -> lists:reverse(even(List,[])).

even([], Acc) -> Acc;
even([H|T], Acc) when H rem 2 == 0 -> 
    even(T, [H|Acc]);
even([_|T], Acc) -> even(T, Acc).

male(People) -> lists:reverse(male(People, [])).
male([], Acc) -> Acc;
male([Person = {male, _}|T], Acc) ->
    male(T, [Person | Acc]);
male([_|T], Acc) ->
    male(T, Acc).

filter(Predicate, List) -> lists:reverse(filter(Predicate, List, [])).

filter(_, [], Acc) -> Acc;
filter(Predicate, [H|T], Acc) -> 
    case Predicate(H) of 
        true -> filter(Predicate, T, [H|Acc]);
        false -> filter(Predicate, T, Acc)
    end.


