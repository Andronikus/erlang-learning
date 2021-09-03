-module(calculator).

-export([rpn/1, rpn_test/0]).

% Public API
rpn(L) when is_list(L) ->
    [Result] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
    Result.

% Private

rpn("+", [V1,V2|Stack]) -> [V1+V2|Stack];
rpn("-", [V1,V2|Stack]) -> [V2-V1|Stack];
rpn("*", [V1,V2|Stack]) -> [V1*V2|Stack];
rpn("/", [V1,V2|Stack]) -> [V2/V1|Stack];
% Patter Match with a value -> add the value to the Stack
rpn(X, Stack) -> 
    [read(X)|Stack].

% Try to convert a number as a string to a float or an integer
read(S) ->
    case string:to_float(S) of 
        {error,no_float} -> list_to_integer(S);
        {V,_} -> io:format("V: ~p~n",[V]), V
    end.

%% Test
rpn_test() ->
    5 = rpn("2 3 +"),
    -1 = rpn("2 3 -"),
    3.0 = rpn("9 3 /"),
    ok.