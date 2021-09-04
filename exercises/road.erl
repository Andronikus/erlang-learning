-module(road).

-export([main/0]).


main() ->
    File = "road.txt",
    {ok, Binary} = file:read_file(File),
    parse_map(Binary).

parse_map(Binary) when is_binary(Binary) ->
    parse_map(binary_to_list(Binary));
parse_map(Str) when is_list(Str) ->
    List = [list_to_integer(S) || S <- string:tokens(Str, "\n\r\t ")],
    lists:reverse(group_values(List, [])).

%% Put the input in the form [{A1,B1,X1},{A2,B2,X2},...,{An,Bn,Xn}]
group_values([], Acc) -> Acc;
group_values([A,B,X|Rest], Acc) -> 
    group_values(Rest, [{A,B,X}|Acc]).

%% Shortest path algorithm
shortest_path({A,B,X}, {{DistA, PathA},{DistB, PathB}}) ->
    AFromA = {DistA + A, [{a,A}|PathA]},
    AFromB = {DistB + B + X, [{x,X},{b,B}|PathB]},
    BFromB = {DistB + B, [{b,B} | PathB]},
    BFromB = {DistA + A + X, [{x,X},{a,A} | PathA]},
    {erlang:min(AFromA, AFromB), erlang:min(BFromB, BFromB)}.
