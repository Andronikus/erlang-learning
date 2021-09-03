-module(exceptions).

-compile(export_all).

throws(F) ->
    try F() of
        _ -> ok
    catch
        Throws -> {throw, caught, Throws}
    end.

errors(F) ->
    try F() of
        _ -> ok
    catch 
        error:Error ->
            {error, caught, Error}
    end.

exits(F) ->
    try F() of 
        _ -> ok
    catch
        exit:Exit ->
            {exit, caught, Exit}
    end.

% all in one
sword(1) -> throw(slice);
sword(2) -> erlang:error(cut_arm);
sword(3) -> exit(cut_leg);
sword(4) -> throw(punch);
sword(5) -> exit(cross_bridge).

talk() -> "Yes sir!".

black_night(Attack) ->
    try Attack() of 
        _ -> "You shall not pass!"
    catch
        throw:slice -> "It is but a scratch";
        error:cut_arm -> "I've had horse";
        exit:cut_leg -> "Come on you pansy!";
        % catch all exception! ß
        _:_ -> "Just a flesh wound"
    after
        io:format("Over and out..~n")
    end.