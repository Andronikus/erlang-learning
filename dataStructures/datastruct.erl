-module(datastruct).

-export([create_robot/0, admin_panel/1, drive_license/1]).
-export([repairman/1]).

%% Type definition
-record(robot, {name, type=industrial, hobbies, details=[]}).
-record(user, {id, name, group, age}).

create_robot() -> 
    #robot{name=andronikus, hobbies=["swing"], details=["smart", "small", "fast"]}.

%% Using types in function's head
admin_panel(#user{name=Name, group=admin}) ->
    Name ++ " is admin and admin privs will be granted";
admin_panel(#user{name=Name}) ->
    Name ++ " access was blocked!".

%% Using types in guards
drive_license(U=#user{}) when U#user.age >= 18 ->
    io:format("~p, your age is ~p. You are able to get drive lic right now~n", [U#user.name, U#user.age]);
drive_license(#user{name=Name}) ->
    io:format("~p, you are under 18! Cannot get drive lic now!~n", [Name]).

%% update type
repairman(Rob=#robot{}) ->
    Details = Rob#robot.details,
    NewRob = Rob#robot{details=["repaired by the repairman"|Details]},
    {repaired, NewRob}.