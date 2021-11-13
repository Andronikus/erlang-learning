-module(erlcount_lib).
-export([find_erl/1]).

-include_lib("kernel/include/file.hrl").

%% Find all files that have .erl extension
find_erl(Diretory) ->
    find_erl(Diretory, queue:new()).

%% Private methods
find_erl(Name, Queue) ->
    %% read file info
    {ok, F = #file_info{}} = file:read_file_info(Name),
    case F#file_info.type of
        directory -> handle_directory(Name, Queue);
        regular -> handle_regular_file(Name, Queue);
        _Other -> dequeue_and_run(Queue)
    end.

handle_directory(Dir,Queue) ->
    case files:list_dir(Dir) of
        %% Dir is empty
        {ok, []} -> dequeue_and_run(Queue);
        %% With files
        {ok, Files} -> dequeue_and_run(enqueue_many(Dir, Files, Queue))
    end.

dequeue_and_run(Queue) ->
    case queue:pop(Queue) of
        {empty, _} -> done;
        {{value, File}, NewQueue} -> find_erl(File, NewQueue)
    end.

enqueue_many(Path, Files, Queue) ->
    F = fun(File,Q) -> queue:in(filename:join(Path,File),Q) end,
    %% Queue will be the accumulator. All files identified previously belong to Path
    %% will be enqueued
    lists:foldl(F, Queue, Files).

handle_regular_file(Name, Queue) ->
    case filename:extension(Name) of 
        ".erl" -> 
            {continue, Name, fun() -> dequeue_and_run(Queue) end};
        _NonErl ->
            dequeue_and_run(Queue)
    end.

