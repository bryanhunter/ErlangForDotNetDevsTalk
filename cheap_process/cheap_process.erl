-module(cheap_process).

-export([start_and_time/1,start/1, start_proc/2]).

start_and_time(HowMany)->
    {Microseconds, ok} = timer:tc(?MODULE, start, [HowMany]),
    io:format("~nCompleted in ~p~n", [Microseconds]).

start(HowMany) ->
    start_proc(HowMany, self()).

start_proc(0, Pid) ->
    Pid ! ok;
start_proc(HowManyMore, Pid) ->
    % How many tens of thousands have we spawned?
    % showprogress(HowManyMore),

    % Create a child process that runs the same code (less one)
    NewPid = spawn(?MODULE, 
		   start_proc, 
		   [HowManyMore-1, Pid]),

    % Send an 'ok' message to the child 
    NewPid ! ok,

    % Wait until we receive an 'ok' message (from our parent).
    receive ok ->
	    ok
    end.

% showprogress(Number) when Number rem 10000 == 0  ->
%    io:format("+");
% showprogress(_) ->
 %   ok.
