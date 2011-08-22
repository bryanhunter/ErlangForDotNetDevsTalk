-module(hello).

-export([say_hello/1]).

say_hello(Name) ->
    boo(),
    io:format("Hello ~s!~n", [Name]).

boo() ->
    io:format("Booooo!").
