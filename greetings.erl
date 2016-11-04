%%% Classic Hello the World
%%% Or anybody's name
-module(greetings).
-export([hello/0, hello/1]).

hello() ->
  io:format("Hello the World~n", []).
  
hello(Someone)  ->
  io:format("Hello ~st~n", Someone).
