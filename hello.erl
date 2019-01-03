%%% Classic Hello the World
%%% Or anybody's name
-module(hello).
-export([world/0, world/1]).

world() ->
  io:format("Hello the World~n", []).
  
world(Someone)  ->
  io:format("Hello ~st~n", Someone).
