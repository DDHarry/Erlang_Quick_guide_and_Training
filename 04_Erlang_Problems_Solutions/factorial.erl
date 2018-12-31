%
-module(factorial).
-export([compute/1]).

compute(0) ->
  1;
compute(N) ->
  N*Compute(N-1).
  
