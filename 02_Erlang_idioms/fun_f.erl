%% Joe Armstrong's fun
-module(fun_f).
-export([
  bool_even/1,
  dbl_list/1,
  srt_even/1
]).

dbl_list(L) ->
  io:format("Enter a list L~n"),
  lists:map(fun(X) -> 2*X end, L).

% sort the even numbers out of a list
srt_even(L) ->
  lists:filter(fun even/1,L).

% ouput a list of true ans false values of L
bool_even(L)  ->
  lists:map(fun even/1,L).

even(X) -> (X rem 2) =:= 0.
