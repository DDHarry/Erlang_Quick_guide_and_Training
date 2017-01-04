% fun  - Gain
%%    The need for fun
%%
%% Note ::  Tail-recursion
%%
-module(fun_b).
-export([increment/1,decrement/1]).
-export([mapm/2,incr/1,decr/1]).

increment([]) ->  [];
increment([H|T])  ->  [H+1|increment(T)].

decrement([]) ->  [];
decrement([H|T])  ->  [H-1|decrement(T)].


%% We recognise the same pattern, hence we abstract it
%% mapm to differentiate it from erlang:map
%% optional
mapm(_, [])  ->  [];
mapm(F, [H|T])  ->  [F(H)|mapm(F, T)].

incr(X) ->  X + 1.
decr(X) ->  X-1.
