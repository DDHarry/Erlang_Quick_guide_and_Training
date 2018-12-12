%% Learn you some Erlang for great good
%% Fred Hébert
%%
-module(fun_a).
-export([one/0,two/0,add/2]).

one() ->  1.
two() ->  22.

add(X,Y)  ->  X() + Y().
