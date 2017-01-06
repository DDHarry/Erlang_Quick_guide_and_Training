%% fun
%%  A function as an argument to another function
%%
-module(fun_c).
-export([either_or_both/2, yesno/1]).



%% either_or_both receives (A,B) as arguments, they are boolean
either_or_both(true,B) when is_boolean(B) ->  true;
either_or_both(A,true) when is_boolean(A) ->  true;
either_or_both(false,false) ->  false.

%% To proceed, "yesno(fun fun:either_or_both/2)."
yesno(P)  ->
  case P(true,false) of
    true  ->  io:format("yes~n");
    false ->  io:format("no~n")
  end.
