
-module(aad_fun).
-export([dbl/1,mtpl/2,multiple/2,trpl/1]).

%% Get the scheme
dbl(X)  ->
  Mult = fun(Times) -> (fun(Y) -> Times*Y end) end,
  Double = Mult(2),
  Double(X).

%% A lighter one - see the anonymous variable X (l12)
%% the compiler will emit a warning "X shadow variable"
trpl(X) ->
  Mult = fun(Times) -> (fun(X) -> Times*X end) end,
  %%Trpl = Mult(3),
 ( Mult(3) ) (X).

%% Compacter - "Y, Times" are foo variable
multiple(Tms,X) ->
  Mult = fun(Times) -> (fun(Y) -> Times*Y end) end,
 (Mult(Tms)) (X).

%% Final version
mtpl(Tms,X) ->
  ((fun(Times) -> (fun(X) -> Times*X end) end)(Tms))(X).
  
