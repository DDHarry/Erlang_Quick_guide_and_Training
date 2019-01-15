
% More fun
%% Derived from Joe Armstrong's Programming Erlang
-module(aae_fun).
-export([cube/1,double/1,hypot/2,mtpl_quad/2,trpl_quad/1]).

%  Y is a shadow variable
double(X) ->
  Double = fun(Y) -> 2*Y end,
  Double(X).

% hence, we can call it "X"
% the compiler emits a warning stating "X is shadowed"
cube(X) ->
  Cube = fun(X) -> X*X*X end,
  Cube(X).


trpl_quad(X)  ->
  (fun(X) ->  3*X*X*X*X end) (X).

% Here the function has two parameters
mtpl_quad(Mtpl,X) ->
  (fun(X) -> Mtpl*X*X*X*X end)(X).

%% fun can have any number of arguments
hypot(A,B)  ->
  (fun(A,B) -> math:sqrt(A*A + B*B) end)(A,B).
  
