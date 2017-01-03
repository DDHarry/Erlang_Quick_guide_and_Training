# Erlang idioms


## Functions with multiple clauses - Pattern matching and case of



## Funs

In functional programming languages, a function you define can be passed as a parameter to another function. Hencen the function parameter is bound to a variable. The latter can be used like any other variable within the function.

A function which accepts such funtion as a parameter ared defined as a *higher-order* function.

Higher-order function enables higher level of abstraction in Erlang.

#### **Origin**

Functional programming comes from the ability to construct such abstractions. It comes from mathematics, lambda calculus.

In lambda calculus, everything functions accepts functions as parameters, operate on them because everything is defined as a function, included numbers, operators ...etc.


Erlang, as a *functional* programming language should be able to handle functions as data :

- pass a function as input to another function,

- return a function as the result of another function,

- put a function in data structure then pick it up later ...

Such a function-as-data object is called a *fun* (sometimes, a *lambda expression* or a *closure*).


## Recursion - Tail recursion

