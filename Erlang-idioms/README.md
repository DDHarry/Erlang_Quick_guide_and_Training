# Erlang idioms


## Functions 1/2 Multiple clauses

## Functions 2/2 Pattern matching and case of



## Funs

In functional programming languages, a function you define can be passed as a parameter to another function. Hencen the function parameter is bound to a variable. The latter can be used like any other variable within the function.

A function which accepts such funtion as a parameter ared defined as a *higher-order* function. Higher-order function enables higher level of abstraction. Such a function-as-data object is sometimes called a *lambda expression* or a *closure*, a *fun* when anonymous (see bottom).


**- Origin**

Functional programming comes from the ability to construct such abstractions. It comes from mathematics, lambda calculus. In lambda calculus, everything functions accepts functions as parameters, operate on them because everything is defined as a function, included numbers, operators ...etc.


Thus, Erlang, as a *functional* programming language, should be able to handle functions as data :

- pass a function as input to another function,

- return a function as the result of another function,

- put a function in data structure then pick it up later ...

#### - Examples

• fun_a.erl from (3)
```erlang
1> c(fun_a).                  %% last time we write it, next time we write directly "2>"
2> fun_a:add(one, two).       %% None of these forms work!
3> fun_a:add(1,2).
4> fun_a:add(one(),two()).
5> fun_a:add(fun_a:one(),fun_a:two()).
6> fun_a:add(one/0,two/0).
7> fun_a:add(fun_a:one/0,fun_a:two/0).
```
2 : if function names are written without a parameter list > atoms

3 : also atoms, cannot be called as functions

4 : shell command > error shell command one/0

5 : bad function  > error in fun_a:add/2

6 : arithmetic expression > error when evaluating arithmetic expression

7 : illegal epression

8 : the correct answer

 >> CORRECT FORM : A new notation
```erlang
fun Module:function/arity     ::  % Use that specific function + bind it to a variable
```
Hence,
```erlang
8>  fun_a:add(fun fun_a:one/0, fun fun_a:two/0).
```

## Recursion - Tail recursion


## Credits
Many of the examples are coming from the following sources. The credits go to their respective authors.

(1) Designing for scalability with Erlang OTP by Francesco Cesarini & Steve Vinoski

(2) Erlang and OTP in action by Martin Logan, Eric Meritt and Richard Carlsson

(3) Learn you some Erlang for great good by Fred Hébert

(4) Programming Erlang Software for a concurrent world by Joe Armstrong

