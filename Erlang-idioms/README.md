# Erlang idioms


# - C -

fun_c.erl



# - F -


## Funs

###• Definition

In functional programming languages, a function you define can be passed as a parameter to another function. Hencen the function parameter is bound to a variable. The latter can be used like any other variable within the function.

A function which accepts such funtion as a parameter ared defined as a *higher-order* function. Higher-order function enables higher level of abstraction. Such a function-as-data object is sometimes called a *lambda expression* or a *closure*, a *fun* when anonymous (see bottom).


####- Origin

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
8> fun_a:add(fun_a:one/0, fun fun_a:two/0). or fun_a:add(fun fun_a:one/0, fun_a:two/0).
```
2 : if function names are written without a parameter list > atoms

3 : also atoms, cannot be called as functions

4 : shell command > error shell command one/0

5 : bad function  > error in fun_a:add/2

6 : arithmetic expression > error when evaluating arithmetic expression

7 : illegal expression

8 : illegal expression

9 : the correct answer

 > CORRECT FORM : A new notation
```erlang
fun Module:function/arity     ::  % Use that specific function + bind it to a variable
```
Hence,
```erlang
9> fun_a:add(fun fun_a:one/0, fun fun_a:two/0).
  >>> 23
```


###• Why, Gain?

funs are an easy way to create abstraction.

• **Abstraction** fun_b.erl from (3)
```erlang
1> L = [1,2,3].
2> fun_b:increment(L).
 >> [2,3,4]

3> fun_b:decrement(L).
 >> [0,1,2]
```

We recognise the same pattern > we abstract the similar part with map/2 (or mapm to show it is different from Erlang:sys).

```erlang
4> fun_b:mapm(fun fun_b:decr/1, L).
>> [0,1,2]
5> fun_b:mapm(fun fun_b:incr/1,L].
>> [2,3,4]
```


• **Function as an argument to another function** fun_c.erl from (2)

either_or_both lies in the fun_c.erl module. If you want to call it

```erlang
fun either_or_both/2
```
From another module, it would be
```erlang
fun fun_c:either_or_both/2
```
You can even bind it to a variable
```erlang
F = fun fun_c:either_or_both/2.
```
or pass it direcctly to another function
```erlang
yesno(fun fun_c:either_or_both/2)
```
Example
```erlang
2> yesno(fun fun_c:either_or_both/2).
 >> yes
```
or
```erlang
3> P = fun fun_c:either_or_both/2.
4> fun_c:yesno(P).
 >> yes
```


### Note : Argument(s)
*fun_a.erl* versus *fun_b.erl* versus *fun_c.erl*

> We see the arguments are provided as an input with ```map``` to the list of arguments in *fun_b.erl*, whereas, in *fun_c.erl*, the arguments are given, already written in the code source.



• **This fun is mine** fun_d.erl
```erlang
> Double = fun(X) -> 2*X.
> Double(7).
 >> 14
 
> Triple = fun(X) -> 3*X.
> Triple(5).
 >> 15
 ```
 We are able to see the pattern. Hence we go further.
 ```erlang
 > Mult = fun(Times) -> (fun(X) -> Times*X end) end.
 > Dbl = Mult(2).
 > Dbl(7).
  >> 14
 
 > Trpl = Mult(3).
 > Trpl(9).
  >> 27
```
We get the same result for ```Qtpl = Mult(5)```
```erlang
> (Mult(5))(6)
 >> 30
 ```
For the module version, see ```fun_d.erl```
```erlang
mtpl(Times,N) ->
   Mult = fun(Tms) -> (fun(X) -> Tms*X end) end,
   Mult(Times),
   (Mult(Times))(N).
```






## Functions 1/2 Multiple clauses

## Functions 2/2 Pattern matching and case of



# - G -

## Guards

fun_c.erl


# - I -

## If Case ... of

```erlang
if X>Y -> a();
   true -> ok
end
```

```erlang

tot_price(Rate,P) ->
         case Rate of
           N -> P*N;
           R -> P*R;
           _ -> "Not the good input"
         end.
```


# - R -

## Recursion - Tail recursion

###• Examples

*fun_b.erl*



## Credits
Many of the examples are coming from the following sources. The credits go to their respective authors.

(1) Designing for scalability with Erlang OTP by Francesco Cesarini & Steve Vinoski

(2) Erlang and OTP in action by Martin Logan, Eric Meritt and Richard Carlsson

(3) Learn you some Erlang for great good by Fred Hébert

(4) Programming Erlang Software for a concurrent world by Joe Armstrong

