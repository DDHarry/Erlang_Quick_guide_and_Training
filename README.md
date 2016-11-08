# Programming Style Guide for [mweet.me](http://mweet.me)


## Conventions used for this style guide

```erlang
$: input prompt for the *nix Shell

> input prompt for the Erlang Shell

or

c(greetings).   % nothing before the command

 >> the result got from the former input
```

## Table of content

A. Sequential Erlang

1. [Starting and stopping the Erlang shell](#erlang-shell)

2. [The Hello World - Module, function](#hello-world-module-function)

3. [Comments and Documentation](#comments-documentation) + comments 122 

4. [Compilation and Code loading](#compilation-code-loading) + ch10 p160

5. [Types](#types) + 8.26 p137

6. [Strings](#strings)

7. [io:format](#io-format) + 8.12 [Escape sequences](#escape-sequences)

8. [More on Modules, Functions, Arguments MFA](#mfa) + arity p116 + attributes p117 + 8.13.Expression
+ 8.14 + 8.15 +8.25 p137

9. [Funs : Higher order functions](#funs)

10. [Lists processing - Lists comprehension](#lists-processing-comprehension) + 4.9 p 70 + 8.16

11. [Guards](#guards)

12. [Case and if](#case-if)

13. [Accumulators](#accumulators) + F. Hébert on accumulators

14. [Records and maps](#records-maps)

Error : no > sequential

15. [Binaries and the Bit syntax](#binaries-bit-syntax)

16. [Apply](#apply)

17. [Arithmetic expressions](#arithmetic-expressions)

18. [Block expression](#block-expressions)

19. [Dynamic code loading](#dynamic-code-loading)

20. [Erlang-preprocessor](#erlang-preprocessor)

21. [Macros](#macros)

22. [Match operators in pattern](#operators-match-pattern)

23. [Pattern matching in types and functions arity](types-functions-pattern-matching) XXXXXX hello/0 & hello/1

24. [Numbers](#numbers)

25. [Operator precedence](#operator-precedence)

26. [The process dictionary](#process-dictionary)

27. [References](#references)

28. [Short-circuit Boolean expressions](#short-circuit-boolean-expressions)

29. [Term comparisons](#term-comparisons)

30. [Types : -spec & -type ]()

31. [The dialyzer](#the-dializer) 9.3

B. Distributed Erlang

32. [Concurrrent programming](#concurrent-programming) - Ch12

33. [Errors in concurrent programs](#errors-concurrent-programs)






<a name="erlang-shell"></a><a name="1"></a>
## [1](erlang-shell). Starting and Stopping the Shell

% A comment in Erlang

%%% A very classical one


``` erlang
$: erl    %% To connect with the Erlang shell

> q().    %% To quit

> ^c      %% To get more options


```

**[ &#8679; to the top](#table-of-content)**




<a name="hello-world-module-function"></a><a name="2"></a>
## [2](#hello-world-module-function). Hello the World - Module, function

In a file named *greetings.erl*.

``` erlang
-module(greetings).
-export([hello/0]).

hello ->
  io:format("Hello the World~n").
```
Then

```erlang
> c(greetings).
> greetings:hello().
>> Hello the World
> halt().
```

**[ &#8679; to the top](#table-of-content)**





<a name="3"></a><a name="comments-documentation"></a>
## [3](#comments-documentation). Comments and Documentation

```erlang

% Comment
%%% To be more fancy

```

**[ &#8679; to the top](#table-of-content)**




<a name="compilation-code-loading"></a><a name="4"></a>
## [4](#compilation-code-loading). Compilation and Code loading
The New Hello World program :

```erlang
-module(greetings).
-export([hello/0, hello/1]).

hello() ->
  io:format("Hello the World ~n").

hello(Someone) ->
  T = [Someone] ,
  io:format(" Hello ~ts~n ", [Someone]).
  
```

Then we compile from the \*nix shell

```erlang
$: erlc greetings.erl

$: erl -noshell -s greetings hello -s init stop

$:  >> Hello the world!
```

Now with an input name :

```erlang

$: erl -noshell -s greetings hello Bob -s init stop

$:  >> Hello 'Bob'!

```
  


**[ &#8679; to the top](#table-of-content)**







<a name="types"></a><a name="5"></a>
## [5](#types). Types


<a name="5.1"></a><a name="types--variables"></a>
### - [5.1](#types--variables) Variables

Starts with an uppper case or the underscore symbole "_"
```erlang
X , _x , Var_is_also_one
```

Erlang variables do not vary : they are immutable!

``` erlang
> X = 3.
``` 

is a pattern-matching operation.

There exists the *anonymous variable* "_" .



<a name="5.2"></a><a name="types--atom"></a>
### - [5.2](#types--atom) atom

An atom starts with a lowercase letters.
```erlang
  like, hEre33 , 'An atom' , another_one , "stop_it"
```


<a name='5.3'></a><a name="types--tuple"></a>
### - [5.3](#types--tuple) Tuple

```erlang

Person = {person, {name, jane, doe} , {height, 1.78}}.

```

Extracting values by pattern-matching

```erlang
> { _ , { -, Who, _ }, { _, _ } = Person.

> Who.

>> jane
```


<a name="5.4"></a><a name="types--list"></a>
### - [5.4](#types--list) Lists

``` erlang
[] % is the empty list

> L =[2,3*7,hello].

> ThingsToBuy = [{apples,4}, {milk,3}, {newspaper,1}].
```

Extracting by Head and Tail of a List

1. First example
```erlang
> L = [H | T].

> H.

>> H = 2

> T.

>> [3*7, Hello].
```

2. Two lists
```erlang
> ThingsToBuyb = [{oranges, 4}, {pears, 6} | ThingsToBuy].
```

**[ &#8679; to the top](#table-of-content)**









Task list

- [X] Live

- [ ] Life

- [ ] Love! Okay, this is done




<a name="title"></a><a name="999"></a>
## 999. Title

<a name="999.1"></a><a name="title--suba"></a>
- [999.1](#title--suba) suba

here


<a name="999.2"></a><a name="title--subb"></a>
- [999.2](#title--subb) subb

here

``` erlang

```

**[ &#8679; to the top](#table-of-content)**




