# Programming Style Guide for [mweet.me](http://mweet.me)


## Conventions used for this style guide

>$ for the *nix Shell

>\> for the Erlang Shell

 > *\>\> the result*


## Table of content

1. [Starting and stopping the Erlang shell](#erlang-shell)

2. [The Hello World - Module, function](#hello-world-module-function)

3. [Compiling](#compiling)

4. [Types](#types)

3. []()



<a name="erlang-shell"></a><a name="1"></a>
## 1. Starting and Stopping the Shell

% A comment in Erlang

``` erlang
erl

%% To quit

q().

%% More options

^c

```

**[ &#8679; to the top](#table-of-content)**




<a name="hello-world-module-function"></a><a name="2"></a>
## 2. Hello the World - Module, function

In a file named greetings.erl.

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



<a name="compiling"></a><a name="3"></a>
## 3. Compiling

- [3.1](#compiling--nix-shell) Compiling and Running in the *nix Shell
The New Hello World program :

```erlang
-module(greetings).
-export([hello/0, hello/1]).

hello() ->
  io:format("Hello the World ~n").

hello(Someone) ->
  T = [Someone],
  io:format("Hello ~ts~n", [Someone]).
  
```
Then we compile from the \*nix shell

```erlang
>$ erlc greetings.erl

>$ erl -noshell -s greetings hello -s init stop

  >> Hello the world!
```

Now with an input name :

```erlang

>$ erl -noshell -s greetings hello Bob -s init stop

  >> Hello 'Bob'!

```
  


<a name="3.2"></a><a name="compiling--erlang-shell"></a>
- [1.2](#compiling--erlang-shell) Compiling and Running in the Erlang Shell

```erlang

$ erl

> c(greetings).

> greetings:hello('Bob').

>> Hello Bob
```

**[ &#8679; to the top](#table-of-content)**







<a name="types"></a><a name="4"></a>
##4. Types

<a name="4.1"></a><a name="types--variables"></a>
### - [4.1](#types--variables) Variables

Starts with an uppper case or the underscore symbole "_"
```erlang
X , _x , Var_is_also_one
```

Erlang variables do not vary : they are immutable!

``` erlang
X = 3.
``` 

is a pattern-matching operation.

There exists the *anonymous variable* "_" .



<a name="4.2"></a><a name="types--atom"></a>
### [4.2](#types--atom) atom

An atom starts with a lowercase letters.

  like, hEre33 , 'An atom' , another_one , "stop_it"


<a name='4.3'></a><a name="types--tuple"></a>
###- [4.3](#types--tuple) Tuple

```erlang

Person = {person, {name, jane, doe} , {height, 1.78}}.

```

Extracting values by pattern-matching

```erlang
{ _ , { -, Who, _ }, { _, _ } = Person.

Who.

>> jane
```

**[ &#8679; to the top](#table-of-content)**






<a name="4.4"></a><a name="Ttypes--list"></a>
- [4.4](#types--list) Lists

``` erlang
L =[2,3*7,hello].

ThingsToBuy = [{apples,4}, {milk,3}, {newspaper,1}].
```

Extracting by Head and Tail of a List

```erlang
L = [H | T].

ThingsToBuyb = [{oranges, 4}, {pears, 6} | ThingsToBuy].
```
**[ &#8679; to the top](#table-of-content)**







<a name="title"></a><a name="999"></a>
## 999. Title

<a name="999.1"></a><a name="title--suba"></a>
- [999.1](#title--suba) suba

here


<a name="999.2"></a><a name="title--subb"></a>
- [999.2](#title--subb) subb

here

``` erlang
-module(name).
-export([func/0]).
```

**[ &#8679; to the top](#table-of-content)**




