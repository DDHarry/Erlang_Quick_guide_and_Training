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
- [4.1](#types--variables) Variables

Starts with an uppper case

Erlang variables do not vary : they are immutable!

```erlang
X =3. ``` 

is a pattern-matching operation.

*atom*


<a name="4.1"></a><a name="types--atom"></a>
- [4.1](#types--atom) atom

An atom is 
*atom*


<a name="types--tuple"></a>
- [2.2](#types--tuple) Tuple

here

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




