# Programming Style Guide for [mweet.me](http://mweet.me)


## Conventions used for this style guide

>$ for the *nix Shell

>\> for the Erlang Shell

>*\>\> the result*


## Table of content

1. [Compiling greetings.erl] (#compiling)

2. [Types](#types)


<a name="compiling"></a><a name="1"></a>
## 1. Compiling

- [1.1](#compiling--nix-shell) Compiling and Running in the *nix Shell
The Hello World program :

```erlang
-module(greetings).
-export([hello/0, hello/1]).

hello() ->
  io:format("Hello the World ~n").

hello(Someone) ->
  T = [Someone],
  io:format("Hello ~ts~n", [Someone]).
  
```

>$ erlc greetings.erl

>$ erl -noshell -s greetings hello -s init stop

>*\>\> Hello the world!*

Now with an input name :

>$ erl -noshell -s greetings hello Bob -s init stop

>*\>\> Hello 'Bob'!*

**[ &#8679; to the top](#table-of-content)**


<a name="1.2"></a><a name="compiling--erlang-shell"></a>
- [1.2](#compiling--erlang-shell) Compiling and Running in the Erlang Shell
$ erl

> \> c(greetings).

>\> greetings:hello('Bob').

>\>\> Hello Bob



<a name="types"></a><a name="2"></a>
##2. Types

<a name="2.1"></a><a name="types--atom"></a>
- [2.1](#types--atom) atom

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




