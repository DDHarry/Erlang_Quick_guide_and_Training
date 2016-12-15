# Programming Style Guide for [mweet.me](http://mweet.me)


## Conventions used for this style guide

For the \*nix Shell
```erlang
$: input prompt for the *nix Shell
>> follow by an *nix shell output
```

For the Erlang Shell
```Erlang
> Erlang_cmd.   % 1st command input after the Erlang Shell prompt
> c(greetings).   % 2nd command
 >> output the result
```

## Sources

. The Erlang Reference Manual - [User's Guide :](http://erlang.org/doc/reference_manual/users_guide.html)

**Other**

Erlang official documentation : http://erlang.org/doc/

• Applications doc - [~/Applications](http://erlang.org/doc/applications.html)

• Modules doc : [~/Modules](http://erlang.org/doc/man_index.html)

• The [Getting started User's guide](http://erlang.org/doc/getting_started/users_guide.html)


## Table of content

A. Sequential Erlang

1. [Starting and stopping the Erlang shell](#erlang-shell)

2. [The Hello World - Module, function](#hello-world-module-function)

3. [Comments and Documentation - edoc](#comments-documentation-edoc)

4. [Compilation, Code loading, Path, Running Erlang codes - Make](#compilation-code-loading-running)

5. [Make, Makefile, Automake](#make-makefile-automake)

6. [Types  + 8.26 p137](#types)

7. [Strings](#strings)

8. [io:format](#io-format) + 8.12 [Escape sequences](#escape-sequences)

9. [More on Modules, Functions, Arguments MFA + arity p116 +attributes p117+8.13.Expression+8.14+8.15+8.25 p137](#mfa)

10. [Funs : Higher order functions](#funs)

11. [Lists processing - Lists comprehension  + 4.9 p 70 + 8.16](#lists-processing-comprehension)

12. [Guards](#guards)

13. [Case and if](#case-if)

14. [Accumulators - Tail recursion Accumulators @FHebert ](#accumulators-tail-recursion)

15. [Records and maps](#records-maps)

Error : no > sequential

16. [Binaries and the Bit syntax](#binaries-bit-syntax)

17. [Apply](#apply)

18. [Arithmetic expressions](#arithmetic-expressions)

19. [Block expression](#block-expressions)

20. [Dynamic code loading](#dynamic-code-loading)

21. [Erlang-preprocessor](#erlang-preprocessor)

22. [Macros](#macros)

23. [Match operators in pattern](#operators-match-pattern)

24. [Pattern matching in types and functions arity  XXXXXX hello/0 & hello/1](types-functions-pattern-matching)

25. [Numbers](#numbers)

26. [Operator precedence](#operator-precedence)

27. [The process dictionary](#process-dictionary)

28. [References](#references)

29. [Short-circuit Boolean expressions](#short-circuit-boolean-expressions)

30. [Term comparisons](#term-comparisons)

31. [Types : -spec & -type ]()

32. [The dialyzer 9.3](#the-dializer)

B. Distributed Erlang

33. [Rebar](#rebar)

34. [Concurrrent programming Ch12 ](#concurrent-programming)

35. [Errors in concurrent programs](#errors-concurrent-programs)






<a name="erlang-shell"></a><a name="1"></a>
## [1](erlang-shell). Starting and Stopping the Shell

``` erlang
$: erl    %% To connect with the Erlang shell
> ^c      %% To get more options
> q().    %% To quit

```

**[ &#8679; to the top](#table-of-content)**




<a name="hello-world-module-function"></a><a name="2"></a>
## [2](#hello-world-module-function). Hello the World

Functions are the basic unit. They live in modules. In a file named *greetings.erl*.

``` erlang
-module(greetings).
-export([hello/0]).

hello ->
  io:format("Hello the World~n").
```
Then compiling,

```erlang
> c(greetings).
> greetings:hello().
>> Hello the World
> halt().
```

**[ &#8679; to the top](#table-of-content)**





<a name="3"></a><a name="comments-documentation-edoc"></a>
## [3](#comments-documentation-edoc). Comments and Documentation - edoc

### • Comments
```erlang

% This is a comment
%%% To be more fancy

> this_function(Arg) ->
      do sthg with Arg. % Comment here, it works
```

### • Erlang inline manual pages
```Shell
$: erl -man io
>> (opens the manual ...)
```

### • module_info/0/1

Extracting information using module_info/0
```Erlang
> c(greetings).
> greetings:module_info().
>> [{module,greetings},
 {exports,[{hello,0},
           {hello,1},
           {module_info,0},
           {module_info,1}]},
 {attributes,[{vsn,[162240441480510559803838439317941366835]}]},
 {compile,[{options,[]},
           {version,"7.0.2"},
           {source,"/Users/jeanbaptiste/workspace/wkserlang/greetings.erl"}]},
 {native,false},
 {md5,<<122,14,95,88,121,242,93,116,30,92,210,87,59,169,
        176,51>>}

```
Information using module_info/1
```Erlang
> greetings:module_info(key).
```
where key is an atom, cf. module_info/0, in (module, functions, exports, attributes, compile, native, md5) cf. module_info/0
```Erlang
> greetings:module_info(functions)
>> [{hello,0},{hello,1},{module_info,0},{module_info,1}]
```

### • eDoc

eDoc is an Erlang program [documentation](http://erlang.org/doc/apps/edoc/chapter.html) generator.

Associated with the nearest signifiant program constructing term, the other constructs being ignored.
```Erlang
%% @doc Prints the value x
> -record(foo{x, y, z});
> print(X) ->
   ...
```
the @doc type is associated with the function print/1 .


###  •• Reference ::

> Erlang [doc on eDoc](http://erlang.org/doc/apps/edoc/chapter.html)




Erlang man pages ```Erlang
                 ```
```Shell
$: erl -man edoc
>> edoc / Edoc
```
Example 1
```Erlang
> edoc:run([], []).
>> generates the doc for the files in the dir where you launch the command
```
Example 2
```Erlang
> edoc:run([], [{source_path, ["."]}, {dir, "documentation_dir"}]).
>> generates the doc for the programs in the directory we are in "."
>>  then creates documentation_dir if needed
>> places it in the directory named documentation_dir
```






**[ &#8679; to the top](#table-of-content)**




<a name="compilation-code-loading-running-make"></a><a name="4"></a>
## [4](#compilation-code-loading-running-make). Compilation and Code loading - Make

<a name="4.1"></a><a name="compilation-code-loading-running-make--paths"></a>
### - [4.1](#compilation-code-loading-running-make--paths) Paths
### • Getting the path

Getting the 'home' directory
```erlang
> init:get_argument(home).
>> {ok,[["/Users/jane"]]}
```

To get the value of the current load path
```erlang
> code:get_path().
>> [".",
    "/usr/local/Cellar/erlang/19.1/lib/erlang/lib/kernel-5.1/ebin",
    "/usr/local/Cellar/erlang/19.1/lib/erlang/lib/stdlib-3.1/ebin",
     ...
    "/usr/local/Cellar/erlang/19.1/lib/erlang/lib/gs-1.6.2/ebin",
   [...]|...]
 ```
 
### • Manipulating the load path - Loaded modules
 
 Erlang shell
 ```erlang
> code:add_path(Dir).  %% Dir is the path to the target directory
 ```
 Inside a module
 ```erlang
 -spec code:add_patha(Dir) => true | {error, bad_directory}  %% add a new directory, Dir, to the start of the load path
 -spec code:add_pathz(Dir) => true | {error, bad_directory}  %% add ''                ...                     load path
 ```
 
Get the list of all loaded modules
```erlang
> code:all_loaded().  %%list of all loaded modules
```
Investigate if anything wrong
```erlang
> code:clash().  %%Just to investigate
```

Path search at Erlang shell startup
```erlang
$: erl -pa Dirb1 -pa Dirb2 -pa DirbN ... -pz Dire1 -pz Direp
```
the ``` -pa Dir1 ``` flags adds ```Dir1``` to the beginning and ``` -pz Dire1 ```, ```-pz DireP``` adds ```Dire1, DireP ``` directories to the end of the code path.




### •• Recommendation

> Place all these features in a file called **".erlang" ** file

> When Erlang starts, it first reads and evaluates all the commands inside this *.erlang* file.
  ```erlang
    io:format("Hello, this is your .erlang file~n").
  ```
 > then
```erlang
 $: erl
 >> Hello, this is your .erlang file
 > Erlang/OTP 19 [erts-8.1] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]
 >
 > Eshell V8.1  (abort with ^G) 
 ```


<a name="4.2"></a><a name="compilation-code-loading-running-make--compiling"></a>
### - [4.2](#compilation-code-loading-running-make--compiling) Compiling


    
### • Development -compile(export_all)
During development, you can use this special feature
```erlang
-compile(export_all).
```
to get faster. And remove it for production. Note this will make code analysis more difficult with the dialyzer.




### • Compiling for Production

Example 1 : the New Hello World program,

```erlang
-module(greetings).
-export([hello/0, hello/1]).

hello() ->
  io:format("Hello the World ~n").

hello(Someone) ->
  io:format(" Hello ~s with an input argument!~n ", [Someone]).
```

- From the Erlang shell
```erlang
$: erl
> c(greetings).
> greetings:hello("Bob").
 >> Hello Bob!
```


Example 2 : the new new Hello World program

```erlang
-module(greetings).
-export([hello/1]).

hello([Someone]) ->
  io:format(" Hello ~s, How are you?~n ", [Someone]),
  init:stop().
```
- in the Erlang shell
```erlang
> c(greetings).
> greetings:hello(["Bob"]).
 >> Hello Bob, how are you?
``` 

- and, in the Unix shell
```shell
$: erlc greetings.erl
$: erl -noshell -s greetings hello Bob
  >> Hello Bob, how are you?
```



<a name="4.3"></a><a name="compilation-code-loading-running-make--running"></a>
### - [4.3](#compilation-code-loading-running-make--running) Running

### • Quick scripting

```shell
$: erl -eval 'io:format("Memory: ~p~n", [erlang:memory(total)]).' -noshell -s init stop
>> Memory: 15561984
```

### • Compile & Run from the CLI prompt *-noshell*
```shell
$: erlc greetings.erl
$: erl -noshell -s greetings hello -s init stop
>> Hello the World!
```
### • Run with a script, *.sh*
In the file ```greetings.sh```. The Erlang ```*.beam``` file should be stored in the same directory than the ```.sh``` file. Or you must precise the absolute path to it with the Erlang ``` -pa /directory/for_the/*.beam_file``` option.

Example 1
```shell
#!/bin/sh
%% The absolute path containing greetings.beam
erl -noshell -pa /home/jane/workspace/ -s greetings hello -s init stop
```
then,
```
$: chmod u+x greetings.sh
$: ./greetings.sh
  >> Hello the World!
```
Example 2
```erlang
-module(greetings).
-export([hello/1]).

hello(A) ->
  io:format("Hello ~s, 1 Arg~n", [A]).
  init:stop.
```
then,
```shell
#!/bin/sh  %% The greetings.sh file
%% The absolute path containing greetings.beam
erl -noshell -pa /home/jane/workspace/ -s greetings hello "Bob"
```

### • Run as an escript
With an escript, you do not need to compile the file. In the file named  ``` greetings```,

Example 1
```shell
#!/usr/bin/env escript    %% in the file named 'greetings'

% Args can be empty - see the Erlang comment
main(Args)  ->
    io:format(Hello the World!~n").
    
>> ./greetings:4: Warning: variable 'Args' is unused
```


Example 2
```shell
#!/usr/bin/env escript

main(A) ->
    io:format("Hello ~s~n", [A]).
```
- when running
```shell
$: ./greetings
 >> Hello
 
$: ./greetings Bob
 >> Hello Bob
```

### •• Nota Bene
> Changing the input type depending of where you run
> Always write on the form ```erlang main(Args)```. *Args* contains a list of the command-line arguments re^resented as atoms. > As previously seen in the example 1, it can be empty. 



Example 3.a : *erl* shell
```erlang
-module(fac).
-export([cmpt/1]).

% in the file fac.erl
cmpt(A) ->
  F = fac(A),
  io:format("Factorial ~w = ~w~n~n",[A,F]).

fac(0)  ->  1;
fac(N)  ->  N * fac(N-1).
```

Example 3.b *Unix* shell
```erlang
-module(fac_noshell).
-export([cmpt/1]).

%% in the file *fac_noshell*
cmpt([A]) ->
  I = list_to_integer(atom_to_list(A)),
  F = fac(I),
  io:format("Factorial ~w = ~w~n~n",[I,F]).

fac(0)  ->  1;
fac(N)  ->  N * fac(N-1).
```
- running it
```shell
erl -noshell -s fac_noshell cmpt 5 -s init stop
```

Example 3.c *escript*
```erlang
#!/usr/bin/env escript

% in the file *fac_escript*
main([A]) ->
    I = list_to_integer(A),
    F = fac(I),
    io:format("Factorial ~w = ~w~n", [I,F]),
    io:format("~n The argument 'A' in main([A]) is a list here~n~n").
    
fac(0) -> 1;
fac(N) ->
    N * fac(N-1).
```
- running it
```shell
$: chmod u+x fac_escript
$: ./factorial 5
 >> Factorial 5 = 120
 ```
 
  
**[ &#8679; to the top](#table-of-content)**


 
<a name="make-makefile-automake"></a><a name="5"></a>
## [5](#make-makefile-automake). Make, Makefile, Automake

<a name="5.1"></a><a name="make-makefile-automake--make-makefiles"></a>
### - [5.1](#make-makefile-automake--make-makefiles) Make, Makefile

### • Getting the path
 
### • Automating compilation (makeFiles)
 
 
### - [5.2](#make-makefile-automake--automake) Automake

### • Getting


 
**[ &#8679; to the top](#table-of-content)**







<a name="types"></a><a name="6"></a>
## [6](#types). Types


<a name="6.1"></a><a name="types--variables"></a>
### - [6.1](#types--variables) Variables

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



<a name="6.2"></a><a name="types--atom"></a>
### - [6.2](#types--atom) atom

An atom starts with a lowercase letters.
```erlang
  like, hEre33 , 'An atom' , another_one , "stop_it"
```


<a name='6.3'></a><a name="types--tuple"></a>
### - [6.3](#types--tuple) Tuple

```erlang

Person = {person, {name, jane, doe} , {height, 1.78}}.

```

Extracting values by pattern-matching

```erlang
> { _ , { -, Who, _ }, { _, _ } = Person.

> Who.

>> jane
```


<a name="6.4"></a><a name="types--list"></a>
### - [6.4](#types--list) Lists

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




