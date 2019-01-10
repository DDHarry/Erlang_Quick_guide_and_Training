# Erlang Tips, Training & Programming guide


Some Erlang tools, libs and a lot of funs ;-)


   ### Note :
   __This is not an introduction to Erlang,
   just a basic guide to be more performant,
   proficient in Erlang.__
      

## 1. Erlang in short

Erlang is a  concurrent and functional programming language whose development favors practicability over purity. It is production oriented. Along with its concurrency characteristics, Erlang also benefits from these interesting properties:

  - **scalability**; because concurrency relies onto small, independant programs, easy to scale up;

  - **fault-tolerance**; it was specifically built to protect against faillures;

  - **soft real-time**; no degradation under heavy loads thanks to a predictable response time and latency;

  - **distributed**; able to manage millions of processes efficiently by taking the best ofthanks to an efficient use of any multi-cores, multi-processors architecture.
  
The syntax of Erlang share this same philosophy, lean, efficient, straightforward and delicious.



## 2. Table of content

1). **01_Sequential_Erlang** - The basics about the Erlang programming language, sequential and also concurrent programming:
- atoms, variables,
- MFA, Module, Function, Arguments,
- lists, lists comprehension,
- recursion, tail-recursion,
- funs, lambda functions,
- records, maps
- BIFs,guards,
- types,
- ETS, DETS, Mnesia;
   
   
2). **02_Distributed_Erlang** - Distributed, or concurrent Erlang, a main feature of this programming language:
- message passing,
- receive, select,
- spawn processes;


3). **03_Deeper Erlang** - Deeper than you think; profiling ...


4). **04_Erlang_Problems_Solutions** - Some exerices and problems. Most of them are derived from the Erlang official documentation, others come from the referenced books listed at the bottom of this page.


5). hello.erl - the very classic "Hello the World".





## 3. Anatomy of an Erlang program

The Erlang unit programming block is called a module. It starts with ```-module(my_first_prog).```  if it is the name of the module and written down to a file whose name must be the same as the module's one. Hence ```my_first_prog.erl``` becomes the name of this file. For more, cf. the references.

```Erlang
-module(my_first_prog).
```

### About the syntax

- Erlang favors the "\_" (underscore) ```special_character``` to complete names for modules, fonctions, variables ...

- Everything is a function and every function, expression returns a value;

- each line ends with ```.```, except when it is not the last part of the returned expression.




## 4. Compiling, Running, Generating eDoc

We can use the Erlang shell we call by the command ```$ erl ```. This REPL feature appears very convenient to try and for little computation. For more sophisticated or complex programs, we need to compile the code which was interpreted in the former case. There exists two ways to compile then run Erlang programs,

- compile, then run within the Erlang shell

``` erlang
   $) erl
   Erlang/OTP ...
   1> c(hello).
   {ok,hello}
   2> hello:world().
   Hello the World
   ok
   3>
```

- compile then run outside the Erlang shell, using ```erlc```.

```Erlang
$) erlc hello.erl
```

```Erlang
yy
```

where ``` $) ``` is the Unix shell and ``` 5> ```, the Erlang shell.




## 5. Getting some help

The references, not citing DuckDuckGo or StackOverflow.




## 6. Some references
- *Programming Erlang - Sofware for a concurrent World* by Joe Armstrong,
- *Designing for scalability with Erlang/OTP* by Franceso Cesarini and Steve Vinovski,
- *Erlang and OTP in action* by Martin Logan, Eric Merritt and Richard Carlsson and
- *Learn you some Erlang for great good* by Fred Hébert.

