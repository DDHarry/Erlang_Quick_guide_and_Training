# Erlang Tips, Training & Programming guide


Some Erlang tools, libs and a lot of funs ;-)


## 1. Erlang in short

Erlang is a  concurrent and functional programming language whose development favors practicability over purity. It is production oriented. Along with its concurrency characteristics, Erlang also benefits from these interesting properties:

  - **scalability**; because concurrency relies onto small, independant programs, easy to scale up;

  - **fault-tolerance**; it was specifically built to protect against faillures;

  - **soft real-time**; no degradation under heavy loads thanks to a predictable response time and latency;

  - **distributed**; able to manage millions of processes efficiently by taking the best ofthanks to an efficient use of any multi-cores, multi-processors architecture.
  
The syntax of Erlang share this same philosophy, lean, efficient, straightforward and delicious.



### Table of content
---

1). **01_Erlang_basics** - The basics about the Erlang programming language, sequential and also concurrent programming:
- atoms, variables,
- MFA, Module, Function, Arguments,
- lists, lists comprehension,
- funs, lambda functions,
- records, maps
- BIFs,guards,
- ETS, DETS, Mnesia;

2). **02_Erlang_idioms** - More in-depth coverage of some essential constructions specific to Erlang or, sometimes to the functional programming languages:
- recursion, tail-recursion,
- more funs,
- types;
   
3). **03_Distributed_Erlang** - Distributed, or concurrent Erlang, a main feature of this programming language:
- message passing,
- receive, select,
- spawn processes;

4). **04_Erlang_Problems_Solutions** - Some exerices and problems. Most of them are derived from the Erlang official documentation, others come from the referenced books listed at the bottom of this page.

5). **05_Deeper Erlang** - Deeper than you think; profiling ...

6). hello.erl - the very classic "Hello the World".




## 2. Compiling, Running, Generating eDoc

There exists two ways to compile then run Erlang programs,

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
```$)``` is the Unix shell, ```5>```, the Erlang shell.




## 3. Getting some help

The references, not citing DuckDuckGo or StackOverflow.




## 4. Some references
- *Programming Erlang - Sofware for a concurrent World* by Joe Armstrong,
- *Designing for scalability with Erlang/OTP* by Franceso Cesarini and Steve Vinovski,
- *Erlang and OTP in action* by Martin Logan, Eric Merritt and Richard Carlsson and
- *Learn you some Erlang for great good* by Fred Hébert.

