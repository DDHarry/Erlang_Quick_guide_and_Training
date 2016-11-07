$$$ How to compile your programs $$$

A. Classic compilations
-----------------------

$ for the *nix Shell
> for the Erlang Shell
>> the result

The file is greetings.erl w/ hello/0 & hello/1 -> returs the entered name


Compiling and Running in the *nix Shell
---------------------------------------
$ erlc greetings.erl

$ erl -noshell -s greetings hello -s init stop
>> Hello the world!

$ erl -noshell -s greetings hello Bob -s init stop
>> Hello 'Bob'!



Compiling and Running in the Erlang Shell
-----------------------------------------
$ erl
> c(greetings).
>greetings:hello


B. Script based compilations
----------------------------
