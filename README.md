# Programming Style Guide for [mweet.me](http://mweet.me)


## Conventions used for this style guide

$ for the *nix Shell
\> for the Erlang Shell
>> the result


## Table of content

1. [Compiling] (#compiling)



## 1. [Compiling](#compiling--types) and Running in the *nix Shell

$ erlc greetings.erl

$ erl -noshell -s greetings hello -s init stop
>> Hello the world!

$ erl -noshell -s greetings hello Bob -s init stop
>> Hello 'Bob'!
