We adopt the foolowing notations

For any *nix shell ``` $ ``` and  ` 1>` for the Erlang shell

An example
```Erlang
   > X =4.
      >> 4
 ```

  

**atom** 
  ----
Atoms start with lower cases. They are not like variables which can have a value.

-> Global

-> No garbage collect

          an_atom | 'An atom with spaces' | myName


**Variable**
--------
Variables in Erlang are so immutable.
-> '_' is the anonymous variable
        X | _ | _tail | Who


**tuple**
-----
{firstName, joe} | {lastName, armstrong}
1)  > T = {firstName, joe}.
    > T.
      >> {firstName, joe}

2)  > Joe = 'joe'.
    > Tu = {firstName, Joe}.
    > Tu.
      >> {firstName,"joe"}
      
