# Designing a concurent application
https://learnyousomeerlang.com/designing-a-concurrent-application

## To compile modules
`erl -make`

## Start erlang shell with compiled modules
```erl -pa ebin/```
The -pa <directory> option tells the Erlang VM to add that path to the places it can look in for modules.

## Another option to compile everyting in running shell is call below function
`make:all([load]).`

## Example of using application
```
1> evserv:start().
<0.34.0>
2> evserv:subscribe(self()).
{ok,#Ref<0.0.0.31>}
3> evserv:add_event("Hey there", "test", FutureDateTime).
ok
4> evserv:listen(5).
[]
5> evserv:cancel("Hey there").
ok
6> evserv:add_event("Hey there2", "test", NextMinuteDateTime).
ok
7> evserv:listen(2000).
[{done,"Hey there2","test"}]
```

## Example of using it with supervisor
```
1> c(evserv), c(sup).
{ok,sup}
2> SupPid = sup:start(evserv, []).
<0.43.0>
3> whereis(evserv).
<0.44.0>
4> exit(whereis(evserv), die).
true
Process <0.44.0> exited for reason die
5> exit(whereis(evserv), die).
Process <0.48.0> exited for reason die
true
6> exit(SupPid, shutdown).
true
7> whereis(evserv).
undefined
```