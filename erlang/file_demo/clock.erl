```erlang
-module(clock).
-export([start/2, stop/0]).

start(Time, Fun) ->
    register(clock, spawn(fun() -> tick(Time, Fun) end)).

stop() ->
    clock ! stop.

tick(Time, Fun) ->
    receive
        stop ->
            void
    after Time ->
        Fun(),
        tick(Time, Fun)
    end.

```

```eshell
4> clock:start(5000, fun() -> io:format("~p~n", [time()]) end).
true
{15,10,31}
{15,10,36}
{15,10,41}
5> clock:stop().
stop
```
