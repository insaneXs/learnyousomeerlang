-module(cat_fsm).
-export([start/0, event/2]).


dont_give_crap() -> 
    receive 
        {Pid, Ref, _Msg} ->
            Pid ! {Ref, meh};
        _-> ok
    end,
    io:format("Switching to 'dont_give_crap' state~n"),
    dont_give_crap().


event(Pid, Event) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, Event},
    receive 
        {Ref, Msg} -> {ok, Msg}
    after 5000 ->
        {error, timeout}
    end.

start() -> spawn_link(fun() -> dont_give_crap() end).
