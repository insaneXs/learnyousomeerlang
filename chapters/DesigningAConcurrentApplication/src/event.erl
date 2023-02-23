-module(event).
-compile(export_all).
-record(state, {server, name="", to_go=0}).

loop(S= #state{server=Server, to_go=[T|Next]}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after T * 1000 ->
        if Next =:= [] -> % 时间已经到达
            Server ! {done, S#state.name};
        Next =/= [] ->
            loop(S#state{to_go=Next}) % 递归处理多余的时间
        end
    end.

% Erlang sleep的 最大时间为49天 因此可能要对sleep时间进行分段
normalize(N) ->
    Limit = 49 * 24 *60 *60,
    [N rem Limit | lists:duplicate(N div Limit, Limit)].

cancel(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, cancel},
    receive
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'Down', Ref, process, Pid, _Reason} ->
            ok
    end.

init(Server, EventName, DateTime) ->
    loop(#state{server=Server, name=EventName, to_go=time_to_go(DateTime)}).

start(EventName, DateTime) ->
    spawn(?MODULE, init, [self(), EventName, DateTime]).

start_link(EventName, DateTime) ->
    spawn_link(?MODULE, init, [self(), EventName, DateTime]).

% 支持{{Year,Month,Day},{Hour,Minute,Second}}形式
time_to_go(TimeOut={{_,_,_}, {_,_,_}}) ->
    Now = calendar:local_time(),
    ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) - calendar:datetime_to_gregorian_seconds(Now),
    Secs = if ToGo > 0 -> ToGo;
            ToGo =< 0 -> 0
        end,
    normalize(Secs).
