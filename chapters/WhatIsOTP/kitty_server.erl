-module(kitty_server).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).

-record(cat, {name, color=green, description}).


make_cat(Name, Col, Desc) ->
    #cat{name=Name, color=Col, description=Desc}.

terminate(Cats) ->
    [io:format("~p was set fress.~n", C#cat.name) || C <- Cats],
    ok.

% server主循环 Cats表示当前还有的猫
loop(Cats) ->
    receive
        % 处理请求小猫消息
        {Pid, Ref, {order, Name, Color, Description}} ->
            % 没有猫 创建新的猫并返回；继续循环
            if Cats =:= [] ->
                Pid ! {Ref, make_cat(Name, Color, Description)},
                loop([]);
            % 队列中有小猫 返回队首的小猫
            Cats =/= [] ->
                Pid ! {Ref, hd(Cats)},
                loop(tl(Cats))
            end;
        % 处理归还小猫的消息
        {return, Cat=#cat{}} ->
            loop([Cat|Cats]);
        % 处理关闭的消息
        {Pid, Ref, terminate} ->
            Pid ! {Ref, ok},
            terminate(Cats);
        Unknown ->
            io:format("Unknwon message: ~p~n", [Unknown]),
            loop(Cats)
    end.
% 初始化 Kitty Server
init() -> loop([]).

% 封装通信协议，封装成简单的API供Client调用

% 预定猫
order_cat(Pid, Name, Color, Description) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, {order, Name, Color, Description}},
    receive 
        {Ref, Cat} -> 
            erlang:demonitor(Ref, [flush]),
            Cat;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.

% 返还猫
return_cat(Pid, Cat=#cat{}) ->
    Pid ! {return, Cat},
    ok.

% 关闭商店
close_shop(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, terminate},
    receive 
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
        after 5000 ->
            erlang:error(timeout)
        end.
% spawn & link kitty server process
start_link() -> spawn_link(fun init/0).

