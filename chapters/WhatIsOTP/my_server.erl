-module(my_server).
-export([start/2, start_link/2, call/2, cast/2, reply/2]).



% 对Client / Server的通用模式的抽象

% 将消息发送和应答过程抽象成一个函数
% call 是同步消息处理的流程，即一个完整的应答
call(Pid, Msg) -> 
    Ref = erlang:monitor(process, Pid),
    Pid ! {sync, self(), Ref, Msg}, % 发送请求
    receive 
        {Ref, Reply} -> % 处理响应
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, Pid, Reason} -> % 处理错误
            erlang:error(Reason)
    after 5000 -> % 处理超时
        erlang:error(timeout)
    end.

% 异步调用过程的抽象
cast(Pid, Msg) ->
    Pid ! {async, Msg},
    ok.

% 消息回复的抽象
reply({Pid, Ref}, Reply) ->
    Pid ! {Ref, Reply}.

% server main loop的抽象
% 主要流程是receive msg -> handle msg -> 再loop
% 其中服务器状态主要依靠State保存
loop(Module, State) ->
    receive 
        {async, Msg} -> 
            NewState1 = Module:handle_cast(Msg, State), % 异步消息处理 其中handle_cast由具体业务模块实现
            loop(Module, NewState1);
        {sync, Pid, Ref, Msg} ->
            NewState2 = Module:handle_call(Msg, {Pid, Ref}, State), % 同步消息处理 其中hanle_call由具体业务模块实现
            loop(Module, NewState2)
    end.

% server 初始化的流程
init(Module, State) -> 
    loop(Module, Module:init(State)).


start(Module, InitialState) ->
    spawn(fun() -> init(Module, InitialState) end).

start_link(Module, InitialState) ->
    spawn_link(fun() -> init(Module, InitialState) end).