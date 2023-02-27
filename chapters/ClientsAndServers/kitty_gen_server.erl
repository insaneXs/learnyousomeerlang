-module(kitty_gen_server).
-export([init/1, start_link/0, order_cat/4, close_shop/1, return_cat/2, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
% behaviour 会希望这个moudle具有哪些函数
-behaviour(gen_server). 

-record(cat, {name, color=green, description}).

% Public API
% gen_server:start_link 的第一个参数是模块名，第二个参数是传给init的参数，第三个参数是debuging options
start_link() -> gen_server:start_link(?MODULE, [], []).

% gen_server:call(Pid, Msg) make a sync call 
order_cat(Pid, Name, Color, Desc) -> 
    gen_server:call(Pid, {order, Name, Color, Desc}, 5000).


close_shop(Pid) ->
    gen_server:call(Pid, terminate).

return_cat(Pid, Cat=#cat{}) ->
    gen_server:cast(Pid, {return, Cat}).





% implemented init callback 
init([]) -> {ok, []}.

% implemented handle_call callback
handle_call({order, Name, Color, Desc}, _From, State) ->
    if State =:= [] ->
        Cat = #cat{name=Name, color=Color, description=Desc},
        {reply, Cat, []}; % 返回{reply, Reply, State}的形式，其中第一个reply表示需要回复信息， 第二个Reply表示回复消息的内容，State表示新状态
        State =/= [] ->
            {reply, hd(State), tl(State)}
        end;
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}. % 返回{stop, Reason, Reply, State}的形式 表示退出进程 Reason是退出的理由，ok是回复消息


% implemented handle_cast callback
handle_cast({return, Cat=#cat{}}, State) ->
    {noreply, [Cat|State]}. % 返回{noreply, State} 表示不回复，并更新新的状态

% handle_info 会在超时 或者 hanle_call/handle_cast 未匹配成功时 被调用 
handle_info(Msg, State) -> 
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(normal, Cats) -> 
    [io:format("~p was set free.~n", [C#cat.name])  || C <- Cats],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



