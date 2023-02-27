-module(kitty_server2).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1, handle_call/3, handle_cast/2, init/1]).

-record(cat, {name, color=green, description}).

% Client API
start_link() ->
    my_server:start_link(?MODULE, []).

order_cat(Pid, Name, Color, Desc) -> 
    my_server:call(Pid, {order, Name, Color, Desc}).

return_cat(Pid, Cat=#cat{}) ->
    my_server:call(Pid, {return, Cat}).

close_shop(Pid) ->
    my_server:call(Pid, terminate).


% private function
terminate(Cats) -> 
    [ io:format("~p was set free.~n", [Cat#cat.name])|| Cat <- Cats],
    exit(normal).


% implement callback
init([])-> [].


% sync command hanle callback，should return state for next loop
handle_call({order, Name, Color, Desc}, From, State) -> % handle order command
    if State =:= [] ->
            Cat = #cat{name=Name, color=Color, description=Desc},
            my_server:reply(From, Cat),
            [];
        State =/= [] ->
            my_server:reply(From, hd(State)),
            tl(State)
    end;
handle_call(terminate, From, State) -> % handle terminate command
    my_server:reply(From, ok),
    terminate(State).

% async command handle callback
handle_cast({return, Cat = #cat{}}, Cats) ->
    [Cat | Cats]. 





