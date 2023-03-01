-module(trade_fsm).
% -export([start/1, start_link/1, trade/2, accept_trade/1, make_offer/2, retract_offer/2, ready/1, cancel/1]).
-compile(export_all).
-behaviour(gen_fsm).

-record(state, {name="", 
                other, 
                ownitems=[],
                otheritems=[],
                monitor,
                from}).

% PUBLIC API

start(Name) ->
    gen_fsm:start(?MODULE, [Name], []).


start_link(Name) ->
    gen_fsm:start_link(?MODULE, [Name], []).


trade(OwnPid, OtherPid) ->
    gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

accept_trade(OwnPid) ->
    gen_fsm:sync_send_event(OwnPid, accept_negotiate).

make_offer(OwnPid, Item) ->
    gen_fsm:send_event(OwnPid, {make_offer, Item}).

retract_offer(OwnPid, Item) ->
    gen_fsm:send_event(OwnPid, {retract_offer, Item}).


ready(OwnPid) ->
    gen_fsm:sync_send_event(OwnPid, ready, infinity).

cancel(OwnPid) ->
    gen_fsm:sync_send_all_state_event(OwnPid, cancel).


% from FSM to FSM

ask_negotiate(OtherPid, OwnPid) ->
    gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

accept_negotiate(OtherPid, OwnPid) ->
    gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

do_offer(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {do_offer, Item}).

undo_offer(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {undo_offer, Item}).
 
are_you_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, are_you_ready).

not_yet(OtherPid) ->
    gen_fsm:send_event(OtherPid, not_yet).

am_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, 'ready!').

ack_trans(OtherPid) ->
    gen_fsm:send_event(OtherPid, ack).

ask_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, ask_commit).

do_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, do_commit).

notify_cancel(OtherPid) ->
    gen_fsm:send_all_state_event(OtherPid, cacnel).


init(Name) ->
    {ok, idle, #state{name=Name}}.


notice(#state{name=N}, Str, Args) ->
    io:format("~s: " ++ Str ++ "~n", [N|Args]).

unexcepted(Msg, State) ->
    io:format("~p received unknown event ~p while in state ~p~n", [self(), Msg, State]).



% private function
add(Item, Items) ->
    [Item | Items].

remove(Item, Items) ->
    Items -- [Item]. 


priority(OwnPid, OtherPid) when OwnPid < OtherPid -> true;
priority(OwnPid, OtherPid) when OwnPid > OtherPid -> false.


commit(S = #state{}) ->
  io:format("Transaction completed for ~s. "
              "Items sent are:~n~p,~n received are:~n~p.~n"
              "This operation should have some atomic save "
              "in a database.~n",
              [S#state.name, S#state.ownitems, S#state.otheritems]).

% implement FSM state 

% 'idle' state will turn to 'ilde_wait' when receives other FSM`s 'ask_negotiate' event
idle({ask_negotiate, OtherPid}, S=#state{}) ->
    Ref = monitor(process, OtherPid),
    notice(S, "~p asked for a trade negotiation", [OtherPid]),
    {next_state, idle_wait, S#state{other=OtherPid, monitor=Ref}};
idle(Event, Data) ->
    unexcepted(Event, idle),
    {next_state, idle, Data}.
% 'idle' state will turn to 'ilde_wait' when receives other User`s 'negotiate' command
idle({negotiate, OtherPid}, From, S=#state{}) ->
    ask_negotiate(OtherPid, self()),
    notice(S, "asking user ~p for a trade", [OtherPid]),
    Ref = monitor(process, OtherPid),
    {next_state, idle_wait, S#state{other=OtherPid, monitor=Ref, from=From}};
idle(Event, _From, Data) ->
    unexcepted(Event, idle),
    {next_state, idle, Data}.

% 'idle_wait' state will turn to 'negotiate' when receives user`s 'negotiate' event
% 'idle_wait' state will turn to 'negotiate' when receives FMS`s 'accept_negotiate' event
idle_wait({ask_negotiate, OtherPid}, S=#state{other=OtherPid}) -> % update state.other when receive other player`s ask
    gen_fsm:reply(S#state.from, ok),
    notice(S, "starting negotiation", []),
    {next_state, negotiate, S};
idle_wait({accept_negotiate, OtherPid}, S=#state{other=OtherPid}) -> 
    gen_fsm:reply(S#state.from, ok),
    notice(S, "starting negotiation", []),
    {next_state, negotiate, S};
idle_wait(Event, Data) ->
    unexcepted(Event, idle),
    {next_state, idle_wait, Data}.

% 'idle_wait' state will turn to 'negotiate' when receives user`s 'accept_negotiate' event
idle_wait(accept_negotiate, _From, S=#state{other=OtherPid}) ->
    accept_negotiate(OtherPid, self()),
    notice(S, "accept negotiation", []),
    {reply, ok, negotiate, S};
idle_wait(Event, _From, Data) ->
    unexcepted(Event, idle_wait),
    {next_state, idle_wait, Data}.

% when 'negotiate' state receive 'make_offer', 'retracting_offer', 'do_offer', 'undo_offer', 'are_you_ready' , it will not change state
% only if it receives 'ready' from User, will it turn to 'wait'
negotiate({make_offer, Item}, S=#state{ownitems=OwnItems}) ->
    do_offer(S#state.other, Item),
    notice(S, "offering ~p", [Item]),
    {next_state, negotiate, S#state{ownitems=add(Item, OwnItems)}};
negotiate({retracting_offer, Item}, S=#state{ownitems=OwnItems}) ->
    undo_offer(S#state.other, Item),
    notice(S, "cancelling offer on ~p", [Item]),
    {next_state, negotiate, S#state{ownitems=remove(Item, OwnItems)}};
negotiate({do_offer, Item}, S=#state{otheritems=OtherItems}) ->
    notice(S, "other player offering ~p", [Item]),
    {next_state, negotiate, S#state{otheritems=add(Item, OtherItems)}};
negotiate({undo_offer, Item}, S=#state{ownitems=OtherItems}) ->
    notice(S, "other player cancelling offer on ~p", [Item]),
    {next_state, negotiate, S=#state{otheritems=remove(Item, OtherItems)}};

negotiate(are_you_ready, S=#state{other=OtherPid}) ->
    io:format("Other user ready to trade.~n"),
    notice(S, "Other user ready to transfer goods:~n"
              "You get ~p, The other side gets ~p",
              [S#state.otheritems, S#state.ownitems]),
    not_yet(OtherPid),
    {next_state, negotiate, S};
negotiate(Event, Data) -> 
    unexcepted(Event, negotiate),
    {next_state, negotiate, Data}.


negotiate(ready, From, S = #state{other=OtherPid}) ->
    are_you_ready(OtherPid),
    notice(S, "asking if ready, waiting", []),
    {next_state, wait, S#state{from=From}};
negotiate(Event, _From, S) ->
    unexcepted(Event, negotiate),
    {next_state, negotiate, S}.


wait({do_offer, Item}, S=#state{otheritems=OtherItems}) ->
    gen_fsm:reply(S#state.from, offer_changed),
    notice(S, "other side offering ~p", [Item]),
    {next_state, negotiate, S#state{otheritems=add(Item, OtherItems)}};
wait({undo_offer, Item}, S=#state{otheritems=OtherItems}) ->
    gen_fsm:reply(S#state.from, offer_changed),
    notice(S, "Other side cancelling offer of ~p", [Item]),
    {next_state, negotiate, S#state{otheritems=remove(Item, OtherItems)}};
wait(are_you_ready, S=#state{}) ->
    am_ready(S#state.other),
    notice(S, "asked if ready, and I am. Waiting for same reply", []),
    {next_state, wait, S};
wait(not_yet, S=#state{}) -> 
    notice(S, "Other not ready yet", []),
    {next_state, wait, S};
wait('ready!', S=#state{}) ->
    am_ready(S#state.other),
    ack_trans(S#state.other),
    gen_fsm:reply(S#state.from, ok),
    notice(S, "other side is ready. Moving to ready state", []),
    {next_state, ready, S};
wait(Event, Data) -> 
    unexcepted(Event, wait),
    {next_state, wait, Data}.

% 'when ready' state receive 'ack' event, the higher priority process will start two phase commit
% and the other one should response the event 
ready(ack, S=#state{}) ->
    case priority(self(), S#state.other) of
        true -> 
            try 
                notice(S, "asking for commit", []),
                ready_commit = ask_commit(S#state.other), % send ask_commit and wait util receive 'ready_commit 'reply
                notice(S, "ordering commit", []),
                ok = do_commit(S#state.other), % send do_commit and wait util receive ''
                notice(S, "committing...", []),
                commit(S),
                {stop, normal, S}
            catch Class:Reason ->
                notice(S, "commit failed", []),
                {stop, {Class, Reason}, S}
            end;
        false ->
            {next_state, ready, S}
        end;
ready(Event, Data) ->
    unexcepted(Event, ready),
    {next_state, ready, Data}.

% lower priorty response two phase commit event
ready(ask_commit, _From, S) ->
    notice(S, "replying to ask_commit", []),
    {reply, ready_commit, ready, S};
ready(do_commit, _From, S) ->
    notice(S, "replying to ask_commit", []),
    commit(S),
    {stop, normal, ok, S};
ready(Event, _From, Data) ->
    unexcepted(Event, ready),
    {next_state, ready, Data}.

handle_event(cancel, _StateName, S=#state{}) ->
    notice(S, "receive cancle evnet", []),
    {stop, other_cancelled, S};
handle_event(Event, StateName, Data) ->
    unexcepted(Event, StateName),
    {next_state, StateName, Data}.

handle_sync_event(cancel, _From, _StateName, S = #state{}) ->
    notify_cancel(S#state.other),
    notice(S, "cancelling trade, sending cancel event", []),
    {stop, cancelled, ok, S};
handle_sync_event(Event, _From, StateName, Data) ->
    unexcepted(Event, StateName),
    {next_state, StateName, Data}.

handle_info({'Down', Ref, process, Pid, Reason}, _, S=#state{other=Pid, monitor=Ref}) -> 
    notice(S, "Other side dead", []),
    {stop, {other_down, Reason}, S};
handle_info(Info, StateName, Data) ->
    unexcepted(Info, StateName),
    {next_state, StateName, Data}.


code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

terminate(normal, ready, S=#state{}) ->
    notice(S, "FSM leaving.", []);
terminate(_Reason, _StateName, _StateData) ->
    ok.