-module(curling_feed).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

% another EventHandler that can broadcast events to Media`s process
init([Pid]) ->
    {ok, Pid}. % use media`s PID as State in order to send msg

handle_event(Event, Pid) ->
    Pid ! {curling_feed, Event},
    {ok, Pid}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.



