-module(curling_scoreboard).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2]).

init([]) -> 
    {ok, []}.

% when Event Manager is being called with notify / async_notify ,
% EventHandler`s handle_event will be invoked 
handle_event({set_teams, TeamA, TeamB}, State) ->
    curling_scoreboard_hw:set_teams(TeamA, TeamB),
    {ok, State};
handle_event({add_points, Team, N}, State) ->
    [curling_scoreboard_hw:add_point(Team) ||  _  <- lists:seq(1, N)],
    {ok, State};
handle_event(next_round, State) ->
    curling_scoreboard_hw:next_round(),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

% when Event Manager is being called  with call (async),
% EventHandler`s handle_call will be invoked 
handle_call(_, State) ->
    {ok, ok, State}.

% otherwise it will invoke EventHandler`s handle_info
handle_info(_, State) ->
    {ok, State}.
