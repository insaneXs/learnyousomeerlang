-module(band_supervisor).
-behaviour(supervisor).

-export([start_link/1, init/1]).

% start a supervisor process, return {supName, Moudle, Args},
% then it will invoke Module:init(Args) function
start_link(Type) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Type).


init(lenient) ->
    init({one_for_one, 3, 60});
init(angry) ->
    init({rest_for_one, 2, 60});
init(jerk) ->
    init({one_for_all, 1, 60});
init(jamband) ->
    {ok, {{simple_one_for_one, 3, 60},
         [{jam_musician,
           {musicians, start_link, [guitar, good]}, % arguments will be appended when call supervisor:start_child()
           temporary, 1000, worker, [musicians]}
         ]}};
init({RestartStrategy, MaxRestart, MaxTime}) ->
    { ok, {
            {RestartStrategy, MaxRestart, MaxTime},
            [{
                singer, % childId
                {musicians, start_link, [singer, good]}, % children`s M-F-A (startFunc)
                permanent, %  restart(): permanent | transient | temporary 
                1000, % timeout() or brutal_kill
                worker, % type: worker or supervisor
                [musicians] % modules
            },{
                bass,
                {musicians, start_link, [bass, good]},
                temporary, 
                1000, 
                worker,
                [musicians]
            },{
                drum,
                {musicians, start_link, [drum, bad]},
                transient,
                1000, 
                worker, 
                [musicians]
            },{
                keytar,
                {musicians, start_link, [keytar, good]},
                transient, 
                1000, 
                worker, 
                [musicians]
            }]
        }
    }.