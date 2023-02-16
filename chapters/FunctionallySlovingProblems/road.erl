-module(road).
-compile(export_all).

% see https://learnyousomeerlang.com/functionally-solving-problems Heathrow to London
main([FileName]) -> 
	{ok, Bin} = file:read_file(FileName),
	Map = parse_map(Bin),
	io:format("~p~n", [optimal_path(Map)]),
	erlang:halt(0).


parse_map(Bin) when is_binary(Bin) ->
	parse_map(binary_to_list(Bin)); % 读取二进制数据 转成string
parse_map(Str) when is_list(Str) -> 
	Values = [list_to_integer(X) || X <- string:tokens(Str, "\r\n\t")], % split string 然后转成integer list
	group_vals(Values, []).


% list 组成三元组的tuple
group_vals([], Acc) ->
	lists:reverse(Acc);
group_vals([A,B,X|Rest], Acc) ->
	group_vals(Rest, [{A,B,X}|Acc]).


% 基于前一阶段算出的值 计算到A B需要的最短路径
% {A, B, X} 表示新的路径长度， {{DistA, PathA}, {DistB, PathB}} 可以看成{{到PreA的最短距离, 到PreA的最短走法}, {到PreB的最短距离, 到PreB的最短走法}}
shortest_step({A, B, X}, {{DistA, PathA}, {DistB, PathB}}) -> 
	OptA1 = {A + DistA, [{a, A}|PathA]},
	OptA2 = {A + DistB + X, [{x, X}, {b, B}|PathB]},
	OptB1 = {B + DistB, [{b, B}|PathB]},
	OptB2 = {B + DistA + X, [{x, X}, {b, B}|PathA]},
	{erlang:min(OptA1, OptA2), erlang:min(OptB1, OptB2)}.

optimal_path(Map) -> 
	{A, B} = lists:foldl(fun shortest_step/2, {{0, []}, {0, []}}, Map), % 计算到A B的最短路径
	{_Dist, Path} = if hd(element(2, A)) =/= {x, 0} -> A; % 如果到A的最短路径的第一步是 {x,0} 说明是从A点出发，否则说明从B点出发
					hd(element(2, B)) =/= {x, 0} -> B
				end,
	lists:reverse(Path). % 反向输出 得到从起点到终点的路径
