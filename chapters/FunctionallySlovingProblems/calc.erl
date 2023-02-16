-module(calc).
-export([rpn/1]).  

% see https://learnyousomeerlang.com/functionally-solving-problems Reverse Polish Notation Calculator

rpn(L) when is_list(L) -> % is_list函数在erlang库中 可以不加module name直接使用
	[Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
	Res.  


read(N) -> 
	case string:to_float(N) of % 会匹配满足float形式的字符串 和 剩下的string部分。匹配不上则会返回{error, no_float}
		{error, no_float} -> list_to_integer(N); % 不是float类型，尝试转integer
		{F, _} -> F
	end.  

rpn("+", [N1, N2|S]) -> [N1 + N2|S];
rpn("-", [N1, N2|S]) -> [N1 - N2|S];
rpn("*", [N1, N2|S]) -> [N1 * N2|S];
rpn("/", [N1, N2|S]) -> [N1 / N2|S];
rpn("^", [N1, N2|S]) -> [math:pow(N2, N1)|S];
rpn("ln", [N|S]) -> [math:log(N)|S];
rpn("log", [N|S]) -> [math:log10(N)|S];
rpn(X, S) -> [read(X)|S]. % 不是操作符，直接入栈

