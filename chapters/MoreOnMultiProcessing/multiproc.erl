-module(multiproc).
-compile(export_all).


important() ->
	receive
		{Priority, Message} when Priority > 10 ->
			[{Priority, Message} | important()]
	after 0 ->
		normal()
	end.

normal() ->
	receive
		{Priority, Message} ->
			[{Priority, Message} | normal()]
	after 0 ->
		[]
	end.