-module(tp).
-compile(export_all).

init() ->
	{ok, NameTable} = dets:open_file(nametable, []),
	dets:insert(NameTable, {"Tomasu"}),
	case gen_tcp:listen(8000, [{active, true}]) of
    {ok, ListenSocket} ->	io:format("Ando~n"), 
							dispatcher(ListenSocket, NameTable);
    {error, eaddrinuse} -> init() end.


 
dispatcher(ListenSocket, NameTable) ->
					{ok, Socket} = gen_tcp:accept(ListenSocket),
					spawn(?MODULE, dispatcher, [ListenSocket, NameTable]),
					psocket(Socket, NameTable).
	


 
psocket(Socket, NameTable) -> receive {tcp, Socket, Msg} -> spawn(?MODULE, pcomando, [self(), Msg, NameTable]), 
												 psocket(Socket, NameTable);
						   {rambo, Msg} -> gen_tcp:send(Socket, Msg), psocket(Socket, NameTable);
						   {close} -> gen_tcp:close(Socket);
						   _ -> io:format("Woops ~n") end.
						   


pcomando(DaddyID, Msg, NameTable) ->
	MsgList = string:tokens(Msg, " "),
	case lists:nth(1, MsgList) of
		"BYE\r\n" -> DaddyID ! {close};
		"FIND" -> case dets:lookup(NameTable, {lists:nth(2, MsgList)}) of
					[] -> {rambo, "N I S M A N E A D O\n"};
					_ -> {rambo, "Acata\n"} end;
		_ -> DaddyID ! {rambo, "F U C K Y O U \n"} end.
