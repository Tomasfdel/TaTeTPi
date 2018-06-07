-module(tp).
-compile(export_all).

init() ->
	case gen_tcp:listen(8000, [{active, true}]) of
    {ok, ListenSocket} ->	io:format("Ando~n"), 
							dispatcher(ListenSocket);
    {error, eaddrinuse} -> init() end.


 
dispatcher(ListenSocket) ->
					{ok, Socket} = gen_tcp:accept(ListenSocket),
					spawn(?MODULE, dispatcher, [ListenSocket]),
					psocket(Socket).
	


 
psocket(Socket) -> receive {tcp, Socket, Msg} -> spawn(?MODULE, pcomando, [self(), Msg]), 
												 psocket(Socket);
						   {rambo, Msg} -> gen_tcp:send(Socket, Msg), psocket(Socket);
						   {close} -> gen_tcp:close(Socket);
						   _ -> io:format("Woops ~n") end.
						   


pcomando(DaddyID, Msg) ->
	MsgList = string:tokens(Msg, " "),
	case lists:nth(1, MsgList) of
		"BYE\r\n" -> DaddyID ! {close};
		_ -> DaddyID ! {rambo, "F U C K Y O U \n"} end.
