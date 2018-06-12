-module(tp).
-compile(export_all).

%~ NameTable: Inserta tuplas unitarias con los nombres de jugador.
%~ GameTable: Inserts tuplas de la forma {juegoID, tablero, jugadores, spectators}.


init() ->
	case gen_tcp:listen(8000, [{active, true}]) of
    {ok, ListenSocket} ->	io:format("Ando~n"), 
							{ok, NameTable} = dets:open_file(playerNames, []),
							{ok, GameTable} = dets:open_file(games, []),
							dispatcher(ListenSocket, NameTable, GameTable);
    {error, eaddrinuse} -> init() end.


 
dispatcher(ListenSocket, NameTable, GameTable) ->
					{ok, Socket} = gen_tcp:accept(ListenSocket),
					spawn(?MODULE, dispatcher, [ListenSocket, NameTable, GameTable]),
					pSocketLogin(Socket, NameTable, GameTable).

	
pSocketLogin(Socket, NameTable, GameTable) -> 
	receive {tcp, Socket, Msg} -> 	spawn(?MODULE, pcomandoLogin, [self(), Msg, NameTable]),
									pSocketLogin(Socket, NameTable, GameTable);
			{rambo, ok, Msg, Username} -> gen_tcp:send(Socket, Msg), psocket(Socket, NameTable, GameTable, Username);
			{rambo, error, Msg} -> gen_tcp:send(Socket, Msg), pSocketLogin(Socket, NameTable, GameTable);
			{close} -> gen_tcp:close(Socket);
			_ -> io:format("Woops ~n") end.

			
pcomandoLogin(DaddyID, Msg, NameTable) ->
	MsgList = string:tokens(string:trim(Msg), " "),
	case lists:nth(1, MsgList) of
		"BYE" ->DaddyID ! {close};
		"CON" ->Username = lists:nth(2, MsgList),
				case dets:lookup(NameTable, Username) of
					[] -> 	dets:insert(NameTable, {Username}), 
							DaddyID ! {rambo, ok, "OK\n", Username};
					_ -> DaddyID ! {rambo, error, "ERROR Nombre ya existente\n"} end;
		_ -> 	DaddyID ! {rambo, error, "ERROR Comando no válido. Inicie sesión con CON NombreDeUsuario\r\n"} end.


 
psocket(Socket, NameTable, GameTable, Username) -> 
	receive {tcp, Socket, Msg} -> 	spawn(?MODULE, pcomando, [self(), Msg, NameTable, GameTable]), 
									psocket(Socket, NameTable, GameTable, Username);
			{rambo, Msg} -> gen_tcp:send(Socket, Msg), psocket(Socket, NameTable, GameTable, Username);
			{close} -> gen_tcp:close(Socket);
			_ -> io:format("Woops ~n") end.


pcomando(DaddyID, Msg, NameTable, GameTable) ->
	MsgList = string:tokens(string:trim(Msg), " "),
	case lists:nth(1, MsgList) of
		"BYE" -> DaddyID ! {close};
		
		"CON" -> 	DaddyID ! {rambo, "ERROR Sesión ya iniciada\n"};
					
		"FIND" -> 	case dets:lookup(NameTable, lists:nth(2, MsgList)) of
					[] -> DaddyID ! {rambo, "N I S M A N E A D O\n"};
					_ -> DaddyID ! {rambo, "Acata\n"} end;
				
		_ -> DaddyID ! {rambo, "ERROR Comando no válido\n"} end.
