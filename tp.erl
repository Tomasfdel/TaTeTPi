-module(tp).
-compile(export_all).

%~ NameTable: Inserta tuplas unitarias con los nombres de jugador.
%~ GameTable: Inserts tuplas de la forma {juegoID, tablero, turno, jugadores, spectators}.


newGame(GameTable) ->
	%~ id, tablero, turno, jugadores, espectadores
	dets:insert(GameTable, {getGameID(), newBoard(), 0, {}, {}}).

getGameID() ->
	%~ dummy pls fix
	1.

newBoard() ->
    {vacio, vacio, vacio,
     vacio, vacio, vacio,
     vacio, vacio, vacio}.
     
changeTurn(Turn) ->
	case Turn of
		0 -> 1;
		1 -> 0 end.
		
turnFig(Turn) ->
	case Turn of
		0 -> x;
		1 -> o end.
     
makeMove(DaddyID, GameTable, GameID, CurrentPlayer, Pos) ->
	case dets:lookup(GameTable, GameID) of
		[] -> DaddyID ! {error, "ERROR Juego no encontrado\n"};
		{GameID, Board, Turn, Players, Spectators} ->
			case element(Turn, Players) of
				CurrentPlayer ->
					case element(Pos, Board) of ->
						vacio -> dets:insert(GameTable, {ok, {GameID, setelement(Pos, Board, turnFig(Turn)), changeTurn(Turn), Players, Spectators}),
								 DaddyID ! {ok, "Tablero updateado correctamente\n"}
						_ -> DaddyID ! {error, "ERROR Posicion previamente ocupada\n"} end;
				_ -> DaddyID ! {error, "ERROR No es tu turno, tramposo!\n"} end;	
		%~ CAMBIAR ESTE MENSAJE ANTES DE ENTREGAR
		_ -> DaddyID ! {error, "ERROR Instructions unclear, dick stuck in ceiling fan\n"} end.


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
					psocket(Socket, NameTable, GameTable).
	


 
psocket(Socket, NameTable, GameTable) -> receive {tcp, Socket, Msg} -> 	spawn(?MODULE, pcomando, [self(), Msg, NameTable, GameTable]), 
																		psocket(Socket, NameTable, GameTable);
						   {rambo, Msg} -> gen_tcp:send(Socket, Msg), psocket(Socket, NameTable, GameTable);
						   {close} -> gen_tcp:close(Socket);
						   _ -> io:format("Woops ~n") end.
						   


pcomando(DaddyID, Msg, NameTable, GameTable) ->
	MsgList = string:tokens(Msg, " "),
	case lists:nth(1, MsgList) of
		"BYE" -> DaddyID ! {close};
		
		"CON" -> 	Username = lists:nth(2, MsgList),
					case dets:lookup(NameTable, Username) of
					[] -> 	dets:insert(NameTable, {Username}), 
							DaddyID ! {rambo, "OK\n"};
					_ -> DaddyID ! {rambo, "ERROR Nombre ya existente\n"} end;
					
		"FIND" -> 	case dets:lookup(NameTable, lists:nth(2, MsgList)) of
					[] -> DaddyID ! {rambo, "N I S M A N E A D O\n"};
					_ -> DaddyID ! {rambo, "Acata\n"} end;
		
		%~ dummy pls fix		
		"NEW" ->	newGame(GameTable);
				
		"PLA" ->	%~ Arreglar GameID
					GameID = 1,
					Pos = lists:nth(3, MsgList),
					spawn(?MODULE, makeMove, [self(), GameTable, GameID, Username, Pos]),
					%~ Refinar esto pls
					receive {_, Msg} -> DaddyID ! {rambo, Msg};
		_ -> DaddyID ! {rambo, Msg} end.

