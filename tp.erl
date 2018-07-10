-module(tp).
-compile(export_all).

%~ NameTable: Inserta tuplas unitarias con los nombres de jugador.
%~ GameTable: Inserts tuplas de la forma {juegoID, tablero, turno, jugadores, jugadoresID ,spectatorID}.

newGame(GameTable, Creator, CreatorID) ->
	%~ id, tablero, turno, jugadores, jugadoresID, espectadoresID
	dets:insert(GameTable, {getGameID(), newBoard(), 1, {Creator}, {CreatorID}, {}}).

getGameID() ->
	%~ dummy pls fix
	1.

newBoard() ->
    {vacio, vacio, vacio,
     vacio, vacio, vacio,
     vacio, vacio, vacio}.
     
changeTurn(Turn) ->
	case Turn of
		1 -> 2;
		2 -> 1 end.
		
turnFig(Turn) ->
	case Turn of
		1 -> x;
		2 -> o end.

sendToList(IDList, Msg) ->
	case IDList of
		[] 	  -> ok;
		[H|T] -> H ! {ok, Msg},
				 sendToList(T, Msg) end.

checkGameEnd(Board, CurrentPlayer) ->
	VictoryMsg = "Fin del juego. Ha ganado "++CurrentPlayer++"!\n",
	DrawMsg = "Fin del juego. Empate! Git gud scrubs.\n",
	case Board of
			{x, x, x,
			 _, _, _,
			 _, _, _} -> {fin, VictoryMsg};

			{_, _, _,
			 x, x, x,
			 _, _, _} -> {fin, VictoryMsg};

			{_, _, _,
			 _, _, _,
			 x, x, x} -> {fin, VictoryMsg};

			{x, _, _,
			 x, _, _,
			 x, _, _} -> {fin, VictoryMsg};

			{_, x, _,
			 _, x, _,
			 _, x, _} -> {fin, VictoryMsg};

			{_, _, x,
			 _, _, x,
			 _, _, x} -> {fin, VictoryMsg};

			{x, _, _,
			 _, x, _,
			 _, _, x} -> {fin, VictoryMsg};

			{_, _, x,
			 _, x, _,
			 x, _, _} -> {fin, VictoryMsg};

			{o, o, o,
			 _, _, _,
			 _, _, _} -> {fin, VictoryMsg};

			{_, _, _,
			 o, o, o,
			 _, _, _} -> {fin, VictoryMsg};

			{_, _, _,
			 _, _, _,
			 o, o, o} -> {fin, VictoryMsg};

			{o, _, _,
			 o, _, _,
			 o, _, _} -> {fin, VictoryMsg};

			{_, o, _,
			 _, o, _,
			 _, o, _} -> {fin, VictoryMsg};

			{_, _, o,
			 _, _, o,
			 _, _, o} -> {fin, VictoryMsg};

			{o, _, _,
			 _, o, _,
			 _, _, o} -> {fin, VictoryMsg};

			{_, _, o,
			 _, o, _,
			 o, _, _} -> {fin, VictoryMsg};

			{A, B, C,
			 D, E, F,
			 G, H, I} when A =/= vacio, B =/= vacio, C =/= vacio,
						   D =/= vacio, E =/= vacio, F =/= vacio,
						   G =/= vacio, H =/= vacio, I =/= vacio ->
				{fin, DrawMsg};

			_ -> ok end.

broadcastMove(Board, PlayerIDs, Spectators, CurrentPlayer, GameTable, GameID) ->
	PlayerList = erlang:tuple_to_list(PlayerIDs),
	SpectatorList = erlang:tuple_to_list(Spectators),
	sendToList(PlayerList, "Tablero updateado correctamente.\n"),
	sendToList(SpectatorList, "Tablero updateado correctamente.\n"),
	case checkGameEnd(Board, CurrentPlayer) of
		{fin, Msg} -> sendToList(PlayerList, Msg),
					  sendToList(SpectatorList, Msg),
					  dets:delete(GameTable, GameID);
		_ -> ok end.
     
makeMove(DaddyID, GameTable, GameID, CurrentPlayer, Pos) ->
	case dets:lookup(GameTable, GameID) of
		[] -> DaddyID ! {error, "ERROR Juego no encontrado\n"};
		[{GameID, Board, Turn, Players, PlayerIDs, Spectators}] ->
			case element(Turn, Players) of
				CurrentPlayer ->
					case element(Pos, Board) of
						%~ Falta chequear final
						vacio -> dets:insert(GameTable, {GameID, setelement(Pos, Board, turnFig(Turn)), changeTurn(Turn), Players, PlayerIDs, Spectators}),
								 %~ Movimiento valido
								 broadcastMove(Board, PlayerIDs, Spectators, CurrentPlayer, GameTable, GameID);
						_ -> DaddyID ! {error, "ERROR Posicion previamente ocupada\n"} end;
				_ -> DaddyID ! {error, "ERROR No es tu turno, tramposo!\n"} end;	
		%~ CAMBIAR ESTE MENSAJE ANTES DE ENTREGAR
		_ -> DaddyID ! {error, "ERROR Instructions unclear, dick stuck in ceiling fan\n"} end.



init() ->
	case gen_tcp:listen(8000, [{active, true}]) of
    {ok, ListenSocket} ->	io:format("Ando~n"),
							{ok, NameTable} = dets:open_file(playerNames, []),
							{ok, GameTable} = dets:open_file(games, []),
							dets:delete_all_objects(NameTable),
							dets:delete_all_objects(GameTable),
							dispatcher(ListenSocket, NameTable, GameTable);
    {error, eaddrinuse} -> init() end.


 
dispatcher(ListenSocket, NameTable, GameTable) ->
					{ok, Socket} = gen_tcp:accept(ListenSocket),
					spawn(?MODULE, dispatcher, [ListenSocket, NameTable, GameTable]),
					pSocketLogin(Socket, NameTable, GameTable).

	
%~ Hacer consistente el formato de los mensajes
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
	receive {tcp, Socket, Msg} -> 	spawn(?MODULE, pcomando, [self(), Msg, NameTable, GameTable, Username]), 
									psocket(Socket, NameTable, GameTable, Username);
			%~ Esto deberia cambiar
			{rambo, Msg} -> gen_tcp:send(Socket, Msg), psocket(Socket, NameTable, GameTable, Username);
			{ok, Msg} -> gen_tcp:send(Socket, Msg), psocket(Socket, NameTable, GameTable, Username);
			{error, Msg} -> gen_tcp:send(Socket, Msg), psocket(Socket, NameTable, GameTable, Username);
			{close} -> gen_tcp:close(Socket);
			_ -> io:format("Woops ~n") end.


pcomando(DaddyID, Msg, NameTable, GameTable, Username) ->
	MsgList = string:tokens(string:trim(Msg), " "),
	case lists:nth(1, MsgList) of
		"BYE" -> DaddyID ! {close};
		
		"CON" -> 	DaddyID ! {rambo, "ERROR Sesión ya iniciada\n"};
					
		"FIND" -> 	case dets:lookup(NameTable, lists:nth(2, MsgList)) of
					[] -> DaddyID ! {rambo, "N I S M A N E A D O\n"};
					_ -> DaddyID ! {rambo, "Acata\n"} end;
		%~ dummy pls fix		
		"NEW" ->	newGame(GameTable, Username, DaddyID),
					%~ Hacer que le pase el GameID de verdad
					DaddyID ! {rambo, "Game creado, tu GameID es 1 Kappa\n"};
				
		"ACC" ->	GameID = erlang:list_to_integer(lists:nth(2, MsgList)),
					case dets:lookup(GameTable, GameID) of
						[] -> DaddyID ! {error, "ERROR Juego no encontrado\n"};
						[{GameID, Board, Turn, Players, PlayerIDs, Spectators}] ->
							case size(Players) of
								%~ Si ponemos random no siempre empieza el creador
								1 -> dets:insert(GameTable, {GameID, Board, Turn, erlang:insert_element(2, Players, Username), erlang:insert_element(2, PlayerIDs, DaddyID), Spectators}),
									 %~ Pls fix GameID
									 DaddyID ! {ok, "OK Te uniste al juego 1 Kappa\n"},
									 %~ Si ponemos random se rompe esta linea
									 element(1, PlayerIDs) ! {ok, "OK Se han unido a tu juego\n"};
								_ -> DaddyID ! {error, "ERROR Juego lleno\n"} end;
						_ -> DaddyID ! {error, "ERROR No sabemos sintaxis de Erlang\n"} end;
		
		"PLA" ->	%~ Arreglar GameID
					GameID = 1,
					Pos = erlang:list_to_integer(lists:nth(3, MsgList)),
					makeMove(DaddyID, GameTable, GameID, Username, Pos);				
		_ -> DaddyID ! {rambo, "ERROR Comando no válido\n"} end.
