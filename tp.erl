-module(tp).
-compile(export_all).
-record(nameTable, {name, playing = {}, spectating = {}}).
-record(gameTable, {gameID,
                    board,
                    turn = 1,
                    players,
                    playerIDs,
                    spectators = {},
                    spectatorIDs = {}}).
-record(ultimoGameID, {dummy = 420, id}).


newGame(Creator, CreatorID) ->
	GameID = getGameID(),
	F = fun() ->
		mnesia:write(#gameTable{gameID     = GameID,
								board      = newBoard(),
								players    = {Creator},
								playerIDs  = {CreatorID}
								}),
		[Player] = mnesia:read(nameTable, Creator),
		Pos = tuple_size(Player#nameTable.playing) + 1,
		NewPlaying = erlang:insert_element(Pos, Player#nameTable.playing, GameID),
		mnesia:write(Player#nameTable{playing = NewPlaying})
	end,
	{atomic, ok} = mnesia:transaction(F),
	GameID.

getGameID() ->
	F = fun() ->
		[ID] = mnesia:read(ultimoGameID, 420),
		NewID = ID#ultimoGameID.id + 1,
		mnesia:write(ID#ultimoGameID{id=NewID}),
		NewID end,
	{atomic, NewID} = mnesia:transaction(F),
	NewID.
	

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

checkGameEnd(Board, GameID, CurrentPlayer) ->
	VictoryMsg = "Fin del juego "++integer_to_list(GameID)++". Ha ganado "++CurrentPlayer++"!\n",
	DrawMsg = "Fin del juego "++integer_to_list(GameID)++". Empate! Git gud scrubs.\n",
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

stateToString(S) ->
	case S of
		x -> " X ";
		o -> " O ";
		_ -> "   " end.


makeBoardString(Board) ->
    stateToString(element(1, Board)) ++ "|" ++ stateToString(element(2, Board)) ++ "|" ++ stateToString(element(3, Board)) ++
    "\n----------\n" ++
    stateToString(element(4, Board)) ++ "|" ++ stateToString(element(5, Board)) ++ "|" ++ stateToString(element(6, Board)) ++
    "\n----------\n" ++
    stateToString(element(7, Board)) ++ "|" ++ stateToString(element(8, Board)) ++ "|" ++ stateToString(element(9, Board)) ++
    "\n".

is_integer_list(S) -> try   _ = list_to_integer(S),
							true
					  catch error:badarg -> false
					  end.

is_element_of_tuple(E, Tuple) -> is_element_of_tuple(E, Tuple, 1, tuple_size(Tuple)).
is_element_of_tuple(E, T, I, S) when I =< S ->
    case element(I, T) of
        E -> true;
        _ -> is_element_of_tuple(E, T, I+1, S)
    end;
is_element_of_tuple(_, _, _, _) -> false.


removePlayers(GameID, Players, Current, Size) when Current =< Size ->
	[CurrentPlayer] = mnesia:read(nameTable, element(Current, Players)),
	NewPlayingList = tuple_to_list(CurrentPlayer#nameTable.playing),
	NewPlaying = list_to_tuple(lists:delete(GameID, NewPlayingList)),
	mnesia:write(CurrentPlayer#nameTable{playing = NewPlaying}),
	removePlayers(GameID, Players, Current + 1, Size);
removePlayers(_, _, _, _) -> ok.

removeSpectators(GameID, Spectators, Current, Size) when Current =< Size ->
	[CurrentPlayer] = mnesia:read(nameTable, element(Current, Spectators)),
	NewSpectatingList = tuple_to_list(CurrentPlayer#nameTable.spectating),
	NewSpectating = list_to_tuple(lists:delete(GameID, NewSpectatingList)),
	mnesia:write(CurrentPlayer#nameTable{spectating = NewSpectating}),
	removeSpectators(GameID, Spectators, Current + 1, Size);
removeSpectators(_, _, _, _) -> ok.

deleteGame(GameID) ->
	F = fun() -> 
		[Game] = mnesia:read(gameTable, GameID),
		removePlayers(GameID, Game#gameTable.players, 1, tuple_size(Game#gameTable.players)),
		removeSpectators(GameID, Game#gameTable.spectators, 1, tuple_size(Game#gameTable.spectators)),
		mnesia:delete({gameTable, GameID}) end,
	{atomic, ok} = mnesia:transaction(F).


broadcastMove(Board, PlayerIDs, SpectatorIDs, CurrentPlayer, GameID) ->
	PlayerList = erlang:tuple_to_list(PlayerIDs),
	SpectatorList = erlang:tuple_to_list(SpectatorIDs),
	BoardString = makeBoardString(Board),
	sendToList(PlayerList, "Tablero updateado correctamente. Estado actual de la partida "++ integer_to_list(GameID) ++":\n" ++ BoardString ++ "\n"),
	sendToList(SpectatorList, "Tablero updateado correctamente. Estado actual de la partida "++ integer_to_list(GameID) ++":\n" ++ BoardString ++ "\n"),
	case checkGameEnd(Board, GameID, CurrentPlayer) of
		{fin, Msg} -> sendToList(PlayerList, Msg),
					  sendToList(SpectatorList, Msg),
					  deleteGame(GameID);
		_ -> ok end.
     
makeMove(DaddyID, GameID, CurrentPlayer, Pos) ->
	F = fun() -> case mnesia:read(gameTable, GameID) of
		[] -> {error, "ERROR Juego no encontrado.\n"};
		[Game] ->
		case tuple_size(Game#gameTable.players) of
			2 -> case element(Game#gameTable.turn, Game#gameTable.players) of
				CurrentPlayer ->
					case element(Pos, Game#gameTable.board) of
						vacio -> NewBoard = setelement(Pos, Game#gameTable.board, turnFig(Game#gameTable.turn)),
								 ok = mnesia:write(Game#gameTable{board = NewBoard, turn = changeTurn(Game#gameTable.turn)}),
								 %~ Movimiento valido
								 broadcastMove(NewBoard, Game#gameTable.playerIDs, Game#gameTable.spectatorIDs, CurrentPlayer, GameID),
								 ok;
						_ -> {error, "ERROR Posicion previamente ocupada.\n"} end;
				_ -> {error, "ERROR No es tu turno, tramposo!\n"} end;
			_ -> {error, "ERROR Esto no es el solitario, papu.\n"} end;
		%~ CAMBIAR ESTE MENSAJE ANTES DE ENTREGAR
		_ -> {error, "ERROR Instructions unclear, dick stuck in ceiling fan.\n"} end end,
	case mnesia:transaction(F) of
		{atomic, {error, Msg}} -> DaddyID ! {error, Msg};
		_ -> ok end.


joinGame(DaddyID, GameID, Username) ->
	F = fun() ->
		case mnesia:read(gameTable, GameID) of
			[] -> {error, "ERROR Juego no encontrado.\n"};
			[Game] ->
				case tuple_size(Game#gameTable.players) of
					1 -> NewPlayers = erlang:insert_element(2, Game#gameTable.players, Username),
						 NewPlaIDs = erlang:insert_element(2, Game#gameTable.playerIDs, DaddyID),
						 mnesia:write(Game#gameTable{players = NewPlayers, playerIDs = NewPlaIDs}),
						 [Player] = mnesia:read(nameTable, Username),
   						 Pos = tuple_size(Player#nameTable.playing) + 1,
						 NewPlaying = erlang:insert_element(Pos, Player#nameTable.playing, GameID),
						 mnesia:write(Player#nameTable{playing = NewPlaying}),
						 ListaGameID = integer_to_list(GameID),
						 element(1, Game#gameTable.playerIDs) ! {ok, "OK "++Username++" se ha unido al juego "++ListaGameID++".\n"},
						 {ok, "OK Te uniste al juego "++ListaGameID++" contra "++element(1, Game#gameTable.players)++" Kappa.\n"};
					_ -> {error, "ERROR Juego lleno.\n"} end;
			_ -> {error, "ERROR No sabemos sintaxis de Erlang.\n"} end end,
	{atomic, Msg} = mnesia:transaction(F),
	DaddyID ! Msg.

spectateGame(DaddyID, GameID, Username) ->
	F = fun() ->
		case mnesia:read(gameTable, GameID) of
			[] -> {error, "ERROR Juego no encontrado.\n"};
			[Game] ->
				Pos = tuple_size(Game#gameTable.spectators) + 1,
				NewSpectators = erlang:insert_element(Pos, Game#gameTable.spectators, Username),
				NewSpectatorIDs = erlang:insert_element(Pos, Game#gameTable.spectatorIDs, DaddyID),
				mnesia:write(Game#gameTable{spectators = NewSpectators, spectatorIDs = NewSpectatorIDs}),
				[Player] = mnesia:read(nameTable, Username),
   				Pos2 = tuple_size(Player#nameTable.spectating) + 1,
				NewSpectating = erlang:insert_element(Pos2, Player#nameTable.spectating, GameID),
				mnesia:write(Player#nameTable{spectating = NewSpectating}),
				{ok, "OK Observando el juego "++integer_to_list(GameID)++". Estado actual de la partida:\n" ++ makeBoardString(Game#gameTable.board) ++ "\n"};
			_ -> {error, "ERROR No sabemos sintaxis de Erlang.\n"} end end,
	{atomic, Msg} = mnesia:transaction(F),
	DaddyID ! Msg.

leaveGame(DaddyID, GameID, Username) ->
	F = fun() ->
		case mnesia:read(gameTable, GameID) of
			[] -> {error, "ERROR Juego no encontrado.\n"};
			[Game] ->
				case is_element_of_tuple(DaddyID, Game#gameTable.spectatorIDs) of
					true ->	SpectatorList = tuple_to_list(Game#gameTable.spectators),
							NewSpectators = list_to_tuple(lists:delete(Username, SpectatorList)),
							SpectatorIDList = tuple_to_list(Game#gameTable.spectatorIDs),
							NewSpectatorIDs = list_to_tuple(lists:delete(DaddyID, SpectatorIDList)),
							mnesia:write(Game#gameTable{spectators = NewSpectators, spectatorIDs = NewSpectatorIDs}),
							[Player] = mnesia:read(nameTable, Username),
							GamesList = tuple_to_list(Player#nameTable.spectating),
							NewSpectating = list_to_tuple(lists:delete(GameID, GamesList)),
							mnesia:write(Player#nameTable{spectating = NewSpectating}),
							{ok, "OK Has dejado de observar la partida "++integer_to_list(GameID)++".\n"};
					false -> {error, "ERROR No es un espectador de ese juego.\n"} end;
			_ -> {error, "ERROR No sabemos sintaxis de Erlang.\n"} end end,
	{atomic, Msg} = mnesia:transaction(F),
	DaddyID ! Msg.
	

checkOtherPlayer(Username, {Username, Player2}, {_, ID2}) -> {Player2, ID2};
checkOtherPlayer(Username, {Player1, Username}, {ID1, _}) -> {Player1, ID1}.


concedeGames(Username, Games, Current, Size) when Current =< Size ->
	case mnesia:read(gameTable, element(Current, Games)) of
		[] -> ok;
		[CurrentGame] -> case tuple_size(CurrentGame#gameTable.players) of
							1 -> Msg = Username ++ " ha abandonado la partida.\n";
							2 -> {OtherPlayer, OtherPlayerID} = checkOtherPlayer(Username, CurrentGame#gameTable.players, CurrentGame#gameTable.playerIDs),
								 Msg = Username ++ " ha abandonado la partida. Ha ganado "++OtherPlayer++"!\n",
								 OtherPlayerID ! {ok, Msg} end,
						 sendToList(tuple_to_list(CurrentGame#gameTable.spectatorIDs), Msg),
						 deleteGame(CurrentGame#gameTable.gameID) end,
	concedeGames(Username, Games, Current + 1, Size);
concedeGames(_, _, _, _) -> ok.

stopSpectatingGames(Username, DaddyID, Games, Current, Size) when Current =< Size ->
	case mnesia:read(gameTable, element(Current, Games)) of
		[] -> ok;
		[Game] -> SpectatorList = tuple_to_list(Game#gameTable.spectators),
				  NewSpectators = list_to_tuple(lists:delete(Username, SpectatorList)),
				  SpectatorIDList = tuple_to_list(Game#gameTable.spectatorIDs),
				  NewSpectatorIDs = list_to_tuple(lists:delete(DaddyID, SpectatorIDList)),
				  mnesia:write(Game#gameTable{spectators = NewSpectators, spectatorIDs = NewSpectatorIDs}) end,
	stopSpectatingGames(Username, DaddyID, Games, Current + 1, Size);
stopSpectatingGames(_, _, _, _, _) -> ok.


closeSession(DaddyID, Username) ->
	F = fun() ->
		[Player] = mnesia:read(nameTable, Username),
		concedeGames(Username, Player#nameTable.playing, 1, tuple_size(Player#nameTable.playing)),
		stopSpectatingGames(Username, DaddyID, Player#nameTable.spectating, 1, tuple_size(Player#nameTable.spectating)),
		mnesia:delete({nameTable, Username}) end,
	{atomic, ok} = mnesia:transaction(F),
	DaddyID ! {close}.
	

init() ->
	case gen_tcp:listen(8000, [{active, true}]) of
    {ok, ListenSocket} ->	io:format("Ando~n"),
							mnesia:delete_schema([node()]),
							mnesia:create_schema([node()]),
							mnesia:start(),
							mnesia:delete_table(ultimoGameID),
							mnesia:create_table(ultimoGameID, [{attributes, record_info(fields, ultimoGameID)}]),
							mnesia:delete_table(nameTable),
							mnesia:create_table(nameTable, [{attributes, record_info(fields, nameTable)}]),
							mnesia:delete_table(gameTable),
							mnesia:create_table(gameTable, [{attributes, record_info(fields, gameTable)}]),
							mnesia:wait_for_tables([ultimoGameID, nameTable, gameTable], 100000),
							mnesia:transaction(fun() -> mnesia:write(#ultimoGameID{dummy = 420, id = 0}) end),
							io:format("Ando~n"),
							dispatcher(ListenSocket);
    {error, eaddrinuse} -> init() end.

 
dispatcher(ListenSocket) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	spawn(?MODULE, dispatcher, [ListenSocket]),
	psocketLogin(Socket).


psocketLogin(Socket) -> 
	receive {tcp, Socket, Msg} -> 	spawn(?MODULE, pcomandoLogin, [self(), Msg]),
									psocketLogin(Socket);
			{ok, Msg, Username} -> gen_tcp:send(Socket, Msg), psocket(Socket, Username);
			{error, Msg} -> gen_tcp:send(Socket, Msg), psocketLogin(Socket);
			{close} -> gen_tcp:close(Socket);
			Msg -> io:format("Login woops: ~p ~n", [Msg]) end.

			
pcomandoLogin(DaddyID, Msg) ->
	MsgList = string:tokens(string:trim(Msg), " "),
	case lists:nth(1, MsgList) of
		"BYE" -> 	case length(MsgList) of
						1 -> DaddyID ! {close};
						_ -> DaddyID ! {error, "ERROR Demasiados argumentos. Modo de uso: BYE\n"}
					end;
		"CON" ->	case length(MsgList) of
						2 -> Username = lists:nth(2, MsgList),
							 case mnesia:transaction(fun() -> mnesia:read(nameTable, Username) end) of
								{aborted, Msg} -> DaddyID ! {error, "ERROR Find transaction broke: "++Msg++"\n"};
								{atomic, []} -> case mnesia:transaction(fun() -> mnesia:write(#nameTable {name = Username}) end) of
													{aborted, Msg} -> DaddyID ! {error, "ERROR Escritura abortada: "++Msg++".\n"};
													{atomic, ok} -> DaddyID ! {ok, "OK.\n", Username}
												end;
								_ -> DaddyID ! {error, "ERROR Nombre ya existente.\n"}
							 end;
						_ -> DaddyID ! {error, "ERROR Cantidad incorrecta de argumentos. Modo de uso: CON NombreDeUsuario\n"}
					end;
						
		_ -> 	DaddyID ! {error, "ERROR Comando no valido. Inicie sesion con CON NombreDeUsuario o desconectese con BYE\n"} end.

 
psocket(Socket, Username) -> 
	receive {tcp, Socket, Msg} -> 	spawn(?MODULE, pcomando, [self(), Msg, Username]), 
									psocket(Socket, Username);
			{ok, Msg} -> gen_tcp:send(Socket, Msg), psocket(Socket, Username);
			{error, Msg} -> gen_tcp:send(Socket, Msg), psocket(Socket, Username);
			{close} -> gen_tcp:close(Socket);
			_ -> io:format("Woops ~n") end.


pcomando(DaddyID, Msg, Username) ->
	MsgList = string:tokens(string:trim(Msg), " "),
	case length(MsgList) of
		0 -> DaddyID ! {error, "ERROR Mensaje vacio.\n"};
		_ -> case lists:nth(1, MsgList) of
					"FIND" -> 	case length(MsgList) of
									2 -> case mnesia:transaction(fun() -> mnesia:read(nameTable, lists:nth(2, MsgList)) end) of
											{aborted, Msg} -> DaddyID ! {error, "ERROR Find transaction broke: "++Msg++"\n"};
											{atomic, []} -> DaddyID ! {ok, "N I S M A N E A D O.\n"};
											_ -> DaddyID ! {ok, "Acata.\n"} 
										end;
									_ -> DaddyID ! {error, "ERROR Cantidad incorrecta de argumentos. Modo de uso: FIND PlayerName\n"}
								end;
								
					"CON" -> 	DaddyID ! {error, "ERROR Sesion ya iniciada.\n"};
					
					"LSG" -> case length(MsgList) of
								1 -> case mnesia:transaction(fun () -> mnesia:match_object({gameTable, '_', '_', '_', {'_'}, '_', '_', '_'}) end) of
										{atomic, Pending} -> case mnesia:transaction(fun () -> mnesia:match_object({gameTable, '_', '_', '_', {'_', '_'}, '_', '_', '_'}) end) of
																{atomic, Full} -> 	MapPending = lists:map(fun({_, GameID, _, _, {Owner}, _, _, _}) -> "ID: "++integer_to_list(GameID)++". Creador: "++Owner++"\n" end, Pending),
																					MapFull = lists:map(fun({_, GameID, _, _, {P1, P2}, _, _, _}) -> "ID: "++integer_to_list(GameID)++". Jugadores: "++P1++" - "++P2++"\n" end, Full),
																					FoldPending = lists:foldr(fun(A,B) -> A ++ B end, "", MapPending),
																					FoldFull = lists:foldr(fun(A,B) -> A ++ B end, "", MapFull),
																					DaddyID ! {ok, "Juegos disponibles:\n" ++ FoldPending ++ "\n\nJuegos llenos:\n" ++ FoldFull ++ "\n"};
																{aborted, Msg} -> DaddyID ! {error, "ERROR Se aborto la busqueda de llenos: "++Msg++"\n"}
															 end;
										{aborted, Msg} -> DaddyID ! {error, "ERROR Se aborto la busqueda de pendientes: "++Msg++"\n"}
									 end;									 
								_ -> DaddyID ! {error, "ERROR Demasiados argumentos. Modo de uso: LSG\n"} 
							 end; 
					% POSSIBLY FIXED
					"NEW" ->	case length(MsgList) of
									1 -> GameID = newGame(Username, DaddyID),
										 DaddyID ! {ok, "Game creado, tu GameID es "++integer_to_list(GameID)++".\n"};
									_ -> DaddyID ! {error, "ERROR Demasiados argumentos. Modo de uso: NEW\n"} 
								end;
					% POSSIBLY FIXED
					"ACC" ->	case length(MsgList) of
									2 -> PosibleGameID = lists:nth(2, MsgList),
									     case is_integer_list(PosibleGameID) of
											true -> GameID = erlang:list_to_integer(PosibleGameID),
													joinGame(DaddyID, GameID, Username);
											false -> DaddyID ! {error, "ERROR Valor de Game ID invalido.\n"} 
										 end;
									_ -> DaddyID ! {error, "ERROR Cantidad incorrecta de argumentos. Modo de uso: ACC GameID\n"}
								end;
					% POSSIBLY FIXED
					"PLA" ->	case length(MsgList) of
									3 -> PosibleGameID = lists:nth(2, MsgList),
										 PosiblePosicion = lists:nth(3, MsgList),
										 case is_integer_list(PosibleGameID) of
											true -> GameID = erlang:list_to_integer(PosibleGameID),
													case is_integer_list(PosiblePosicion) of
														true -> Posicion = erlang:list_to_integer(PosiblePosicion),
																case (Posicion >= 1 andalso Posicion =< 9) of
																	true -> makeMove(DaddyID, GameID, Username, Posicion);
																	false -> DaddyID ! {error, "ERROR Posicion fuera de rango.\n"}
																end;
														false -> DaddyID ! {error, "ERROR Valor de Posicion invalido.\n"} 
													end;
											false -> DaddyID ! {error, "ERROR Valor de GameID invalido.\n"} 
										 end;
									_ -> DaddyID ! {error, "ERROR Cantidad incorrecta de argumentos. Modo de uso: PLA GameID Posicion\n"} 
								end;
					% POSSIBLY FIXED
					"OBS" ->	case length(MsgList) of
									2 -> PosibleGameID = lists:nth(2, MsgList),
									     case is_integer_list(PosibleGameID) of
											true -> GameID = erlang:list_to_integer(PosibleGameID),
													spectateGame(DaddyID, GameID, Username);
											false -> DaddyID ! {error, "ERROR Valor de Game ID invalido.\n"} 
										 end;
									_ -> DaddyID ! {error, "ERROR Cantidad incorrecta de argumentos. Modo de uso: OBS GameID\n"}
								end;
					% POSSIBLY FIXED
					"LEA" -> 	case length(MsgList) of
									2 -> PosibleGameID = lists:nth(2, MsgList),
									     case is_integer_list(PosibleGameID) of
											true -> GameID = erlang:list_to_integer(PosibleGameID),
													leaveGame(DaddyID, GameID, Username);
											false -> DaddyID ! {error, "ERROR Valor de Game ID invalido.\n"} 
										 end;
									_ -> DaddyID ! {error, "ERROR Cantidad incorrecta de argumentos. Modo de uso: LEA GameID\n"}
								end;
					% PLACEHOLDER	
					"BYE" -> 	case length(MsgList) of
									1 -> closeSession(DaddyID, Username);
									_ -> DaddyID ! {error, "ERROR Demasiados argumentos. Modo de uso: BYE\n"}
								end;
								
					_ -> DaddyID ! {error, "ERROR Comando no valido.\n"} 
			 end
	end.
