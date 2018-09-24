-module(game).
-export([newGame/2, isIntegerList/1, joinGame/3, spectateGame/3, makeMove/4, leaveGame/3, closeSession/2, listGames/1, isElementOfList/2]).
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

isIntegerList(S) -> try   _ = list_to_integer(S),
							true
					  catch error:badarg -> false
					  end.

isElementOfTuple(E, Tuple) -> isElementOfTuple(E, Tuple, 1, tuple_size(Tuple)).
isElementOfTuple(E, T, I, S) when I =< S ->
    case element(I, T) of
        E -> true;
        _ -> isElementOfTuple(E, T, I+1, S)
    end;
isElementOfTuple(_, _, _, _) -> false.

isElementOfList(_E, []) -> false;
isElementOfList(E, [H|T]) ->
	case E == H of
		true  -> true;
		false -> isElementOfList(E, T) end.


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


listGames(DaddyID) -> case mnesia:transaction(fun () -> mnesia:match_object({gameTable, '_', '_', '_', {'_'}, '_', '_', '_'}) end) of
						{atomic, Pending} -> case mnesia:transaction(fun () -> mnesia:match_object({gameTable, '_', '_', '_', {'_', '_'}, '_', '_', '_'}) end) of
												{atomic, Full} -> 	SortedPending = lists:sort(fun(A, B) -> element(2, A) =< element(2,B) end, Pending),
																	SortedFull = lists:sort(fun(A, B) -> element(2, A) =< element(2,B) end, Full),
																	MapPending = lists:map(fun({_, GameID, _, _, {Owner}, _, _, _}) -> "ID: "++integer_to_list(GameID)++". Creador: "++Owner++"\n" end, SortedPending),
																	MapFull = lists:map(fun({_, GameID, _, _, {P1, P2}, _, _, _}) -> "ID: "++integer_to_list(GameID)++". Jugadores: "++P1++" - "++P2++"\n" end, SortedFull),
																	FoldPending = lists:foldr(fun(A,B) -> A ++ B end, "", MapPending),
																	FoldFull = lists:foldr(fun(A,B) -> A ++ B end, "", MapFull),
																	DaddyID ! {ok, "Juegos disponibles:\n" ++ FoldPending ++ "\n\nJuegos llenos:\n" ++ FoldFull ++ "\n"};
												{aborted, Msg} -> DaddyID ! {error, "ERROR Se aborto la busqueda de llenos: "++Msg++"\n"}
											 end;
						{aborted, Msg} -> DaddyID ! {error, "ERROR Se aborto la busqueda de pendientes: "++Msg++"\n"}
					  end.



broadcastMove(Board, PlayerIDs, SpectatorIDs, CurrentPlayer, GameID) ->
	PlayerListDup = erlang:tuple_to_list(PlayerIDs),
	SpectatorList = erlang:tuple_to_list(SpectatorIDs),
	PlayerList = lists:usort(PlayerListDup),
	io:format("La lista de jugadores es: ~p~n", [PlayerList]),
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
				case isElementOfTuple(Username, Game#gameTable.spectators) or isElementOfTuple(Username, Game#gameTable.players) of
				%~ CAMBIAR MENSAJE ANTES DE ENTREGAR
				true -> {ok, "OK Ya estabas en el juego, no hice nada FeelsEmpleadoEstatalMan\n"};
				false ->	
					Pos = tuple_size(Game#gameTable.spectators) + 1,
					NewSpectators = erlang:insert_element(Pos, Game#gameTable.spectators, Username),
					NewSpectatorIDs = erlang:insert_element(Pos, Game#gameTable.spectatorIDs, DaddyID),
					mnesia:write(Game#gameTable{spectators = NewSpectators, spectatorIDs = NewSpectatorIDs}),
					[Player] = mnesia:read(nameTable, Username),
					Pos2 = tuple_size(Player#nameTable.spectating) + 1,
					NewSpectating = erlang:insert_element(Pos2, Player#nameTable.spectating, GameID),
					mnesia:write(Player#nameTable{spectating = NewSpectating}),
					{ok, "OK Observando el juego "++integer_to_list(GameID)++". Estado actual de la partida:\n" ++ makeBoardString(Game#gameTable.board) ++ "\n"} end;
			_ -> {error, "ERROR No sabemos sintaxis de Erlang.\n"} end end,
	{atomic, Msg} = mnesia:transaction(F),
	DaddyID ! Msg.

leaveGame(DaddyID, GameID, Username) ->
	F = fun() ->
		case mnesia:read(gameTable, GameID) of
			[] -> {error, "ERROR Juego no encontrado.\n"};
			[Game] ->
				case isElementOfTuple(DaddyID, Game#gameTable.spectatorIDs) of
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
