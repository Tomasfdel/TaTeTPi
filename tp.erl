-module(tp).
-compile(export_all).
-record(nameTable, {name, dummy}).
-record(gameTable, {gameID,
                    board,
                    turn,
                    players,
                    playerIDs,
                    spectators}).
-record(ultimoGameID, {dummy = 420, id}).


newGame(Creator, CreatorID) ->
	GameID = getGameID(),
	F = fun() ->
		mnesia:write(#gameTable{gameID     = GameID,
								board      = newBoard(),
								turn       = 1,
								players    = {Creator},
								playerIDs  = {CreatorID},
								spectators = {} 
								}) end,
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


broadcastMove(Board, PlayerIDs, Spectators, CurrentPlayer, GameID) ->
	PlayerList = erlang:tuple_to_list(PlayerIDs),
	SpectatorList = erlang:tuple_to_list(Spectators),
	BoardString = makeBoardString(Board),
	sendToList(PlayerList, "Tablero updateado correctamente. Estado actual de la partida "++ integer_to_list(GameID) ++":\n" ++ BoardString ++ "\n"),
	sendToList(SpectatorList, "Tablero updateado correctamente. Estado actual de la partida "++ integer_to_list(GameID) ++":\n" ++ BoardString ++ "\n"),
	case checkGameEnd(Board, GameID, CurrentPlayer) of
		{fin, Msg} -> sendToList(PlayerList, Msg),
					  sendToList(SpectatorList, Msg),
					  {atomic, ok} = mnesia:transaction(fun() -> mnesia:delete({gameTable, GameID}) end);
		_ -> ok end.
     
makeMove(DaddyID, GameID, CurrentPlayer, Pos) ->
	F = fun() -> case mnesia:read(gameTable, GameID) of
		[] -> {error, "ERROR Juego no encontrado\n"};
		[{_, GameID, Board, Turn, Players, PlayerIDs, Spectators} = Game] ->
		case tuple_size(Players) of
			2 -> case element(Turn, Players) of
				CurrentPlayer ->
				%~ REVISAR QUE HAYA UNA POSICION VALIDA
					case element(Pos, Board) of
						vacio -> NewBoard = setelement(Pos, Board, turnFig(Turn)),
								 ok = mnesia:write(Game#gameTable{board = NewBoard, turn = changeTurn(Turn)}),
								 %~ Movimiento valido
								 broadcastMove(NewBoard, PlayerIDs, Spectators, CurrentPlayer, GameID),
								 ok;
						_ -> {error, "ERROR Posicion previamente ocupada\n"} end;
				_ -> {error, "ERROR No es tu turno, tramposo!\n"} end;
			_ -> {error, "ERROR Esto no es el solitario, papu.\n"} end;
		%~ CAMBIAR ESTE MENSAJE ANTES DE ENTREGAR
		_ -> {error, "ERROR Instructions unclear, dick stuck in ceiling fan\n"} end end,
	case mnesia:transaction(F) of
		{atomic, {error, Msg}} -> DaddyID ! {error, Msg};
		_ -> ok end.


joinGame(DaddyID, GameID, Username) ->
	F = fun() ->
		case mnesia:read(gameTable, GameID) of
			[] -> DaddyID ! {error, "ERROR Juego no encontrado\n"};
			[Game] ->
				case tuple_size(Game#gameTable.players) of
					1 -> NewPlayers = erlang:insert_element(2, Game#gameTable.players, Username),
						 NewPlaIDs = erlang:insert_element(2, Game#gameTable.playerIDs, DaddyID),
						 mnesia:write(Game#gameTable{players = NewPlayers, playerIDs = NewPlaIDs}),
						 element(1, Game#gameTable.playerIDs) ! {ok, "OK Se han unido a tu juego\n"},
						 {ok, "OK Te uniste al juego "++integer_to_list(GameID)++" Kappa\n"};
					_ -> {error, "ERROR Juego lleno\n"} end;
			_ -> {error, "ERROR No sabemos sintaxis de Erlang\n"} end end,
	{atomic, Msg} = mnesia:transaction(F),
	DaddyID ! Msg.

spectateGame(DaddyID, GameID) ->
	F = fun() ->
		case mnesia:read(gameTable, GameID) of
			[] -> {error, "ERROR Juego no encontrado\n"};
			[Game] ->
				Pos = tuple_size(Game#gameTable.spectators) + 1,
				NewSpectators = erlang:insert_element(Pos, Game#gameTable.spectators, DaddyID),
				mnesia:write(Game#gameTable{spectators = NewSpectators}),
				{ok, "OK Observando el juego "++integer_to_list(GameID)++". Estado actual de la partida:\n" ++ makeBoardString(Game#gameTable.board) ++ "\n"};
			_ -> {error, "ERROR No sabemos sintaxis de Erlang\n"} end end,
	{atomic, Msg} = mnesia:transaction(F),
	DaddyID ! Msg.


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
	pSocketLogin(Socket).

	
%~ Hacer consistente el formato de los mensajes
pSocketLogin(Socket) -> 
	receive {tcp, Socket, Msg} -> 	spawn(?MODULE, pcomandoLogin, [self(), Msg]),
									pSocketLogin(Socket);
			{rambo, ok, Msg, Username} -> gen_tcp:send(Socket, Msg), psocket(Socket, Username);
			{rambo, error, Msg} -> gen_tcp:send(Socket, Msg), pSocketLogin(Socket);
			{close} -> gen_tcp:close(Socket);
			Msg -> io:format("Login woops: ~p ~n", [Msg]) end.

			
pcomandoLogin(DaddyID, Msg) ->
	MsgList = string:tokens(string:trim(Msg), " "),
	case lists:nth(1, MsgList) of
		"BYE" ->DaddyID ! {close};
		"CON" ->Username = lists:nth(2, MsgList),
				F = fun() ->
					case mnesia:read(nameTable, Username) of
						[] -> 	mnesia:write(#nameTable {name = Username}), 
								{rambo, ok, "OK\n", Username};
						_  -> 	{rambo, error, "ERROR Nombre ya existente\n"} end end,
				{atomic, DaddyMsg} = mnesia:transaction(F),
				DaddyID ! DaddyMsg;
		_ -> 	DaddyID ! {rambo, error, "ERROR Comando no valido. Inicie sesion con CON NombreDeUsuario\r\n"} end.


 
psocket(Socket, Username) -> 
	receive {tcp, Socket, Msg} -> 	spawn(?MODULE, pcomando, [self(), Msg, Username]), 
									psocket(Socket, Username);
			%~ Esto deberia cambiar
			{rambo, Msg} -> gen_tcp:send(Socket, Msg), psocket(Socket, Username);
			{ok, Msg} -> gen_tcp:send(Socket, Msg), psocket(Socket, Username);
			{error, Msg} -> gen_tcp:send(Socket, Msg), psocket(Socket, Username);
			{close} -> gen_tcp:close(Socket);
			_ -> io:format("Woops ~n") end.


pcomando(DaddyID, Msg, Username) ->
	MsgList = string:tokens(string:trim(Msg), " "),
	case lists:nth(1, MsgList) of
		"BYE" -> 	DaddyID ! {close};
		
		"CON" -> 	DaddyID ! {rambo, "ERROR Sesión ya iniciada\n"};
					
		"FIND" -> 	case mnesia:transaction(fun() -> mnesia:read(nameTable, lists:nth(2, MsgList)) end) of
					{aborted, Msg} -> DaddyID ! {rambo, "Find transaction broke: "++Msg++"~n"};
					{atomic, []} -> DaddyID ! {rambo, "N I S M A N E A D O\n"};
					_ -> DaddyID ! {rambo, "Acata\n"} end;
					
		"NEW" ->	GameID = newGame(Username, DaddyID),
					DaddyID ! {rambo, "Game creado, tu GameID es "++integer_to_list(GameID)++"\n"};
				
		"ACC" ->	GameID = erlang:list_to_integer(lists:nth(2, MsgList)),
					joinGame(DaddyID, GameID, Username);
		
		"PLA" ->	GameID = erlang:list_to_integer(lists:nth(2, MsgList)),
					Pos = erlang:list_to_integer(lists:nth(3, MsgList)),
					makeMove(DaddyID, GameID, Username, Pos);				
		
		"OBS" ->	GameID = erlang:list_to_integer(lists:nth(2, MsgList)),
					spectateGame(DaddyID, GameID);

		_ -> DaddyID ! {rambo, "ERROR Comando no válido\n"} end.
