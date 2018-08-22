-module(tp).
-compile(export_all).
-import(game, [newGame/2, isIntegerList/1, joinGame/3, spectateGame/3, makeMove/4, leaveGame/3, closeSession/2]).
-record(nameTable, {name, playing = {}, spectating = {}}).
-record(gameTable, {gameID,
                    board,
                    turn = 1,
                    players,
                    playerIDs,
                    spectators = {},
                    spectatorIDs = {}}).
-record(ultimoGameID, {dummy = 420, id}).
-define(TableList, [nameTable, gameTable, ultimoGameID]).


add_node(Node) ->
    case rpc:call(Node, mnesia, system_info, [is_running]) of
	{badrpc, _} ->
        exit("Nodo no disponible.");
	yes ->
            copy_tables(?TableList, Node);
	no ->
	    exit("Mnesia no se esta ejecutando en el nodo que se quiere agregar.") end.

copy_tables([], _) ->
    ok;
copy_tables([T|Tablenguels], Node) ->
    case mnesia:add_table_copy(T, Node, disc_copies) of
	{atomic, ok} ->
	    copy_tables(Tablenguels, Node);
	{aborted, R} ->
	    exit("Error en add_table_copy: " ++ lists:flatten(io_lib:format("~p", [R]))) end.

init() ->
	init(8000).

inito() ->
	init('a@x', 8001).

init(Port) ->
	case gen_tcp:listen(Port, [{active, true}]) of
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

do_init_slave_node(MasterNode) when MasterNode =/= node() ->
  %~ ok = mnesia:create_schema()
  mnesia:start(),
  {ok, _} = mnesia:change_config(extra_db_nodes, [MasterNode]),
  mnesia:change_table_copy_type(schema, node(), disc_copies),
  copy_tables(?TableList, node()).

init(DaddyNode, Port) ->
	case gen_tcp:listen(Port, [{active, true}]) of
    {ok, ListenSocket} ->	io:format("Ando~n"),
							do_init_slave_node(DaddyNode),
							io:format("Ando~n"),
							dispatcher(ListenSocket);
							%~ mnesia:create_schema([node()]),
							%~ mnesia:start(),
							%~ case rpc:call(DaddyNode, mnesia, change_config, [extra_db_nodes, [node()]]) of
								%~ {badrpc, Msg} -> io:format("Error agregando al scheme: ~p~n", [Msg]);
								%~ _ 			  -> case rpc:call(DaddyNode, tp, add_node, [node()]) of
												 %~ {badrpc, Msg} -> io:format("Error copiando tablas: ~p~n", [Msg]);
												 %~ _			   -> mnesia:wait_for_tables(?TableList, 100000),
																  %~ io:format("Ando~n"),
																  %~ dispatcher(ListenSocket)
												 %~ end
								%~ end;
    {error, eaddrinuse} -> init(DaddyNode, Port) end.

 
dispatcher(ListenSocket) ->
	io:format("Entre a dispatcher~n"),
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	io:format("Acepte la conexion~n"),
	spawn(?MODULE, dispatcher, [ListenSocket]),
	psocketLogin(Socket).


psocketLogin(Socket) -> 
	io:format("Entre a psocketLogin~n"),
	receive {tcp, Socket, Msg} -> 	spawn(?MODULE, pcomandoLogin, [self(), Msg]),
									psocketLogin(Socket);
			{ok, Msg, Username} -> gen_tcp:send(Socket, Msg), psocket(Socket, Username);
			{error, Msg} -> gen_tcp:send(Socket, Msg), psocketLogin(Socket);
			{close} -> gen_tcp:close(Socket);
			Msg -> io:format("Login woops: ~p ~n", [Msg]) end.

			
pcomandoLogin(DaddyID, Msg) ->
	io:format("Entre a pcomandoLogin~n"),
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
								A -> io:format("El read devolvio ~p~n", [A]);
								_ 			-> DaddyID ! {error, "ERROR Nombre ya existente.\n"}
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
									     case isIntegerList(PosibleGameID) of
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
										 case isIntegerList(PosibleGameID) of
											true -> GameID = erlang:list_to_integer(PosibleGameID),
													case isIntegerList(PosiblePosicion) of
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
									     case isIntegerList(PosibleGameID) of
											true -> GameID = erlang:list_to_integer(PosibleGameID),
													spectateGame(DaddyID, GameID, Username);
											false -> DaddyID ! {error, "ERROR Valor de Game ID invalido.\n"} 
										 end;
									_ -> DaddyID ! {error, "ERROR Cantidad incorrecta de argumentos. Modo de uso: OBS GameID\n"}
								end;
					% POSSIBLY FIXED
					"LEA" -> 	case length(MsgList) of
									2 -> PosibleGameID = lists:nth(2, MsgList),
									     case isIntegerList(PosibleGameID) of
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
