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
-define(PstatTimeout, 5000).
-define(Infinite, 100000).

getpbalanceID() -> whereis(balance).

sendToNodes([], _) -> ok;
sendToNodes([Node | NodeList], Workload) -> Nodepbalance = rpc:call(Node, tp, getpbalanceID, []),
                                            Nodepbalance ! {update, node(), Workload},
                                            sendToNodes(NodeList, Workload).


pstat() -> receive after ?PstatTimeout -> sendToNodes([node() | nodes()], statistics(total_active_tasks)) end,
           pstat().

minWorkloadNode(Node1, Workload1, {Node2, Workload2}) -> 
      io:format("N1: ~p  W1: ~p~n{N2, W2} = ~p~n", [Node1, Workload1, {Node2, Workload2}]),
      if Workload1 < Workload2 ->  {Node1, Workload1};
         true -> {Node2, Workload2} end.

pbalance(Dictionary) -> receive {update, Node, Workload} -> pbalance(dict:store(Node, Workload, Dictionary));
                                {getmin, Pid} ->  io:format("El diccionario es: ~p~n", [Dictionary]),
                                                  io:format("Llamo al fold~n"),
                                                  FoldResult = dict:fold(fun minWorkloadNode/3, {dummyNode, ?Infinite}, Dictionary),
                                                  io:format("El resultado del fold es: ~p~n", [FoldResult]),
                                                  Pid ! {balanceResponse, element(1, FoldResult)},
                                                  pbalance(Dictionary) end.



copyTables([], _) ->
    ok;
copyTables([T|Tablenguels], Node) ->
    case mnesia:add_table_copy(T, Node, disc_copies) of
	{atomic, ok} ->
	    copyTables(Tablenguels, Node);
	{aborted, R} ->
	    exit("Error al copiar las tablas: " ++ lists:flatten(io_lib:format("~p", [R]))) end.

connectToNode(MasterNode) ->
  % Es necesario este when?
  % when MasterNode =/= node() 
  pong = net_adm:ping(MasterNode),
  mnesia:start(),
  %~ Agrega el nodo al schema
  {ok, _} = mnesia:change_config(extra_db_nodes, [MasterNode]),
  mnesia:change_table_copy_type(schema, node(), disc_copies),
  copyTables(?TableList, node()).


%~ Auxiliar para testear rapido
init() ->
	init(8000).
	
%~ Auxiliar para testear rapido
inito() ->
	init('a@x', 8001).

init(Port) ->
	case gen_tcp:listen(Port, [{active, true}]) of
    {ok, ListenSocket} ->	io:format("Ando~n"),
							register(balance, spawn(?MODULE, pbalance, [dict:store(node(), statistics(total_active_tasks), dict:new())])),
							spawn(?MODULE, pstat, []),
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

init(DaddyNode, Port) ->
	case gen_tcp:listen(Port, [{active, true}]) of
    {ok, ListenSocket} ->	io:format("El socket vive, la lucha sigue~n"),
							register(balance, spawn(?MODULE, pbalance, [dict:store(node(), statistics(total_active_tasks), dict:new())])),
							spawn(?MODULE, pstat, []),
							connectToNode(DaddyNode),
							io:format("Conectado con exito~n"),
							dispatcher(ListenSocket);
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
								_ 			-> DaddyID ! {error, "ERROR Nombre ya existente.\n"}
							 end;
						_ -> DaddyID ! {error, "ERROR Cantidad incorrecta de argumentos. Modo de uso: CON NombreDeUsuario\n"}
					end;
						
		_ -> 	DaddyID ! {error, "ERROR Comando no valido. Inicie sesion con CON NombreDeUsuario o desconectese con BYE\n"} end.

 
psocket(Socket, Username) -> 
	receive {tcp, Socket, Msg} -> 	whereis(balance) ! {getmin, self()},
	                                receive {balanceResponse, SpawningNode} -> spawn(SpawningNode, tp, pcomando, [self(), Msg, Username]), 
									                                           psocket(Socket, Username) end;
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
					% POSSIBLY FIXED	
					"BYE" -> 	case length(MsgList) of
									1 -> closeSession(DaddyID, Username);
									_ -> DaddyID ! {error, "ERROR Demasiados argumentos. Modo de uso: BYE\n"}
								end;
								
					_ -> DaddyID ! {error, "ERROR Comando no valido.\n"} 
			 end
	end.
