:- module(server,[
		create_server/1
	]).

	:- use_module(interface).
	:- use_module(library(socket)).
	
	
	/*Create a TCP-IP server listening at Port*/
	create_server(Port):-
		tcp_socket(SocketId),		%Create the socket
		tcp_bind(SocketId, Port),	%Bind the socket to the port
		tcp_listen(SocketId, 5),	%Tells the socket to keep listening to incoming conections. It allows up to 5 pending conections.
		tcp_open_socket(SocketId, StreamPair),	%Asocia un stream al socket
		dispatch(StreamPair).
	
	/*Deja en espera al servidor hasta que aparezca una solicitud,
	**luego crea un hilo que atiende la nueva solicitud.*/
	dispatch(StreamPair):-
		%Espera una solicitud y cuando llega instancia
		%SocketId=NuevoSocket_Clnt-Svr Peer=IP_clnt
		tcp_accept(StreamPair, NewSocketId, Peer),
		%Crea un nuevo thread que ejecuta process_client 
		thread_create(process_client(NewSocketId, Peer), _, [detached(true)]),
        dispatch(StreamPair).

	
	process_client(Socket, _Peer):-
		tcp_open_socket(Socket,StreamPair),
		stream_pair(StreamPair,Input,_Output),
		handle_service(Input),!.

	handle_service(Input):-
		%Se lee el stream.
		read_line_to_codes(Input, Command),
		format(atom(String), '~s', [Command]),
		term_to_atom(Term,String),
		
		%===IMPORTANTE===
		%%Las conexiones son cerradas por el cliente.
		
		%Se efectua la accion.
		do_action(Term),!.	