:- module(server,[
		create_server/1
	]).

	:- use_module(action_server,[do_action/1,fin/1]).
	:- use_module(library(socket)).
	
	
	/*Crea un servidor que espera solicitudes en Port*/
	create_server(Port):-
		tcp_socket(Socket),		%Crea el socket de solicitudes
		tcp_bind(Socket, Port),	%Enlaza el socket al puerto de escucha
		tcp_listen(Socket, 5),	%Activa el socket para que espere solicitudes
		tcp_open_socket(Socket, AcceptFd, _),	%Asocia un stream al socket
		dispatch(AcceptFd).
	
	/*Deja en espera al servidor hasta que aparezca una solicitud,
	**luego crea un hilo que atiende la nueva solicitud.*/
	dispatch(AcceptFd):-
		%Espera una solicitud y cuando llega instancia
		%Socket=NuevoSocket_Clnt-Svr Peer=IP_clnt
		tcp_accept(AcceptFd, Socket, Peer),
		%Crea un nuevo thread que ejecuta process_client 
		thread_create(process_client(Socket, Peer), _, [detached(true)]),
                      
        %Luego de crear el nuevo thread vuelve a esperar nuevas solicitudes
        fin(false) -> (
        	dispatch(AcceptFd),!
        ); 
        	true.

	
	process_client(Socket, _Peer):-
			tcp_open_socket(Socket,Input,_Output),
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
		
		