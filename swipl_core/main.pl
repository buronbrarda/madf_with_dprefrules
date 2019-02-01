:- use_module(server).
:- use_module(action_server).
:- use_module(bd_manager, [conectarse/2,desconectarse/0]).


run:-
	%Se crea el controlador de concurrencia.
	mutex_create(main_mutex),	
	set_mutex(main_mutex),
	writeln("Mutex creado"),
	
	%Se conecta a la BD.
	conectarse(autosmart_user,autosmart),
	writeln("Conectado a la BD"),
	
	%Se lanza el planificador.
	thread_create(planificar, _,[alias(planificador),detached(true)]),
	writeln("Planificador lanzado"),
	
	Port = 10000,
	thread_create(create_server(Port), _, [alias(servidor),detached(true)]),
	write("Servidor escuchando en el puerto: "),writeln(Port),
	
	esperar_fin,
	
	%Se desconecta de la BD.
	desconectarse,
		
	writeln("Bye bye!").
	
	
planificar:-
	do_action(acomodar_y_despachar),
	
	sleep(1),
	fin(false) -> planificar ; true.
		
esperar_fin:-
	sleep(20),
	fin(false) -> esperar_fin ; true.