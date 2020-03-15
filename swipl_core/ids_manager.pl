:- module(ids_manager,[
		next_id/2,
		reset_id/1
	]).

	
	/***********************************************************************************
		next_id(?Id).
		reset_id.
		
		This predicates let manage a set of unique identifiers.
		Id begins in 0.
	************************************************************************************/
	:- dynamic n_id/2.
	
	next_id(Key,Id):-
		n_id(Key,Id),!,
		retractall(n_id(Key,Id)),
		NextId is Id + 1,
		assert(n_id(Key,NextId)).
	
	next_id(Key,1):-
		assert(n_id(Key,2)).
	
	reset_id(Key):-
		retractall(n_id(Key,_)),
		assert(n_id(Key,1)).