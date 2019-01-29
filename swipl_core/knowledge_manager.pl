:- module(knowledge_manager,[

	]).

:-dynamic criterion/1


add_criterion(Criterion):-
	assert(criterion(Criterion)).

remove_criteria:-
	retract_all(criterion(_)).



