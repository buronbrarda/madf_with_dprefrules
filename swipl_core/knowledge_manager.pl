:- module(knowledge_manager,[

	]).

:- dynamic feature/1.
:- dynamic criterion/1.
:- dynamic values/2.
:- dynamic alternative/1.
:- dynamic profile_rule/2.
:- dynamic cpref_rule/2.


add_alternative(Id,Evidence):-
	assert(alternative(Id)),
	forall(member([Feature,Value], Evidence),(
		Fact =.. [Feature, Id, Value],
		assert(Fact)
	)).

remove_alternative(Id):-
	assert(alternative(Id)),
	forall(feature(Feature),(
		Fact =.. [Feature, Id, _Value],
		retract(Fact)
	).

remove_alternatives:-
	retract_all(alternative(_)),
	forall(feature(Feature), (
		Fact =.. [Feature, _Id, _Value],
		retract_all(Fact)
	).
	

add_pair(Criterion, Domain):-
	assert(criterion(Criterion))
	assert(values(Criterion,Domain)).

remove_pair(Criterion):-
	retract(criterion(Criterion)),
	retract(values(Criterion,_)).

remove_pairs:-
	retract_all(criterion(_)),
	retract_all(values(_,_).

add_profile_rule(Id, Rule):-
	assert(profile_rule(Id, Rule)).

remove_profile_rule(Id):-
	retract(profile_rule(Id,_)).

remove_profile_rule:-
	retract_all(profile_rule(_,_)).



add_cpref_rule(Id, Rule):-
	assert(cpref_rule(Id, Rule)).

remove_cpref_rule(Id):-
	retract(cpref_rule(Id,_)).

remove_cpref_rules:-
	retract_all(cpref_rule(_,_)).


