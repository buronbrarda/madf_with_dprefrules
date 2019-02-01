:- module(data_manager,[
		feature/1,
		criterion/1,
		values/2,
		alternative/1,
		profile_rule/2,
		cpref_rule/2,
		
		add_assessed_alternative/2,
		add_alternative/2,
		remove_alternative/1,
		remove_alternatives/0,
		
		add_pair/2,
		remove_pair/1,
		remove_pairs/0,
		
		add_profile_rule/2,
		remove_profile_rule/1,
		remove_profile_rules/0,
		
		add_cpref_rule/2,
		remove_cpref_rule/1,
		remove_cpref_rules/0	
	]).


:- dynamic feature/1.
:- dynamic criterion/1.
:- dynamic values/2.
:- dynamic alternative/1.
:- dynamic profile_rule/2.
:- dynamic cpref_rule/2.


add_assessed_alternative(Id,Knowledge):-
	%Verify singleton alternatives.
	not(alternative(Id)),
	assert(alternative(Id)),
	
	forall(member([Criterion,Value],Knowledge),(
		%Verify criterion-value domain.
		values(Criterion,Domain),
		member(Value,Domain),
		
		Assessment =.. [Criterion, Id, Value],
		assert(Assessment)
	)),!.

% If fails, execute contingency plan and fail.
add_assessed_alternative(Id,_Knowledge):-
	alternative(Id),
	remove_alternative(Id),
	false.
	

add_alternative(Id,Evidence):-
	not(alternative(Id)),!,
	assert(alternative(Id)),
	forall(member([Feature,Value], Evidence),(
		Fact =.. [Feature, Id, Value],
		assert(Fact)
	)).

% If fails, execute contingency plan and fail.	
add_alternative(Id,_Evidence):-
	alternative(Id),
	retract(alternative(Id)),
	false.


remove_alternative(Id):-
	retract(alternative(Id)),
	
	%Remove related evidence.
	forall(feature(Feature),(
		Fact =.. [Feature, Id, _],
		retract(Fact)
	)),
	
	%Remove related assessments.
	forall(criterion(Criterion),(
		Assessment =.. [Criterion, Id, _],
		retract(Assessment)
	)).
	

remove_alternatives:-
	retractall(alternative(_)),
	
	%Remove evidence.
	forall(feature(Feature), (
		Fact =.. [Feature, _, _],
		retractall(Fact)
	)),
	
	%Remove assessments.
	forall(criterion(Criterion), (
		Assessment =.. [Criterion, _, _],
		retractall(Assessment)
	)).
	


add_pair(Criterion, Domain):-
	not(criterion(Criterion)),
	is_list(Domain),
	
	assert(criterion(Criterion)),
	assert(values(Criterion,Domain)).

remove_pair(Criterion):-
	retract(criterion(Criterion)),
	retract(values(Criterion,_)).

remove_pairs:-
	retractall(criterion(_)),
	retractall(values(_,_)).



add_profile_rule(Id, Rule):-
	assert(profile_rule(Id, Rule)).

remove_profile_rule(Id):-
	retract(profile_rule(Id,_)).

remove_profile_rules:-
	retractall(profile_rule(_,_)).



add_cpref_rule(Id, Rule):-
	assert(cpref_rule(Id, Rule)).

remove_cpref_rule(Id):-
	retract(cpref_rule(Id,_)).

remove_cpref_rules:-
	retractall(cpref_rule(_,_)).