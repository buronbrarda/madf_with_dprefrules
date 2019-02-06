:- module(data_manager,[
		feature/1,
		criterion/1,
		values/2,
		alternative/1,
		profile_rule/2,
		cpref_rule/2,
		fact/3,
		assessment/3,
		
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
:- dynamic fact/3.
:- dynamic assessment/3.

:-use_module(cpref_rules_interpreter, [coherent_cpref_rule/1, op(1101, xfx, ==>)]).


add_assessed_alternative(Id,Knowledge):-
	%Verify singleton alternatives.
	not(alternative(Id)),
	assert(alternative(Id)),
	
	forall(member([Criterion,Value],Knowledge),(
		%Verify criterion-value domain.
		values(Criterion,Domain),
		member(Value,Domain),!,
		
		assert(assessment(Criterion, Id, Value))
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
		feature(Feature),
		assert(fact(Feature, Id, Value))
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
		retract(fact(Feature,Id,_))
	)),
	
	%Remove related assessments.
	forall(criterion(Criterion),(
		retract(assessment(Criterion,Id,_))
	)).
	

remove_alternatives:-
	retractall(alternative(_)),
	
	%Remove related evidence.
	retractall(fact(_,_,_)),
	
	%Remove assessments.
	retractall(assessment(_,_,_)).
	


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
	coherent_cpref_rule(Rule),
	assert(cpref_rule(Id, Rule)).

remove_cpref_rule(Id):-
	retract(cpref_rule(Id,_)).

remove_cpref_rules:-
	retractall(cpref_rule(_,_)).
	
	/*
	
	%=============== JUST TO DEBUG ==============%
	
	alternative(a1). 
    alternative(a2). 
    alternative(a3). 
    alternative(a4).
    
    assessment(cost,a1,bad).
    assessment(location,a1,good).
    assessment(size,a1,reg).
    
    assessment(cost,a2,good).
    assessment(location,a2,reg).
    assessment(size,a2,good).
    
    assessment(cost,a3,vgood).
    assessment(location,a3,bad).
    assessment(size,a3,vbad).
    
    assessment(cost,a4,bad).
    assessment(location,a4,reg).
    assessment(size,a4,good).
    
	
	% ========================================
    %       Criteria
    % ========================================
    criterion(cost).
    criterion(location).
    criterion(size).
    
    % ========================================
    %       Values
    % ========================================
    values(cost,[vbad,bad,reg,good,vgood]).
    values(location,[vbad,bad,reg,good,vgood]).
    values(size,[vbad,bad,reg,good,vgood]).
    
    
    % ========================================
    %       CP - Rules
    % ========================================
    
    %--Tim's Rules
    
    % R1: 
    cpref_rule(r1, 
        better(X,Y,cost) ==> pref(X,Y)
    ).
    
    % R2: 
    cpref_rule(r2, (
        better(X,Y,location),
        equal(X,Y,cost) ==> pref(X,Y)
    )).
    
    % R3:
    cpref_rule(r3, (
        better(X,Y,size),
        equal(X,Y,location),
        equal(X,Y,cost) ==> pref(X,Y)
    )).
    
    %================================================================
    
    %--August's Exceptions
    
    % R4: 
    cpref_rule(r4, (
        better(X,Y,location), min(X,location,good),
        worse(X,Y,cost) ==> pref(X,Y)
    )).
    
    % R5: 
    cpref_rule(r5, (
        better(X,Y,location), min(X,location,reg),
        worse(X,Y,cost), min(X,cost,bad) ==> pref(X,Y)
    )).
    
    %================================================================
    
    %--Kate's Exceptions
    
    % R6: 
    cpref_rule(r6, (
        better(X,Y,size), min(X,size,reg),
        worse(X,Y,location), min(X,location,bad), max(Y,location,good),
        better(X,Y,cost) ==> pref(X,Y)
    )).
    
    % R7: 
    cpref_rule(r7, (
        equal(X,Y,cost),
        worse(X,Y,location), min(X,location,bad), max(Y,location,good),
        better(X,Y,size), min(X,size,reg) ==> pref(X,Y)
    )).
	
	
	%--Kate's extra rule.
	
	% R8: Impose the minimal requirements to prefer the size over the location.
    cpref_rule(r8, (
        better(X,Y,cost), min(X,cost,good),
        better(X,Y,size), min(X,size,reg) ==> pref(X,Y)
    )).
    
    */