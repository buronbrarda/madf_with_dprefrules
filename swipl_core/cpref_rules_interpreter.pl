:- module(cpref_rules_interpreter,[
		op(1101, xfx, ==>),
		
		coherent_cpref_rule/1,
		
		generate_pre_comparisons/0,
		
		consult_cpref_rule/3
	]).
	
	:-use_module(data_manager).
	
	:-dynamic c_relation/4.
	
	:-op(1101, xfx, ==>).
	
	
	generate_pre_comparisons:-
		retractall(c_relation(_,_,_,_)),
		
		forall((
			alternative(X),alternative(Y),Y\=X,
			criterion(C,_),
			relation(C,R,X,Y)
		),(
			assert(c_relation(C,R,X,Y))
		)).
		
		
	relation(C,equal,X,Y):-
		evidence(X,C,V),
		evidence(Y,C,V),!.
	
	relation(C,better,X,Y):-
		evidence(X,C,Vx),
		evidence(Y,C,Vy),
		criterion(C,Values),
		greater_value(Vx,Vy,Values),!.
		
	relation(_C,worse,_X,_Y).
		
	
	% ============================================================================================
	% 		These predicates define how each cpref-rule premises must be interpreted.
	% ============================================================================================
	better(X,Y,C):- 
		c_relation(C,better,X,Y).
	
	min(X,C,Min):-
		evidence(X,C,Vx), criterion(C,Domain), geq_value(Vx,Min,Domain).
	
	max(Y,C,Max):-
		evidence(Y,C,Vy), criterion(C,Domain), geq_value(Max,Vy,Domain).
		
	equal(X,Y,C):-
		c_relation(C,equal,X,Y).
		
	worse(X,Y,C):-
		c_relation(C,worse,X,Y).
	
	not_better(X,Y,C):-
		(c_relation(C,equal,X,Y);c_relation(C,worse,X,Y)).
	
	not_worse(X,Y,C):-
		(c_relation(C,equal,X,Y);c_relation(C,better,X,Y)).
		
	min_distance(X,Y,C,Min_Dist):-
		distance(X,Y,C,Dist), Dist >= Min_Dist .
		
	max_distance(X,Y,C,Min_Dist):-
		distance(X,Y,C,Dist), Dist =< Min_Dist .
		
		
	% ============================================================================================
	% ============================================================================================
		
	
	
	/***********************************************************************************
		greater_value(+V, +U, +Domain).
		geq_Values(+V, +U, +Domain).
		
		Defines whether V > U and V >= U, with respect to their position in the list
		Domain. V > U iff pos(V,Values) > pos(U,Values).
	************************************************************************************/
	greater_value(V,U,number):-
		!, V > U.
		
	greater_value(V,U,-number):-
		!, V < U.
		
	greater_value(V,U,between(X1,X2)):-
		X1 >= X2,!,
		V > U.
		
	greater_value(V,U,between(_,_)):-
		!,U > V.
		
	greater_value(V,U,Domain):-
		nth0(Index_1,Domain,V),
		nth0(Index_2,Domain,U),
		Index_1 > Index_2.
	
	geq_value(V,U,Domain):- 
		not(greater_value(U,V,Domain)).
	
	distance(V,U,Domain,Distance):-
		not(is_list(Domain)),!,
		Distance is abs(V-U).
	
	distance(V,U,Domain,Distance):-
		nth0(Index_1,Domain,V),
		nth0(Index_2,Domain,U),
		is(Distance, abs(Index_1 - Index_2)).
	
	/**********************************************************************************/
	

	% =================================================================================
	% 		These predicates define an interpreter of cpref-rules
	% =================================================================================
	
	%Check "better", "worse", "equal" and "dont_care" clauses conditions.	
	clause_conditions(Premise,Previous_Clauses,[Criterion,Clause]):-
		Premise =.. [Clause,_X,_Y,Criterion],
		member(Clause, [better,worse,equal,dont_care,not_better,not_worse]),!,
		
		criterion(Criterion),
		not(member([Criterion,_],Previous_Clauses)).
		
	
	%Check "min" clauses conditions	
	clause_conditions(min(_X,Criterion,Min_V),Previous_Clauses,[Criterion,min,Min_V]):-
		!,criterion(Criterion),								%Check criterion existence.
		values(Criterion,Domain), member(Min_V,Domain),		%Check criterion domain.
		/*member([Criterion,_Clause],Previous_Clauses),		%Check previous b_premise, w_premise or e_premise.
		
		(%Check that Min_V < Max_V when there exists a previous max clause.
			(member([Criterion,max,Max_V],Previous_Clauses), greater_value(Max_V,Min_V,Domain));
			(not(member([Criterion,max,_],Previous_Clauses)))
		),!,
		*/
		
		not(member([Criterion,min,_Value],Previous_Clauses)).		%Check non-duplicate min.
		
	
	%Check "max" clauses conditions
	clause_conditions(max(_Y,Criterion,Max_V),Previous_Clauses,[Criterion,max,Max_V]):-
		!,criterion(Criterion),								%Check criterion existence.
		values(Criterion,Domain), member(Max_V,Domain),		%Check criterion domain.
		/*member([Criterion,worse],Previous_Clauses),			%Check previous w_premise.
		
		
		(%Check that Min_V < Max_V when there exists a previous min clause.
			(member([Criterion,min,Min_V],Previous_Clauses), greater_value(Max_V,Min_V,Domain));
			(not(member([Criterion,min,_],Previous_Clauses)))
		),!,
		*/
		
		not(member([Criterion,max,_Value],Previous_Clauses)).		%Check non-duplicate max.
	
	
	clause_conditions(min_distance(_X,_Y,Criterion,Min_V),Previous_Clauses,[Criterion,min_distance,Min_V]):-
		!,criterion(Criterion),								%Check criterion existence.
		number(Min_V), Min_V >= 2,							%Check Min_V is number and discard trivial cases with distance < 2.
		
		member([Criterion,better],Previous_Clauses),		%Check previous b_premise.
		
		not(member([Criterion,min_distance,_],Previous_Clauses)).		%Check non-duplicate min_distance.
		
	
	
	clause_conditions(max_distance(_X,_Y,Criterion,Max_V),Previous_Clauses,[Criterion,max_distance,Max_V]):-
		!,criterion(Criterion),								%Check criterion existence.
		number(Max_V),										%Check Max_V is number.
		
		member([Criterion,worse],Previous_Clauses),			%Check previous w_premise.
		
		not(member([Criterion,max_distance,_],Previous_Clauses)).		%Check non-duplicate max_distance.
	
	
	/***********************************************************************************
		coherent_cpref_rule(+CPrefRule).
		
		Checks whther CPrefRule is a coherent CPref-Rule.
		Checks criteria existence and criteria domain that are evaluated in CPrefRule.
		Also, check CPrefRule syntax errors.
	************************************************************************************/
	coherent_cpref_rule(Body ==> pref(X,Y)):-
		coherent_body(Body, [], [X,Y], Clause_Output),
		member([_Criterion, better], Clause_Output),!,		%Check whether it has a b_premise.
		X \== Y.											%Check X and Y are different variables.
	
	
	coherent_body((Premise, Body), Previous_Clauses, [X,Y], [Clause|Clause_Output]):-
		!,clause_conditions(Premise,Previous_Clauses,Clause),
		
		coherent_body(Body, [Clause|Previous_Clauses], [X,Y], Clause_Output).
		
	
	coherent_body(Premise, Previous_Clauses, [_X,_Y], [Clause]):-
		clause_conditions(Premise,Previous_Clauses,Clause).
		
	
	/***********************************************************************************
		consult_cpref_rule(+CPrefRule).
		
		It defines an interpreter for Cpref-Rules.
		CPrefRule is the Rule's Id that is needed to consult.
	************************************************************************************/
	consult_cpref_rule(CPrefRule,Premises,Claim):-
		cpref_rule(CPrefRule,Premises ==> Claim),
		consult_premises(Premises).
	
	
	consult_premises((Premise, Body)):-
		!,call(Premise),
		consult_premises(Body).
	
	
	consult_premises(Premise):-
		call(Premise).