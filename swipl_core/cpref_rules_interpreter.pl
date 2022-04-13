:- module(cpref_rules_interpreter,[
		op(1101, xfx, ==>),
		
		coherent_cpref_rule/1,
		
		generate_pre_comparisons/0,
		
		consult_cpref_rule/3
	]).
	
	:-use_module(data_manager).
	
	:-dynamic c_relation/4.
	
	:-op(1101, xfx, ==>).
	
	
	%To optimize consults in cpref-rules.
	generate_pre_comparisons:-
		retractall(c_relation(_,_,_,_)),
		
		forall((
			alternative(X),alternative(Y),Y\=X,
			criterion(C,_),
			relation(C,R,X,Y)
		),(
			assert(c_relation(C,R,X,Y)) %c_ comes from 'compiled'.
		)).
		
		
	relation(C,equal,X,Y):-
		evidence(X,C,V),
		evidence(Y,C,V),!.
	
	relation(C,better,X,Y):-
		evidence(X,C,Vx),
		evidence(Y,C,Vy),
		criterion(C,Values),
		greater_value(Vx,Vy,Values),!.
		
	relation(C,worse,X,Y):-
		evidence(X,C,Vx),
		evidence(Y,C,Vy),
		criterion(C,Values),
		greater_value(Vy,Vx,Values).
	
	
	% ============================================================================================
	% 		These predicates define how each cpref-rule premises must be interpreted.
	% ============================================================================================
	better(X,Y,C):- 
		c_relation(C,better,X,Y).
		
	equal(X,Y,C):-
		c_relation(C,equal,X,Y).
		
	worse(X,Y,C):-
		c_relation(C,worse,X,Y).
	
	not_better(X,Y,C):-
		not(c_relation(C,better,X,Y)).
	
	not_worse(X,Y,C):-
		not(c_relation(C,worse,X,Y)).
		
	max(X,C,V):-
		not((
			evidence(X,C,Vx),
			criterion(C,Domain),
			greater_value(Vx,V,Domain)
		)).
		
	min(X,C,V):-
		evidence(X,C,Vx),
		criterion(C,Domain),
		geq_value(Vx,V,Domain).
		
	min_dist(X,Y,C,Min_Dist):-
		evidence(X,C,V), evidence(Y,C,U), criterion(C,Domain),
		distance(V,U,Domain,Dist), Dist >= Min_Dist.
		
	max_dist(X,Y,C,Max_Dist):-
		evidence(X,C,V), evidence(Y,C,U), criterion(C,Domain),
		distance(V,U,Domain,Dist), Dist =< Max_Dist.
	
	
	at_least_better_by(X,Y,C,0):-
		!,equal(X,Y,C).
		
	at_least_better_by(X,Y,C,Min_Dist):-
		better(X,Y,C),min_dist(X,Y,C,Min_Dist).
	
	at_most_worse_by(X,Y,C,_):-
		not_worse(X,Y,C),!.
		
	at_most_worse_by(X,Y,C,Max_Dist):-
		evidence(X,C,V), evidence(Y,C,U), criterion(C,Domain),
		distance(V,U,Domain,Dist), Dist =< Max_Dist.
		
	know(X,C):-
		evidence(X,C,_).
		
	unknown(X,C):-
		not(evidence(X,C,_)).
		
	
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
		X1 =< X2,!,
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
	
	%Check "better", "worse", "equal", "not_better" and "not_worse" clauses conditions.	
	clause_conditions(Premise,Previous_Clauses,[Criterion,Clause]):-
		Premise =.. [Clause,_X,_Y,Criterion],
		member(Clause, [better,worse,equal]),!,
		
		ground(Criterion),							%Check groundness.
		criterion(Criterion,_),
		not(member([Criterion,_],Previous_Clauses)). % Check not previous occurrence of a premise evaluating Criterion.
	
	
	clause_conditions(min_dist(_X,_Y,Criterion,Min_V),Previous_Clauses,[Criterion,min_dist,Min_V]):-
		!,ground(Criterion), ground(Min_V),							%Check groundness.
		criterion(Criterion,_),									%Check criterion existence.
		number(Min_V), Min_V >= 1,									%Check Min_V correctness.
		
		member([Criterion,better],Previous_Clauses),				%Check previous b_premise.
		
		not(member([Criterion,min_dist,_],Previous_Clauses)).		%Check non-duplicate min_distance.
		
	
	clause_conditions(at_least_better_by(_X,_Y,Criterion,Min_V),Previous_Clauses,[Criterion,at_least_better_by,Min_V]):-
		!,ground(Criterion), ground(Min_V),							%Check groundness.
		criterion(Criterion,_),										%Check criterion existence.
		number(Min_V), Min_V >= 0,									%Check Min_V correctness.
		
		not(member([Criterion,at_least_better_by,_],Previous_Clauses)),		%Check non-duplicate min_distance.
		not(member([Criterion,at_most_worse_by,_],Previous_Clauses)).		%Check non-duplicate max_distance.
	
	
	clause_conditions(max_dist(_X,_Y,Criterion,Max_V),Previous_Clauses,[Criterion,max_dist,Max_V]):-
		!, ground(Criterion), ground(Max_V),					%Check groundness.
		criterion(Criterion,_),									%Check criterion existence.
		number(Max_V), Max_V >= 1,								%Check Max_V correctness.
		
		member([Criterion,worse],Previous_Clauses),				%Check previous w_premise.
		
		not(member([Criterion,max_dist,_],Previous_Clauses)).		%Check non-duplicate max_distance.
		
	
	clause_conditions(at_most_worse_by(_X,_Y,Criterion,Max_V),Previous_Clauses,[Criterion,at_most_worse_by,Max_V]):-
		!,ground(Criterion), ground(Max_V),							%Check groundness.
		criterion(Criterion,_),										%Check criterion existence.
		number(Max_V), Max_V >= 0,									%Check Min_V correctness.
		
		not(member([Criterion,at_least_better_by,_],Previous_Clauses)),		%Check non-duplicate min_distance.
		not(member([Criterion,at_most_worse_by,_],Previous_Clauses)).		%Check non-duplicate max_distance.
		
			
	clause_conditions(max(_X,Criterion,Max_V),Previous_Clauses,[Criterion,max,Max_V]):-
		!,ground(Criterion), ground(Max_V),						%Check groundness.
		criterion(Criterion,Domain),							%Check criterion existence.
		legal_value(Max_V,Domain),								%Check Max_V correctness.
		
		member([Criterion,_],Previous_Clauses),					%Check previous criterion evaluation.
		not(member([Criterion,max,_],Previous_Clauses)).		%Check no-duplication.		
		
	clause_conditions(known(_Y,Criterion),Previous_Clauses,[Criterion,known,null]):-
		!,ground(Criterion), 				%Check groundness.
		criterion(Criterion,_),				%Check criterion existence.
		
		member([Criterion,at_most_worse_by,_],Previous_Clauses).	%Check previous criterion evaluation.
	
	clause_conditions(min(_X,Criterion,Min_V),Previous_Clauses,[Criterion,min,Min_V]):-
		!, ground(Criterion), ground(Min_V),					%Check groundness.
		criterion(Criterion,Domain),							%Check criterion existence.
		legal_value(Min_V,Domain),								%Check Max_V correctness.
		not(member([Criterion,min,_],Previous_Clauses)).		%Check no-duplication.
		
		
	clause_conditions(unknown(_Y,Criterion),Previous_Clauses,[Criterion,unknown,null]):-
		!,ground(Criterion), 				%Check groundness.
		criterion(Criterion,_),				%Check criterion existence.
		
		not(member([Criterion,at_most_worse_by,_],Previous_Clauses)),
		member([Criterion,min,_],Previous_Clauses).	%Check previous criterion evaluation.
	
	
	/***********************************************************************************
		coherent_cpref_rule(+CPrefRule).
		
		Checks whther CPrefRule is a coherent CPref-Rule.
		Checks criteria existence and criteria domain that are evaluated in CPrefRule.
		Also, check CPrefRule syntax errors.
	************************************************************************************/
	coherent_cpref_rule(Body ==> pref(X,Y)):-
		coherent_body(Body, [], [X,Y], Clause_Output),
		better_check(Clause_Output),						
		X \== Y.											%Check X and Y are different variables.
	
	
	better_check(Clause_Output):-
		member([_Criterion, better], Clause_Output),!. 	%Check whether it has a b_premise.
		
	better_check(Clause_Output):-
		member([_Criterion,at_least_better_by,Min_V], Clause_Output), Min_V > 0,!.	%Check whether it has an exigence.
		
	better_check(Clause_Output):-
		member([Criterion,min,_], Clause_Output), member([Criterion,unknown,null], Clause_Output),!. %Check whether it has risk.
		
	
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