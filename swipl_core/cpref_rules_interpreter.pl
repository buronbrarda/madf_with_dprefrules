:- module(cpref_rules_interpreter,[
		op(1101, xfx, ==>),
		
		coherent_cpref_rule/1,
		
		consult_cpref_rule/2
	]).
	
	:-use_module(data_manager).
	:-use_module(translator, [assessment/3]).
	
	:-op(1101, xfx, ==>).
	
	% ============================================================================================
	% 		These predicates define how each cpref-rule premises must be interpreted.
	% ============================================================================================
	better(X,Y,C,[Assessment_X, Assessment_Y]):- 
		assessment(C,X,Vx), assessment(C,Y,Vy), Assessment_X =.. [C,X,Vx],  Assessment_Y =.. [C,Y,Vy],
		alternative(X), alternative(Y),
		values(C,Domain), greater_value(Vx,Vy,Domain).
	
	min(X,C,Value,[Assessment]):-
		assessment(C,X,Vx), Assessment=..[C,X,Vx], alternative(X),values(C,Domain), geq_value(Vx,Value,Domain).
	
	max(Y,C,Value,[Assessment]):-
		assessment(C,Y,Vy), Assessment=..[C,Y,Vy], alternative(Y),values(C,Domain), geq_value(Value,Vy,Domain).
		
	equal(X,Y,C,[Assessment_X, Assessment_Y]):-
		assessment(C,X,V), assessment(C,Y,V), Assessment_X=..[C,X,V], Assessment_Y=..[C,Y,V], alternative(X), alternative(Y).
		
	worse(X,Y,C,[Assessment_X, Assessment_Y]):-
		assessment(C,X,Vx), assessment(C,Y,Vy),
		Assessment_X =..[C,X,Vx], Assessment_Y =..[C,Y,Vy],
		values(C,Domain), greater_value(Vy,Vx,Domain).
		
	dont_care(X,Y,C,[Assessment_X, Assessment_Y]):-
		assessment(C,X,Vx), assessment(C,Y,Vy),
		Assessment_X =..[C,X,Vx], Assessment_Y =..[C,Y,Vy].
	
	not_better(X,Y,C,[Assessment_X, Assessment_Y]):-
		(equal(X,Y,C,[Assessment_X, Assessment_Y]); worse(X,Y,C,[Assessment_X, Assessment_Y])).
	
	not_worse(X,Y,C,[Assessment_X, Assessment_Y]):-
		(equal(X,Y,C,[Assessment_X, Assessment_Y]); better(X,Y,C,[Assessment_X, Assessment_Y])).
		
	
	% ============================================================================================
	% ============================================================================================
		
	
	
	/***********************************************************************************
		greater_value(+V, +U, +Domain).
		geq_Values(+V, +U, +Domain).
		
		Defines whether V > U and V >= U, with respect to their position in the list
		Domain. V > U iff pos(V,Values) > pos(U,Values).
	************************************************************************************/
	greater_value(V,U,Domain):-
		nth0(Index_1,Domain,V),
		nth0(Index_2,Domain,U),
		Index_1 > Index_2.
	
	geq_value(V,U,Domain):- 
		nth0(Index_1,Domain,V),
		nth0(Index_2,Domain,U),
		Index_1 >= Index_2.
		
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
		
		member([Criterion,better],Previous_Clauses),		%Check previous w_premise.
		
		not(member([Criterion,min_distance,_],Previous_Clauses)).		%Check non-duplicate min_distance.
		
	
	
	clause_conditions(min_d_difference(_X,_Y,C1,C2,Min_V),Previous_Clauses,[[C1,C2],min_d_difference,Min_V]):-
		!,criterion(C1),criterion(C2),						%Check criterion existence.
		number(Min_V), Min_V > 0,							%Check Min_V is number and discard trivial cases with min_difference = 0.
		
		member([C1,better],Previous_Clauses),				%Check previous b_premise.
		member([C2,worse],Previous_Clauses),				%Check previous w_premise.
		
		not(member([[C1,C2],min_d_difference,_],Previous_Clauses)).		%Check non-duplicate min_distance.
		
	
	clause_conditions(min_d_factor(_X,_Y,C1,C2,Min_V),Previous_Clauses,[[C1,C2],min_d_factor,Min_V]):-
		!,criterion(C1),criterion(C2),						%Check criterion existence.
		number(Min_V), Min_V > 1,							%Check Min_V is number and discard imposible cases with min_factor > 1.
		
		member([C1,better],Previous_Clauses),				%Check previous b_premise.
		member([C2,worse],Previous_Clauses),				%Check previous w_premise.
		
		not(member([[C1,C2],min_d_factor,_],Previous_Clauses)).		%Check non-duplicate min_distance.
	
	
	/***********************************************************************************
		coherent_cpref_rule(+CPrefRule).
		
		Checks whther CPrefRule is a coherent CPref-Rule.
		Checks criteria existence and criteria domain that are evaluated in CPrefRule.
		Also, check CPrefRule syntax errors.
	************************************************************************************/
	coherent_cpref_rule(Body ==> pref(X,Y)):-
		coherent_body(Body, [], [X,Y], Clause_Output),
		member([_Criterion, better], Clause_Output),!,	%Check whether it has a b_premise.
		X \== Y.											%Check X and Y are different variables.
	
	
	coherent_body((Premise, Body), Previous_Clauses, [X,Y], [Clause|Clause_Output]):-
		!,clause_conditions(Premise,Previous_Clauses,Clause),
		
		coherent_body(Body, [Clause|Previous_Clauses], [X,Y], Clause_Output).
		
	
	coherent_body(Premise, Previous_Clauses, [_X,_Y], [Clause]):-
		clause_conditions(Premise,Previous_Clauses,Clause).
		
	
	/***********************************************************************************
		consult_cpref_rule(+CPrefRule, -Facts).
		
		Let CPrefRule = (Premise1, .., PremiseN) ==> Claim. This predicate returns in Facts
		a set of necesary facts to hold with all premises of CPrefRule.
	************************************************************************************/
	consult_cpref_rule(Premises ==> _Claim, Facts):-
		consult_premises(Premises, Facts).
	
	
	consult_premises((Premise, Body), Facts):-
		!,Premise =.. MetaPremise,
		append(MetaPremise, [PremFacts], NewMetaPremise),
		NewPremise =.. NewMetaPremise,
		call(NewPremise),
		consult_premises(Body,BodyFacts),
		union(PremFacts, BodyFacts, Facts).
	
	
	consult_premises(Premise, PremFacts):-
		Premise =.. MetaPremise,
		append(MetaPremise, [PremFacts], NewMetaPremise),
		NewPremise =.. NewMetaPremise,
		call(NewPremise).