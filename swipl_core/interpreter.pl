:- module(interpreter,[
		op(1101, xfx, ==>),
		
		coherent_cpref_rule/1,
		
		consult_cpref_rule/2
	]).
	
	:-use_module(utils).
	:-use_module(data_manager).
	
	:-op(1101, xfx, ==>).
	
	% ============================================================================================
	% 		These predicates define how each cpref-rule premises must be interpreted.
	% ============================================================================================
	better(X,Y,C,[FactX, FactY]):- 
		FactX =..[C,X,Vx], FactY =..[C,Y,Vy], call(FactX), call(FactY),
		alternative(X), alternative(Y),
		values(C,Values), greater_value(Vx,Vy,Values).
	
	min(X,C,Value,[Fact]):-
		Fact=..[C,X,Vx], call(Fact),alternative(X),values(C,AssValues), geq_value(Vx,Value,AssValues).
	
	max(Y,C,Value,[Fact]):-
		Fact=..[C,Y,Vy], call(Fact),alternative(Y),values(C,AssValues), geq_value(Value,Vy,AssValues).
		
	equal(X,Y,C,[FactX, FactY]):-
		FactX=..[C,X,V], FactY=..[C,Y,V],call(FactX),call(FactY), alternative(X), alternative(Y).
	
	
	worse(X,Y,C,[FactX, FactY]):- 
		FactX =..[C,X,Vx], FactY =..[C,Y,Vy], call(FactX), call(FactY),
		values(C,Values), greater_value(Vy,Vx,Values).
	
	% ============================================================================================
	% ============================================================================================
		
	
	
	/***********************************************************************************
		greater_value(+V, +U, +Values).
		geq_Values(+V, +U, +Values).
		
		Defines whether V > U and V >= U, with respect to their position in the list
		Values. V > U iff pos(V,Values) > pos(U,Values).
	************************************************************************************/
	greater_value(V,U,Values):-
		pos(V,Values,PosV),
		pos(U,Values,PosU),
		PosV > PosU.
	
	geq_value(V,U,Values):- 
		member(V,Values),
		member(U,Values),
		not(greater_value(U,V,Values)).

	
	pos(V, [V|_], 0):-!.
	
	pos(V, [_|T], Out):-
		pos(V, T, Pos),
		Out is Pos + 1.
	
	/**********************************************************************************/
	

	% =================================================================================
	% 		These predicates define an interpreter of cpref-rules
	% =================================================================================
	
	premise(better).
	premise(worse).
	premise(equal).
	
	
	/***********************************************************************************
		coherent_cpref_rule(+CPrefRule).
		
		Checks whther CPrefRule is a coherent CPref-Rule.
		Checks criteria existence and criteria domain that are evaluated in CPrefRule.
		Also, check CPrefRule syntax errors.
	************************************************************************************/
	coherent_cpref_rule(Body ==> pref(X,Y)):-
		coherent_body(Body, [], [X,Y], Criteria_Output),
		member([_Criterion, better], Criteria_Output),!,	%Check whether it has a b_premise.
		X \== Y.											%Check X and Y are different variables.
	
	%Check better, worse and equal clauses.	
	coherent_body((Premise, Body), Criteria, [X,Y], [[C,PType]|Criteria_Output]):-
		premise(PType),
		Premise =.. [PType,X,Y,C],!,
		
		criterion(C),								%Check criterion existence.
		not(member([C,_PType],Criteria)),			%Check non-duplicate criterion.
		
		coherent_body(Body, [[C,PType]|Criteria], [X,Y], Criteria_Output).
		
	
	%Check min(X,C,V) clauses.
	coherent_body((Premise, Body), Criteria, [X,Y], Criteria_Output):-
		Premise =.. [min,X,C,V],!,
		
		criterion(C),								%Check criterion existence.
		values(C,Domain), member(V,Domain),			%Check criterion domain.
		member([C,_PType],Criteria),				%Check previous b_premise, w_premise or e_premise.
		
		not(member([C,min,_Value],Criteria)),		%Check non-duplicate min.
		
		coherent_body(Body, [[C,min,V]|Criteria], [X,Y], Criteria_Output).
	
	
	
	%Check max(Y,C,V) clauses.
	coherent_body((Premise, Body), Criteria, [X,Y], Criteria_Output):-
		Premise =.. [max,Y,C,Max_V],!,
		
		criterion(C),								%Check criterion existence.
		values(C,Domain), member(Max_V,Domain),		%Check criterion domain.
		member([C,worse],Criteria),					%Check previous w_premise.
		
		
		(%Check that Min_V < Max_V when there exists a previous min clause.
			(member([C,min,Min_V],Criteria), greater_value(Max_V,Min_V,Domain));
			(not(member([C,min,_],Criteria)))
		),!,
		
		not(member([C,max,_Value],Criteria)),		%Check non-duplicate max.
		
		coherent_body(Body, [[C,max,Max_V]|Criteria], [X,Y], Criteria_Output).
		
	
	%Check better, worse and equal clauses.	
	coherent_body(Premise, Criteria, [X,Y], [[C,PType]]):-
		premise(PType),
		Premise =.. [PType,X,Y,C],!,
		
		criterion(C),								%Check criterion existence.
		not(member([C,_PremiseType],Criteria)).		%Check non-duplicate criterion.
		
		
	%Check min(X,C,V) clauses.
	coherent_body(Premise, Criteria, [X,_Y], []):-
		Premise =.. [min,X,C,V],!,
		
		criterion(C),								%Check criterion existence.
		values(C,Domain), member(V,Domain),			%Check criterion domain.
		member([C,_PType],Criteria),				%Check previous b_premise, w_premise or e_premise.
		
		not(member([C,min,_Value],Criteria)).		%Check non-duplicate min.
	
	
	
	%Check max(Y,C,V) clauses.
	coherent_body(Premise, Criteria, [_X,Y], []):-
		Premise =.. [max,Y,C,Max_V],!,
		
		criterion(C),								%Check criterion existence.
		values(C,Domain), member(Max_V,Domain),		%Check criterion domain.
		member([C,worse],Criteria),					%Check previous w_premise.
		
		
		(%Check that Min_V < Max_V when there exists a previous min clause.
			(member([C,min,Min_V],Criteria), greater_value(Max_V,Min_V,Domain));
			(not(member([C,min,_],Criteria)))
		),!,
		
		not(member([C,max,_Value],Criteria)).		%Check non-duplicate max.

	
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