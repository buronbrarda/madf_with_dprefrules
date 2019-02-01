:- module(interpreter,[
		consult_cpref_rule/2
	]).
	
	:-use_module(utils).
	:-use_module(knowledge_manager).
	
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