:- module(interpreter,[
		consult_grule/2
	]).
	
	:-use_module(utils).
	:-use_module(translator).
	
	% ============================================================================================
	% 		These predicates define how each g-rule premise must be interpreted.
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
		
	not_better(X,Y,C,[FactX, FactY]):- equal(X,Y,C,[FactX, FactY]),!.
	not_better(X,Y,C,[FactX, FactY]):- worse(X,Y,C,[FactX, FactY]).
	
	not_worse(X,Y,C,[FactX, FactY]):- equal(X,Y,C,[FactX, FactY]),!.
	not_worse(X,Y,C,[FactX, FactY]):- better(X,Y,C,[FactX, FactY]).
	
	dont_care(X,Y,C,[FactX, FactY]):- equal(X,Y,C,[FactX, FactY]),!.
	dont_care(X,Y,C,[FactX, FactY]):- better(X,Y,C,[FactX, FactY]),!.
	dont_care(X,Y,C,[FactX, FactY]):- worse(X,Y,C,[FactX, FactY]).
	
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
	

	% ============================================================================================
	% 		These predicates define an interpreter for the g-rules
	% ============================================================================================
	
	/***********************************************************************************
		consult_grule(+GRule, -Facts).
		
		Let GRule = (Premise1, .., PremiseN) ==> Claim. This predicate returns in Facts
		a set of necesary facts to hold with all premises of GRule.
	************************************************************************************/
	consult_grule(Premises ==> _Claim, Facts):-
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