:- module(decision_framework,[
		run/2,
		
		assessments/2,	
		
		explicitly_preferred/2,
		weakly_preferred/2,
		transitively_preferred/2,
		strict_preferred/2,
		equivalent/2,
		incomparable/2,
		
		recommended_alternatives/1,
		justification_rules/3,
		equivalent_groups_ranking/1
	]).
	
	:-reexport(data_manager).
	
	:-use_module(arg_generator, [argument/4]).
	:-use_module(translator, [assessments/2, generate_assessments/0]).
	:-use_module(argumentation_framework, [warranted/1, justification/4, generate_warranted_conclusions/0]).
	:-use_module(utils).
	
	:-reexport(profile_rules_interpreter, [op(1020, xfy, is), op(1010, xfy, if), op(1000, xfy, or), op(900, xfy, and), op(800, xfy, in)]).
	
	:-dynamic explicitly_preferred/2.
	:-dynamic reaches/2.
	:-dynamic preference_amount/2.
	
	
	
	run(Selection,Order):-
		init,
		
		recommended_alternatives(Selection),
		
		equivalent_groups_ranking(Order).
		
	
	init:-
		generate_assessments,
		
		generate_warranted_conclusions,
		
		generate_preferences.
				
		
		
	
	/***********************************************************************************
		reset_warranted.
		
		For every warranted conclusion of the form preferred(X,Y) assert a new predicate
		explicitly_preferred(X,Y).
			
	************************************************************************************/
	generate_preferences:-
		%generate explicit preferences.
		retractall(explicitly_preferred(_,_)),
		forall(warranted(pref(X,Y)), assert_preference(X,Y)),
		
		generate_transitive_preferences.
	
	assert_preference(X,Y):-
		not(explicitly_preferred(X,Y)),!,
		assert(explicitly_preferred(X,Y)).
	
	assert_preference(_,_).
	
	
	
	/***********************************************************************************
		weakly_preferred(?X,?Y).
		
		An alternative X is weakly preferred over Y iff X is transitively preferred over
		Y but it is not the case that X is explecitly preferred over Y. That means that
		there is no argument-based explanation to justify that X is preferred over Y.
			
	************************************************************************************/
	weakly_preferred(X,Y):-
		alternative(X), alternative(Y),
		transitively_preferred(X,Y),
		explicitly_preferred(X,Y).
	
	
	/***********************************************************************************
		transitively_preferred(?X,?Y).
		
		An alternative X is transitively preferred over Y iff there exists a transitive
		sequence between X and Y.
			
	************************************************************************************/
	
	transitively_preferred(X,Y):-
		reaches(X,Y),!.
	
	/***********************************************************************************
		strict_preferred(?X,?Y).
		
		True iff X is transitively preferred over Y and Y is not transitively preferred
		over X.
			
	************************************************************************************/
	strict_preferred(X,Y):-
		alternative(X), alternative(Y), X \= Y,
		transitively_preferred(X,Y),
		not(transitively_preferred(Y,X)).	
	

	/***********************************************************************************
		equivalent(?X,?Y).
		
		True iff X is just as preferred as Y.
			
	************************************************************************************/
	equivalent(X,Y):-
		alternative(X), alternative(Y), X \= Y,
		transitively_preferred(X,Y),
		transitively_preferred(Y,X).
	
	equivalent_alternatives(X, [X|Equivalent_Alts]):-
		findall(Y,equivalent(X,Y),Equivalent_Alts).
	
	equivalent_groups(Groups):-
		equivalent_groups([],Groups).
		
	equivalent_groups(Visited, [New_Group|Groups]):-
		alternative(X), not(member(X,Visited)),
		equivalent_alternatives(X,New_Group),
		append(Visited,New_Group,New_Visited),
		equivalent_groups(New_Visited,Groups),!.
		
	equivalent_groups(_, []).

	/***********************************************************************************
		incomparable(?X,?Y).
		
		True iff there not exist any preference relation between X and Y.			
	************************************************************************************/
	incomparable(X,Y):-
		alternative(X), alternative(Y), X \= Y,
		not(transitively_preferred(X,Y)),
		not(transitively_preferred(Y,X)).

	
	/***********************************************************************************
		generate_transitive_preferences.
		
		Generates transitive preferences in terms of the alternatives that are reached
		by another alternative according to the explicit preferences.
		We will say that X reachs Y if there exists a "explict preference path" between
		X and Y. For example, if we get that explicitly_preferred(X,Z) and
		explicitly_preferred(Z,Y), then we can say that X reachs Y through Z. And, also 
		that Y and Z are reached by X.
	************************************************************************************/
	generate_transitive_preferences:-
		retractall(reaches(_,_)),
		forall((
			alternative(X)
		), assert_reached_by(X)).
	
	
	assert_reached_by(X):-
		forall(explicitly_preferred(X,Y),assert_reached_by(X,Y)).
	
	assert_reached_by(X,Y):-
		reaches(X,Y),!.
	
	assert_reached_by(X,Y):-
		X \= Y,!,
		
		assert(reaches(X,Y)),
		
		forall((
			reached_by(Y,Z),not(reaches(X,Z))
		),(
			assert_reached_by(X,Z)
		)).
	
	
	assert_reached_by(X,X).
			
	
	reached_by(X,Y):-
		reaches(X,Y).
	
	reached_by(X,Y):-
		explicitly_preferred(X,Y).
		
	
	/***********************************************************************************
		justification_rules(?X,?Y,?Rules).
		
		Rules are all the cpref-rules that were used to genereates the arguments supporting
		that X is strict preferred over Y. Note that it includes transitives conclusions. 
			
	************************************************************************************/
	justification_rules(X,Y,Rules):-
		explicitly_preferred(X,Y),
		Claim =.. [pref,X,Y],
		findall(Arg_Rules, argument(_,Arg_Rules,_,Claim), Rules_List),
		flatten(Rules_List, Aux),
		list_to_set(Aux, Rules).
	
	
	/***********************************************************************************
		recommended_alternatives(?Alternatives).
		
		Define which Alternatives should be chosen by the decision maker.

	************************************************************************************/
		
	
	recommended_alternatives(Alternatives):-
		findall(X,(
			alternative(X),
			not((alternative(Y), X \= Y, strict_preferred(Y,X)))
		), Aux),  %Just choice those alternatives such that do not have a better one.
		list_to_set(Aux,Alternatives).
		
	
	
	/***********************************************************************************
		equivalent_groups_ranking(?Ranking).
		
		

	************************************************************************************/
	equivalent_groups_ranking(Ranking):-
		assert_preferences_amount,
		sort_groups([],[],Sorted),
		aggregate_sorted_groups(Sorted,[],Ranking),
		retractall(preference_amount(_,_)).
	
	
	assert_preferences_amount:-
		retractall(preference_amount(_,_)),
		equivalent_groups(Groups),
		forall(member(G,Groups), (
				G = [X|_],
				findall(Y,strict_preferred(X,Y),StrictAux),
				list_to_set(StrictAux,StrictList),
				length(StrictList,StrictCount),
				assert(preference_amount(G,StrictCount))
			)).

	sort_groups([],[],Sorted):-
		preference_amount(Group_A,N),
		not((
			preference_amount(Group_B,M),
			Group_A \= Group_B,
			M > N
		)),sort_groups([Group_A],[(Group_A,N)],Sorted),!.

	sort_groups(Visited,[(Group_B,M)|List],Sorted):-
		preference_amount(Group_A,N),
		N =< M,
		not(member(Group_A,Visited)),
		not((
			preference_amount(Group_C,M2),
			not(member(Group_C,Visited)),
			Group_C \= Group_A,
			M2 > N
		)),sort_groups([Group_A|Visited],[(Group_A,N),(Group_B,M)|List],Sorted),!.

	sort_groups(_,L,L).
	
	aggregate_sorted_groups(In_Process,Aux,Aggregated_Set):-
		In_Process = [(_,N)|_],
		
		findall((E,N), (member((E,N),In_Process)), Substract_Equivalent_Set),
		
		findall(E, member((E,N),Substract_Equivalent_Set), Equivalent_Set),
		
		subtract(In_Process,Substract_Equivalent_Set,Substracted_List),
		
		aggregate_sorted_groups(Substracted_List,[Equivalent_Set|Aux],Aggregated_Set),!.
	
	
	aggregate_sorted_groups([],Aggregated_Set,Aggregated_Set).
		
		