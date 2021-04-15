:- module(decision_framework,[		
		run/4,
		
		argument/3,
		dtree_node/5,
		
		complement/2,
		defeats/2,
		rule/2,
		claim/2,
		
		explicitly_preferred/2,
		weakly_preferred/2,
		transitively_preferred/2,
		strict_preferred/2,
		equivalent/2,
		incomparable/2,
		
		selected_alternative/1,
		selected_alternatives/1,
		justification_rules/3,
		ranking_parent/2,
		
		justification/4
	]).
	
	:-reexport(data_manager).
	:-reexport(cpref_rules_interpreter,[op(1101, xfx, ==>)]).
	:-use_module(arg_generator, [argument/3, args_count/1]).
	:-use_module(argumentation_framework).
	
	:-dynamic explicitly_preferred/2.
	:-dynamic reaches/2.
	:-dynamic preference_amount/3.
	:-dynamic ranking_parent/2.
	
	
	
	run(Selection,Args_Count,Reasoning_Time,Selection_Time):-
		init(T1,T2),
		
		selected_alternatives(Selection),
		
		generate_equivalent_groups_ranking,
			
		get_time(T3),
			
		is(Reasoning_Time, round((T2 - T1)*1000)),
		is(Selection_Time, round((T3 - T2)*1000)),
		
		args_count(Args_Count).
		
	
	init(T1,T2):-
		
		get_time(T1),
		
		generate_warranted_conclusions,
		
		get_time(T2),
		
		generate_preferences.
				
		
		
	
	/***********************************************************************************
		reset_warranted.
		
		For every warranted conclusion of the form preferred(X,Y) assert a new predicate
		explicitly_preferred(X,Y).
			
	************************************************************************************/
	generate_preferences:-
		%generate explicit preferences.
		retractall(explicitly_preferred(_,_)),
		forall((
			alternative(X), alternative(Y), X \= Y,
			warranted_conclusion(pref(X,Y))
		), assert_preference(X,Y)),
		
		generate_transitive_preferences.
	
	assert_preference(X,Y):-
		not(explicitly_preferred(X,Y)),!,
		assert(explicitly_preferred(X,Y)).
	
	assert_preference(_,_).
	
	
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
		weakly_preferred(?X,?Y).
		
		An alternative X is weakly preferred over Y iff X is transitively preferred over
		Y but it is not the case that X is explicitly preferred over Y. That means that
		there is no argument-based explanation to justify that X is preferred over Y.
			
	************************************************************************************/
	weakly_preferred(X,Y):-
		alternative(X), alternative(Y),
		transitively_preferred(X,Y),
		not(explicitly_preferred(X,Y)).

	/***********************************************************************************
		equivalent(?X,?Y).
		
		True iff X is just as preferred as Y.
			
	************************************************************************************/
	equivalent(X,X).
	
	equivalent(X,Y):-
		alternative(X), alternative(Y), X \= Y,
		transitively_preferred(X,Y),
		transitively_preferred(Y,X).
	
	equivalent_groups(Groups):-
		equivalent_groups([],Groups).
		
	equivalent_groups(Visited, [New_Group|Groups]):-
		alternative(X), not(member(X,Visited)),!,
		findall(Y, equivalent(X,Y), New_Group),
		append(New_Group,Visited,New_Visited),
		equivalent_groups(New_Visited,Groups),!.
		
	equivalent_groups(_, []).

	/***********************************************************************************
		incomparable(?X,?Y).
		
		True iff there not exist any preference relation between X and Y.			
	************************************************************************************/
	incomparable(X,Y):-
		alternative(X), alternative(Y), X\=Y,
		not(transitively_preferred(X,Y)),
		not(transitively_preferred(Y,X)).
	
		
	
	/***********************************************************************************
		justification_rules(?X,?Y,?Rules).
		
		Rules are all the cpref-rules that were used to genereates the arguments supporting
		that X is strict preferred over Y. Note that it includes transitives conclusions.
		
		Both X and Y can be a list of alternatives.
			
	************************************************************************************/
	justification_rules(X,Y,Rules):-
		explicitly_preferred(X,Y),
		findall(Arg_Rule, (
			claim(Arg_Id,pref(X,Y)),
			rule(Arg_Id,Arg_Rule)
		),Rule_List),
		list_to_set(Rule_List, Rules),!.
		
	justification_rules(X,Y,[]):-
		not(is_list(X)),
		not(is_list(Y)),
		not(explicitly_preferred(X,Y)),!.
		
	justification_rules(X,[Y|Group],Rules):-
		explicitly_preferred(X,Y),
		findall(Arg_Rule, (
			claim(Arg_Id,pref(X,Y)),
			rule(Arg_Id,Arg_Rule)
		),Rule_List_X),
		justification_rules(X,Group,Rules_G),!,
		append(Rule_List_X, Rules_G, Aux),
		list_to_set(Aux,Rules).
		
	justification_rules(X,[Y|Group],Rules_G):-
		not(explicitly_preferred(X,Y)),
		justification_rules(X,Group,Rules_G),!.
		
	justification_rules(_,[],[]):-!.
		
	justification_rules([X|Group],Y,Rules):-
		explicitly_preferred(X,Y),
		findall(Arg_Rule, (
			claim(Arg_Id,pref(X,Y)),
			rule(Arg_Id,Arg_Rule)
		),Rule_List_X),
		justification_rules(Group,Y,Rules_G),!,
		append(Rule_List_X, Rules_G, Aux),
		list_to_set(Aux,Rules).
		
	justification_rules([X|Group],Y,Rules_G):-
		not(explicitly_preferred(X,Y)),
		justification_rules(Group,Y,Rules_G),!.
		
	justification_rules([],_,[]):-!.
	
	
	justification_rules(Group_X,Group_Y,Rules):-		
		is_list(Group_X),
		is_list(Group_Y),
		
		findall(Aux_Rules, (member(X,Group_X), justification_rules(X,Group_Y,Aux_Rules)), Aux_List),
		
		flatten(Aux_List, Aux_Flat),
		
		list_to_set(Aux_Flat,Rules).
	
	/***********************************************************************************
		selected_alternatives(?Alternatives).
		
		Define which Alternatives should be chosen by the decision maker.

	************************************************************************************/
		
	
	selected_alternatives(Alternatives):-
		findall(X,selected_alternative(X), Aux),  
		list_to_set(Aux,Alternatives).
	
	
	%Just choice those alternatives such that do not have a better one.
	selected_alternative(X):-
		alternative(X),
		not((alternative(Y), X \= Y, strict_preferred(Y,X))).
	
	/***********************************************************************************
		generate_equivalent_groups_ranking
		
		

	************************************************************************************/
	
	
	generate_equivalent_groups_ranking:-	
		retractall(ranking_parent(_,_)),
		equivalent_groups(Groups),
		forall((member(G,Groups), G=[X|_], not(strict_preferred(_,X))),(
			assert(ranking_parent(null,G)),
			generate_ranking(G,Groups)
		)).
	
	
	generate_ranking(Parent,Groups):-
		not(ranking_parent(Parent,_)),!,
		children_groups(Parent,Children,Groups),
		forall((member(G,Children)),(
			assert(ranking_parent(Parent,G)),
			generate_ranking(G,Groups)
		)).
	
	generate_ranking(_,_).
	
	children_groups(Parent,Children,Groups):-
		Parent=[X|_],
		findall(Child,(
			member(Child,Groups),
			Child = [Y|_],
			strict_preferred(X,Y),
			not( (member([Z|_],Groups), strict_preferred(X,Z), strict_preferred(Z,Y)) )
		),Children).