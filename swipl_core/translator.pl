:- module(translator,[
        assessment/3,
		
		remove_assessments/1,
		remove_assessments/0,
		
		generate_assessments/0
    ]).
    
    
    :-use_module(data_manager, [alternative/1, criterion/1, profile_rule/2]).
    :-use_module(profile_rules_interpreter).
    
    
    :- dynamic assessment/3.
    
    
    generate_assessments:-
	    remove_assessments,
	    
	    forall(alternative(Alternative),(
	    	forall(profile_rule(_,Rule),
	    		try_to_assess(Rule,Alternative)
			)
		)),
		
		verify_assessments_base.
    
    
    try_to_assess(Rule, Alternative):-
	    Rule = (Criterion is Assessment if _Condition),
	    eval_profile_rule(Rule,Alternative),
	    assert(assessment(Criterion,Alternative,Assessment)),!.
	    
	try_to_assess(_,_).
	
    	
    
    %Check Consistency and Completness.
    verify_assessments_base:-
	    forall(criterion(C),
	       forall(alternative(A),(
	           findall(V,assessment(C,A,V),Values),
	           length(Values, 1)
	       ))  
	    ).
	    
	remove_assessments(Alternative):-
	    retractall(assessment(_,Alternative,_)).
	
	remove_assessments:-
		retractall(assessment(_,_,_)).