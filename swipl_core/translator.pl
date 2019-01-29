:- module(translator,[
        op(1101, xfx, ==>),
        alternative/1,
        crit/1,
        values/2,
        grule/2,
        
        translate/0,
        
        reset_knowledge/0,
        clean_facts/0,
		clean_alternatives/0,
        add_fact/1,
		add_alternative/1,
        
        show_knowledge/0
    ]).
    
    :-use_module(utils).
    
    :-op(1101, xfx, ==>).
    :-consult(knowledge).
    
    
    translate:-
	    clean_facts,
	    
	    forall(eval_profile_rule(Alternative,Profile_Rule),(
	       Profile_Rule =.. [profile_rule, Criterion, Value, _Expresion],
	       Knowledge_Piece =.. [Criterion, Alternative, Value],
	       assert(Knowledge_Piece)
	    )),
	    
	    check_knowledge_base.
    
    
    eval_profile_rule(Alternative, Profile_Rule):-
	    alternative(Alternative),
	    
	    Profile_Rule =.. [profile_rule, _Criterion, _Value, Expresion],
	    call(Profile_Rule),
	    
	    forall(member(SetCondition,Expresion),(
	       SetCondition =.. [Feature, Condition],
	       eval_profile_condition(Alternative, Feature, Condition)
	    )).
    
    
    eval_profile_condition(Alternative, Feature, set(PosibleValues)):-
	    Evidence_Piece =.. [Feature, Alternative, FeatureValue],
        call(Evidence_Piece),
        member(FeatureValue, PosibleValues),!.
     
    
    eval_profile_condition(Alternative, Feature, range(null, Value_B)):-
        Evidence_Piece =.. [Feature, Alternative, FeatureValue],
        call(Evidence_Piece),
        Value_B >= FeatureValue,!.
        
    eval_profile_condition(Alternative, Feature, range(Value_A, null)):-
        Evidence_Piece =.. [Feature, Alternative, FeatureValue],
        call(Evidence_Piece),
        FeatureValue >= Value_A,!.
    
        
    eval_profile_condition(Alternative, Feature, range(Value_A, Value_B)):-
        Value_A \= null, Value_B \= null,
        Evidence_Piece =.. [Feature, Alternative, FeatureValue],
        call(Evidence_Piece),
        FeatureValue >= Value_A,
        Value_B >= FeatureValue.
    
    %Check Consistency and Completness.
    check_knowledge_base:-
	    forall(crit(C),
	       forall(alternative(A),(
	           Knowledge_piece =.. [C,A,V],
	           findall(V,Knowledge_piece,Values),
	           length(Values, 1)
	       ))  
	    ),!.
	    
	check_knowledge_base:-
		writeln('The knowledge base isn\'t complete or consitent. Check the profile_rules and the evidence.'),
		writeln('==========================================================================================='),
		writeln('KNOWLEDGE-BASE:'),
		show_knowledge.
    
    % ============================================================================================
    % ============================================================================================
    
    
    clean_facts:-
        forall((crit(C), alternative(X)), (F =..[C,X,_], retractall(F))).
    
	clean_alternatives:-
		retractall(alternative(_)).
    
    add_fact(Fact):-
        Fact =.. [C,X,V],
        crit(C),
        alternative(X),
        values(C,Values), member(V,Values),
        assert(Fact).
    
	add_alternative(Alt):-
		assert(alternative(Alt)).
    
    
    reset_knowledge:-
        %Clean evidence.
        clean_facts,    
        
        %Clean data.
        retractall(alternative(_)),                     
        retractall(crit(_)),
        retractall(values(_,_)),
        retractall(grule(_,_)),
        
        %Reload evidence and data.
        consult(knowledge),
        translate.
    
    
    /***********************************************************************************
        show_knowledge.
        
        Prints the evidence in tabular format.
    ************************************************************************************/
    
    show_knowledge:-
        findall(Crit, crit(Crit), Criteria),
        write('\t\t'), forall(member(C,Criteria), (write(C), write('\t\t'))), writeln(''),
        writeln('========================================================================================'),
        forall(alternative(X),(
            write(X), write('\t\t'),
            forall(member(C,Criteria),(
                Fact =.. [C,X,V],
                call(Fact),
                write(V), write('\t\t')
            )), writeln('')
        )).