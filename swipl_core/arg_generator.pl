:- module(arg_generator,[
		op(200, fx, ~),
		argument/4,			%argument(Id,Rules,Claim)					
		
		generate_arguments/0,
		args_count/1,
		
		print_arg/1,
		print_args/0,
		print_args/1,
		print_rule/1
	]).
	
	:-use_module(data_manager, [alternative/1,cpref_rule/2]).
	:-use_module(cpref_rules_interpreter).
	:-use_module(ids_manager).
	
	:-op(200, xf, ~).

	:-dynamic argument/4.
	
	
	/***********************************************************************************
		remove_arguments.
		
		Removes the working set of arguments and resets the id-manager.
			
	************************************************************************************/
	remove_arguments:-
		reset_id(arguments),
		retractall(argument(_,_,_,_)).	
		
		
	
	/***********************************************************************************
		generate_arguments.
		
		Generates the working set of arguments from the set of cpref-rules, the set of  
		values, the set of alternatives and the evidence.
			
	************************************************************************************/
	generate_arguments:-
	 
		remove_arguments,
		
		generate_pre_comparisons,
					
		% Generates arguments from cpref_rules
		forall(consult_cpref_rule(RuleId,Premises,pref(X,Y)),(
			next_id(arguments,ArgId),
			assert(argument(ArgId,[cpref_rule(RuleId,Premises ==> pref(X,Y))],[],pref(X,Y)))
			
		)),
		
		% Generate opposite-arguments from the previous generated arguments.
		% Note that opposite-arguments can be derived from other arguments.
		forall((alternative(X), alternative(Y), X\=Y, rules_for_preference(X,Y,Rules,SubArgs)),(
			next_id(arguments,ArgId),
			assert( argument(ArgId,Rules,SubArgs, ~pref(Y,X) ) )
		)). 


	/***********************************************************************************
		rules_for_preference(+X,+Y,?Rules).
		
		True iff Rules is a set of rules whose claims derive pref(X,Y), where X and Y
		must be instantiated with two alternatives.
			
	************************************************************************************/
	rules_for_preference(X,Y,Rules,SubArgs):-
		rules_for_preference(X,Y,Rules,SubArgs,[],[]).

	rules_for_preference(X,Y,[Rule],[ArgId],Visited,VisitedClaims):-
		argument(ArgId,[Rule],_,pref(X,Y)),
		not(member(ArgId,Visited)),
		not(member(pref(X,Y),VisitedClaims)), %Ensure non-trivialiaty.
		not(member(pref(Y,X),VisitedClaims)). %Ensure consistency of arguments.
	
	rules_for_preference(X,Z,[Rule|Rules],[ArgId|SubArgs],Visited,VisitedClaims):-
		argument(ArgId,[Rule],_,pref(X,Y)),
		Y \= Z,
		not(member(ArgId,Visited)),
		not(member(pref(X,Y),VisitedClaims)), %Ensure non-trivialiaty.
		not(member(pref(Y,X),VisitedClaims)), %Ensure consistency of arguments.
		rules_for_preference(Y,Z,Rules,SubArgs,[ArgId|Visited],[pref(X,Y)|VisitedClaims]).

	
	/***********************************************************************************
		print_args.
		
		Prints all generated arguments.
			
	************************************************************************************/
	print_args:-
		forall(argument(Id,_Rule,_,_Claim), print_arg(Id)).
		
	
	/***********************************************************************************
		print_args(+RuleId).
		
		Prints all generated arguments from the rule RuleId.
			
	************************************************************************************/
	print_args(RuleId):-
		forall((argument(ArgId,[cpref_rule(RuleId,_)],_,_Claim), cpref_rule(RuleId,_)), print_arg(ArgId)).
		
	
	
	
	print_arg(Id):-
		argument(Id,RuleIds,_,Claim),
		writeln('<'),
			write('\tId: '), writeln(Id),
			write('\tRules: '), 
				forall(member(Id,RuleIds),(
					write('\t\t- '),print_rule(Id)
				)),
			write('\tClaim: '), writeln(Claim),
		writeln('>').	
	
	/***********************************************************************************
		args_count(?N).
		
		N is the number of generated arguments.
			
	************************************************************************************/
	args_count(N):-
		findall(Id,argument(Id,_,_,_),Args),
		length(Args,N).
	

	/***********************************************************************************
		printRule(+RuleId).
		
		Prints the grule identified with RuleId.
			
	************************************************************************************/
	print_rule(RuleId):-
		cpref_rule(RuleId,Premises ==> Claim),
		Claim =.. [_Head,'X','Y'],
		write(Premises),write(' ==> '),writeln(Claim).
	
		
	help_me:-
		writeln('Arguments Generator Predicates:'),
		writeln('\t- reset_arguments: Generates the working set of arguments based on the set of g-rules, values, the set of alternatives and the evidence.'),
		writeln('\t- print_arg(+Id): Prints the argument identefied by Id.'),
		writeln('\t- print_args: Prints all generated arguments.'),
		writeln('\t- print_args(+RuleId): Prints all generated arguments from the rule RuleId.'),
		writeln('\t- count_args(?N): N is the number of generated arguments.'),
		writeln('\t- print_rule(+RuleId): Prints the grule identified with RuleId.'),
		writeln('\t- help_me: Shows this text.'),
		writeln(' '),writeln('================================================================='),writeln(' ').
		