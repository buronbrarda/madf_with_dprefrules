:- module(arg_generator,[
		argument/3,			%argument(Id,Rules,Claim)					
		
		generate_arguments/0,
		args_count/1,
		
		print_arg/1,
		print_args/0,
		print_args/1,
		print_rule/1
	]).
	
	:-use_module(data_manager, [cpref_rule/2]).
	:-use_module(cpref_rules_interpreter).
	:-use_module(ids_manager).
	
	
	:-dynamic argument/3.
	
	
	
	/***********************************************************************************
		remove_arguments.
		
		Removes the working set of arguments and resets the id-manager.
			
	************************************************************************************/
	remove_arguments:-
		reset_id(arguments),
		retractall(argument(_,_,_)).	
		
		
	
	/***********************************************************************************
		generate_arguments.
		
		Generates the working set of arguments from the set of cpref-rules, the set of  
		values, the set of alternatives and the evidence.
			
	************************************************************************************/
	generate_arguments:-
	 
		remove_arguments,
		
		generate_pre_comparisons,
					
		% Generates arguments from cpref_rules
		forall(consult_cpref_rule(RuleId,Premises,Claim),(
				next_id(arguments,ArgId),
				assert(argument(ArgId,[cpref_rule(RuleId,Premises ==> Claim)],Claim))
			)
		).
	
	/***********************************************************************************
		print_args.
		
		Prints all generated arguments.
			
	************************************************************************************/
	print_args:-
		forall(argument(Id,_Rules,_Claim), print_arg(Id)).
		
	
	/***********************************************************************************
		print_args(+RuleId).
		
		Prints all generated arguments from the rule RuleId.
			
	************************************************************************************/
	print_args(RuleId):-
		forall((argument(ArgId,Rules,_Claim), member(cpref_rule(RuleId,_), Rules)), print_arg(ArgId)).
	
	
	
	print_arg(Id):-
		argument(Id,Rules,Claim),
		writeln('<'),
			write('\tId: '), writeln(Id),
			writeln('\tRules: {'),
				forall(member(R,Rules),(write('\t\t'),write(R),wrteln(','))),
			writeln('\t}'),
			write('\tClaim: '), writeln(Claim),
		writeln('>').	
	
	/***********************************************************************************
		args_count(?N).
		
		N is the number of generated arguments.
			
	************************************************************************************/
	args_count(N):-
		findall(_,argument(_,_,_),Args),
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
		