:- module(arg_generator,[
		argument/4,			%argument(Id,Rules,Premises,Claim)					
		
		generate_arguments/0,
		count_args/1,
		
		print_arg/1,
		print_args/0,
		print_args/1,
		print_rule/1
	]).
	
	:-use_module(data_manager, [cpref_rule/2]).	
	:-use_module(cpref_rules_interpreter).
	:-use_module(utils, [next_id/2, reset_id/1, equals_sets/2]).
	
	
	:-dynamic argument/4.
	:-dynamic raw_argument/4.
	
	
	
	/***********************************************************************************
		remove_arguments.
		
		Removes the working set of arguments and resets the id-manager.
			
	************************************************************************************/
	remove_arguments:-
		reset_id(arguments),
		reset_id(raw_arguments),
		retractall(argument(_,_,_,_)),
		retractall(raw_argument(_,_,_,_)).	
		
		
	
	/***********************************************************************************
		generate_arguments.
		
		Generates the working set of arguments from the set of cpref-rules, the set of  
		values, the set of alternatives and the evidence.
			
	************************************************************************************/
	generate_arguments:-
		remove_arguments,
					
		% Generates raw_arguments from grules
		forall(cpref_rule(RuleId, Premises ==> Claim),
			forall(consult_cpref_rule(Premises ==> Claim, Facts),(
				next_id(raw_arguments,RawId),
				assert(raw_argument(RawId,RuleId,Facts,Claim))
			))
		),

		% Generates arguments from raw_arguments. (Aggregate rules and assigns Ids)
		forall(argument(Rules, Facts, Claim),(
			gen_arg(Rules,Facts,Claim)
		)),
		
		%Finally, clears all raw_arguments.
		retractall(raw_argument(_,_,_,_)).
	
	
	% Generates arguments from raw_arguments. (Aggregate rules and assigns Ids)
	argument([Rule|Rules], Facts, Claim):-
		raw_argument(Id,Rule,Facts,Claim),
		argument_rules(Rules,[Id],Facts,Claim).
	
	
	argument_rules([Rule|Rules],Ids,OtherFacts,Claim):-
		raw_argument(Id,Rule,Facts,Claim), not(member(Id,Ids)),
		equals_sets(Facts,OtherFacts),!,
		argument_rules(Rules,[Id|Ids],OtherFacts,Claim).
	
	argument_rules([],_Ids,_Facts,_Claim):-!.
	
	
	% Assings id to arguments and avoids argument replication.
	gen_arg(Rules,Facts,Claim):-
		not((argument(_,OtherRules,OtherFacts,Claim), equals_sets(Rules,OtherRules), equals_sets(Facts,OtherFacts))),!,
		next_id(arguments,Id),
		assert(argument(Id,Rules,Facts,Claim)).
	
	
	gen_arg(_Rules,_Facts,_Claim).
	
	
	/***********************************************************************************
		print_args.
		
		Prints all generated arguments.
			
	************************************************************************************/
	print_args:-
		forall(argument(Id,_Rules,_Facts,_Claim), print_arg(Id)).
		
	
	/***********************************************************************************
		print_args(+RuleId).
		
		Prints all generated arguments from the rule RuleId.
			
	************************************************************************************/
	print_args(RuleId):-
		forall((argument(ArgId,Rules,_Facts,_Claim), member(RuleId, Rules)), print_arg(ArgId)).
	
	
	
	print_arg(Id):-
		argument(Id,Rules,Facts,Claim),
		writeln('<'),
			write('\tId: '), writeln(Id),
			write('\tRules: '), writeln(Rules),
			write('\tClaim: '), writeln(Claim),
			writeln('\tPremises:'), do_print_facts(Facts),
		writeln('>').
		
	
	do_print_facts(Facts):-
		writeln('\t{'),
			print_facts(Facts),
		writeln('\t}').
		
	
	print_facts([Fx,Fy]):-
		!,write('\t\t'),write(Fx),write(', '),writeln(Fy).
		
	print_facts([Fx,Fy|Facts]):-
		write('\t\t'),write(Fx),write(', '),writeln(Fy),
		print_facts(Facts).
		
	
	/***********************************************************************************
		count_args(?N).
		
		N is the number of generated arguments.
			
	************************************************************************************/
	count_args(N):-
		findall(_,argument(_,_),Args),
		length(Args,N).
	

	/***********************************************************************************
		printRule(+RuleId).
		
		Prints the grule identified with RuleId.
			
	************************************************************************************/
	print_rule(RuleId):-
		grule(RuleId,Premises ==> Claim),
		Claim =.. [_Head,x,y],
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
		