:- module(argumentation_framework,[
		generate_warranted_conclusions/0,
		
		complement/2,
		defeats/2,
		claim/2,
		rules/2,
		subargs/2,
		
		dtree_node/5,
		
		warranted_conclusion/1,
		
		justification/4
	]).
	
	:-use_module(ids_manager).
	:-use_module(data_manager, [has_priority/2, importance_statement/2]).
	:-use_module(arg_generator, [op(200, fx, ~),argument/4, generate_arguments/0, args_count/1]).
	
	:-dynamic d_in_conflict/2.
	:-dynamic d_properly_defeats/2.
	:-dynamic d_blocks/2.
	:-dynamic d_arg_line/2.
	:-dynamic dtree_node/5.
	:-dynamic marked_argument/2.
	
	
	%===================================================================================
	generate_warranted_conclusions:-
		get_time(T1),
		generate_arguments,
		get_time(T2),
		ArgGTime is  round((T2 - T1)*1000),
		writeln(ArgGTime),
		args_count(N),
		writeln(N).
		/*get_time(T3),
		generate_argument_relations,
		get_time(T4),
		RelationsTime is  round((T4 - T3)*1000),
		writeln(RelationsTime),
		generate_marked_arguments.*/
		%generate_dtree_nodes.
		
	
		
	/***********************************************************************************
		rules(?ArgId, ?RuleIds).
		
		True when RuleIds is the set of rule Ids of the argument identified with ArgId.
	************************************************************************************/
	rules(ArgId,RuleIds):-
		argument(ArgId, Rules, _SubArgs, _Claim),
		findall(RuleId, member(cpref_rule(RuleId,_),Rules), RuleIds).
		
	
	/***********************************************************************************
		claim(?ArgId, ?Claim).
		
		True when Claim is the claim of the argument ArgId.
	************************************************************************************/
	claim(ArgId, Claim):-
		argument(ArgId, _Rules, _SubArgs, Claim).
	
	/***********************************************************************************
		claim(?ArgId, ?Subargs).
		
		True when Subargs is the set of the subarguments of the argument ArgId.
	************************************************************************************/
	subargs(ArgId,SubArgs):-
		argument(ArgId, _Rules, SubArgs, _Claim).

	/***********************************************************************************
		complement(+ClaimA, +ClaimB).
		
		Define whether a literal is the complement of another literal.
	************************************************************************************/
	complement(pref(X,Y),pref(Y,X)).
	
	/*complement(pref(X,Y),~pref(X,Y)).
	complement(~pref(X,Y),pref(X,Y)).
	*/
	
	
	/***********************************************************************************
		in_conflict(+ArgA, +ArgB, +SubArgB).
		
		Define whether the argument ArgA is in conflict with argument Argb
		at Subargument SubArgB.
	************************************************************************************/
	in_conflict(Arg_Id_A, Arg_Id_B, Arg_Id_B):-
		claim(Arg_Id_A, Claim_A),
		claim(Arg_Id_B, Claim_B),
		complement(Claim_A,Claim_B).

	in_conflict(Arg_Id_A, Arg_Id_B, SubArgBi):-
		argument(Arg_Id_A, _RulesA, _SubArgsA, ClaimA),
		argument(Arg_Id_B, _RulesB, SubArgsB, ClaimB),
		not(complement(ClaimA,ClaimB)),
		sub_conflict(Arg_Id_A,SubArgsB,SubArgBi).

	sub_conflict(Arg_Id_A,SubArgsB,SubConflictArg):-
		member(SubArgBi, SubArgsB),
		in_conflict(Arg_Id_A, SubArgBi, SubConflictArg),!.	% The cut is for optimization, but it isn't necessary for correctness. 
									  		% It is not possible to have two subarguments with the same claim
	
	/***********************************************************************************
		stronger(+ArgA, +ArgB).
		
		Defines whether ArgA is stronger than Argb.
	************************************************************************************/	
	/*stronger(Arg_Id_A,Arg_Id_B):-
		rules(Arg_Id_A, Rules_A),
		rules(Arg_Id_B, Rules_B),
		forall((member(RA, Rules_A), member(RB,Rules_B)), stronger_rule(RA,RB)),!.*/
		
	stronger(Arg_Id_A,Arg_Id_B):-
		rules(Arg_Id_A, Rules_A),
		rules(Arg_Id_B, Rules_B),
		member(RA, Rules_A),
		member(RB, Rules_B),
		stronger_rule(RA,RB),
		not((
			member(RBPrime, Rules_B),
			stronger_rule(RBPrime,RA)
		)),!.

	/***********************************************************************************
		stronger_rule(+RuleA, +RuleB).
		
		Defines whether RuleA is stronger than RuleB.
	************************************************************************************/
	stronger_rule(RuleA,RuleB):-
		importance_statement(AgentA,(RuleA > RuleB)),
		not((
			importance_statement(AgentB,(RuleB > RuleA)),
			has_priority(AgentB,AgentA)
		)),!.
	
	
	/***********************************************************************************
		defeats(+ArgA, +ArgB).
		
		An argument A defeats an argument B, either if A properly defeats, or blocks B.
		It is defined in term of dynamic facts to improve performance. 
	************************************************************************************/
	defeats(Arg_Id_A,Arg_Id_B):-
		d_properly_defeats(Arg_Id_A,Arg_Id_B).
	
	defeats(Arg_Id_A,Arg_Id_B):-
		d_blocks(Arg_Id_A,Arg_Id_B).
	
	
	/***********************************************************************************
		proper_defeats(+ArgA, +ArgB).
		
		An argument A properly defeats an argument B iff they are in conflict and
		A is stronger than B.
	************************************************************************************/
	properly_defeats(Arg_Id_A,Arg_Id_B):-
		in_conflict(Arg_Id_A,Arg_Id_B,SubArg_B),
		stronger(Arg_Id_A,SubArg_B).
	
	
	/***********************************************************************************
		blocks(+ArgA, +ArgB).
		
		An argument A blocks an argument B iff they are in conflict and A is not
		stronger than B and vice-versa.
	************************************************************************************/
	blocks(Arg_Id_A, Arg_Id_B):-
		in_conflict(Arg_Id_A,Arg_Id_B,SubArg_B),
		not(stronger(Arg_Id_A, SubArg_B)), 
		not(stronger(SubArg_B, Arg_Id_A)).	
		
	
	% Arguments relation in_conflic, properly_defeats and blocks must be pre-calculated
	% in order to improve performace.
	generate_argument_relations:-
		retractall(d_properly_defeats(_,_)),
		retractall(d_blocks(_,_)),
		
		
		forall((properly_defeats(Arg_Id_A,Arg_Id_B)), (
			assert(d_properly_defeats(Arg_Id_A,Arg_Id_B))
		)),
		
		forall((blocks(Arg_Id_A,Arg_Id_B)), (
			assert(d_blocks(Arg_Id_A,Arg_Id_B))
		)).
	
	
	/***********************************************************************************
		conflict_free(+Arg_A, +Line).
		
		An argument A is conflict-free w.r.t a set of arguments S iff A is not in
		conflict with any argument in S. An argument A is not in conflict with other
		argument B iff they do not defeat each other. 
		In this case, it is say that A and B are conflict-free
	************************************************************************************/
	conflict_free(Arg_A,S):-
		forall(member(Arg_B,S), conflict_free(Arg_A,Arg_B)).	
	
	
	conflict_free(Arg_A, Arg_B):-
		not(defeats(Arg_A,Arg_B)),
		not(defeats(Arg_B,Arg_A)).
	
	
	
	generate_marked_arguments:-
		retractall(marked_argument(_,_)),
		findall(A,(argument(A,_,_,_), not(defeats(_,A)) ), Args),
		grounded_mark(Args).
	
	
	grounded_mark([]):-
		%If there is no more undefeated arguments, mark all the remaining (defeated) arguments as D.
		!, forall((argument(A,_,_,_), not(marked_argument(A,_))), assert(marked_argument(A,'D'))).
		
			
	grounded_mark(UndefeatedArgs):-
		%All undefeated arguments are marked U.
		forall(member(A, UndefeatedArgs), assert(marked_argument(A,'U')) ),
		
		%Find all the arguments that are defeated by the undefeated arguments and mark it D.
		findall(B,(
			argument(B,_,_,_),
			not(marked_argument(B,_)),
			marked_argument(A,'U'),
			defeats(A,B)
		), DefeatedArgs),
		list_to_set(DefeatedArgs,Aux),
		forall(member(Defeated,Aux),assert(marked_argument(Defeated,'D'))),
				
		%Find all the arguments that are defeated by the defeated arguments, i.e. the new undefeated args.
		findall(A,(
			argument(A,_,_,_),
			not(marked_argument(A,_)),
			forall(defeats(B,A), marked_argument(B,'D'))
		), Args),
		
		grounded_mark(Args).
	
	/***********************************************************************************
		acceptable_arg_line(?Arg_Id, ?Line).
		
		Is true when Line is an acceptable argumentation line for the argument identified
		with Arg_Id.
		Let pros(L) and cons(L) be the arguments in odd and even positions, repectively.
		An argumentation line L is acceptable iff:
			- Every argument in L defeats its predecessor,
			- There is no repetead argument nor sub-argument in conflict in pros(L),
			- pros(L) and cons(L) are conflict-free.
			
		NOTE_1: In the way it is implemented, Line must be exhaustive.
		An argumentation line L is exhaustive when it is not possible to obtain a longer
		line that remains acceptable.
		
		NOTE_2: This way of defining acceptable_arg_line is compatible with Dung's
		grounded semantics.
	************************************************************************************/
	
	acceptable_arg_line(Arg_Id,[Arg_Id|Line]):-
		acceptable_arg_line([],[Arg_Id],Line,con).
		
	acceptable_arg_line(Allies,[E|Enemies],[Defeater|Line],pro):-
		defeats(Defeater,E),
		conflict_free(Defeater,Allies),
		not(member(Defeater,Allies)),  		% Avoid (sub-)arguments repetition	
		acceptable_arg_line([E|Enemies],[Defeater|Allies],Line,con).
	
	acceptable_arg_line(Allies,[E|_Enemies],[],pro):-
		not(( 
			defeats(Defeater,E),
			conflict_free(Defeater,Allies),
			not(member(Defeater,Allies))  	% Avoid (sub-)arguments repetition		
		)).
		
	acceptable_arg_line(Allies,[E|Enemies],[Defeater|Line],con):-
		defeats(Defeater,E),
		conflict_free(Defeater,Allies),
		in_conflict(Defeater,E,SubArgE),   %Avoid sub-args repetition for pros(L).
		list_to_set([SubArgE,E],Aux),
		append(Aux,Enemies,NewEnemies),
		acceptable_arg_line(NewEnemies,[Defeater|Allies],Line,pro).
	
	acceptable_arg_line(Allies,[E|_Enemies],[],con):-
		not(( 
			defeats(Defeater,E),
			conflict_free(Defeater,Allies)
		)).
		
	/***********************************************************************************
		dialectical_tree(?Arg_Id, ?Tree).
		
		Is true when Tree is a dialectical tree for the argument identified with Arg_Id.
		
		Tree = (Root, [Child_1_Subtree, .. ,Child_N_Subtree])
		Child_N_Subtree = (Child_N, [Child_N1_Subtree, .. ,Child_NN_Subtree])
	************************************************************************************/
	
	dialectical_tree(Arg_Id,Tree):-
		forall(acceptable_arg_line(Arg_Id,Line), assert(d_arg_line(Arg_Id,Line))),
		argumentation_tree(Arg_Id,Arg_Id,[Arg_Id],Tree),
		retractall(d_arg_line(Arg_Id,_)).
	
	
	% Recursively, build an argumentation tree that consider all the 
	% all the argumentation lines builds for Root_Id
	argumentation_tree(Arg_Id,Root_Id,Root_Line,(Arg_Id,SubTrees)):-
		dtree_node_children(Root_Id,Root_Line,Children),
		findall(Tree,(
			member(C_Id,Children),
			append(Root_Line,[C_Id],New_Line),
			argumentation_tree(C_Id,Root_Id,New_Line,Tree)
		),SubTrees).
		
		
	% Search for the children of a node according to the current Root_Line.
	% It consider all the exhaustive argumentation lines for Root_Id
	dtree_node_children(Root_Id,Root_Line,Children):-
		findall(Child_Id,(
			d_arg_line(Root_Id,Line),
			append(Root_Line,[Child_Id|_],Line)		% if append(L1,L2,L3) then prefix(L1,L3)
		), Aux),
		list_to_set(Aux,Children).
	
	
	
	/***********************************************************************************
		In order to improve performance, the marking procedure for dialectical trees
		will be executed during the generation of the linked version of dialectical
		trees that will be used to export the data to other programs.
		Recall that marking procedure is necesary to determine wheter a conclusion is
		warranted within the current instance of the argumentation framework.
		
		===============================================================================
		
		Every node within a linked dialectical tree is a tuple 
		dtree_node(Id, Parent, Children, ArgId, Status), where:
		  - Id is the node id.
		  - Parent is the node's parent id
		  - Children is the list of node's children ids
		  - ArgId is the node's argument id.
		  - Status is the result of the marking procedure.
			  Status is 'U' (undefeated) when every child of the node is marked 'D'
		      Status is 'D' (defeated) when the node has at least one child marked 'U'.
	************************************************************************************/
		
	generate_dtree_nodes:-
		reset_id(dtree_node),
		retractall(dtree_node(_,_,_,_,_)),
		
		forall((argument(ArgId,_,_,_),dialectical_tree(ArgId,Tree)), assert_dtree_nodes(Tree,null)).
			
	
	assert_dtree_nodes(Dtree,null):-
		next_id(dtree_node,Node_Id),
		Dtree = (ArgId,Subtrees), 
		findall(Child_Id,(member(ST, Subtrees),assert_dtree_nodes(ST,Node_Id,Child_Id)),Children),
		node_status(Children,Status),
		assert(dtree_node(Node_Id,null,Children,ArgId,Status)).
		
	assert_dtree_nodes(Dtree,Father,Node_Id):-
		next_id(dtree_node,Node_Id),
		Dtree = (ArgId,Subtrees),
		findall(Child_Id,(member(ST, Subtrees),assert_dtree_nodes(ST,Node_Id,Child_Id)),Children),
		node_status(Children,Status),
		assert(dtree_node(Node_Id,Father,Children,ArgId,Status)),!.
		
	
	%If every child of a node N is marked as U, then N is marked as D.		
	node_status(Children_Ids, 'D'):-
		member(Child_Id,Children_Ids),
		dtree_node(Child_Id,_,_,_,'U'),!.
		
	node_status(_,'U').
	
	
	/***********************************************************************************
		warranted(?Claim).
		
		Defines wheter Claim is warranted.
	************************************************************************************/
	warranted_conclusion(Claim):-
		claim(ArgId, Claim),
		marked_argument(ArgId,'U'),!.
		%dtree_node(_, null, _, ArgId, 'U'),!.
	
	
	/***********************************************************************************
		justification(?Claim, ?Claim_U_Trees, ?NoClaim_U_Trees, ?BothClaim_D_Trees).
		
		Defines the set of dialectical that justify the warranty status of the claim
		Claim. Claim_U_Trees is the set of undefeated dialectical in favour of Claim.
		NoClaim_U_Trees is the set of undefeated dialectical against Claim.
		BothClaim_D_Trees is the set of defeated dialectical trees, both, in favour and
		against Claim.
	************************************************************************************/
	justification(Claim, Claim_U_Trees, NoClaim_U_Trees, BothClaim_D_Trees):-		
		findall(Node_Id, (
			claim(Arg_Id, Claim),
			dtree_node(Node_Id, null, _, Arg_Id, 'U')
		), Aux_1),
		
		list_to_set(Aux_1,Claim_U_Trees),
		
		findall(Node_Id, (
			complement(Claim,No_Claim),
			claim(Arg_Id,No_Claim),
			dtree_node(Node_Id, null, _, Arg_Id, 'U')
		), Aux_2),
		
		list_to_set(Aux_2,NoClaim_U_Trees),
		
		findall(Node_Id, (
			claim(Arg_Id,Claim),
			dtree_node(Node_Id, null, _, Arg_Id, 'D')
		), Claim_D_Trees),
		
		
		findall(Node_Id, (
			complement(Claim,No_Claim),
			claim(Arg_Id,No_Claim),
			dtree_node(Node_Id, null, _, Arg_Id, 'D')
		), NoClaim_D_Trees),
		
			
		append(Claim_D_Trees, NoClaim_D_Trees, Aux_3),
		
		list_to_set(Aux_3,BothClaim_D_Trees).
		
	
	
	export_args_and_defeats:-
		open('AF.dl',write,Out,[create([write])]),
    	
    	forall(argument(Id,_,_,_),(
    		swritef(String1, 'arg(%t).\n',[Id]),
    		write(Out,String1)
    	)),
    	
    	forall(defeats(IdA,IdB),( 
    		swritef(String2, 'att(%t,%t).\n',[IdA,IdB]),
    		write(Out,String2)
    	)),
    	
    	close(Out). 
		
	