:- module(argumentation_framework,[
		generate_warranted_conclusions/0,
		
		complement/2,
		defeats/2,
		in_conflict/2,
		claim/2,
		rule/2,
		
		dtree_node/5,
		
		warranted_conclusion/1,
		
		defeat_explanation/3,
		justification/4
	]).
	
	:-use_module(ids_manager).
	:-use_module(data_manager, [has_priority/2, importance_statement/2]).
	:-use_module(arg_generator, [argument/3, generate_arguments/0]).
	
	:-dynamic d_in_conflict/2.
	:-dynamic d_defeats/2.
	:-dynamic d_stronger/2.
	:-dynamic d_arg_line/2.
	:-dynamic dtree_node/5.
	:-dynamic d_democratic_stronger_rule/2.
	:-dynamic d_democratic_equivalent_rule/2.
	
	%===================================================================================
	generate_warranted_conclusions:-
		generate_arguments,
		generate_argument_relations,
		generate_dtree_nodes.
	
		
	/***********************************************************************************
		rules(?Arg_Id, ?RuleId).
		
		True when RuleId is the rule's Id of the argument Argument identified with Id.
	************************************************************************************/
	rule(Arg_Id,RuleId):-
		argument(Arg_Id, [cpref_rule(RuleId,_)],_Claim).
		
	
	/***********************************************************************************
		claim(?Arg_Id?, Claim).
		
		True when Claim is the claim of the argument Argument identified with Id.
	************************************************************************************/
	claim(Arg_Id, Claim):-
		argument(Arg_Id, _Rule, Claim).
	
	
	/***********************************************************************************
		complement(+ClaimA, +ClaimB).
		
		Define whether a literal is the complement of another literal.
	************************************************************************************/
	complement(pref(X,Y),pref(Y,X)).
	
	
	/***********************************************************************************
		in_conflict(+ArgA, +ArgB).
		
		Define whether two arguments ArgA and Argb are in conflict.
	************************************************************************************/
	in_conflict(Arg_Id_A, Arg_Id_B):-
		claim(Arg_Id_A, Claim_A),
		claim(Arg_Id_B, Claim_B),
		complement(Claim_A,Claim_B).
	
	
	/***********************************************************************************
		max_support_group(+R1 > +R2, +Agent, ?Support).
		
		True iff Support is the subset of agents from Agents that considers that R1 > R2
		and there is no other agent in Agent that considers that R2 > R1 and has higher
		priority than theirs. 
	************************************************************************************/
	max_support_group(R1 > R2, Agents, Support):-
			findall(AgA,(
				member(AgA,Agents),
				importance_statement(AgA,(R1 > R2)),
				not((
					member(AgB,Agents),
					importance_statement(AgB,(R2 > R1)),
					has_priority(AgB,AgA)
				))
			),Support),!.
	
	/***********************************************************************************
		stronger(+ArgA, +ArgB).
		
		Defines whether ArgA is stronger than Argb.
	************************************************************************************/
	
	
	democratic_strogest_rule_result(RA,RB,_Visited,_SupportA,_SupportB,0,0,none):-
		assert(d_democratic_equivalent_rule(RA,RB)),!.
		
	democratic_strogest_rule_result(RA,RB,_Visited,_SupportA,_SupportB,LengthA,LengthB,RA):-
		LengthA > LengthB,
		assert(d_democratic_stronger_rule(RA,RB)),!.
		
	democratic_strogest_rule_result(RA,RB,_Visited,_SupportA,_SupportB,LengthA,LengthB,RB):-
		LengthB > LengthA,
		assert(d_democratic_stronger_rule(RB,RA)),!.
		
	democratic_strogest_rule_result(RA,RB,Visited,SupportA,SupportB,_,_,Result):-
		union(SupportA,SupportB,Aux),
		append(Visited,Aux,NewVisited),
		democratic_strongest_rule(RA,RB,NewVisited,Result),!.
	
	democratic_strogest_rule(RA,RB,_,RA):-
		d_democratic_strongeer_rule(RA,RB),!.
		
	democratic_strogest_rule(RA,RB,_,none):-
		d_democratic_equivalent_rules(RA,RB),!.
	
	democratic_strogest_rule(RA,RB,_,none):-
		d_democratic_equivalent_rules(RB,RA),!.
	
	democratic_strongest_rule(RA,RB,Visited,Result):-
		findall(Ag,agent(Ag),Agents),
		subtract(Agents,Visited,S),
		max_support_group(RA > RB, S, SupportA),
		max_support_group(RB > RA, S, SupportB),
		length(SupportA,LA),
		length(SupportB,LB),
		democratic_strogest_rule_result(RA,RB,Visited,SupportA,SupportB,LA,LB,Result),!.	
	
	
	stronger(Arg_Id_A,Arg_Id_B):-
		democratic_defeat,!,
		rule(Arg_Id_A, RA),
		rule(Arg_Id_B, RB),
		democratic_strongest_rule(RA,RB,[],Result), %Result must be a free variable.
		RA = Result.
	
	stronger(Arg_Id_A,Arg_Id_B):-
		rule(Arg_Id_A, RA),
		rule(Arg_Id_B, RB),
		importance_statement(AgentA,(RA > RB)),
		forall(importance_statement(AgentB,(RB > RA)), has_priority(AgentA,AgentB)),!.
	
	
	/***********************************************************************************
		defeats(+ArgA, +ArgB).
		
		An argument A defeats an argument B iff they are in conflict and
		B is not stronger than A.
	************************************************************************************/
	defeats(Arg_Id_A,Arg_Id_B):-
		in_conflict(Arg_Id_A,Arg_Id_B),
		not(d_stronger(Arg_Id_B,Arg_Id_A)).	
		
	
	% Defeat and conflict-stronger relation is pre-calculate in order to improve performace.
	generate_argument_relations:-
		retractall(d_democratic_stronger_rule(_,_)),
		retractall(d_democratic_equivalent_rule(_,_)),
		
		retractall(d_defeats(_,_)),
		retractall(d_stronger(_,_)),
		
		forall((
			in_conflict(Arg_Id_A,Arg_Id_B),
			stronger(Arg_Id_A,Arg_Id_B)	
		), assert(d_stronger(Arg_Id_A,Arg_Id_B))),
		
		forall((defeats(Arg_Id_A,Arg_Id_B)), (
			assert(d_defeats(Arg_Id_A,Arg_Id_B))
		)).
	
	
	/***********************************************************************************
		concordant_with(+Arg_A, +Line).
		
		An argument A is concordant with an argumentation line L iff A is not in
		conflict with any argument in L.
	************************************************************************************/
	/*concordant_with(Arg_A,Arg_List):-
		not((member(Arg_B,Arg_List), in_conflict(Arg_A,Arg_B))).	
	*/
	
	
	/***********************************************************************************
		acceptable_arg_line(?Arg_Id, ?Line).
		
		Is true when Line is an acceptable argumentation line for the argument identified
		with Arg_Id.
		An argumentation line L is acceptable iff:
			- Every argument in L defeats its predecessor, and
			- No argument appears twice in L.
			
		NOTE: In the way it is defined, Line is exhaustive.
		An argumentation line L is exhaustive when it is not possible to obtain a longer
		line that remains acceptable.
	************************************************************************************/
	
	acceptable_arg_line(Arg_Id,Inverse_Line):-
		acceptable_arg_line(Arg_Id,[],Line),
		reverse(Line,Inverse_Line).
	
	acceptable_arg_line(Last_Arg,Line,Aux):-
		d_defeats(Defeater,Last_Arg),
		not(member(Defeater,[Last_Arg|Line])),
		acceptable_arg_line(Defeater,[Last_Arg|Line],Aux).
		
	acceptable_arg_line(Last_Arg,Line,[Last_Arg|Line]):-
		not((
			d_defeats(Defeater,Last_Arg),
			not(member(Defeater,[Last_Arg|Line]))
		)).
		
	
	
	/***********************************************************************************
		acceptable_arg_line(?Arg_Id, ?Line).
		
		Is true when Line is an acceptable argumentation line for the argument identified
		with Arg_Id.
		An argumentation line L is acceptable iff:
			- Every argument in L defeats its predecessor,
			- No argument appears twice in L, and
			-  pros(L) and cons(L) are concordant, where pros(L) are the arguments in
			odd positions and cons(L) are the arguments in even positions.
			
		NOTE: In the way it is defined, Line is exhaustive.
		An argumentation line L is exhaustive when it is not possible to obtain a longer
		line that remains acceptable.
	************************************************************************************/
	
	/*acceptable_arg_line(Arg_Id,[Arg_Id|Line]):-
		acceptable_arg_line([],[Arg_Id],Line,con).
		
	acceptable_arg_line(Allies,[E|Enemies],[Defeater|Line],pro):-
		defeats(Defeater,E),
		not(( append(Allies,Enemies,Union), member(Defeater,Union) )),  % Avoid arguments repetition
		concordant_with(Defeater,Allies),
		acceptable_arg_line([E|Enemies],[Defeater|Allies],Line,con).
	
	acceptable_arg_line(Allies,[E|Enemies],[],pro):-
		not(( 
			defeats(Defeater,E),
			not((append(Allies,Enemies,Union), member(Defeater,Union))),
			concordant_with(Defeater,Allies)
		)).
		
	acceptable_arg_line(Allies,[E|Enemies],[Defeater|Line],con):-
		defeats(Defeater,E),
		concordant_with(Defeater,Allies),
		not(( append(Allies,Enemies,Union), member(Defeater,Union) )),  % Avoid arguments repetition
		acceptable_arg_line([E|Enemies],[Defeater|Allies],Line,pro).
	
	acceptable_arg_line(Allies,[E|Enemies],[],con):-
		not(( 
			defeats(Defeater,E),
			not(( append(Allies,Enemies,Union), member(Defeater,Union) )),  % Avoid arguments repetition
			concordant_with(Defeater,Allies)
		)).
	*/
	
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
		dtree_node(Id, Parent, Children, Arg_Id, Status), where:
		  - Id is the node id.
		  - Parent is the node's parent id
		  - Children is the list of node's children ids
		  - Arg_Id is the node's argument id.
		  - Status is the result of the marking procedure.
			  Status is 'U' (undefeated) when every child of the node is marked 'D'
		      Status is 'D' (defeated) when the node has at least one child marked 'U'.
	************************************************************************************/
	
	generate_dtree_nodes:-
		reset_id(dtree_node),
		retractall(dtree_node(_,_,_,_,_)),
		
		forall((argument(Arg_Id,_,_),dialectical_tree(Arg_Id,Tree)), assert_dtree_nodes(Tree,null)).
			
	
	assert_dtree_nodes(Dtree,null):-
		next_id(dtree_node,Node_Id),
		Dtree = (Arg_Id,Subtrees), 
		findall(Child_Id,(member(ST, Subtrees),assert_dtree_nodes(ST,Node_Id,Child_Id)),Children),
		node_status(Children,Status),
		assert(dtree_node(Node_Id,null,Children,Arg_Id,Status)).
		
	assert_dtree_nodes(Dtree,Father,Node_Id):-
		next_id(dtree_node,Node_Id),
		Dtree = (Arg_Id,Subtrees),
		findall(Child_Id,(member(ST, Subtrees),assert_dtree_nodes(ST,Node_Id,Child_Id)),Children),
		node_status(Children,Status),
		assert(dtree_node(Node_Id,Father,Children,Arg_Id,Status)),!.
		
	
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
		claim(Arg_Id, Claim),
		dtree_node(_, null, _, Arg_Id, 'U'),!.
	
	
	
	
	/***********************************************************************************
		defeat_explanation(+Arg_IdA, +Arg_IdB, ?Explanation).
		
		An Explanation is the set of agents that agree that the Arg_IdA's cpref-rule
		is more important that the Arg_IdB's cpref-rule.
		Explanation will be an empty set if none agent agree on that.
	************************************************************************************/
	defeat_explanation(Arg_IdA, Arg_IdB, Explanation):-
		rule(Arg_IdA,Rule_A), rule(Arg_IdB,RuleB),
		findall(Agent,importance_statement(Agent,(Rule_A > RuleB)), Explanation).
	
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
		
	
	
	
		
	