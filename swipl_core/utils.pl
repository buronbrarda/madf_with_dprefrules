:- module(utils,[
		next_id/2,
		reset_id/1,
		
		contained/2,
		strict_contained/2,
		equals_sets/2,
		join/3,
		before/3,
		
		subtree/2,
		children/3,
		print_tree/1
	]).

	
	/***********************************************************************************
		next_id(?Id).
		reset_id.
		
		This predicates let manage a set of unique identifiers.
		Id begins in 0.
	************************************************************************************/
	:- dynamic utils:n_id/2.
	
	next_id(Key,Id):-
		utils:n_id(Key,Id),!,
		retractall(utils:n_id(Key,Id)),
		NextId is Id + 1,
		assert(utils:n_id(Key,NextId)).
	
	next_id(Key,1):-
		assert(utils:n_id(Key,2)).
	
	reset_id(Key):-
		retractall(utils:n_id(Key,_)),
		assert(utils:n_id(Key,1)).

	
	/***********************************************************************************
		contained(+SetA, +SetB).
		strict_contained(+SetA, +SetB).
		
		Defines whether SetA is a subset of SetB.
	************************************************************************************/
	contained(SetA,SetB):- forall(member(E,SetA),member(E,SetB)).
	strict_contained(SetA,SetB):- contained(SetA,SetB), not(contained(SetB,SetA)).
	equals_sets(SetA,SetB):-	contained(SetA,SetB),contained(SetB,SetA).
	
	
		
	
	/***********************************************************************************
		join(+List, +E, -PairList).
		
		PairList = { (X,E) | X belongs to List }.
	************************************************************************************/
	join([E1], E2, [(E1,E2)]):-!.
	
	join([E1|List], E2, [(E1,E2)|Result]):-
		join(List,E2,Result).
		
	
	
	/***********************************************************************************
		before(?E1, ?E2, +List).
		
		Represent the relation between the elements in List that are one next to the
		other. The element E1 appears inmediatly before E2 in List.
	************************************************************************************/
	
	before(E1, E2, [E1, E2]):-!.
	
	before(E1, E2, [E1|List]):-
		List = [E2|_].
		
	before(E1, E2, [_|List]):-
		before(E1, E2, List).
		
		
	
	/***********************************************************************************
		subtree(?Subtree, +Tree).
		
		Subtree is a subtree of Tree.
	************************************************************************************/
	
	subtree((E,[]),(E,[])):-!.
	
	subtree(Subtree,Subtree).
	
	subtree((E,Subtree),(_,OtherSubtree)):-
		member(NextSubtree,OtherSubtree),
		subtree((E,Subtree),NextSubtree).
		
		
	/***********************************************************************************
		children(?Children, ?E, +Tree).
		
		Children are the children of E in Tree.
	************************************************************************************/
	
	children([E],null,(E,_)).
	
	children(Children, E, Tree):-
		subtree((E,Subtree),Tree),
		findall(Child, member((Child,_),Subtree), Children).
		
		
	
	/***********************************************************************************
		print_tree(+Tree).
		
		Prints all nodes of a tree ordered by level.
		
		The format which the tree is printed is:
			(Father, Child)
	************************************************************************************/	
	print_tree(Tree):-
		print_tree([null],[],Tree),!.
	
	print_tree([],[],_):-!.
	
	
	print_tree([P|[]],Children,Tree):-
		children(P_Children,P,Tree),
		P_Children \= [],!,
		forall(member(Child, P_Children),(
			write('('),
				write(P), write(','),write(Child),
			write(')  ')
		)),
		writeln(' '),
		writeln('-----------------------------------------------'),
		append(Children,P_Children,All_Children),
		print_tree(All_Children,[],Tree),!.
	
	print_tree([_|[]],_,_):-!.		%This predicate is just to avoid fail when
									%P_Children = [].
	
	print_tree([P|Parents],Children,Tree):-
		children(P_Children,P,Tree),
		forall(member(Child, P_Children),(
			write('('),
				write(P), write(','), write(Child),
			write(')  ')
		)),
		append(Children,P_Children,All_Children),
		print_tree(Parents,All_Children,Tree),!.