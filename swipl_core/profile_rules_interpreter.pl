:- module(profile_rules_interpreter,[
		op(1020, xfy, is),
		op(1010, xfy, if),
		op(1000, xfy, or),
		op(900, xfy, and),
		op(800, xfy, in),
		
		check_profile_rule/1,
		
		eval_profile_rule/2
	]).
	
	
	:-use_module(data_manager).
	
	:-op(1020, xfy, is).
	:-op(1010, xfy, if).
	:-op(1000, xfy, or).
	:-op(900, xfy, and).
	:-op(800, xfy, in).
	
	
	check_profile_rule(Criterion is Assessment if Condition):-
		criterion(Criterion),
		values(Criterion, Domain),
		member(Assessment,Domain),
		check_profile_rule_condition(Condition).
		
	check_profile_rule_condition(Condition_A or Condition_B):-
		check_profile_rule_condition(Condition_A),
		check_profile_rule_condition(Condition_B),!.
		
	check_profile_rule_condition(Condition_A and Condition_B):-
		check_profile_rule_condition(Condition_A),
		check_profile_rule_condition(Condition_B),!.
	
	
	check_profile_rule_condition(Feature < Value):-
		acceptable_expression(Feature,Value),!.
	
	check_profile_rule_condition(Value < Feature):-
		acceptable_expression(Feature,Value),!.
	
	
	check_profile_rule_condition(Feature =< Value):-
		acceptable_expression(Feature,Value),!.
	
	check_profile_rule_condition(Value =< Feature):-
		acceptable_expression(Feature,Value),!.
		
		
	check_profile_rule_condition(Feature > Value):-
		acceptable_expression(Feature,Value),!.
	
	check_profile_rule_condition(Value > Feature):-
		acceptable_expression(Feature,Value),!.	
		
		
	check_profile_rule_condition(Feature >= Value):-
		acceptable_expression(Feature,Value),!.
		
	check_profile_rule_condition(Value >= Feature):-
		acceptable_expression(Feature,Value),!.
		
		
	check_profile_rule_condition(Feature == Value):-
		acceptable_expression(Feature,Value),!.
	
	check_profile_rule_condition(Value == Feature):-
		acceptable_expression(Feature,Value),!.
		
	
		
	check_profile_rule_condition(Feature in Values):-
		forall(member(V,Values),acceptable_expression(Feature,V)).
	
	acceptable_expression(Feature,Value):-
		feature(Feature),
		feature_domain(Feature,Domain),
		acceptable_value(Value,Domain).
	
	acceptable_value(Value,Domain):-
		member(Value,Domain),!.
		
	acceptable_value(Value,interval(Vi,Vf)):-
		Vi =< Value, Value =< Vf.
	
	
	eval_profile_rule((Criterion is Assessment if Condition), Alternative):-
		criterion(Criterion),
		values(Criterion, Domain),
		member(Assessment,Domain),
		alternative(Alternative),
		eval_features(Condition,Alternative).		
		
	eval_features(Condition_A or Condition_B, Alternative):-
		eval_features(Condition_A,Alternative); eval_features(Condition_B,Alternative),!.
		
	eval_features(Condition_A and Condition_B, Alternative):-
		eval_features(Condition_A, Alternative),
		eval_features(Condition_B, Alternative),!.
		
		
		
	eval_features(Value < Feature, Alternative):-
		feature_domain(Feature,Domain),
		fact(Feature,Alternative,Evidence_Value),
		evidence_greater_value(Evidence_Value,Value,Domain),!.
		
	eval_features(Feature > Value, Alternative):-
		feature_domain(Feature,Domain),
		fact(Feature,Alternative,Evidence_Value),
		evidence_greater_value(Evidence_Value,Value,Domain),!.
	
	
	
	eval_features(Value > Feature, Alternative):-
		feature_domain(Feature,Domain),
		fact(Feature,Alternative,Evidence_Value),
		evidence_greater_value(Value,Evidence_Value,Domain),!.
	
	eval_features(Feature < Value, Alternative):-
		feature_domain(Feature,Domain),
		fact(Feature,Alternative,Evidence_Value),
		evidence_greater_value(Value,Evidence_Value,Domain),!.
	
	
	
	eval_features(Value =< Feature, Alternative):-
		feature_domain(Feature,Domain),
		fact(Feature,Alternative,Evidence_Value),
		evidence_geq_value(Evidence_Value,Value,Domain),!.
	
	eval_features(Feature >= Value, Alternative):-
		feature_domain(Feature,Domain),
		fact(Feature,Alternative,Evidence_Value),
		evidence_geq_value(Evidence_Value,Value,Domain),!.
	
	
	
	eval_features(Value >= Feature, Alternative):-
		feature_domain(Feature,Domain),
		fact(Feature,Alternative,Evidence_Value),
		evidence_geq_value(Value,Evidence_Value,Domain),!.
		
	eval_features(Feature =< Value, Alternative):-
		feature_domain(Feature,Domain),
		fact(Feature,Alternative,Evidence_Value),
		evidence_geq_value(Value,Evidence_Value,Domain),!.
	
	
	
	eval_features(Value == Feature, Alternative):-
		fact(Feature,Alternative,Value),!.
		
	eval_features(Feature == Value, Alternative):-
		fact(Feature,Alternative,Value),!.
	
	
	
	eval_features(Feature in Set, Alternative):-
		fact(Feature,Alternative,Value),
		member(Value,Set).
		
		
	
	evidence_greater_value(V1,V2,interval(_,_)):-
		!,V1 > V2.
	
	evidence_greater_value(V1,V2,Domain):-
		nth0(Index_1,Domain,V1),
		nth0(Index_2,Domain,V2),
		Index_1 > Index_2.
		
	
	evidence_geq_value(V1,V2,interval(_,_)):-
		!,V1 >= V2.
	
	evidence_geq_value(V1,V2,Domain):-
		nth0(Index_1,Domain,V1),
		nth0(Index_2,Domain,V2),
		Index_1 >= Index_2.