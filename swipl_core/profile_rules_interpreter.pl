:- module(profile_rules_interpreter,[
		op(1020, xfx, is),
		op(1010, xfx, if),
		op(1000, xfx, or),
		op(900, xfx, and),
		op(800, xfx, in),
		
		eval_profile_rule/2
	]).
	
	
	:-use_module(data_manager).
	
	:-op(1020, xfx, is).
	:-op(1010, xfx, if).
	:-op(1000, xfx, or).
	:-op(900, xfx, and).
	:-op(800, xfx, in).
	
	eval_profile_rule((Criterion is Assessment if Condition), Alternative):-
		criterion(Criterion),
		values(Criterion, Domain),
		member(Assessment,Domain),
		alternative(Alternative),
		trace,
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
		
		
	
	evidence_greater_value(V1,V2,real_numbers):-
		!,V1 > V2.
	
	evidence_greater_value(V1,V2,Domain):-
		nth0(Index_1,Domain,V1),
		nth0(Index_2,Domain,V2),
		Index_1 > Index_2.
		
	
	evidence_geq_value(V1,V2,real_numbers):-
		!,V1 >= V2.
	
	evidence_geq_value(V1,V2,Domain):-
		nth0(Index_1,Domain,V1),
		nth0(Index_2,Domain,V2),
		Index_1 >= Index_2.