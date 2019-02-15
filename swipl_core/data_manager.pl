:- module(data_manager,[
		feature/1,
		feature_domain/2,
		criterion/1,
		values/2,
		alternative/1,
		profile_rule/2,
		cpref_rule/2,
		fact/3,
		
		add_alternative/2,
		
		remove_alternative/1,
		remove_alternatives/0,
		
		add_criterion/2,
		remove_criterion/1,
		remove_criteria/0,
		
		add_feature/2,
		remove_feature/1,
		remove_features/0,
		
		add_profile_rule/2,
		remove_profile_rule/1,
		remove_profile_rules/0,
		
		add_cpref_rule/2,
		remove_cpref_rule/1,
		remove_cpref_rules/0
	]).


	:- dynamic feature/1.
	:- dynamic feature_domain/2.
	:- dynamic criterion/1.
	:- dynamic values/2.
	:- dynamic alternative/1.
	:- dynamic profile_rule/2.
	:- dynamic cpref_rule/2.
	:- dynamic fact/3.
	
	
	:-use_module(cpref_rules_interpreter, [coherent_cpref_rule/1, op(1101, xfx, ==>)]).
	:-use_module(profile_rules_interpreter).
	:-use_module(translator, [assessment/3, remove_assessments/1, remove_assessments/0]).
	
	add_alternative(Id,Evidence):-
		not(alternative(Id)),!,
		assert(alternative(Id)),
		forall(member([Feature,Value], Evidence),(
			feature(Feature),
			assert(fact(Feature, Id, Value))
		)).
	
	% If fails, execute contingency plan and fail.	
	add_alternative(Id,_Evidence):-
		alternative(Id),
		retract(alternative(Id)),
		false.
	
	
	remove_alternative(Id):-
		retract(alternative(Id)),
		
		%Remove related evidence.
		forall(feature(Feature),(
			retract(fact(Feature,Id,_))
		)),
		
		%Remove related assessments.
		remove_assessments(Id).
		
	
	remove_alternatives:-
		retractall(alternative(_)),
		
		%Remove related evidence.
		retractall(fact(_,_,_)),
		
		%Remove assessments.
		remove_assessments.
		
	
	
	add_criterion(Criterion, Domain):-
		not(criterion(Criterion)),
		is_set(Domain),
		
		assert(criterion(Criterion)),
		assert(values(Criterion,Domain)).
	
	remove_criterion(Criterion):-
		retract(criterion(Criterion)),
		retract(values(Criterion,_)).
	
	remove_criteria:-
		retractall(criterion(_)),
		retractall(values(_,_)).
	
	
	add_feature(Feature, Domain):-
		not(feature(Feature)),
		(Domain = real_numbers; is_set(Domain)),!,
		
		assert(feature(Feature)),
		assert(feature_domain(Feature,Domain)).
	
	remove_feature(Feature):-
		retract(feature(Feature)),
		retract(feature_domain(Feature,_)).
	
	remove_features:-
		retractall(feature(_)),
		retractall(feature_domain(_,_)).
	
	
	add_profile_rule(Id,Atom_Rule):-
		atom(Atom_Rule),!,
		term_to_atom(Rule,Atom_Rule),
		check_profile_rule(Rule),
		assert(profile_rule(Id, Rule)).
	
	add_profile_rule(Id, Rule):-
		check_profile_rule(Rule),
		assert(profile_rule(Id, Rule)).
	
	remove_profile_rule(Id):-
		retract(profile_rule(Id,_)).
	
	remove_profile_rules:-
		retractall(profile_rule(_,_)).
	
	
	
	add_cpref_rule(Id, Rule):-
		coherent_cpref_rule(Rule),
		assert(cpref_rule(Id, Rule)).
	
	remove_cpref_rule(Id):-
		retract(cpref_rule(Id,_)).
	
	remove_cpref_rules:-
		retractall(cpref_rule(_,_)).
	
	
	
	
	%=============== JUST TO DEBUG ==============%
	
	/*
	
	alternative(a1). 
    alternative(a2). 
    alternative(a3).
    alternative(a4).
	alternative(a5). 
    alternative(a6). 
    alternative(a7). 
    alternative(a8).
	alternative(a9). 
    alternative(a10). 
    alternative(a11). 
    alternative(a12). 
    alternative(a13). 
    alternative(a14).
	alternative(a15). 
    alternative(a16). 
    alternative(a17). 
    alternative(a18).
	alternative(a19). 
    alternative(a20).
	alternative(a21). 
    alternative(a22). 
    alternative(a23). 
    alternative(a24).
	alternative(a25). 
    alternative(a26). 
    alternative(a27). 
    alternative(a28).
	alternative(a29). 
    alternative(a30).
    
    fact(price, a1, 615).	fact(distance, a1, 135).              
    fact(price, a2, 360).	fact(distance, a2, 55).
    fact(price, a3, 250).	fact(distance, a3, 80).
    fact(price, a4, 370).	fact(distance, a4, 15).
	fact(price, a5, 430).	fact(distance,a5,5).      		     
	fact(price, a6, 375).	fact(distance,a6,90).
	fact(price, a7, 390).	fact(distance,a7,10).
	fact(price, a8, 600).	fact(distance,a8,90).
	fact(price, a9, 320).	fact(distance,a9,10).
	fact(price, a10, 630).	fact(distance,a10,140).
	fact(price, a11, 700).	fact(distance,a11,80).
	fact(price, a12, 335).	fact(distance,a12,45).
	fact(price, a13, 485).	fact(distance,a13,100).       
	fact(price, a14, 260).	fact(distance,a14,45).
	fact(price, a15, 490).	fact(distance,a15,20).
	fact(price, a16, 290).	fact(distance,a16,15).
	fact(price, a17, 250).	fact(distance,a17,180).
	fact(price, a18, 570).	fact(distance,a18,40).
	fact(price, a19, 495).	fact(distance,a19,160).
	fact(price, a20, 690).	fact(distance,a20,130).
	fact(price, a21, 280).	fact(distance,a21,50).         
	fact(price, a22, 300).	fact(distance,a22,150).
	fact(price, a23, 500).	fact(distance,a23,120).
	fact(price, a24, 665).	fact(distance,a24,150).
	fact(price, a25, 565).	fact(distance,a25,70).
	fact(price, a26, 360).	fact(distance,a26,150).
	fact(price, a27, 475).	fact(distance,a27,90).
	fact(price, a28, 420).	fact(distance,a28,35).
	fact(price, a29, 480).	fact(distance,a29,60).
	fact(price, a30, 430).	fact(distance,a30,110).
	
	
	
	fact(noise,a1,med).    	fact(noise,a2,med).    fact(noise,a3,high).     fact(noise,a4,med).
	fact(noise,a5,med).    	fact(noise,a6,high).	fact(noise,a7,high).     fact(noise,a8,low).
	fact(noise,a9,high).    fact(noise,a10,med).    fact(noise,a11,low).    fact(noise,a12,high).
	fact(noise,a13,low).    fact(noise,a14,low).    fact(noise,a15,low).    fact(noise,a16,high).
	fact(noise,a17,med).    fact(noise,a18,high).   fact(noise,a19,med).    fact(noise,a20,med).
	fact(noise,a21,low).   fact(noise,a22,high).   fact(noise,a23,med).    fact(noise,a24,low).
	fact(noise,a25,low).    fact(noise,a26,low).    fact(noise,a27,low).    fact(noise,a28,high).
	fact(noise,a29,med).    fact(noise,a30,low).
	
	fact(area,a1,75).     fact(area,a2,85).   	fact(area,a3,30).     fact(area,a4,80).
	fact(area,a5,60).     fact(area,a6,90).		fact(area,a7,75).     fact(area,a8,65).
	fact(area,a9,55).     fact(area,a10,50).  	fact(area,a11,60).    fact(area,a12,70).
	fact(area,a13,70).    fact(area,a14,40).    fact(area,a15,65).    fact(area,a16,30).
	fact(area,a17,50).    fact(area,a18,90).    fact(area,a19,45).    fact(area,a20,50).
	fact(area,a21,30).    fact(area,a22,80).    fact(area,a23,75).    fact(area,a24,30).
	fact(area,a25,60).    fact(area,a26,50).    fact(area,a27,50).    fact(area,a28,60).
	fact(area,a29,50).    fact(area,a30,60).
	
	fact(rooms,a1,2).     fact(rooms,a2,2).     fact(rooms,a3,1).     fact(rooms,a4,3).
	fact(rooms,a5,1).     fact(rooms,a6,3).		fact(rooms,a7,2).     fact(rooms,a8,3).
	fact(rooms,a9,2).     fact(rooms,a10,1).    fact(rooms,a11,2).    fact(rooms,a12,2).
	fact(rooms,a13,2).    fact(rooms,a14,1).    fact(rooms,a15,2).    fact(rooms,a16,1).
	fact(rooms,a17,2).    fact(rooms,a18,3).    fact(rooms,a19,1).    fact(rooms,a20,1).
	fact(rooms,a21,1).    fact(rooms,a22,3).    fact(rooms,a23,2).    fact(rooms,a24,1).
	fact(rooms,a25,2).    fact(rooms,a26,2).    fact(rooms,a27,2).    fact(rooms,a28,1).
	fact(rooms,a29,1).    fact(rooms,a30,2).
    
    feature(price).
    feature(distance).
	feature(noise).
	feature(area).
	feature(rooms).
	
	feature_domain(price, real_numbers).
	feature_domain(distance, real_numbers).
	feature_domain(noise, [low,med,high]).
	feature_domain(area, real_numbers).
	feature_domain(rooms, real_numbers).
	
	% ========================================
    %       Criteria
    % ========================================
    criterion(cost).
    criterion(location).
    criterion(size).
    
    % ========================================
    %       Values
    % ========================================
    values(cost,[vbad,bad,reg,good,vgood]).
    values(location,[vbad,bad,reg,good,vgood]).
    values(size,[vbad,bad,reg,good,vgood]).
    
    % ========================================
    %       Profile - Rules
    % ========================================
    
    profile_rule(p1, location is vgood if noise == low and distance =< 15).
	profile_rule(p2, location is good if noise == low and 15 < distance and distance =< 20).
	profile_rule(p3, location is reg if (noise == med and distance =< 15) or (noise == high and distance =< 10)).
	profile_rule(p4, location is bad if ((noise == high and 10 < distance) or (noise == med and 15 < distance) or (noise == low and 20 < distance)) and distance =< 60).
	profile_rule(p5, location is vbad if 60 < distance).
	profile_rule(p6, cost is vgood if price =< 300).
	profile_rule(p7, cost is good if 300 < price and price =< 400).
	profile_rule(p8, cost is reg if 400 < price and price =< 450).
	profile_rule(p9, cost is bad if 450 < price and price =< 600).
	profile_rule(p10, cost is vbad if 600 < price).
	profile_rule(p11, size is vgood if 100 =< area and 3 =< rooms).
	profile_rule(p12, size is good if (70 =< area and area < 100 and 2 =< rooms) or (100 =< area and rooms < 3)).
	profile_rule(p13, size is reg if (50 =< area and area < 70 and 2 =< rooms) or (70 < area and rooms == 1)).
	profile_rule(p14, size is bad if (35 =< area and area < 50 and rooms =< 3) or (35 =< area and area < 70 and rooms == 1) ).
	profile_rule(p15, size is vbad if area < 35).
    
    
    % ========================================
    %       CP - Rules
    % ========================================
    
    %--Tim's Rules
    
    % R1: 
    cpref_rule(r1, 
        better(X,Y,cost) ==> pref(X,Y)
    ).
    
    % R2: 
    cpref_rule(r2, (
        better(X,Y,location),
        equal(X,Y,cost) ==> pref(X,Y)
    )).
    
    % R3:
    cpref_rule(r3, (
        better(X,Y,size),
        equal(X,Y,location),
        equal(X,Y,cost) ==> pref(X,Y)
    )).
    
    %================================================================
    
    %--August's Exceptions
    
    % R4: 
    cpref_rule(r4, (
        better(X,Y,location), min(X,location,good),
        worse(X,Y,cost) ==> pref(X,Y)
    )).
    
    % R5: 
    cpref_rule(r5, (
        better(X,Y,location), min(X,location,reg),
        worse(X,Y,cost), min(X,cost,bad) ==> pref(X,Y)
    )).
    
    %================================================================
    
    %--Kate's Exceptions
    
    % R6: 
    cpref_rule(r6, (
        better(X,Y,size), min(X,size,reg),
        worse(X,Y,location), min(X,location,bad), max(Y,location,good),
        better(X,Y,cost) ==> pref(X,Y)
    )).
    
    % R7: 
    cpref_rule(r7, (
        equal(X,Y,cost),
        worse(X,Y,location), min(X,location,bad), max(Y,location,good),
        better(X,Y,size), min(X,size,reg) ==> pref(X,Y)
    )).
	
	
	%--Kate's extra rule.
	
	% R8: Impose the minimal requirements to prefer the size over the location.
    cpref_rule(r8, (
        better(X,Y,cost), min(X,cost,good),
        better(X,Y,size), min(X,size,reg) ==> pref(X,Y)
    )).
    
    */