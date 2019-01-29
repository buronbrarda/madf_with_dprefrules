:-dynamic alternative/1, crit/1, values/2, grule/2.
    
    
    
    %=================================================================================
    
    %Evidence
    
    price(a1, 615).    	price(a2, 360).     price(a3, 250).     price(a4, 370).
	price(a5, 430).    	price(a6, 375).		price(a7, 390).     price(a8, 600).
	price(a9, 320).    	price(a10, 630).    price(a11, 700).    price(a12, 335).
	price(a13, 485).    price(a14, 260).    price(a15, 490).    price(a16, 290).
	price(a17, 250).    price(a18, 570).    price(a19, 495).    price(a20, 690).
	price(a21, 280).    price(a22, 300).    price(a23, 500).    price(a24, 665).
	price(a25, 565).    price(a26, 360).    price(a27, 475).    price(a28, 420).
	price(a29, 480).    price(a30, 430).
	
	dist(a1, 135).     dist(a2, 55).     dist(a3, 80).     dist(a4, 15).
	dist(a5, 5).      dist(a6, 90).		dist(a7, 10).     dist(a8, 90).
	dist(a9, 10).     dist(a10, 140).   dist(a11, 80).    dist(a12, 45).
	dist(a13, 100).   dist(a14, 45).    dist(a15, 490).   dist(a16, 15).
	dist(a17, 180).   dist(a18, 40).    dist(a19, 160).   dist(a20, 130).
	dist(a21, 50).    dist(a22, 150).   dist(a23, 120).   dist(a24, 150).
	dist(a25, 70).    dist(a26, 150).   dist(a27, 90).    dist(a28, 35).
	dist(a29, 60).    dist(a30, 110).
	
	noise(a1, med).    	noise(a2, med).    noise(a3, high).     noise(a4, med).
	noise(a5, med).    	noise(a6, high).	noise(a7, high).     noise(a8, low).
	noise(a9, high).    noise(a10, med).    noise(a11, low).    noise(a12, high).
	noise(a13, low).    noise(a14, low).    noise(a15, low).    noise(a16, high).
	noise(a17, med).    noise(a18, high).   noise(a19, med).    noise(a20, med).
	noise(a21, low).   noise(a22, high).   noise(a23, med).    noise(a24, low).
	noise(a25, low).    noise(a26, low).    noise(a27, low).    noise(a28, high).
	noise(a29, med).    noise(a30, low).
	
	area(a1, 75).     area(a2, 85).   	area(a3, 30).     area(a4, 80).
	area(a5, 60).     area(a6, 90).		area(a7, 75).     area(a8, 65).
	area(a9, 55).     area(a10, 50).  	area(a11, 60).    area(a12, 70).
	area(a13, 70).    area(a14, 40).    area(a15, 65).    area(a16, 30).
	area(a17, 50).    area(a18, 90).    area(a19, 45).    area(a20, 50).
	area(a21, 30).    area(a22, 80).    area(a23, 75).    area(a24, 30).
	area(a25, 60).    area(a26, 50).    area(a27, 50).    area(a28, 60).
	area(a29, 50).    area(a30, 60).
	
	rooms(a1, 2).     rooms(a2, 2).     rooms(a3, 1).     rooms(a4, 3).
	rooms(a5, 1).     rooms(a6, 3).		rooms(a7, 2).     rooms(a8, 3).
	rooms(a9, 2).     rooms(a10, 1).    rooms(a11, 2).    rooms(a12, 2).
	rooms(a13, 2).    rooms(a14, 1).    rooms(a15, 2).    rooms(a16, 1).
	rooms(a17, 2).    rooms(a18, 3).    rooms(a19, 1).    rooms(a20, 1).
	rooms(a21, 1).    rooms(a22, 3).    rooms(a23, 2).    rooms(a24, 1).
	rooms(a25, 2).    rooms(a26, 2).    rooms(a27, 2).    rooms(a28, 1).
	rooms(a29, 1).    rooms(a30, 2).
    
    %=================================================================================
    
    
    %Profile Rules
    
    % range(x,y) --> [X,Y]
    
    %cost is V if R1 <= price <= R2
    profile_rule(cost, vbad, [price(range(601,null))]).
    profile_rule(cost, bad, [price(range(450,600))]).
    profile_rule(cost, reg, [price(range(401,451))]).
    profile_rule(cost, good, [price(range(301,400))]).
    profile_rule(cost, vgood, [price(range(0,300))]).
    
    %=================================================================================
    
    profile_rule(location, vbad, [dist(range(61,null))]).
    
	profile_rule(location, bad, [dist(range(21,60))]).
    profile_rule(location, bad, [dist(range(11,20)), noise(set([high]))]).
	profile_rule(location, bad, [dist(range(16,20)), noise(set([med]))]).
	
    %location is reg if (dist <= 15 && noise = med) || (dist <= 10 && noise = high)
    profile_rule(location, reg, [dist(range(null,15)), noise(set([med]))]).
    profile_rule(location, reg, [dist(range(null,10)), noise(set([high]))]).
    
    profile_rule(location, good, [dist(range(16,20)), noise(set([low]))]).
    profile_rule(location, vgood, [dist(range(null,15)), noise(set([low]))]).
    
    %=================================================================================
    
    
    profile_rule(size, vbad, [ area(range(null,34)) ]).
    
	profile_rule(size, bad, [ area(range(35,49)), rooms(range(1,2)) ]).
    profile_rule(size, bad, [ area(range(50,69)), rooms(set([1])) ]).
	
	profile_rule(size, reg, [ area(range(50,69)), rooms(range(2,3)) ]).
	profile_rule(size, reg, [ area(range(70,99)), rooms(set([1])) ]).
	profile_rule(size, reg, [ area(range(100,null)), rooms(set([1])) ]).
	
    profile_rule(size, good, [ area(range(70,99)), rooms(range(2,3)) ]).
	
    profile_rule(size, vgood, [ area(range(100,null)), rooms(range(3,null)) ]).
    
        
    % ========================================
    %       Alternatives
    % ========================================
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
    
    
    % ========================================
    %       Criteria
    % ========================================
    crit(cost).
    crit(location).
    crit(size).
    
    :-forall(crit(C), ((dynamic C/2), export(C/2)) ).
    
    % ========================================
    %       Values
    % ========================================
    values(cost,[vbad,bad,reg,good,vgood]).
    values(location,[vbad,bad,reg,good,vgood]).
    values(size,[vbad,bad,reg,good,vgood]).
    
    
    % ========================================
    %       CP - Rules
    % ========================================
    
    %--Tim's Rules
    
    % R1: 
    grule(r1, 
        better(X,Y,cost) ==> preferred(X,Y)
    ).
    
    % R2: 
    grule(r2, (
        better(X,Y,location),
        equal(X,Y,cost) ==> preferred(X,Y)
    )).
    
    % R3:
    grule(r3, (
        better(X,Y,size),
        equal(X,Y,location),
        equal(X,Y,cost) ==> preferred(X,Y)
    )).
    
    %================================================================
    
    %--August's Exceptions
    
    % R4: 
    grule(r4, (
        better(X,Y,location), min(X,location,good),
        worse(X,Y,cost) ==> preferred(X,Y)
    )).
    
    % R5: 
    grule(r5, (
        better(X,Y,location), min(X,location,reg),
        worse(X,Y,cost), min(X,cost,bad) ==> preferred(X,Y)
    )).
    
    %================================================================
    
    %--Kate's Exceptions
    
    % R6: 
    grule(r6, (
        better(X,Y,size), min(X,size,reg),
        worse(X,Y,location), min(X,location,bad), max(Y,location,good),
        better(X,Y,cost) ==> preferred(X,Y)
    )).
    
    % R7: 
    grule(r7, (
        equal(X,Y,cost),
        worse(X,Y,location), min(X,location,bad), max(Y,location,good),
        better(X,Y,size), min(X,size,reg) ==> preferred(X,Y)
    )).
	
	
	%--August's extra rule.
	
	% R8: Impose the minimal requirements to prefer the size over the location.
    grule(r8, (
        better(X,Y,cost), min(X,cost,good),
        better(X,Y,size), min(X,size,reg) ==> preferred(X,Y)
    )).