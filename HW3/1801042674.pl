%--------------------------PART1------------------------------
% Knowledge Base
% Room Information
:- style_check(-singleton).
roomid(z23, 1). 
roomid(z10, 2).
roomid(z6, 3).  

roomcapacity(z23,100).
roomcapacity(z10,75).
roomcapacity(z6,75).

roomwhen(z23,8).
roomwhen(z10,12).
roomwhen(z6,2).

roomitem(z23,smartboard).
roomitem(z23,handicapped).
roomitem(z23,projector).
roomitem(z10,smartboard).
roomitem(z10,projector).
roomitem(z6,smartboard).

roomoccupancyinformation(z23,cse222,8).
roomoccupancyinformation(z10,cse341,12).
roomoccupancyinformation(z6,cse331,2).
roomoccupancyinformation(z6,cse351,2).
% Course Information

courseid(cse341,4).
courseid(cse331,5).
courseid(cse222,6).
courseid(cse351,13).

courseinstructor(cse341,yakupgenc).
courseinstructor(cse331,alparslan).
courseinstructor(cse222,zekeriya).
courseinstructor(cse351,yakupgenc).

coursecapacity(cse341,75).
coursecapacity(cse331,75).
coursecapacity(cse222,75).
coursecapacity(cse351,75).

courseitem(cse351,projector).
courseitem(cse351,smartboard).
courseitem(cse222,smartboard).
courseitem(cse341,projector).
courseitem(cse331,smartboard).

coursetime(cse341,12).
coursetime(cse331,2).
coursetime(cse222,8).
coursetime(cse351,2).

% Instructor Information
instructorid(yakupgenc,7).
instructorid(alparslan,8).
instructorid(zekeriya,9).

instructoritem(yakupgenc,projector).
instructoritem(alparslan,smartboard).
instructoritem(zekeriya,projector).

instructorcourse(yakupgenc,cse341).
instructorcourse(alparslan,cse331).
instructorcourse(zekeriya,cse222).
instructorcourse(yakupgenc,cse351).
% Student Information

student(ahmet,10,nothandicapped,[cse222,cse341,cse351]).
student(mehmet,11,nothandicapped,[cse222,cse331,cse341]).
student(bill,12,handicapped,[cse341]).



% rules


schedule_conflict():-
    courseid(X,Y),
    courseid(Z,W),
    X \== Z,
    roomoccupancyinformation(K,X,L),

    roomoccupancyinformation(M,Z,N),
    K == M,
    L == N,
    format("There is conflict between : "),
    format("X:~w Y:~w Z:~w W:~w", [X, Y, Z, W]).   

checkwhichroomtogivenclass(X):-
    courseinstructor(X,Y),
    coursecapacity(X,Z),
    courseitem(X,K),
    coursetime(X,L),

    
    roomcapacity(M,N), % m =room
    roomwhen(A,B), % a = room
    roomitem(U,W), % u= room
    
    instructoritem(Y,T),
    roomitem(Q,T),

    Z =< N,
    B == L,
    W == K,

    A==U,
    A==M,
    U==M,
    Q==A,
    Q==M,
    Q==U,
    format("The room is : "),
    format("M:~w A:~w U:~w Q:~w", [M,A,U,Q]).

checkwhichroomtowhichclass():-
    checkwhichroomtogivenclass(X),
    format(" X:~w ", [X]).

%there is some missings it writes more than 3 or 4 but result is true .
enrollstudenttogivenclass(X):-
    courseinstructor(X,Y),
    coursecapacity(X,Z),
    courseitem(X,K),
    coursetime(X,L),

    
    roomcapacity(M,N), % m =room
    roomwhen(A,B), % a = room
    roomitem(U,W), % u= room
    
    instructoritem(Y,T),
    roomitem(Q,T),

    student(R,P,I,S),
    student(AA,BB,nothandicapped,DD),

    Z =< N,
    B == L,
    W == K,

    A==U,
    A==M,
    U==M,
    Q==A,
    Q==M,
    Q==U,

    roomitem(Q,V),

    
    %( condition -> then_clause ; else_clause )

    (V \== I -> format("Student:~w", [AA]);format("Student:~w", [R])).
    
%there is some missings it writes more than 3 or 4 but result is true .
checkwhichstudenttowhichclass():-
    enrollstudenttogivenclass(X),
    format(" Lecture:~w ", [X]).

%-------------------------PART2-------------------------------
% knowledge base
flight(istanbul,izmir,2).
flight(istanbul,rize,4).
flight(istanbul,ankara,1).
flight(ankara,izmir,6).
flight(ankara,rize,5).
flight(ankara,diyarbakir,8).
flight(ankara,van,4).
flight(van,gaziantep,3).
flight(izmir,antalya,2).
flight(antalya,diyarbakir,4).
flight(antalya,erzincan,3).
flight(canakkale,erzincan,6).


% rules
direct_route(X, Y, C):-
    flight(X, Y, C).

direct_route(X, Y, C):-
    flight(Y, X, C).

route(X , Y , C) :-  
    cost(X , Y , C , []) ; direct_route(X,Y,C).
 
cost(X , Y , C , _) :-  
    direct_route(X , Y , C),
    format("X:~w Y:~w C:~w", [X, Y, C]).   

cost(X , Y , C , M) :-  
    \+ member(X , M)  , direct_route(X , Z , L) , 
    cost(Z , Y , E , [X | M]) , X \= Y,  C is L + E,
    format("X:~w Y:~w Z:~w K:~w M:~w L:~w \n", [X, Y, Z, C, M, L]).