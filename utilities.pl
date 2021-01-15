getZonePIPairs(Pairs) :- findall((ZId, PIId), (zone(ZId,_), propertyInstance(ZId,PIId,_,_,_)), Ls), sort(Ls,Pairs).

getActuators(Actuators) :- findall((A), actuator(A,_), Ls), sort(Ls, Actuators).

filterRequests(ZId, PIId, Requests, Ls) :- findall((Value, UId), member((ZId, PIId, Value, UId), Requests), Ls).

filterActions(A, Actions, Ls) :- findall((Value), member((A,Value), Actions), Ls).

groupPerPI(Requests, SortedGroups) :- 
    getZonePIPairs(Pairs), 
    findall((ZId,PIId,Ls) ,( member((ZId,PIId),Pairs), filterRequests(ZId, PIId, Requests, Ls) ),Groups), 
    sort(Groups,SortedGroups).

groupPerActuator(Actions, SortedGroups) :- 
    getActuators(Actuators), 
    findall((A,Ls) ,( member(A, Actuators), filterActions(A, Actions, Ls) ),Groups), 
    sort(Groups,SortedGroups).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% setActuatorWith[Max/Min/Avg/Mode](Actions, Min, Max, ExecutableActions)
%   - Actions: list of (Actuator, Value)
%   - Min: minimum allowed value
%   - Max: maximum allowed value
%   - ExecutableActions: list of (Actuator, Value)

setActuatorsWithMax(Actions, Min, Max, ExecutableActions) :-
    findall(Actuator, member((Actuator,_), Actions), L),
    sort(L,SortedL),
    sort(0, @>, Actions, SortedActions), % sorts actions in descending order and removes duplicates. See: http://eu.swi-prolog.org/pldoc/doc_for?object=sort/4
    pickOneActionPerActuator(SortedL, SortedActions, Min, Max, ExecutableActions).

setActuatorsWithMin(Actions, Min, Max, ExecutableActions) :-
    findall(Actuator, member((Actuator,_), Actions), L),
    sort(L,SortedL),
    sort(0, @<, Actions, SortedActions), % sorts actions in ascending order and removes duplicates. See: http://eu.swi-prolog.org/pldoc/doc_for?object=sort/4
    pickOneActionPerActuator(SortedL, SortedActions, Min, Max, ExecutableActions).

setActuatorsWithAvg(Actions, Min, Max, ExecutableActions) :-
    group(Actions,Partitions),
    getAvg(Partitions, Min, Max,  ExecutableActions).

setActuatorsWithMode(Actions, Min, Max, ExecutableActions) :-
    group(Actions,Partitions),
    getMode(Partitions, Min, Max, ExecutableActions).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getAvg([],_,_,[]).
getAvg([P|Partitions], Min, Max, [(K,V2)|Ls]) :-
    member((K,_),P),
    findall(V, member((K,V),P), VLs),
    avg(VLs, Avg),
    applyGreaterThanCap(Avg,Max,V1),
    applySmallerThanCap(V1,Min,V2),
    getAvg(Partitions, Min, Max, Ls).

getMode([],_,_,[]).
getMode([P|Partitions], Min, Max, [(K,V2)|Ls]) :-
    member((K,_),P),
    findall(V, member((K,V),P), VLs),
    mode(VLs, Mode),
    applyGreaterThanCap(Mode,Max,V1),
    applySmallerThanCap(V1,Min,V2),
    getMode(Partitions, Min, Max, Ls).

applyGreaterThanCap(Value, Cap, Cap) :-
    Value > Cap.
applyGreaterThanCap(Value, Cap, Value) :-
    Value =< Cap.

applySmallerThanCap(Value, Cap, Cap) :-
    Value < Cap.
applySmallerThanCap(Value, Cap, Value) :-
    Value >= Cap.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pickOneActionPerActuator([],_,_,_,[]).
pickOneActionPerActuator([Actuator|ActuatorList], Actions, Min, Max, [(Actuator,V2)|NewActions]) :-
    member((Actuator,V), Actions), 
    applyGreaterThanCap(V,Max,V1),
    applySmallerThanCap(V1,Min,V2),
    pickOneActionPerActuator(ActuatorList, Actions, Min, Max, NewActions).

avg( List, Avg ):-
    sumlist( List, Sum ),
    length( List, Length),
    (  Length > 0
    -> Avg is Sum / Length
    ;  Avg is 0
    ).

mode(L, X) :-
    frequency(L, X, NMax),
    \+ (frequency(L, _, NBigger),
        NMax < NBigger).

frequency(L, I, N) :-
    sort(L, LSorted),
    member(I, LSorted),
    count_of(L, I, N).

count_of([],         _,  0).
count_of([H|Rest], E, N1) :- 
  equality_reified(H, E, Bool),
  count_of(Rest, E, N0),
  (Bool == true -> N1 is N0 + 1 ; N1 = N0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

group(Ls, Partitions) :-
    msort(Ls,SLs),
	group(SLs, [], [], Partitions).
group([],CLs,Acc,[CLs|Acc]).
group([(K,V)|Xs], [], Acc, Ls) :-
    group(Xs, [(K,V)], Acc, Ls).
group([(K,V)|Xs], [(K,V1)|CLs], Acc, Ls) :-
    group(Xs, [(K,V),(K,V1)|CLs], Acc, Ls).
group([(K,V)|Xs], [(K1,V1)|CLs], Acc, Ls) :-
    dif(K,K1),
    group(Xs, [(K,V)], [[(K1,V1)|CLs]|Acc], Ls).

equality_reified(X, X, true).
equality_reified(X, Y, false) :-
   dif(X, Y).