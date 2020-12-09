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

pickOneActionPerActuator([],_,_,_,[]).
pickOneActionPerActuator([Actuator|ActuatorList], Actions, Min, Max, [(Actuator,V2)|NewActions]) :-
    member((Actuator,V), Actions), 
    applyGreaterThanCap(V,Max,V1),
    applySmallerThanCap(V1,Min,V2),
    pickOneActionPerActuator(ActuatorList, Actions, Min, Max, NewActions).

getAvg([],_,_,[]).
getAvg([P|Partitions], Min, Max, [(K,V2)|Ls]) :-
    member((K,_),P),
    findall(V, member((K,V),P), VLs),
    avg(VLs, Avg),
    applyGreaterThanCap(Avg,Max,V1),
    applySmallerThanCap(V1,Min,V2),
    getAvg(Partitions, Min, Max, Ls).

avg( List, Avg ):-
    sumlist( List, Sum ),
    length( List, Length),
    (  Length > 0
    -> Avg is Sum / Length
    ;  Avg is 0
    ).

getMode([],_,_,[]).
getMode([P|Partitions], Min, Max, [(K,V2)|Ls]) :-
    member((K,_),P),
    findall(V, member((K,V),P), VLs),
    mode(VLs, Mode),
    applyGreaterThanCap(Mode,Max,V1),
    applySmallerThanCap(V1,Min,V2),
    getMode(Partitions, Min, Max, Ls).

count_of([],         _,  0).
count_of([H|Rest], E, N1) :- 
  equality_reified(H, E, Bool),
  count_of(Rest, E, N0),
  (Bool == true -> N1 is N0 + 1 ; N1 = N0).

frequency(L, I, N) :-
  sort(L, LSorted),
  member(I, LSorted),
  count_of(L, I, N).

mode(L, X) :-
  frequency(L, X, NMax),
  \+ (frequency(L, _, NBigger),
      NMax < NBigger).

equality_reified(X, X, true).
equality_reified(X, Y, false) :-
   dif(X, Y).

applyGreaterThanCap(Value, Cap, Cap) :-
    Value > Cap.
applyGreaterThanCap(Value, Cap, Value) :-
    Value =< Cap.

applySmallerThanCap(Value, Cap, Cap) :-
    Value < Cap.
applySmallerThanCap(Value, Cap, Value) :-
    Value >= Cap.
    