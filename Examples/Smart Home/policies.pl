:- consult('infr').
:- consult('../../reasoner.pl').

% mediateRequests/2  for each pair (Zone, PropertyInstance) obtains users' requests, 
% averages them and then applies policies based on the propertyInstance and various environmental parameters.

mediateRequests(Requests, Mediated) :-
    groupPerPI(Requests, NewRequests),
    writeln(NewRequests),
    mediateRequest(NewRequests, Mediated).

groupPerPI(Requests, Groups) :- 
    getZonePIPairs(Pairs), 
    getRequestPerPI(Requests, Pairs, Groups).

mediateRequest([],[]).
mediateRequest([(Z,PI,Ls)|Reqs], [Mediated|OtherMediatedReqs]) :-
    mediatePI(Z,PI,Ls,Mediated),
    mediateRequest(Reqs, OtherMediatedReqs).

mediatePI(Z, PI, Ls, (Z, PI, Avg)) :-
    findall((WeightedV,W), (member((V,_,Roles), Ls), once(getUserWeight(Roles, W)), WeightedV is V*W), Zip),
    zip(Values, Weights, Zip), sum_list(Values, Sum), sum_list(Weights, SumW), Avg is Sum/SumW. %weighted average

getUserWeight(Roles, 2) :-
    member(owner, Roles).
getUserWeight(_, 1).


% associateActions/2 given a set of (mediated) requests, returns a list of actions of the type (ActuatorId, Value). 
% To do this, for each mediated request, 
% assigns the action to be performed to each actuator of the propertyInstance. 
% Once all requests have been "rolled out" it checks if an actuator has more than one action assigned 
% and if so, mediate between those actions.
associateActions(Requests, ExecutableActions) :-
    actionsFor(Requests, Actions),             % returns a list of actions (ActuatorId, Value)
    setActuators(Actions, ExecutableActions).  % [Defined by Admin] determines a single action for each actuator

% actionsFor/2 for each mediated request takes the set of actuators and sensors assigned to the propertyInstance and 
% for each of them assigns the corresponding action.
actionsFor([],[]).
actionsFor([(Z, PI, V)|Reqs], Actions) :-
    propertyInstance(Z, PI, _, ActuatorList, SensorList),              % given a Zone and its PropertyInstace takes the set of actuators and sensors
    selectActionsForPI(Z, PI, V, ActuatorList, SensorList, Actions1),  % [Defined by Admin] for each actuator in the PropertyInstance assigns an action (ActuatorId, Value)    
    actionsFor(Reqs, Actions2), append(Actions1, Actions2, Actions).   % combines these actions with the others

% determineActions defined by SysAdmin (e.g. can use sensor values taken from the 4th param) %%%%%%%%%%%%%%%%%%%%%%%%%
selectActionsForPI(_, _, V, Actuators, _, Actions):-
    length(Actuators, L),
    triggerAll(V, L, Actuators, Actions).

triggerAll(_, _, [], []).
triggerAll(V, L, [A|Actuators], [(A,VNew)|Actions]):-
    VNew is V/L,
    triggerAll(V, L, Actuators, Actions).

% resolveActions defined by SysAdmin, cannot read data from sensors (?) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (will mainly do avg, max, min, mode, cap) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setActuators(Actions, ExecutableActions) :- setActuatorsWithMax(Actions, -inf, inf, ExecutableActions).


optimiseEnergy(Actions, Optimised) :-
    writeln(helo),
    findall((A,V), distinct((member((A,V), Actions), actuator(A, light))), Light),
    findall((A,V), distinct((member((A,V), Actions), actuator(A, temp))), Temp),
    energyConsumption(Light, Cons),
    maxLightConsumption(Max),
    writeln((Light, Cons, Max)),
    findBestSubset(Light, Cons, Max, TmpOptimised),
    append(TmpOptimised, Temp, Optimised).
optimiseEnergy([], []).

findBestSubset(Actions, Cons, Max, Optimised) :-
    Cons > Max, findall((L,Sub), (subSet(Actions, Sub), energyConsumption(Sub, C), C =< Max, length(Sub,L), L > 0), AllActions),
    sort(AllActions, [(_,Optimised)|_]).
findBestSubset(Actions, Cons, Max, [(mainLight,V)]) :-
    Cons > Max, findall(1, (subSet(Actions, Sub), energyConsumption(Sub, C), C =< Max, length(Sub,L), L > 0), []),
    member((mainLight,V), Actions).
findBestSubset(Actions, Cons, Max, Actions) :-
    Cons =< Max.
findBestSubset([], _, _, []).

energyConsumption([], 0).
energyConsumption([(A,V)|Actuators], NewCons) :-
    energyConsumption(Actuators, Cons),
    actuatorConsumption(A, E),
    NewCons is Cons + (V*E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


getZonePIPairs(Pairs) :- findall((ZId, PIId), (zone(ZId), propertyInstance(ZId,PIId,_,_,_)), Ls), sort(Ls,Pairs).

filterRequests(ZId, PIId, Requests, Ls) :- findall((Value, UId, URoles), distinct((member((ZId, PIId, Value, UId), Requests), roles(UId, URoles))), Ls).

getRequestPerPI(Requests, Pairs, Groups) :-
    findall((ZId,PIId,Ls),  distinct((member((ZId,PIId),Pairs), filterRequests(ZId, PIId, Requests, Ls), length(Ls, Len), Len > 0)), Groups).


filterRequestsByRoles(Requests, Roles, Values) :-
    findall((V,U,RLs), distinct((member((V,U,RLs), Requests), intersection(RLs, Roles, [_|_]))), Values).

getRequestsValues(Requests, Values) :-
    findall(V, (member((V,_,_), Requests)), Values).

maxAction([], _).
maxAction([(A,Value)|Considered], Max) :-
    maxAction(Considered, (A,Value), Max).

maxAction([], Max, Max).
maxAction([(A,Value1)|Considered], (_,_,Value2), Max) :-
    Value1 >= Value2,
    maxAction(Considered, (A,Value1), Max).
maxAction([(_,_,Value1)|Considered], (A,Value2), Max) :-
    Value1 < Value2,
    maxAction(Considered, (A,Value2), Max).

minRequest([], _).
minRequest([(Z,PI,Value)|Considered], Min) :-
    minRequest(Considered, (Z,PI,Value), Min).

minRequest([], Min, Min).
minRequest([(Z1,PI1,Value1)|Considered], (_,_,Value2), Min) :-
    Value1 =< Value2,
    minRequest(Considered, (Z1,PI1,Value1), Min).
minRequest([(_,_,Value1)|Considered], (Z2,PI2,Value2), Min) :-
    Value1 > Value2,
    minRequest(Considered, (Z2,PI2,Value2), Min).

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
        NMax < NBigger), !.

frequency(L, I, N) :-
    sort(L, LSorted),
    member(I, LSorted),
    count_of(L, I, N).

count_of([],         _,  0).
count_of([H|Rest], E, N1) :- 
  equality_reified(H, E, Bool),
  count_of(Rest, E, N0),
  (Bool == true -> N1 is N0 + 1 ; N1 = N0).

equality_reified(X, X, true).
equality_reified(X, Y, false) :-
   dif(X, Y).

take(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :- M is N-1, take(M, Xs, Ys).

zip([], [], []).
zip([X|Xs], [Y|Ys], [(X,Y)|Zs]) :- zip(Xs,Ys,Zs).

subSet([], []).
subSet([E|Tail], [E|NTail]):-
  subSet(Tail, NTail).
subSet([_|Tail], NTail):-
  subSet(Tail, NTail).

unordered_subset(Set, SubSet):-
    length(Set, LSet),
    between(0,LSet, LSubSet),
    length(NSubSet, LSubSet),
    permutation(SubSet, NSubSet),
    subSet(Set, NSubSet).

setActuatorsWithMax(Actions, Min, Max, ExecutableActions) :-
    findall(Actuator, member((Actuator,_), Actions), L),
    sort(L,SortedL),
    sort(0, @>, Actions, SortedActions), % sorts actions in descending order and removes duplicates. See: http://eu.swi-prolog.org/pldoc/doc_for?object=sort/4
    pickOneActionPerActuator(SortedL, SortedActions, Min, Max, ExecutableActions).

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