
%:- set_prolog_stack(global, limit(100 000 000 000)).
:- use_module(library(statistics)).

% sim(N, M, A, O, Tot) :-
%     simulate(N, Ms, As, Os, Tots),
%     average(Ms, M),
%     average(As, A),
%     average(Os, O),
%     average(Tots, Tot),
%     write('Mediate: '),  writeln(M),
%     write('Associate: '),writeln(A),
%     write('Optimise: '), writeln(O),
%     write('Tot: '), writeln(Tots).

% average( List, Average ):- 
%     sum_list( List, Sum ),
%     length( List, Length ),
%     Length > 0, 
%     Average is Sum / Length.

% simulate(0, [], [], []).
% simulate(N, [M|Mediate], [A|Associate], [O|Optimise]) :-
%     time_react(M, A, O, _),
%     NewN is N-1,
%     simulate(NewN, Mediate, Associate, Optimise).

time_react(Mediate, Associate, Optimise, Tot) :- 
    statistics(cputime, Start),
    statistics(cputime, BeforeT1),
        getRequests(_, ValidRequests),               % request harvesting (into a list of <A,Z,Pi,V>), discarding invalid requests
        mediateRequests(ValidRequests, MediatedRequests),   % [Defined by Admin] determines a list of <Z,Pi,V> such that there is at most one request per Pi
        validMediation(MediatedRequests),
    statistics(cputime, AfterT1),
    Mediate is AfterT1 - BeforeT1,

    statistics(cputime, BeforeT2),
        associateActions(MediatedRequests, TmpActions),     % [Defined by Admin] determines a list of <Actuator,Value> such that there is ...
    statistics(cputime, AfterT2),
    Associate is AfterT2 - BeforeT2,

    statistics(cputime, BeforeT3),
        optimiseEnergy(TmpActions, Actions),                % [Defined by Admin] determines a list of <Actuator,Value> such that the energy consumption is minimised                % [Defined by Admin] optimises the energy consumption of the actions
        validActions(Actions),
    statistics(cputime, AfterT3),
    Optimise is AfterT3 - BeforeT3,
    statistics(cputime, End),
    Tot is End - Start.

getRequests(Requests, ValidRequestsSorted) :-
    findall((ZId, PIId, Value, UId), set(UId, ZId, PIId, Value), Requests),
    findall((ZId, PIId, Value, UId), ( member((ZId, PIId, Value, UId), Requests), user(UId, Zones), member(Z ,Zones), in(ZId, Z), validRequest(ZId, PIId, Value) ), ValidRequests), %%% TO CHECK!!!
    sort(ValidRequests, ValidRequestsSorted).

validMediation(Reqs) :-
    sort(Reqs, OrderedReqs),
    \+( ( member((Z,PI,V1), OrderedReqs), member((Z,PI,V2), OrderedReqs), dif(V1,V2) ) ),
    \+( ( member((Z,PI,V), OrderedReqs), \+( validRequest(Z,PI,V) ) ) ).

validActions(Actions) :-
    sort(Actions, OrderedActions),
    \+( ( member((A,V1), OrderedActions), member((A,V2), OrderedActions), dif(V1,V2) ) ),
    \+( ( member((A,V), OrderedActions), \+( validValue(A,V) ) ) ).

validRequest(_,_,_). % [Defined by Admin]
validValue(_,_).     % [Defined by Admin]

in(Z1, Z1).
in(Z1, Z2) :- subzone(Z1, Z2).
in(Z1, Z2) :- subzone(Z1, Z3), in(Z3, Z2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mediateRequests(Requests, Mediated) :- 
    groupPerPI(Requests, NewRequests),
    mediateRequest(NewRequests, Mediated).

groupPerPI(Requests, Groups) :- 
    getZonePIPairs(Pairs), 
    getRequestPerPI(Requests, Pairs, Groups).

mediateRequest([Req|Reqs], [Mediated|OtherMediatedReqs]) :- %Ls is [(V,U,Roles)...]
    Req = (Z,PI,_), zone(Z), % get the zone policy
    propertyInstance(Z, PI, Prop, _, [Sensor]),
    sensorValue(Sensor, SensedValue),
    preprocessRequests(Prop, Req, NewReq),
    mediatePI(NewReq,Prop,SensedValue,TmpMediated),
    adjustMediated(Prop, TmpMediated, Mediated),
    mediateRequest(Reqs, OtherMediatedReqs).
mediateRequest([],[]).

preprocessRequests(_, (Z,PI,[]), (Z,PI,[])).
preprocessRequests(Prop, (Z,PI,[R|Ls]), (Z,PI,[NewR|NewLs])) :-
    preprocessRequest(Prop, R, NewR),
    preprocessRequests(Prop, (Z,PI,Ls), (Z,PI,NewLs)).

preprocessRequest(temp, R, R).
preprocessRequest(light, (high,U,Roles), (100,U,Roles)).
preprocessRequest(light, (medium,U,Roles), (50,U,Roles)).
preprocessRequest(light, (low,U,Roles), (10,U,Roles)).

adjustMediated(temp, (Z,PI,Value), (Z,PI,NewValue)) :-
    minimumAllowedTemperature(Min), maximumAllowedTemperature(Max),
    ((Value >= Min, Value =< Max, NewValue is Value);
    (Value < Min, NewValue is Min); 
    (Value > Max, NewValue is Max)).
adjustMediated(light, (Z,PI,Value), (Z,PI,NewValue)) :-
    ((Value >= 0, NewValue is Value);
    (Value < 0, NewValue is 0)).

mediatePI((Z,PI,Ls),temp,_,(Z,PI,Mode)) :- %not public
    \+ in(Z, public_campus),
    findall(V, (member((V,_,_), Ls)), Values),
    mode(Values, Mode). % consensus policy, the option with more "votes" win
mediatePI((Z,PI,Ls),temp,_,(Z,PI,Mediated)) :- % public
    in(Z, public_campus),
    directorsPolicy(Ls, Mediated). % director's policy
mediatePI((Z,PI,Ls),light,_,(Z,PI,Avg)) :- % weighted average of the values
    findall((WeightedV,W), (member((V,_,Roles), Ls), once(getUserWeight(light, Roles, W)), WeightedV is V*W), Zip),
    zip(Values, Weights, Zip), sum_list(Values, Sum), sum_list(Weights, SumW), Avg is Sum/SumW.

getUserWeight(light, Roles, 3) :-
    member(rector, Roles).
getUserWeight(light, Roles, 2.5) :-
    member(head_dept, Roles).
getUserWeight(light, Roles, 2) :-
    member(professor, Roles).
getUserWeight(light, Roles, 2) :-
    member(secretary, Roles).
getUserWeight(light, Roles, 2) :-
    member(reasearcher, Roles).
getUserWeight(light, _, 1).

directorsPolicy(Requests, Mode):-
    filterRequestsByRoles(Requests, [rector, head_dept], [R|Rs]),
    getRequestsValues([R|Rs], Values),
    mode(Values, Mode).
directorsPolicy(Requests, Mode):-
    filterRequestsByRoles(Requests, [rector, head_dept], []),
    getRequestsValues(Requests, Values),
    mode(Values, Mode).

associateActions(Requests, Actions) :-
    findall((A,V), (member((Z,PI,V),Requests), propertyInstance(Z, PI, temp, [A], _)), TmpTempActions),
    findall((A, Avg), (group_by(A, V, member((A,V), TmpTempActions), Grouped), avg(Grouped, Avg)), TempActions),
    findall((Z,PI,V), (member((Z,PI,V), Requests), propertyInstance(Z, PI, light, _, _)), LightRequests),
    bestLightCombinationPerPI(LightRequests, LightActions),
    append(TempActions, LightActions, Actions).
associateActions([], []).

bestLightCombinationPerPI([(Z,PI,V)|Requests], Actions) :-
    propertyInstance(Z, PI, light, Actuators, _),
    findBestLightCombination(Actuators, V, TmpActions),
    bestLightCombinationPerPI(Requests, NewActions),
    append(TmpActions, NewActions, Actions).
bestLightCombinationPerPI([(Z,PI,V)|Requests], Actions) :-
    propertyInstance(Z, PI, light, Actuators, _),
    \+ findBestLightCombination(Actuators, V, _),
    findall((A,100), (member(A, Actuators)), TmpActions),
    bestLightCombinationPerPI(Requests, NewActions),
    append(TmpActions, NewActions, Actions).
bestLightCombinationPerPI([], []).

findBestLightCombination(Actuators, V, Actions) :-
    findall((E,A), (member(A, Actuators), actuatorConsumption(A, E)), EAs),
    sort(EAs, SortedEAs), findall(A, member((_,A), SortedEAs), As),
    findall((E,Actions), (subSet(As, Sub), evalSubset(Sub, V, Actions, C), C >= V, energyConsumption(Actions, E)), AllActions),
    %findall((E,Actions), (unordered_subset(Actuators, Sub), evalSubset(Sub, V, Actions, C), C >= V, energyConsumption(Actions, E)), AllActions),
    sort(AllActions, [(E,Actions)|_]).
    
evalSubset([A|As], V, [(A,100)|Actions], NewC) :-
    V > 0, actuatorCapacity(A, L), actuatorType(A, onOff),
    NewV is V-L, evalSubset(As, NewV, Actions, C), NewC is C+L.
evalSubset([A|As], V, [(A,NewL)|Actions], NewC) :-
    V > 0, actuatorCapacity(A, L), actuatorType(A, dimm), 
    (L =< V -> NewL is 100; NewL is V*100/L), TotL is (NewL/100)*L,
    NewV is V-TotL, evalSubset(As, NewV, Actions, C), NewC is C+TotL.
evalSubset(Actuators, V, Actions, 0) :-
    V =< 0, findall((A,0), member(A,Actuators), Actions).
evalSubset([], _, [], 0).


%evalSubset([A|As], V, [(A,100)|Actions], NewC) :-
%    V > 0, actuatorCapacity(A, L), actuatorType(A, onOff),
%    evalSubset(As, 0, Actions, C), NewC is C+L.
%evalSubset([A|As], V, [(A,NewL)|Actions], NewC) :-
%    V > 0, actuatorCapacity(A, L), actuatorType(A, dimm), 
%    (L =< V -> NewL is 100; NewL is V*100/L),
%    evalSubset(As, 0, Actions, C), NewC is C+L.
%evalSubset(Actuators, 0, Actions, 0) :-
%    findall((A,0), member(A,Actuators), Actions).
%evalSubset([], _, [], 0).

optimiseEnergy(Actions, Optimised) :-
    optimiseTemperature(Actions, TempActions),
    optimiseLight(Actions, LightActions),
    append(TempActions, LightActions, Optimised).

optimiseTemperature(Actions, Optimised) :-
    findall((A,V), distinct((member((A,V), Actions), actuator(A, temp))), Considered),
    maxTemperatureConsumption(Max), adjustConsumption(temp, Max, Considered, Optimised).
optimiseTemperature([], []).

optimiseLight(Actions, Optimised) :-
    findall((A,V), distinct((member((A,V), Actions), actuator(A, light), actuatorType(A, dimm))), Considered),
    findall((A,V), distinct((member((A,V), Actions), actuator(A, light), actuatorType(A, onOff))), OnOffConsidered),
    energyConsumption(OnOffConsidered, EOnOff),
    maxLightConsumption(Max), NewMax is Max - EOnOff,
    adjustConsumption(light, NewMax, Considered, DimmOptimised),
    findall((A,NewV), (member((A,V), DimmOptimised), (V > 0 -> NewV is V; NewV is 0)), DimmValues),
    append(DimmValues, OnOffConsidered, Optimised).
optimiseLight([], []).

adjustConsumption(PropType, MaxConsumption, Requests, NewRequests) :-
    energyConsumption(Requests, E), E > MaxConsumption,
    adjustedRequests(PropType, MaxConsumption, Requests, Requests, NewRequests).
adjustConsumption(_, MaxConsumption, Requests, Requests) :-
    energyConsumption(Requests, E), E =< MaxConsumption.
adjustConsumption(_, _, [], []).

adjustedRequests(temp, MaxConsumption, [(A,V)|Requests], Original, NewRequests) :-
    energyConsumption([(A,V)|Requests], E), E > MaxConsumption,
    member((A,OldV), Original), Diff is OldV - V, Diff < 2,
    sort(2, @>=, [(A,V)|Requests], [(A1,Max)|TmpRequests]), %sort by decreasing temperature
    temperatureTuning((A1,Max), (A1,NewV)),
    adjustedRequests(temp, MaxConsumption, [(A1,NewV)|TmpRequests], Original, NewRequests).
adjustedRequests(temp, MaxConsumption, [(A,V)|Requests], Original, [(A,V)|Requests]) :-
    energyConsumption([(A,V)|Requests], E), E > MaxConsumption,
    member((A,OldV), Original), Diff is OldV - V, Diff >= 2.
adjustedRequests(temp, MaxConsumption, Requests, _, Requests) :-
    energyConsumption(Requests, E), E =< MaxConsumption.

adjustedRequests(light, MaxConsumption, Requests, Original, NewRequests) :-
    energyConsumption(Requests, E), E > MaxConsumption,
    member((A,V), Requests), member((A,OldV), Original), Diff is OldV - V, Diff < 5,
    findall(NewSetting, (adjustedLight(Requests, NewSetting)), TmpRequests),
    adjustedRequests(light, MaxConsumption, TmpRequests, Original, NewRequests).
adjustedRequests(light, MaxConsumption, Requests, Original, Requests) :-
    energyConsumption(Requests, E), E > MaxConsumption,
    member((A,V), Requests), member((A,OldV), Original), Diff is OldV - V, Diff >= 5.
adjustedRequests(light, MaxConsumption, Requests, _, Requests) :-
    energyConsumption(Requests, E), E =< MaxConsumption.

temperatureTuning((A,V), (A,NewV)) :- NewV is V - 0.5.

adjustedLight(Requests, (Actuator,NewValue)) :- 
    member((Actuator,Value), Requests), lightTuning((Actuator,Value), (Actuator,NewValue)).

lightTuning((A,V), (A,NewV)) :- NewV is V - 1.

energyConsumption([(A,Value)|Considered], Cons) :-
    propertyInstance(_, _, temp, [A], [Sensor]), 
    energyConsumption(Considered, Cons1),
    actuatorConsumption(A, C),
    sensorValue(Sensor, T), Diff is Value - T,
    (Diff < 0 -> Cons is Cons1 ; Cons is Cons1 + (Diff*C)).
energyConsumption([(A,Value)|Considered], Cons) :-
    actuator(A, light),
    energyConsumption(Considered, Cons1),
    actuatorConsumption(A, C),
    (Value < 0 -> Cons is Cons1; Cons is Cons1 + (Value*C)).
energyConsumption([], 0).


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