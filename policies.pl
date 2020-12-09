:-consult('utilities').

% mediateRequests/2  for each pair (Zone, PropertyInstance) obtains users' requests, 
% averages them and then applies policies based on the propertyInstance and various environmental parameters.
mediateRequests([],[]).
mediateRequests([(Z,PI,Ls)|Reqs], MediatedReqs) :-
    mediatePI(Z,PI,Ls,Mediated), % mediate the requests
    mediateRequests(Reqs, OtherMediatedReqs),
    append(Mediated, OtherMediatedReqs, MediatedReqs).

mediatePI(_,_,[],[]).
mediatePI(Z, PI, Ls, [(Z, PI, Avg)]) :-
    findall(V, (member((V,U),Ls), user(U,Zones), member(Z,Zones)), Values), % get values
    zone(Z, Policy), % get the zone policy
    avg(Values,AvgTmp),
    % department-wise policy
    propertyInstance(Z, PI, Prop, _, _),
    findValue(Policy, Prop, AvgTmp, Avg). % apply some environmental policies


findValue(_, temp, TempValue, Value) :-
    season(S),
    % eco-policy
    (
        ((S = winter ; S = autumn), (TempValue > 22, Value is 22; TempValue < 18, Value is 18; Value is TempValue))
        ;
        ((S = summer ; S = spring), (TempValue > 28, Value is 28; TempValue < 24, Value is 24; Value is TempValue))
    ).

findValue(east, light, LightValue, Value) :-
    (LightValue > 255, Value is 255; LightValue < 100, Value is 100; Value is LightValue).

findValue(west, light, LightValue, Value) :-
    weather(W),
    (
        (W = sunny, (LightValue > 255, Value is 255; LightValue < 100, Value is 100; Value is LightValue))
        ;
        (LightValue > 255, Value is 255; LightValue < 180, Value is 180; Value is LightValue)
    ).


% determineActions defined by SysAdmin (e.g. can use sensor values taken from the 4th param) %%%%%%%%%%%%%%%%%%%%%%%%%
selectActionsForPI(_, _, V, ActuatorList, _, Actions) :-
    length(ActuatorList, ActuatorsNumber),
    triggerAllActuators(V, ActuatorsNumber, ActuatorList, Actions).

triggerAllActuators(_, _, [], []).

triggerAllActuators(V, ActuatorsNumber, [Actuator|ActuatorList], [(Actuator,VNew)|Actions]) :-
    dif(Actuator, heater),
    VNew is V/ActuatorsNumber,
    triggerAllActuators(V, ActuatorsNumber, ActuatorList, Actions).

triggerAllActuators(V, ActuatorsNumber, [heater|ActuatorList], [(heater,100)|Actions]) :-
    V > 0, triggerAllActuators(V, ActuatorsNumber, ActuatorList, Actions).

triggerAllActuators(V, ActuatorsNumber, [heater|ActuatorList], [(heater,0)|Actions]) :-
    V =< 0, triggerAllActuators(V, ActuatorsNumber, ActuatorList, Actions).

% resolveActions defined by SysAdmin, cannot read data from sensors (?) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (will mainly do avg, max, min, mode, cap) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setActuators(Actions, ExecutableActions) :- setActuatorsWithMax(Actions, -inf ,inf, ExecutableActions).