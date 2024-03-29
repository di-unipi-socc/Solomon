:- consult('infr').
:- consult('../../reasoner.pl').

% mediateRequests/2  for each pair (Zone, PropertyInstance) obtains users' requests, 
% averages them and then applies policies based on the propertyInstance and various environmental parameters.

mediateRequests(Requests, Mediated) :-
    groupPerPI(Requests, NewRequests),
    mediateRequest(NewRequests, Mediated).

mediateRequest([],[]).
mediateRequest([(Z,PI,Ls)|Reqs], [Mediated|OtherMediatedReqs]) :-
    mediatePI(Z,PI,Ls,Mediated),
    mediateRequest(Reqs, OtherMediatedReqs).

mediatePI(_, _, [], undef).
mediatePI(Z, PI, Ls, (Z, PI, Avg)) :-
    findall(V, member((V,_),Ls), Values), % get values
    zone(Z, Policy), % get the zone policy
    avg(Values,AvgTmp),
    % department-wise policy
    propertyInstance(Z, PI, Prop, _, [Sensor]),
    sensorValue(Sensor, SensedValue),
    findValue(Policy, Prop, SensedValue, AvgTmp, Avg). % apply some environmental policies


findValue(_, temp, _, TempValue, Value) :-
    season(S),
    % eco-policy
    (
        ((S = winter ; S = autumn), (TempValue > 22, Value is 22; TempValue < 18, Value is 18; Value is TempValue))
        ;
        ((S = summer ; S = spring), (TempValue > 28, Value is 28; TempValue < 24, Value is 24; Value is TempValue))
    ).

findValue(east, light, _, LightValue, Value) :-
    (LightValue > 255, Value is 255; LightValue < 100, Value is 100; Value is LightValue).

findValue(west, light, Brightness, LightValue, Value) :-
    (
        (Brightness > 100, (LightValue > 255, Value is 255; LightValue < 100, Value is 100; Value is LightValue))
        ;
        (LightValue > 255, Value is 255; LightValue < 180, Value is 180; Value is LightValue)
    ).

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
actionsFor([undef|Reqs], Actions) :-
    actionsFor(Reqs, Actions).
actionsFor([(Z, PI, V)|Reqs], Actions) :-
    propertyInstance(Z, PI, _, ActuatorList, SensorList),              % given a Zone and its PropertyInstace takes the set of actuators and sensors
    selectActionsForPI(Z, PI, V, ActuatorList, SensorList, Actions1),  % [Defined by Admin] for each actuator in the PropertyInstance assigns an action (ActuatorId, Value)    
    actionsFor(Reqs, Actions2), append(Actions1, Actions2, Actions).   % combines these actions with the others

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
setActuators(Actions, ExecutableActions) :- setActuatorsWithMax(Actions, -inf,inf, ExecutableActions).