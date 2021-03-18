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

mediatePI(Z, PI, Ls, (Z, PI, Avg)) :-
    findall(V, member((V,_),Ls), Values), % get values
    avg(Values,Avg).


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
setActuators(Actions, ExecutableActions) :- setActuatorsWithMax(Actions, 0, 100, ExecutableActions).