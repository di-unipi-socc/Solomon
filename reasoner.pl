:-consult('model').     % Infrastructure
:-consult('policies').  % Admin-defined policies
:-consult('checker').   % Verifies the consistency of the infrastructure

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% react/3 first collects in Requests all users' requests (to set the value of a
% propertyInstance of an existing zone) in the format:
%  (ZoneId, PropertyInstance, ListOfRequests).
% react/3 then mediates the collected Requests into MediatedRequests in the format:
%  (ZoneId, PropertyInstance, MediatedValue),
% react/3 finally determine the Actions to achieve:
% a list of pair (ActuatorId, Value)
react(Requests, MediatedRequests, Actions) :- 
    getRequests(Requests),                        % collects all users' requests
    mediateRequests(Requests, MediatedRequests),  % [Defined by Admin] mediates them, yielding at most one request per PropertyInstance
    associateActions(MediatedRequests, Actions).  % determines Actions to be executed

% getRequests/1 collects in Reqs all users' requests (to set the value of a
% propertyInstance of an existing zone) in the format: (ZoneId, PropertyInstance, ListOfRequests)
% where ListOfRequests is a list containing all the users' requests for a specific PropertyInstance in a specific Zone
getRequests(Reqs) :- 
    findall((Z, PI, PIReqs), requestsPerPI(Z, PI, PIReqs), AllReqs), sort(AllReqs, Reqs).

% requestsPerPI returns all users' requests for a specific PropertyInstance in a specific Zone
requestsPerPI(Z, PI, PIReqs) :- 
    propertyInstance(Z, PI, _, _, _), findall((V, UId),( set(UId, Z, PI, V), user(UId,Zones), member(Z,Zones) ), PIReqs).

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
