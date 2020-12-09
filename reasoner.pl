:-consult('model').
:-consult('policies').
:-consult('checker').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

react(Requests, MediatedRequests, Actions) :- 
    getRequests(Requests),                          % collects all agents'requests
    mediateRequests(Requests, MediatedRequests),    % mediates them, yielding at most one request per PropertyInstance
    associateActions(MediatedRequests, Actions).    % determines Actions to be executed

getRequests(Reqs) :- 
    findall((Z, PI, PIReqs), requestsPerPI(Z, PI, PIReqs), AllReqs), sort(AllReqs, Reqs).

requestsPerPI(Z, PI, PIReqs) :- 
    propertyInstance(Z, PI, _, _, _), findall((V, Ag),( set(Ag, Z, PI, V)), PIReqs).

associateActions(Requests, ExecutableActions) :-
    actionsFor(Requests, Actions),               % returns a list of actions (ActuatorId, Value)
    setActuators(Actions, ExecutableActions).  % determines a single action for each actuator

actionsFor([],[]).
actionsFor([(Z, PI, V)|Reqs], Actions) :-
    propertyInstance(Z, PI, _, ActuatorList, SensorList), 
    selectActionsForPI(Z, PI, V, ActuatorList, SensorList, Actions1),       
    actionsFor(Reqs, Actions2), append(Actions1, Actions2, Actions).
