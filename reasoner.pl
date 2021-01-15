:-consult('checker'). % Verifies the consistency of the infrastructure
:-consult('utilities'). % Some utilities

% H1: the world is partitioned in zones, each characterised by one or more property instances
% H2: Agents express their "desires" by requesting to set property instances to some values of the form
%     set(Agent,Zone,PI,Value)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% actuator(AId, Property).
% sensor(SId, Property).
% sensorValue(SId, Value).

% zone(ZId, AppliedPolicy).
% propertyInstance(ZId, PIId, Property, ActuatorsLs, SensorsLs).

% user(UId, AllowedZonesLs).
% set(UId, ZId, PIId, Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

react(Requests, MediatedRequests, Actions) :- 
    getRequests(Requests, ValidRequests),               % request harvesting (into a list of <A,Z,Pi,V>), discarding invalid requests
    mediateRequests(ValidRequests, MediatedRequests),   % [Defined by Admin] determines a list of <Z,Pi,V> such that there is at most one request per Pi
    validMediation(MediatedRequests),
    associateActions(MediatedRequests, Actions),         % [Defined by Admin] determines a list of <Actuator,Value> such that there is ...
    validActions(Actions).

getRequests(Requests, ValidRequests) :-
    findall((ZId, PIId, Value, UId), set(UId, ZId, PIId, Value), Requests),
    findall((ZId, PIId, Value, UId), ( member((ZId, PIId, Value, UId), Requests), user(UId, Zones), member(ZId ,Zones), validRequest(ZId, PIId, Value) ), ValidRequests).

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



% Alternative with better performance
% validActions(Actions) :-
%     sort(Actions, OrderedActions),
%     validActionSequence(OrderedActions).

% validActionSequence([]).
% validActionSequence([(_,_)]).
% validActionSequence([(A,V),(X,Y)|L]) :- validValue(A,V), dif(A,X), validActionSequence([(X,Y)|L]).