:- discontiguous user/2.
:- discontiguous roles/2.
:- discontiguous sensor/2.
:- discontiguous actuator/2.
:- discontiguous actuatorType/2.
:- discontiguous actuatorCapacity/2.
:- discontiguous actuatorConsumption/2.

:- use_module(library(lists)).


% H1: the world is partitioned in zones, each characterised by one or more property instances
% H2: Agents express their "desires" by requesting to set property instances to some values of the form
%     set(Agent,Zone,PI,Value)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% propertyType(TypeId).

% actuator(AId, Property).
% sensor(SId, Property).
% sensorValue(SId, Value).

% zone(ZId, AppliedPolicy).
% subzone(ZId, SubZIds).
% propertyInstance(ZId, PIId, Property, ActuatorsLs, SensorsLs).

% user(UId, AllowedZonesLs).
% roles(UId, RolesLs).
% set(UId, ZId, PIId, Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

react(Requests, MediatedRequests, Actions) :- 
    getRequests(Requests, ValidRequests),               % request harvesting (into a list of <A,Z,Pi,V>), discarding invalid requests
    mediateRequests(ValidRequests, MediatedRequests),   % [Defined by Admin] determines a list of <Z,Pi,V> such that there is at most one request per Pi
    validMediation(MediatedRequests),
    associateActions(MediatedRequests, TmpActions),     % [Defined by Admin] determines a list of <Actuator,Value> such that there is ...
    %validActions(TmpActions),
    optimiseEnergy(TmpActions, Actions),                % [Defined by Admin] optimises the energy consumption of the actions
    validActions(Actions).

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


% Alternative with better performance
% validActions(Actions) :-
%     sort(Actions, OrderedActions),
%     validActionSequence(OrderedActions).

% validActionSequence([]).
% validActionSequence([(_,_)]).
% validActionSequence([(A,V),(X,Y)|L]) :- validValue(A,V), dif(A,X), validActionSequence([(X,Y)|L]).

propertyType(temp).
propertyType(light).


%actuator(heating_public, temp).
actuator(heating_cs_private_1, temp).
actuatorConsumption(heating_cs_private_1, 2).

actuator(heating_cs_private_2, temp).
actuatorConsumption(heating_cs_private_2, 2).

actuator(heating_public, temp).
actuatorConsumption(heating_public, 2).

actuator(heating_phy_private_1, temp).
actuatorConsumption(heating_phy_private_1, 2).

actuator(heating_phy_private_2, temp).
actuatorConsumption(heating_phy_private_2, 2).


actuator(lampOnOff1_cs_private_1, light).
actuatorType(lampOnOff1_cs_private_1, onOff).
actuatorCapacity(lampOnOff1_cs_private_1, 75).
actuatorConsumption(lampOnOff1_cs_private_1, 1).

actuator(lampOnOff2_cs_private_1, light).
actuatorType(lampOnOff2_cs_private_1, onOff).
actuatorCapacity(lampOnOff2_cs_private_1, 75).
actuatorConsumption(lampOnOff2_cs_private_1, 2).

actuator(lampDimm1_cs_private_1, light).
actuatorType(lampDimm1_cs_private_1, dimm).
actuatorCapacity(lampDimm1_cs_private_1, 75).
actuatorConsumption(lampDimm1_cs_private_1, 1).

actuator(lampOnOff1_cs_private_2, light).
actuatorType(lampOnOff1_cs_private_2, onOff).
actuatorCapacity(lampOnOff1_cs_private_2, 75).
actuatorConsumption(lampOnOff1_cs_private_2, 1).

actuator(lampOnOff2_cs_private_2, light).
actuatorType(lampOnOff2_cs_private_2, onOff).
actuatorCapacity(lampOnOff2_cs_private_2, 75).
actuatorConsumption(lampOnOff2_cs_private_2, 2).

actuator(lampDimm1_cs_private_2, light).
actuatorType(lampDimm1_cs_private_2, dimm).
actuatorCapacity(lampDimm1_cs_private_2, 75).
actuatorConsumption(lampDimm1_cs_private_2, 1).

actuator(lampOnOff1_cs_public, light).
actuatorType(lampOnOff1_cs_public, onOff).
actuatorCapacity(lampOnOff1_cs_public, 75).
actuatorConsumption(lampOnOff1_cs_public, 1).

actuator(lampOnOff2_cs_public, light).
actuatorType(lampOnOff2_cs_public, onOff).
actuatorCapacity(lampOnOff2_cs_public, 75).
actuatorConsumption(lampOnOff2_cs_public, 2).

actuator(lampDimm1_cs_public, light).
actuatorType(lampDimm1_cs_public, dimm).
actuatorCapacity(lampDimm1_cs_public, 75).
actuatorConsumption(lampDimm1_cs_public, 1).

actuator(lampDimm2_cs_public, light).
actuatorType(lampDimm2_cs_public, dimm).
actuatorCapacity(lampDimm2_cs_public, 75).
actuatorConsumption(lampDimm2_cs_public, 2).

actuator(lampOnOff1_phy_private_1, light).
actuatorType(lampOnOff1_phy_private_1, onOff).
actuatorCapacity(lampOnOff1_phy_private_1, 75).
actuatorConsumption(lampOnOff1_phy_private_1, 1).

actuator(lampOnOff2_phy_private_1, light).
actuatorType(lampOnOff2_phy_private_1, onOff).
actuatorCapacity(lampOnOff2_phy_private_1, 75).
actuatorConsumption(lampOnOff2_phy_private_1, 2).

actuator(lampDimm1_phy_private_1, light).
actuatorType(lampDimm1_phy_private_1, dimm).
actuatorCapacity(lampDimm1_phy_private_1, 75).
actuatorConsumption(lampDimm1_phy_private_1, 1).

actuator(lampOnOff1_phy_private_2, light).
actuatorType(lampOnOff1_phy_private_2, onOff).
actuatorCapacity(lampOnOff1_phy_private_2, 75).
actuatorConsumption(lampOnOff1_phy_private_2, 1).

actuator(lampOnOff2_phy_private_2, light).
actuatorType(lampOnOff2_phy_private_2, onOff).
actuatorCapacity(lampOnOff2_phy_private_2, 75).
actuatorConsumption(lampOnOff2_phy_private_2, 2).

actuator(lampDimm1_phy_private_2, light).
actuatorType(lampDimm1_phy_private_2, dimm).
actuatorCapacity(lampDimm1_phy_private_2, 75).
actuatorConsumption(lampDimm1_phy_private_2, 1).

actuator(lampOnOff1_phy_public, light).
actuatorType(lampOnOff1_phy_public, onOff).
actuatorCapacity(lampOnOff1_phy_public, 75).
actuatorConsumption(lampOnOff1_phy_public, 1).

actuator(lampOnOff2_phy_public, light).
actuatorType(lampOnOff2_phy_public, onOff).
actuatorCapacity(lampOnOff2_phy_public, 75).
actuatorConsumption(lampOnOff2_phy_public, 2).

actuator(lampDimm1_phy_public, light).
actuatorType(lampDimm1_phy_public, dimm).
actuatorCapacity(lampDimm1_phy_public, 75).
actuatorConsumption(lampDimm1_phy_public, 1).

actuator(lampDimm2_phy_public, light).
actuatorType(lampDimm2_phy_public, dimm).
actuatorCapacity(lampDimm2_phy_public, 75).
actuatorConsumption(lampDimm2_phy_public, 2).


sensor(temperature_cs_public, temp).
sensor(temperature_cs_private_1, temp).
sensor(temperature_cs_private_2, temp).

sensor(temperature_phy_public, temp).
sensor(temperature_phy_private_1, temp).
sensor(temperature_phy_private_2, temp).


sensor(light_cs_public, light).
sensor(light_cs_private_1, light).
sensor(light_cs_private_2, light).

sensor(light_phy_public, light).
sensor(light_phy_private_1, light).
sensor(light_phy_private_2, light).

%actuator(heating_public, temp).
actuator(heating_bio_private_1, temp).
actuatorConsumption(heating_bio_private_1, 2).

actuator(heating_bio_private_2, temp).
actuatorConsumption(heating_bio_private_2, 2).

actuator(heating_med_private_1, temp).
actuatorConsumption(heating_med_private_1, 2).

actuator(heating_med_private_2, temp).
actuatorConsumption(heating_med_private_2, 2).


actuator(lampOnOff1_bio_private_1, light).
actuatorType(lampOnOff1_bio_private_1, onOff).
actuatorCapacity(lampOnOff1_bio_private_1, 75).
actuatorConsumption(lampOnOff1_bio_private_1, 1).

actuator(lampOnOff2_bio_private_1, light).
actuatorType(lampOnOff2_bio_private_1, onOff).
actuatorCapacity(lampOnOff2_bio_private_1, 75).
actuatorConsumption(lampOnOff2_bio_private_1, 2).

actuator(lampDimm1_bio_private_1, light).
actuatorType(lampDimm1_bio_private_1, dimm).
actuatorCapacity(lampDimm1_bio_private_1, 75).
actuatorConsumption(lampDimm1_bio_private_1, 1).

actuator(lampOnOff1_bio_private_2, light).
actuatorType(lampOnOff1_bio_private_2, onOff).
actuatorCapacity(lampOnOff1_bio_private_2, 75).
actuatorConsumption(lampOnOff1_bio_private_2, 1).

actuator(lampOnOff2_bio_private_2, light).
actuatorType(lampOnOff2_bio_private_2, onOff).
actuatorCapacity(lampOnOff2_bio_private_2, 75).
actuatorConsumption(lampOnOff2_bio_private_2, 2).

actuator(lampDimm1_bio_private_2, light).
actuatorType(lampDimm1_bio_private_2, dimm).
actuatorCapacity(lampDimm1_bio_private_2, 75).
actuatorConsumption(lampDimm1_bio_private_2, 1).

actuator(lampOnOff1_bio_public, light).
actuatorType(lampOnOff1_bio_public, onOff).
actuatorCapacity(lampOnOff1_bio_public, 75).
actuatorConsumption(lampOnOff1_bio_public, 1).

actuator(lampOnOff2_bio_public, light).
actuatorType(lampOnOff2_bio_public, onOff).
actuatorCapacity(lampOnOff2_bio_public, 75).
actuatorConsumption(lampOnOff2_bio_public, 2).

actuator(lampDimm1_bio_public, light).
actuatorType(lampDimm1_bio_public, dimm).
actuatorCapacity(lampDimm1_bio_public, 75).
actuatorConsumption(lampDimm1_bio_public, 1).

actuator(lampDimm2_bio_public, light).
actuatorType(lampDimm2_bio_public, dimm).
actuatorCapacity(lampDimm2_bio_public, 75).
actuatorConsumption(lampDimm2_bio_public, 2).

actuator(lampOnOff1_med_private_1, light).
actuatorType(lampOnOff1_med_private_1, onOff).
actuatorCapacity(lampOnOff1_med_private_1, 75).
actuatorConsumption(lampOnOff1_med_private_1, 1).

actuator(lampOnOff2_med_private_1, light).
actuatorType(lampOnOff2_med_private_1, onOff).
actuatorCapacity(lampOnOff2_med_private_1, 75).
actuatorConsumption(lampOnOff2_med_private_1, 2).

actuator(lampDimm1_med_private_1, light).
actuatorType(lampDimm1_med_private_1, dimm).
actuatorCapacity(lampDimm1_med_private_1, 75).
actuatorConsumption(lampDimm1_med_private_1, 1).

actuator(lampOnOff1_med_private_2, light).
actuatorType(lampOnOff1_med_private_2, onOff).
actuatorCapacity(lampOnOff1_med_private_2, 75).
actuatorConsumption(lampOnOff1_med_private_2, 1).

actuator(lampOnOff2_med_private_2, light).
actuatorType(lampOnOff2_med_private_2, onOff).
actuatorCapacity(lampOnOff2_med_private_2, 75).
actuatorConsumption(lampOnOff2_med_private_2, 2).

actuator(lampDimm1_med_private_2, light).
actuatorType(lampDimm1_med_private_2, dimm).
actuatorCapacity(lampDimm1_med_private_2, 75).
actuatorConsumption(lampDimm1_med_private_2, 1).

actuator(lampOnOff1_med_public, light).
actuatorType(lampOnOff1_med_public, onOff).
actuatorCapacity(lampOnOff1_med_public, 75).
actuatorConsumption(lampOnOff1_med_public, 1).

actuator(lampOnOff2_med_public, light).
actuatorType(lampOnOff2_med_public, onOff).
actuatorCapacity(lampOnOff2_med_public, 75).
actuatorConsumption(lampOnOff2_med_public, 2).

actuator(lampDimm1_med_public, light).
actuatorType(lampDimm1_med_public, dimm).
actuatorCapacity(lampDimm1_med_public, 75).
actuatorConsumption(lampDimm1_med_public, 1).

actuator(lampDimm2_med_public, light).
actuatorType(lampDimm2_med_public, dimm).
actuatorCapacity(lampDimm2_med_public, 75).
actuatorConsumption(lampDimm2_med_public, 2).


sensor(temperature_bio_public, temp).
sensor(temperature_bio_private_1, temp).
sensor(temperature_bio_private_2, temp).

sensor(temperature_med_public, temp).
sensor(temperature_med_private_1, temp).
sensor(temperature_med_private_2, temp).


sensor(light_bio_public, light).
sensor(light_bio_private_1, light).
sensor(light_bio_private_2, light).

sensor(light_med_public, light).
sensor(light_med_private_1, light).
sensor(light_med_private_2, light).



zone(campus).

zone(public_campus).

zone(cs_dept).
zone(phy_dept).
zone(bio_dept).
zone(med_dept).

zone(cs_public).
zone(cs_private).

zone(phy_public).
zone(phy_private).

zone(bio_public).
zone(bio_private).

zone(med_public).
zone(med_private).

zone(cs_private_room_1).
zone(cs_private_room_2).

zone(phy_private_room1).
zone(phy_private_room2).

zone(bio_private_room1).
zone(bio_private_room2).

zone(med_private_room1).
zone(med_private_room2).

subzone(cs_dept, campus).
subzone(phy_dept, campus).

subzone(public_campus, campus).
subzone(cs_public, public_campus).
subzone(phy_public, public_campus).

subzone(cs_public, cs_dept).
subzone(cs_private, cs_dept).

subzone(phy_public, phy_dept).
subzone(phy_private, phy_dept).

subzone(cs_private_room_1, cs_private).
subzone(cs_private_room_2, cs_private).

subzone(phy_private_room_1, phy_private).
subzone(phy_private_room_2, phy_private).

subzone(bio_dept, campus).
subzone(med_dept, campus).

subzone(bio_public, public_campus).
subzone(med_public, public_campus).

subzone(bio_public, bio_dept).
subzone(bio_private, bio_dept).

subzone(med_public, med_dept).
subzone(med_private, med_dept).

subzone(bio_private_room_1, bio_private).
subzone(bio_private_room_2, bio_private).

subzone(med_private_room_1, med_private).
subzone(med_private_room_2, med_private).


propertyInstance(cs_public, temperature, temp, [heating_public], [temperature_cs_public]).
propertyInstance(cs_private_room_1, temperature, temp, [heating_cs_private_1], [temperature_cs_private_1]).
propertyInstance(cs_private_room_2, temperature, temp, [heating_cs_private_2], [temperature_cs_private_2]).

propertyInstance(phy_public, temperature, temp, [heating_public], [temperature_phy_public]).
propertyInstance(phy_private_room_1, temperature, temp, [heating_phy_private_1], [temperature_phy_private_1]).
propertyInstance(phy_private_room_2, temperature, temp, [heating_phy_private_2], [temperature_phy_private_2]).


propertyInstance(cs_public, light, light, [lampOnOff1_cs_public, lampOnOff2_cs_public, lampDimm1_cs_public, lampDimm2_cs_public], [light_cs_public]).
propertyInstance(cs_private_room_1, light, light, [lampOnOff1_cs_private_1, lampOnOff2_cs_private_1, lampDimm1_cs_private_1], [light_cs_private_1]).
propertyInstance(cs_private_room_2, light, light, [lampOnOff1_cs_private_2, lampOnOff2_cs_private_2, lampDimm1_cs_private_2], [light_cs_private_2]).

propertyInstance(phy_public, light, light, [lampOnOff1_phy_public, lampOnOff2_phy_public, lampDimm1_phy_public, lampDimm2_phy_public], [light_phy_public]).
propertyInstance(phy_private_room_1, light, light, [lampOnOff1_phy_private_1, lampOnOff2_phy_private_1, lampDimm1_phy_private_1], [light_phy_private_1]).
propertyInstance(phy_private_room_2, light, light, [lampOnOff1_phy_private_2, lampOnOff2_phy_private_2, lampDimm1_phy_private_2], [light_phy_private_2]).

propertyInstance(bio_public, temperature, temp, [heating_public], [temperature_bio_public]).
propertyInstance(bio_private_room_1, temperature, temp, [heating_bio_private_1], [temperature_bio_private_1]).
propertyInstance(bio_private_room_2, temperature, temp, [heating_bio_private_2], [temperature_bio_private_2]).

propertyInstance(med_public, temperature, temp, [heating_public], [temperature_med_public]).
propertyInstance(med_private_room_1, temperature, temp, [heating_med_private_1], [temperature_med_private_1]).
propertyInstance(med_private_room_2, temperature, temp, [heating_med_private_2], [temperature_med_private_2]).


propertyInstance(bio_public, light, light, [lampOnOff1_bio_public, lampOnOff2_bio_public, lampDimm1_bio_public, lampDimm2_bio_public], [light_bio_public]).
propertyInstance(bio_private_room_1, light, light, [lampOnOff1_bio_private_1, lampOnOff2_bio_private_1, lampDimm1_bio_private_1], [light_bio_private_1]).
propertyInstance(bio_private_room_2, light, light, [lampOnOff1_bio_private_2, lampOnOff2_bio_private_2, lampDimm1_bio_private_2], [light_bio_private_2]).

propertyInstance(med_public, light, light, [lampOnOff1_med_public, lampOnOff2_med_public, lampDimm1_med_publiorc, lampDimm2_med_public], [light_med_public]).
propertyInstance(med_private_room_1, light, light, [lampOnOff1_med_private_1, lampOnOff2_med_private_1, lampDimm1_med_private_1], [light_med_private_1]).
propertyInstance(med_private_room_2, light, light, [lampOnOff1_med_private_2, lampOnOff2_med_private_2, lampDimm1_med_private_2], [light_med_private_2]).


user(the_rector, [campus]).
roles(the_rector, [rector, professor]).

user(head_cs, [cs_dept, public_campus]).
roles(head_cs, [head_dept, professor]).

user(head_phy, [phy_dept, public_campus]).
roles(head_phy, [head_dept, professor]).

user(user_1, [cs_private_room_1, public_campus]).
roles(user_1, [reasearcher]).

user(user_2, [cs_private_room_1, public_campus]).
roles(user_2, [reasearcher]).

user(user_3, [phy_private_room_1, public_campus]).
roles(user_3, [professor]).

user(user_4, [public_campus]).
roles(user_4, [secretary]).

user(guest_1, [public_campus]).
roles(guest_1, []).

user(guest_2, [public_campus]).
roles(guest_2, []).

user(guest_3, [public_campus]).
roles(guest_3, []).



set(the_rector, cs_private_room_1, temperature, 23).
set(head_cs, cs_private_room_1, temperature, 30).
set(user_1, cs_private_room_1, temperature, 18).
set(user_2, cs_private_room_1, temperature, 18).

set(user_4, cs_public, temperature, 18).
set(guest_1, cs_public, temperature, 26).
set(guest_2, cs_public, temperature, 26).

set(head_phy, phy_public, temperature, 20).
set(user_3, phy_public, temperature, 28).
set(guest_3, phy_public, temperature, 28).

set(the_rector, cs_private_room_1, temperature, high).
set(user_1, cs_private_room_1, light, high).
set(user_2, cs_private_room_1, light, low).

set(guest_1, cs_public, light, medium).
set(guest_2, cs_public, light, medium).
set(user_4, cs_public, light, high).

set(head_phy, phy_public, light, medium).
set(user_3, phy_public, light, medium).
set(guest_3, phy_public, light, medium).



sensorValue(temperature_cs_public, 0).
sensorValue(temperature_cs_private_1, 0).
sensorValue(temperature_phy_public, 0).

sensorValue(light_cs_public, 0).
sensorValue(light_cs_private_1, 0).
sensorValue(light_phy_public, 0).

actuatorSetting(heating_public, 23).
actuatorSetting(heating_cs_private_1, 23).
actuatorSetting(heating_public, 23).

maxTemperatureConsumption(42).
maxLightConsumption(42).

minimumAllowedTemperature(18).
maximumAllowedTemperature(25).


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

directorsPolicy(Requests, Max):-
    filterRequestsByRoles(Requests, [rector, head_dept], [R|Rs]),
    getRequestsValues([R|Rs], Values),
    max_list(Values, Max).
directorsPolicy(Requests, Avg):-
    filterRequestsByRoles(Requests, [rector, head_dept], []),
    findall((WeightedV,W), (member((V,_,Roles), Requests), 
                            once(getUserWeight(temp, Roles, W)), 
                            WeightedV is V*W), Zip),
    zip(Values, Weights, Zip),  sum_list(Values, Sum), sum_list(Weights, SumW), Avg is Sum/SumW.

getUserWeight(temp, Roles, 5) :-
    member(rector, Roles).
getUserWeight(temp, Roles, 5) :-
    member(head_dept, Roles).
getUserWeight(temp, Roles, 3) :-
    member(professor, Roles).
getUserWeight(temp, Roles, 2) :-
    member(secretary, Roles).
getUserWeight(temp, Roles, 2) :-
    member(reasearcher, Roles).
getUserWeight(temp, _, 1).

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
    findall((E,Actions), (subSet(As, Sub), 
                          evalSubset(Sub, V, Actions, C), C >= V, 
                          energyConsumption(Actions, E)), 
                          AllActions),
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