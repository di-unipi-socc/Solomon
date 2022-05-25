:- discontiguous user/2.
:- discontiguous roles/2.
:- discontiguous actuator/2.
:- discontiguous actuatorConsumption/2.

% Sample data: static %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% propertyType(TypeId).
propertyType(light).
propertyType(temp).

% actuator(AId, TypeId).

actuator(smallLight, light).
actuatorConsumption(smallLight, 25).

actuator(mainLight, light).
actuatorConsumption(mainLight, 60).

actuator(cornerLight, light).
actuatorConsumption(cornerLight, 50).

actuator(ac, temp).
actuatorConsumption(ac, 1000).

% sensor(SId, TypeId).
sensor(brightness, light).
sensor(temperature, temp).

% zone(ZId, AppliedPolicy).
zone(home).
zone(livingroom).

subzone(livingroom, home).

% propertyInstance(ZoneId, PropertyInstanceId, PropertyType, ListOfActuators, ListOfSensors)
propertyInstance(livingroom, studyingLight, light, [cornerLight, mainLight], [brightness]).
propertyInstance(livingroom, movieLight, light, [cornerLight, smallLight], [brightness]).
propertyInstance(livingroom, readingLight, light, [smallLight], [brightness]).

propertyInstance(livingroom, roomTemp, temp, [ac], [temperature]).

% user(UId, AllowedZonesLs).
user(alice, [home]).
roles(alice, [owner]).
user(bob, [home]).
roles(bob, []).

% Sample data: dynamic %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sensorValue(SId, Value).
sensorValue(brightness, 20). % out of 255
sensorValue(temperature, 22). % degrees centigrade

actuatorSetting(smallLight, 0).
actuatorSetting(mainLight, 100).
actuatorSetting(cornerLight, 0).
actuatorSetting(ac, 23).

% set(UId, ZId, PIId, Value).
set(alice, livingroom, movieLight, 20).
set(bob, livingroom, studyingLight, 80).

set(alice, livingroom, roomTemp, 20).
set(bob, livingroom, roomTemp, 26).

maxLightConsumption(1000).
