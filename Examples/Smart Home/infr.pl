% Sample data: static %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% propertyType(TypeId).
propertyType(light).
propertyType(temp).

% actuator(AId, TypeId).

actuator(smallLight, light).
actuator(mainLight, light).
actuator(cornerLight, light).

actuator(ac, temp).

% sensor(SId, TypeId).
sensor(brightness, light).
sensor(temperature, temp).

% zone(ZId, AppliedPolicy).
zone(livingroom, _).

% propertyInstance(ZoneId, PropertyInstanceId, PropertyType, ListOfActuators, ListOfSensors)
propertyInstance(livingroom, studyingLight, light, [cornerLight, mainLight], [brightness]).
propertyInstance(livingroom, movieLight, light, [cornerLight, smallLight], [brightness]).
propertyInstance(livingroom, readingLight, light, [smallLight], [brightness]).

propertyInstance(livingroom, roomTemp, temp, [ac], [temperature]).

% user(UId, AllowedZonesLs).
user(alice, [livingroom]).
user(bob, [livingroom]).

% Sample data: dynamic %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sensorValue(SId, Value).
sensorValue(brightness, 20). % out of 255
sensorValue(temperature, 22). % degrees centigrade

% set(UId, ZId, PIId, Value).
set(alice, livingroom, movieLight, 20).
set(bob, livingroom, studyingLight, 80).

set(alice, livingroom, roomTemp, 20).
set(bob, livingroom, roomTemp, 26).
