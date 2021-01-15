% Sample data: static %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% propertyType(TypeId).
propertyType(light).

% actuator(AId, TypeId).
actuator(wardrobeLight1, light).
actuator(bedLight1, light).
actuator(mainLight1,   light).
actuator(cornerLight1,   light).

% sensor(SId, TypeId).
sensor(brightness1,  light).

% zone(ZId, AppliedPolicy).
zone(bedroom, _).

% propertyInstance(ZoneId, PropertyInstanceId, PropertyType, ListOfActuators, ListOfSensors)
propertyInstance(bedroom, morningLight,   light,  [wardrobeLight1],                          [brightness1]).
propertyInstance(bedroom, nightLight,     light,  [bedLight1],                               [brightness1]).
propertyInstance(bedroom, movieLight,     light,  [cornerLight1, bedLight1],                 [brightness1]).
propertyInstance(bedroom, workLight,      light,  [cornerLight1, mainLight1],                [brightness1]).
propertyInstance(bedroom, cleaningLight,  light,  [mainLight1],                              [brightness1]).
propertyInstance(bedroom, dinnerLight,    light,  [cornerLight1, bedLight1, wardrobeLight1], [brightness1]).

% user(UId, AllowedZonesLs).
user(u1, [bedroom]).
user(u2, [bedroom]).

% Sample data: dynamic %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sensorValue(SId, Value).
sensorValue(brightness, 20). % out of 100

% set(UId, ZId, PIId, Value).
set(u1, bedroom, movieLight, 50).
set(u1, bedroom, nightLight, 100).
set(u2, bedroom, movieLight, 10).
