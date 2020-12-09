%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% actuator(AId, Property).
% sensor(SId, Property).
% sensorValue(SId, Value).

% zone(ZId, AppliedPolicy).
% propertyInstance(ZId, PIId, Property, ActuatorsLs, SensorsLs).

% user(UId, AllowedZonesLs).
% set(UId, ZId, PIId, Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Two wings (East, West)
% Five rooms per wing (4 "individual" 1 shared)
% Each pair of individual rooms (1&3, 2&4) share the same ac
% Each common room has its own ac
% Each "individual" room has a big and a small light
% Each common room has two big lights
% room_E_1 has an additional heater
% Each "individual" room has a lignt sensor
% Each pair has a temp sensor
% Each common room has a temp sensor and two light sensors
% Each user has his/her own room

daytime(morning).
weather(sunny).
season(winter).

actuator(acOdd_E, temp).
actuator(acEven_E, temp).
actuator(acCommonRoom_E, temp).

actuator(heater, temp).

actuator(biglightRoom_E_1, light).
actuator(biglightRoom_E_2, light).
actuator(biglightRoom_E_3, light).
actuator(biglightRoom_E_4, light).
actuator(biglightCommonRoom_E_1, light).
actuator(biglightCommonRoom_E_2, light).

actuator(smalllightRoom_E_1, light).
actuator(smalllightRoom_E_2, light).
actuator(smalllightRoom_E_3, light).
actuator(smalllightRoom_E_4, light).

actuator(acOdd_W, temp).
actuator(acEven_W, temp).
actuator(acCommonRoom_W, temp).

actuator(biglightRoom_W_1, light).
actuator(biglightRoom_W_2, light).
actuator(biglightRoom_W_3, light).
actuator(biglightRoom_W_4, light).
actuator(biglightCommonRoom_W_1, light).
actuator(biglightCommonRoom_W_2, light).

actuator(smalllightRoom_W_1, light).
actuator(smalllightRoom_W_2, light).
actuator(smalllightRoom_W_3, light).
actuator(smalllightRoom_W_4, light).



sensor(tempOdd_E, temp).
sensor(tempEven_E, temp).
sensor(tempCommonRoom_E, temp).

sensor(lightRoom_E_1, light).
sensor(lightRoom_E_2, light).
sensor(lightRoom_E_3, light).
sensor(lightRoom_E_4, light).
sensor(lightCommonRoom_E_1, light).
sensor(lightCommonRoom_E_2, light).

sensor(tempOdd_W, temp).
sensor(tempEven_W, temp).
sensor(tempCommonRoom_W, temp).

sensor(lightRoom_W_1, light).
sensor(lightRoom_W_2, light).
sensor(lightRoom_W_3, light).
sensor(lightRoom_W_4, light).
sensor(lightCommonRoom_W_1, light).
sensor(lightCommonRoom_W_2, light).



zone(room_E_1, east).
zone(room_E_2, east).
zone(room_E_3, east).
zone(room_E_4, east).
zone(commonRoom_E, east).

zone(room_W_1, west).
zone(room_W_2, west).
zone(room_W_3, west).
zone(room_W_4, west).
zone(commonRoom_W, west).



propertyInstance(room_E_1, roomTemp, temp, [acOdd_E, heater], [tempOdd_E]).
propertyInstance(room_E_2, roomTemp, temp, [acEven_E], [tempEven_E]).
propertyInstance(room_E_3, roomTemp, temp, [acOdd_E], [tempOdd_E]).
propertyInstance(room_E_4, roomTemp, temp, [acEven_E], [tempEven_E]).
propertyInstance(commonRoom_E, commonRoomTemp, temp, [acCommonRoom_E], [tempCommonRoom_E]).

propertyInstance(room_E_1, roomLight, light, [biglightRoom_E_1, smalllightRoom_E_1], [lightRoom_E_1]).
propertyInstance(room_E_2, roomLight, light, [biglightRoom_E_2, smalllightRoom_E_2], [lightRoom_E_2]).
propertyInstance(room_E_3, roomLight, light, [biglightRoom_E_3, smalllightRoom_E_3], [lightRoom_E_3]).
propertyInstance(room_E_4, roomLight, light, [biglightRoom_E_4, smalllightRoom_E_4], [lightRoom_E_4]).
propertyInstance(commonRoom_E, commonRoomLight, light, [biglightCommonRoom_E_1, biglightCommonRoom_E_2], [lightCommonRoom_E_1, lightCommonRoom_E_2]).

propertyInstance(room_E_1, nightLight, light, [biglightRoom_E_1, smalllightRoom_E_1], [lightRoom_E_1]).
propertyInstance(room_E_2, nightLight, light, [biglightRoom_E_2, smalllightRoom_E_2], [lightRoom_E_2]).
propertyInstance(room_E_3, nightLight, light, [biglightRoom_E_3, smalllightRoom_E_3], [lightRoom_E_3]).
propertyInstance(room_E_4, nightLight, light, [biglightRoom_E_4, smalllightRoom_E_4], [lightRoom_E_4]).


propertyInstance(room_W_1, roomTemp, temp, [acOdd_W], [tempOdd_W]).
propertyInstance(room_W_2, roomTemp, temp, [acEven_W], [tempEven_W]).
propertyInstance(room_W_3, roomTemp, temp, [acOdd_W], [tempOdd_W]).
propertyInstance(room_W_4, roomTemp, temp, [acEven_W], [tempEven_W]).
propertyInstance(commonRoom_W, commonRoomTemp, temp, [acCommonRoom_W], [tempCommonRoom_W]).

propertyInstance(room_W_1, roomLight, light, [biglightRoom_W_1, smalllightRoom_W_1], [lightRoom_W_1]).
propertyInstance(room_W_2, roomLight, light, [biglightRoom_W_2, smalllightRoom_W_2], [lightRoom_W_2]).
propertyInstance(room_W_3, roomLight, light, [biglightRoom_W_3, smalllightRoom_W_3], [lightRoom_W_3]).
propertyInstance(room_W_4, roomLight, light, [biglightRoom_W_4, smalllightRoom_W_4], [lightRoom_W_4]).
propertyInstance(commonRoom_W, commonRoomLight, light, [biglightCommonRoom_W_1, biglightCommonRoom_W_2], [lightCommonRoom_W_1, lightCommonRoom_W_2]).

propertyInstance(room_W_1, nightLight, light, [biglightRoom_W_1, smalllightRoom_W_1], [lightRoom_W_1]).
propertyInstance(room_W_2, nightLight, light, [biglightRoom_W_2, smalllightRoom_W_2], [lightRoom_W_2]).
propertyInstance(room_W_3, nightLight, light, [biglightRoom_W_3, smalllightRoom_W_3], [lightRoom_W_3]).
propertyInstance(room_W_4, nightLight, light, [biglightRoom_W_4, smalllightRoom_W_4], [lightRoom_W_4]).



user(u1, [room_E_1, commonRoom_E, commonRoom_W]).
user(u2, [room_E_2, commonRoom_E, commonRoom_W]).
user(u3, [room_E_3, commonRoom_E, commonRoom_W]).
user(u4, [room_E_4, commonRoom_E, commonRoom_W]).

user(u5, [room_W_1, commonRoom_E, commonRoom_W]).
user(u6, [room_W_2, commonRoom_E, commonRoom_W]).
user(u7, [room_W_3, commonRoom_E, commonRoom_W]).
user(u8, [room_W_4, commonRoom_E, commonRoom_W]).



sensorValue(tempOdd_E,22).
sensorValue(tempEven_E,20).
sensorValue(tempCommonRoom_E,25).

sensorValue(lightRoom_E_1,100).
sensorValue(lightRoom_E_2,120).
sensorValue(lightRoom_E_3,200).
sensorValue(lightRoom_E_4,0).
sensorValue(lightCommonRoom_E_1,80).
sensorValue(lightCommonRoom_E_2,160).

sensorValue(tempOdd_W,24).
sensorValue(tempEven_W,24).
sensorValue(tempCommonRoom_W,24).

sensorValue(lightRoom_W_1,110).
sensorValue(lightRoom_W_2,120).
sensorValue(lightRoom_W_3,140).
sensorValue(lightRoom_W_4,150).
sensorValue(lightCommonRoom_W_1,160).
sensorValue(lightCommonRoom_W_2,170).



set(u1, room_E_1, roomLight, 0).
set(u1, room_E_1, roomTemp, 18).

set(u3, room_E_3, roomLight, 255).
set(u3, room_E_3, roomTemp, 28).


set(u4, room_E_2, roomLight, 0).
set(u4, room_E_2, roomTemp, 18).

set(u6, room_E_2, roomLight, 255).
set(u6, room_E_2, roomTemp, 28).


set(u2, commonRoom_E, commonRoomLight, 100).
set(u2, commonRoom_E, commonRoomTemp, 18).

set(u5, commonRoom_E, commonRoomLight, 100).
set(u5, commonRoom_E, commonRoomTemp, 23).

set(u8, commonRoom_E, commonRoomLight, 255).
set(u8, commonRoom_E, commonRoomTemp, 28).