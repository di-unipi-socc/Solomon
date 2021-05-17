# Solomon

Solomon is a declarative open-source framework to represent smart environments, user-set goals and customisable mediation policies to reconcile contrasting (user and/or global) goals and relative actuator settings, encompassing multiple IoT systems.

## Background & Requirements

Solomon is written in Prolog. Prolog is a logic programming language as it is based on first-order logic. A Prolog program is a finite set of *clauses* of the form

```prolog
a :- b1, ... , bn.
```

stating  that `a` holds  when `b1` and ··· and `bn` holds,  where `n >= 0` and `a`, `b1` ..., `bn` are atomic literals. Clauses can also contain inclusive disjunctions (i.e. logic ORs) among literals `bi` and `bj`, represented by `bi; bj`. Clauses with empty condition are also called facts. Prolog variables begin with upper-case letters, lists are denoted by square brackets, and negation by `\+`.

To run Solomon, please install [SWI-Prolog](https://www.swi-prolog.org/Download.html).

## Why Solomon?

In Solomon we distinguish two different types of conflict that can arise:

1. Different users can set different goals on their desired state of the environment (e.g. on target temperature), 
2. The Admin can set global objectives that must be met (e.g. on maximum energy consumption, on law constraints), which may conflict with the user-set goals.
   
Even after reconciling the previous types of conflicts into one target state satisfying all set (user and/or global) goals, a final configuration of the actuators involved must also be determined. Indeed, given a final target state, we need to:

* Determine the correct configuration for each actuator acting on that state, and 
* Mediate between any conflicting configurations that a single actuator possibly receives.

Solomon tames the effects of the aforementioned types of conflict by allowing to flexibly specify *ad-hoc* mediation policies for distinct zones of a smart environment and possible conflicting settings of target actuators. Such policies can resolve conflicts among users' goals, among users' and system administrator's goals, and on actuators configuration. Last, but not least, the declarative nature of Solomon makes it easy to write, maintain and extend arbitrary mediation policies encompassing multiple IoT verticals.

## Quickstart Example

### The Model

#### Model the Smart Home

To model smart environments, we first build up a dictionary of all types of environmental parameters (propertyTypes) we can monitor (via sensors) and/or act upon (via actuators). Given a propertyType we can then define actuators and sensors that sense or operate on that. 
```prolog
propertyType(light).
propertyType(temp).

sensor(brightness,  light).
sensor(temperature, temp).
sensorValue(brightness, 20).
sensorValue(temperature, 22).

actuator(smallLight, light).
actuator(mainLight, light).
actuator(cornerLight, light).
actuator(ac, temp).
```

System administrators can divide smart environments into different zones, which allow distinguishing which global policy to apply to specific sets of sensors and actuators.  A zone groups one or more propertyIinstances, defining a set of actuators and a set of sensors that operate on a specific property type.
```prolog
zone(livingroom, _).

propertyInstance(livingroom, studyingLight, light, [cornerLight, mainLight], [brightness]).
propertyInstance(livingroom, movieLight, light, [cornerLight, smallLight], [brightness]).
propertyInstance(livingroom, readingLight, light, [smallLight], [brightness]).
propertyInstance(livingroom, roomTemp, temp, [ac], [temperature]).
```

#### Model Users and Goals

Given a user, we define the zones in which she\he is authorized to set goals.

```prolog
user(alice, [livingroom]).
set(alice, livingroom, movieLight, 20).
set(alice, livingroom, roomTemp, 20).

user(bob, [livingroom]).
set(bob, livingroom, studyingLight, 80).
set(bob, livingroom, roomTemp, 26).
```

Finally through `mediateRequests/2` and `associateActions/2` the admin can:
* mediates requests referring to the same propertyInstance so to determine a target state by solving all user-user and user-admin conflicts (`mediateRequests/2`),
* determines actions (i.e. settings) for individual IoT actuators so to achieve the target state, by also resolving possible conflicting actions found for a single actuator (`associateActions/2`).

It is also possible to checks that inputs and outputs of each phase are well-formed, through predicates `validMediation/1`, and `validActions/1`.

### Running Solomon

To try to perform mediation with the model described above (`SmartHome`), simply query the predicate, in `Examples/SmartHome/policies.pl`:

```prolog
?- react(Requests, MediatedRequests, Actions).

Requests = [(livingroom, movieLight, 20, alice),  (livingroom, studyingLight, 80, bob),  (livingroom, roomTemp, 20, alice),  (livingroom, roomTemp, 26, bob)],
MediatedRequests = [(livingroom, movieLight, 20),  (livingroom, readingLight, 0),  (livingroom, roomTemp, 23),  (livingroom, studyingLight, 80)],
Actions = [(ac, 23),  (cornerLight, 40),  (mainLight, 40),  (smallLight, 10)].
```
where `Requests` is the list of all users' requests, `MediatedRequests` the list containing the target states for each propertyInstance and `Actions` is the list of the actions to perform to reach the final target state.
