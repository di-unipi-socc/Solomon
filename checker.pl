wellDefinedPropertyInstances :-
    findall((ZId, PId, PropertyType, X, Type), illDefinedPropertyInstance(ZId, PId, PropertyType, X, Type), L),
    ((L==[], ansi_format([bold,fg(blue)],'### Properties instances are all well defined. ###################',[]), nl, true) ;
     (dif(L,[]), ansi_format([bold,fg(red)],'### Some properties instances are not well defined. ###################################################',[]), nl, xwrite(L), false)
    ).

illDefinedPropertyInstance(ZId, PId, PropertyType, X, Type):-
    propertyInstance(ZId, PId, PropertyType, ListOfActuators, ListOfSensors),
    ( \+ propertyType(PropertyType) 
    ;
    ((member(X, ListOfActuators), ((actuator(X,Type), dif(Type,PropertyType)); \+ actuator(X,Type)))
     ;
     (member(X, ListOfSensors), ((sensor(X,Type), dif(Type,PropertyType)); \+ sensor(X,Type))))
    ).

xwrite([]) :- ansi_format([bold,fg(red)],'#######################################################################################################',[]), nl.

xwrite([(ZId, PId, PropertyType, X, Type)|L]) :-
    \+ var(Type),
    write('- '),ansi_format([bold], X, []), write(' in '), ansi_format([bold], '~w:~w', [ZId,PId]), ansi_format([fg(red)], ' has wrong type ', []),ansi_format([bold,fg(red)], Type, []), 
    write(' [expected type: '),ansi_format([bold,fg(blue)], PropertyType, []),write(']'),nl,nl,
    xwrite(L).

xwrite([(ZId, PId, PropertyType, X, Type)|L]) :-
    var(Type),
    ( 
        (propertyType(PropertyType), write('- '), ansi_format([bold], X, []), write(' in '), ansi_format([bold], '~w:~w', [ZId,PId]), ansi_format([fg(red)], ' does not exist!', []) )
        ;
        ( write('- PropertyType '), ansi_format([bold], PropertyType, []), write(' in '), ansi_format([bold], '~w:~w', [ZId,PId]), ansi_format([fg(red)], ' does not exist!', []) ) 
    ),
    nl,nl,
    xwrite(L).


:- wellDefinedPropertyInstances.