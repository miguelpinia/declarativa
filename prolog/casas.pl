/*
 * Dada una lista de elementos y dos elementos Vecino_Izq, Vecino_Der,
 * unifica si X y Y son vecinos, en otro caso, no unifica.
 */
vecino_izquierdo([Izq|[Der|Vecinos]],Vecino_Izq,Vecino_Der):-
    (Izq = Vecino_Izq, Der = Vecino_Der);
    vecino_izquierdo([Der|Vecinos],Vecino_Izq,Vecino_Der).

/*
 * Dada una lista de elementos y dos elementos de esa lista, el
 * predicado unifica si esos dos elementos son vecinos entre sí.
 */
vecino(Vecinos, Vecino_1, Vecino_2):-
    vecino_izquierdo(Vecinos, Vecino_1, Vecino_2);
    vecino_izquierdo(Vecinos, Vecino_2, Vecino_1).

/*
 * Representa el estado de las casa de acuerdo a los predicados que
 * numeran las casas.
 */
casitas([casa(1, _, noruego, _, _, _),
         casa(2, azul, _, _, _, _),
         casa(3, _, _, leche, _, _),
         casa(4, _, _, _, _, _),
         casa(5, _, _, _, _, _)]).

/*
 * Construye el estado de las casas a partir de los predicados que
 * satisfacen la combinación de las casas.
 */
casas(Casas) :-
    casitas(Casas),
    member(casa(_, roja, ingles, _, _, _), Casas),
    member(casa(_, _, sueco, _, perro, _), Casas),
    member(casa(_, _, danes, te, _, _), Casas),
    vecino_izquierdo(Casas, casa(_, verde, _, _, _, _), casa(_, blanca, _, _, _, _)),
    member(casa(_, verde, _, cafe, _, _), Casas),
    member(casa(_, _, _, _, pajaros, pallmall), Casas),
    member(casa(_, amarilla, _, _, _, dunhill), Casas),
    vecino(Casas, casa(_, _, _, _, _, blend), casa(_, _, _, _, gatos, _)),
    vecino(Casas, casa(_, _, _, _, caballos, _), casa(_, _, _, _, _, dunhill)),
    member(casa(_, _, _, cerveza, _, bluemaster), Casas),
    member(casa(_, _, aleman, _, _, prince), Casas),
    vecino(Casas, casa(_, _, _, _, _, blend), casa(_, _, _, agua, _, _)),
    member(casa(_, _, _, _, peces, _), Casas), !.

/*
 * Dado el nombre de una mascota, unifica con el nombre de su dueño a
 * partir de la construcción del predicado de casas.
 */
mascota(Mascota,Persona):-
    casas(Casas),
    member(casa(_, _, Persona, _, Mascota, _), Casas).

/*
 * Pregunta quien es el dueño de los peces.
 */
dueno_peces(Persona) :-
    mascota(peces, Persona), !.
