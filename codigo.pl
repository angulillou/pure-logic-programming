
%----------------------PREDICADOS AUXILIARES---------------------------------------------

nat(0).
nat(s(X)) :- nat(X).

mayor(s(_),0).
mayor(s(X), s(Y)):-
	mayor(X, Y).

menor(0, s(X)) :- nat(X).
menor(s(X), s(Y)) :- menor(X, Y).

len([],0).
len([_|L],N) :- 
	suma(N1,s(0),N),
	len(L,N1).

suma(X,0,X).
suma(X,s(Y),s(Z)):- suma(X,Y,Z).

resta(X, 0, X).
resta(X, s(Y), Z) :- resta(X, Y, s(Z)).

producto(0, _ , 0).
producto(s(X), Y, Z2) :- 
	producto(X, Y, Z),
	suma(Z, Y, Z2).

division(X, Y, 0) :- menor(X, Y).
division(X, Y, s(Z)) :-
	resta(X, Y, R), 
	division(R, Y, Z).

equal_lengths([]).
equal_lengths([[]]).
equal_lengths([[_|_]]).
equal_lengths([X,Y|Rest]) :- 
  len(X, Len), 
  len(Y, Len), 
  equal_lengths([Y|Rest]).

size([], 0, _).
size([X|Xs], F, C) :-
        len(X, C),
        len([X|Xs], F).

%------------------------------------------------------------------------------------------

%-Comprueba que el cada nivel tiene como minimo un vivienda
esNivel([V]):-
 nat(V).
esNivel([V|L]):-
nat(V),
esNivel(L).

%-comprueba que cada edificio tiene como minimo un nivel y una vivienda en cada nivel
basic_building([V]):-
	esNivel(V).

basic_building([N|L]):-
	esNivel(N),
	basic_building(L).

%DEFINIR TIPO: X es un edificio como el anterior pero además todos los niveles deben tener el mismo número de viviendas

building(X) :-
	basic_building(X),
	equal_lengths(X).

%DEFINIR PREDICADO: C es el nivel N-esimo del edificio X (la lista con todas las viviendas de ese nivel)

level(X,N,C) :-
	building(X),
	nlevel(X,N,C).

nlevel([X|_],s(0),C) :-
	C = X.

nlevel([_|L],N,C) :-
	nat(N),
	mayor(N,s(0)),
	resta(N,s(0),N1),
	level(L,N1,C).
 

%DEFINIR PREDICADO: C es la lista formada por las viviendas N-esimas de todos niveles del edificio X
column(X,N,C):-
	building(X),
	ncolumn(X,N,C).

ncolumn([],_,[]).
ncolumn([X|L], N, [R|C]):-   
	nlevel(X, N, R), 
	ncolumn(L,N,C).	

%DEFINIR PREDICADO: C es la lista del las columnas de viviendas del edificio X
columns(X,C):-
	building(X),
	tcolumns(X,C).	

tcolumns([[]|_],[]).
tcolumns(X, [F|F2]) :- 
	prim_col(X, F, RestoEdif),
        tcolumns(RestoEdif, F2).
prim_col([],[],[]).
prim_col([[L|R]|F], [L|H], [R|R1]) :- prim_col(F, H, R1).

%DEFINIR PREDICADO: T es el numero total de personas que viven en el edificio X
%total_people(X,T) :-
total_people(X,T) :-
	building(X),
	total(X,T).

plusList([],0).
plusList([X|Xs],S) :- 
	plusList(Xs,SXs), suma(SXs,X,S).

total([],0).
total([X|Xs],T):-
	total(Xs,Z),
	plusList(X,K),
	suma(Z,K,T).

%DEFINIR PREDICADO: A es es la media de personas que viven en cada vivienda del edificio X, redondeada al numero natural mas cercano

average(X,A):-
	building(X),
	media(X,A).

media(X,Media):- 
	total(X,T), %total de personas que viven el en edifiico
	size(X,F,C), %calcula numero de filas y columnas
	producto(F,C,V),%total viviendas edificio
	division(T, V, Media).
