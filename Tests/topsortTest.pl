
:- use_module(topsort).

/* Separate groups */

a1((["one"],[],"one")).
a2((["two"],[],"two")).
a3((["three"],[],"three")).
a4((["four"],[],"four")).

sa([A1,A2,A3,A4]) :- a1(A1), a2(A2), a3(A3), a4(A4).

tsa(Groups) :- sa(Defs), topsort(Defs,Groups), writef("result groups: %w\n",[Groups]).

/* One big group */
b1((["1"],["2"],"1")).
b2((["2"],["4"],"2")).
b3((["3"],["1"],"3")).
b4((["4"],["3"],"4")).

sb([B1,B2,B3,B4]) :- b1(B1), b2(B2), b3(B3), b4(B4).

tsb(Groups) :- sb(Defs), topsort(Defs,Groups), writef("result groups: %w\n",[Groups]).

/* A group with a tail */
c1((["alpha"],["beta"],"alpha")).
c2((["beta"],["gamma"],"beta")).
c3((["gamma"],["alpha"],"gamma")).
c4((["delta"],["gamma"],"delta")).

sc([B1,B2,B3,B4]) :- c1(B1), c2(B2), c3(B3), c4(B4).

tsc(Groups) :- sc(Defs), topsort(Defs,Groups), writef("result groups: %w\n",[Groups]).
