:- module(t,['a@assert/0'/0]).

'a@assert/0'() :- 'a@a/3'([1,2,3], [4,5,6], XX27), XX27 = [1,2,3,4,5,6], 'a@p/2'("a", "b"), 'a@gp/2'("a", "d"), 'a@forallA1/2'(Xy, Xx).
'a@forallA1/2'(Xy, Xx) :- 'a@gp/2'(Xx, Xy), 'a@forallB1/2'(Xy, Xx), !, fail.
'a@forallA1/2'(Xy, Xx).
'a@forallB1/2'(Xy, Xx) :- 'a@m/1'(Xy), !, fail.
'a@forallB1/2'(Xy, Xx).
'a@a/3'([], XX, XX) :- !.
'a@a/3'([XE | XX], XY, [XE | XX11]) :- !, 'a@a/3'(XX, XY, XX11).
'a@a/3'(_, _, _) :- abort.
'a@p/2'("a", "b").
'a@p/2'("c", "b").
'a@p/2'("b", "d").
'a@p/2'("g", "d").
'a@m/1'("d").
'a@gp/2'(Xx, Xy) :- !, 'a@p/2'(Xx, Xz), 'a@p/2'(Xz, Xy).
'a@fred/1'("fred") :- !.