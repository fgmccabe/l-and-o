:- module(polyfill,[exit/1,'_int_plus'/3,'_int_minus'/3,'_int_times'/3,'_int_div'/3,
                    '_flt_plus'/3,'_flt_minus'/3,'_flt_times'/3,'_flt_div'/3,
                    '_int_abs'/2,'_flt_abs'/2,
                    '_unify'/2]).



exit(X) :- halt(X).

'_unify'(X,Y) :- unify_with_occurs_check(X,Y).

'_int_plus'(X,Y,Z) :- Z is X+Y.
'_int_minus'(X,Y,Z) :- Z is X-Y.
'_int_times'(X,Y,Z) :- Z is X*Y.
'_int_div'(X,Y,Z) :- Z is div(X,Y).
'_flt_plus'(X,Y,Z) :- Z is X+Y.
'_flt_minus'(X,Y,Z) :- Z is X-Y.
'_flt_times'(X,Y,Z) :- Z is X*Y.
'_flt_div'(X,Y,Z) :- Z is X/Y.
'_int_abs'(X,Y) :- Y is abs(X).
'_flt_abs'(X,Y) :- Y is abs(X).