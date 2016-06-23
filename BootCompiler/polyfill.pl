:- module(polyfill,[exit/1,'_int_plus'/3,'_int_minus'/3,'_int_times'/3,'_int_div'/3,
                    '_flt_plus'/3,'_flt_minus'/3,'_flt_times'/3,'_flt_div'/3,
                    '_int_abs'/2,'_flt_abs'/2,
                    explode/2, implode/2,
                    '_unify'/2,
                    '_isCcChar'/1,'_isCfChar'/1,'_isCnChar'/1,'_isCoChar'/1,'_isCsChar'/1,
                    '_isLlChar'/1,'_isLmChar'/1,'_isLoChar'/1,'_isLtChar'/1,'_isLuChar'/1,
                    '_isMcChar'/1,'_isMeChar'/1,'_isMnChar'/1,'_isNdChar'/1,'_isNlChar'/1,
                    '_isNoChar'/1,'_isPcChar'/1,'_isPdChar'/1,'_isPeChar'/1,'_isPfChar'/1,
                    '_isPiChar'/1,'_isPoChar'/1,'_isPsChar'/1,'_isScChar'/1,'_isSkChar'/1,
                    '_isSmChar'/1,'_isSoChar'/1,'_isZlChar'/1,'_isZpChar'/1,'_isZsChar'/1,
                    '_isLetterChar'/1,
                    '_int2str'/5,
                    '_flt2str'/6
                    ]).



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

explode(S,C) :- string_codes(S,CC), listify(CC,C).

listify([],'lo.list#[]').
listify([E|L],'lo.list#,..'(E,LL)) :- listify(L,LL).

implode(C,S) :- string_codes(S,C).

/* Unicode character class */

'_isCcChar'(X) :- unicode_property(X,category('Cc')). % is Other, control char
'_isCfChar'(X) :- unicode_property(X,category('Cf')). % is Other, format char
'_isCnChar'(X) :- unicode_property(X,category('Cn')). % is Other, unassigned char
'_isCoChar'(X) :- unicode_property(X,category('Co')). % is Other, private char
'_isCsChar'(X) :- unicode_property(X,category('Cs')). % is Other, surrogate char
'_isLlChar'(X) :- unicode_property(X,category('Ll')). % is Letter, lowercase char
'_isLmChar'(X) :- unicode_property(X,category('Lm')). % is Letter, modifier char
'_isLoChar'(X) :- unicode_property(X,category('Lo')). % is Letter, other char
'_isLtChar'(X) :- unicode_property(X,category('Lt')). % is Letter, title char
'_isLuChar'(X) :- unicode_property(X,category('Lu')). % is Letter, uppercase char
'_isMcChar'(X) :- unicode_property(X,category('Mc')). % is Mark, spacing char
'_isMeChar'(X) :- unicode_property(X,category('Me')). % is Mark, enclosing char
'_isMnChar'(X) :- unicode_property(X,category('Mn')). % is Mark, nonspacing char
'_isNdChar'(X) :- unicode_property(X,category('Nd')). % is Number, decimal digit
'_isNlChar'(X) :- unicode_property(X,category('Nl')). % is Number, letter char
'_isNoChar'(X) :- unicode_property(X,category('No')). % is Number, other char
'_isPcChar'(X) :- unicode_property(X,category('Pc')). % is Punctuation, connector
'_isPdChar'(X) :- unicode_property(X,category('Pd')). % is Punctuation, dash char
'_isPeChar'(X) :- unicode_property(X,category('Pe')). % is Punctuation, close char
'_isPfChar'(X) :- unicode_property(X,category('Pf')). % is Punctuation, final quote
'_isPiChar'(X) :- unicode_property(X,category('Pi')). % is Punctuation, initial quote
'_isPoChar'(X) :- unicode_property(X,category('Po')). % is Punctuation, other char
'_isPsChar'(X) :- unicode_property(X,category('Ps')). % is Punctuation, open char
'_isScChar'(X) :- unicode_property(X,category('Sc')). % is Symbol, currency char
'_isSkChar'(X) :- unicode_property(X,category('Sk')). % is Symbol, modifier char
'_isSmChar'(X) :- unicode_property(X,category('Sm')). % is Symbol, math char
'_isSoChar'(X) :- unicode_property(X,category('So')). % is Symbol, other char
'_isZlChar'(X) :- unicode_property(X,category('Zl')). % is Separator, line char
'_isZpChar'(X) :- unicode_property(X,category('Zp')). % is Separator, para char
'_isZsChar'(X) :- unicode_property(X,category('Zs')). % is Separator, space char

'_isLetterChar'(X) :- unicode_property(X,category('Le')). % is letter char

'_int2str'(Ix,_,_,_,Str) :- number_string(Ix,Str).
'_flt2str'(Dx,_,_,_,_,Str) :- number_string(Dx,Str).

