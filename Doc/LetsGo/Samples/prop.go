/*
 * A propositional Theorem Prover, based on the old Wang tp in LISP 1.5!
 */
 
 (test)..{
   include "sys:go/stdlib.gof".
   include "sys:go/io.gof".
   
   form ::= letter(symbol) | impl(form,form) | not(form) | or(form,form) 
     | and(form,form) | equiv(form,form) | F | T.
     
   
   th1(a1,a2,[],c) => th2(a1,a2,[],[],c).
   th1(a1,a2,[A,..a],c) => disj(member(A,c),
                                (A=letter(_)?th1l(member(A,a1),

   thl(not(U),a1,a2,c1,c2) => th1r(U,a1,a2,c1,c2).
   thl(and(A,B),a1,a2,c1,c2) => th2l([A,B],a1,a2,c1,c2).
   thl(or(A,B),a1,a2,c1,c2) => conj(th1l(A,a1,a2,c1,c2),
                                    th1l(B,a1,a2,c1,c2)).
   thl(impl(A,B),a1,a2,c1,c2) => conj(th1l(B,a1,a2,c1,c2),
                                    th1r(A,a1,a2,c1,c2)).
   thl(equiv(A,B),a1,a2,c1,c2) => conj(th2l([A,B],a1,a2,c1,c2),
                                       th2r([A,B],a1,a2,c1,c2)).


   thr(not(U),a1,a2,c1,c2) => th1l(U,a1,a2,c1,c2).
   thr(and(A,B),a1,a2,c1,c2) => conj(th1r(A,a1,a2,c1,c2),
                                    th1r(B,a1,a2,c1,c2)).
   thr(or(A,B),a1,a2,c1,c2) => th2r([A,B],a1,a2,c1,c2).
   thr(impl(A,B),a1,a2,c1,c2) => th11([A,B],a1,a2,c1,c2).
   thr(equiv(A,B),a1,a2,c1,c2) => conj(th11([A,B],a1,a2,c1,c2),
                                       th11([B,A],a1,a2,c1,c2)).

   th1l(letter(X),a1,a2,c1,c2) => disj(member(letter(X),c1),
                                       th([letter(X),..a1],a2,c1,c2)).
   th1l(X,a1,a2,c1,c2) => disj(member(X,c2),
                               th(a1,[X,..a2],c1,c2)).
   th1r(letter(X),a1,a2,c1,c2) => disj(member(letter(X),a1),
                                       th(a1,a2,[letter(X),..c1],c2)).
   th1r(X,a1,a2,c1,c2) => disj(member(X,a2),th(a1,a2,c1,[X,..c2])).
   
   th2l([letter(X),Y],a1,a2,c1,c2) => disj(member(letter(X),c1),
                                          th1l(Y,[letter(X),..a1],a2,c1,c2)).
   th2l([X,Y],a1,a2,c1,c2) => disj(member(X,c2),
                                   th1l(Y,a1,[X,..a2],c1,c2)).
   th2r([letter(X),Y],a1,a2,c1,c2) => disj(member(letter(X),a1),
                                          th1r(Y,a1,a2,[letter(X),..c1],c2)).
   th2r([X,Y],a1,a2,c1,c2) => disj(member(X,a2),
                                   th1l(Y,a1,a2,c1,[X,..c2])).
                                          
   th11(letter(X),v2,a1,a2,c1,c2) => disj(member(letter(X),c1),
                                          th1r(v2,[letter(X),..a1],a2,c1,c2)).

   th11(v1,v2,a1,a2,c1,c2) => disj(member(v1,c2),
                                  th1r(v2,a1,[v1,..a2],c1,c2)).
                                  
   theorem(P,C) => th1([],[],P,C).
   
   conj(T,T) => T.
   conj(_,_) => F.
   
   disj(F,F) => F.
   disj(_,_) => T.
   
   member(_,[]) => F.
   member(X,[X,.._]) => T.
   member(X,[_,..Y]) => member(X,Y).
   
   
   