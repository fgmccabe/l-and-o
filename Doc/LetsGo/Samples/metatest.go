metatest{
  import meta.
  import parent.
  import go.io.

  Parent = $parent(_,_)


main..{
  include "sys:go/stdlib.gof".
  include "sys:go/io.gof".
  include "meta.gof".
  include "dynclauses.gof".
  include "meta.gh".
  
  callterm::= 	male(symbol) | female(symbol) | 
  		parent(symbol,symbol) | age(symbol,number) |
  		ancestor(symbol,symbol) | likes(symbol,symbol).
  				
  Male = $dynamic_facts[callterm]([male('k'),male('t'),male('f'),male('st')]). 
  					
  Female =$dynamic_facts[callterm]([female('s'),female('m'),female('a')]).
  
  Parent = $dynamic_facts[callterm]([
  		parent('am','k'),parent('l','k'),parent('k','t'),parent('s','t'),
  		parent('k','a'),parent('s','a'),
  		parent('f','st'),parent('m','st')]).
  			
  Ancestor= $dynamic_clauses[callterm]([
                cl(ancestor(A,D),is(parent(A,D))),
  		cl(ancestor(A,D), conj([is(parent(P,D)),is(ancestor(A,P))]))]).

  Age = $dynamic_facts[callterm]([ 
  	        age('am',85),age('l',88),age('k',50),age('s',48),age('f',43),
  	        age('m',40),age('a',4),age('t',6),age('st',1)]).
  	
  Likes= $dynamic_clauses[callterm]([
                cl(likes('k',P),is(parent('k',P))),
  		cl(likes(P,'a'),TRUE)]).

  -- The evalc class maps the 'surface' predicate term to an actual evaluation
  
  axioms[] <~ clauses[callterm].
  axioms(){
    cls(male(P),Cont):--Male.cls(male(P),Cont).
    cls(female(P),Cont):--Female.cls(female(P),Cont).
    cls(parent(P,C),Cont):--Parent.cls(parent(P,C),Cont).
    cls(age(P,Ag),Cont):--Age.cls(age(P,Ag),Cont).
    cls(ancestor(A,D),Cont):--Ancestor.cls(ancestor(A,D),Cont).
    cls(likes(P1,P2),Cont):--  Likes.cls(likes(P1,P2),Cont).
  
    add(male(X)) -> Male.add(male(X)).
    add(female(X)) -> Female.add(female(X)).
    add(parent(X,Y)) -> Parent.add(parent(X,Y)).
  
    del(male(X)) -> Male.del(male(X)).
    del(female(X)) -> Female.del(female(X)).
    del(parent(X,Y)) -> Parent.del(parent(X,Y)).

    delcl(cl(likes(X,Y),Bdy)) -> Likes.del(cl(likes(X,Y),Bdy)).
  }.

  ask(aX,S,Q) -> stdout.outLine("Query is:"<>S^0<>" such that "<>Q^0);
      (evaluate(aX,Q) ?
         stdout.outLine("Answer is:"<>S^0)
     | stdout.outLine("Failed")).
		
  askall(aX,S,Q) -> stdout.outLine("Query is: all "<>S^0<>" such that "<>Q^0);
      ( evaluate(aX,Q) *>
        stdout.outLine("Answer is:"<>S^0)); -- {__break("")});
      stdout.outLine("no (more) answers").
		
  main()-> 
      aX = $axioms();
      askall(aX,P0,is(male(P0)));
      ask(aX,P,disj([is(male(P)),is(female(P))]));
      ask(aX,P1,conj([is(male(P1)),is(female(P1))]));
      askall(aX,P2,disj([is(male(P2)),is(female(P2))]));
      ask(aX,(M,A),conj([is(male(M)),is(age(M,A)),isnot(parent(M,_))]));
      askall(aX,An,is(ancestor('am',An))).
}