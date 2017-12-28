:-module(wff,[isAlgebraicTypeDef/6,isQuantified/3,getQuantifiers/3,isConstrained/3,
    isContractSpec/6,packageName/2,pkgName/2,sameLength/3,deComma/2,tupleize/4]).
:-use_module(abstract).
:-use_module(misc).
:-use_module(errors).

isAlgebraicTypeDef(Stmt,Lc,Quants,Constraints,Head,Body) :-
  isUnary(Stmt,_,"type",Term),
  isAlgebraicTypeDef(Term,Lc,Quants,Constraints,Head,Body).
isAlgebraicTypeDef(Stmt,Lc,Quants,Constraints,Head,Body) :-
  locOfAst(Stmt,Lc),
  getQuantifiers(Stmt,Quants,Inner),
  isConstrained(Inner,Constraints,TpStmt),
  isBinary(TpStmt,"::=",Head,Body).

isAlgebraicTypeDef(Term) :- isAlgebraicTypeDef(Term,_,_,_,_,_).

isQuantified(T,V,B) :- isBinary(T,"~~",L,B), isUnary(L,"all",V).

getQuantifiers(T,LV,B) :- isQuantified(T,V,B), deComma(V,LV).
getQuantifiers(T,[],T).

isContractSpec(St,Lc,Quants,Constraints,Con,Body) :-
  isUnary(St,Lc,"contract",I),
  isBinary(I,"::=",L,R),
  isBraceTuple(R,_,Body),
  contractSpec(L,Quants,Constraints,Con).

contractSpec(S,Quants,Constraints,Con) :-
  isQuantified(S,V,B), deComma(V,Quants),
  contractSpec(B,_,Constraints,Con).
contractSpec(S,[],Constraints,Con) :-
  isBinary(S,"|:",L,Con),
  deComma(L,Constraints).
contractSpec(S,[],[],S).

isConstrained(T,C,R) :-
  isBinary(T,"|:",L,R),
  deComma(L,C).
isConstrained(T,[],T).

deComma(T,LL) :-
  isBinary(T,",",L,R),
  deComma(L,Lf),
  deComma(R,Rf),
  concat(Lf,Rf,LL).
deComma(T,[T]).

packageName(T,Pkg) :- isIden(T,Pkg).
packageName(T,Pkg) :- isString(T,Pkg).
packageName(T,Pkg) :- isBinary(T,".",L,R),
  packageName(L,LP),
  packageName(R,RP),
  string_concat(LP,".",I),
  string_concat(I,RP,Pkg).

pkgName(T,pkg(Pkg,v(Version))) :-
  isBinary(T,"#",L,R),
  packageName(L,Pkg),
  packageVersion(R,Version).
pkgName(T,pkg(Pkg,defltVersion)) :-
  packageName(T,Pkg).

packageVersion(T,Pkg) :- isIden(T,Pkg).
packageVersion(T,Pkg) :- isString(T,Pkg).
packageVersion(integer(_,Ix),Pkg) :- atom_string(Ix,Pkg).
packageVersion(T,Pkg) :- isBinary(T,".",L,R),
  packageVersion(L,LP),
  packageVersion(R,RP),
  string_concat(LP,".",I),
  string_concat(I,RP,Pkg).

sameLength(L1,L2,_) :- length(L1,L), length(L2,L),!.
sameLength(L1,_,Lc) :-
  length(L1,L),
  reportError("expecting %s elements",[L],Lc).

tupleize(app(_,name(_,","),tuple(_,"()",[L,R])), Lc, Op, tuple(Lc,Op,[L|Rest])) :-
    getTupleArgs(R,Rest).
tupleize(T,Lc,Op,tuple(Lc,Op,[T])).

getTupleArgs(app(_,name(_,","),tuple(_,"()",[L,R])), [L|Rest]) :-
    getTupleArgs(R,Rest).
getTupleArgs(T,[T]).
