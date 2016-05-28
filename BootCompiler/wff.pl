:-module(wff,[wffModule/2]).
:-use_module(errors).
:-use_module(abstract).
:-use_module(misc).
:-use_module(keywords).

wffModule(Fl,Term) :- 
    isBraceTerm(Term,Mod,Els),!,
    wwfModuleName(Fl,Mod),
    wffThetaEnv(Els).
wffModule(_,Term) :-
    locOfAst(Term,Lc),
    reportError("Invalid module: %w",[Term],Lc).

extractPackageName(Fl,Name) :-
    (string_concat(Prefix,".gol",Fl), segment(Prefix,'/',Segs),last(Segs,Name);
      segment(Fl,'/',Segs),last(Segs,Name)).

verifyModuleName(Fl,Name) :-
  (string_concat(Prefix,".gol",Fl), segment(Prefix,"/",FlSegs) ; segment(Fl,"/",FlSegs)),
  reverse(FlSegs,Segs),
  verifyName(Name,Segs).

verifyName(Nm,[Seg|Rest]) :- isBinary(Nm,".",L,name(_,Seg)), verifyName(L,Rest).
verifyName(name(_,Seg),[Seg|_]).

wwfModuleName(Fl,Mod) :- verifyModuleName(Fl,Mod).

wffPackageName(Term) :- isBinary(Term,".",L,R), isName(L,_), wffPackageName(R).
wffPackageName(Term) :- isName(Term,_).

wffThetaEnv([]).
wffThetaEnv([Pri|Stmts]) :- 
    isPrivate(Pri,El,_),
    wffThetaEnv([El|Stmts]).
wffThetaEnv([St|Stmts]) :-
    wffStmt(St),
    wffThetaEnv(Stmts).

wffStmt(St) :-
  isAlgebraicTypeDef(St).
wffStmt(St) :-
  isTypeAnnotation(St).
wffStmt(St) :-
  wffFunctionDefn(St).
wffStmt(St) :- wffClause(St).
wffStmt(St) :- locOfAst(St,Lc), reportError("Cannot understand %w",[St],Lc).

wffTypeExp(name(_,_)).
wffTypeExp(T) :- isSquare(T,_,A), wffTypeExps(A).
wffTypeExp(T) :- isBinary(T,"=>",L,R), isTuple(L,A), wffTypeExps(A), wffTypeExp(R).
wffTypeExp(T) :- braceTuple(T,_,A), wffFaceTypes(A).

wffFaceTypes([]).
wffFaceTypes([F|A]) :- isTypeAnnotation(F), wffFaceTypes(A).

wffTypeExps([]).
wffTypeExps([T|L]) :- wffTypeExp(T), wffTypeExps(L).

convertTypeDef(St,[InheritThing,InheritShow|Elements],Tail) :-
  isAlgebraicTypeDef(St,Lc,Head,Body),
  binary(Lc,"<~",Head,name(Lc,"showable"),InheritShow),
  binary(Lc,"<~",Head,name(Lc,"thing"),InheritThing),
  convertConstructors(Body,Head,Elements,Tail).

convertConstructors(Pair,Head,Elements,Tail) :-
  isBinary(Pair,"|",L,R),
  convertConstructors(L,Head,Elements,L1),
  convertConstructors(R,Head,L1,Tail).
convertConstructors(Term,Head,Elements,Tail) :-
  convertConstructor(Term,Head,Elements,Tail).

convertConstructor(name(Lc,Nm),Tp,[TpRule,InheritRule,Body|Tail],Tail) :-
  binary(Lc,":",name(Lc,Nm),Tp,TpRule),
  binary(Lc,"<=",name(Lc,Nm),name(Lc,"thing"),InheritRule), /* Inherit from thing */

  unary(Lc,"dS",string(Lc,Nm),StrValue),
  roundTerm(Lc,"display",[name(Lc,"_")],Hd),
  binary(Lc,"=>",Hd,StrValue,ShowFun),

  braceTuple(Content,Lc,[ShowFun]),
  binary(Lc,"..",name(Lc,Nm),Content,Body).
convertConstructor(Con,Tp,[TpRule,InheritRule,Body|Tail],Tail) :-
  isRound(Con,Nm,ArgTypes), /* Construct con:(T1,..,Tn)=>Tp */
  funType(Lc,ArgTypes,Tp,FT),
  binary(Lc,name(Lc,Nm),FT,TpRule),
  PrVar = name(Lc,"__0"),       /* Construct con(__1,..,__n) <= thing */
  genArgShow(Lc,1,PrVar,ArgTypes,Args,Exps),
  roundTerm(Lc,Nm,Args,ConHead),
  binary(Lc,"<=",ConHead,name(Lc,"thing"),InheritRule),
  unary(Lc,"dS",")",Close),    /* Construct con(__1,..,__n) .. { disp(Pr) => ...__i.disp(Pr)... } */
  binary(Lc,",..",Close,name(Lc,"[]"),Term),
  assembleDisplayArgs(Lc,Term,Exps,ArgDisps),
  unary(Lc,"dS","(",Open),
  binary(Lc,",..",Open,ArgDisps,AD1),
  unary(Lc,"dS",Nm,NmD),
  binary(Lc,",..",NmD,AD1,DispArgs),
  unary(Lc,"dSeq",DispArgs,Result),
  roundTerm(Lc,"display",[PrVar],Hd),
  binary(Lc,"=>",Hd,Result,DispFun),
  braceTuple(Lc,[DispFun],Content),
  binary(Lc,"..",ConHead,Content,Body).

genArgShow(_,_,_,[],[],[]).
genArgShow(Lc,No,Pr,[_|Tps],[V|Args],[ShowV|Exps]) :-
  genVar(Lc,No,V,NxNo),
  roundTerm(Lc,"display",[Pr],ShCall),
  binary(Lc,".",V,ShCall,ShowV),
  genArgShow(Lc,NxNo,Pr,Tps,Args,Exps).

genVar(Lc,No,name(Lc,Name),NxNo) :-
  number_string(NN,No),
  string_concat("__",NN,Name),
  NxNo is No+1.

assembleDisplayArgs(_,Term,[],Term).
assembleDisplayArgs(Lc,Term,[Exp|Exps],Disp) :-
  assembleDisplayArgs(Lc,Term,Exps,DispExp),
  unary(Lc,"dS",",",Comma),
  binary(Lc,",..",Comma,DispExp,E1),
  binary(Lc,",..",Exp,E1,Disp).

hasType(Lc,Nm,Tp,St) :- binary(Lc,":",name(Lc,Nm),Tp,St).

funType(Lc,Args,Res,Tp) :- binary(Lc,"=>",tuple(Lc,Args),Res,Tp).
genericType(Lc,Nm,Args,Tp) :- squareTerm(Lc,Nm,Args,Tp).

thingType(name(Lc,"thing"),Lc).

showType(Lc,FT) :-
  funType(Lc,[name(Lc,"integer")],name(Lc,"SString"),FT).

isPrivate(Term,T,Lc) :- isUnary(Term,"private",T), locOfAst(T,Lc).

isAlgebraicTypeDef(Term,Lc,Head,Body) :- isBinary(Term,"::=",Head,Body), locOfAst(Term,Lc).
isAlgebraicTypeDef(Term) :- isAlgebraicTypeDef(Term,_,_,_).

isTypeAnnotation(Term) :-
  isBinary(Term,":",L,R),
  (wffIden(L) ; isRound(L,ClNm,Args), \+isKeyword(ClNm), wffPtns(Args)),
  wffTypeExp(R).

wffFunctionDefn(Term) :-
  isBinary(Term,"=>",Hd,Result),
  isBinary(Hd,"::",Head,Cond),
  isRound(Head,Fn,Args),
  \+isKeyword(Fn),
  wffPtns(Args),
  wffCond(Cond),
  wffExp(Result).
wffFunctionDefn(Term) :-
  isBinary(Term,"=>",Head,Result),
  isRound(Head,Fn,Args),
  \+isKeyword(Fn),
  wffPtns(Args),
  wffExp(Result).

wffClause(Term) :- 
  isBinary(Term,":-",Head,Body),
  isRound(Head,Op,Args),
  \+ isKeyword(Op),
  wffPtns(Args),
  wffCond(Body).
wffClause(Term) :-
  isRound(Term,Op,Args),
  \+ isKeyword(Op),
  wffPtns(Args).

wffExps([]).
wffExps([P|T]) :- wffExp(P), wffExps(T).

wffExp(name(_,K)) :- \+isKeyword(K).
wffExp(name(Lc,K)) :- isKeyword(K), reportError("unexpected keyword: %s",[K],Lc).
wffExp(integer(_,_)).
wffExp(long(_,_)).
wffExp(float(_,_)).
wffExp(string(_,_)).
wffExp(interString(_,Segs)) :- wffStringSegments(Segs).
wffExp(tuple(_,"()",A)) :- wffExps(A).
wffExp(tuple(_,"[]",A)) :- wffExps(A).
wffExp(tuple(_,"{}",A)) :- wffThetaEnv(A).
wffExp(T) :- isBinary(T,":",L,R), wffExp(L), wffTypeExp(R).
wffExp(T) :- isBinary(T,".",L,R), wffExp(L), wffExp(R).
wffExp(T) :- isBinary(T,"#",L,R), wffPackageName(L), wffExp(R).
wffExp(T) :- isBinary(T,"|",L,R), isBinary(L,"?",Tst,Th), wffCond(Tst), wffExp(Th), wffExp(R).
wffExp(T) :- isBinary(T,"=>",L,R), wffExp(L), wffExp(R).
wffExp(T) :- isBinary(T,":-",L,R), wffExp(L), wffExp(R).
wffExp(T) :- isBinary(T,"+",L,R), wffExp(L), wffExp(R).
wffExp(T) :- isBinary(T,"-",L,R), wffExp(L), wffExp(R).
wffExp(T) :- isBinary(T,"*",L,R), wffExp(L), wffExp(R).
wffExp(T) :- isBinary(T,"/",L,R), wffExp(L), wffExp(R).
wffExp(T) :- isBinary(T,"%",L,R), wffExp(L), wffExp(R).
wffExp(T) :- isRoundTerm(T,Op,Args), wffExp(Op), wffExps(Args).
wffExp(T) :- locOfAst(T,Lc), reportError("expression %w not well formed",[T],Lc).

wffStringSegments([]).
wffStringSegments([S|M]) :- wffStringSegment(S), wffStringSegments(M).

wffStringSegment(string(_,_)).
wffStringSegment(display(_,T)) :- wffExp(T).
wffStringSegment(format(_,T,_)) :- wffExp(T).

wffCond(name(_,"true")).
wffCond(name(_,"false")).
wffCond(C) :- isBinary(C,",",L,R), wffCond(L), wffCond(R).
wffCond(C) :- isBinary(C,"|",L,R), isBinary(L,"?",T,Th), wffCond(T),wffCond(Th),wffCond(R).
wffCond(C) :- isBinary(C,"|",L,R), wffCond(L), wffCond(R).
wffCond(C) :- isUnary(C,"!",R), wffCond(R).
wffCond(C) :- isBinary(C,"*>",L,R), wffCond(L), wffCond(R).
wffCond(C) :- isTuple(C,[Cx]), wffCond(Cx).
wffCond(C) :- isBinary(C,".=",L,R), wffPtn(L),wffExp(R).
wffCond(C) :- isBinary(C,"=.",L,R), wffExp(L),wffPtn(R).
wffCond(C) :- isBinary(C,Comp,L,R), member(Comp,["==","!=","<=","-->"]),wffExp(L),wffExp(R).
wffCond(C) :- isRoundTerm(C,Op,Args), wffExp(Op), wffExps(Args).

wffPtns([]).
wffPtns([P|T]) :- wffPtn(P), wffPtns(T).

wffPtn(name(_,O)) :- \+ isKeyword(O).
wffPtn(integer(_,_)).
wffPtn(long(_,_)).
wffPtn(float(_,_)).
wffPtn(string(_,_)).
wffPtn(interString(Lc,_)) :- reportError("interpolated string not allowed in pattern",[],Lc).
wffPtn(tuple(_,"()",A)) :- wffPtns(A).
wffPtn(tuple(_,"[]",A)) :- wffPtns(A).
wffPtn(tuple(Lc,"{}",_)) :- reportError("not permitted as pattern",[],Lc).
wffPtn(T) :- isRoundTerm(T,Op,Args), wffExp(Op), wffPtns(Args).

wffIden(Nm) :- ifthen(isName(Nm,_),
    true,
    (locOfAst(Nm,Lc),
    reportError("expecting identifier %w",[Nm],Lc))).

privatize(_,[],Els,Els).
privatize(Lc,[St|More],[unary(Lc,"private",St)|Tail]) :- privatize(Lc,More,Tail).


ifthen(T,Th,_) :- T, !, Th.
ifthen(_,_,El) :- El.