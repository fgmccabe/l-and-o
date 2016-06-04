
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

  isBraceTuple(Content,Lc,[ShowFun]),
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
  isBraceTuple(Lc,[DispFun],Content),
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

privatize(_,[],Els,Els).
privatize(Lc,[St|More],[unary(Lc,"private",St)|Tail]) :- privatize(Lc,More,Tail).

