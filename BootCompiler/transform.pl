:- module(transform,[]).

:- use_module(canon).
:- use_module(transutils).
:- use_module(errors).
:- use_module(types).
:- use_module(debug).


transformProg(prog(Pkg,Imports,Defs,Others,_Fields,Types),Opts,Rules) :-
  makeDefsMap(Pkg,Opts,Defs,DfList,Rest),
  makeTypesMap(Pkg,Opts,Types,Rest,[]),
  moduleMap(Pkg,DfList,Map),
  transformDefs(Map,Opts,Defs,Rules,[]).

transformDefs(_,_,[],Rules,Rules).
transformDefs(Map,Opts,[Def|Defs],Rules,Rx) :-
  transformDef(Map,Opts,Def,Rules,R0),
  transformDefs(Map,Opts,Defs,R0,Rx).

transformDef(Map,Opts,function(Lc,Nm,Tp,Eqns),Rules,Rx) :-
  pushOpt(Opts,inProg(Nm),QOpts),
  transformEquations(Map,QOpts,Eqns,1,No,Rules,R0),
  failSafeEquation(Map,QOpts,Lc,Nm,No,Tp,R0,Rx).

transformEquations(_,_,[],No,No,Rules,Rules).
transformEquations(Map,Opts,[Eqn|Defs],No,Nx,Rules,Rx) :-
  transformEqn(Map,Opts,No,Eqn,Rules,R0),
  N1 is No+1,
  transformEquations(Map,Opts,Defs,N1,Nx,R0,Rx).

transformEqn(Map,Opts,QNo,equation(Lc,Nm,A,Cond,Value),[clse(Nm,Q,Args,Body)|Rx],Rx) :-
  extraVars(Map,Extra),                                     % extra variables coming from labels
  debugPreamble(Nm,QNo,Extra,Q0,LbLx,PDx,Map,Opts,ClOpts),  % are we debugging?
  trPtns(A,Args,[Rep|Extra],Q0,Q1,PreG,Px,PostG,PostGx,Map,ClOpts), % head args
  trGoal(Cond,PGx,[neck(Lc)|CGx],Q1,Q2,Map,ClOpts),         % condition goals
  trExp(Value,Rep,PreV,PVx,PVx,Px,Q2,Q,Map,ClOpts),         % replacement expression
  labelAccess(Q,Map,Body,LbLx),                             % generate label access goals
  frameDebug(Nm,QNo,FBg,FBx,Q,ClOpts),                      % generate frame entry debugging
  deframeDebug(Nm,QNo,PostG,[],ClOpts),                     % generate frame exit debugging
  breakDebug(Nm,QNo,Lc,CGx,PreV,Map,ClOpts).                % generate break point debugging

trPtns([],Args,Args,Q,Q,Pre,Pre,Post,Post,_,_) :-!.
trPtns([P|More],[A|Args],Ax,Q,Qx,Pre,Prx,Post,Psx,Map,Opts) :-
  trPtn(P,A,Q,Q0,Pre,Pre0,Post,Pst0,Map,Opts),
  trPtns(More,Args,Ax,Q0,Qx,Pre0,Prx,Pst0,Psx,Map,Opts).

trPtn(v(Lc,"this"),ThVr,Q,Qx,Pre,Pre,Post,Post,Map,Opts) :- 
  thisVar(Map,ThVr),!,
  merge([ThVr],Q,Qx).
trPtn(v(Lc,"this"),idnt("_"),Q,Qx,Pre,Prx,Post,Pstx,Map,Opts) :- !,
  reportError("'this' not defined here",[],Lc).
trPtn(v(Lc,Nm),A,Q,Qx,Pre,Prx,Post,Pstx,Map,Opts) :- !,
  trVarPtn(Lc,Nm,A,Q,Qx,Pre,Prx,Post,Pstx,Map,Opts).
trPtn(intLit(Ix),intgr(Ix),Q,Q,Pre,Pre,Post,Post,_,_) :-!.
trPtn(longLit(Ix),long(Ix),Q,Q,Pre,Pre,Post,Post,_,_) :-!.
trPtn(floatLit(Ix),float(Ix),Q,Q,Pre,Pre,Post,Post,_,_) :-!.
trPtn(stringLit(Ix),strg(Ix),Q,Q,Pre,Pre,Post,Post,_,_) :-!.
trPtn(pkgRef(Lc,Pkg,Rf),A,Q,Qx,Pre,Pre,Post,Pstx,Map,Opts) :- !,
  lookupPkgRef(Map,Pkg,Rf,Reslt),
  gensym("$v",Xi),
  implementPkgRefPtn(Reslt,Lc,Pkg,Rf,idnt(Xi),Q,Qx,Post,Pstx).
trPtn(apply(Lc,O,A),cons(Op,Args),Q,Qx,Pre,Px,Post,Pstx,Map,Opts) :-
  trExp(O,Op,Pre,P0,Post,Pst0,Q,Q0,Map,Opts),
  trPtns(A,Args,Q0,Qx,P0,Px,Pst0,Pstx,Map,Opts).
trPtn(where(P,C),Ptn,Q,Qx,Pre,Px,Post,Pstx,Map,Opts) :-
  trPtn(P,Ptn,Q,Q0,Pre,P0,Post,Pst0,Map,Opts),
  trGoal(C,P0,Px,Q0,Qx,Map,Opts).
trPtn(XX,void,Q,Q,Pre,Pre,Post,Post,_,_) :-
  reportMsg("internal: cannot transform %s as pattern",[XX]).

trVarPtn(Lc,Nm,A,Q,Qx,Pre,Prx,Post,Pstx,Map,Opts) :-
  lookupVarName(Nm,Map,V),!,
  gensym("$v",X),
  implementVarPtn(V,Nm,idnt(X),A,Q,Qx,Pre,Prx,Post,Pstx).
trVarPtn(Lc,Nm,idnt("_"),Q,Q,Pre,Pre,Post,Post,_,_)
  reportError("'%s' not defined",[Nm],Lc).

implementVarPtn(localVar(Vn,ClVr,TVr),_,X,X,Q,Qx,[call(Vn,[X,ClVr,TVr])|Pre],Pre,Post,Post) :- !, % instance var
  merge([X,ClVr,TVr],Q,Qx).
implementVarPtn(moduleVar(_,Vn),_,X,X,Q,[X|Q],[call(Vn,X)|Pre],Pre,Post,Post) :- !. % module variable
implementVarPtn(labelArg(N,ClVr,TVr),_,_,idnt(N),Q,Qx,Pre,Pre,Post,Post) :- !,    % argument from label
  merge([idnt(N),ClVr,TVr],Q,Qx).
implementVarPtn(moduleClass(_,Enum,_),_,_,enum(Enum),Q,Q,Pre,Pre,Post,Post).
implementVarPtn(localClass(Enum,_,LbVr,ThVr),_,_,cons(Enum,[LbVr,ThVr]),Q,Qx) :-
  merge([LbVr,ThVr],Q,Qx).
implementVarPtn(inherit(prg(Nm,_),LbVr,ThVr),_,_,cons(strct(Nm,2),[LbV,ThVr]),Q,Qx) :-
  merge([LbVr,ThVr],Q,Qx).
implementVarPtn(notInMap,Nm,_,idnt(Nm),Q,Qx,Pre,Pre,Post,Post) :-                 % variable local to rule
  merge([idnt(Nm)],Q,Qx).

implementPkgRefPtn(moduleVar(Pkg,Vn),_,_,_,Xi,Q,[Xi|Q],[call(Vn,[Xi])|Pre],Pre,Post,Post).
implementPkgRefPtn(moduleClass(_,Enum,_),_,_,_,enum(Enum),Q,Q,Pre,Pre,Post,Post).
implementPkgRefPtn(Other,Lc,Pkg,Rf,_,Q,Q,Post,Post) :-
  reportError("illegal access to %s#%s",[Pkg,Ref],Lc).

trExps([],Args,Args,Q,Q,Pre,Pre,Post,Post,_,_) :-!.
trExps([P|More],[A|Args],Ax,Q,Qx,Pre,Prx,Post,Psx,Map,Opts) :-
  trExp(P,A,Q,Q0,Pre,Pre0,Post,Pst0,Map,Opts),
  trExps(More,Args,Ax,Q0,Qx,Pre0,Prx,Pst0,Psx,Map,Opts).

trExp(v(Lc,"this"),ThVr,Q,Qx,Pre,Pre,Post,Post,Map,Opts) :- 
  thisVar(Map,ThVr),!,
  merge([ThVr],Q,Qx).
trExp(v(Lc,Nm),Vr,Q,Qx,Pre,Px,Post,Pstx,Map,Opts) :-
  trVarExp(Lc,Nm,Vr,Q,Qx,Pre,Px,Post,Pstx,Map,Opts).
trExp(intLit(Ix),intgr(Ix),Q,Q,Pre,Pre,Post,Post,_,_) :-!.
trExp(longLit(Ix),long(Ix),Q,Q,Pre,Pre,Post,Post,_,_) :-!.
trExp(floatLit(Ix),float(Ix),Q,Q,Pre,Pre,Post,Post,_,_) :-!.
trExp(stringLit(Ix),strg(Ix),Q,Q,Pre,Pre,Post,Post,_,_) :-!.
trExp(pkgRef(Lc,Pkg,Rf),Exp,Q,Qx,Pre,Pre,Post,Pstx,Map,Opts) :-
  lookupPkgRef(Map,Pkg,Rf,Reslt),
  gensym("$v",Xi),
  implementPkgRefExp(Reslt,Lc,Pkg,Rf,idnt(Xi),Q,Qx,Post,Pstx).
trExp(apply(Lc,Op,A),Exp,Q,Qx,Pre,Px,Post,Pstx,Map,Opts) :-
  trExps(A,Args,Q,Q0,APre,APx,APost,Apstx,Map,Opts),
  gensym("$v",X),
  concat([X],Args,XArgs),
  merge([X],Q0,Q1),
  trExpCallOp(Op,idnt(X),XArgs,Exp,Q1,Qx,OPre,Opx,OPost,OPstx,Map,Opts).

trExp(XX,void,Q,Q,Pre,Pre,Post,Post,_,_) :-
  reportMsg("internal: cannot transform %s as expression",[XX]).

trVarExp(Lc,Nm,Exp,Q,Qx,Pre,Prx,Post,Pstx,Map,Opts) :-
  lookupVarName(Nm,Map,V),!,
  gensym("$v",X),
  implementVarExp(V,Lc,Nm,idnt(X),Exp,Q,Qx,Pre,Prx,Post,Pstx).
trVarExp(Lc,Nm,idnt("_"),Q,Q,Pre,Pre,Post,Post,_,_)
  reportError("'%s' not defined",[Nm],Lc).

trExpCallOp(v(Lc,Nm),X,XArgs,X,Q,Q,Pre,Pre,[ecall(EscName,XArgs)|Tail],Tail,Map,Opts) :-
  isEscape(Nm,EscName),!.
trExpCallOp(v(Lc,Nm),X,XArgs,Exp,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts) :-
  lookupFunName(Map,Nm,Reslt),
  implementFunCall(Reslt,Nm,X,XArgs,Exp,Q,Qx,Pre,Px,Tail,Tailx).
trExpCallOp(dot(Lc,Rec,Fld),X,XArgs,Exp,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts) :-
  trCons(Fld,XArgs,Op),
  C = cons(Op,XArgs),
  trExpCallDot(Rec,Rec,C,X,XArgs,Exp,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts).
trExpCallOp(pkgRef(_,Pkg,Nm),X,XArgs,X,Q,Q,Pre,Pre,[call(Fun,XArgs)|Tail],Tail,Map,Opts) :-
  lookupPkgRef(Map,Pkg,Nm,moduleFun(_,Fun,_)).




trExpCallDot(v(_,Nm),Rec,C,X,XArgs,Exp,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts) :-
  lookupFunName(Map,Nm,Reslt),
  implementDotFunCall(Reslt,Rec,C,X,XArgs,Exp,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts).
trExpCallDot(_,Rec,C,X,XArgs,Exp,Q,Qx,Pre,Px,[ocall(C,Rc,Rc)|Tail],Tail,Map,Opts) :-
  trExp(Rec,Rc,Q,Q0,Pre,Px,TailR,TailxR,Map,Opts),
  merge([X],Q0,Qx).

implementDotFunCall(inherit(_,Super,LblVr,ThVr), Rec, C, X, Q,Qx,[call(Super,[C,LblVar,ThVr])|Tail],Tail) :-
  merge([X,LblVr,ThVr],Q,Qx).
implementDotFunCall(_,Rec,C,X,Q,Qx,Pre,Px,[ocall(C,Rc,Rc)|Tail],Tail,Map,Opts) :-
  trExp(Rec,Rc,Q,Q0,Pre,Px,TailR,TailxR,Map,Opts),
  merge([X],Q0,Qx).


implementFunCall(localFun(Fn,LblVr,ThVr),_,X,Args,X,Q,Qx,Pre,Pre,[call(Fn,XArgs)|Tail],Tail) :-
  concat(Args,[X,LblVr,ThVr],XArgs),
  merge([X,LblVr,ThVr],Q,Qx).
implementFunCall(moduleFun(_,Fn),_,X,Args,X,Q,Qx,Pre,Pre,[call(Fn,XArgs)|Tail],Tail) :-
  concat(Args,[X],XArgs),
  merge([X],Q,Qx).
implementFunCall(inheritField(Super,LblVr,ThVr),Nm,X,Args,X,Q,Qx,Pre,Pre,
        [call(Super,[cons(Op,XArgs),LblVr,ThVr])|Tail],Tail) :-
  concat(Args,[X],XArgs),
  trCons(Nm,XArgs,Op),
  merge([X,LblVr,ThVr],Q,Qx).

implementVarExp(localVar(V,LblVr,ThVr),_,_,X,X,Q,Qx,Pre,Pre,[call(Vn,[X,LbVr,ThVr])|Tail],Tail) :-
  merge([X,LbVr,ThVr],Q,Qx).
implementVarExp(moduleVar(_,V),_,_,X,X,Q,Qx,Pre,Pre,[call(V,[X])|Tail],Tail) :-
  merge([X],Q,Qx).
implementVarExp(labelArg(N,LblVr,ThVar),_,_,_,idnt(N),Q,Qx,Pre,Pre,Tail,Tail) :-
  merge([idnt(N),LblVr,ThVar],Q,Qx).
implementVarExp(inheritField(Super,LblVr,ThVr),Lc,Nm,X,X,Q,Qx,
      [call(Super,[cons(strct(Nm,1),[X]),LblVr,ThVr])|Pre],Pre,Tail,Tail),
  merge([X,LblVr,ThVr],Q,Qx).
implementVarExp(moduleClass(_,Enum,_),_,_,_,enum(Enum),Q,Q,Pre,Pre,Tail,Tail).
implementVarExp(localClass(Enum,_,LbVr,ThVr),_,_,_,cons(Enum,[LbVr,ThVr]),Q,Qx,Pre,Pre,Tail,Tail) :-
  merge([LbVr,ThVr],Q,Qx).
implementVarExp(inherit(prg(Nm,_),LbVr,ThVr),_,_,_,cons(strct(Nm,2),[LbV,ThVr]),Q,Qx,Pre,Pre,Tail,Tail) :-
  merge([LbVr,ThVr],Q,Qx).
implementVarExp(notInMap,_,Nm,_,idnt(Nm),Q,Qx,Pre,Pre,Tail,Tail) :-
  merge([idnt(Nm)],Q,Qx).
implementVarExp(_Other,Lc,Nm,_,idnt(Nm),Q,Q,Pre,Pre,Tail,Tail) :-
  reportError("cannot handle %s in expression",[Nm],Lc).

implementPkgRefExp(moduleVar(Pkg,Vn),_,_,_,Xi,Q,[Xi|Q],[call(Vn,[Xi])|Pre],Pre,Post,Post).
implementPkgRefExp(moduleClass(_,Enum,_),_,_,_,enum(Enum),Q,Q,Pre,Pre,Post,Post).

implementPkgRefExp(Other,Lc,Pkg,Rf,_,Q,Q,Post,Post) :-
  reportError("illegal access to %s#%s",[Pkg,Ref],Lc).

makeDefsMap(Pkg,[Def|Rest],Map,Mx) :-
  makeDefMap(Pkg,Def,Map,M0),
  makeDefsMap(Pkg,Rest,M0,Mx).
makeDefsMap(_,[],Map,Map).

makeDefMap(Pkg,function(_,Nm,Tp,_),[(Nm,modFun(Pkg,LclName,Tp)|Mx],Mx) :-
  localName(Pkg,"@",Nm,LclName).
makeDefMap(Pkg,predicate(_,Nm,Tp,_),[(Nm,modPred(Pkg,LclName,Tp)|Mx],Mx) :-
  localName(Pkg,"@",Nm,LclName).
makeDefMap(Pkg,defn(_,Nm,_,Tp,_),[(Nm,modDefn(Pkg,LclName,Tp)|Mx],Mx) :-
  localName(Pkg,"@",Nm,LclName).
makeDefMap(Pkg,class(_,Nm,Tp,_),[(Nm,modClass(Pkg,LclName,Tp)|Mx],Mx) :-
  localName(Pkg,"#",Nm,LclName).
makeDefMap(Pkg,typeDef(_,Nm,Tp,_),[(Nm,modType(Pkg,LclName,Tp)|Mx],Mx) :-
  localName(Pkg,"*",Nm,LclName).


  
