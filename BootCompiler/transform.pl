:- module(transform,[transformProg/3]).

:- use_module(canon).
:- use_module(transutils).
:- use_module(errors).
:- use_module(types).
:- use_module(debug).
:- use_module(misc).
:- use_module(escapes).
:- use_module(location).


transformProg(prog(Pkg,_Imports,Defs,Others,_Fields,Types),Opts,Rules) :-
  makePkgMap(Pkg,Defs,Types,Map),
  transformModuleDefs(Pkg,Map,Opts,Defs,R0,Rx,Rx,[]),
  transformOthers(Pkg,Map,Opts,Others,Rules,R0).

transformModuleDefs(_,_,_,[],Rules,Rules,Ex,Ex).
transformModuleDefs(Pkg,Map,Opts,[Def|Defs],Rules,Rx,Ex,Exx) :-
  transformMdlDef(Pkg,Map,Opts,Def,Rules,R0,Ex,Ex1),
  transformModuleDefs(Pkg,Map,Opts,Defs,R0,Rx,Ex1,Exx).

transformMdlDef(Pkg,Map,Opts,function(Lc,Nm,Tp,Eqns),Rules,Rx,Ex,Exx) :-
  localName(Pkg,"@",Nm,LclName),
  transformEquations(Map,Opts,LclName,Eqns,1,_,Rules,R0,Ex,Exx),
  failSafeEquation(Map,Opts,Lc,LclName,Tp,R0,Rx).
transformMdlDef(Pkg,Map,Opts,predicate(_,Nm,_,Clses),Rules,Rx,Ex,Exx) :-
  localName(Pkg,"@",Nm,LclName),
  transformClauses(Map,Opts,LclName,Clses,1,_,Rules,Rx,Ex,Exx).
transformMdlDef(Pkg,Map,Opts,defn(_,Nm,Cond,_,Value),Rules,Rx,Ex,Exx) :-
  localName(Pkg,"@",Nm,LclName),
  transformDefn(Map,Opts,Nm,LclName,Cond,Value,Rules,Rx,Ex,Exx).

transformEquations(_,_,_,[],No,No,Rules,Rules,Ex,Ex).
transformEquations(Map,Opts,LclName,[Eqn|Defs],No,Nx,Rules,Rx,Ex,Exx) :-
  transformEqn(Map,Opts,LclName,No,Eqn,Rules,R0,Ex,Ex0),
  N1 is No+1,
  transformEquations(Map,Opts,LclName,Defs,N1,Nx,R0,Rx,Ex0,Exx).

transformEqn(Map,Opts,LclName,QNo,equation(_,Nm,A,Cond,Value),[clse(Q,prg(LclName,Arity),Args,Body)|Rx],Rx,Ex,Exx) :-
  extraVars(Map,Extra),                                     % extra variables coming from labels
  debugPreamble(Nm,Extra,Q0,LbLx,FBg,Opts,ClOpts),  % are we debugging?
  trPtns(A,Args,[Rep|Extra],Q0,Q1,PreG,PGx,PGx,PostGx,Map,ClOpts,Ex,Ex0), % head args
  trGoal(Cond,PostGx,[neck|CGx],Q1,Q2,Map,ClOpts,Ex0,Ex1),         % condition goals
  trExp(Value,Rep,Q2,Q3,PreV,PVx,PVx,Px,Map,ClOpts,Ex1,Exx),         % replacement expression
  labelAccess(Q3,Q,Map,Body,LbLx),                             % generate label access goals
  frameDebug(Nm,QNo,FBg,PreG,Q,ClOpts),                      % generate frame entry debugging
  deframeDebug(Nm,QNo,Px,[],ClOpts),                     % generate frame exit debugging
  length(Args,Arity),
  breakDebug(Nm,CGx,PreV,ClOpts).                % generate break point debugging

failSafeEquation(Map,Opts,Lc,LclName,Tp,[clse([],prg(LclName,Arity),Anons,G)|Rest],Rest) :-
  extraVars(Map,Extra),                                     % extra variables coming from labels
  typeArity(Tp,Ar),
  length(Extra,EA),
  Arity is Ar+1+EA,
  genAnons(Arity,Anons),
  genRaise(Opts,Lc,LclName,G,[]).

genRaise(_,Lc,LclName,[raise(cons(strct("error",4),[strg(LclName),intgr(Lno),intgr(Off),intgr(Sz)]))|P],P) :-
  lcLine(Lc,Lno),
  lcColumn(Lc,Off),
  lcSize(Lc,Sz).

genAnons(0,[]).
genAnons(K,[anon|Rest]) :-
  K>0,
  K1 is K-1,
  genAnons(K1,Rest).

transformClauses(_,_,_,[],No,No,Rules,Rules,Ex,Ex).
transformClauses(Map,Opts,LclName,[Cl|Defs],No,Nx,Rules,Rx,Ex,Exx) :-
  transformClause(Map,Opts,LclName,No,Cl,Rules,R0,Ex,Ex0),
  N1 is No+1,
  transformClauses(Map,Opts,LclName,Defs,N1,Nx,R0,Rx,Ex0,Exx).

transformClause(Map,Opts,LclName,QNo,clause(_,Nm,A,Cond,Body),[clse(Q,prg(LclName,Arity),Args,Goals)|Rx],Rx,Ex,Exx) :-
  extraVars(Map,Extra),                                     % extra variables coming from labels
  debugPreamble(Nm,Extra,Q0,LbLx,FBg,Opts,ClOpts),  % are we debugging?
  trPtns(A,Args,Extra,Q0,Q1,PreG,PGx,PGx,PostGx,Map,ClOpts,Ex,Ex0), % head args
  trGoal(Cond,PostGx,CGx,Q1,Q2,Map,ClOpts,Ex0,Ex1),         % condition goals
  trGoal(Body,CGx,CGy,Q2,Q3,Map,ClOpts,Ex1,Exx),
  labelAccess(Q3,Q,Map,Goals,LbLx),                             % generate label access goals
  frameDebug(Nm,QNo,FBg,PreG,Q,ClOpts),                      % generate frame entry debugging
  deframeDebug(Nm,QNo,Px,[],ClOpts),                     % generate frame exit debugging
  length(Args,Arity),
  breakDebug(Nm,CGy,Px,ClOpts).                % generate break point debugging
transformClause(Map,Opts,LclName,QNo,strong(_,Nm,A,Cond,Body),[clse(Q,prg(LclName,Arity),Args,Goals)|Rx],Rx,Ex,Exx) :-
  extraVars(Map,Extra),                                     % extra variables coming from labels
  debugPreamble(Nm,Extra,Q0,LbLx,FBg,Opts,ClOpts),  % are we debugging?
  trPtns(A,Args,Extra,Q0,Q1,PreG,PGx,PGx,PostGx,Map,ClOpts,Ex,Ex0), % head args
  trGoal(Cond,PostGx,[neck|CGx],Q1,Q2,Map,ClOpts,Ex0,Ex1),         % condition goals
  trGoal(Body,CGx,CGy,Q2,Q3,Map,ClOpts,Ex1,Exx),
  labelAccess(Q3,Q,Map,Goals,LbLx),                             % generate label access goals
  frameDebug(Nm,QNo,FBg,PreG,Q,ClOpts),                      % generate frame entry debugging
  deframeDebug(Nm,QNo,Px,[],ClOpts),                     % generate frame exit debugging
  length(Args,Arity),
  breakDebug(Nm,CGy,Px,ClOpts).                % generate break point debugging

transformDefn(Map,Opts,Nm,LclName,Cond,Value,[clse(Q,prg(LclName,Arity),[Rep],Body)|Rx],Rx,Ex,Exx) :-
  extraVars(Map,Extra),                                     % extra variables coming from labels
  debugPreamble(Nm,Extra,Q0,LbLx,FBg,Opts,ClOpts),  % are we debugging?
  trGoal(Cond,PostGx,[neck|CGx],Q0,Q2,Map,ClOpts,Ex,Ex0),         % condition goals
  trExp(Value,Rep,Q2,Q3,PreV,PVx,PVx,Px,Map,ClOpts,Ex0,Exx),         % replacement expression
  labelAccess(Q3,Q,Map,Body,LbLx),                             % generate label access goals
  frameDebug(Nm,QNo,FBg,PostGx,Q,ClOpts),                      % generate frame entry debugging
  deframeDebug(Nm,QNo,Px,[],ClOpts),                     % generate frame exit debugging
  length([_|Extra],Arity),
  breakDebug(Nm,CGx,PreV,ClOpts).                % generate break point debugging

transformOthers(_,_,_,[],Rx,Rx).
transformOthers(Pkg,Map,Opts,[assertion(Lc,G)|Others],Rules,Rx) :-
  collect(Others,isAssertion,Asserts,Rest),
  transformAssertions(Pkg,Map,Opts,[assertion(Lc,G)|Asserts],Rules,R0),
  transformOthers(Pkg,Map,Opts,Rest,R0,Rx).

transformAssertions(Pkg,Map,Opts,Asserts,Rules,Rx) :-
  rfold(Asserts,transform:collectGoal,true(_),G),
  localName(Pkg,"@","assert",LclName),
  transformClause(Map,Opts,LclName,1,clause('',"assert",[],true(''),G),Rules,R0,R0,Rx).

collectGoal(assertion(_,G),true(_),G) :-!.
collectGoal(assertion(_,G),O,conj(O,G)).

trPtns([],Args,Args,Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trPtns([P|More],[A|Args],Ax,Q,Qx,Pre,Prx,Post,Psx,Map,Opts,Ex,Exx) :-
  trPtn(P,A,Q,Q0,Pre,Pre0,Post,Pst0,Map,Opts,Ex,Ex0),
  trPtns(More,Args,Ax,Q0,Qx,Pre0,Prx,Pst0,Psx,Map,Opts,Ex0,Exx).

trPtn(v(_,"this"),ThVr,Q,Qx,Pre,Pre,Post,Post,Map,_Opts,Ex,Ex) :- 
  thisVar(Map,ThVr),!,
  merge([ThVr],Q,Qx).
trPtn(v(Lc,"this"),idnt("_"),Q,Q,Pre,Pre,Post,Post,_Map,_,Ex,Ex) :- !,
  reportError("'this' not defined here",[],Lc).
trPtn(v(Lc,Nm),A,Q,Qx,Pre,Prx,Post,Pstx,Map,Opts,Ex,Ex) :- !,
  trVarPtn(Lc,Nm,A,Q,Qx,Pre,Prx,Post,Pstx,Map,Opts).
trPtn(intLit(Ix),intgr(Ix),Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trPtn(floatLit(Ix),float(Ix),Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trPtn(stringLit(Ix),strg(Ix),Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trPtn(pkgRef(Lc,Pkg,Rf),Ptn,Q,Qx,Pre,Pre,Post,Pstx,Map,_,Ex,Ex) :- !,
  lookupPkgRef(Map,Pkg,Rf,Reslt),
  gensym("Xi",Xi),
  implementPkgRefPtn(Reslt,Lc,Pkg,Rf,idnt(Xi),Ptn,Q,Qx,Post,Pstx).
trPtn(tuple(_,Ptns),tpl(P),Q,Qx,Pre,Px,Post,Postx,Map,Opts,Ex,Exx) :-
  trPtns(Ptns,P,Q,Qx,Pre,Px,Post,Postx,Map,Opts,Ex,Exx).
trPtn(apply(_,O,A),Ptn,Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx) :-
  trPtns(A,Args,[],Q,Q0,APre,AP0,APost,APs0,Map,Opts,Ex,Ex0),
  trPtnCallOp(O,Args,Ptn,Q0,Qx,APre,AP0,APost,APs0,Pre,Px,Post,Pstx,Map,Opts,Ex0,Exx).

trPtn(where(P,C),Ptn,Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx) :-
  trPtn(P,Ptn,Q,Q0,Pre,P0,Post,Pstx,Map,Opts,Ex,Ex0),
  trGoal(C,P0,Px,Q0,Qx,Map,Opts,Ex0,Exx).
trPtn(XX,void,Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-
  reportMsg("internal: cannot transform %s as pattern",[XX]).

trVarPtn(_,Nm,A,Q,Qx,Pre,Prx,Post,Pstx,Map,_) :-
  lookupVarName(Map,Nm,V),!,
  gensym("X",X),
  implementVarPtn(V,Nm,idnt(X),A,Q,Qx,Pre,Prx,Post,Pstx).
trVarPtn(Lc,Nm,idnt("_"),Q,Q,Pre,Pre,Post,Post,_,_) :-
  reportError("'%s' not defined",[Nm],Lc).

implementVarPtn(localVar(Vn,ClVr,TVr),_,X,X,Q,Qx,[call(Vn,[X,ClVr,TVr])|Pre],Pre,Post,Post) :- !, % instance var
  merge([X,ClVr,TVr],Q,Qx).
implementVarPtn(moduleVar(_,Vn),_,X,X,Q,[X|Q],[call(Vn,X)|Pre],Pre,Post,Post) :- !. % module variable
implementVarPtn(labelArg(N,ClVr,TVr),_,_,idnt(N),Q,Qx,Pre,Pre,Post,Post) :- !,    % argument from label
  merge([idnt(N),ClVr,TVr],Q,Qx).
implementVarPtn(moduleClass(_,enum(Enum),_),_,_,enum(Enum),Q,Q,Pre,Pre,Post,Post).
implementVarPtn(localClass(Enum,_,LbVr,ThVr),_,_,cons(Enum,[LbVr,ThVr]),Q,Qx,Pre,Pre,Post,Post) :-
  merge([LbVr,ThVr],Q,Qx).
implementVarPtn(inherit(prg(Nm,_),LbVr,ThVr),_,_,cons(strct(Nm,2),[LbVr,ThVr]),Q,Qx,Pre,Pre,Post,Post) :-
  merge([LbVr,ThVr],Q,Qx).
implementVarPtn(notInMap,Nm,_,idnt(Nm),Q,Qx,Pre,Pre,Post,Post) :-                 % variable local to rule
  merge([idnt(Nm)],Q,Qx).

trPtnCallOp(v(_,Nm),Args,Ptn,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,_,Ex,Ex) :-
  lookupFunName(Map,Nm,Reslt),
  gensym("X",X),
  implementPtnCall(Reslt,Nm,idnt(X),Args,Ptn,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx).

implementPtnCall(localFun(Fn,LblVr,ThVr),_,X,Args,X,Q,Qx,Pre,Px,Tail,[call(Fn,XArgs)|Tailx],Pre,Px,Tail,Tailx) :-
  concat(Args,[X,LblVr,ThVr],XArgs),
  merge([X,LblVr,ThVr],Q,Qx).
implementPtnCall(moduleFun(_,Fn),_,X,Args,X,Q,Qx,Pre,Px,Tail,[call(Fn,XArgs)|Tailx],Pre,Px,Tail,Tailx) :-
  concat(Args,[X],XArgs),
  merge([X],Q,Qx).
implementPtnCall(inheritField(Super,LblVr,ThVr),Nm,X,Args,X,Q,Qx,Pre,Px,Tail,[call(Super,[cons(Op,XArgs),LblVr,ThVr])|Tailx],Pre,Px,Tail,Tailx):-
  concat(Args,[X],XArgs),
  trCons(Nm,XArgs,Op),
  merge([X,LblVr,ThVr],Q,Qx).
implementPtnCall(moduleClass(_,Mdl,_),_,_,Args,cons(Mdl,Args),Q,Q,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx).
implementPtnCall(localClass(Mdl,_,LbVr,ThVr),_,_,Args,cons(Mdl,XArgs),Q,Qx,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx) :-
  concat(Args,[LbVr,ThVr],XArgs),
  merge([LbVr,ThVr],Q,Qx).
implementPtnCall(inherit(prg(Mdl,_),_,LbVr,ThVr),_,_,Args,cons(strct(Mdl,Ar),Args),Q,Qx,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx) :-
  merge([LbVr,ThVr],Q,Qx),
  length(Args,Ar).

implementPkgRefPtn(moduleVar(_,Vn),_,_,_,Xi,Xi,Q,[Xi|Q],[call(Vn,[Xi])|Tail],Tail).
implementPkgRefPtn(moduleClass(_,enum(Enum),_),_,_,_,_,enum(Enum),Q,Q,Tail,Tail).
implementPkgRefPtn(_,Lc,Pkg,Rf,_,idnt("_"),Q,Q,Post,Post) :-
  reportError("illegal access to %s#%s",[Pkg,Rf],Lc).

trExps([],[],Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trExps([P|More],[A|Args],Q,Qx,Pre,Prx,Post,Psx,Map,Opts,Ex,Exx) :-
  trExp(P,A,Q,Q0,Pre,Pre0,Post,Pst0,Map,Opts,Ex,Ex0),
  trExps(More,Args,Q0,Qx,Pre0,Prx,Pst0,Psx,Map,Opts,Ex0,Exx).

trExp(v(_,"this"),ThVr,Q,Qx,Pre,Pre,Post,Post,Map,_,Ex,Ex) :- 
  thisVar(Map,ThVr),!,
  merge([ThVr],Q,Qx).
trExp(v(Lc,Nm),Vr,Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Ex) :-
  trVarExp(Lc,Nm,Vr,Q,Qx,Pre,Px,Post,Pstx,Map,Opts).
trExp(intLit(Ix),intgr(Ix),Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trExp(floatLit(Ix),float(Ix),Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trExp(stringLit(Ix),strg(Ix),Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trExp(pkgRef(Lc,Pkg,Rf),Exp,Q,Qx,Pre,Pre,Post,Pstx,Map,_,Ex,Ex) :-
  lookupPkgRef(Map,Pkg,Rf,Reslt),
  gensym("Xi",Xi),
  implementPkgRefExp(Reslt,Lc,Pkg,Rf,idnt(Xi),Exp,Q,Qx,Post,Pstx).
trExp(tuple(_,A),tpl(TA),Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx) :-
  trExps(A,TA,Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx).
trExp(apply(_,Op,A),Exp,Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx) :-
  trExps(A,Args,Q,Q0,APre,APx,APost,APostx,Map,Opts,Ex,Ex0),
  gensym("X",X),
  trExpCallOp(Op,idnt(X),Args,Exp,Q0,Qx,APre,APx,APost,APostx,Pre,Px,Post,Pstx,Map,Opts,Ex0,Exx).
trExp(dot(_,Rec,Fld),Exp,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  gensym("XV",XV),
  X = idnt(XV),
  trCons(Fld,[X],S),
  C = cons(S,[X]),
  trDotExp(Rec,C,X,Exp,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx).

trExp(XX,void,Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-
  reportMsg("internal: cannot transform %s as expression",[XX]).

trDotExp(v(Lc,Nm),C,X,Exp,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  lookupVarName(Map,Nm,Reslt),
  implementDotExp(Reslt,v(Lc,Nm),C,X,X,Exp,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx).
trDotExp(R,C,X,X,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  trExp(R,Rc,Q,Q0,Pre,Px,Tail,[ocall(C,Rc,Rc)|Tailx],Map,Opts,Ex,Exx),
  merge([X],Q0,Qx).

implementDotExp(inherit(_,Super,ClVr,ThVr),_,C,X,X,Q,Qx,Pre,Pre,[call(Super,[C,ClVr,ThVr])|Tail],Tail,_,_,Ex,Ex) :-
  merge([X,ClVr,ThVr],Q,Qx).
implementDotExp(_,R,C,X,X,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  trExp(R,Rc,Q,Q0,Pre,Px,Tail,[ocall(C,Rc,Rc)|Tailx],Map,Opts,Ex,Exx),
  merge([X],Q0,Qx).

trVarExp(Lc,Nm,Exp,Q,Qx,Pre,Prx,Post,Pstx,Map,_) :-
  lookupVarName(Map,Nm,V),!,
  gensym("X",X),
  implementVarExp(V,Lc,Nm,idnt(X),Exp,Q,Qx,Pre,Prx,Post,Pstx).
trVarExp(Lc,Nm,idnt("_"),Q,Q,Pre,Pre,Post,Post,_,_) :-
  reportError("'%s' not defined",[Nm],Lc).

trExpCallDot(v(_,Nm),Rec,C,X,XArgs,Exp,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  lookupFunName(Map,Nm,Reslt),
  implementDotFunCall(Reslt,Rec,C,X,XArgs,Exp,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx).
trExpCallDot(_,Rec,C,X,_,X,Q,Qx,Pre,APx,Tail,[ocall(C,Rc,Rc)|TailR],Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  trExp(Rec,Rc,Q,Qx,APx,Px,TailR,Tailx,Map,Opts,Ex,Exx).

implementDotFunCall(inherit(_,Super,LblVr,ThVr),_,C,X,Q,Qx,Pre,Px,Tail,[call(Super,[C,LblVr,ThVr])|Tailx],Pre,Px,Tail,Tailx,_,_,Ex,Ex) :-
  merge([X,LblVr,ThVr],Q,Qx).
implementDotFunCall(_,Rec,C,X,Q,Qx,Pre,APx,Tail,ATail,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  trExp(Rec,Rc,Q,Q0,APx,Px,ATail,[ocall(C,Rc,Rc)|Tailx],Map,Opts,Ex,Exx),
  merge([X],Q0,Qx).

trExpCallOp(v(_,Nm),X,Args,X,Q,Qx,Pre,Px,Tail,[ecall(Nm,XArgs)|Tailx],Pre,Px,Tail,Tailx,_,_,Ex,Ex) :-
  concat([X],Args,XArgs),
  merge([X],Q,Qx),
  isEscape(Nm),!.
trExpCallOp(v(_,Nm),X,Args,Exp,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,_,Ex,Ex) :-
  lookupFunName(Map,Nm,Reslt),
  implementFunCall(Reslt,Nm,X,Args,Exp,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx).
trExpCallOp(dot(_,Rec,Fld),X,Args,Exp,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  concat([X],Args,XArgs),
  merge([X],Q,Q1),
  trCons(Fld,XArgs,Op),
  C = cons(Op,XArgs),
  trExpCallDot(Rec,Rec,C,X,XArgs,Exp,Q1,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx).
trExpCallOp(pkgRef(_,Pkg,Nm),X,Args,X,Q,Qx,Pre,Px,Tail,[call(Fun,XArgs)|Tailx],Pre,Px,Tail,Tailx,Map,_,Ex,Ex) :-
  concat([X],Args,XArgs),
  merge([X],Q,Qx),
  lookupPkgRef(Map,Pkg,Nm,moduleFun(_,Fun)).

implementFunCall(localFun(Fn,LblVr,ThVr),_,X,Args,X,Q,Qx,Pre,Px,Tail,[call(Fn,XArgs)|Tailx],Pre,Px,Tail,Tailx) :-
  concat(Args,[X,LblVr,ThVr],XArgs),
  merge([X,LblVr,ThVr],Q,Qx).
implementFunCall(moduleFun(_,Fn),_,X,Args,X,Q,Qx,Pre,Px,Tail,[call(Fn,XArgs)|Tailx],Pre,Px,Tail,Tailx) :-
  concat(Args,[X],XArgs),
  merge([X],Q,Qx).
implementFunCall(inheritField(Super,LblVr,ThVr),Nm,X,Args,X,Q,Qx,Pre,Px,Tail,[call(Super,[cons(Op,XArgs),LblVr,ThVr])|Tailx],Pre,Px,Tail,Tailx):-
  concat(Args,[X],XArgs),
  trCons(Nm,XArgs,Op),
  merge([X,LblVr,ThVr],Q,Qx).
implementFunCall(moduleClass(_,Mdl,_),_,_,Args,cons(Mdl,Args),Q,Q,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx).
implementFunCall(localClass(Mdl,_,LbVr,ThVr),_,_,Args,cons(Mdl,XArgs),Q,Qx,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx) :-
  concat(Args,[LbVr,ThVr],XArgs),
  merge([LbVr,ThVr],Q,Qx).
implementFunCall(inherit(prg(Mdl,_),_,LbVr,ThVr),_,_,Args,cons(strct(Mdl,Ar),Args),Q,Qx,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx) :-
  merge([LbVr,ThVr],Q,Qx),
  length(Args,Ar).

implementVarExp(localVar(Vn,LblVr,ThVr),_,_,X,X,Q,Qx,Pre,Pre,[call(Vn,[X,LblVr,ThVr])|Tail],Tail) :-
  merge([X,LblVr,ThVr],Q,Qx).
implementVarExp(moduleVar(_,V),_,_,X,X,Q,Qx,Pre,Pre,[call(V,[X])|Tail],Tail) :-
  merge([X],Q,Qx).
implementVarExp(labelArg(N,LblVr,ThVar),_,_,_,idnt(N),Q,Qx,Pre,Pre,Tail,Tail) :-
  merge([idnt(N),LblVr,ThVar],Q,Qx).
implementVarExp(inheritField(Super,LblVr,ThVr),_,Nm,X,X,Q,Qx,
      [call(Super,[cons(strct(Nm,1),[X]),LblVr,ThVr])|Pre],Pre,Tail,Tail) :-
  merge([X,LblVr,ThVr],Q,Qx).
implementVarExp(moduleClass(_,enum(Enum),_),_,_,_,enum(Enum),Q,Q,Pre,Pre,Tail,Tail).
implementVarExp(localClass(Enum,_,LbVr,ThVr),_,_,_,cons(Enum,[LbVr,ThVr]),Q,Qx,Pre,Pre,Tail,Tail) :-
  merge([LbVr,ThVr],Q,Qx).
implementVarExp(inherit(prg(Nm,_),LbVr,ThVr),_,_,_,cons(strct(Nm,2),[LbVr,ThVr]),Q,Qx,Pre,Pre,Tail,Tail) :-
  merge([LbVr,ThVr],Q,Qx).
implementVarExp(notInMap,_,Nm,_,idnt(Nm),Q,Qx,Pre,Pre,Tail,Tail) :-
  merge([idnt(Nm)],Q,Qx).
implementVarExp(_Other,Lc,Nm,_,idnt(Nm),Q,Q,Pre,Pre,Tail,Tail) :-
  reportError("cannot handle %s in expression",[Nm],Lc).

implementPkgRefExp(moduleVar(_,Vn),_,_,_,Xi,Q,[Xi|Q],[call(Vn,[Xi])|Pre],Pre,Post,Post).
implementPkgRefExp(moduleClass(_,enum(Enum),_),_,_,_,enum(Enum),Q,Q,Pre,Pre,Post,Post).

implementPkgRefExp(_,Lc,Pkg,Ref,_,Q,Q,Post,Post) :-
  reportError("illegal access to %s#%s",[Pkg,Ref],Lc).

trGoal(true(_),Goals,Goals,Q,Q,_,_,Ex,Ex) :-!.
trGoal(false(_),[fail|Rest],Rest,Q,Q,_,_,Ex,Ex) :- !.
trGoal(conj(L,R),Goals,Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  trGoal(L,Goals,G0,Q,Q0,Map,Opts,Ex,Ex0),
  trGoal(R,G0,Gx,Q0,Qx,Map,Opts,Ex0,Exx).
trGoal(disj(L,R),[call(DisjPr,LQ)|Gx],Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  trGoal(L,LG,[],[],Q0,Map,Opts,Ex,Ex0),
  trGoal(R,RG,[],Q0,LQ,Map,Opts,Ex0,Ex1),
  genNewName(Map,"or",LQ,DisjPr),
  Cl1 = clse(LQ,DisjPr,LQ,LG),
  Cl2 = clse(LQ,DisjPr,LQ,RG),
  Ex1 = [Cl1,Cl2|Exx],
  merge(LQ,Q,Qx).
trGoal(conditional(T,L,R),[call(CondPr,LQ)|Gx],Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  trGoal(T,TG,[neck|LG],[],Q0,Map,Opts,Ex,Ex0),
  trGoal(L,LG,[],Q0,Q1,Map,Opts,Ex0,Ex1),
  trGoal(R,RG,[],Q1,LQ,Map,Opts,Ex1,Ex2),
  genNewName(Map,"cond",LQ,CondPr),
  Cl1 = clse(LQ,CondPr,LQ,TG),
  Cl2 = clse(LQ,CondPr,LQ,RG),
  Ex2 = [Cl1,Cl2|Exx],
  merge(LQ,Q,Qx).
trGoal(one(T),[call(OnePr,LQ)|Gx],Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  trGoal(T,TG,[neck],[],LQ,Map,Opts,Ex,Ex0),
  genNewName(Map,"one",LQ,OnePr),
  Cl1 = clse(LQ,OnePr,LQ,TG),
  Ex0 = [Cl1|Exx],
  merge(LQ,Q,Qx).
trGoal(neg(T),[call(NegPr,LQ)|Gx],Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  trGoal(T,TG,[neck,fail],[],LQ,Map,Opts,Ex,Ex0),
  genNewName(Map,"neg",LQ,NegPr),
  Cl1 = clse(LQ,NegPr,LQ,TG),
  Cl2 = clse(LQ,NegPr,LQ,[]),
  Ex0 = [Cl1,Cl2|Exx],
  merge(LQ,Q,Qx).
trGoal(forall(L,R),[call(APr,LQ)|Gx],Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  trGoal(L,LG,[call(BPr,LQ),neck,fail],[],Q0,Map,Opts,Ex,Ex0),
  trGoal(R,RG,[neck,fail],Q0,LQ,Map,Opts,Ex0,Ex1),
  genNewName(Map,"forallA",LQ,APr),
  genNewName(Map,"forallB",LQ,BPr),
  ACl1 = clse(LQ,APr,LQ,LG),
  ACl2 = clse(LQ,APr,LQ,[]),
  BCl1 = clse(LQ,BPr,LQ,RG),
  BCl2 = clse(LQ,BPr,LQ,[]),
  Ex1 = [ACl1,ACl2,BCl1,BCl2|Exx],
  merge(LQ,Q,Qx).
trGoal(equals(_,L,R),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  trExp(L,Lx,Q,Q0,G,G0,G0,G1,Map,Opts,Ex,Ex0),
  trExp(R,Rx,Q0,Qx,G1,G2,G2,G3,Map,Opts,Ex0,Exx),
  G3 = [equals(Lx,Rx)|Gx].
trGoal(unify(_,L,R),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  trPtn(L,Lx,Q,Q0,G,G0,G0,G1,Map,Opts,Ex,Ex0),
  trPtn(R,Rx,Q0,Qx,G1,G2,G2,G3,Map,Opts,Ex0,Exx),
  G3 = [equals(Lx,Rx)|Gx].
trGoal(match(_,L,R),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  trPtn(L,Lx,Q,Q0,G,G0,G0,G1,Map,Opts,Ex,Ex0),
  trExp(R,Rx,Q0,Qx,G1,G2,G2,G3,Map,Opts,Ex0,Exx),
  G3 = [equals(Lx,Rx)|Gx].
trGoal(call(_,Pred,Args),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  trExps(Args,AG,Q,Q0,G,Pr,Pr,Post,Map,Opts,Ex,Ex0),
  trGoalCall(Pred,AG,Post,Gx,Q0,Qx,Map,Opts,Ex0,Exx).

trGoalCall(v(_,Nm),Args,[ecall(Nm,Args)|Tail],Tail,Q,Q,_,_,Ex,Ex) :-
  isEscape(Nm),!.
trGoalCall(v(_,Nm),Args,G,Gx,Q,Qx,Map,_,Ex,Ex) :-
  lookupRelName(Map,Nm,RSpec),
  implementGoalCall(RSpec,Nm,Args,G,Gx,Q,Qx).
trGoalCall(dot(_,Rec,Pred),Args,G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  trCons(Pred,Args,Op),
  trGoalDot(Rec,cons(Op,Args),G,Gx,Q,Qx,Map,Opts,Ex,Exx).
trGoalCall(pkgRef(_,Pkg,Rf),Args,[call(Rel,Args)|Gx],Gx,Q,Q,Map,_,Ex,Ex) :-
  lookupPkgRef(Map,Pkg,Rf,moduleRel(_,Rel)).

implementGoalCall(localRel(Fn,LblVr,ThVr),_,Args,[call(Fn,XArgs)|Tail],Tail,Q,Qx) :-
  concat(Args,[LblVr,ThVr],XArgs),
  merge([LblVr,ThVr],Q,Qx).
implementGoalCall(moduleRel(_,Fn),_,Args,[call(Fn,Args)|Tail],Tail,Q,Q).
implementGoalCall(inheritField(Super,LblVr,ThVr),Pred,Args,[call(Super,[cons(Op,Args),LblVr,ThVr])|Tail],Tail,Q,Qx) :-
  trCons(Pred,Args,Op),
  merge([LblVr,ThVr],Q,Qx).
implementGoalCall(_,Pred,G,G,Q,Q) :-
  reportMsg("cannot handle source for %s",[Pred]).

trGoalDot(v(_,Nm),C,[call(Super,[C,LbVr,ThVr])|Gx],Gx,Q,Qx,Map,_,Ex,Ex) :- 
  lookupVarName(Nm,Map,inherit(_,Super,LbVr,ThVr)),!,
  merge([LbVr,ThVr],Q,Qx).
trGoalDot(Rec,C,G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  trExp(Rec,NR,G,G0,G0,G1,Q,Qx,Map,Opts,Ex,Exx),
  G1 = [ocall(C,NR,NR)|Gx].
