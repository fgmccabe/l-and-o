:- module(debug,[debugPreamble/7,frameDebug/6,deframeDebug/5,breakDebug/4,lineDebug/5]).

:- use_module(misc).
:- use_module(location).
:- use_module(transutils).

genProfile(Opts,Lc,QNo,[call(prg("lo.profile@event",5),[strg(PkgName),intgr(Lno),intgr(Off),intgr(Sz),intgr(QNo)])|P],P) :-
  isOption(genProfile,Opts),
  isOption(pkgName(PkgName),Opts),!,
  lcLine(Lc,Lno),
  lcColumn(Lc,Off),
  lcSize(lc,Sz).
genProfile(_,_,_,P,P).

isDebug(Nm,Opts) :-
  isOption(debugging,Opts),
  isOption(inProg(Nm),Opts).

% are we debugging?
debugPreamble(Nm,Q,Qx,[call(prg("go.debug@current",1),[v(DgVr)])|PrGx],PrGx,Opts,ClOpts) :-
  isDebug(Nm,Opts),!,
  gensym("$D",DgVr),
  gensym("$F",FrVr),
  Qx = [v(DgVr),v(FrVr)|Q],
  pushOpt(dbgVars(Nm,DgVr,FrVr),Opts,ClOpts).
debugPreamble(_,Q,Q,P,P,Opts,Opts).

frameDebug(Nm,QNo,FB,FBx,Q,Opts) :-
  isOption(dbgVars(Nm,DgVr,FrVr),Opts),!,
  trCons("frame",3,RlNm),
  constructFrameList(Q,FQ),
  FB=[defn(v(FrVr),FQ),
      ocall(cons(RlNm,[stg(Nm),intgr(QNo),v(FrVr)]),[v(DgVr)])|FBx].
frameDebug(_,_,F,F,_,_).

deframeDebug(Nm,QNo,FB,FBx,Opts) :-
  isOption(dbgVars(Nm,DgVr,FrVr),Opts),!,
  trCons("deframe",3,RlNm),
  FB=[ocall(cons(RlNm,[stg(Nm),intgr(QNo),v(FrVr)]),[v(DgVr)])|FBx].
deframeDebug(_,_,F,F,_).

constructFrameList([],enum("go.stdlib@[]")).
constructFrameList([v(V)|Vars],FQ) :-
  starts_with(V,"_"),!,
  constructFrameList(Vars,FQ).
constructFrameList([v(V)|Vars],
      cons(strct("go.stdlib@,..",2),[cons(strct("go.stdlib@,",2),[strg(V),v(V)]),Q])) :-
  constructFrameList(Vars,Q).

breakDebug(Nm,[ocall(cons(BC,[strg(Nm)]),[DgVr,DgVr])|BG],BG,Opts) :-
  isOption(dbgVars(Nm,DgVr,_),Opts),!,
  trCons("break",1,BC).
breakDebug(_,G,G,_).

lineDebug(Lc,QNo,[ocall(cons(LC,[strg(PkgName),intgr(Lno),intgr(Off),intgr(Sz),intgr(QNo)]),[DgVr,DgVr])|P],P,Opts) :-
  isOption(dbgVars(_,DgVr,_),Opts),
  isOption(pkgName(PkgName),Opts),!,
  trCons("line",5,LC),
  lcLine(Lc,Lno),
  lcColumn(Lc,Off),
  lcSize(lc,Sz).
lineDebug(_,_,P,P,_).

