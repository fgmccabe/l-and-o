:- module(debug,[debugPreamble/7]).

:- use_module(misc).
:- use_module(location).

debugPreamble(Map,Opts,Ox,Lc,QNo,Pre,Px) :-
  genProfile(Opts,Lc,QNo,Pre,P0),
  genDebug(Map,Opts,Ox,Lc,QNo,P0,Px).

genProfile(Opts,Lc,QNo,[call(prg("lo.profile@event",5),[strg(PkgName),intgr(Lno),intgr(Off),intgr(Sz),intgr(Qno)])|P],P) :-
  is_member(genProfile,Opts),!,
  is_member(pkgName(PkgName),Opts),
  lcLine(Lc,Lno),
  lcColumn(Lc,Off),
  lcSize(lc,Sz).
genProfile(_,_,_,P,P).


% are we debugging?
debugPreamble(Nm,QNo,Q,Qx,PrG,PrGx,Map,Opts,ClOpts) :-
  isDebug(Nm,Opts),!,
  gensym("$D",DgVr),
  gensym("$F",FrVr),
  Prg=[call(prg("go.debug@current",1),[v(DgVr)])|PrGx],
  Qx = [v(DgVr),v(FrVr)|Q],
  pushOpt(dbgVars(Nm,DgVr,FrVr),Opts,ClOpts).
debugPreamble(_,_,Q,Q,P,P,_,Opts,Opts).

frameDebug(Nm,Qno,FB,FBx,Q,Opts) :-
  is_member(dbgVars(Nm,DgVr,FrVr),Opts),!,
  trCons("frame",3,RlNm),
  constructFrameList(Q,FQ),
  FB=[defn(v(FrVr),FQ),
      ocall(cons(RlNm,[stg(Nm),intgr(QNo),v(FrVr)]),[v(DgVr)])|FBx].

deframeDebug(Nm,Qno,FB,FBx,Opts) :-
  is_member(dbgVars(Nm,DgVr,FrVr),Opts),!,
  trCons("deframe",3,RlNm),
  FB=[ocall(cons(RlNm,[stg(Nm),intgr(QNo),v(FrVr)]),[v(DgVr)])|FBx].

constructFrameList([],enum("go.stdlib#[]")).
constructFrameList([v(V)|Vars],FQ) :-
  starts_with(V,"_"),!,
  constructFrameList(Vars,FQ).
constructFrameList([v(V)|Vars],
      cons(strct("go.stdlib#,..",2),[cons(strct("go.stdlib#,",2),[strg(Id),v(Id)]),FQ])) :-
  constructFrameList(Vars,Q).


