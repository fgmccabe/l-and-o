:- module(transform,[transformProg/3]).

:- use_module(canon).
:- use_module(transutils).
:- use_module(errors).
:- use_module(types).
:- use_module(debug).
:- use_module(misc).
:- use_module(escapes).
:- use_module(location).

transformProg(prog(Pkg,Imports,Defs,Others,Fields,Types,Contracts,Impls),
    Opts,export(Pkg,Imports,Fields,Types,Classes,Rules,Contracts,Impls)) :-
  makePkgMap(Pkg,Defs,Types,Imports,Classes,Map),
  pushOpt(Opts,pkgName(Pkg),POpts),
  transformModuleDefs(Defs,Pkg,Map,POpts,R1,Rx,Rx,[]),
  transformOthers(Pkg,Map,POpts,Others,Inits,Rules,R0),
  packageInit(Pkg,Map,POpts,Inits,R0,R1).

transformModuleDefs([],_,_,_,Rules,Rules,Ex,Ex).
transformModuleDefs([Def|Defs],Pkg,Map,Opts,Rules,Rx,Ex,Exx) :-
  transformMdlDef(Def,Pkg,Map,Opts,Rules,R0,Ex,Ex1),
  transformModuleDefs(Defs,Pkg,Map,Opts,R0,Rx,Ex1,Exx).

transformMdlDef(function(Lc,Nm,Tp,Cx,Eqns),Pkg,Map,Opts,Rules,Rx,Ex,Exx) :-
  transformFunction(Pkg,Map,Opts,function(Lc,Nm,Tp,Cx,Eqns),_,Rules,Rx,Ex,Exx).
transformMdlDef(grammar(Lc,Nm,Tp,Cx,Rls),Pkg,Map,Opts,Rules,Rx,Ex,Exx) :-
  transformGrammar(Pkg,Map,Opts,grammar(Lc,Nm,Tp,Cx,Rls),_,Rules,Rx,Ex,Exx).
transformMdlDef(predicate(Lc,Nm,Tp,Cx,Clses),Pkg,Map,Opts,Rules,Rx,Ex,Exx) :-
  transformPredicate(Pkg,Map,Opts,predicate(Lc,Nm,Tp,Cx,Clses),_,Rules,Rx,Ex,Exx).
transformMdlDef(defn(Lc,Nm,_,Cond,_,Value),Pkg,Map,Opts,Rules,Rx,Ex,Exx) :-
  transformDefn(Pkg,Map,Opts,Lc,Nm,_,Cond,Value,Rules,Rx,Ex,Exx).
transformMdlDef(class(Lc,Nm,Tp,Cx,Defs,Face),_,Map,Opts,Rules,Rx,Ex,Exx) :-
  transformClass(Map,Opts,class(Lc,Nm,Tp,Cx,Defs,Face),_,Rules,Rx,_,_,Ex,Exx).
transformMdlDef(enum(Lc,Nm,Tp,Cx,Defs,Face),Prefix,Map,Opts,Rules,Rx,Ex,Exx) :-
  transformEnum(Map,Opts,enum(Lc,Nm,Tp,Cx,Defs,Face),_,Rules,Rx,_,_,Ex,Ex0),
  localName(Prefix,"@",Nm,LclName),
  localName(Prefix,"#",Nm,EnumName),
  Ex0 = [clse([],prg(LclName,1),[enum(EnumName)],[neck])|Exx].
transformMdlDef(typeDef(_,_,_,_),_,_,_,Rules,Rules,Ex,Ex).
transformMdlDef(contract(_,_,_,_,_),_,_,_,Rules,Rules,Ex,Ex).
transformMdlDef(impl(Lc,_,ImplName,_,_,_,Body,_,Face),_,Map,Opts,Rules,Rx,Ex,Exx) :-
  transformImplementation(Lc,ImplName,Body,Face,Map,Opts,Rules,Rx,Ex,Exx).

transformFunction(Prefix,Map,Opts,function(Lc,Nm,Tp,[],Eqns),LclFun,Rules,Rx,Ex,Exx) :-
  localName(Prefix,"@",Nm,LclName),
  typeArity(Tp,Ar),
  Arity is Ar+1,
  LclFun = prg(LclName,Arity),
  pushOpt(Opts,inProg(Nm),FOpts),
  transformEquations(Map,FOpts,LclFun,Eqns,1,_,Rules,R0,Ex,Exx),
  failSafeEquation(Map,FOpts,Lc,LclFun,Tp,R0,Rx).

transformEquations(_,_,_,[],No,No,Rules,Rules,Ex,Ex).
transformEquations(Map,Opts,LclFun,[Eqn|Defs],No,Nx,Rules,Rx,Ex,Exx) :-
  transformEqn(Eqn,Map,Opts,LclFun,No,Rules,R0,Ex,Ex0),
  N1 is No+1,
  transformEquations(Map,Opts,LclFun,Defs,N1,Nx,R0,Rx,Ex0,Exx).

transformEqn(equation(Lc,Nm,A,Cond,Value),Map,Opts,LclFun,QNo,[clse(Q,LclFun,Args,Body)|Rx],Rx,Ex,Exx) :-
  extraVars(Map,Extra),                                   % extra variables coming from labels
  debugPreamble(Nm,Extra,Q0,LbLx,FBg,Opts,ClOpts),        % are we debugging?
  trPtns(A,Args,[Rep|Extra],Q0,Q1,PreG,PreGx,PostG,PreV,Map,ClOpts,Ex,Ex0), % head args
  trGoal(Cond,PreGx,[neck|CGx],Q1,Q2,Map,ClOpts,Ex0,Ex1),   % condition goals
  trExp(Value,Rep,Q2,Q3,PreV,PVx,PVx,Px,Map,ClOpts,Ex1,Exx),  % replacement expression
  labelAccess(Q3,Q,Map,Body,LbLx),                        % generate label access goals
  frameDebug(Nm,QNo,FBg,LG,Q,ClOpts),                     % generate frame entry debugging
  lineDebug(Lc,LG,PreG,ClOpts),                           % line debug after setting up frame
  deframeDebug(Nm,QNo,Px,[],ClOpts),                      % generate frame exit debugging
  breakDebug(Nm,CGx,PostG,ClOpts).                         % generate break point debugging

failSafeEquation(Map,Opts,Lc,LclFun,Tp,[clse([],LclFun,Anons,G)|Rest],Rest) :-
  extraVars(Map,Extra),                                     % extra variables coming from labels
  typeArity(Tp,Ar),
  length(Extra,EA),
  Arity is Ar+1+EA,
  genAnons(Arity,Anons),
  genRaise(Opts,Lc,LclFun,G,[]).

genRaise(_,Lc,LclName,[raise(cons(strct("error",4),[LclName,intgr(Lno),intgr(Off),intgr(Sz)]))|P],P) :-
  lcLine(Lc,Lno),
  lcColumn(Lc,Off),
  lcSize(Lc,Sz).

transformPredicate(Prefix,Map,Opts,predicate(_,Nm,Tp,[],Clses),LclFun,Rules,Rx,Ex,Exx) :-
  localName(Prefix,"@",Nm,LclName),
  typeArity(Tp,Arity),
  LclFun = prg(LclName,Arity),
  pushOpt(Opts,inProg(Nm),POpts),
  transformClauses(Map,POpts,LclFun,Clses,1,_,Rules,Rx,Ex,Exx).

transformClauses(_,_,_,[],No,No,Rules,Rules,Ex,Ex).
transformClauses(Map,Opts,LclFun,[Cl|Defs],No,Nx,Rules,Rx,Ex,Exx) :-
  transformClause(Map,Opts,LclFun,No,Cl,Rules,R0,Ex,Ex0),
  N1 is No+1,
  transformClauses(Map,Opts,LclFun,Defs,N1,Nx,R0,Rx,Ex0,Exx).

transformClause(Map,Opts,LclFun,QNo,clause(Lc,Nm,A,Cond,Body),[clse(Q,LclFun,Args,Goals)|Rx],Rx,Ex,Exx) :-
  extraVars(Map,Extra),                                   % extra variables coming from labels
  debugPreamble(Nm,Extra,Q0,G0,G1,Opts,ClOpts),        % are we debugging?
  trPtns(A,Args,Extra,Q0,Q1,G4,G5,G7,G8,Map,ClOpts,Ex,Ex0), % head args
  trGoal(Cond,G5,G6,Q1,Q2,Map,ClOpts,Ex0,Ex1),       % condition goals
  trGoal(Body,G6,G7,Q2,Q3,Map,ClOpts,Ex1,Exx),
  labelAccess(Q3,Q,Map,Goals,G0),                       % generate label access goals
  frameDebug(Nm,QNo,G1,G2,Q,ClOpts),                     % generate frame entry debugging
  lineDebug(Lc,G2,G3,ClOpts),                           % line debug after setting up frame
  deframeDebug(Nm,QNo,G8,[],ClOpts),                      % generate frame exit debugging
  breakDebug(Nm,G3,G4,ClOpts).                           % generate break point debugging
transformClause(Map,Opts,LclFun,QNo,strong(Lc,Nm,A,Cond,Body),[clse(Q,LclFun,Args,Goals)|Rx],Rx,Ex,Exx) :-
  extraVars(Map,Extra),                                   % extra variables coming from labels
  debugPreamble(Nm,Extra,Q0,G0,G1,Opts,ClOpts),        % are we debugging?
  trPtns(A,Args,Extra,Q0,Q1,G4,G5,G7,G8,Map,ClOpts,Ex,Ex0), % head args
  trGoal(Cond,G5,[neck|G6],Q1,Q2,Map,ClOpts,Ex0,Ex1), % condition goals
  trGoal(Body,G6,G7,Q2,Q3,Map,ClOpts,Ex1,Exx),
  labelAccess(Q3,Q,Map,Goals,G0),                       % generate label access goals
  frameDebug(Nm,QNo,G1,G2,Q,ClOpts),                     % generate frame entry debugging
  lineDebug(Lc,G2,G3,ClOpts),                           % line debug after setting up frame
  deframeDebug(Nm,QNo,G8,[],ClOpts),                      % generate frame exit debugging
  breakDebug(Nm,G3,G4,ClOpts).                           % generate break point debugging

transformDefn(Outer,Map,Opts,Lc,Nm,prg(LclName,1),Cond,Value,
      [clse(Q,prg(LclName,Arity),[Rep|Extra],Body)|Rx],Rx,Ex,Exx) :-
  localName(Outer,"@",Nm,LclName),
  pushOpt(Opts,inProg(Nm),DOpts),
  extraVars(Map,Extra),                                   % extra variables coming from labels
  debugPreamble(Nm,Extra,Q0,G0,G1,DOpts,ClOpts),        % are we debugging?
  trGoal(Cond,G4,[neck|G5],Q0,Q2,Map,ClOpts,Ex,Ex0), % condition goals
  trExp(Value,Rep,Q2,Q3,G5,G6,G6,G7,Map,ClOpts,Ex0,Exx),         % replacement expression
  labelAccess(Q3,Q,Map,Body,G0),                        % generate label access goals
  frameDebug(Nm,0,G1,G2,Q,ClOpts),                       % generate frame entry debugging
  lineDebug(Lc,G2,G3,ClOpts),                         % line debug after setting up frame
  deframeDebug(Nm,0,G7,[],ClOpts),                        % generate frame exit debugging
  length([_|Extra],Arity),
  breakDebug(Nm,G3,G4,ClOpts).                         % generate break point debugging

transformGrammar(Prefix,Map,Opts,grammar(_,Nm,Tp,[],Rls),LclFun,Rules,Rx,Ex,Exx) :-
  localName(Prefix,"@",Nm,LclName),
  typeArity(Tp,Arity),
  LclFun = prg(LclName,Arity),
  pushOpt(Opts,inProg(Nm),POpts),
  transformGrammarRules(Map,POpts,LclFun,Rls,1,_,Rules,Rx,Ex,Exx).

transformGrammarRules(_,_,_,[],No,No,Rules,Rules,Ex,Ex).
transformGrammarRules(Map,Opts,LclFun,[Rl|Defs],No,Nx,Rules,Rx,Ex,Exx) :-
  transformGrammarRule(Rl,Map,Opts,LclFun,No,Rules,R0,Ex,Ex0),
  N1 is No+1,
  transformGrammarRules(Map,Opts,LclFun,Defs,N1,Nx,R0,Rx,Ex0,Exx).

transformGrammarRule(grammarRule(Lc,Nm,A,PB,Body),Map,Opts,LclFun,QNo,
      [clse(Q,LclFun,[StIn,StX|Args],Goals)|Rx],Rx,Ex,Exx) :-
  extraVars(Map,Extra),                                   % extra variables coming from labels
  debugPreamble(Nm,Extra,Q0,G0,G1,Opts,ClOpts),        % are we debugging?
  trPtns(A,Args,Extra,Q0,Q1,G4,G5,G6,G7,Map,ClOpts,Ex,Ex0), % head args
  genVar("StIn",StIn),
  dcgBody(Body,G5,G6,StIn,StOut,[StIn|Q1],Q2,Map,ClOpts,Ex0,Ex1), % grammar body
  pushTerminals(PB,G7,G8,StOut,StX,Q2,Q4,Map,ClOpts,Ex1,Exx),                % push back
  labelAccess(Q4,Q,Map,Goals,G0),                       % generate label access goals
  frameDebug(Nm,QNo,G1,G2,Q,ClOpts),                     % generate frame entry debugging
  lineDebug(Lc,G2,G3,ClOpts),                           % line debug after setting up frame
  deframeDebug(Nm,QNo,G8,[],ClOpts),                     % generate frame exit debugging
  breakDebug(Nm,G3,G4,ClOpts).                          % generate break point debugging

dcgBody(terminals(_,Terms),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  pushTerminals(Terms,G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx).
dcgBody(eof(Lc,StrmVar),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  mkCanon(Lc,Strm,S0),
  trGoal(call(Lc,StrmVar,[S0]),G,G0,Q,Qx,Map,Opts,Ex,Exx),
  joinStream(Strm,Strmx,G0,Gx).
dcgBody(conj(_,Lhs,Rhs),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  dcgBody(Lhs,G,G0,Strm,Strm0,Q,Q0,Map,Opts,Ex,Ex0),
  dcgBody(Rhs,G0,Gx,Strm0,Strmx,Q0,Qx,Map,Opts,Ex0,Exx).
dcgBody(disj(_,Lhs,Rhs),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  dcgDisj(Lhs,Rhs,G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx).
dcgBody(conditional(_,Tst,Lhs,Rhs),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  dcgConditional(Tst,Lhs,Rhs,G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx).
dcgBody(one(_,Tst),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  dcgOne(Tst,G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx).
dcgBody(neg(_,Tst),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  joinStream(Strm,Strmx,G,G0),
  dcgNeg(Tst,G0,Gx,Strm,Q,Qx,Map,Opts,Ex,Exx).
dcgBody(ahead(_,Tst),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  joinStream(Strm,Strmx,G,G0),
  dcgAhead(Tst,G0,Gx,Strm,Q,Qx,Map,Opts,Ex,Exx).
dcgBody(guard(_,Lhs,Rhs),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  dcgBody(Lhs,G,G0,Strm,Strmx,Q,Q0,Map,Opts,Ex,Ex0),
  trGoal(Rhs,G0,Gx,Q0,Qx,Map,Opts,Ex0,Exx).
dcgBody(goal(_,Goal),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  joinStream(Strm,Strmx,G,G0),
  trGoal(Goal,G0,Gx,Q,Qx,Map,Opts,Ex,Exx).
dcgBody(dip(_,V,Cond),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  joinStream(Strm,Strmx,G,G0),
  trExp(V,StrmVr,Q,Q0,G0,G1,G1,[unify(Strm,StrmVr)|G2],Map,Opts,Ex,Ex0),
  trGoal(Cond,G2,Gx,Q0,Qx,Map,Opts,Ex0,Exx).
dcgBody(call(Lc,NT,Args),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  lineDebug(Lc,G,G0,Opts),
  trExps(Args,AG,[],Q,Q0,G0,Pr,Pr,G3,Map,Opts,Ex,Ex0),
  (var(Strmx) -> genVar("Stx",Strmx) ; true),
  trGoalCall(NT,[Strm,Strmx|AG],G3,Gx,Q0,Qx,Map,Opts,Ex0,Exx).

dcgDisj(Lhs,Rhs,[call(DisProg,[Strm,Strmx|DQ])|G],G,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  (var(Strmx) -> genVar("DjOut",Strmx) ; true),
  genVar("DjStrm",DjStrm),
  dcgBody(Lhs,LG,[],DjStrm,DjStrmx,[],LQ,Map,Opts,Ex,Ex0),
  dcgBody(Rhs,RG,[],DjStrm,DjStrmy,LQ,DQ,Map,Opts,Ex0,Ex1),
  genstr("Disj",DisPr),
  length(DQ,Ar),
  Arity is Ar+2,
  trPrg(DisPr,Arity,DisProg),
  C1 = clse([DjStrm|DQ],DisProg,[DjStrm,DjStrmx|DQ],LG),
  C2 = clse([DjStrm|DQ],DisProg,[DjStrm,DjStrmy|DQ],RG),
  Ex1 = [C1,C2|Exx],
  merge(DQ,Q,Qx).

dcgOne(Lhs,[call(OneProg,[Strm,Strmx|DQ])|G],G,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  genVar("OneStm",OneStm),
  (var(Strmx) -> genVar("DjOut",Strmx) ; true),
  dcgBody(Lhs,LG,[neck],OneStm,OneStmx,[],DQ,Map,Opts,Ex,[C1|Exx]),
  genstr("OneGr",OnePr),
  length(DQ,Ar),
  Arity is Ar+2,
  trPrg(OnePr,Arity,OneProg),
  C1 = clse([OneStm|DQ],OneProg,[OneStm,OneStmx|DQ],LG),
  merge(DQ,Q,Qx).

dcgNeg(Tst,[call(NegProg,[Strm|TQ])|G],G,Strm,Q,Qx,Map,Opts,Ex,Exx) :-
  genVar("NegStrm",NegStrm),
  dcgBody(Tst,TG,[neck,fail],NegStrm,_,[],TQ,Map,Opts,Ex,[C1,C2|Exx]),
  genstr("Neg",NegPr),
  length(TQ,Ar),
  Arity is Ar+1,
  trPrg(NegPr,Arity,NegProg),
  C1 = clse([NegStrm|TQ],NegProg,[NegStrm|TQ],TG),
  C2 = clse([NegStrm|TQ],NegProg,[NegStrm|TQ],[]),
  merge(TQ,Q,Qx).

dcgConditional(Tst,Lhs,Rhs,[call(CondProg,[Strm,Strmx|DQ])|G],G,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  (var(Strmx) -> genVar("CndOut",Strmx) ; true),
  genVar("CondStrm",CndStrm),
  dcgBody(Tst,TG,[neck|LG],CndStrm,CndStrm0,[],TQ,Map,Opts,Ex,Ex0),
  dcgBody(Lhs,LG,[],CndStrm0,CndStrmx,TQ,LQ,Map,Opts,Ex0,Ex1),
  dcgBody(Rhs,RG,[],CndStrm,CndStrmy,LQ,DQ,Map,Opts,Ex1,[C1,C2|Exx]),
  genstr("Cond",CondPr),
  length(DQ,Ar),
  Arity is Ar+2,
  trPrg(CondPr,Arity,CondProg),
  C1 = clse([CndStrm|DQ],CondProg,[CndStrm,CndStrmx|DQ],TG),
  C2 = clse([CndStrm|DQ],CondProg,[CndStrm,CndStrmy|DQ],RG),
  merge(DQ,Q,Qx).

dcgAhead(Tst,[call(HdProg,[Strm|TQ])|G],G,Strm,Q,Qx,Map,Opts,Ex,Exx) :-
  genVar("HedStrm",HedStrm),
  dcgBody(Tst,TG,[],HedStrm,_,[],TQ,Map,Opts,Ex,[C1|Exx]),
  genstr("Hed",HedPr),
  length(TQ,Ar),
  Arity is Ar+1,
  trPrg(HedPr,Arity,HdProg),
  C1 = clse([HedStrm|TQ],HdProg,[HedStrm|TQ],TG),
  merge(TQ,Q,Qx).

pushTerminals([],G,Gx,Strm,Strmx,Q,Q,_,_,Ex,Ex) :-
  joinStream(Strm,Strmx,G,Gx).
pushTerminals([term(Lc,SV,T)|More],G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  genVar("NStrm",NStrm),
  mkCanon(Lc,Strm,S0),
  mkCanon(Lc,NStrm,S1),
  trGoal(call(Lc,SV,[S0,T,S1]),G,G0,Q,Q0,Map,Opts,Ex,Ex0),
  pushTerminals(More,G0,Gx,NStrm,Strmx,[NStrm|Q0],Qx,Map,Opts,Ex0,Exx).

mkCanon(Lc,idnt(Nm),v(Lc,Nm)).

joinStream(X,X,G,G).
joinStream(Strm,Strmx,[unify(Strm,Strmx)|Gx],Gx).

transformOthers(_,_,_,[],[neck],Rx,Rx).
transformOthers(Pkg,Map,Opts,[assertion(Lc,G)|Others],[call(AssertName,[])|Inits],Rules,Rx) :-
  collect(Others,canon:isAssertion,Asserts,Rest),
  transformAssertions(Pkg,Map,Opts,Lc,[assertion(Lc,G)|Asserts],AssertName,Rules,R0),
  transformOthers(Pkg,Map,Opts,Rest,Inits,R0,Rx).
transformOthers(Pkg,Map,Opts,[show(Lc,E)|Others],[call(ShowName,[])|Inits],Rules,Rx) :-
  collect(Others,canon:isShow,Shows,Rest),
  transformShows(Pkg,Map,Opts,Lc,[show(Lc,E)|Shows],ShowName,Rules,R0),
  transformOthers(Pkg,Map,Opts,Rest,Inits,R0,Rx).

transformAssertions(Pkg,Map,Opts,Lc,Asserts,prg(LclName,0),Rules,Rx) :-
  rfold(Asserts,transform:collectGoal,true(_),G),
  localName(Pkg,"@","assert",LclName),
  transformClause(Map,Opts,prg(LclName,0),1,clause(Lc,"assert",[],true(''),G),Rules,R0,R0,Rx).

collectGoal(assertion(_,G),true(_),G) :-!.
collectGoal(assertion(_,G),O,conj(O,G)).

transformShows(Pkg,Map,Opts,Lc,Asserts,prg(LclName,0),Rules,Rx) :-
  rfold(Asserts,transform:collectShow,true(_),G),
  localName(Pkg,"@","show",LclName),
  transformClause(Map,Opts,prg(LclName,0),1,clause(Lc,"show",[],true(''),G),Rules,R0,R0,Rx).

collectShow(show(Lc,G),true(_),show(Lc,G)) :-!.
collectShow(show(Lc,G),O,conj(O,show(Lc,G))).

packageInit(Pkg,_,_,Inits,[clse([],prg(InitNm,0),[],Inits)|R],R) :-
  localName(Pkg,"@","init",InitNm).

transformClass(Map,Opts,class(Lc,Nm,_,_,Defs,Face),LblPrg,Rules,Rx,Entry,Entry,Ex,Exx) :-
  labelDefn(Map,Opts,Lc,Nm,LblPrg,LclName,Rules,R0),
  genClassMap(Map,Opts,Lc,LclName,Defs,Face,CMap,R0,En0,Ex,Ex1),!,
  transformClassBody(LclName,Defs,CMap,Opts,En1,Rx,En0,En1,Ex1,Exx).

transformEnum(Map,Opts,enum(Lc,Nm,_,_,Defs,Face),LblPrg,Rules,Rx,Entry,Enx,Ex,Exx) :-
  labelDefn(Map,Opts,Lc,Nm,LblPrg,LclName,Entry,Enx),
  genClassMap(Map,Opts,Lc,LclName,Defs,Face,CMap,Rules,En0,Ex,Ex1),!,
  transformClassBody(LclName,Defs,CMap,Opts,En1,Rx,En0,En1,Ex1,Exx).

labelDefn(Map,Opts,Lc,Nm,LblPrg,LclName,[clse(Q,Access,[cons(Con,[LblTerm])|Extra],Body)|Rx],Rx) :-
  lookupVarName(Map,Nm,Spec),
  trCons(Nm,1,Con),
  makeLabelTerm(Spec,Access,LblTerm,LblPrg),
  LblPrg = prg(LclName,_),
  pushOpt(Opts,inProg(Nm),DOpts),
  extraVars(Map,Extra),                                   % extra variables coming from labels
  debugPreamble(Nm,Extra,Q0,LbLx,FBg,DOpts,ClOpts),       % are we debugging?
  labelAccess(Q0,Q,Map,Body,LbLx),                        % generate label access goals
  frameDebug(Nm,0,FBg,LG,Q,ClOpts),                       % generate frame entry debugging
  lineDebug(Lc,LG,[neck|CGx],ClOpts),                     % line debug after setting up frame
  deframeDebug(Nm,0,Px,[],ClOpts),                        % generate frame exit debugging
  breakDebug(Nm,CGx,Px,ClOpts).                           % generate break point debugging

makeLabelTerm(localClass(LclName,Strct,LblPrg,LblVr,ThVr),prg(LclName,3),cons(Strct,[LblVr,ThVr]),LblPrg).
makeLabelTerm(moduleClass(Access,Strct,LblPrg),Access,cons(Strct,[]),LblPrg).
makeLabelTerm(moduleImpl(Access,Strct,LblPrg),Access,cons(Strct,[]),LblPrg).

findClassBody(Defs,Stmts) :-
  is_member(classBody(_,_,_,Stmts,_,_),Defs),!.
findClassBody(Defs,Stmts) :-
  is_member(implBody(_,_,Stmts,_,_),Defs),!.

/* A class body of the form
lbl(A1,..,Ak)..{
  prog1 :- ...
}
is mapped to 

pkg#lbl(prog1(X1,..,Xm),Lb,Th) :- pkg#lbl@prog(X1,..,Xm,Lb,Th)

together with the specific translations of prog1
*/
transformClassBody(Outer,Defs,Map,Opts,Rules,Rx,Entry,Enx,Ex,Exx) :-
  findClassBody(Defs,Stmts),!,
  transformClassDefs(Outer,Map,Opts,Stmts,Rules,Rx,Entry,Enx,Ex,Exx).
transformClassBody(_,_,_,_,Rules,Rules,Entry,Entry,Ex,Ex).

transformClassDefs(_,_,_,[],Rules,Rules,Entry,Entry,Extra,Extra).
transformClassDefs(Outer,Map,Opts,[Def|Defs],Rules,Rx,Entry,Enx,Ex,Exx) :-
  transformClassDef(Outer,Map,Opts,Def,Rules,R0,Entry,En0,Ex,Ex1),
  transformClassDefs(Outer,Map,Opts,Defs,R0,Rx,En0,Enx,Ex1,Exx).

transformClassDef(Prefix,Map,Opts,function(Lc,Nm,Tp,Cx,Eqns),Rules,Rx,Entry,Enx,Ex,Exx) :-
  transformFunction(Prefix,Map,Opts,function(Lc,Nm,Tp,Cx,Eqns),LclFun,Rules,Rx,Ex,Exx),
  entryClause(Nm,Prefix,LclFun,Entry,Enx).
transformClassDef(Prefix,Map,Opts,predicate(Lc,Nm,Tp,Cx,Clses),Rules,Rx,Entry,Enx,Ex,Exx) :-
  transformPredicate(Prefix,Map,Opts,predicate(Lc,Nm,Tp,Cx,Clses),LclPred,Rules,Rx,Ex,Exx),
  entryClause(Nm,Prefix,LclPred,Entry,Enx).
transformClassDef(Prefix,Map,Opts,defn(Lc,Nm,_,Cond,_,Value),Rules,Rx,Entry,Enx,Ex,Exx) :-
  transformDefn(Prefix,Map,Opts,Lc,Nm,LclDefn,Cond,Value,Rules,Rx,Ex,Exx),
  entryClause(Nm,Prefix,LclDefn,Entry,Enx).
transformClassDef(Prefix,Map,Opts,class(Lc,Nm,Tp,Cx,Defs,Face),Rules,Rx,Entry,Enx,Ex,Exx) :-
  transformClass(Map,Opts,class(Lc,Nm,Tp,Cx,Defs,Face),LclProg,Rules,Rx,Entry,E0,Ex,Exx),
  entryClause(Nm,Prefix,LclProg,E0,Enx).
transformClassDef(Prefix,Map,Opts,enum(Lc,Nm,Tp,Cx,Defs,Face),Rules,Rx,Entry,Enx,Ex,Exx) :-
  transformEnum(Map,Opts,enum(Lc,Nm,Tp,Cx,Defs,Face),LclProg,Rules,Rx,Entry,E0,Ex,Exx),
  entryClause(Nm,Prefix,LclProg,E0,Enx).

entryClause(Name,Prefix,EntryPrg,[clse(Q,prg(Prefix,3),
      [cons(Con,Args),LblVr,ThVr],[neck,call(EntryPrg,Q)])|Rx],Rx) :-
  EntryPrg = prg(_,Arity),
  genVars(Arity,Args),
  genVar("This",ThVr),
  genVar("Lbl",LblVr),
  concat(Args,[LblVr,ThVr],Q),
  trCons(Name,Arity,Con).

transformImplementation(Lc,ImplName,Def,Face,Map,Opts,Rules,Rx,Ex,Exx) :-
  labelDefn(Map,Opts,Lc,ImplName,_,LclName,Rules,R0),
  genClassMap(Map,Opts,Lc,LclName,[Def],Face,CMap,R0,En0,Ex,Ex1),!,
  transformClassBody(LclName,[Def],CMap,Opts,En1,Rx,En0,En1,Ex1,Exx).
  
trPtns([],Args,Args,Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trPtns([P|More],[A|Args],Ax,Q,Qx,Pre,Prx,Post,Psx,Map,Opts,Ex,Exx) :-
  trPtn(P,A,Q,Q0,Pre,Pre0,Post,Pst0,Map,Opts,Ex,Ex0),
  trPtns(More,Args,Ax,Q0,Qx,Pre0,Prx,Pst0,Psx,Map,Opts,Ex0,Exx).

trPtn(v(_,"this"),ThVr,Q,Qx,Pre,Pre,Post,Post,Map,_,Ex,Ex) :- 
  thisVar(Map,ThVr),!,
  merge([ThVr],Q,Qx).
trPtn(v(Lc,"this"),idnt("_"),Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :- !,
  reportError("'this' not defined here",[],Lc).
trPtn(v(Lc,Nm),A,Q,Qx,Pre,Prx,Post,Pstx,Map,Opts,Ex,Ex) :- !,
  trVarPtn(Lc,Nm,A,Q,Qx,Pre,Prx,Post,Pstx,Map,Opts).
trPtn(enum(Lc,Nm),A,Q,Qx,Pre,Prx,Post,Pstx,Map,Opts,Ex,Ex) :- !,
  trVarPtn(Lc,Nm,A,Q,Qx,Pre,Prx,Post,Pstx,Map,Opts).
trPtn(intLit(Ix),intgr(Ix),Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trPtn(floatLit(Ix),float(Ix),Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trPtn(stringLit(Ix),strg(Ix),Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trPtn(pkgRef(Lc,Pkg,Rf),Ptn,Q,Qx,Pre,Pre,Post,Pstx,Map,_,Ex,Ex) :- !,
  lookupPkgRef(Map,Pkg,Rf,Reslt),
  genVar("Xi",Xi),
  implementPkgRefPtn(Reslt,Lc,Pkg,Rf,Xi,Ptn,Q,Qx,Post,Pstx).
trPtn(dot(Rc,Fld),Exp,Q,Qx,Pre,Px,Post,Post,Map,Opts,Ex,Exx) :-
  genVar("XV",X),
  trCons(Fld,[X],S),
  C = cons(S,[X]),
  trDotExp(Rc,C,X,Exp,Q,Qx,Pre,Pi,Pi,Px,Map,Opts,Ex,Exx).
trPtn(tuple(_,Ptns),tpl(P),Q,Qx,Pre,Px,Post,Postx,Map,Opts,Ex,Exx) :-
  trPtns(Ptns,P,[],Q,Qx,Pre,Px,Post,Postx,Map,Opts,Ex,Exx).
trPtn(dict(_,A),Xi,Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx) :-
  genVar("Xi",Xi),
  trEntryPtrns(A,Xi,[Xi|Q],Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx).
trPtn(apply(O,A),Ptn,Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx) :-
  trPtns(A,Args,[],Q,Q0,APre,AP0,APost,APs0,Map,Opts,Ex,Ex0),
  trPtnCallOp(O,Args,Ptn,Q0,Qx,APre,AP0,APost,APs0,Pre,Px,Post,Pstx,Map,Opts,Ex0,Exx).
trPtn(where(P,C),Ptn,Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx) :-
  trPtn(P,Ptn,Q,Q0,Pre,P0,Post,Pstx,Map,Opts,Ex,Ex0),
  trGoal(C,P0,Px,Q0,Qx,Map,Opts,Ex0,Exx).
trPtn(XX,Exp,Q,Qx,Pre,Pre,Post,Postx,Map,Opts,Ex,Exx) :-
  trExp(XX,Exp,Q,Qx,Post,Pi,Pi,Postx,Map,Opts,Ex,Exx).

trVarPtn(_,"_",idnt("_"),Q,Q,Pre,Pre,Post,Post,_,_).
trVarPtn(_,Nm,A,Q,Qx,Pre,Prx,Post,Pstx,Map,_) :-
  lookupVarName(Map,Nm,V),!,
  genVar("X",X),
  implementVarPtn(V,Nm,X,A,Q,Qx,Pre,Prx,Post,Pstx).
trVarPtn(Lc,Nm,idnt("_"),Q,Q,Pre,Pre,Post,Post,_,_) :-
  reportError("'%s' not defined",[Nm],Lc).

implementVarPtn(localVar(Vn,ClVr,TVr),_,X,X,Q,Qx,[call(Vn,[X,ClVr,TVr])|Pre],Pre,Post,Post) :- !, % instance var
  merge([X,ClVr,TVr],Q,Qx).
implementVarPtn(moduleVar(_,Vn),_,X,X,Q,[X|Q],[call(Vn,[X])|Pre],Pre,Post,Post) :- !. % module variable
implementVarPtn(labelArg(N,ClVr,TVr),_,_,N,Q,Qx,Pre,Pre,Post,Post) :- !,    % argument from label
  merge([N,ClVr,TVr],Q,Qx).
implementVarPtn(moduleClass(_,enum(Enum),_),_,_,enum(Enum),Q,Q,Pre,Pre,Post,Post).
implementVarPtn(localClass(_,Enum,_,LbVr,ThVr),_,_,cons(Enum,[LbVr,ThVr]),Q,Qx,Pre,Pre,Post,Post) :-
  merge([LbVr,ThVr],Q,Qx).
implementVarPtn(inherit(Nm,LbVr,ThVr),_,_,cons(strct(Nm,2),[LbVr,ThVr]),Q,Qx,Pre,Pre,Post,Post) :-
  merge([LbVr,ThVr],Q,Qx).
implementVarPtn(notInMap,Nm,_,idnt(Nm),Q,Qx,Pre,Pre,Post,Post) :-                 % variable local to rule
  merge([idnt(Nm)],Q,Qx).

trPtnCallOp(v(_,Nm),Args,X,Q,Qx,Pre,Px,Tail,[ecall(Nm,XArgs)|Tailx],Pre,Px,Tail,Tailx,_,_,Ex,Ex) :-
  concat(Args,[X],XArgs),
  merge([X],Q,Qx),
  isEscape(Nm),!.
trPtnCallOp(v(_,Nm),Args,Ptn,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,_,Ex,Ex) :-
  lookupFunName(Map,Nm,Reslt),
  genVar("X",X),
  implementPtnCall(Reslt,Nm,X,Args,Ptn,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx).

implementPtnCall(localFun(Fn,LblVr,ThVr),_,X,Args,X,Q,Qx,Pre,Px,Tail,[call(Fn,XArgs)|Tailx],Pre,Px,Tail,Tailx) :-
  concat(Args,[X,LblVr,ThVr],XArgs),
  merge([X,LblVr,ThVr],Q,Qx).
implementPtnCall(moduleFun(_,Fn),_,X,Args,X,Q,Qx,Pre,Px,Tail,[call(Fn,XArgs)|Tailx],Pre,Px,Tail,Tailx) :-
  concat(Args,[X],XArgs),
  merge([X],Q,Qx).
implementPtnCall(inheritField(Super,LblVr,ThVr),Nm,X,Args,X,Q,Qx,Pre,Px,Tail,
      [call(Super,[cons(Op,XArgs),LblVr,ThVr])|Tailx],Pre,Px,Tail,Tailx):-
  concat(Args,[X],XArgs),
  trCons(Nm,XArgs,Op),
  merge([X,LblVr,ThVr],Q,Qx).
implementPtnCall(moduleClass(_,Mdl,_),_,_,Args,cons(Mdl,Args),Q,Q,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx).
implementPtnCall(localClass(_,Mdl,_,LbVr,ThVr),_,_,Args,
      cons(Mdl,XArgs),Q,Qx,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx) :-
  concat(Args,[LbVr,ThVr],XArgs),
  merge([LbVr,ThVr],Q,Qx).
implementPtnCall(inherit(Nm,_,LbVr,ThVr),_,_,Args,
      cons(strct(Nm,Ar),Args),Q,Qx,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx) :-
  merge([LbVr,ThVr],Q,Qx),
  length(Args,Ar).
implementPtnCall(moduleImpl(_,Mdl,_),_,_,Args,cons(Mdl,Args),Q,Q,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx).

implementPkgRefPtn(moduleVar(_,Vn),_,_,_,Xi,Xi,Q,[Xi|Q],[call(Vn,[Xi])|Tail],Tail).
implementPkgRefPtn(moduleClass(_,enum(Enum),_),_,_,_,_,enum(Enum),Q,Q,Tail,Tail).
implementPkgRefPtn(_,Lc,Pkg,Rf,_,idnt("_"),Q,Q,Post,Post) :-
  reportError("illegal access to %s#%s",[Pkg,Rf],Lc).

trEntryPtrns([],_,Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex).
trEntryPtrns([(Ky,Vl)|R],Xi,Q,Qx,Pre,Px,Post,Postx,Map,Opts,Ex,Exx):-
  trExp(Ky,KyExp,Q,Q0,Pre,Pre0,Post,Pst0,Map,Opts,Ex,Ex0),
  trPtn(Vl,VlPtn,Q0,Q1,Pre0,Pre1,Pst0,[ocall(cons(Op,XArgs),Xi,Xi)|Pst1],Map,Opts,Ex0,Ex1),
  XArgs = [KyExp,VlPtn],
  trCons("present",XArgs,Op),
  trEntryPtrns(R,Xi,Q1,Qx,Pre1,Px,Pst1,Postx,Map,Opts,Ex1,Exx).

trExps([],Args,Args,Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trExps([P|More],[A|Args],Extra,Q,Qx,Pre,Prx,Post,Psx,Map,Opts,Ex,Exx) :-
  trExp(P,A,Q,Q0,Pre,Pre0,Post,Pst0,Map,Opts,Ex,Ex0),
  trExps(More,Args,Extra,Q0,Qx,Pre0,Prx,Pst0,Psx,Map,Opts,Ex0,Exx).

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
  genVar("Xi",Xi),
  implementPkgRefExp(Reslt,Lc,Pkg,Rf,Xi,Exp,Q,Qx,Post,Pstx).
trExp(tuple(_,A),tpl(TA),Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx) :-
  trExps(A,TA,[],Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx).
trExp(dict(_,A),Exp,Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx) :-
  trMapEntries(A,enum('lo.index#trEmpty'),Exp,Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx).
trExp(apply(Op,A),Exp,Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx) :-
  trExps(A,Args,[],Q,Q0,APre,APx,APost,APostx,Map,Opts,Ex,Ex0),
  genVar("X",X),
  trExpCallOp(Op,X,Args,Exp,Q0,Qx,APre,APx,APost,APostx,Pre,Px,Post,Pstx,Map,Opts,Ex0,Exx).
trExp(dot(Rec,Fld),Exp,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  genVar("XV",X),
  trCons(Fld,[X],S),
  C = cons(S,[X]),
  trDotExp(Rec,C,X,Exp,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx).
trExp(where(P,C),Ptn,Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx) :-
  trExp(P,Ptn,Q,Q0,Pre,P0,Post,Pstx,Map,Opts,Ex,Ex0),
  trGoal(C,P0,Px,Q0,Qx,Map,Opts,Ex0,Exx).
trExp(conditional(Lc,T,L,R),Rslt,Q,Qx,Pre,Prx,Post,Post,Map,Opts,Ex,Exx) :- !,
  genVar("CndV",Rslt),
  lineDebug(Lc,Pre,[call(CondPr,[Rslt|LQ])|Prx],Opts),
  trGoal(T,TG,[neck|LG],[],Q0,Map,Opts,Ex,Ex0),
  trExp(L,LRslt,Q0,Q1,LG,Lx,Lx,[],Map,Opts,Ex0,Ex1),
  trExp(R,RRslt,Q1,LQ,RG,Rx,Rx,[],Map,Opts,Ex1,Ex2),
  genNewName(Map,"condExp",LQ,CondPr),
  Cl1 = clse(LQ,CondPr,[LRslt|LQ],TG),
  Cl2 = clse(LQ,CondPr,[RRslt|LQ],RG),
  Ex2 = [Cl1,Cl2|Exx],
  merge([Rslt|LQ],Q,Qx).
trExp(XX,void,Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-
  reportMsg("internal: cannot transform %s as expression",[XX]).

trDotExp(v(Lc,Nm),C,X,Exp,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  lookupVarName(Map,Nm,Reslt),
  implementDotExp(Reslt,v(Lc,Nm),C,X,Exp,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx).
trDotExp(R,C,X,X,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  trExp(R,Rc,Q,Q0,Pre,Px,Tail,[ocall(C,Rc,Rc)|Tailx],Map,Opts,Ex,Exx),
  merge([X],Q0,Qx).

implementDotExp(inherit(_,Super,ClVr,ThVr),_,C,X,X,Q,Qx,Pre,Pre,[call(Super,[C,ClVr,ThVr])|Tail],Tail,_,_,Ex,Ex) :-
  merge([X,ClVr,ThVr],Q,Qx).
implementDotExp(moduleClass(_,_,LblPrg),R,C,X,X,Q,Qx,Pre,Prx,Tail,Tailx,Map,Opts,Ex,Exx) :-
  trExp(R,Rc,Q,Q0,Pre,Prx,Tail,[call(LblPrg,[C,Rc,Rc])|Tailx],Map,Opts,Ex,Exx),
  merge([X],Q0,Qx).
implementDotExp(_,R,C,X,X,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  trExp(R,Rc,Q,Q0,Pre,Px,Tail,[ocall(C,Rc,Rc)|Tailx],Map,Opts,Ex,Exx),
  merge([X],Q0,Qx).

trVarExp(_,"_",idnt("_"),Q,Q,Pre,Pre,Post,Post,_,_).
trVarExp(Lc,Nm,Exp,Q,Qx,Pre,Prx,Post,Pstx,Map,_) :-
  lookupVarName(Map,Nm,V),!,
  genVar("X",X),
  implementVarExp(V,Lc,Nm,X,Exp,Q,Qx,Pre,Prx,Post,Pstx).
trVarExp(Lc,Nm,idnt("_"),Q,Q,Pre,Pre,Post,Post,_,_) :-
  reportError("'%s' not defined",[Nm],Lc).

trExpCallOp(v(_,Nm),X,Args,X,Q,Qx,Pre,Px,Tail,[ecall(Nm,XArgs)|Tailx],Pre,Px,Tail,Tailx,_,_,Ex,Ex) :-
  concat(Args,[X],XArgs),
  merge([X],Q,Qx),
  isEscape(Nm),!.
trExpCallOp(v(_,Nm),X,Args,Exp,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,_,Ex,Ex) :-
  lookupFunName(Map,Nm,Reslt),
  implementFunCall(Reslt,Nm,X,Args,Exp,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx).
trExpCallOp(dot(Rec,Fld),X,Args,Exp,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  concat(Args,[X],XArgs),
  merge([X],Q,Q1),
  trCons(Fld,XArgs,Op),
  C = cons(Op,XArgs),
  trExpCallDot(Rec,Rec,C,X,Exp,Q1,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx).
trExpCallOp(pkgRef(_,Pkg,Nm),X,Args,X,Q,Qx,Pre,Px,Tail,[call(Fun,XArgs)|Tailx],Pre,Px,Tail,Tailx,Map,_,Ex,Ex) :-
  concat(Args,[X],XArgs),
  merge([X],Q,Qx),
  lookupPkgRef(Map,Pkg,Nm,moduleFun(_,Fun)).

trExpCallDot(v(_,Nm),Rec,C,X,Exp,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  lookupFunName(Map,Nm,Reslt),
  implementDotFunCall(Reslt,Rec,C,X,Exp,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx).
trExpCallDot(_,Rec,C,X,X,Q,Qx,Pre,APx,Tail,[ocall(C,Rc,Rc)|ATlx],Pre,Px,Tail,ATlx,Map,Opts,Ex,Exx) :-
  trExp(Rec,Rc,Q,Qx,APx,Rx,Rx,Px,Map,Opts,Ex,Exx).

implementDotFunCall(inherit(_,Super,LblVr,ThVr),_,C,X,X,Q,Qx,Pre,Px,Tail,
      [call(Super,[C,LblVr,ThVr])|Tailx],Pre,Px,Tail,Tailx,_,_,Ex,Ex) :-
  merge([X,LblVr,ThVr],Q,Qx).
implementDotFunCall(_,Rec,C,X,X,Q,Qx,Pre,APx,Tail,[ocall(C,Rc,Rc)|Tailx],Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  trExp(Rec,Rc,Q,Q0,APx,Rx,Rx,Px,Map,Opts,Ex,Exx),
  merge([X],Q0,Qx).

implementFunCall(localFun(Fn,LblVr,ThVr),_,X,Args,X,Q,Qx,Pre,Px,Tail,[call(Fn,XArgs)|Tailx],Pre,Px,Tail,Tailx) :-
  concat(Args,[X,LblVr,ThVr],XArgs),
  merge([X,LblVr,ThVr],Q,Qx).
implementFunCall(moduleFun(_,Fn),_,X,Args,X,Q,Qx,Pre,Px,Tail,[call(Fn,XArgs)|Tailx],Pre,Px,Tail,Tailx) :-
  concat(Args,[X],XArgs),
  merge([X],Q,Qx).
implementFunCall(inheritField(Super,LblVr,ThVr),Nm,X,Args,X,Q,Qx,Pre,Px,Tail,
      [call(Super,[cons(Op,XArgs),LblVr,ThVr])|Tailx],Pre,Px,Tail,Tailx):-
  concat(Args,[X],XArgs),
  trCons(Nm,XArgs,Op),
  merge([X,LblVr,ThVr],Q,Qx).
implementFunCall(moduleClass(_,Mdl,_),_,_,Args,cons(Mdl,Args),Q,Q,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx).
implementFunCall(localClass(_,Mdl,_,LbVr,ThVr),_,_,Args,cons(Mdl,XArgs),Q,Qx,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx) :-
  concat(Args,[LbVr,ThVr],XArgs),
  merge([LbVr,ThVr],Q,Qx).
implementFunCall(inherit(Mdl,_,LbVr,ThVr),_,_,Args,
      cons(strct(Mdl,Ar),Args),Q,Qx,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx) :-
  merge([LbVr,ThVr],Q,Qx),
  length(Args,Ar).
implementFunCall(moduleImpl(_,Mdl,_),_,_,Args,cons(Mdl,Args),Q,Q,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx).

implementVarExp(localVar(Vn,LblVr,ThVr),_,_,X,X,Q,Qx,[call(Vn,[X,LblVr,ThVr])|Pre],Pre,Tail,Tail) :-
  merge([X,LblVr,ThVr],Q,Qx).
implementVarExp(moduleVar(_,V),_,_,X,X,Q,Qx,[call(V,[X])|Pre],Pre,Tail,Tail) :-
  merge([X],Q,Qx).
implementVarExp(labelArg(N,LblVr,ThVar),_,_,_,N,Q,Qx,Pre,Pre,Tail,Tail) :-
  merge([N,LblVr,ThVar],Q,Qx).
implementVarExp(inheritField(Super,LblVr,ThVr),_,Nm,X,X,Q,Qx,
      [call(Super,[cons(V,[X]),LblVr,ThVr])|Pre],Pre,Tail,Tail) :-
  trCons(Nm,1,V),
  merge([X,LblVr,ThVr],Q,Qx).
implementVarExp(moduleClass(_,enum(Enum),_),_,_,_,enum(Enum),Q,Q,Pre,Pre,Tail,Tail).
implementVarExp(moduleImpl(_,enum(Enum),_),_,_,_,enum(Enum),Q,Q,Pre,Pre,Tail,Tail).
implementVarExp(localClass(_,Enum,_,LbVr,ThVr),_,_,_,cons(Enum,[LbVr,ThVr]),Q,Qx,Pre,Pre,Tail,Tail) :-
  merge([LbVr,ThVr],Q,Qx).
implementVarExp(inherit(Nm,_,LbVr,ThVr),_,_,_,cons(strct(Nm,2),[LbVr,ThVr]),Q,Qx,Pre,Pre,Tail,Tail) :-
  merge([LbVr,ThVr],Q,Qx).
implementVarExp(notInMap,_,Nm,_,idnt(Nm),Q,Qx,Pre,Pre,Tail,Tail) :-
  merge([idnt(Nm)],Q,Qx).
implementVarExp(_Other,Lc,Nm,_,idnt(Nm),Q,Q,Pre,Pre,Tail,Tail) :-
  reportError("cannot handle %s in expression",[Nm],Lc).

implementPkgRefExp(moduleVar(_,Vn),_,_,_,Xi,Q,[Xi|Q],[call(Vn,[Xi])|Pre],Pre,Post,Post).
implementPkgRefExp(moduleClass(_,enum(Enum),_),_,_,_,enum(Enum),Q,Q,Pre,Pre,Post,Post).

implementPkgRefExp(_,Lc,Pkg,Ref,_,Q,Q,Post,Post) :-
  reportError("illegal access to %s#%s",[Pkg,Ref],Lc).

trMapEntries([],Exp,Exp,Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex).
trMapEntries([(Ky,Vl)|R],SoFar,Exp,Q,Qx,Pre,Px,Post,Postx,Map,Opts,Ex,Exx) :-
  trExp(Ky,KyExp,Q,Q0,Pre,P0,Post,Post0,Map,Opts,Ex,Ex0),
  trExp(Vl,VlExp,Q0,Q1,P0,P1,Post0,[ocall(cons(Op,XArgs),SoFar,SoFar)|Post1],Map,Opts,Ex0,Ex1),
  genVar("X",X),
  XArgs = [KyExp,VlExp,X],
  trCons("put",XArgs,Op),
  merge(Q1,[X],Q2),
  trMapEntries(R,X,Exp,Q2,Qx,P1,Px,Post1,Postx,Map,Opts,Ex1,Exx).

trGoal(true(_),Goals,Goals,Q,Q,_,_,Ex,Ex) :-!.
trGoal(false(_),[fail|Rest],Rest,Q,Q,_,_,Ex,Ex) :- !.
trGoal(conj(L,R),Goals,Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  trGoal(L,Goals,G0,Q,Q0,Map,Opts,Ex,Ex0),
  trGoal(R,G0,Gx,Q0,Qx,Map,Opts,Ex0,Exx).
trGoal(disj(Lc,L,R),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  lineDebug(Lc,G,[call(DisjPr,LQ)|Gx],Opts),
  trGoal(L,LG,[],[],Q0,Map,Opts,Ex,Ex0),
  trGoal(R,RG,[],Q0,LQ,Map,Opts,Ex0,Ex1),
  genNewName(Map,"or",LQ,DisjPr),
  Cl1 = clse(LQ,DisjPr,LQ,LG),
  Cl2 = clse(LQ,DisjPr,LQ,RG),
  Ex1 = [Cl1,Cl2|Exx],
  merge(LQ,Q,Qx).
trGoal(conditional(Lc,T,L,R),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  lineDebug(Lc,G,[call(CondPr,LQ)|Gx],Opts),
  trGoal(T,TG,[neck|LG],[],Q0,Map,Opts,Ex,Ex0),
  trGoal(L,LG,[],Q0,Q1,Map,Opts,Ex0,Ex1),
  trGoal(R,RG,[],Q1,LQ,Map,Opts,Ex1,Ex2),
  genNewName(Map,"cond",LQ,CondPr),
  Cl1 = clse(LQ,CondPr,LQ,TG),
  Cl2 = clse(LQ,CondPr,LQ,RG),
  Ex2 = [Cl1,Cl2|Exx],
  merge(LQ,Q,Qx).
trGoal(one(Lc,T),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  lineDebug(Lc,G,[call(OnePr,LQ)|Gx],Opts),
  trGoal(T,TG,[neck],[],LQ,Map,Opts,Ex,Ex0),
  genNewName(Map,"one",LQ,OnePr),
  Cl1 = clse(LQ,OnePr,LQ,TG),
  Ex0 = [Cl1|Exx],
  merge(LQ,Q,Qx).
trGoal(neg(Lc,T),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  lineDebug(Lc,G,[call(NegPr,LQ)|Gx],Opts),
  trGoal(T,TG,[neck,fail],[],LQ,Map,Opts,Ex,Ex0),
  genNewName(Map,"neg",LQ,NegPr),
  Cl1 = clse(LQ,NegPr,LQ,TG),
  Cl2 = clse(LQ,NegPr,LQ,[]),
  Ex0 = [Cl1,Cl2|Exx],
  merge(LQ,Q,Qx).
trGoal(forall(Lc,L,R),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  lineDebug(Lc,G,[call(APr,LQ)|Gx],Opts),
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
trGoal(match(Lc,L,R),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  lineDebug(Lc,G3,[match(Lx,Rx)|Gx],Opts),
  trPtn(L,Lx,Q,Q0,G,G0,G0,G1,Map,Opts,Ex,Ex0),
  trExp(R,Rx,Q0,Qx,G1,G2,G2,G3,Map,Opts,Ex0,Exx).
trGoal(unify(Lc,L,R),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  lineDebug(Lc,G3,[unify(Lx,Rx)|Gx],Opts),
  trExp(L,Lx,Q,Q0,G,G0,G0,G1,Map,Opts,Ex,Ex0),
  trExp(R,Rx,Q0,Qx,G1,G2,G2,G3,Map,Opts,Ex0,Exx).
trGoal(phrase(_,NT,Strm,Rem),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  trExp(Strm,StIn,Q,Q0,G,G0,G0,G1,Map,Opts,Ex,Ex0),
  dcgBody(NT,G1,G2,StIn,StOut,[StIn,StOut|Q0],Q2,Map,Opts,Ex0,Ex1), % grammar body
  trExp(Rem,StOut,Q2,Qx,G2,G3,G3,Gx,Q2,Qx,Map,Opts,Ex1,Exx).
trGoal(phrase(_,NT,Strm),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  trExp(Strm,StIn,Q,Q0,G,G0,G0,G1,Map,Opts,Ex,Ex0),
  dcgBody(NT,G1,Gx,StIn,_,[StIn|Q0],Qx,Map,Opts,Ex0,Exx).
trGoal(show(Lc,Exp),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  trLocation(Lc,Loc,G,G0,Q,Q0,Map,Opts,Ex,Ex0),
  trExp(Exp,Trm,Q0,Qx,G0,G1,G1,[ecall("_display",[Loc,Trm])|Gx],Map,Opts,Ex0,Exx).
trGoal(call(Lc,Pred,Args),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  lineDebug(Lc,G,G0,Opts),
  trExps(Args,AG,[],Q,Q0,G0,Pr,Pr,G3,Map,Opts,Ex,Ex0),
  trGoalCall(Pred,AG,G3,Gx,Q0,Qx,Map,Opts,Ex0,Exx).

trLocation(loc(Ln,Col,_,Sz),tpl([intgr(Ln),intgr(Col),intgr(Sz)]),G,G,Q,Q,_,_,Ex,Ex).

trGoalCall(v(_,Nm),Args,[ecall(Nm,Args)|Tail],Tail,Q,Q,_,_,Ex,Ex) :-
  isEscape(Nm),!.
trGoalCall(v(_,Nm),Args,G,Gx,Q,Qx,Map,_,Ex,Ex) :-
  lookupRelName(Map,Nm,RSpec),
  implementGoalCall(RSpec,Nm,Args,G,Gx,Q,Qx).
trGoalCall(dot(Rec,Pred),Args,G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  trCons(Pred,Args,Op),
  trGoalDot(Rec,cons(Op,Args),G,Gx,Q,Qx,Map,Opts,Ex,Exx).
trGoalCall(pkgRef(_,Pkg,Rf),Args,[call(Rel,Args)|Gx],Gx,Q,Q,Map,_,Ex,Ex) :-
  lookupPkgRef(Map,Pkg,Rf,moduleRel(_,Rel)).

implementGoalCall(localRel(Fn,LblVr,ThVr),_,Args,[call(Fn,XArgs)|Tail],Tail,Q,Qx) :-
  concat(Args,[LblVr,ThVr],XArgs),
  merge([LblVr,ThVr],Q,Qx).
implementGoalCall(moduleRel(_,Fn),_,Args,[call(Fn,Args)|Tail],Tail,Q,Q).
implementGoalCall(inheritField(Super,LblVr,ThVr),Pred,Args,
      [call(Super,[cons(Op,Args),LblVr,ThVr])|Tail],Tail,Q,Qx) :-
  trCons(Pred,Args,Op),
  merge([LblVr,ThVr],Q,Qx).
implementGoalCall(_,Pred,G,G,Q,Q) :-
  reportMsg("cannot handle source for %s",[Pred]).

trGoalDot(v(_,Nm),C,[call(Super,[C,LbVr,ThVr])|Gx],Gx,Q,Qx,Map,_,Ex,Ex) :- 
  lookupVarName(Map,Nm,inherit(_,Super,LbVr,ThVr)),!,
  merge([LbVr,ThVr],Q,Qx).
trGoalDot(Rec,C,G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  trExp(Rec,NR,Q,Qx,G,G0,G0,G1,Map,Opts,Ex,Exx),
  G1 = [ocall(C,NR,NR)|Gx].

genClassMap(Map,Opts,Lc,LclName,Defs,Face,[lyr(LclName,List,Lc,LblGl,LbVr,ThVr)|Map],Entry,En,Ex,Exx) :-
  genVar("LbV",LbVr),
  genVar("ThV",ThVr),
  pickAllFieldsFromFace(Face,Fields),
  makeClassMtdMap(Defs,LclName,LbVr,ThVr,LblGl,[],L0,Fields,Map,Opts,Ex,Ex0),
  makeInheritanceMap(Defs,LclName,LbVr,ThVr,Map,Opts,L0,List,Fields,Entry,En,Ex0,Exx).

pickAllFieldsFromFace(Tp,Fields) :-
  moveQuants(Tp,_,faceType(Fields)).

makeClassMtdMap([],_,_,_,void,List,List,_,_,_,Ex,Ex).
makeClassMtdMap([classBody(_,_,enum(_,_),Stmts,_,_)|Rules],LclName,LbVr,ThVr,LblGl,List,Lx,Fields,Map,Opts,Ex,Exx) :- 
  collectMtds(Stmts,LclName,LbVr,ThVr,List,L0,Fields),
  collectLabelVars([],LbVr,ThVr,L0,L1),
  extraVars(Map,Extra),
  makeLblTerm(enum(LclName),Extra,LblTerm),
  (Extra =[] -> LblGl = [] ; LblGl = [unify(LbVr,LblTerm)]),
  makeClassMtdMap(Rules,LclName,LbVr,ThVr,_,L1,Lx,Fields,Map,Opts,Ex,Exx).
makeClassMtdMap([classBody(_,_,Hd,Stmts,_,_)|Rules],LclName,LbVr,ThVr,LblGl,List,Lx,Fields,Map,Opts,Ex,Exx) :- 
  collectMtds(Stmts,LclName,LbVr,ThVr,List,L0,Fields),
  trPtn(Hd,Lbl,[],Vs,LblGl,Px,Px,[unify(LbVr,LblTerm)],Map,Opts,Ex,Ex0),
  collectLabelVars(Vs,LbVr,ThVr,L0,L1),
  extraVars(Map,Extra),
  makeLblTerm(Lbl,Extra,LblTerm),
  makeClassMtdMap(Rules,LclName,LbVr,ThVr,_,L1,Lx,Fields,Map,Opts,Ex0,Exx).
makeClassMtdMap([labelRule(_,_,_,_,_)|Rules],LclName,LbVr,ThVr,LblGl,List,L0,Fields,Map,Opts,Ex,Exx) :-
  makeClassMtdMap(Rules,LclName,LbVr,ThVr,LblGl,List,L0,Fields,Map,Opts,Ex,Exx).
makeClassMtdMap([implBody(_,enum(_,_),Stmts,_,_)|Rules],LclName,LbVr,ThVr,LblGl,List,Lx,Fields,Map,Opts,Ex,Exx) :- 
  collectMtds(Stmts,LclName,LbVr,ThVr,List,L0,Fields),
  collectLabelVars([],LbVr,ThVr,L0,L1),
  extraVars(Map,Extra),
  makeLblTerm(enum(LclName),Extra,LblTerm),
  (Extra =[] -> LblGl = [] ; LblGl = [unify(LbVr,LblTerm)]),
  makeClassMtdMap(Rules,LclName,LbVr,ThVr,_,L1,Lx,Fields,Map,Opts,Ex,Exx).
makeClassMtdMap([implBody(_,Hd,Stmts,_,_)|Rules],LclName,LbVr,ThVr,LblGl,List,Lx,Fields,Map,Opts,Ex,Exx) :- 
  collectMtds(Stmts,LclName,LbVr,ThVr,List,L0,Fields),
  trPtn(Hd,Lbl,[],Vs,LblGl,Px,Px,[unify(LbVr,LblTerm)],Map,Opts,Ex,Ex0),
  collectLabelVars(Vs,LbVr,ThVr,L0,L1),
  extraVars(Map,Extra),
  makeLblTerm(Lbl,Extra,LblTerm),
  makeClassMtdMap(Rules,LclName,LbVr,ThVr,_,L1,Lx,Fields,Map,Opts,Ex0,Exx).

makeLblTerm(enum(Nm),[],enum(Nm)) :- !.
makeLblTerm(enum(Nm),Extra,cons(strct(Nm,Ar),Extra)) :- !, length(Extra,Ar).
makeLblTerm(cons(strct(Nm,_),Args),Extra,cons(strct(Nm,Arity),As)) :- !,
  concat(Args,Extra,As),
  length(As,Arity).

makeInheritanceMap([],_,_,_,_,_,List,List,_,En,En,Ex,Ex).
makeInheritanceMap([classBody(_,_,_,_,_,_)|Defs],LclName,LbVr,ThVr,Map,Opts,List,Lx,Fields,Entry,En,Ex,Exx) :-
  makeInheritanceMap(Defs,LclName,LbVr,ThVr,Map,Opts,List,Lx,Fields,Entry,En,Ex,Exx).
makeInheritanceMap([implBody(_,_,_,_,_)|Defs],LclName,LbVr,ThVr,Map,Opts,List,Lx,Fields,Entry,En,Ex,Exx) :-
  makeInheritanceMap(Defs,LclName,LbVr,ThVr,Map,Opts,List,Lx,Fields,Entry,En,Ex,Exx).
makeInheritanceMap([labelRule(_,_,P,R,FaceTp)|Defs],LclName,LbVr,ThVr,Map,Opts,List,Lx,Fields,Entry,En,Ex,Exx) :-
  pickAllFieldsFromFace(FaceTp,InhFields),
  extraVars(Map,Extra),
  genVar("CV",CV),
  trPtn(P,Ptn,Extra,Q0,Body,Pre0,Pre0,Prx,Map,Opts,Ex,Ex0),
  trExp(R,Repl,Q0,Q1,Prx,Px,Px,[ocall(CV,Repl,ThVr)],Map,Opts,Ex0,Ex1),
  genstr("^",S),
  string_concat(LclName,S,SuperName),
  Super = prg(SuperName,3),
  merge(Q1,[CV,ThVr],Q),
  Ex1 =  [clse(Q,Super,[CV,Ptn,ThVr],Body)|Ex2],
  makeInheritFields(InhFields,types:isPredType,LclName,Super,Fields,LbVr,ThVr,Entry,En0,List,L1),
  makeInheritanceMap(Defs,LclName,LbVr,ThVr,Map,Opts,L1,Lx,Fields,En0,En,Ex2,Exx).

makeInheritFields([],_,_,_,_,_,_,Entry,Entry,List,List).
makeInheritFields([(Nm,Tp)|InhFields],Test,LclName,Super,Fields,LbVr,ThVr,Entry,En,List,Lx) :-
  is_member((Nm,_),List),
  \+ call(Test,Tp),!,
  makeInheritFields(InhFields,Test,LclName,Super,Fields,LbVr,ThVr,Entry,En,List,Lx).
makeInheritFields([(Nm,Tp)|InhFields],Test,LclName,Super,Fields,LbVr,ThVr,Entry,En,List,Lx) :-
  inheritClause(Nm,Tp,LclName,Super,Entry,En0),
  makeInheritFields(InhFields,Test,LclName,Super,Fields,LbVr,ThVr,En0,En,[(Nm,inheritField(Super,LbVr,ThVr))|List],Lx).

inheritClause(Name,Tp,Prefix,Super,[clse(Q,prg(Prefix,3),[cons(Con,Args),LbVr,ThVr],
      [neck,call(Super,[cons(Con,Args),LbVr,ThVr])])|En],En) :-
  fieldArity(Tp,Arity),
  genVars(Arity,Args),
  genVar("This",ThVr),
  genVar("Lbl",LbVr),
  concat(Args,[LbVr,ThVr],Q),
  trCons(Name,Arity,Con).

fieldArity(Tp,Arity) :- isFunctionType(Tp,Ar), !, Arity is Ar+1.
fieldArity(Tp,Arity) :- isPredType(Tp,Arity),!.
fieldArity(Tp,Arity) :- isClassType(Tp,Ar), !, Arity is Ar+1.
fieldArity(_,1).

collectMtds([],_,_,_,List,List,_).
collectMtds([Entry|Defs],OuterNm,LbVr,ThVr,List,Lx,Fields) :-
  collectMtd(Entry,OuterNm,LbVr,ThVr,List,L0),
  collectMtds(Defs,OuterNm,LbVr,ThVr,L0,Lx,Fields).

collectMtd(function(_,Nm,Tp,_,_),OuterNm,Lbl,ThV,List,[(Nm,localFun(prg(LclName,Arity),Lbl,ThV))|List]) :-
  localName(OuterNm,"@",Nm,LclName),
  typeArity(Tp,Ar),
  Arity is Ar+3.
collectMtd(predicate(_,Nm,Tp,_,_),OuterNm,Lbl,ThV,List,[(Nm,localRel(prg(LclName,Arity),Lbl,ThV))|List]) :-
  localName(OuterNm,"@",Nm,LclName),
  typeArity(Tp,Ar),
  Arity is Ar+2.
collectMtd(grammar(_,Nm,Tp,_,_),OuterNm,Lbl,ThV,List,[(Nm,localRel(prg(LclName,Arity),Lbl,ThV))|List]) :-
  localName(OuterNm,"@",Nm,LclName),
  typeArity(Tp,Ar),
  Arity is Ar+4.
collectMtd(defn(_,Nm,_,_,_,_),OuterNm,Lbl,ThV,List,[(Nm,localVar(prg(LclName,3),Lbl,ThV))|List]) :-
  localName(OuterNm,"@",Nm,LclName).
collectMtd(enum(_,Nm,_,_,_,_),OuterNm,Lbl,ThV,List,
      [(Nm,localClass(OuterNm,strct(LclName,2),prg(LclName,3),Lbl,ThV))|List]) :-
  localName(OuterNm,"@",Nm,LclName).
collectMtd(class(_,Nm,Tp,_,_,_),OuterNm,Lbl,ThV,List,
      [(Nm,localClass(OuterNm,strct(LclName,Arity),prg(LclName,3),Lbl,ThV))|List]) :-
  localName(OuterNm,"@",Nm,LclName),
  typeArity(Tp,Ar),
  Arity is Ar+2.

collectLabelVars([],_,_,List,List).
collectLabelVars([V|Args],LbVr,ThVr,List,Lx) :-
  V=idnt(Nm),
  collectLabelVars(Args,LbVr,ThVr,[(Nm,labelArg(V,LbVr,ThVr))|List],Lx).
