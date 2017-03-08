'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.resolve's'0.0.1'n9o9'()9'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.misc'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.freshen'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.canon'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.unify'e'*'s\"I2'overloadOthers'FT4Lt'lo.comp.canon*canonOther'Lt'lo.comp.types*implEntry't'lo.comp.errors*report't'lo.comp.errors*report'Lt'lo.comp.canon*canonOther''overload'PT6Lt'lo.comp.canon*canonDef'Lt'lo.comp.types*implEntry'Lt'lo.comp.types*implEntry'Lt'lo.comp.canon*canonDef't'lo.comp.errors*report't'lo.comp.errors*report'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.resolve@init'():- !.
'lo.comp.resolve@findImplementation'(XImplName, 'lo.core#,..'(XI, X_35063), XI):- ocall('name%1'(XXV5446),XI,XI),
    XXV5446 = XImplName.
'lo.comp.resolve@findImplementation'(XImplName, 'lo.core#,..'(X_35065, XD), XSpec):- 'lo.comp.resolve@findImplementation'(XImplName, XD, XSpec).
'lo.comp.resolve@formOver'(XV, X_35066, 'lo.core#[]', XV):- !.
'lo.comp.resolve@formOver'(XV, X_35067, XArgs, 'lo.comp.canon#apply'(XLc, XV, 'lo.comp.canon#tpl'(XArgs))):- !.
'lo.comp.resolve@formOver'(_, _, _, _):- raise_exception('error'("lo.comp.resolve@formOver", 201, 3, 21)).
'lo.comp.resolve@resolveDependents'('lo.core#[]', X_35068, X_35069, XArgs, XArgs, XRp, XRp).
'lo.comp.resolve@resolveDependents'('lo.core#,..'(XC, XL), XLc, XDict, XA, XArgs, XRp, XRpx):- 'lo.comp.resolve@resolveContract'(XLc, XC, XDict, XA, XAs, XRp, XRp0),
    'lo.comp.resolve@resolveDependents'(XL, XLc, XDict, XAs, XArgs, XRp0, XRpx).
'lo.comp.resolve@resolve'('lo.comp.canon#implVar'(X_35071, 'lo.comp.canon#v'(XLc, XNm)), X_35072, X_35073, X_35074, 'lo.comp.canon#v'(XLc, XNm), XRp, XRp).
'lo.comp.resolve@resolve'('lo.comp.types#implEntry'(XImpNm, XIC), XC, XLc, XDict, XXb20144, XRp, XRpx):- ocall('freshen%1'(XXV5448),'lo.comp.freshen$freshen$lo.comp.types*constraint','lo.comp.freshen$freshen$lo.comp.types*constraint'),
    ocall('_empty%1'(XXV5447),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%3'(XIC, XXV5447, XXe5078),XXV5448,XXV5448),
    '()2'(X_35075, XImpCon) = XXe5078,
    'lo.comp.types@moveConConstraints'(XImpCon, XCx, XCT),
    'lo.comp.unify@sameContract'(XCT, XC, 'lo.core#[]'),
    'lo.comp.resolve@resolveDependents'(XCx, XLc, XDict, XArgs, 'lo.core#[]', XRp, XRpx),
    'lo.comp.resolve@formOver'('lo.comp.canon#v'(XLc, XImpNm), XLc, XArgs, XXb20144).
'lo.comp.resolve@resolve'(XI, XC, XLc, X_35076, 'lo.comp.canon#v'(XLc, XXa102), XRp, XRpx):- ocall('disp%1'(XXV5449),'lo.core$display$lo.comp.types*constraint','lo.core$display$lo.comp.types*constraint'),
    ocall('_call%2'(XC, XXe5079),XXV5449,XXV5449),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot resolve contract "), 'lo.core#,..'(XXe5079, 'lo.core#[]'))), XXd39876),
    'lo.comp.errors@reportError'(XXd39876, XLc, XRp, XRpx),
    '_str_gen'("void", XXa102).
'lo.comp.resolve@resolveContract'(XLc, XC, XDict, 'lo.core#,..'(XOver, XVs), XVs, XRp, XRpx):- XC = 'lo.comp.types#conTract'(X_35080, X_35081, X_35082),
    'lo.comp.resolve@one297'(XImpl, XDict, XXd39878, XC),
    'lo.comp.resolve@one298'(XRpx, XRp, XOver, XDict, XLc, XC, XImpl).
'lo.comp.resolve@resolveContract'(XLc, 'lo.comp.types#implementsFace'(X_35083, X_35084), XDict, XVs, XVs, XRp, XRp).
'lo.comp.resolve@resolveContract'(XLc, XC, X_35085, XVs, XVs, XRp, XRpx):- ocall('disp%1'(XXV5450),'lo.core$display$lo.comp.types*constraint','lo.core$display$lo.comp.types*constraint'),
    ocall('_call%2'(XC, XXe5080),XXV5450,XXV5450),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("no implementation known for "), 'lo.core#,..'(XXe5080, 'lo.core#[]'))), XXd39883),
    'lo.comp.errors@reportError'(XXd39883, XLc, XRp, XRpx).
'lo.comp.resolve@resolveContracts'(X_35088, 'lo.core#[]', X_35089, 'lo.core#[]', XRp, XRp).
'lo.comp.resolve@resolveContracts'(XLc, 'lo.core#,..'(XCon, XC), XDict, XCV, XRp, XRpx):- 'lo.comp.resolve@resolveContract'(XLc, XCon, XDict, XCV, XVs, XRp, XRp0),
    'lo.comp.resolve@resolveContracts'(XLc, XC, XDict, XVs, XRp0, XRpx).
'lo.comp.resolve@overloadRef'('lo.comp.canon#mtd'(XLc, XNm), 'lo.core#,..'(XDT, 'lo.core#[]'), XRArgs, 'lo.comp.canon#dot'(XLc, XDT, XNm), XRArgs).
'lo.comp.resolve@overloadRef'('lo.comp.canon#v'(XLc, XNm), XDT, XRArgs, 'lo.comp.canon#v'(XLc, XNm), XXb20154):- 'lo.list@<>'(XDT, XRArgs, XXb20154).
'lo.comp.resolve@overloadRef'('lo.comp.canon#mtd'(XLc, XNm), X_35092, XRArgs, 'lo.comp.canon#v'(XLc, XNm), XRArgs).
'lo.comp.resolve@addExtra'('lo.core#[]', XT, XT):- !.
'lo.comp.resolve@addExtra'(XEx, 'lo.comp.canon#tpl'(XA), 'lo.comp.canon#tpl'(XXd39884)):- !,
    'lo.list@<>'(XEx, XA, XXd39884).
'lo.comp.resolve@addExtra'(XEx, 'lo.comp.canon#apply'(XLc, XOp, 'lo.comp.canon#tpl'(XA)), 'lo.comp.canon#apply'(XLc, XOp, 'lo.comp.canon#tpl'(XXd39886))):- !,
    'lo.list@<>'(XEx, XA, XXd39886).
'lo.comp.resolve@addExtra'(_, _, _):- raise_exception('error'("lo.comp.resolve@addExtra", 92, 3, 19)).
'lo.comp.resolve@genVar'(XLc, XNm, 'lo.comp.canon#v'(XLc, XXc531)):- !,
    '_str_gen'(XNm, XXc531).
'lo.comp.resolve@genVar'(_, _, _):- raise_exception('error'("lo.comp.resolve@genVar", 205, 3, 35)).
'lo.comp.resolve@defineCVars'(X_35093, 'lo.core#[]', XDict, 'lo.core#[]', XDict).
'lo.comp.resolve@defineCVars'(XLc, 'lo.core#,..'('lo.comp.types#implementsFace'(X_35095, X_35096), XCx), XDict, XCVars, XFDict):- 'lo.comp.resolve@defineCVars'(XLc, XCx, XDict, XCVars, XFDict).
'lo.comp.resolve@defineCVars'(XLc, 'lo.core#,..'(XCon, XCx), XDict, 'lo.core#,..'(XNV, XCVars), XFDict):- XCon = 'lo.comp.types#conTract'(X_35099, X_35100, X_35101),
    'lo.comp.types@implementationName'(XCon, XXd39891),
    XImplNm = XXd39891,
    'lo.comp.types@contractName'(XCon, XXd39892),
    'lo.comp.resolve@genVar'(XLc, XXd39892, XXd39893),
    XNV = XXd39893,
    'lo.comp.resolve@defineCVars'(XLc, XCx, 'lo.core#,..'('lo.comp.canon#implVar'(XImplNm, XNV), XDict), XCVars, XFDict).
'lo.comp.resolve@overloadFunction'(XLc, XNm, XTp, 'lo.core#[]', XEqns, XDict, XRp, XRpx, 'lo.comp.canon#funDef'(XLc, XNm, XTp, 'lo.core#[]', XXd39896)):- !,
    'lo.comp.resolve@overloadRules'(XEqns, XDict, 'lo.core#[]', XRp, XRpx, XXd39896).
'lo.comp.resolve@overloadFunction'(XLc, XNm, XTp, XCx, XEqns, XDict, XRp, XRpx, 'lo.comp.canon#funDef'(XLc, XNm, XTp, 'lo.core#[]', XXd39898)):- 'lo.comp.resolve@defineCVars'(XLc, XCx, XDict, XCVars, XFDict),
    !,
    'lo.comp.resolve@overloadRules'(XEqns, XFDict, XCVars, XRp, XRpx, XXd39898).
'lo.comp.resolve@overloadFunction'(_, _, _, _, _, _, _, _, _):- raise_exception('error'("lo.comp.resolve@overloadFunction", 34, 3, 104)).
'lo.comp.resolve@overloadPredicate'(XLc, XNm, XTp, 'lo.core#[]', XCls, XDict, XRp, XRpx, 'lo.comp.canon#relDef'(XLc, XNm, XTp, 'lo.core#[]', XXd39900)):- !,
    'lo.comp.resolve@overloadRules'(XCls, XDict, 'lo.core#[]', XRp, XRpx, XXd39900).
'lo.comp.resolve@overloadPredicate'(XLc, XNm, XTp, XCx, XCls, XDict, XRp, XRpx, 'lo.comp.canon#relDef'(XLc, XNm, XTp, 'lo.core#[]', XXd39902)):- 'lo.comp.resolve@defineCVars'(XLc, XCx, XDict, XCVars, XFDict),
    !,
    'lo.comp.resolve@overloadRules'(XCls, XFDict, XCVars, XRp, XRpx, XXd39902).
'lo.comp.resolve@overloadPredicate'(_, _, _, _, _, _, _, _, _):- raise_exception('error'("lo.comp.resolve@overloadPredicate", 39, 3, 103)).
'lo.comp.resolve@overloadDefn'(XLc, XNm, XTp, 'lo.core#[]', XExp, XCond, XDict, XRp, XRpx, 'lo.comp.canon#varDef'(XLc, XNm, XTp, 'lo.core#[]', XRExp, XRCond)):- 'lo.comp.resolve@resolveCond'(XCond, XDict, XRp, XRp0, XXd39904),
    XRCond = XXd39904,
    'lo.comp.resolve@resolveTerm'(XExp, XDict, XRp0, XRpx, XXd39905),
    XRExp = XXd39905,
    !.
'lo.comp.resolve@overloadDefn'(XLc, XNm, XTp, XCx, XExp, XCond, XDict, XRp, XRpx, 'lo.comp.canon#varDef'(XLc, XNm, XTp, 'lo.core#[]', 'lo.comp.canon#lambda'('lo.comp.canon#equation'(XLc, XNm, 'lo.comp.canon#tpl'(XCVars), XRExp, 'lo.comp.canon#trueCond')), XRCond)):- 'lo.comp.resolve@defineCVars'(XLc, XCx, XDict, XCVars, XFDict),
    'lo.comp.resolve@resolveCond'(XCond, XFDict, XRp, XRp0, XXd39907),
    XRCond = XXd39907,
    'lo.comp.resolve@resolveTerm'(XExp, XFDict, XRp0, XRpx, XXd39908),
    XRExp = XXd39908,
    !.
'lo.comp.resolve@overloadDefn'(_, _, _, _, _, _, _, _, _, _):- raise_exception('error'("lo.comp.resolve@overloadDefn", 44, 3, 167)).
'lo.comp.resolve@overloadClass'(XLc, XNm, XTp, XCx, XRules, XFace, XDict, XRp, XRpx, 'lo.comp.canon#classDef'(XLc, XNm, XTp, 'lo.core#[]', XXd39913, XFace)):- 'lo.comp.resolve@defineCVars'(XLc, XCx, XDict, XCVars, XFDict),
    !,
    'lo.comp.resolve@overloadRules'(XRules, XFDict, XCVars, XRp, XRpx, XXd39913).
'lo.comp.resolve@overloadClass'(_, _, _, _, _, _, _, _, _, _):- raise_exception('error'("lo.comp.resolve@overloadClass", 58, 3, 162)).
'lo.comp.resolve@overloadRules'('lo.core#[]', X_35103, X_35104, XRp, XRp, 'lo.core#[]'):- !.
'lo.comp.resolve@overloadRules'('lo.core#,..'(XRl, XL), XDict, XEx, XRp, XRpx, 'lo.core#,..'(XXd39915, XXd39916)):- !,
    'lo.comp.resolve@overloadRule'(XRl, XDict, XEx, XRp, XRp0, XXd39915),
    'lo.comp.resolve@overloadRules'(XL, XDict, XEx, XRp0, XRpx, XXd39916).
'lo.comp.resolve@overloadRules'(_, _, _, _, _, _):- raise_exception('error'("lo.comp.resolve@overloadRules", 77, 3, 33)).
'lo.comp.resolve@overloadGrammar'(XLc, XNm, XTp, 'lo.core#[]', XRules, XDict, XRp, XRpx, 'lo.comp.canon#grammDef'(XLc, XNm, XTp, 'lo.core#[]', XXd39918)):- !,
    'lo.comp.resolve@overloadRules'(XRules, XDict, 'lo.core#[]', XRp, XRpx, XXd39918).
'lo.comp.resolve@overloadGrammar'(XLc, XNm, XTp, XCx, XRules, XDict, XRp, XRpx, 'lo.comp.canon#grammDef'(XLc, XNm, XTp, 'lo.core#[]', XXd39920)):- 'lo.comp.resolve@defineCVars'(XLc, XCx, XDict, XCVars, XFDict),
    !,
    'lo.comp.resolve@overloadRules'(XRules, XFDict, XCVars, XRp, XRpx, XXd39920).
'lo.comp.resolve@overloadGrammar'(_, _, _, _, _, _, _, _, _):- raise_exception('error'("lo.comp.resolve@overloadGrammar", 53, 3, 107)).
'lo.comp.resolve@overloadRule'('lo.comp.canon#equation'(XLc, XNm, XArgs, XRep, XCond), XDict, XEx, XRp, XRpx, 'lo.comp.canon#equation'(XLc, XNm, XXd39924, XXd39925, XRCond)):- 'lo.comp.resolve@resolveCond'(XCond, XDict, XRp, XRp0, XXd39922),
    XXd39922 = XRCond,
    !,
    'lo.comp.resolve@resolveTerm'(XArgs, XDict, XRp0, XRp1, XXd39923),
    'lo.comp.resolve@addExtra'(XEx, XXd39923, XXd39924),
    'lo.comp.resolve@resolveTerm'(XRep, XDict, XRp1, XRpx, XXd39925).
'lo.comp.resolve@overloadRule'('lo.comp.canon#clause'(XLc, XNm, XArgs, XCond), XDict, XEx, XRp, XRpx, 'lo.comp.canon#clause'(XLc, XNm, XXd39928, XXd39929)):- !,
    'lo.comp.resolve@resolveTerm'(XArgs, XDict, XRp, XRp0, XXd39927),
    'lo.comp.resolve@addExtra'(XEx, XXd39927, XXd39928),
    'lo.comp.resolve@resolveCond'(XCond, XDict, XRp0, XRpx, XXd39929).
'lo.comp.resolve@overloadRule'('lo.comp.canon#grRule'(XLc, XNm, XArgs, XHed, XBody), XDict, XEx, XRp, XRpx, 'lo.comp.canon#grRule'(XLc, XNm, XXd39932, XXd39933, XXd39934)):- !,
    'lo.comp.resolve@resolveTerm'(XArgs, XDict, XRp, XRp0, XXd39931),
    'lo.comp.resolve@addExtra'(XEx, XXd39931, XXd39932),
    'lo.comp.resolve@resolveGr'(XHed, XDict, XRp0, XRp1, XXd39933),
    'lo.comp.resolve@resolveGr'(XBody, XDict, XRp1, XRpx, XXd39934).
'lo.comp.resolve@overloadRule'('lo.comp.canon#clRule'(XLc, XNm, XArgs, XRepl, XCond, XTp), XDict, XEx, XRp, XRpx, 'lo.comp.canon#clRule'(XLc, XNm, XXd39937, XXd39938, XXd39939, XTp)):- !,
    'lo.comp.resolve@resolveTerm'(XArgs, XDict, XRp, XRp0, XXd39936),
    'lo.comp.resolve@addExtra'(XEx, XXd39936, XXd39937),
    'lo.comp.resolve@resolveTerm'(XRepl, XDict, XRp0, XRp1, XXd39938),
    'lo.comp.resolve@resolveCond'(XCond, XDict, XRp1, XRpx, XXd39939).
'lo.comp.resolve@overloadRule'(_, _, _, _, _, _):- raise_exception('error'("lo.comp.resolve@overloadRule", 81, 3, 212)).
'lo.comp.resolve@resolveTerminals'('lo.core#[]', X_35107, XRp, XRp, 'lo.core#[]'):- !.
'lo.comp.resolve@resolveTerminals'('lo.core#,..'('()3'(XLc, XOp, XT), XL), XDict, XRp, XRpx, 'lo.core#,..'('()3'(XLc, XXd39941, XXd39942), XXd39943)):- !,
    'lo.comp.resolve@resolveTerm'(XOp, XDict, XRp, XRp0, XXd39941),
    'lo.comp.resolve@resolveTerm'(XT, XDict, XRp0, XRp1, XXd39942),
    'lo.comp.resolve@resolveTerminals'(XL, XDict, XRp1, XRpx, XXd39943).
'lo.comp.resolve@resolveTerminals'(_, _, _, _, _):- raise_exception('error'("lo.comp.resolve@resolveTerminals", 161, 3, 34)).
'lo.comp.resolve@resolveGr'('lo.comp.canon#grTerms'(XTerms), XDict, XRp, XRpx, 'lo.comp.canon#grTerms'(XXd39945)):- !,
    'lo.comp.resolve@resolveTerminals'(XTerms, XDict, XRp, XRpx, XXd39945).
'lo.comp.resolve@resolveGr'('lo.comp.canon#grConj'(XL, XR), XDict, XRp, XRpx, 'lo.comp.canon#grConj'(XXd39947, XXd39948)):- !,
    'lo.comp.resolve@resolveGr'(XL, XDict, XRp, XRp0, XXd39947),
    'lo.comp.resolve@resolveGr'(XR, XDict, XRp0, XRpx, XXd39948).
'lo.comp.resolve@resolveGr'('lo.comp.canon#grDisj'(XL, XR), XDict, XRp, XRpx, 'lo.comp.canon#grDisj'(XXd39950, XXd39951)):- !,
    'lo.comp.resolve@resolveGr'(XL, XDict, XRp, XRp0, XXd39950),
    'lo.comp.resolve@resolveGr'(XR, XDict, XRp0, XRpx, XXd39951).
'lo.comp.resolve@resolveGr'('lo.comp.canon#grCond'(XT, XL, XR), XDict, XRp, XRpx, 'lo.comp.canon#grCond'(XXd39953, XXd39954, XXd39955)):- !,
    'lo.comp.resolve@resolveGr'(XT, XDict, XRp, XRp0, XXd39953),
    'lo.comp.resolve@resolveGr'(XL, XDict, XRp0, XRp1, XXd39954),
    'lo.comp.resolve@resolveGr'(XR, XDict, XRp1, XRpx, XXd39955).
'lo.comp.resolve@resolveGr'('lo.comp.canon#grOne'(XT), XDict, XRp, XRpx, 'lo.comp.canon#grOne'(XXd39957)):- !,
    'lo.comp.resolve@resolveGr'(XT, XDict, XRp, XRpx, XXd39957).
'lo.comp.resolve@resolveGr'('lo.comp.canon#grNeg'(XT), XDict, XRp, XRpx, 'lo.comp.canon#grNeg'(XXd39959)):- !,
    'lo.comp.resolve@resolveGr'(XT, XDict, XRp, XRpx, XXd39959).
'lo.comp.resolve@resolveGr'('lo.comp.canon#grAhed'(XT), XDict, XRp, XRpx, 'lo.comp.canon#grAhed'(XXd39961)):- !,
    'lo.comp.resolve@resolveGr'(XT, XDict, XRp, XRpx, XXd39961).
'lo.comp.resolve@resolveGr'('lo.comp.canon#grDip'(XT, XC), XDict, XRp, XRpx, 'lo.comp.canon#grDip'(XXd39963, XXd39964)):- !,
    'lo.comp.resolve@resolveTerm'(XT, XDict, XRp, XRp0, XXd39963),
    'lo.comp.resolve@resolveCond'(XC, XDict, XRp0, XRpx, XXd39964).
'lo.comp.resolve@resolveGr'('lo.comp.canon#grTest'(XC), XDict, XRp, XRpx, 'lo.comp.canon#grTest'(XXd39966)):- !,
    'lo.comp.resolve@resolveCond'(XC, XDict, XRp, XRpx, XXd39966).
'lo.comp.resolve@resolveGr'('lo.comp.canon#grCall'(XLc0, 'lo.comp.canon#over'(XLc, XT, XCx), 'lo.comp.canon#tpl'(XArgs)), XDict, XRp, XRpx, 'lo.comp.canon#grCall'(XLc0, XOverOp, 'lo.comp.canon#tpl'(XNArgs))):- 'lo.comp.resolve@resolveContracts'(XLc, XCx, XDict, XDterms, XRp, XRp0),
    'lo.comp.resolve@overloadRef'(XT, XDterms, XRArgs, XOverOp, XNArgs),
    'lo.comp.resolve@resolveTerms'(XArgs, XDict, XRp0, XRpx, XXd39968),
    XRArgs = XXd39968,
    !.
'lo.comp.resolve@resolveGr'('lo.comp.canon#grCall'(XLc, XOp, XArgs), XDict, XRp, XRpx, 'lo.comp.canon#grCall'(XLc, XXd39971, XXd39972)):- !,
    'lo.comp.resolve@resolveTerm'(XOp, XDict, XRp, XRp0, XXd39971),
    'lo.comp.resolve@resolveTerm'(XArgs, XDict, XRp, XRpx, XXd39972).
'lo.comp.resolve@resolveGr'(_, _, _, _, _):- raise_exception('error'("lo.comp.resolve@resolveGr", 145, 3, 85)).
'lo.comp.resolve@resolveTerms'('lo.core#[]', X_35110, XRp, XRp, 'lo.core#[]'):- !.
'lo.comp.resolve@resolveTerms'('lo.core#,..'(Xt, Xl), XD, XRp, XRpx, 'lo.core#,..'(XXd39974, XXd39975)):- !,
    'lo.comp.resolve@resolveTerm'(Xt, XD, XRp, XRp0, XXd39974),
    'lo.comp.resolve@resolveTerms'(Xl, XD, XRp0, XRpx, XXd39975).
'lo.comp.resolve@resolveTerms'(_, _, _, _, _):- raise_exception('error'("lo.comp.resolve@resolveTerms", 123, 3, 30)).
'lo.comp.resolve@resolveCond'('lo.comp.canon#trueCond', X_35113, XRp, XRp, 'lo.comp.canon#trueCond'):- !.
'lo.comp.resolve@resolveCond'('lo.comp.canon#falseCond', X_35114, XRp, XRp, 'lo.comp.canon#falseCond'):- !.
'lo.comp.resolve@resolveCond'('lo.comp.canon#conjCond'(XL, XR), XDict, XRp, XRpx, 'lo.comp.canon#conjCond'(XXd39977, XXd39978)):- !,
    'lo.comp.resolve@resolveCond'(XL, XDict, XRp, XRp0, XXd39977),
    'lo.comp.resolve@resolveCond'(XR, XDict, XRp0, XRpx, XXd39978).
'lo.comp.resolve@resolveCond'('lo.comp.canon#disjCond'(XL, XR), XDict, XRp, XRpx, 'lo.comp.canon#disjCond'(XXd39980, XXd39981)):- !,
    'lo.comp.resolve@resolveCond'(XL, XDict, XRp, XRp0, XXd39980),
    'lo.comp.resolve@resolveCond'(XR, XDict, XRp0, XRpx, XXd39981).
'lo.comp.resolve@resolveCond'('lo.comp.canon#forallCond'(XL, XR), XDict, XRp, XRpx, 'lo.comp.canon#forallCond'(XXd39983, XXd39984)):- !,
    'lo.comp.resolve@resolveCond'(XL, XDict, XRp, XRp0, XXd39983),
    'lo.comp.resolve@resolveCond'(XR, XDict, XRp0, XRpx, XXd39984).
'lo.comp.resolve@resolveCond'('lo.comp.canon#condCond'(XT, XL, XR), XDict, XRp, XRpx, 'lo.comp.canon#condCond'(XXd39986, XXd39987, XXd39988)):- !,
    'lo.comp.resolve@resolveCond'(XT, XDict, XRp, XRp0, XXd39986),
    'lo.comp.resolve@resolveCond'(XL, XDict, XRp0, XRp1, XXd39987),
    'lo.comp.resolve@resolveCond'(XR, XDict, XRp1, XRpx, XXd39988).
'lo.comp.resolve@resolveCond'('lo.comp.canon#oneCond'(XT), XDict, XRp, XRpx, 'lo.comp.canon#oneCond'(XXd39990)):- !,
    'lo.comp.resolve@resolveCond'(XT, XDict, XRp, XRpx, XXd39990).
'lo.comp.resolve@resolveCond'('lo.comp.canon#negCond'(XT), XDict, XRp, XRpx, 'lo.comp.canon#negCond'(XXd39992)):- !,
    'lo.comp.resolve@resolveCond'(XT, XDict, XRp, XRpx, XXd39992).
'lo.comp.resolve@resolveCond'('lo.comp.canon#unifyCond'(XLc, XL, XR), XDict, XRp, XRpx, 'lo.comp.canon#unifyCond'(XLc, XXd39994, XXd39995)):- !,
    'lo.comp.resolve@resolveTerm'(XL, XDict, XRp, XRp0, XXd39994),
    'lo.comp.resolve@resolveTerm'(XR, XDict, XRp0, XRpx, XXd39995).
'lo.comp.resolve@resolveCond'('lo.comp.canon#phraseCond'(XLc, XT, XS, XR), XDict, XRp, XRpx, 'lo.comp.canon#phraseCond'(XLc, XXd39997, XXd39998, XXd39999)):- !,
    'lo.comp.resolve@resolveGr'(XT, XDict, XRp, XRp0, XXd39997),
    'lo.comp.resolve@resolveTerm'(XS, XDict, XRp0, XRp1, XXd39998),
    'lo.comp.resolve@resolveTerm'(XR, XDict, XRp1, XRpx, XXd39999).
'lo.comp.resolve@resolveCond'('lo.comp.canon#callCond'(XLc, 'lo.comp.canon#over'(X_35115, XT, XCx), 'lo.comp.canon#tpl'(XArgs)), XDict, XRp, XRpx, 'lo.comp.canon#callCond'(XLc, XOverOp, 'lo.comp.canon#tpl'(XRArgs))):- 'lo.comp.resolve@resolveContracts'(XLc, XCx, XDict, XDTerms, XRp, XRp0),
    'lo.comp.resolve@resolveTerms'(XArgs, XDict, XRp0, XRpx, XXd40001),
    'lo.comp.resolve@overloadRef'(XT, XDTerms, XXd40001, XOverOp, XRArgs),
    !.
'lo.comp.resolve@resolveCond'('lo.comp.canon#callCond'(XLc, XP, XA), XDict, XRp, XRpx, 'lo.comp.canon#callCond'(XLc, XXd40004, XXd40005)):- !,
    'lo.comp.resolve@resolveTerm'(XP, XDict, XRp, XRp0, XXd40004),
    'lo.comp.resolve@resolveTerm'(XA, XDict, XRp0, XRpx, XXd40005).
'lo.comp.resolve@resolveCond'('lo.comp.canon#isTrue'(XC), XDict, XRp, XRpx, 'lo.comp.canon#isTrue'(XXd40007)):- !,
    'lo.comp.resolve@resolveTerm'(XC, XDict, XRp, XRpx, XXd40007).
'lo.comp.resolve@resolveCond'(_, _, _, _, _):- raise_exception('error'("lo.comp.resolve@resolveCond", 127, 3, 41)).
'lo.comp.resolve@overloadOther'('lo.comp.canon#expShow'(XLc, XShow), XDict, XRp, XRpx, 'lo.comp.canon#expShow'(XLc, XXd40009)):- !,
    'lo.comp.resolve@resolveTerm'(XShow, XDict, XRp, XRpx, XXd40009).
'lo.comp.resolve@overloadOther'('lo.comp.canon#integrity'(XLc, XCond), XDict, XRp, XRpx, 'lo.comp.canon#integrity'(XLc, XXd40011)):- !,
    'lo.comp.resolve@resolveCond'(XCond, XDict, XRp, XRpx, XXd40011).
'lo.comp.resolve@overloadOther'(_, _, _, _, _):- raise_exception('error'("lo.comp.resolve@overloadOther", 223, 3, 88)).
'lo.comp.resolve@overloadOthers'('lo.core#[]', X_35116, XRp, XRp, 'lo.core#[]'):- !.
'lo.comp.resolve@overloadOthers'('lo.core#,..'(Xo, Xl), XDict, XRp, XRpx, 'lo.core#,..'(XXd40013, XXd40014)):- !,
    'lo.comp.resolve@overloadOther'(Xo, XDict, XRp, XRp0, XXd40013),
    'lo.comp.resolve@overloadOthers'(Xl, XDict, XRp0, XRpx, XXd40014).
'lo.comp.resolve@overloadOthers'(_, _, _, _, _):- raise_exception('error'("lo.comp.resolve@overloadOthers", 219, 3, 32)).
'lo.comp.resolve@resolveTerm'('lo.comp.canon#v'(XLc, XNm), X_35119, XRp, XRp, 'lo.comp.canon#v'(XLc, XNm)):- !.
'lo.comp.resolve@resolveTerm'('lo.comp.canon#int'(XIx), X_35120, XRp, XRp, 'lo.comp.canon#int'(XIx)):- !.
'lo.comp.resolve@resolveTerm'('lo.comp.canon#flt'(XDx), X_35121, XRp, XRp, 'lo.comp.canon#flt'(XDx)):- !.
'lo.comp.resolve@resolveTerm'('lo.comp.canon#str'(XSx), X_35122, XRp, XRp, 'lo.comp.canon#str'(XSx)):- !.
'lo.comp.resolve@resolveTerm'('lo.comp.canon#dot'(XLc, XRc, XFld), XDict, XRp, XRpx, 'lo.comp.canon#dot'(XLc, XXd40020, XFld)):- !,
    'lo.comp.resolve@resolveTerm'(XRc, XDict, XRp, XRpx, XXd40020).
'lo.comp.resolve@resolveTerm'('lo.comp.canon#tpl'(XArgs), XDict, XRp, XRpx, 'lo.comp.canon#tpl'(XXd40022)):- !,
    'lo.comp.resolve@resolveTerms'(XArgs, XDict, XRp, XRpx, XXd40022).
'lo.comp.resolve@resolveTerm'('lo.comp.canon#whre'(XTrm, XCond), XDict, XRp, XRpx, 'lo.comp.canon#whre'(XXd40024, XXd40025)):- !,
    'lo.comp.resolve@resolveTerm'(XTrm, XDict, XRp, XRp0, XXd40024),
    'lo.comp.resolve@resolveCond'(XCond, XDict, XRp0, XRpx, XXd40025).
'lo.comp.resolve@resolveTerm'('lo.comp.canon#condExp'(XCond, XThen, XElse), XDict, XRp, XRpx, 'lo.comp.canon#condExp'(XXd40027, XXd40028, XXd40029)):- !,
    'lo.comp.resolve@resolveCond'(XCond, XDict, XRp, XRp0, XXd40027),
    'lo.comp.resolve@resolveTerm'(XThen, XDict, XRp0, XRp1, XXd40028),
    'lo.comp.resolve@resolveTerm'(XElse, XDict, XRp1, XRpx, XXd40029).
'lo.comp.resolve@resolveTerm'('lo.comp.canon#apply'(XLc, 'lo.comp.canon#over'(X_35123, XT, XCx), 'lo.comp.canon#tpl'(XArgs)), XDict, XRp, XRpx, 'lo.comp.canon#apply'(XLc, XOverOp, 'lo.comp.canon#tpl'(XRArgs))):- 'lo.comp.resolve@resolveContracts'(XLc, XCx, XDict, XDTerms, XRp, XRp0),
    'lo.comp.resolve@resolveTerms'(XArgs, XDict, XRp0, XRpx, XXd40031),
    'lo.comp.resolve@overloadRef'(XT, XDTerms, XXd40031, XOverOp, XRArgs),
    !.
'lo.comp.resolve@resolveTerm'('lo.comp.canon#apply'(XLc, XOp, XArgs), XDict, XRp, XRpx, 'lo.comp.canon#apply'(XLc, XXd40034, XXd40035)):- !,
    'lo.comp.resolve@resolveTerm'(XOp, XDict, XRp, XRp0, XXd40034),
    'lo.comp.resolve@resolveTerm'(XArgs, XDict, XRp0, XRpx, XXd40035).
'lo.comp.resolve@resolveTerm'('lo.comp.canon#over'(XLc, XT, XCx), XDict, XRp, XRpx, XOver):- 'lo.comp.resolve@cond415'(XXd40044, XXd40043, XXd40042, XXd40041, XXe5081, XXV5451, XXd40040, XXd40039, XRpx, XXd40038, XXd40037, XOver, XNArgs, XOverOp, XT, XRp0, XRp, XDTerms, XDict, XCx, XLc),
    !.
'lo.comp.resolve@resolveTerm'('lo.comp.canon#mtd'(XLc, XNm), X_35126, XRp, XRpx, 'lo.comp.canon#v'(XLc, XNm)):- ocall('disp%1'(XXV5452),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('_call%2'(XNm, XXe5082),XXV5452,XXV5452),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot find implementation for naked method "), 'lo.core#,..'(XXe5082, 'lo.core#[]'))), XXd40049),
    'lo.comp.errors@reportError'(XXd40049, XLc, XRp, XRpx),
    !.
'lo.comp.resolve@resolveTerm'('lo.comp.canon#lambda'(XRl), XDict, XRp, XRpx, 'lo.comp.canon#lambda'(XXd40051)):- !,
    'lo.comp.resolve@overloadRule'(XRl, XDict, 'lo.core#[]', XRp, XRpx, XXd40051).
'lo.comp.resolve@resolveTerm'('lo.comp.canon#theta'(XD, XO), XDict, XRp, XRpx, 'lo.comp.canon#theta'(XXd40053, XXd40054)):- !,
    'lo.comp.resolve@overloadDefs'(XD, XDict, XRp, XRp0, XXd40053),
    'lo.comp.resolve@overloadOthers'(XO, XDict, XRp0, XRpx, XXd40054).
'lo.comp.resolve@resolveTerm'(_, _, _, _, _):- raise_exception('error'("lo.comp.resolve@resolveTerm", 97, 3, 41)).
'lo.comp.resolve@overloadImplementation'(XLc, XINm, XImplName, XSpec, XCx, XHd, XTh, XFace, XDict, XRp, XRpx, 'lo.comp.canon#implDef'(XLc, XINm, XImplName, XSpec, 'lo.core#[]', XXd40057, XXd40058, XFace)):- 'lo.comp.resolve@defineCVars'(XLc, XCx, XDict, XCVars, XFDict),
    !,
    'lo.comp.resolve@resolveTerm'(XHd, XDict, XRp, XRp0, XXd40056),
    'lo.comp.resolve@addExtra'(XCVars, XXd40056, XXd40057),
    'lo.comp.resolve@resolveTerm'(XTh, XFDict, XRp0, XRpx, XXd40058).
'lo.comp.resolve@overloadImplementation'(_, _, _, _, _, _, _, _, _, _, _, _):- raise_exception('error'("lo.comp.resolve@overloadImplementation", 62, 3, 234)).
'lo.comp.resolve@overloadDef'('lo.comp.canon#funDef'(XLc, XNm, XTp, XCx, XEqns), XDict, XRp, XRpx, XXd40060):- !,
    'lo.comp.resolve@overloadFunction'(XLc, XNm, XTp, XCx, XEqns, XDict, XRp, XRpx, XXd40060).
'lo.comp.resolve@overloadDef'('lo.comp.canon#relDef'(XLc, XNm, XTp, XCx, XCls), XDict, XRp, XRpx, XXd40061):- !,
    'lo.comp.resolve@overloadPredicate'(XLc, XNm, XTp, XCx, XCls, XDict, XRp, XRpx, XXd40061).
'lo.comp.resolve@overloadDef'('lo.comp.canon#varDef'(XLc, XNm, XTp, XCx, XValue, XCond), XDict, XRp, XRpx, XXd40062):- !,
    'lo.comp.resolve@overloadDefn'(XLc, XNm, XTp, XCx, XValue, XCond, XDict, XRp, XRpx, XXd40062).
'lo.comp.resolve@overloadDef'('lo.comp.canon#classDef'(XLc, XNm, XTp, XCx, XRules, XFace), XDict, XRp, XRpx, XXd40063):- !,
    'lo.comp.resolve@overloadClass'(XLc, XNm, XTp, XCx, XRules, XFace, XDict, XRp, XRpx, XXd40063).
'lo.comp.resolve@overloadDef'('lo.comp.canon#grammDef'(XLc, XNm, XTp, XCx, XRules), XDict, XRp, XRpx, XXd40064):- !,
    'lo.comp.resolve@overloadGrammar'(XLc, XNm, XTp, XCx, XRules, XDict, XRp, XRpx, XXd40064).
'lo.comp.resolve@overloadDef'(XT, X_35129, XRp, XRp, XT):- XT = 'lo.comp.canon#typeDef'(X_35130, X_35131, X_35132, X_35133),
    !.
'lo.comp.resolve@overloadDef'(XC, X_35134, XRp, XRp, XC):- XC = 'lo.comp.canon#cnDefn'(X_35135, X_35136, X_35137),
    !.
'lo.comp.resolve@overloadDef'('lo.comp.canon#implDef'(XLc, XINm, XImplName, XSpec, XCx, XHd, XTh, XFace), XDict, XRp, XRpx, XXd40067):- !,
    'lo.comp.resolve@overloadImplementation'(XLc, XINm, XImplName, XSpec, XCx, XHd, XTh, XFace, XDict, XRp, XRpx, XXd40067).
'lo.comp.resolve@overloadDef'(_, _, _, _, _):- raise_exception('error'("lo.comp.resolve@overloadDef", 21, 3, 99)).
'lo.comp.resolve@overloadDefs'('lo.core#[]', X_35138, XRp, XRp, 'lo.core#[]'):- !.
'lo.comp.resolve@overloadDefs'('lo.core#,..'(XDf, XL), XDc, XRp, XRpx, 'lo.core#,..'(XXd40068, XXd40069)):- !,
    'lo.comp.resolve@overloadDef'(XDf, XDc, XRp, XRp0, XXd40068),
    'lo.comp.resolve@overloadDefs'(XL, XDc, XRp0, XRpx, XXd40069).
'lo.comp.resolve@overloadDefs'(_, _, _, _, _):- raise_exception('error'("lo.comp.resolve@overloadDefs", 17, 3, 30)).
'lo.comp.resolve@declareImplementations'('lo.core#[]', XDict, XDict).
'lo.comp.resolve@declareImplementations'('lo.core#,..'('lo.comp.canon#implDef'(X_35142, X_35143, XImplName, XSpec, X_35144, X_35145, X_35146, X_35147), XDefs), XDict, XRDict):- 'lo.comp.resolve@declareImplementations'(XDefs, 'lo.core#,..'('lo.comp.types#implEntry'(XImplName, XSpec), XDict), XRDict).
'lo.comp.resolve@declareImplementations'('lo.core#,..'(X_35150, XDefs), XDict, XRDict):- 'lo.comp.resolve@declareImplementations'(XDefs, XDict, XRDict).
'lo.comp.resolve@overload'(XDefs, XDict, XRDict, XXb20226, XRp, XRpx):- 'lo.comp.resolve@declareImplementations'(XDefs, XDict, XRDict),
    'lo.comp.resolve@overloadDefs'(XDefs, XRDict, XRp, XRpx, XXb20226).
'lo.comp.resolve^findImplementation'('_call%3'(XV31818, XV31819, XV31820), 'lo.comp.resolve^findImplementation', _):- 'lo.comp.resolve@findImplementation'(XV31818, XV31819, XV31820).
'lo.comp.resolve^formOver'('_call%4'(XV31821, XV31822, XV31823, XV31824), 'lo.comp.resolve^formOver', _):- 'lo.comp.resolve@formOver'(XV31821, XV31822, XV31823, XV31824).
'lo.comp.resolve^resolveDependents'('_call%7'(XV31825, XV31826, XV31827, XV31828, XV31829, XV31830, XV31831), 'lo.comp.resolve^resolveDependents', _):- 'lo.comp.resolve@resolveDependents'(XV31825, XV31826, XV31827, XV31828, XV31829, XV31830, XV31831).
'lo.comp.resolve^resolve'('_call%7'(XV31832, XV31833, XV31834, XV31835, XV31836, XV31837, XV31838), 'lo.comp.resolve^resolve', _):- 'lo.comp.resolve@resolve'(XV31832, XV31833, XV31834, XV31835, XV31836, XV31837, XV31838).
'lo.comp.resolve@one297'(XImpl, XDict, XXd39878, XC):- 'lo.comp.types@implementationName'(XC, XXd39878),
    'lo.comp.resolve@findImplementation'(XXd39878, XDict, XImpl),
    !.
'lo.comp.resolve@one298'(XRpx, XRp, XOver, XDict, XLc, XC, XImpl):- 'lo.comp.resolve@resolve'(XImpl, XC, XLc, XDict, XOver, XRp, XRpx),
    !.
'lo.comp.resolve^resolveContract'('_call%7'(XV31839, XV31840, XV31841, XV31842, XV31843, XV31844, XV31845), 'lo.comp.resolve^resolveContract', _):- 'lo.comp.resolve@resolveContract'(XV31839, XV31840, XV31841, XV31842, XV31843, XV31844, XV31845).
'lo.comp.resolve^resolveContracts'('_call%6'(XV31846, XV31847, XV31848, XV31849, XV31850, XV31851), 'lo.comp.resolve^resolveContracts', _):- 'lo.comp.resolve@resolveContracts'(XV31846, XV31847, XV31848, XV31849, XV31850, XV31851).
'lo.comp.resolve^overloadRef'('_call%5'(XV31852, XV31853, XV31854, XV31855, XV31856), 'lo.comp.resolve^overloadRef', _):- 'lo.comp.resolve@overloadRef'(XV31852, XV31853, XV31854, XV31855, XV31856).
'lo.comp.resolve^addExtra'('_call%3'(XV31857, XV31858, XV31859), 'lo.comp.resolve^addExtra', _):- 'lo.comp.resolve@addExtra'(XV31857, XV31858, XV31859).
'lo.comp.resolve^genVar'('_call%3'(XV31860, XV31861, XV31862), 'lo.comp.resolve^genVar', _):- 'lo.comp.resolve@genVar'(XV31860, XV31861, XV31862).
'lo.comp.resolve^defineCVars'('_call%5'(XV31863, XV31864, XV31865, XV31866, XV31867), 'lo.comp.resolve^defineCVars', _):- 'lo.comp.resolve@defineCVars'(XV31863, XV31864, XV31865, XV31866, XV31867).
'lo.comp.resolve^overloadFunction'('_call%9'(XV31868, XV31869, XV31870, XV31871, XV31872, XV31873, XV31874, XV31875, XV31876), 'lo.comp.resolve^overloadFunction', _):- 'lo.comp.resolve@overloadFunction'(XV31868, XV31869, XV31870, XV31871, XV31872, XV31873, XV31874, XV31875, XV31876).
'lo.comp.resolve^overloadPredicate'('_call%9'(XV31877, XV31878, XV31879, XV31880, XV31881, XV31882, XV31883, XV31884, XV31885), 'lo.comp.resolve^overloadPredicate', _):- 'lo.comp.resolve@overloadPredicate'(XV31877, XV31878, XV31879, XV31880, XV31881, XV31882, XV31883, XV31884, XV31885).
'lo.comp.resolve^overloadDefn'('_call%10'(XV31886, XV31887, XV31888, XV31889, XV31890, XV31891, XV31892, XV31893, XV31894, XV31895), 'lo.comp.resolve^overloadDefn', _):- 'lo.comp.resolve@overloadDefn'(XV31886, XV31887, XV31888, XV31889, XV31890, XV31891, XV31892, XV31893, XV31894, XV31895).
'lo.comp.resolve^overloadClass'('_call%10'(XV31896, XV31897, XV31898, XV31899, XV31900, XV31901, XV31902, XV31903, XV31904, XV31905), 'lo.comp.resolve^overloadClass', _):- 'lo.comp.resolve@overloadClass'(XV31896, XV31897, XV31898, XV31899, XV31900, XV31901, XV31902, XV31903, XV31904, XV31905).
'lo.comp.resolve^overloadRules'('_call%6'(XV31906, XV31907, XV31908, XV31909, XV31910, XV31911), 'lo.comp.resolve^overloadRules', _):- 'lo.comp.resolve@overloadRules'(XV31906, XV31907, XV31908, XV31909, XV31910, XV31911).
'lo.comp.resolve^overloadGrammar'('_call%9'(XV31912, XV31913, XV31914, XV31915, XV31916, XV31917, XV31918, XV31919, XV31920), 'lo.comp.resolve^overloadGrammar', _):- 'lo.comp.resolve@overloadGrammar'(XV31912, XV31913, XV31914, XV31915, XV31916, XV31917, XV31918, XV31919, XV31920).
'lo.comp.resolve^overloadRule'('_call%6'(XV31921, XV31922, XV31923, XV31924, XV31925, XV31926), 'lo.comp.resolve^overloadRule', _):- 'lo.comp.resolve@overloadRule'(XV31921, XV31922, XV31923, XV31924, XV31925, XV31926).
'lo.comp.resolve^resolveTerminals'('_call%5'(XV31927, XV31928, XV31929, XV31930, XV31931), 'lo.comp.resolve^resolveTerminals', _):- 'lo.comp.resolve@resolveTerminals'(XV31927, XV31928, XV31929, XV31930, XV31931).
'lo.comp.resolve^resolveGr'('_call%5'(XV31932, XV31933, XV31934, XV31935, XV31936), 'lo.comp.resolve^resolveGr', _):- 'lo.comp.resolve@resolveGr'(XV31932, XV31933, XV31934, XV31935, XV31936).
'lo.comp.resolve^resolveTerms'('_call%5'(XV31937, XV31938, XV31939, XV31940, XV31941), 'lo.comp.resolve^resolveTerms', _):- 'lo.comp.resolve@resolveTerms'(XV31937, XV31938, XV31939, XV31940, XV31941).
'lo.comp.resolve^resolveCond'('_call%5'(XV31942, XV31943, XV31944, XV31945, XV31946), 'lo.comp.resolve^resolveCond', _):- 'lo.comp.resolve@resolveCond'(XV31942, XV31943, XV31944, XV31945, XV31946).
'lo.comp.resolve^overloadOther'('_call%5'(XV31947, XV31948, XV31949, XV31950, XV31951), 'lo.comp.resolve^overloadOther', _):- 'lo.comp.resolve@overloadOther'(XV31947, XV31948, XV31949, XV31950, XV31951).
'lo.comp.resolve^overloadOthers'('_call%5'(XV31952, XV31953, XV31954, XV31955, XV31956), 'lo.comp.resolve^overloadOthers', _):- 'lo.comp.resolve@overloadOthers'(XV31952, XV31953, XV31954, XV31955, XV31956).
'lo.comp.resolve@cond414'(XXd40038, XXd40037, XLc, XOverOp, XOver, XNArgs):- XNArgs = 'lo.core#[]',
    !,
    XOver = XOverOp.
'lo.comp.resolve@cond414'(XXd40038, XXd40037, XLc, XOverOp, XOver, XNArgs):- XOver = 'lo.comp.canon#apply'(XLc, XOverOp, 'lo.comp.canon#tpl'(XNArgs)).
'lo.comp.resolve@cond415'(XXd40044, XXd40043, XXd40042, XXd40041, XXe5081, XXV5451, XXd40040, XXd40039, XRpx, XXd40038, XXd40037, XOver, XNArgs, XOverOp, XT, XRp0, XRp, XDTerms, XDict, XCx, XLc):- 'lo.comp.resolve@resolveContracts'(XLc, XCx, XDict, XDTerms, XRp, XRp0),
    !,
    'lo.comp.resolve@overloadRef'(XT, XDTerms, 'lo.core#[]', XOverOp, XNArgs),
    'lo.comp.resolve@cond414'(XXd40038, XXd40037, XLc, XOverOp, XOver, XNArgs),
    XRp0 = XRpx.
'lo.comp.resolve@cond415'(XXd40044, XXd40043, XXd40042, XXd40041, XXe5081, XXV5451, XXd40040, XXd40039, XRpx, XXd40038, XXd40037, XOver, XNArgs, XOverOp, XT, XRp0, XRp, XDTerms, XDict, XCx, XLc):- ocall('disp%1'(XXV5451),'lo.core$display$lo.core*list'('lo.core$display$lo.comp.types*constraint'),'lo.core$display$lo.core*list'('lo.core$display$lo.comp.types*constraint')),
    ocall('_call%2'(XCx, XXe5081),XXV5451,XXV5451),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot find implementation for contracts "), 'lo.core#,..'(XXe5081, 'lo.core#[]'))), XXd40044),
    'lo.comp.errors@reportError'(XXd40044, XLc, XRp0, XRpx),
    XOver = XT.
'lo.comp.resolve^resolveTerm'('_call%5'(XV31957, XV31958, XV31959, XV31960, XV31961), 'lo.comp.resolve^resolveTerm', _):- 'lo.comp.resolve@resolveTerm'(XV31957, XV31958, XV31959, XV31960, XV31961).
'lo.comp.resolve^overloadImplementation'('_call%12'(XV31962, XV31963, XV31964, XV31965, XV31966, XV31967, XV31968, XV31969, XV31970, XV31971, XV31972, XV31973), 'lo.comp.resolve^overloadImplementation', _):- 'lo.comp.resolve@overloadImplementation'(XV31962, XV31963, XV31964, XV31965, XV31966, XV31967, XV31968, XV31969, XV31970, XV31971, XV31972, XV31973).
'lo.comp.resolve^overloadDef'('_call%5'(XV31974, XV31975, XV31976, XV31977, XV31978), 'lo.comp.resolve^overloadDef', _):- 'lo.comp.resolve@overloadDef'(XV31974, XV31975, XV31976, XV31977, XV31978).
'lo.comp.resolve^overloadDefs'('_call%5'(XV31979, XV31980, XV31981, XV31982, XV31983), 'lo.comp.resolve^overloadDefs', _):- 'lo.comp.resolve@overloadDefs'(XV31979, XV31980, XV31981, XV31982, XV31983).
'lo.comp.resolve^declareImplementations'('_call%3'(XV31984, XV31985, XV31986), 'lo.comp.resolve^declareImplementations', _):- 'lo.comp.resolve@declareImplementations'(XV31984, XV31985, XV31986).
'lo.comp.resolve^overload'('_call%6'(XV31987, XV31988, XV31989, XV31990, XV31991, XV31992), 'lo.comp.resolve^overload', _):- 'lo.comp.resolve@overload'(XV31987, XV31988, XV31989, XV31990, XV31991, XV31992).
