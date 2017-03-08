'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.transform's'0.0.1'n13o13'()13'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.args'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.canon'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.debug'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.escapes'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.freevars'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.misc'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.transutils'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'s\"I1'transformProg'PT5t'lo.comp.canon*canonPkg't'lo.comp.args*compOption't'lo.comp.term*prProg't'lo.comp.errors*report't'lo.comp.errors*report'\"s'I0'n0o0'()0'n0o0'()0'n1o1'()1'n2o2'()2's'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc's\"c'lo.coerce$coercion'T2t'lo.comp.location*location't'lo.comp.term*tloc'T0\"").
'lo.comp.transform@init'():- !.
'lo.comp.transform@extraArity'(XArity, XVars, XXe4940):- ocall('+%1'(XXV5302),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@length'(XVars, XXd39293),
    ocall('_call%3'(XXd39293, XArity, XXe4940),XXV5302,XXV5302).
'lo.comp.transform@makeLabelTerm'('lo.comp.transutils#localClass'(XLclName, XStrct, X_34119, XLblVr, XThVr), XLclName, 'lo.comp.term#cons'('lo.comp.term#strct'(XStrct, 2), 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XLclName).
'lo.comp.transform@makeLabelTerm'('lo.comp.transutils#moduleClass'(XAccess, XStrct, 0), XAccess, 'lo.comp.term#enum'(XStrct), XAccess).
'lo.comp.transform@makeLabelTerm'('lo.comp.transutils#moduleClass'(XAccess, XStrct, XAr), XAccess, 'lo.comp.term#cons'('lo.comp.term#strct'(XStrct, 0), 'lo.core#[]'), XAccess).
'lo.comp.transform@makeLabelTerm'('lo.comp.transutils#moduleImpl'(XAccess, XStrct), XAccess, XStrct, XAccess).
'lo.comp.transform@labelDefn'(XMap, XNm, XLclName, 'lo.core#,..'('lo.comp.term#clse'(XExtra, 'lo.comp.term#prg'(XAccess, XXe4942), 'lo.core#,..'('lo.comp.term#cons'(XXb19357, 'lo.core#,..'(XLblTerm, 'lo.core#[]')), XExtra), 'lo.core#,..'('lo.comp.term#neck', 'lo.core#[]')), XRx), XRx):- 'lo.comp.transutils@lookupVarName'(XMap, XNm, XSpec),
    'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transform@extraArity'(1, XExtra, XArA),
    'lo.comp.transform@makeLabelTerm'(XSpec, XAccess, XLblTerm, XLclName),
    ocall('size%1'(XXV5303),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%1'(XXV5304),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XExtra, XXe4941),XXV5303,XXV5303),
    ocall('_call%3'(XXe4941, 1, XXe4942),XXV5304,XXV5304),
    'lo.comp.transutils@trCons'(XNm, 1, XXb19357).
'lo.comp.transform@pickAllFieldsFromFace'(XTp, XFields):- 'lo.comp.types@moveQuants'(XTp, X_34126, XQTp),
    'lo.comp.types@moveConstraints'(XQTp, X_34127, 'lo.comp.types#faceType'(XFields)).
'lo.comp.transform@joinStream'(XX, XX, XG, XG).
'lo.comp.transform@joinStream'(XStrm, XStrmx, 'lo.core#,..'('lo.comp.term#unfy'('lo.core#none', XStrm, XStrmx), XGx), XGx).
'lo.comp.transform@mkCanon'('lo.comp.term#varbl'(XNm), 'lo.comp.canon#v'('lo.comp.location#std', XNm)):- !.
'lo.comp.transform@mkCanon'(_, _):- raise_exception('error'("lo.comp.transform@mkCanon", 260, 3, 29)).
'lo.comp.transform@mkClosure'(XLam, 'lo.core#[]', 'lo.comp.term#enum'(XLam)):- !.
'lo.comp.transform@mkClosure'(XLam, XFreeVars, 'lo.comp.term#cons'('lo.comp.term#strct'(XLam, XXe4943), XFreeVars)):- !,
    ocall('size%1'(XXV5305),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('_call%2'(XFreeVars, XXe4943),XXV5305,XXV5305).
'lo.comp.transform@mkClosure'(_, _, _):- raise_exception('error'("lo.comp.transform@mkClosure", 627, 3, 30)).
'lo.comp.transform@genRaise'(XLc, XLclName, 'lo.core#,..'('lo.comp.term#except'('lo.core#some'(XXe4944), 'lo.comp.term#cons'('lo.comp.term#strct'("error", 4), 'lo.core#,..'('lo.comp.term#strng'(XLclName), 'lo.core#,..'('lo.comp.term#intgr'(XXb19370), 'lo.core#,..'('lo.comp.term#intgr'(XXb19372), 'lo.core#,..'('lo.comp.term#intgr'(XXb19374), 'lo.core#[]')))))), XP), XP, XRp, XRp):- ocall('_coerce%1'(XXV5306),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe4944),XXV5306,XXV5306),
    'lo.comp.location@lineOf'(XLc, XXb19370),
    'lo.comp.location@columnOf'(XLc, XXb19372),
    'lo.comp.location@widthOf'(XLc, XXb19374).
'lo.comp.transform@failSafeEquation'(XLc, XNm, XLclPrg, XArity, 'lo.core#,..'('lo.comp.term#clse'('lo.core#[]', 'lo.comp.term#prg'(XLclPrg, XArity), XXb19384, XG), XRest), XRest, XRp, XRpx):- 'lo.comp.transform@genRaise'(XLc, XLclPrg, XG, 'lo.core#[]', XRp, XRpx),
    'lo.comp.transutils@genAnons'(XArity, XXb19384).
'lo.comp.transform@implementFunCall'('lo.comp.transutils#localFun'(XFn, X_34135, X_34136, XAr, XLblVr, XThVr), XLc, X_34137, XX, XArgs, XX, XQ, XXb19391, XPre, XPx, XTail, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XXe4945), 'lo.comp.term#prg'(XFn, XXe4947), XXb19397), XTailx), XPre, XPx, XTail, XTailx, X_34145, X_34146, XEx, XEx, XRp, XRp):- 'lo.list@merge'('lo.core#,..'(XX, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XXb19391),
    ocall('_coerce%1'(XXV5307),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe4945),XXV5307,XXV5307),
    ocall('size%1'(XXV5308),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%1'(XXV5309),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XArgs, XXe4946),XXV5308,XXV5308),
    ocall('_call%3'(XXe4946, 3, XXe4947),XXV5309,XXV5309),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XX, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XXb19397).
'lo.comp.transform@implementFunCall'('lo.comp.transutils#moduleFun'(X_34147, XFn, X_34148, X_34149, XAr), XLc, X_34150, XX, XArgs, XX, XQ, XXb19402, XPre, XPx, XTail, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XXe4948), 'lo.comp.term#prg'(XFn, XXe4950), XXb19406), XTailx), XPre, XPx, XTail, XTailx, X_34154, X_34155, XEx, XEx, XRp, XRp):- 'lo.list@merge'('lo.core#,..'(XX, 'lo.core#[]'), XQ, XXb19402),
    ocall('_coerce%1'(XXV5310),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe4948),XXV5310,XXV5310),
    ocall('size%1'(XXV5311),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%1'(XXV5312),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XArgs, XXe4949),XXV5311,XXV5311),
    ocall('_call%3'(XXe4949, 1, XXe4950),XXV5312,XXV5312),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XX, 'lo.core#[]'), XXb19406).
'lo.comp.transform@implementFunCall'('lo.comp.transutils#inheritField'(XSuper, XLblVr, XThVr), XLc, XNm, XX, XArgs, XX, XQ, XXb19413, XPre, XPx, XTail, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XXe4951), XSuper, 'lo.core#,..'('lo.comp.term#cons'(XXb19415, XXb19417), 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')))), XTailx), XPre, XPx, XTail, XTailx, X_34164, X_34165, XEx, XEx, XRp, XRp):- 'lo.list@merge'('lo.core#,..'(XX, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XXb19413),
    ocall('_coerce%1'(XXV5313),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe4951),XXV5313,XXV5313),
    ocall('+%1'(XXV5314),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@length'(XArgs, XXd39299),
    ocall('_call%3'(XXd39299, 1, XXe4952),XXV5314,XXV5314),
    'lo.comp.transutils@trCons'(XNm, XXe4952, XXb19415),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XX, 'lo.core#[]'), XXb19417).
'lo.comp.transform@implementFunCall'('lo.comp.transutils#moduleClass'(XMdl, X_34166, X_34167), X_34168, X_34169, X_34170, XArgs, 'lo.comp.term#cons'('lo.comp.term#strct'(XMdl, XXe4953), XArgs), XQ, XQ, XPre, XPx, XTail, XTailx, XPre, XPx, XTail, XTailx, X_34171, X_34172, XEx, XEx, XRp, XRp):- ocall('size%1'(XXV5315),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('_call%2'(XArgs, XXe4953),XXV5315,XXV5315).
'lo.comp.transform@implementFunCall'('lo.comp.transutils#localClass'(XMdl, X_34173, X_34174, XLbVr, XThVr), X_34175, X_34176, X_34177, XArgs, 'lo.comp.term#cons'('lo.comp.term#strct'(XMdl, XXe4955), XXb19431), XQ, XXb19435, XPre, XPx, XTail, XTailx, XPre, XPx, XTail, XTailx, X_34182, X_34183, XEx, XEx, XRp, XRp):- ocall('size%1'(XXV5316),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%1'(XXV5317),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XArgs, XXe4954),XXV5316,XXV5316),
    ocall('_call%3'(XXe4954, 2, XXe4955),XXV5317,XXV5317),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XXb19431),
    'lo.list@merge'('lo.core#,..'(XLbvr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XXb19435).
'lo.comp.transform@implementFunCall'('lo.comp.transutils#inherit'(XMdl, X_34184, XLbVr, XThVr), X_34185, X_34186, X_34187, XArgs, 'lo.comp.term#cons'('lo.comp.term#strct'(XMdl, XXe4956), XArgs), XQ, XXb19441, XPre, XPx, XTail, XTailx, XPre, XPx, XTail, XTailx, X_34190, X_34191, XEx, XEx, XRp, XRp):- ocall('size%1'(XXV5318),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('_call%2'(XArgs, XXe4956),XXV5318,XXV5318),
    'lo.list@merge'('lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XXb19441).
'lo.comp.transform@implementFunCall'('lo.comp.transutils#moduleImpl'(X_34192, XMdl), X_34193, X_34194, X_34195, 'lo.core#[]', XMdl, XQ, XQ, XPre, XPx, XTail, XTailx, XPre, XPx, XTail, XTailx, X_34196, X_34197, XEx, XEx, XRp, XRp).
'lo.comp.transform@implementFunCall'('lo.comp.transutils#moduleImpl'(X_34198, XMdl), X_34199, X_34200, X_34201, XArgs, 'lo.comp.term#cons'(XMdl, XArgs), XQ, XQ, XPre, XPx, XTail, XTailx, XPre, XPx, XTail, XTailx, X_34202, X_34203, XEx, XEx, XRp, XRp).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#localVar'(XVn, X_34204, XLblVr, XThVr), XLc, XNm, XX, XQ, XXb19449, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XXe4957), 'lo.comp.term#prg'(XVn, 3), 'lo.core#,..'(XX, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')))), XPre), XPre, XTail, XTail, XRp, XRp):- 'lo.comp.transutils@genVarbl'(XNm, XX),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XXb19449),
    ocall('_coerce%1'(XXV5319),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe4957),XXV5319,XXV5319).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#moduleVar'(X_34212, XV, X_34213), XLc, XNm, XX, XQ, XXb19459, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XXe4958), 'lo.comp.term#prg'(XV, 1), 'lo.core#,..'(XX, 'lo.core#[]')), XPre), XPre, XTail, XTail, XRp, XRp):- 'lo.comp.transutils@genVarbl'(XNm, XX),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#[]'), XQ, XXb19459),
    ocall('_coerce%1'(XXV5320),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe4958),XXV5320,XXV5320).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#labelArg'(XN, XLblVr, XThVar), X_34217, X_34218, XN, XQ, XXb19469, XPre, XPre, XTail, XTail, XRp, XRp):- 'lo.list@merge'('lo.core#,..'(XN, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVar, 'lo.core#[]'))), XQ, XXb19469).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#inheritField'(XSuper, XLblVr, XThVr), XLc, XNm, XX, XQ, XXb19474, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XXe4959), XSuper, 'lo.core#,..'('lo.comp.term#cons'(XXb19476, 'lo.core#,..'(XX, 'lo.core#[]')), 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')))), XPre), XPre, XTail, XTail, XRp, XRp):- 'lo.comp.transutils@genVarbl'(XNm, XX),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XXb19474),
    ocall('_coerce%1'(XXV5321),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe4959),XXV5321,XXV5321),
    'lo.comp.transutils@trCons'(XNm, 1, XXb19476).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#moduleClass'(XEnum, X_34230, 0), X_34231, X_34232, 'lo.comp.term#enum'(XEnum), XQ, XQ, XPre, XPre, XTail, XTail, XRp, XRp).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#moduleImpl'(X_34233, 'lo.comp.term#enum'(XEnum)), X_34234, X_34235, 'lo.comp.term#enum'(XEnum), XQ, XQ, XPre, XPre, XTail, XTail, XRp, XRp).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#localClass'(XEnum, X_34236, X_34237, XLbVr, XThVr), X_34238, X_34239, 'lo.comp.term#cons'('lo.comp.term#strct'(XEnum, 2), 'lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XXb19496, XPre, XPre, XTail, XTail, XRp, XRp):- 'lo.list@merge'('lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XXb19496).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#inherit'(XNm, X_34244, XLbVr, XThVr), X_34245, X_34246, 'lo.comp.term#cons'('lo.comp.term#strct'(XNm, 2), 'lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XXb19504, XPre, XPre, XTail, XTail, XRp, XRp):- 'lo.list@merge'('lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XXb19504).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#notInMap', X_34251, XNm, 'lo.comp.term#varbl'(XNm), XQ, XXb19508, XPre, XPre, XTail, XTail, XRp, XRp):- 'lo.list@merge'('lo.core#,..'('lo.comp.term#varbl'(XNm), 'lo.core#[]'), XQ, XXb19508).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#moduleFun'(X_34253, X_34254, X_34255, XAcc, X_34256), X_34257, X_34258, 'lo.comp.term#enum'(XAcc), XQ, XQ, XPre, XPre, XTail, XTail, XRp, XRp).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#moduleRel'(X_34259, X_34260, X_34261, XAcc, X_34262), X_34263, X_34264, 'lo.comp.term#enum'(XAcc), XQ, XQ, XPre, XPre, XTail, XTail, XRp, XRp).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#localFun'(X_34265, X_34266, XClosure, X_34267, XLblVr, XThVr), X_34268, X_34269, 'lo.comp.term#cons'('lo.comp.term#strct'(XClosure, 2), 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XQ, XPre, XPre, XTail, XTail, XRp, XRp).
'lo.comp.transform@implementVarExp'(X_34272, XLc, XNm, 'lo.comp.term#varbl'(XNm), XQ, XQ, XPre, XPre, XTail, XTail, XRp, XRpx):- ocall('disp%1'(XXV5322),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('_call%2'(XNm, XXe4960),XXV5322,XXV5322),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot handle "), 'lo.core#,..'(XXe4960, 'lo.core#,..'('lo.core#ss'(" in expression"), 'lo.core#[]')))), XXd39306),
    'lo.comp.errors@reportError'(XXd39306, XLc, XRp, XRpx).
'lo.comp.transform@trVarExp'(X_34276, "_", 'lo.comp.term#anon', XQ, XQ, XPre, XPre, XPost, XPost, X_34277, X_34278, XRp, XRp).
'lo.comp.transform@trVarExp'(XLc, XNm, XExp, XQ, XQx, XPre, XPrx, XPost, XPstx, XMap, X_34279, XRp, XRpx):- 'lo.comp.transutils@lookupVarName'(XMap, XNm, XV),
    'lo.comp.transform@implementVarExp'(XV, XLc, XNm, XExp, XQ, XQx, XPre, XPrx, XPost, XPstx, XRp, XRpx).
'lo.comp.transform@trVarExp'(XLc, XNm, 'lo.comp.term#anon', XQ, XQ, XPre, XPre, XPost, XPost, X_34280, X_34281, XRp, XRpx):- ocall('disp%1'(XXV5323),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('_call%2'(XNm, XXe4961),XXV5323,XXV5323),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("'"), 'lo.core#,..'(XXe4961, 'lo.core#,..'('lo.core#ss'("' not defined"), 'lo.core#[]')))), XXd39313),
    'lo.comp.errors@reportError'(XXd39313, XLc, XRp, XRpx).
'lo.comp.transform@implementPtnCall'('lo.comp.transutils#localFun'(XFn, X_34285, X_34286, XAr, XLblVr, XThVr), X_34287, XTLc, XX, XArgs, XX, XQ, XXb19523, XPre, XPx, XTail, 'lo.core#,..'('lo.comp.term#call'(XTLc, 'lo.comp.term#prg'(XFn, XXe4963), XXb19528), XTailx), XPre, XPx, XTail, XTailx, XRp, XRp):- 'lo.list@merge'('lo.core#,..'(XX, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XXb19523),
    ocall('size%1'(XXV5324),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%1'(XXV5325),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XArgs, XXe4962),XXV5324,XXV5324),
    ocall('_call%3'(XXe4962, 3, XXe4963),XXV5325,XXV5325),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XX, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XXb19528).
'lo.comp.transform@implementPtnCall'('lo.comp.transutils#moduleFun'(X_34295, XFn, X_34296, X_34297, XAr), X_34298, XTLc, XX, XArgs, XX, XQ, XXb19533, XPre, XPx, XTail, 'lo.core#,..'('lo.comp.term#call'(XTLc, 'lo.comp.term#prg'(XFn, XXe4965), XXb19536), XTailx), XPre, XPx, XTail, XTailx, XRp, XRp):- 'lo.list@merge'('lo.core#,..'(XX, 'lo.core#[]'), XQ, XXb19533),
    ocall('size%1'(XXV5326),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%1'(XXV5327),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XArgs, XXe4964),XXV5326,XXV5326),
    ocall('_call%3'(XXe4964, 1, XXe4965),XXV5327,XXV5327),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XX, 'lo.core#[]'), XXb19536).
'lo.comp.transform@implementPtnCall'('lo.comp.transutils#inheritField'(XSuper, XLblVr, XThVr), XNm, XTLc, XX, XArgs, XX, XQ, XXb19543, XPre, XPx, XTail, 'lo.core#,..'('lo.comp.term#call'(XTLc, XSuper, 'lo.core#,..'('lo.comp.term#cons'(XXb19544, XXb19546), 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')))), XTailx), XPre, XPx, XTail, XTailx, XRp, XRp):- 'lo.list@merge'('lo.core#,..'(XX, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XXb19543),
    ocall('+%1'(XXV5328),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@length'(XArgs, XXd39314),
    ocall('_call%3'(XXd39314, 1, XXe4966),XXV5328,XXV5328),
    'lo.comp.transutils@trCons'(XNm, XXe4966, XXb19544),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XX, 'lo.core#[]'), XXb19546).
'lo.comp.transform@implementPtnCall'('lo.comp.transutils#moduleClass'(XMdl, X_34310, X_34311), X_34312, X_34313, X_34314, XArgs, 'lo.comp.term#cons'('lo.comp.term#strct'(XMdl, XXe4967), XArgs), XQ, XQ, XPre, XPx, XTail, XTailx, XPre, XPx, XTail, XTailx, XRp, XRp):- ocall('size%1'(XXV5329),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('_call%2'(XArgs, XXe4967),XXV5329,XXV5329).
'lo.comp.transform@implementPtnCall'('lo.comp.transutils#localClass'(XAcc, X_34315, X_34316, XLbVr, XThVr), X_34317, X_34318, X_34319, XArgs, 'lo.comp.term#cons'('lo.comp.term#strct'(XAcc, XXe4969), XXb19560), XQ, XXb19564, XPre, XPx, XTail, XTailx, XPre, XPx, XTail, XTailx, XRp, XRp):- ocall('size%1'(XXV5330),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%1'(XXV5331),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XArgs, XXe4968),XXV5330,XXV5330),
    ocall('_call%3'(XXe4968, 2, XXe4969),XXV5331,XXV5331),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XXb19560),
    'lo.list@merge'('lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XXb19564).
'lo.comp.transform@implementPtnCall'('lo.comp.transutils#inherit'(XNm, X_34324, XLbVr, XThVr), X_34325, X_34326, X_34327, XArgs, 'lo.comp.term#cons'('lo.comp.term#strct'(XNm, XXe4970), XArgs), XQ, XXb19570, XPre, XPx, XTail, XTailx, XPre, XPx, XTail, XTailx, XRp, XRp):- ocall('size%1'(XXV5332),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('_call%2'(XArgs, XXe4970),XXV5332,XXV5332),
    'lo.list@merge'('lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XXb19570).
'lo.comp.transform@implementPtnCall'('lo.comp.transutils#moduleImpl'(X_34330, XMdl), X_34331, X_34332, X_34333, XArgs, 'lo.comp.term#cons'(XMdl, XArgs), XQ, XQ, XPre, XPx, XTail, XTailx, XPre, XPx, XTail, XTailx, XRp, XRp).
'lo.comp.transform@trPtnCallOp'(XNm, XTLc, XArgs, XX, XQ, XXb19574, XPre, XPx, XTail, 'lo.core#,..'('lo.comp.term#ecall'(XTLc, XNm, XXb19576), XTailx), XPre, XPx, XTail, XTailx, X_34337, X_34338, XEx, XEx, XRp, XRp):- 'lo.comp.escapes@isEscape'(XNm),
    'lo.comp.transutils@genVarbl'("Xa", XX),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#[]'), XQ, XXb19574),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XX, 'lo.core#[]'), XXb19576).
'lo.comp.transform@trPtnCallOp'(XNm, XTLc, XArgs, XPtn, XQ, XQx, XAPre, XAPx, XAPost, XAPstx, XPre, XPx, XTail, XTailx, XMap, X_34339, XEx, XEx, XRp, XRpx):- 'lo.comp.transutils@lookupFunName'(XMap, XNm, XReslt),
    'lo.comp.transutils@genVarbl'("Xb", XX),
    'lo.comp.transform@implementPtnCall'(XReslt, XNm, XTLc, XX, XArgs, XPtn, XQ, XQx, XAPre, XAPx, XAPost, XAPstx, XPre, XPx, XTail, XTailx, XRp, XRpx).
'lo.comp.transform@implementVarPtn'('lo.comp.transutils#localVar'(XVn, X_34340, XClVr, XTVr), XNm, XTLc, XX, XQ, XXb19583, 'lo.core#,..'('lo.comp.term#call'(XTLc, 'lo.comp.term#prg'(XVn, 3), 'lo.core#,..'(XX, 'lo.core#,..'(XClVr, 'lo.core#,..'(XTVr, 'lo.core#[]')))), XPre), XPre, XPost, XPost, XRp, XRp):- 'lo.comp.transutils@genVarbl'(XNm, XX),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#,..'(XClVr, 'lo.core#,..'(XTVr, 'lo.core#[]'))), XQ, XXb19583).
'lo.comp.transform@implementVarPtn'('lo.comp.transutils#moduleVar'(X_34348, XVn, X_34349), XNm, XTLc, XX, XQ, 'lo.core#,..'(XX, XQ), 'lo.core#,..'('lo.comp.term#call'(XTLc, 'lo.comp.term#prg'(XVn, 1), 'lo.core#,..'(XX, 'lo.core#[]')), XPre), XPre, XPost, XPost, XRp, XRp):- 'lo.comp.transutils@genVarbl'(XNm, XX).
'lo.comp.transform@implementVarPtn'('lo.comp.transutils#labelArg'(XN, XClVr, XTVr), X_34353, X_34354, XN, XQ, XXb19600, XPre, XPre, XPost, XPost, XRp, XRp):- 'lo.list@merge'('lo.core#,..'(XN, 'lo.core#,..'(XClVr, 'lo.core#,..'(XTVr, 'lo.core#[]'))), XQ, XXb19600).
'lo.comp.transform@implementVarPtn'('lo.comp.transutils#moduleClass'(XEnum, X_34358, 0), X_34359, X_34360, 'lo.comp.term#enum'(XEnum), XQ, XQ, XPre, XPre, XPost, XPost, XRp, XRp).
'lo.comp.transform@implementVarPtn'('lo.comp.transutils#localClass'(XEnum, X_34361, 0, XLbVr, XThVr), X_34362, X_34363, 'lo.comp.term#cons'('lo.comp.term#strct'(XEnum, 2), 'lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XXb19610, XPre, XPre, XPost, XPost, XRp, XRp):- 'lo.list@merge'('lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XXb19610).
'lo.comp.transform@implementVarPtn'('lo.comp.transutils#inherit'(XNm, X_34368, XLbVr, XThVr), X_34369, X_34370, 'lo.comp.term#cons'('lo.comp.term#strct'(XNm, 2), 'lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XXb19618, XPre, XPre, XPost, XPost, XRp, XRp):- 'lo.list@merge'('lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XXb19618).
'lo.comp.transform@implementVarPtn'('lo.comp.transutils#notInMap', XNm, X_34375, 'lo.comp.term#varbl'(XNm), XQ, XXb19622, XPre, XPre, XPost, XPost, XRp, XRp):- 'lo.list@merge'('lo.core#,..'('lo.comp.term#varbl'(XNm), 'lo.core#[]'), XQ, XXb19622).
'lo.comp.transform@trVarPtn'(X_34377, "_", 'lo.comp.term#anon', XQ, XQ, XPre, XPre, XPost, XPost, X_34378, X_34379, XRp, XRp).
'lo.comp.transform@trVarPtn'(XLc, XNm, XA, XQ, XQx, XPre, XPrx, XPost, XPstx, XMap, X_34380, XRp, XRpx):- 'lo.comp.transutils@lookupVarName'(XMap, XNm, XV),
    ocall('_coerce%1'(XXV5333),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe4971),XXV5333,XXV5333),
    'lo.comp.transform@implementVarPtn'(XV, XNm, 'lo.core#some'(XXe4971), XA, XQ, XQx, XPre, XPrx, XPost, XPstx, XRp, XRpx).
'lo.comp.transform@trVarPtn'(XLc, XNm, 'lo.comp.term#anon', XQ, XQ, XPre, XPre, XPost, XPost, X_34381, X_34382, XRp, XRpx):- ocall('disp%1'(XXV5334),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('_call%2'(XNm, XXe4972),XXV5334,XXV5334),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("'"), 'lo.core#,..'(XXe4972, 'lo.core#,..'('lo.core#ss'("' not defined"), 'lo.core#[]')))), XXd39322),
    'lo.comp.errors@reportError'(XXd39322, XLc, XRp, XRpx).
'lo.comp.transform@trExpCallOp'(XLc, 'lo.comp.canon#v'(X_34386, XNm), XArgs, XX, XQ, XXb19625, XPre, XPx, XTail, 'lo.core#,..'('lo.comp.term#ecall'('lo.core#some'(XXe4973), XNm, XXb19628), XTailx), XPre, XPx, XTail, XTailx, X_34390, X_34391, XEx, XEx, XRp, XRp):- 'lo.comp.escapes@isEscape'(XNm),
    'lo.comp.transutils@genVarbl'("Xc", XX),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#[]'), XQ, XXb19625),
    ocall('_coerce%1'(XXV5335),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe4973),XXV5335,XXV5335),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XX, 'lo.core#[]'), XXb19628).
'lo.comp.transform@trExpCallOp'(XLc, 'lo.comp.canon#v'(X_34392, XNm), XArgs, XExp, XQ, XXb19633, XAPre, XAPx, XAPost, XAPstx, XPre, XPx, XTail, XTailx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transutils@lookupFunName'(XMap, XNm, XReslt),
    'lo.comp.transutils@genVarbl'("Xd", XX),
    'lo.comp.transform@implementFunCall'(XReslt, XLc, XNm, XX, XArgs, XExp, XQ, XQx, XAPre, XAPx, XAPost, XAPstx, XPre, XPx, XTail, XTailx, XMap, XOpts, XEx, XExx, XRp, XRpx),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#[]'), XQx, XXb19633).
'lo.comp.transform@trExpCallOp'(XLc, XT, XArgs, XX, XQ, XXb19635, XPre, XAPx, XTail, 'lo.core#,..'('lo.comp.term#ocall'('lo.core#some'(XXe4974), XC, XCl, XCl), XTailx), XPre, XPx, XTail, XTailx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@trExp'(XT, XCl, XQ, XQx, XAPx, XRx, XRx, XPx, XMap, XOpts, XEx, XExx, XRp, XRpx),
    'lo.comp.transutils@genVarbl'("Xe", XX),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XX, 'lo.core#[]'), XXd39324),
    XXArgs = XXd39324,
    'lo.list@length'(XXArgs, XXd39325),
    'lo.comp.transutils@trCons'("_call", XXd39325, XXd39326),
    XC = 'lo.comp.term#cons'(XXd39326, XXArgs),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#[]'), XQx, XXb19635),
    ocall('_coerce%1'(XXV5336),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe4974),XXV5336,XXV5336).
'lo.comp.transform@implementDotExp'('lo.comp.transutils#inherit'(X_34397, XSuper, XClVr, XThVr), X_34398, XTLc, XC, XX, XX, XQ, XXb19643, XPre, XPre, 'lo.core#,..'('lo.comp.term#call'(XTLc, XSuper, 'lo.core#,..'(XC, 'lo.core#,..'(XClVr, 'lo.core#,..'(XThVr, 'lo.core#[]')))), XTail), XTail, X_34406, X_34407, XEx, XEx, XRp, XRp):- 'lo.list@merge'('lo.core#,..'(XX, 'lo.core#,..'(XClVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XXb19643).
'lo.comp.transform@implementDotExp'(X_34408, XR, XTLc, XC, XX, XX, XQ, XXb19650, XPre, XPx, XTail, XTailx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@trExp'(XR, XRc, XQ, XQ0, XPre, XPx, XTail, 'lo.core#,..'('lo.comp.term#ocall'(XTLc, XC, XRc, XRc), XTailx), XMap, XOpts, XEx, XExx, XRp, XRpx),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#[]'), XQ0, XXb19650).
'lo.comp.transform@trDotExp'(XLc, 'lo.comp.canon#v'(X_34411, XNm), XC, XX, XExp, XQ, XQx, XPre, XPx, XTail, XTailx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transutils@lookupVarName'(XMap, XNm, XReslt),
    ocall('_coerce%1'(XXV5337),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe4975),XXV5337,XXV5337),
    'lo.comp.transform@implementDotExp'(XReslt, 'lo.comp.canon#v'(XLc, XNm), 'lo.core#some'(XXe4975), XC, XX, XExp, XQ, XQx, XPre, XPx, XTail, XTailx, XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@trDotExp'(XLc, XR, XC, XX, XX, XQ, XXb19653, XPre, XPx, XTail, XTailx, XMap, XOpts, XEx, XExx, XRp, XRpx):- ocall('_coerce%1'(XXV5338),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe4976),XXV5338,XXV5338),
    'lo.comp.transform@trExp'(XR, XRc, XQ, XQ0, XPre, XPx, XTail, 'lo.core#,..'('lo.comp.term#ocall'('lo.core#some'(XXe4976), XC, XRc, XRc), XTailx), XMap, XOpts, XEx, XExx, XRp, XRpx),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#[]'), XQ0, XXb19653).
'lo.comp.transform@trPtns'('lo.core#[]', XArgs, XArgs, XQ, XQ, XPre, XPre, XPost, XPost, X_34414, X_34415, XEx, XEx, XRp, XRp).
'lo.comp.transform@trPtns'('lo.core#,..'(XP, XMore), 'lo.core#,..'(XA, XArgs), XAx, XQ, XQx, XPre, XPrx, XPost, XPsx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@one290'(XRp0, XRp, XEx0, XEx, XOpts, XMap, XPst0, XPost, XPre0, XPre, XQ0, XQ, XA, XP),
    'lo.comp.transform@trPtns'(XMore, XArgs, XAx, XQ0, XQx, XPre0, XPrx, XPst0, XPsx, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@pushTerminals'('lo.core#[]', X_34418, X_34419, XG, XGx, XStrm, XStrmx, XQ, XQ, XEx, XEx, XRp, XRp):- 'lo.comp.transform@joinStream'(XStrm, XStrmx, XG, XGx).
'lo.comp.transform@pushTerminals'('lo.core#,..'('()3'(XLc, XSV, XT), XMore), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx):- 'lo.comp.transutils@genVarbl'("NStrm", XNStrm),
    'lo.comp.transform@mkCanon'(XStrm, XXd39335),
    'lo.comp.transform@mkCanon'(XNStrm, XXd39336),
    'lo.comp.transform@trGoal'('lo.comp.canon#callCond'(XLc, XSV, 'lo.comp.canon#tpl'('lo.core#,..'(XXd39335, 'lo.core#,..'(XT, 'lo.core#,..'(XXd39336, 'lo.core#[]'))))), XG, XG0, XQ, XQ0, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@pushTerminals'(XMore, XMap, XOpts, XG0, XGx, XNStrm, XStrmx, 'lo.core#,..'(XNStrm, XQ0), XQx, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@dcgDisj'(XLhs, XRhs, XMap, XOpts, 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XDisProg, 'lo.core#,..'(XStrm, 'lo.core#,..'(XStrmx, XDQ))), XG), XG, XStrm, XStrmx, XQ, XXb19661, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@cond405'(XXd39343, XQ, XQ0, XStrmx),
    'lo.comp.transutils@genVarbl'("DjStrm", XDjStrm),
    'lo.comp.transform@dcgBody'(XLhs, XMap, XOpts, XLG, 'lo.core#[]', XDjStrm, XDjStrmx, 'lo.core#[]', XLQ, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@dcgBody'(XRhs, XMap, XOpts, XRG, 'lo.core#[]', XDjStrm, XDjStrmy, XLQ, XDQ, XEx0, XEx1, XRp0, XRpx),
    ocall('+%1'(XXV5339),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@length'(XDQ, XXd39344),
    ocall('_call%3'(XXd39344, 2, XXe4977),XXV5339,XXV5339),
    'lo.comp.transutils@genNewName'(XMap, "Disj", XXe4977, XDisProg),
    XC1 = 'lo.comp.term#clse'('lo.core#,..'(XDjStrm, XDQ), XDisProg, 'lo.core#,..'(XDjStrm, 'lo.core#,..'(XDjStrmx, XDQ)), XLG),
    XC2 = 'lo.comp.term#clse'('lo.core#,..'(XDjStrm, XDQ), XDisProg, 'lo.core#,..'(XDjStrm, 'lo.core#,..'(XDjStrmy, XDQ)), XRG),
    XEx1 = 'lo.core#,..'(XC1, 'lo.core#,..'(XC2, XExx)),
    'lo.list@merge'(XDQ, XQ0, XXb19661).
'lo.comp.transform@dcgConditional'(XTst, XLhs, XRhs, XMap, XOpts, 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XCondProg, 'lo.core#,..'(XStrm, 'lo.core#,..'(XStrmx, XDQ))), XG), XG, XStrm, XStrmx, XQ, XXb19666, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@cond406'(XStrmx),
    'lo.comp.transutils@genVarbl'("CondStrm", XCndStrm),
    'lo.comp.transform@dcgBody'(XTst, XMap, XOpts, XTG, 'lo.core#,..'('lo.comp.term#neck', XLG), XCndStrm, XCndStrm0, 'lo.core#[]', XTQ, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@dcgBody'(XLhs, XMap, XOpts, XLG, 'lo.core#[]', XCndStrm0, XCndStrmx, XTQ, XLQ, XEx0, XEx1, XRp0, XRp1),
    'lo.comp.transform@dcgBody'(XRhs, XMap, XOpts, XRG, 'lo.core#[]', XCndStrm, XCndStrmy, XLQ, XDQ, XEx1, 'lo.core#,..'(XC1, 'lo.core#,..'(XC2, XExx)), XRp1, XRpx),
    ocall('+%1'(XXV5340),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@length'(XDQ, XXd39358),
    ocall('_call%3'(XXd39358, 2, XXe4978),XXV5340,XXV5340),
    'lo.comp.transutils@genNewName'(XMap, "Cond", XXe4978, XCondProg),
    XC1 = 'lo.comp.term#clse'('lo.core#,..'(XCndStrm, XDQ), XCondProg, 'lo.core#,..'(XCndStrm, 'lo.core#,..'(XCndStrmx, XDQ)), XTG),
    XC2 = 'lo.comp.term#clse'('lo.core#,..'(XCndStrm, XDQ), XCondProg, 'lo.core#,..'(XCndStrm, 'lo.core#,..'(XCndStrmy, XDQ)), XRG),
    'lo.list@merge'(XDQ, XQ, XXb19666).
'lo.comp.transform@dcgOne'(XLhs, XMap, XOpts, 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XOneProg, 'lo.core#,..'(XStrm, 'lo.core#,..'(XStrmx, XDQ))), XG), XG, XStrm, XStrmx, XQ, XXb19671, XEx, XExx, XRp, XRpx):- 'lo.comp.transutils@genVarbl'("OneStm", XOneStm),
    'lo.comp.transform@cond407'(XStrmx),
    'lo.comp.transform@dcgBody'(XLhs, XMap, XOpts, XLG, 'lo.core#,..'('lo.comp.term#neck', 'lo.core#[]'), XOneStm, XOneStmx, 'lo.core#[]', XDQ, XEx, 'lo.core#,..'(XC1, XExx), XRp, XRpx),
    ocall('+%1'(XXV5341),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@length'(XDQ, XXd39369),
    ocall('_call%3'(XXd39369, 2, XXe4979),XXV5341,XXV5341),
    'lo.comp.transutils@genNewName'(XMap, "One", XXe4979, XOneProg),
    XC1 = 'lo.comp.term#clse'('lo.core#,..'(XOneStm, XDQ), XOneProg, 'lo.core#,..'(XOneStm, 'lo.core#,..'(XOneStmx, XDQ)), XLG),
    'lo.list@merge'(XDQ, XQ, XXb19671).
'lo.comp.transform@dcgNeg'(XTst, XMap, XOpts, 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XNegProg, 'lo.core#,..'(XStrm, XTQ)), XG), XG, XStrm, XQ, XXb19675, XEx, XExx, XRp, XRpx):- 'lo.comp.transutils@genVarbl'("NegStrm", XNegStrm),
    'lo.comp.transform@dcgBody'(XTst, XMap, XOpts, XTG, 'lo.core#,..'('lo.comp.term#neck', 'lo.core#,..'('lo.comp.term#fail', 'lo.core#[]')), XNegStrm, X_34461, 'lo.core#[]', XTQ, XEx, 'lo.core#,..'(XC1, 'lo.core#,..'(XC2, XExx)), XRp, XRpx),
    ocall('+%1'(XXV5342),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@length'(XTQ, XXd39378),
    ocall('_call%3'(XXd39378, 1, XXe4980),XXV5342,XXV5342),
    'lo.comp.transutils@genNewName'(XMap, "Neg", XXe4980, XNegProg),
    XC1 = 'lo.comp.term#clse'('lo.core#,..'(XNegStrm, XTQ), XNegProg, 'lo.core#,..'(XNegStrm, XTQ), XTG),
    XC2 = 'lo.comp.term#clse'('lo.core#,..'(XNegStrm, XTQ), XNegProg, 'lo.core#,..'(XNegStrm, XTQ), 'lo.core#[]'),
    'lo.list@merge'(XTQ, XQ, XXb19675).
'lo.comp.transform@dcgAhead'(XTst, XMap, XOpts, 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XHdProg, 'lo.core#,..'(XStrm, XTQ)), XG), XG, XStrm, XQ, XXb19679, XEx, XExx, XRp, XRpx):- 'lo.comp.transutils@genVarbl'("HedStrm", XHedStrm),
    'lo.comp.transform@dcgBody'(XTst, XMap, XOpts, XTG, 'lo.core#[]', XHedStrm, X_34470, 'lo.core#[]', XTQ, XEx, 'lo.core#,..'(XC1, XExx), XRp, XRpx),
    ocall('+%1'(XXV5343),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@length'(XTQ, XXd39386),
    ocall('_call%3'(XXd39386, 1, XXe4981),XXV5343,XXV5343),
    'lo.comp.transutils@genNewName'(XMap, "Hed", XXe4981, XHdProg),
    XC1 = 'lo.comp.term#clse'('lo.core#,..'(XHedStrm, XTQ), XHdProg, 'lo.core#,..'(XHedStrm, XTQ), XTG),
    'lo.list@merge'(XTQ, XQ, XXb19679).
'lo.comp.transform@trGoal'('lo.comp.canon#trueCond', XGoals, XGoals, XQ, XQ, X_34474, X_34475, XEx, XEx, XRp, XRp).
'lo.comp.transform@trGoal'('lo.comp.canon#falseCond', 'lo.core#,..'('lo.comp.term#fail', XRest), XRest, XQ, XQ, X_34477, X_34478, XEx, XEx, XRp, XRp).
'lo.comp.transform@trGoal'('lo.comp.canon#conjCond'(XL, XR), XGoals, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@trGoal'(XL, XGoals, XG0, XQ, XQ0, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XR, XG0, XGx, XQ0, XQx, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@trGoal'('lo.comp.canon#disjCond'(XL, XR), 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XDisjPr, XLQ), XGx), XGx, XQ, XXb19685, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@trGoal'(XL, XLG, 'lo.core#[]', 'lo.core#[]', XQ0, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XR, XRG, 'lo.core#[]', XQ0, XLQ, XMap, XOpts, XEx0, XEx1, XRp0, XRpx),
    'lo.list@length'(XLQ, XXd39390),
    'lo.comp.transutils@genNewName'(XMap, "or", XXd39390, XDisjPr),
    XCl1 = 'lo.comp.term#clse'(XLQ, XDisjPr, XLQ, XLG),
    XCl2 = 'lo.comp.term#clse'(XLQ, XDisjPr, XLQ, XRG),
    XEx1 = 'lo.core#,..'(XCl1, 'lo.core#,..'(XCl2, XExx)),
    'lo.list@merge'(XLQ, XQ, XXb19685).
'lo.comp.transform@trGoal'('lo.comp.canon#condCond'(XT, XL, XR), 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XCondPr, XLQ), XGx), XGx, XQ, XXb19689, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@trGoal'(XT, XTG, 'lo.core#,..'('lo.comp.term#neck', XLG), 'lo.core#[]', XQ0, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XL, XLG, 'lo.core#[]', XQ0, XQ1, XMap, XOpts, XEx0, XEx1, XRp0, XRp1),
    'lo.comp.transform@trGoal'(XR, XRG, 'lo.core#[]', XQ1, XLQ, XMap, XOpts, XEx1, XEx2, XRp1, XRpx),
    'lo.list@length'(XLQ, XXd39396),
    'lo.comp.transutils@genNewName'(XMap, "cond", XXd39396, XCondPr),
    XCl1 = 'lo.comp.term#clse'(XLQ, XCondPr, XLQ, XTG),
    XCl2 = 'lo.comp.term#clse'(XLQ, XCondPr, XLQ, XRG),
    XEx2 = 'lo.core#,..'(XCl1, 'lo.core#,..'(XCl2, XExx)),
    'lo.list@merge'(XLQ, XQ, XXb19689).
'lo.comp.transform@trGoal'('lo.comp.canon#oneCond'(XT), 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XOnePr, XLQ), XGx), XGx, XQ, XXb19693, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@trGoal'(XT, XTG, 'lo.core#,..'('lo.comp.term#neck', 'lo.core#[]'), 'lo.core#[]', XLQ, XMap, XOpts, XEx, XEx0, XRp, XRpx),
    'lo.list@length'(XLQ, XXd39402),
    'lo.comp.transutils@genNewName'(XMap, "one", XXd39402, XOnePr),
    XCl1 = 'lo.comp.term#clse'(XLQ, XOnePr, XLQ, XTG),
    XEx0 = 'lo.core#,..'(XCl1, XExx),
    'lo.list@merge'(XLQ, XQ, XXb19693).
'lo.comp.transform@trGoal'('lo.comp.canon#negCond'(XT), 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XNegPr, XLQ), XGx), XGx, XQ, XXb19697, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@trGoal'(XT, XTG, 'lo.core#,..'('lo.comp.term#neck', 'lo.core#,..'('lo.comp.term#fail', 'lo.core#[]')), 'lo.core#[]', XLQ, XMap, XOpts, XEx, XEx0, XRp, XRpx),
    'lo.list@length'(XLQ, XXd39407),
    'lo.comp.transutils@genNewName'(XMap, "neg", XXd39407, XNegPr),
    XCl1 = 'lo.comp.term#clse'(XLQ, XNegPr, XLQ, XTG),
    XCl2 = 'lo.comp.term#clse'(XLQ, XNegPr, XLQ, 'lo.core#[]'),
    XEx0 = 'lo.core#,..'(XCl1, 'lo.core#,..'(XCl2, XExx)),
    'lo.list@merge'(XLQ, XQ, XXb19697).
'lo.comp.transform@trGoal'('lo.comp.canon#forallCond'(XL, XR), 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XAPr, XLQ), XGx), XGx, XQ, XXb19701, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@trGoal'(XL, XLG, 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XBPr, XLQ), 'lo.core#,..'('lo.comp.term#neck', 'lo.core#,..'('lo.comp.term#fail', 'lo.core#[]'))), 'lo.core#[]', XQ0, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XR, XRG, 'lo.core#,..'('lo.comp.term#neck', 'lo.core#,..'('lo.comp.term#fail', 'lo.core#[]')), XQ0, XLQ, XMap, XOpts, XEx0, XEx1, XRp0, XRpx),
    'lo.list@length'(XLQ, XXd39418),
    XQAr = XXd39418,
    'lo.comp.transutils@genNewName'(XMap, "forallA", XQAr, XAPr),
    'lo.comp.transutils@genNewName'(XMap, "forallB", XQAr, XBPr),
    XACl1 = 'lo.comp.term#clse'(XLQ, XAPr, XLQ, XLG),
    XACl2 = 'lo.comp.term#clse'(XLQ, XAPr, XLQ, 'lo.core#[]'),
    XBCl1 = 'lo.comp.term#clse'(XLQ, XBPr, XLQ, XRG),
    XBCl2 = 'lo.comp.term#clse'(XLQ, XBPr, XLQ, 'lo.core#[]'),
    XEx1 = 'lo.core#,..'(XACl1, 'lo.core#,..'(XACl2, 'lo.core#,..'(XBCl1, 'lo.core#,..'(XBCl2, XExx)))),
    'lo.list@merge'(XLQ, XQ, XXb19701).
'lo.comp.transform@trGoal'('lo.comp.canon#unifyCond'(XLc, XL, XR), XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx):- ocall('_coerce%1'(XXV5344),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe4982),XXV5344,XXV5344),
    'lo.comp.debug@lineDebug'(XLc, XG3, 'lo.core#,..'('lo.comp.term#unfy'('lo.core#some'(XXe4982), XLx, XRx), XGx), XOpts),
    'lo.comp.transform@trExp'(XL, XLx, XQ, XQ0, XG, XG0, XG0, XG1, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trExp'(XR, XRx, XQ0, XQx, XG1, XG2, XG2, XG3, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@trGoal'('lo.comp.canon#phraseCond'(XLc, XNT, XStrm, XRem), XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@trExp'(XStrm, XStIn, XQ, XQ0, XG, XG0, XG0, XG1, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.debug@lineDebug'(XLc, XG1, XG2, XOpts),
    'lo.comp.transform@dcgBody'(XNT, XMap, XOpts, XG2, XG3, XStIn, XStOut, XQ0, XQ2, XEx0, XEx1, XRp0, XRp1),
    'lo.comp.transform@trExp'(XRem, XOut, XQ2, XQx, XG3, XG4, XG4, XG5, XMap, XOpts, XEx1, XExx, XRp1, XRpx),
    'lo.comp.transform@joinStream'(XOut, XStOut, XG5, XGx).
'lo.comp.transform@trGoal'('lo.comp.canon#callCond'(XLc, XPred, 'lo.comp.canon#tpl'(XArgs)), XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.debug@lineDebug'(XLc, XG, XG0, XOpts),
    'lo.comp.transform@trExps'(XArgs, XAG, 'lo.core#[]', XQ, XQ0, XG0, XPr, XPr, XG3, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoalCall'(XPred, XAG, XG3, XGx, XQ0, XQx, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@trGoal'('lo.comp.canon#isTrue'(XE), XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@trExp'(XE, XExp, XQ, XQx, XG, XG0, XG0, 'lo.core#,..'('lo.comp.term#unfy'('lo.core#none', XExp, 'lo.comp.term#enum'("lo.core#true")), XGx), XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@implementGoalCall'('lo.comp.transutils#localRel'(XFn, X_34506, X_34507, XAr, XLblVr, XThVr), XLc, X_34508, XArgs, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XXe4983), 'lo.comp.term#prg'(XFn, XXe4985), XXb19712), XTail), XTail, XQ, XXb19717, X_34514, X_34515, XEx, XEx, XRp, XRp):- ocall('_coerce%1'(XXV5345),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe4983),XXV5345,XXV5345),
    ocall('size%1'(XXV5346),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%1'(XXV5347),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XArgs, XXe4984),XXV5346,XXV5346),
    ocall('_call%3'(XXe4984, 2, XXe4985),XXV5347,XXV5347),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XXb19712),
    'lo.list@merge'('lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XXb19717).
'lo.comp.transform@implementGoalCall'('lo.comp.transutils#moduleRel'(X_34516, XFn, X_34517, X_34518, XAr), XLc, X_34519, XArgs, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XXe4986), 'lo.comp.term#prg'(XFn, XXe4987), XArgs), XTail), XTail, XQ, XQ, X_34521, X_34522, XEx, XEx, XRp, XRp):- ocall('_coerce%1'(XXV5348),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe4986),XXV5348,XXV5348),
    ocall('size%1'(XXV5349),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('_call%2'(XArgs, XXe4987),XXV5349,XXV5349).
'lo.comp.transform@implementGoalCall'('lo.comp.transutils#inheritField'(XSuper, XLblVr, XThVr), XLc, XPred, XArgs, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XXe4988), XSuper, 'lo.core#,..'('lo.comp.term#cons'(XXb19726, XArgs), 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')))), XTail), XTail, XQ, XXb19735, X_34529, X_34530, XEx, XEx, XRp, XRp):- ocall('_coerce%1'(XXV5350),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe4988),XXV5350,XXV5350),
    'lo.list@length'(XArgs, XXb19725),
    'lo.comp.transutils@trCons'(XPred, XXb19725, XXb19726),
    'lo.list@merge'('lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XXb19735).
'lo.comp.transform@implementGoalCall'('lo.comp.transutils#notInMap', XLc, XPred, XArgs, XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.list@length'(XArgs, XXd39434),
    'lo.comp.transutils@trCons'("_call", XXd39434, XXd39435),
    'lo.comp.transform@trGoalDot'(XLc, 'lo.comp.canon#v'(XLc, XPred), 'lo.comp.term#cons'(XXd39435, XArgs), XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@implementGoalCall'(X_34531, XLc, XPred, X_34532, XG, XG, XQ, XQ, X_34533, X_34534, XEx, XEx, XRp, XRpx):- ocall('disp%1'(XXV5351),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('_call%2'(XPred, XXe4989),XXV5351,XXV5351),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot handle source for "), 'lo.core#,..'(XXe4989, 'lo.core#[]'))), XXd39441),
    'lo.comp.errors@reportError'(XXd39441, XLc, XRp, XRpx).
'lo.comp.transform@trGoalDot'(X_34537, 'lo.comp.canon#v'(XLc, XNm), XC, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XXe4990), XSuper, 'lo.core#,..'(XC, 'lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')))), XGx), XGx, XQ, XXb19745, XMap, X_34544, XEx, XEx, XRp, XRp):- 'lo.comp.transutils@lookupVarName'(XMap, XNm, 'lo.comp.transutils#inherit'(X_34545, XSuper, XLbVr, XThVr)),
    ocall('_coerce%1'(XXV5352),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe4990),XXV5352,XXV5352),
    'lo.list@merge'('lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XXb19745).
'lo.comp.transform@trGoalDot'(XLc, XRec, XC, XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx):- ocall('_coerce%1'(XXV5353),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe4991),XXV5353,XXV5353),
    'lo.comp.transform@trExp'(XRec, XNR, XQ, XQx, XG, XG0, XG0, 'lo.core#,..'('lo.comp.term#ocall'('lo.core#some'(XXe4991), XC, XNR, XNR), XGx), XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@trGoalCall'('lo.comp.canon#v'(XLc, XNm), XArgs, 'lo.core#,..'('lo.comp.term#ecall'('lo.core#some'(XXe4992), XNm, XArgs), XTail), XTail, XQ, XQ, X_34548, X_34549, XEx, XEx, XRp, XRp):- 'lo.comp.escapes@isEscape'(XNm),
    ocall('_coerce%1'(XXV5354),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe4992),XXV5354,XXV5354).
'lo.comp.transform@trGoalCall'('lo.comp.canon#v'(XLc, XNm), XArgs, XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transutils@lookupRelName'(XMap, XNm, XRSpec),
    'lo.comp.transform@implementGoalCall'(XRSpec, XLc, XNm, XArgs, XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@trGoalCall'('lo.comp.canon#dot'(XLc, XRec, XPred), XArgs, XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.list@length'(XArgs, XXd39446),
    'lo.comp.transutils@trCons'(XPred, XXd39446, XXd39447),
    'lo.comp.transform@trGoalDot'(XLc, XRec, 'lo.comp.term#cons'(XXd39447, XArgs), XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@trExps'('lo.core#[]', XArgs, XArgs, XQ, XQ, XPre, XPre, XPost, XPost, X_34550, X_34551, XEx, XEx, XRp, XRp).
'lo.comp.transform@trExps'('lo.core#,..'(XP, XMore), 'lo.core#,..'(XA, XArgs), XExtra, XQ, XQx, XPre, XPrx, XPost, XPsx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@one291'(XRp0, XRp, XEx0, XEx, XOpts, XMap, XPst0, XPost, XPre0, XPre, XQ0, XQ, XA, XP),
    'lo.comp.transform@trExps'(XMore, XArgs, XExtra, XQ0, XQx, XPre0, XPrx, XPst0, XPsx, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@dcgBody'('lo.comp.canon#grTerms'(XTerms), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@pushTerminals'(XTerms, XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@dcgBody'('lo.comp.canon#grConj'(XLhs, XRhs), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@dcgBody'(XLhs, XMap, XOpts, XG, XG0, XStrm, XStrm0, XQ, XQ0, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@dcgBody'(XRhs, XMap, XOpts, XG0, XGx, XStrm0, XStrmx, XQ0, XQx, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@dcgBody'('lo.comp.canon#grDisj'(XLhs, XRhs), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@dcgDisj'(XLhs, XRhs, XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@dcgBody'('lo.comp.canon#grCond'(XTst, XLhs, XRhs), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@dcgConditional'(XTst, XLhs, XRhs, XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@dcgBody'('lo.comp.canon#grOne'(XTst), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@dcgOne'(XTst, XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@dcgBody'('lo.comp.canon#grNeg'(XTst), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@joinStream'(XStrm, XStrmx, XG, XG0),
    'lo.comp.transform@dcgNeg'(XTst, XMap, XOpts, XG0, XGx, XStrm, XQ, XQx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@dcgBody'('lo.comp.canon#grAhed'(XTst), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@joinStream'(XStrm, XStrmx, XG, XG0),
    'lo.comp.transform@dcgAhead'(XTst, XMap, XOpts, XG0, XGx, XStrm, XQ, XQx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@dcgBody'('lo.comp.canon#grTest'(XGoal), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@joinStream'(XStrm, XStrmx, XG, XG0),
    'lo.comp.transform@trGoal'(XGoal, XG0, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@dcgBody'('lo.comp.canon#grDip'(XV, XCond), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@joinStream'(XStrm, XStrmx, XG, XG0),
    'lo.comp.transform@trExp'(XV, XStrmVr, XQ, XQ0, XG0, XG1, XG1, 'lo.core#,..'('lo.comp.term#unfy'('lo.core#none', XStrm, XStrmVr), XG2), XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XCond, XG2, XGx, XQ0, XQx, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@dcgBody'('lo.comp.canon#grCall'(XLc, XNT, 'lo.comp.canon#tpl'(XArgs)), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx):- 'lo.comp.debug@lineDebug'(XLc, XG, XG0, XOpts),
    'lo.comp.transform@trExps'(XArgs, XAG, 'lo.core#[]', XQ, XQ0, XG0, XPr, XPr, XG3, XMap, XOpts, XEx, XEx0, XRp, XRpx),
    'lo.comp.transform@cond408'(XXd39451, XQ0, XQ1, XStrmx),
    'lo.comp.transform@trGoalCall'(XNT, 'lo.core#,..'(XStrm, 'lo.core#,..'(XStrmx, XAG)), XG3, XGx, XQ1, XQx, XMap, XOpts, XEx0, XExx, XRp, XRpx).
'lo.comp.transform@trLambda'('lo.comp.canon#equation'(XLc, XNm, 'lo.comp.canon#tpl'(XA), XExp, XCond), XClosure, XQ, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.freevars@freeVarsInRule'('lo.comp.canon#equation'(XLc, XNm, 'lo.comp.canon#tpl'(XA), XExp, XCond), XQ, 'lo.core#[]', XXd39456),
    XFreeVars = XXd39456,
    'lo.comp.transutils@genNewName'(XMap, XNm, 3, 'lo.comp.term#prg'(XLam, X_34558)),
    'lo.comp.transform@trPtns'(XA, XArgs, 'lo.core#,..'(XRep, 'lo.core#[]'), 'lo.core#[]', XQ1, XGoals, XPreGx, XPostG, 'lo.core#[]', XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XCond, XPreGx, 'lo.core#,..'('lo.comp.term#neck', XPostGx), XQ1, XQ2, XMap, XOpts, XEx0, XEx1, XRp0, XRp1),
    'lo.comp.transform@trExp'(XExp, XRep, XQ2, XQ3, XPostGx, XPVx, XPVx, XPostG, XMap, XOpts, XEx1, XEx2, XRp1, XRp2),
    'lo.comp.transform@mkClosure'(XLam, XFreeVars, XXd39460),
    XClosure = XXd39460,
    ocall('size%1'(XXV5355),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    'lo.list@merge'(XFreeVars, XQ3, XXd39461),
    ocall('_call%2'(XArgs, XXe4993),XXV5355,XXV5355),
    'lo.comp.transutils@trCons'("_call", XXe4993, XXd39463),
    XEx2 = 'lo.core#,..'('lo.comp.term#clse'(XXd39461, 'lo.comp.term#prg'(XLam, 3), 'lo.core#,..'('lo.comp.term#cons'(XXd39463, XArgs), 'lo.core#,..'(XClosure, 'lo.core#,..'('lo.comp.term#anon', 'lo.core#[]'))), XGoals), XEx3),
    'lo.comp.transform@failSafeEquation'(XLc, 'lo.comp.term#strng'(XNm), XLam, 3, XEx3, XExx, XRp2, XRpx).
'lo.comp.transform@trLambda'('lo.comp.canon#clause'(XLc, XNm, 'lo.comp.canon#tpl'(XA), XCond), XClosure, XQ, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.freevars@freeVarsInRule'('lo.comp.canon#clause'(XLc, XNm, 'lo.comp.canon#tpl'(XA), XCond), XQ, 'lo.core#[]', XXd39473),
    XFreeVars = XXd39473,
    'lo.comp.transutils@genNewName'(XMap, XNm, 3, 'lo.comp.term#prg'(XLam, X_34565)),
    'lo.comp.transform@trPtns'(XA, XArgs, 'lo.core#[]', 'lo.core#[]', XQ1, XGoals, XPreGx, XPostG, 'lo.core#[]', XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XCond, XPreGx, XPostG, XQ1, XQ2, XMap, XOpts, XEx0, XEx1, XRp0, XRpx),
    'lo.comp.transform@mkClosure'(XLam, XFreeVars, XXd39475),
    XClosure = XXd39475,
    ocall('size%1'(XXV5356),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    'lo.list@merge'(XFreeVars, XQ2, XXd39476),
    ocall('_call%2'(XArgs, XXe4994),XXV5356,XXV5356),
    'lo.comp.transutils@trCons'("_call", XXe4994, XXd39478),
    XEx1 = 'lo.core#,..'('lo.comp.term#clse'(XXd39476, 'lo.comp.term#prg'(XLam, 3), 'lo.core#,..'('lo.comp.term#cons'(XXd39478, XArgs), 'lo.core#,..'(XClosure, 'lo.core#,..'('lo.comp.term#anon', 'lo.core#[]'))), XGoals), XExx).
'lo.comp.transform@trLambda'('lo.comp.canon#grRule'(XLc, XNm, 'lo.comp.canon#tpl'(XA), 'lo.comp.canon#grTerms'(XPB), XBody), XClosure, XQ, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.freevars@freeVarsInRule'('lo.comp.canon#grRule'(XLc, XNm, 'lo.comp.canon#tpl'(XA), 'lo.comp.canon#grTerms'(XPB), XBody), XQ, 'lo.core#[]', XXd39488),
    XFreeVars = XXd39488,
    'lo.comp.transutils@genNewName'(XMap, XNm, 3, 'lo.comp.term#prg'(XLam, X_34570)),
    'lo.comp.transform@trPtns'(XA, XArgs, 'lo.core#[]', 'lo.core#[]', XQ1, XGoals, XPreGx, XPostG, XG7, XMap, XClOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transutils@genVarbl'("StIn", XStIn),
    'lo.comp.transform@dcgBody'(XBody, XMap, XOpts, XPreGx, XPostG, XStIn, XStOut, 'lo.core#,..'(XStIn, XQ1), XQ2, XEx0, XEx1, XRp0, XRp1),
    'lo.comp.transform@pushTerminals'(XPB, XMap, XOpts, XG7, 'lo.core#[]', XStOut, XStX, XQ2, XQ4, XEx1, XEx2, XRp1, XRpx),
    'lo.comp.transform@mkClosure'(XLam, XFreeVars, XXd39491),
    XClosure = XXd39491,
    ocall('size%1'(XXV5357),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%1'(XXV5358),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XArgs, XXe4995),XXV5357,XXV5357),
    ocall('_call%3'(XXe4995, 2, XXe4996),XXV5358,XXV5358),
    'lo.comp.transutils@trCons'("_call", XXe4996, XXd39492),
    XCallStrct = 'lo.comp.term#cons'(XXd39492, 'lo.core#,..'(XStIn, 'lo.core#,..'(XStX, XArgs))),
    'lo.list@merge'(XFreeVars, XQ4, XXd39496),
    XEx2 = 'lo.core#,..'('lo.comp.term#clse'(XXd39496, 'lo.comp.term#prg'(XLam, 3), 'lo.core#,..'(XCallStrct, 'lo.core#,..'(XClosure, 'lo.core#,..'('lo.comp.term#anon', 'lo.core#[]'))), XGoals), XExx).
'lo.comp.transform@trExp'('lo.comp.canon#v'(X_34578, "this"), XThVr, XQ, XXb19774, XPre, XPre, XPost, XPost, XMap, X_34580, XEx, XEx, XRp, XRp):- 'lo.comp.transutils@thisVar'(XMap, XThVr),
    'lo.list@merge'('lo.core#,..'(XThVr, 'lo.core#[]'), XQ, XXb19774).
'lo.comp.transform@trExp'('lo.comp.canon#v'(XLc, XNm), XVr, XQ, XQx, XPre, XPx, XPost, XPstx, XMap, XOpts, XEx, XEx, XRp, XRpx):- 'lo.comp.transform@trVarExp'(XLc, XNm, XVr, XQ, XQx, XPre, XPx, XPost, XPstx, XMap, XOpts, XRp, XRpx).
'lo.comp.transform@trExp'('lo.comp.canon#int'(XIx), 'lo.comp.term#intgr'(XIx), XQ, XQ, XPre, XPre, XPost, XPost, X_34581, X_34582, XEx, XEx, XRp, XRp).
'lo.comp.transform@trExp'('lo.comp.canon#flt'(XIx), 'lo.comp.term#flot'(XIx), XQ, XQ, XPre, XPre, XPost, XPost, X_34583, X_34584, XEx, XEx, XRp, XRp).
'lo.comp.transform@trExp'('lo.comp.canon#str'(XIx), 'lo.comp.term#strng'(XIx), XQ, XQ, XPre, XPre, XPost, XPost, X_34585, X_34586, XEx, XEx, XRp, XRp).
'lo.comp.transform@trExp'('lo.comp.canon#tpl'(XA), XXb19783, XQ, XQx, XPre, XPx, XPost, XPstx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@trExps'(XA, XTA, 'lo.core#[]', XQ, XQx, XPre, XPx, XPost, XPstx, XMap, XOpts, XEx, XExx, XRp, XRpx),
    'lo.comp.term@mkTpl'(XTA, XXb19783).
'lo.comp.transform@trExp'('lo.comp.canon#apply'(XLc, XOp, 'lo.comp.canon#tpl'(XA)), XExp, XQ, XQx, XPre, XPx, XPost, XPstx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@trExps'(XA, XArgs, 'lo.core#[]', XQ, XQ0, XAPre, XAPx, XAPost, XAPostx, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trExpCallOp'(XLc, XOp, XArgs, XExp, XQ0, XQx, XAPre, XAPx, XAPost, XAPostx, XPre, XPx, XPost, XPstx, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@trExp'('lo.comp.canon#dot'(XLc, XRec, XFld), XExp, XQ, XXb19788, XPre, XPx, XTail, XTailx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transutils@genVarbl'("XV", XX),
    'lo.comp.transutils@trCons'(XFld, 1, XXd39503),
    'lo.comp.transform@trDotExp'(XLc, XRec, 'lo.comp.term#cons'(XXd39503, 'lo.core#,..'(XX, 'lo.core#[]')), XX, XExp, XQ, XQx, XPre, XPx, XTail, XTailx, XMap, XOpts, XEx, XExx, XRp, XRpx),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#[]'), XQx, XXb19788).
'lo.comp.transform@trExp'('lo.comp.canon#whre'(XP, XC), XPtn, XQ, XQx, XPre, XPx, XPost, XPstx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@trExp'(XP, XPtn, XQ, XQ0, XPre, XP0, XPost, XPstx, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XC, XP0, XPx, XQ0, XQx, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@trExp'('lo.comp.canon#condExp'(XT, XL, XR), XRslt, XQ, XXb19792, XPre, XPrx, XPost, XPost, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transutils@genVarbl'("CndV", XRslt),
    'lo.comp.debug@lineDebug'(XLc, XPre, 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XCondPr, 'lo.core#,..'(XRslt, XLQ)), XPrx), XOpts),
    'lo.comp.transform@trGoal'(XT, XTG, 'lo.core#,..'('lo.comp.term#neck', XLG), 'lo.core#[]', XQ0, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trExp'(XL, XLRslt, XQ0, XQ1, XLG, XLx, XLx, 'lo.core#[]', XMap, XOpts, XEx0, XEx1, XRp0, XRp1),
    'lo.comp.transform@trExp'(XR, XRRslt, XQ1, XLQ, XRG, XRx, XRx, 'lo.core#[]', XMap, XOpts, XEx1, XEx2, XRp1, XRpx),
    'lo.list@length'(XLQ, XXd39510),
    'lo.comp.transutils@genNewName'(XMap, "condExp", XXd39510, XCondPr),
    XCl1 = 'lo.comp.term#clse'(XLQ, XCondPr, 'lo.core#,..'(XLRslt, XLQ), XTG),
    XCl2 = 'lo.comp.term#clse'(XLQ, XCondPr, 'lo.core#,..'(XRRslt, XLQ), XRG),
    XEx2 = 'lo.core#,..'(XCl1, 'lo.core#,..'(XCl2, XExx)),
    'lo.list@merge'('lo.core#,..'(XRslt, XLQ), XQ, XXb19792).
'lo.comp.transform@trExp'('lo.comp.canon#lambda'(XRl), XRslt, XQ, XQ, XPr, XPr, XPost, XPost, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transutils@allLabelArgs'(XMap, 'lo.core#[]', XXd39517),
    'lo.list@merge'(XXd39517, XQ, XXd39518),
    'lo.comp.transform@trLambda'(XRl, XRslt, XXd39518, XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@trExp'(XXX, 'lo.comp.term#anon', XQ, XQ, XPre, XPre, XPost, XPost, X_34597, X_34598, XEx, XEx, XRp, XRpx):- ocall('disp%1'(XXV5359),'lo.core$display$lo.comp.canon*canonTerm','lo.core$display$lo.comp.canon*canonTerm'),
    ocall('_call%2'(XXX, XXe4997),XXV5359,XXV5359),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("internal: cannot transform "), 'lo.core#,..'(XXe4997, 'lo.core#,..'('lo.core#ss'(" as expression"), 'lo.core#[]')))), XXd39525),
    'lo.comp.errors@reportError'(XXd39525, 'lo.comp.location#std', XRp, XRpx).
'lo.comp.transform@trPtn'('lo.comp.canon#v'(X_34602, "this"), XThVr, XQ, XXb19796, XPre, XPre, XPost, XPost, XMap, X_34604, XEx, XEx, XRp, XRp):- 'lo.comp.transutils@thisVar'(XMap, XThVr),
    'lo.list@merge'('lo.core#,..'(XThVr, 'lo.core#[]'), XQ, XXb19796).
'lo.comp.transform@trPtn'('lo.comp.canon#v'(XLc, "this"), 'lo.comp.term#anon', XQ, XQ, XPre, XPre, XPost, XPost, X_34605, X_34606, XEx, XEx, XRp, XRpx):- 'lo.comp.errors@reportError'("'this' not defined here", XLc, XRp, XRpx).
'lo.comp.transform@trPtn'('lo.comp.canon#v'(XLc, XNm), XA, XQ, XQx, XPre, XPrx, XPost, XPstx, XMap, XOpts, XEx, XEx, XRp, XRpx):- 'lo.comp.transform@trVarPtn'(XLc, XNm, XA, XQ, XQx, XPre, XPrx, XPost, XPstx, XMap, XOpts, XRp, XRpx).
'lo.comp.transform@trPtn'('lo.comp.canon#int'(XIx), 'lo.comp.term#intgr'(XIx), XQ, XQ, XPre, XPre, XPost, XPost, X_34607, X_34608, XEx, XEx, XRp, XRp).
'lo.comp.transform@trPtn'('lo.comp.canon#flt'(XIx), 'lo.comp.term#flot'(XIx), XQ, XQ, XPre, XPre, XPost, XPost, X_34609, X_34610, XEx, XEx, XRp, XRp).
'lo.comp.transform@trPtn'('lo.comp.canon#str'(XIx), 'lo.comp.term#strng'(XIx), XQ, XQ, XPre, XPre, XPost, XPost, X_34611, X_34612, XEx, XEx, XRp, XRp).
'lo.comp.transform@trPtn'('lo.comp.canon#dot'(XLc, XRc, XFld), XExp, XQ, XQx, XPre, XPx, XPost, XPost, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transutils@genVarbl'("XV", XX),
    'lo.comp.transutils@trCons'(XFld, 1, XXd39526),
    'lo.comp.transform@trDotExp'(XLc, XRc, 'lo.comp.term#cons'(XXd39526, 'lo.core#,..'(XX, 'lo.core#[]')), XX, XExp, XQ, XQx, XPre, XPi, XPi, XPx, XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@trPtn'('lo.comp.canon#tpl'(XPtns), XXb19807, XQ, XQx, XPre, XPx, XPost, XPostx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@trPtns'(XPtns, XP, 'lo.core#[]', XQ, XQx, XPre, XPx, XPost, XPostx, XMap, XOpts, XEx, XExx, XRp, XRpx),
    'lo.comp.term@mkTpl'(XP, XXb19807).
'lo.comp.transform@trPtn'('lo.comp.canon#apply'(XLc, 'lo.comp.canon#v'(X_34614, XNm), 'lo.comp.canon#tpl'(XA)), XPtn, XQ, XQx, XPre, XPx, XPost, XPstx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@trPtns'(XA, XArgs, 'lo.core#[]', XQ, XQ0, XAPre, XAP0, XAPost, XAPs0, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    ocall('_coerce%1'(XXV5360),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe4998),XXV5360,XXV5360),
    'lo.comp.transform@trPtnCallOp'(XNm, 'lo.core#some'(XXe4998), XArgs, XPtn, XQ0, XQx, XAPre, XAP0, XAPost, XAPs0, XPre, XPx, XPost, XPstx, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@trPtn'('lo.comp.canon#whre'(XP, XC), XPtn, XQ, XQx, XPre, XPx, XPost, XPstx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@trPtn'(XP, XPtn, XQ, XQ0, XPre, XP0, XPost, XPstx, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XC, XP0, XPx, XQ0, XQx, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@trPtn'(XXX, XExp, XQ, XQx, XPre, XPre, XPost, XPostx, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@trExp'(XXX, XExp, XQ, XQx, XPost, XPi, XPi, XPostx, XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@collectMtd'('lo.comp.canon#funDef'(X_34615, XNm, XTp, X_34616, X_34617), XOuterNm, XLbl, XThV, XList, 'lo.core#,..'('()2'(XNm, 'lo.comp.transutils#localFun'(XXb19813, XXb19814, XXb19815, XXb19816, XLbl, XThV)), XList)):- 'lo.comp.transutils@localName'(XOuterNm, "@", XNm, XXb19813),
    'lo.comp.transutils@localName'(XOuterNm, "%", XNm, XXb19814),
    'lo.comp.transutils@localName'(XOuterNm, "^", XNm, XXb19815),
    'lo.comp.transutils@effectiveArity'(XTp, 0, XXb19816).
'lo.comp.transform@collectMtd'('lo.comp.canon#relDef'(X_34619, XNm, XTp, X_34620, X_34621), XOuterNm, XLbl, XThV, XList, 'lo.core#,..'('()2'(XNm, 'lo.comp.transutils#localRel'(XXb19820, XXb19821, XXb19822, XXb19823, XLbl, XThV)), XList)):- 'lo.comp.transutils@localName'(XOuterNm, "@", XNm, XXb19820),
    'lo.comp.transutils@localName'(XOuterNm, "%", XNm, XXb19821),
    'lo.comp.transutils@localName'(XOuterNm, "^", XNm, XXb19822),
    'lo.comp.transutils@effectiveArity'(XTp, 0, XXb19823).
'lo.comp.transform@collectMtd'('lo.comp.canon#grammDef'(X_34623, XNm, XTp, X_34624, X_34625), XOuterNm, XLbl, XThV, XList, 'lo.core#,..'('()2'(XNm, 'lo.comp.transutils#localRel'(XXb19827, XXb19828, XXb19829, XXb19830, XLbl, XThV)), XList)):- 'lo.comp.transutils@localName'(XOuterNm, "@", XNm, XXb19827),
    'lo.comp.transutils@localName'(XOuterNm, "%", XNm, XXb19828),
    'lo.comp.transutils@localName'(XOuterNm, "^", XNm, XXb19829),
    'lo.comp.transutils@effectiveArity'(XTp, 0, XXb19830).
'lo.comp.transform@collectMtd'('lo.comp.canon#varDef'(X_34627, XNm, X_34628, X_34629, X_34630, X_34631), XOuterNm, XLbl, XThV, XList, 'lo.core#,..'('()2'(XNm, 'lo.comp.transutils#localVar'(XXb19834, XXb19835, XLbl, XThV)), XList)):- 'lo.comp.transutils@localName'(XOuterNm, "@", XNm, XXb19834),
    'lo.comp.transutils@localName'(XOuterNm, "%", XNm, XXb19835).
'lo.comp.transform@collectMtd'('lo.comp.canon#classDef'(X_34633, XNm, XTp, X_34634, X_34635, X_34636), XOuterNm, XLbl, XThV, XList, 'lo.core#,..'('()2'(XNm, 'lo.comp.transutils#localClass'(XXb19839, XXb19840, 0, XLbl, XThV)), XList)):- 'lo.comp.transform@neg328'(XTp),
    'lo.comp.transutils@localName'(XOuterNm, "@", XNm, XXb19839),
    'lo.comp.transutils@localName'(XOuterNm, "%", XNm, XXb19840).
'lo.comp.transform@collectMtd'('lo.comp.canon#classDef'(X_34638, XNm, XTp, X_34639, X_34640, X_34641), XOuterNm, XLbl, XThV, XList, 'lo.core#,..'('()2'(XNm, 'lo.comp.transutils#localClass'(XXb19844, XXb19845, XXb19846, XLbl, XThV)), XList)):- 'lo.comp.transutils@localName'(XOuterNm, "@", XNm, XXb19844),
    'lo.comp.transutils@localName'(XOuterNm, "%", XNm, XXb19845),
    'lo.comp.transutils@effectiveArity'(XTp, 0, XXb19846).
'lo.comp.transform@collectMtds'('lo.core#[]', X_34643, X_34644, X_34645, XList, XList, X_34646).
'lo.comp.transform@collectMtds'('lo.core#,..'(XEntry, XDefs), XOuterNm, XLbVr, XThVr, XList, XLx, XFields):- 'lo.comp.transform@collectMtd'(XEntry, XOuterNm, XLbVr, XThVr, XList, XL0),
    'lo.comp.transform@collectMtds'(XDefs, XOuterNm, XLbVr, XThVr, XL0, XLx, XFields).
'lo.comp.transform@collectLabelVars'('lo.core#[]', X_34648, X_34649, XList, XList).
'lo.comp.transform@collectLabelVars'('lo.core#,..'('lo.comp.term#varbl'(XNm), XArgs), XLbVr, XThVr, XList, XLx):- 'lo.comp.transform@collectLabelVars'(XArgs, XLbVr, XThVr, 'lo.core#,..'('()2'(XNm, 'lo.comp.transutils#labelArg'('lo.comp.term#varbl'(XNm), XLbVr, XThVr)), XList), XLx).
'lo.comp.transform@makeLblTerm'('lo.comp.term#enum'(XNm), 'lo.core#[]', 'lo.comp.term#enum'(XNm)).
'lo.comp.transform@makeLblTerm'('lo.comp.term#enum'(XNm), XExtra, 'lo.comp.term#cons'('lo.comp.term#strct'(XNm, XXe4999), XExtra)):- ocall('size%1'(XXV5361),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('_call%2'(XExtra, XXe4999),XXV5361,XXV5361).
'lo.comp.transform@makeLblTerm'('lo.comp.term#cons'('lo.comp.term#strct'(XNm, X_34652), XArgs), XExtra, 'lo.comp.term#cons'('lo.comp.term#strct'(XNm, XXe5002), XXb19860)):- ocall('size%1'(XXV5362),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('size%1'(XXV5363),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%1'(XXV5364),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XArgs, XXe5000),XXV5362,XXV5362),
    ocall('_call%2'(XExtra, XXe5001),XXV5363,XXV5363),
    ocall('_call%3'(XXe5000, XXe5001, XXe5002),XXV5364,XXV5364),
    'lo.list@<>'(XArgs, XExtra, XXb19860).
'lo.comp.transform@makeClassMtdMap'('lo.core#[]', X_34653, X_34654, X_34655, X_34656, XList, XList, X_34657, X_34658, X_34659, XEx, XEx, XRp, XRp).
'lo.comp.transform@makeClassMtdMap'('lo.core#,..'('lo.comp.canon#clRule'(XLc, X_34661, 'lo.comp.canon#v'(X_34662, X_34663), 'lo.comp.canon#theta'(XStmts, X_34664), X_34665, X_34666), XRules), XLclName, XLbVr, XThVr, XLblGl, XList, XLx, XFields, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@collectMtds'(XStmts, XLclName, XLbVr, XThVr, XList, XL0, XFields),
    'lo.comp.transform@collectLabelVars'('lo.core#[]', XLbVr, XThVr, XL0, XL1),
    'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transform@makeLblTerm'('lo.comp.term#enum'(XLclName), XExtra, XLblTerm),
    'lo.comp.transform@cond409'(XXd39536, XXd39535, XLblTerm, XLbVr, XXd39534, XXe5003, XXV5365, XLc, XLblGl, XExtra),
    'lo.comp.transform@makeClassMtdMap'(XRules, XLclName, XLbVr, XThVr, X_34668, XL1, XLx, XFields, XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@makeClassMtdMap'('lo.core#,..'('lo.comp.canon#clRule'(XLc, X_34670, XHd, 'lo.comp.canon#theta'(XStmts, X_34671), X_34672, X_34673), XRules), XLclName, XLbVr, XThVr, XLblGl, XList, XLx, XFields, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@collectMtds'(XStmts, XLclName, XLbVr, XThVr, XList, XL0, XFields),
    ocall('_coerce%1'(XXV5366),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe5004),XXV5366,XXV5366),
    'lo.comp.transform@trPtn'(XHd, XLbl, 'lo.core#[]', XVs, XLblGl, XPx, XPx, 'lo.core#,..'('lo.comp.term#unfy'('lo.core#some'(XXe5004), XLbVr, XLblTerm), 'lo.core#[]'), XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@collectLabelVars'(XVs, XLbVr, XThVr, XL0, XL1),
    'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transform@makeLblTerm'(XLbl, XExtra, XLblTerm),
    'lo.comp.transform@makeClassMtdMap'(XRules, XLclName, XLbVr, XThVr, X_34675, XL1, XLx, XFields, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@makeClassMtdMap'('lo.core#,..'('lo.comp.canon#clRule'(X_34677, X_34678, X_34679, X_34680, X_34681, X_34682), XRules), XLclName, XLbVr, XThVr, XLblGl, XList, XL0, XFields, XMap, XOpts, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@makeClassMtdMap'(XRules, XLclName, XLbVr, XThVr, XLblGl, XList, XL0, XFields, XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@inheritClause'(XName, XTp, XTLc, XPrefix, XSuper, 'lo.core#,..'('lo.comp.term#clse'(XXb19873, 'lo.comp.term#prg'(XPrefix, 3), 'lo.core#,..'(XCon, 'lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), 'lo.core#,..'('lo.comp.term#neck', 'lo.core#,..'('lo.comp.term#call'(XTLc, XSuper, 'lo.core#,..'(XCon, 'lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')))), 'lo.core#[]'))), XEn), XEn):- 'lo.comp.transutils@effectiveArity'(XTp, 0, XXd39540),
    XArity = XXd39540,
    'lo.comp.transutils@genVars'(XArity, XXd39541),
    XArgs = XXd39541,
    'lo.comp.transutils@genVarbl'("This", XThVr),
    'lo.comp.transutils@genVarbl'("Lbl", XLbVr),
    'lo.comp.transutils@trCons'(XName, XArity, XXd39542),
    XCon = 'lo.comp.term#cons'(XXd39542, XArgs),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XXb19873).
'lo.comp.transform@makeInheritFields'('lo.core#[]', X_34694, X_34695, X_34696, X_34697, X_34698, X_34699, XEntry, XEntry, XList, XList).
'lo.comp.transform@makeInheritFields'('lo.core#,..'('()2'(XNm, XTp), XInhFields), XLclName, XTLc, XSuper, XFields, XLbVr, XThVr, XEntry, XEn, XList, XLx):- ocall('in%2'('()2'(XNm, X_34701), XList),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.transform@neg329'(XTp),
    'lo.comp.transform@makeInheritFields'(XInhFields, XLclName, XTLc, XSuper, XFields, XLbVr, XThVr, XEntry, XEn, XList, XLx).
'lo.comp.transform@makeInheritFields'('lo.core#,..'('()2'(XNm, XTp), XInhFields), XLclName, XTLc, XSuper, XFields, XLbVr, XThVr, XEntry, XEn, XList, XLx):- 'lo.comp.transform@inheritClause'(XNm, XTp, XTLc, XLclName, XSuper, XEntry, XEn0),
    'lo.comp.transform@makeInheritFields'(XInhFields, XLclName, XTLc, XSuper, XFields, XLbVr, XThVr, XEn0, XEn, 'lo.core#,..'('()2'(XNm, 'lo.comp.transutils#inheritField'(XSuper, XLbVr, XThVr)), XList), XLx).
'lo.comp.transform@makeInheritanceMap'('lo.core#[]', X_34704, X_34705, X_34706, X_34707, X_34708, XList, XList, X_34709, XEn, XEn, XEx, XEx, XRp, XRp).
'lo.comp.transform@makeInheritanceMap'('lo.core#,..'('lo.comp.canon#clRule'(X_34711, X_34712, X_34713, 'lo.comp.canon#theta'(X_34714, X_34715), X_34716, X_34717), XDefs), XLclName, XLbVr, XThVr, XMap, XOpts, XList, XLx, XFields, XEntry, XEn, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@makeInheritanceMap'(XDefs, XLclName, XLbVr, XThVr, XMap, XOpts, XList, XLx, XFields, XEntry, XEn, XEx, XExx, XRp, XRpx).
'lo.comp.transform@makeInheritanceMap'('lo.core#,..'('lo.comp.canon#clRule'(XLc, X_34719, XP, XR, X_34720, XFaceTp), XDefs), XLclName, XLbVr, XThVr, XMap, XOpts, XList, XLx, XFields, XEntry, XEn, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@pickAllFieldsFromFace'(XFaceTp, XInhFields),
    'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transutils@genVarbl'("CV", XCV),
    'lo.comp.transform@trPtn'(XP, XPtn, XExtra, XQ0, XBody, XPre0, XPre0, XPrx, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    ocall('_coerce%1'(XXV5367),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe5005),XXV5367,XXV5367),
    'lo.comp.transform@trExp'(XR, XRepl, XQ0, XQ1, XPrx, XPx, XPx, 'lo.core#,..'('lo.comp.term#ocall'('lo.core#some'(XXe5005), XCV, XRepl, XThVr), 'lo.core#[]'), XMap, XOpts, XEx0, XEx1, XRP0, XRp1),
    'lo.comp.transutils@genNewName'(XMap, "^", 3, XSuper),
    'lo.list@merge'(XQ1, 'lo.core#,..'(XCV, 'lo.core#,..'(XThVr, 'lo.core#[]')), XXd39551),
    XEx1 = 'lo.core#,..'('lo.comp.term#clse'(XXd39551, XSuper, 'lo.core#,..'(XCV, 'lo.core#,..'(XPtn, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XBody), XEx2),
    ocall('_coerce%1'(XXV5368),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe5006),XXV5368,XXV5368),
    'lo.comp.transform@makeInheritFields'(XInhFields, XLclName, 'lo.core#some'(XXe5006), XSuper, XFields, XLbVr, XThVr, XEntry, XEn0, XList, XL1),
    'lo.comp.transform@makeInheritanceMap'(XDefs, XLclName, XLbVr, XThVr, XMap, XOpts, XL1, XLx, XFields, XEn0, XEn, XEx2, XExx, XRp1, XRpx).
'lo.comp.transform@genClassMap'(XMap, XOpts, XLc, XLclName, XDefs, XFace, 'lo.core#,..'('lo.comp.transutils#lyr'(XLclName, XList, XLblGl, XLbVr, XThVr), XMap), XEntry, XEn, XEx, XExx, XRp, XRpx):- 'lo.comp.transutils@genVarbl'("LbV", XLbVr),
    'lo.comp.transutils@genVarbl'("ThV", XThVr),
    'lo.comp.transform@pickAllFieldsFromFace'(XFace, XFields),
    'lo.comp.transform@makeClassMtdMap'(XDefs, XLclName, XLbVr, XThVr, XLblGl, 'lo.core#[]', XL0, XFields, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@makeInheritanceMap'(XDefs, XLclName, XLbVr, XThVr, XMap, XOpts, XL0, XList, XFields, XEntry, XEn, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@findClassBody'(XDefs, XStmts):- ocall('in%2'('lo.comp.canon#clRule'(X_34729, X_34730, X_34731, 'lo.comp.canon#theta'(XStmts, X_34732), X_34733, X_34734), XDefs),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.transform@transformDefn'(XMap, XOpts, XLc, XNm, XCond, 'lo.comp.canon#lambda'('lo.comp.canon#equation'(X_34735, X_34736, 'lo.comp.canon#tpl'(XCVars), XValue, X_34737)), 'lo.core#,..'('lo.comp.term#clse'(XQ, XLclPrg, XArgs, XBody), XRx), XRx, XEx, XExx, XRp, XRpx):- 'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transform@trPtns'(XCVars, XArgs, 'lo.core#,..'(XRep, XExtra), XExtra, XQ0, XPreG, XG0, XG7, XG8, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    ocall('+%1'(XXV5369),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@length'(XExtra, XXd39561),
    ocall('_call%3'(XXd39561, 1, XXe5007),XXV5369,XXV5369),
    XLclPrg = 'lo.comp.term#prg'(XLclName, XXe5007),
    'lo.comp.transutils@lookupVarName'(XMap, XNm, XReslt),
    'lo.comp.transutils@programAccess'(XReslt, XLclName, X_34740, X_34741, X_34742),
    'lo.comp.debug@debugPreamble'(XNm, XExtra, XQ0, XG0, XG1, XOpts, XClOpts),
    'lo.comp.transform@trGoal'(XCond, XG4, 'lo.core#,..'('lo.comp.term#neck', XG5), XQ0, XQ2, XMap, XClOpts, XEx0, XEx1, XRp0, XRp1),
    'lo.comp.transform@trExp'(XValue, XRep, XQ2, XQ3, XG5, XG6, XG6, XG7, XMap, XClOpts, XEx1, XExx, XRp1, XRpx),
    'lo.comp.transutils@labelAccess'(XQ3, XQ, XMap, XBody, XPreG),
    'lo.comp.debug@frameDebug'(XNm, 0, XQ, XG1, XG2, XClOpts),
    'lo.comp.debug@lineDebug'(XLc, XG2, XG3, XClOpts),
    'lo.comp.debug@deframeDebug'(XNm, 0, XG8, 'lo.core#[]', XClOpts),
    'lo.comp.debug@breakDebug'(XNm, XG3, XG4, XClOpts).
'lo.comp.transform@transformDefn'(XMap, XOpts, XLc, XNm, XCond, XValue, 'lo.core#,..'('lo.comp.term#clse'(XQ, XLclPrg, 'lo.core#,..'(XRep, XExtra), XBody), XRx), XRx, XEx, XExx, XRp, XRpx):- 'lo.comp.transutils@extraVars'(XMap, XExtra),
    ocall('+%1'(XXV5370),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@length'(XExtra, XXd39564),
    ocall('_call%3'(XXd39564, 1, XXe5008),XXV5370,XXV5370),
    XLclPrg = 'lo.comp.term#prg'(XLclName, XXe5008),
    'lo.comp.transutils@lookupVarName'(XMap, XNm, XReslt),
    'lo.comp.transutils@programAccess'(XReslt, XLclName, X_34746, X_34747, X_34748),
    'lo.comp.debug@debugPreamble'(XNm, XExtra, XQ0, XG0, XG1, XOpts, XClOpts),
    'lo.comp.transform@trGoal'(XCond, XG4, 'lo.core#,..'('lo.comp.term#neck', XG5), XQ0, XQ2, XMap, XClOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trExp'(XValue, XRep, XQ2, XQ3, XG5, XG6, XG6, XG7, XMap, XClOpts, XEx0, XExx, XRp0, XRpx),
    'lo.comp.transutils@labelAccess'(XQ3, XQ, XMap, XBody, XG0),
    'lo.comp.debug@frameDebug'(XNm, 0, XQ, XG1, XG2, XClOpts),
    'lo.comp.debug@lineDebug'(XLc, XG2, XG3, XClOpts),
    'lo.comp.debug@deframeDebug'(XNm, 0, XG7, 'lo.core#[]', XClOpts),
    'lo.comp.debug@breakDebug'(XNm, XG3, XG4, XClOpts).
'lo.comp.transform@transformGrammarRule'('lo.comp.canon#grRule'(XLc, XNm, 'lo.comp.canon#tpl'(XA), 'lo.comp.canon#grTerms'(XPB), XBody), XMap, XOpts, XLclFun, XArity, XExtra, XQNo, 'lo.core#,..'('lo.comp.term#clse'(XQ, 'lo.comp.term#prg'(XLclFun, XArity), 'lo.core#,..'(XStIn, 'lo.core#,..'(XStX, XArgs)), XGoals), XRx), XRx, XEx, XExx, XRp, XRpx):- 'lo.comp.debug@debugPreamble'(XNm, XExtra, XQ0, XG0, XG1, XOpts, XClOpts),
    'lo.comp.transform@trPtns'(XA, XArgs, XExtra, XQ0, XQ1, XG4, XG5, XG6, XG7, XMap, XClOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transutils@genVarbl'("StIn", XStIn),
    'lo.comp.transform@dcgBody'(XBody, XMap, XClOpts, XG5, XG6, XStIn, XStOut, 'lo.core#,..'(XStIn, XQ1), XQ2, XEx0, XEx1, XRp0, XRp1),
    'lo.comp.transform@pushTerminals'(XPB, XMap, XClOpts, XG7, XG8, XStOut, XStX, XQ2, XQ4, XEx1, XExx, XRp1, XRpx),
    'lo.comp.transutils@labelAccess'(XQ4, XQ, XMap, XGoals, XG0),
    'lo.comp.debug@frameDebug'(XNm, XQNo, XQ, XG1, XG2, XClOpts),
    'lo.comp.debug@lineDebug'(XLc, XG2, XG3, XClOpts),
    'lo.comp.debug@deframeDebug'(XNm, XQNo, XG8, 'lo.core#[]', XClOpts),
    'lo.comp.debug@breakDebug'(XNm, XG3, XG4, XClOpts),
    ocall('size%1'(XXV5371),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%1'(XXV5372),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XArgs, XXe5009),XXV5371,XXV5371),
    ocall('_call%3'(XXe5009, 2, XXe5010),XXV5372,XXV5372),
    XArity = XXe5010.
'lo.comp.transform@transformGrammarRules'(X_34754, X_34755, X_34756, X_34757, X_34758, 'lo.core#[]', XNo, XRules, XRules, XEx, XEx, XRp, XRp).
'lo.comp.transform@transformGrammarRules'(XMap, XOpts, XLclFun, XArity, XExtra, 'lo.core#,..'(XRl, XDefs), XNo, XRules, XRx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@transformGrammarRule'(XRl, XMap, XOpts, XLclFun, XArity, XExtra, XNo, XRules, XR0, XEx, XEx0, XRp, XRp0),
    ocall('+%1'(XXV5373),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XNo, 1, XXe5011),XXV5373,XXV5373),
    'lo.comp.transform@transformGrammarRules'(XMap, XOpts, XLclFun, X_34760, XExtra, XDefs, XXe5011, XR0, XRx, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@closureEntry'(XMap, XTLc, XProg, XClosure, XArity, 'lo.core#,..'('lo.comp.term#clse'(XQ, 'lo.comp.term#prg'(XClosure, 3), 'lo.core#,..'(XCallStrct, 'lo.core#,..'(XClosureCons, 'lo.core#,..'('lo.comp.term#anon', 'lo.core#[]'))), 'lo.core#,..'('lo.comp.term#call'(XTLc, 'lo.comp.term#prg'(XProg, XXb19916), XQ), 'lo.core#[]')), XL), XL):- 'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transutils@genVars'(XArity, XXd39568),
    XArgs = XXd39568,
    'lo.list@<>'(XArgs, XExtra, XXd39569),
    XQ = XXd39569,
    'lo.comp.transutils@trCons'("_call", XArity, XXd39570),
    XCallStrct = 'lo.comp.term#cons'(XXd39570, XArgs),
    'lo.comp.transform@cond410'(XXd39574, XXd39573, XXe5012, XXV5374, XXd39572, XClosure, XClosureCons, XExtra),
    'lo.list@length'(XQ, XXb19916).
'lo.comp.transform@transformGrammar'('lo.comp.canon#grammDef'(XLc, XNm, X_34766, 'lo.core#[]', XRls), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx):- 'lo.comp.transutils@lookupRelName'(XMap, XNm, XReslt),
    'lo.comp.transutils@programAccess'(XReslt, XLclFun, X_34767, XClosure, X_34768),
    'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transform@transformGrammarRules'(XMap, XOpts, XLclFun, XArity, XExtra, XRls, 1, XRules, XRx, XEx, XEx0, XRp, XRpx),
    ocall('_coerce%1'(XXV5375),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe5013),XXV5375,XXV5375),
    'lo.comp.transform@closureEntry'(XMap, 'lo.core#some'(XXe5013), XLclFun, XClosure, XArity, XEx0, XExx).
'lo.comp.transform@transformClause'('lo.comp.canon#clause'(XLc, XNm, 'lo.comp.canon#tpl'(XA), XBody), XMap, XOpts, XPrdNme, XArity, XExtra, XQNo, 'lo.core#,..'('lo.comp.term#clse'(XQ, 'lo.comp.term#prg'(XPrdNme, XArity), XArgs, XGoals), XRx), XRx, XEx, XExx, XRp, XRpx):- 'lo.comp.debug@debugPreamble'(XNm, XExtra, XQ0, XG0, XG1, XOpts, XClOpts),
    'lo.comp.transform@trPtns'(XA, XArgs, XExtra, XQ0, XQ1, XG4, XG5, XG7, XG8, XMap, XClOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XBody, XG5, XG7, XQ1, XQ3, XMap, XClOpts, XEx0, XExx, XRp0, XRpx),
    'lo.comp.transutils@labelAccess'(XQ3, XQ, XMap, XGoals, XG0),
    'lo.comp.debug@frameDebug'(XNm, XQNo, XQ, XG1, XG2, XClOpts),
    'lo.comp.debug@lineDebug'(XLc, XG2, XG3, XClOpts),
    'lo.comp.debug@deframeDebug'(XNm, XQNo, XG8, 'lo.core#[]', XClOpts),
    'lo.comp.debug@breakDebug'(XNm, XG3, XG4, XClOpts),
    ocall('size%1'(XXV5376),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('_call%2'(XArgs, XXe5014),XXV5376,XXV5376),
    XArity = XXe5014.
'lo.comp.transform@transformClauses'(X_34770, X_34771, X_34772, X_34773, X_34774, 'lo.core#[]', XNo, XRules, XRules, XEx, XEx, XRpx, XRpx).
'lo.comp.transform@transformClauses'(XMap, XOpts, XPrdNme, XArity, XExtra, 'lo.core#,..'(XCl, XDefs), XNo, XRules, XRx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@transformClause'(XCl, XMap, XOpts, XPrdNme, XArity, XExtra, XNo, XRules, XR0, XEx, XEx0, XRp, XRp0),
    ocall('+%1'(XXV5377),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XNo, 1, XXe5015),XXV5377,XXV5377),
    'lo.comp.transform@transformClauses'(XMap, XOpts, XPrdNme, X_34776, XExtra, XDefs, XXe5015, XR0, XRx, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@transformPredicate'('lo.comp.canon#relDef'(XLc, XNm, X_34777, 'lo.core#[]', XClses), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx):- 'lo.comp.transutils@lookupRelName'(XMap, XNm, XReslt),
    'lo.comp.transutils@programAccess'(XReslt, XPrdNme, X_34778, XClosure, X_34779),
    'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transform@transformClauses'(XMap, XOpts, XPrdNme, XArity, XExtra, XClses, 1, XRules, XRx, XEx, XEx0, XRp, XRpx),
    ocall('_coerce%1'(XXV5378),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe5016),XXV5378,XXV5378),
    'lo.comp.transform@closureEntry'(XMap, 'lo.core#some'(XXe5016), XPrdNme, XClosure, XArity, XEx0, XExx).
'lo.comp.transform@transformEqn'('lo.comp.canon#equation'(XLc, XNm, 'lo.comp.canon#tpl'(XA), XValue, XCond), XMap, XOpts, XLclFun, XArity, XExtra, XQNo, 'lo.core#,..'('lo.comp.term#clse'(XQ, 'lo.comp.term#prg'(XLclFun, XXe5018), XArgs, XBody), XRx), XRx, XEx, XExx, XRp, XRpx):- 'lo.comp.debug@debugPreamble'(XNm, XExtra, XQ0, XLbLx, XFBg, XOpts, XClOpts),
    'lo.comp.transform@trPtns'(XA, XArgs, 'lo.core#,..'(XRep, XExtra), XQ0, XQ1, XPreG, XPreGx, XPostG, XPreV, XMap, XClOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XCond, XPreGx, 'lo.core#,..'('lo.comp.term#neck', XCGx), XQ1, XQ2, XMap, XClOpts, XEx0, XEx1, XRp0, XRp1),
    'lo.comp.transform@trExp'(XValue, XRep, XQ2, XQ3, XPreV, XPVx, XPVx, XPx, XMap, XClOpts, XEx1, XExx, XRp1, XRpx),
    'lo.comp.transutils@labelAccess'(XQ3, XQ, XMap, XBody, XLbLx),
    'lo.comp.debug@frameDebug'(XNm, XQNo, XQ, XFBg, XLG, XClOpts),
    'lo.comp.debug@lineDebug'(XLc, XLG, XPreG, XClOpts),
    'lo.comp.debug@deframeDebug'(XNm, XQNo, XPx, 'lo.core#[]', XClOpts),
    'lo.comp.debug@breakDebug'(XNm, XCGx, XPostG, XClOpts),
    ocall('size%1'(XXV5381),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%1'(XXV5382),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XA, XXe5019),XXV5381,XXV5381),
    ocall('_call%3'(XXe5019, 1, XXe5020),XXV5382,XXV5382),
    XArity = XXe5020,
    ocall('size%1'(XXV5379),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%1'(XXV5380),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%2'(XExtra, XXe5017),XXV5379,XXV5379),
    ocall('_call%3'(XArity, XXe5017, XXe5018),XXV5380,XXV5380).
'lo.comp.transform@transformEquations'(X_34783, X_34784, X_34785, X_34786, X_34787, 'lo.core#[]', X_34788, XRules, XRules, XEx, XEx, XRp, XRp).
'lo.comp.transform@transformEquations'(XMap, XOpts, XLclFun, XArity, XExtra, 'lo.core#,..'(XEqn, XDefs), XNo, XRules, XRx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@transformEqn'(XEqn, XMap, XOpts, XLclFun, XArity, XExtra, XNo, XRules, XR0, XEx, XEx0, XRp, XRp0),
    ocall('+%1'(XXV5383),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XNo, 1, XXe5021),XXV5383,XXV5383),
    'lo.comp.transform@transformEquations'(XMap, XOpts, XLclFun, X_34790, XExtra, XDefs, XXe5021, XR0, XRx, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@transformFunction'('lo.comp.canon#funDef'(XLc, XNm, XTp, 'lo.core#[]', XEqns), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx):- 'lo.comp.transutils@lookupFunName'(XMap, XNm, XReslt),
    'lo.comp.transutils@programAccess'(XReslt, XLclFun, X_34791, XClosure, X_34792),
    'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transform@transformEquations'(XMap, XOpts, XLclFun, XAr, XExtra, XEqns, 1, XRules, XR0, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@failSafeEquation'(XLc, 'lo.comp.term#strng'(XNm), XLclFun, XAr, XR0, XRx, XRp0, XRpx),
    ocall('_coerce%1'(XXV5384),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe5022),XXV5384,XXV5384),
    'lo.comp.transform@closureEntry'(XMap, 'lo.core#some'(XXe5022), XLclFun, XClosure, XAr, XEx0, XExx).
'lo.comp.transform@entryClause'(XMap, XName, XTLc, 'lo.core#,..'('lo.comp.term#clse'(XQ, 'lo.comp.term#prg'(XXb19937, 3), 'lo.core#,..'('lo.comp.term#cons'(XXb19939, XArgs), 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), 'lo.core#,..'('lo.comp.term#neck', 'lo.core#,..'('lo.comp.term#call'(XTLc, 'lo.comp.term#prg'(XProg, XXe5023), XQ), 'lo.core#[]'))), XRx), XRx):- 'lo.comp.transutils@lookupVarName'(XMap, XName, XReslt),
    'lo.comp.transutils@programAccess'(XReslt, XProg, X_34799, X_34800, XArity),
    'lo.comp.transutils@genVars'(XArity, XXd39581),
    XArgs = XXd39581,
    'lo.comp.transutils@genVarbl'("This", XThVr),
    'lo.comp.transutils@genVarbl'("Lbl", XLblVr),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XXd39584),
    XQ = XXd39584,
    'lo.comp.transutils@layerName'(XMap, XXb19937),
    'lo.comp.transutils@trCons'(XName, XArity, XXb19939),
    ocall('+%1'(XXV5385),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XArity, 2, XXe5023),XXV5385,XXV5385).
'lo.comp.transform@closureAccess'(XMap, XName, 'lo.core#,..'('lo.comp.term#clse'('lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), 'lo.comp.term#prg'(XXb19952, 3), 'lo.core#,..'(XLamCons, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), 'lo.core#[]'), XRx), XRx):- 'lo.comp.transutils@lookupVarName'(XMap, XName, XReslt),
    'lo.comp.transutils@programAccess'(XReslt, X_34809, X_34810, XClosure, X_34811),
    'lo.comp.transutils@genVarbl'("This", XThVr),
    'lo.comp.transutils@genVarbl'("Lbl", XLblVr),
    'lo.comp.transutils@trCons'(XName, 1, XXd39585),
    XLamCons = 'lo.comp.term#cons'(XXd39585, 'lo.core#,..'('lo.comp.term#cons'('lo.comp.term#strct'(XClosure, 2), 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), 'lo.core#[]')),
    'lo.comp.transutils@layerName'(XMap, XXb19952).
'lo.comp.transform@transformClassDef'('lo.comp.canon#funDef'(XLc, XNm, XTp, XCx, XEqns), XMap, XOpts, XRules, XRx, XEntry, XEnx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@transformFunction'('lo.comp.canon#funDef'(XLc, XNm, XTp, XCx, XEqns), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx),
    ocall('_coerce%1'(XXV5386),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe5024),XXV5386,XXV5386),
    'lo.comp.transform@entryClause'(XMap, XNm, 'lo.core#some'(XXe5024), XEntry, XEn0),
    'lo.comp.transform@closureAccess'(XMap, XNm, XEn0, XEnx).
'lo.comp.transform@transformClassDef'('lo.comp.canon#relDef'(XLc, XNm, XTp, XCx, XClses), XMap, XOpts, XRules, XRx, XEntry, XEnx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@transformPredicate'('lo.comp.canon#relDef'(XLc, XNm, XTp, XCx, XClses), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx),
    ocall('_coerce%1'(XXV5387),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe5025),XXV5387,XXV5387),
    'lo.comp.transform@entryClause'(XMap, XNm, 'lo.core#some'(XXe5025), XEntry, XEn0),
    'lo.comp.transform@closureAccess'(XMap, XNm, XEn0, XEnx).
'lo.comp.transform@transformClassDef'('lo.comp.canon#grammDef'(XLc, XNm, XTp, XCx, XClses), XMap, XOpts, XRules, XRx, XEntry, XEnx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@transformGrammar'('lo.comp.canon#grammDef'(XLc, XNm, XTp, XCx, XClses), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx),
    ocall('_coerce%1'(XXV5388),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe5026),XXV5388,XXV5388),
    'lo.comp.transform@entryClause'(XMap, XNm, 'lo.core#some'(XXe5026), XEntry, XEn0),
    'lo.comp.transform@closureAccess'(XMap, XNm, XEn0, XEnx).
'lo.comp.transform@transformClassDef'('lo.comp.canon#varDef'(XLc, XNm, X_34815, X_34816, XValue, XCond), XMap, XOpts, XRules, XRx, XEntry, XEnx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@transformDefn'(XMap, XOpts, XLc, XNm, XCond, XValue, XRules, XRx, XEx, XExx, XRp, XRpx),
    ocall('_coerce%1'(XXV5389),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe5027),XXV5389,XXV5389),
    'lo.comp.transform@entryClause'(XMap, XNm, 'lo.core#some'(XXe5027), XEntry, XEnx).
'lo.comp.transform@transformClassDef'('lo.comp.canon#classDef'(XLc, XNm, XTp, XCx, XDefs, XFace), XMap, XOpts, XRules, XRx, XEntry, XEnx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@transformClass'(XLc, XNm, XDefs, XFace, XMap, XOpts, XRules, XRx, XEntry, XE0, XEx, XExx, XRp, XRpx),
    ocall('_coerce%1'(XXV5390),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe5028),XXV5390,XXV5390),
    'lo.comp.transform@entryClause'(XMap, XNm, 'lo.core#some'(XXe5028), XE0, XEnx).
'lo.comp.transform@transformClassDefs'('lo.core#[]', X_34817, X_34818, XRules, XRules, XEntry, XEntry, XExtra, XExtra, XRp, XRp).
'lo.comp.transform@transformClassDefs'('lo.core#,..'(XDef, XDefs), XMap, XOpts, XRules, XRx, XEntry, XEnx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@transformClassDef'(XDef, XMap, XOpts, XRules, XR0, XEntry, XEn0, XEx, XEx1, XRp, XRp0),
    'lo.comp.transform@transformClassDefs'(XDefs, XMap, XOpts, XR0, XRx, XEn0, XEnx, XEx1, XExx, XRp0, XRpx).
'lo.comp.transform@transformClassBody'(XDefs, XMap, XOpts, XRules, XRx, XEntry, XEnx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@one292'(XStmts, XDefs),
    'lo.comp.transform@transformClassDefs'(XStmts, XMap, XOpts, XRules, XRx, XEntry, XEnx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@transformClassBody'(X_34820, X_34821, X_34822, XRules, XRules, XEntry, XEntry, XEx, XEx, XRp, XRp).
'lo.comp.transform@transformClass'(XLc, XNm, XDefs, XFace, XMap, XOpts, XRules, XRx, XEntry, XEntry, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@labelDefn'(XMap, XNm, XLclName, XRules, XR0),
    'lo.comp.transform@one293'(XRp0, XRp, XEx1, XEx, XEn0, XR0, XCMap, XFace, XDefs, XLclName, XLc, XOpts, XMap),
    'lo.comp.transform@transformClassBody'(XDefs, XCMap, XOpts, XEn1, XRx, XEn0, XEn1, XEx1, XExx, XRp0, XRpx).
'lo.comp.transform@transformImplementation'(XLc, XImplName, 'lo.comp.canon#tpl'('lo.core#[]'), XBody, XFace, XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@transformClass'(XLc, XImplName, 'lo.core#,..'('lo.comp.canon#clRule'(XLc, XImplName, 'lo.comp.canon#v'(XLc, XImplName), XBody, 'lo.comp.canon#trueCond', XFace), 'lo.core#[]'), XFace, XMap, XOpts, XRules, XR0, XR0, XRx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@transformImplementation'(XLc, XImplName, XHd, XBody, XFace, XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@transformClass'(XLc, XImplName, 'lo.core#,..'('lo.comp.canon#clRule'(XLc, XImplName, 'lo.comp.canon#apply'(XLc, 'lo.comp.canon#v'(XLc, XImplName), XHd), XBody, 'lo.comp.canon#trueCond', XFace), 'lo.core#[]'), XFace, XMap, XOpts, XRules, XR0, XR0, XRx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@transformMdlDef'('lo.comp.canon#funDef'(XLc, XNm, XTp, XCx, XEqns), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@transformFunction'('lo.comp.canon#funDef'(XLc, XNm, XTp, XCx, XEqns), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@transformMdlDef'('lo.comp.canon#grammDef'(XLc, XNm, XTp, XCx, XRls), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@transformGrammar'('lo.comp.canon#grammDef'(XLc, XNm, XTp, XCx, XRls), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@transformMdlDef'('lo.comp.canon#relDef'(XLc, XNm, XTp, XCx, XClses), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@transformPredicate'('lo.comp.canon#relDef'(XLc, XNm, XTp, XCx, XClses), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@transformMdlDef'('lo.comp.canon#varDef'(XLc, XNm, X_34825, X_34826, XValue, XCond), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@transformDefn'(XMap, XOpts, XLc, XNm, XCond, XValue, XRules, XRx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@transformMdlDef'('lo.comp.canon#classDef'(XLc, XNm, XTp, X_34827, XDefs, XFace), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@transformClass'(XLc, XNm, XDefs, XFace, XMap, XOpts, XRules, XRx, X_34828, X_34829, XEx, XExx, XRp, XRpx).
'lo.comp.transform@transformMdlDef'('lo.comp.canon#typeDef'(X_34830, X_34831, X_34832, X_34833), X_34834, X_34835, XRules, XRules, XEx, XEx, XRp, XRp).
'lo.comp.transform@transformMdlDef'('lo.comp.canon#cnDefn'(X_34836, X_34837, X_34838), X_34839, X_34840, XRules, XRules, XEx, XEx, XRp, XRp).
'lo.comp.transform@transformMdlDef'('lo.comp.canon#implDef'(XLc, X_34841, XImplName, X_34842, X_34843, XHd, XBody, XFace), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@transformImplementation'(XLc, XImplName, XHd, XBody, XFace, XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@transformModuleDefs'('lo.core#[]', X_34844, X_34845, XRules, XRules, XEx, XEx, XRp, XRp).
'lo.comp.transform@transformModuleDefs'('lo.core#,..'(XDef, XDefs), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx):- 'lo.comp.transform@transformMdlDef'(XDef, XMap, XOpts, XRules, XR0, XEx, XEx1, XRp, XRp0),
    'lo.comp.transform@transformModuleDefs'(XDefs, XMap, XOpts, XR0, XRx, XEx1, XExx, XRp0, XRpx).
'lo.comp.transform@isShow'('lo.comp.canon#expShow'(X_34847, X_34848)).
'lo.comp.transform@collectShow'('lo.comp.canon#expShow'(XLc, XS), 'lo.comp.canon#trueCond', 'lo.comp.canon#callCond'(XLc, 'lo.comp.canon#v'(XLc, "_logmsg"), 'lo.comp.canon#tpl'('lo.core#,..'(XS, 'lo.core#[]')))):- !.
'lo.comp.transform@collectShow'('lo.comp.canon#expShow'(XLc, XS), XO, 'lo.comp.canon#conjCond'('lo.comp.canon#callCond'(XLc, 'lo.comp.canon#v'(XLc, "_logmsg"), 'lo.comp.canon#tpl'('lo.core#,..'(XS, 'lo.core#[]'))), XO)):- !.
'lo.comp.transform@collectShow'(_, _, _):- raise_exception('error'("lo.comp.transform@collectShow", 300, 3, 76)).
'lo.comp.transform@transformShows'(XShows, XMap, XOpts, XLc, 'lo.comp.term#prg'(XShowNm, XArity), XRules, XRx, XRp, XRpx):- 'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transutils@layerName'(XMap, XXd39619),
    'lo.comp.transutils@localName'(XXd39619, "@", "show", XXd39620),
    XShowNm = XXd39620,
    ocall('foldRight%1'(XXV5391),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_call%4'('lo.comp.transform^collectShow', 'lo.comp.canon#trueCond', XShows, XXe5029),XXV5391,XXV5391),
    'lo.comp.transform@transformClause'('lo.comp.canon#clause'(XLc, "show", 'lo.comp.canon#tpl'('lo.core#[]'), XXe5029), XMap, XOpts, XShowNm, XArity, XExtra, 1, XRules, XR0, XR0, XRx, XRp, XRpx).
'lo.comp.transform@isAssertion'('lo.comp.canon#integrity'(X_34851, X_34852)).
'lo.comp.transform@collectGoal'('lo.comp.canon#integrity'(X_34853, XG), 'lo.comp.canon#trueCond', XG):- !.
'lo.comp.transform@collectGoal'('lo.comp.canon#integrity'(X_34854, XG), XO, 'lo.comp.canon#conjCond'(XG, XO)):- !.
'lo.comp.transform@collectGoal'(_, _, _):- raise_exception('error'("lo.comp.transform@collectGoal", 290, 3, 41)).
'lo.comp.transform@transformAssertions'(XAsserts, XMap, XOpts, XLc, 'lo.comp.term#prg'(XAssertNm, XArity), XRules, XRx, XRp, XRpx):- 'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transutils@layerName'(XMap, XXd39624),
    'lo.comp.transutils@localName'(XXd39624, "@", "assert", XXd39625),
    XAssertNm = XXd39625,
    ocall('foldRight%1'(XXV5392),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_call%4'('lo.comp.transform^collectGoal', 'lo.comp.canon#trueCond', XAsserts, XXe5030),XXV5392,XXV5392),
    'lo.comp.transform@transformClause'('lo.comp.canon#clause'(XLc, "assert", 'lo.comp.canon#tpl'('lo.core#[]'), XXe5030), XMap, XOpts, XAssertNm, XArity, XExtra, 1, XRules, XR0, XR0, XRx, XRp, XRpx).
'lo.comp.transform@transformOthers'('lo.core#[]', X_34855, X_34856, 'lo.core#,..'('lo.comp.term#neck', 'lo.core#[]'), XRx, XRx, XRp, XRp).
'lo.comp.transform@transformOthers'('lo.core#,..'('lo.comp.canon#integrity'(XLc, XG), XOthers), XMap, XOpts, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XXe5031), XAssertName, 'lo.core#[]'), XInits), XRules, XRx, XRp, XRpx):- 'lo.comp.misc@collect'(XOthers, 'lo.comp.transform^isAssertion', XAsserts, XRest),
    'lo.comp.transform@transformAssertions'('lo.core#,..'('lo.comp.canon#integrity'(XLc, XG), XAsserts), XMap, XOpts, XLc, XAssertName, XRules, XR0, XRp, XRp0),
    'lo.comp.transform@transformOthers'(XRest, XMap, XOpts, XInits, XR0, XRx, XRp0, XRpx),
    ocall('_coerce%1'(XXV5393),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe5031),XXV5393,XXV5393).
'lo.comp.transform@transformOthers'('lo.core#,..'('lo.comp.canon#expShow'(XLc, XE), XOthers), XMap, XOpts, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XXe5032), XShowName, 'lo.core#[]'), XInits), XRules, XRx, XRp, XRpx):- 'lo.comp.misc@collect'(XOthers, 'lo.comp.transform^isShow', XShows, XRest),
    'lo.comp.transform@transformShows'('lo.core#,..'('lo.comp.canon#expShow'(XLc, XE), XShows), XMap, XOpts, XLc, XShowName, XRules, XR0, XRp, XRp0),
    'lo.comp.transform@transformOthers'(XRest, XMap, XOpts, XInits, XR0, XRx, XRp0, XRpx),
    ocall('_coerce%1'(XXV5394),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe5032),XXV5394,XXV5394).
'lo.comp.transform@thetaInit'(XMap, XInits, 'lo.core#,..'('lo.comp.term#clse'('lo.core#[]', 'lo.comp.term#prg'(XXb19995, 0), 'lo.core#[]', XInits), XR), XR):- 'lo.comp.transutils@layerName'(XMap, XXb19994),
    'lo.comp.transutils@localName'(XXb19994, "@", "init", XXb19995).
'lo.comp.transform@transformProg'('lo.comp.canon#canonPkg'(XPkSpec, XImports, XDefs, XOthers), XOpt, 'lo.comp.term#prProg'(XPkSpec, XRules), XRp, XRpx):- XPkSpec = 'lo.comp.package#pkgSpec'('lo.repo#pkg'(XPkg, XVers), X_34865, X_34866, X_34867, X_34868, X_34869, X_34870),
    'lo.comp.transutils@makePkgMap'(XPkg, XDefs, XImports, XXd39634),
    XXd39634 = '()2'(XEnums, XMap),
    'lo.comp.transutils@trOpts'(XOpt, XPkg, XXd39635),
    XPOpts = XXd39635,
    'lo.comp.transform@transformModuleDefs'(XDefs, XMap, XPOpts, XR1, XRx, XRx, 'lo.core#[]', XRp, XRp0),
    'lo.comp.transform@transformOthers'(XOthers, XMap, XPOpts, XInits, XRules, XR0, XRp0, XRpx),
    'lo.comp.transform@thetaInit'(XMap, XInits, XR0, XR1).
'lo.comp.transform@trLocation'(XLc, 'lo.comp.canon#tpl'('lo.core#,..'('lo.comp.canon#int'(XXd39636), 'lo.core#,..'('lo.comp.canon#int'(XXd39638), 'lo.core#,..'('lo.comp.canon#int'(XXd39640), 'lo.core#[]'))))):- !,
    'lo.comp.location@lineOf'(XLc, XXd39636),
    'lo.comp.location@columnOf'(XLc, XXd39638),
    'lo.comp.location@widthOf'(XLc, XXd39640).
'lo.comp.transform@trLocation'(_, _):- raise_exception('error'("lo.comp.transform@trLocation", 304, 3, 75)).
'lo.comp.transform@enumAccess'(XMap, XName, 'lo.core#,..'('lo.comp.term#clse'(XExtra, 'lo.comp.term#prg'(XAccessName, XXe5033), 'lo.core#,..'(XLamCons, XExtra), 'lo.core#[]'), XRx), XRx):- 'lo.comp.transutils@lookupVarName'(XMap, XName, XReslt),
    'lo.comp.transutils@programAccess'(XReslt, XLclName, XAccessName, X_34876, 0),
    'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transform@cond411'(XXd39649, XXd39648, XXe5034, XXV5396, XXd39647, XLclName, XLamCons, XExtra),
    ocall('+%1'(XXV5395),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@length'(XExtra, XXd39646),
    ocall('_call%3'(XXd39646, 1, XXe5033),XXV5395,XXV5395).
'lo.comp.transform@enumAccess'(X_34877, X_34878, XRls, XRls).
'lo.comp.transform@implementPkgRefPtn'('lo.comp.transutils#moduleVar'(X_34879, XVn, X_34880), XLc, X_34881, X_34882, XXi, XXi, XQ, 'lo.core#,..'(XXi, XQ), 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XXe5035), 'lo.comp.term#prg'(XVn, 1), 'lo.core#,..'(XXi, 'lo.core#[]')), XTail), XTail, XRp, XRp):- ocall('_coerce%1'(XXV5397),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe5035),XXV5397,XXV5397).
'lo.comp.transform@implementPkgRefPtn'('lo.comp.transutils#moduleClass'(XEnum, X_34886, 0), X_34887, X_34888, X_34889, X_34890, 'lo.comp.term#enum'(XEnum), XQ, XQ, XTail, XTail, XRp, XRp).
'lo.comp.transform@implementPkgRefPtn'(X_34891, XLc, XPkg, XRf, X_34892, 'lo.comp.term#anon', XQ, XQ, XPost, XPost, XRp, XRpx):- ocall('disp%1'(XXV5398),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('disp%1'(XXV5399),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('_call%2'(XPkg, XXe5036),XXV5398,XXV5398),
    ocall('_call%2'(XRf, XXe5037),XXV5399,XXV5399),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("illegal access to "), 'lo.core#,..'(XXe5036, 'lo.core#,..'('lo.core#ss'("#"), 'lo.core#,..'(XXe5037, 'lo.core#[]'))))), XXd39657),
    'lo.comp.errors@reportError'(XXd39657, XLc, XRp, XRpx).
'lo.comp.transform@implementPkgRefExp'('lo.comp.transutils#moduleVar'(X_34897, XVn, X_34898), XLc, X_34899, X_34900, XXi, XXi, XQ, 'lo.core#,..'(XXi, XQ), 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XXe5038), 'lo.comp.term#prg'(XVn, 1), 'lo.core#,..'(XXi, 'lo.core#[]')), XPre), XPre, XRp, XRp):- ocall('_coerce%1'(XXV5400),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe5038),XXV5400,XXV5400).
'lo.comp.transform@implementPkgRefExp'('lo.comp.transutils#moduleClass'(XEnum, X_34904, 0), X_34905, X_34906, X_34907, X_34908, 'lo.comp.term#enum'(XEnum), XQ, XQ, XPre, XPre, XRp, XRp).
'lo.comp.transform@implementPkgRefExp'(X_34909, XLc, XPkg, XRef, XXi, XXi, XQ, XQ, XPre, XPre, XRp, XRpx):- ocall('disp%1'(XXV5401),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('disp%1'(XXV5402),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('_call%2'(XPkg, XXe5039),XXV5401,XXV5401),
    ocall('_call%2'(XRef, XXe5040),XXV5402,XXV5402),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("illegal access to "), 'lo.core#,..'(XXe5039, 'lo.core#,..'('lo.core#ss'("#"), 'lo.core#,..'(XXe5040, 'lo.core#[]'))))), XXd39665),
    'lo.comp.errors@reportError'(XXd39665, XLc, XRp, XRpx).
'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'('lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc%1'('lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc')):- !.
'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'('_coerce%2'(XV31507, XV31508), XLbl2209, XThis2209):- !,
    'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc@_coerce'(XV31507, XV31508, XLbl2209, XThis2209).
'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'('_coerce%1'('lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc^_coerce'(XLbl2210, XThis2210)), XLbl2210, XThis2210).
'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc@_coerce'('lo.comp.location#loc'(XLine, XOff, XCol, XLen, XPth), 'lo.comp.term#tloc'(XOff, XLen), XLbV2535, XThV2535):- !.
'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc@_coerce", 812, 5, 51)).
'lo.comp.transform^extraArity'('_call%3'(XV30727, XV30728, XV30729), 'lo.comp.transform^extraArity', _):- 'lo.comp.transform@extraArity'(XV30727, XV30728, XV30729).
'lo.comp.transform^makeLabelTerm'('_call%4'(XV30730, XV30731, XV30732, XV30733), 'lo.comp.transform^makeLabelTerm', _):- 'lo.comp.transform@makeLabelTerm'(XV30730, XV30731, XV30732, XV30733).
'lo.comp.transform^labelDefn'('_call%5'(XV30734, XV30735, XV30736, XV30737, XV30738), 'lo.comp.transform^labelDefn', _):- 'lo.comp.transform@labelDefn'(XV30734, XV30735, XV30736, XV30737, XV30738).
'lo.comp.transform^pickAllFieldsFromFace'('_call%2'(XV30739, XV30740), 'lo.comp.transform^pickAllFieldsFromFace', _):- 'lo.comp.transform@pickAllFieldsFromFace'(XV30739, XV30740).
'lo.comp.transform^joinStream'('_call%4'(XV30741, XV30742, XV30743, XV30744), 'lo.comp.transform^joinStream', _):- 'lo.comp.transform@joinStream'(XV30741, XV30742, XV30743, XV30744).
'lo.comp.transform^mkCanon'('_call%2'(XV30745, XV30746), 'lo.comp.transform^mkCanon', _):- 'lo.comp.transform@mkCanon'(XV30745, XV30746).
'lo.comp.transform^mkClosure'('_call%3'(XV30747, XV30748, XV30749), 'lo.comp.transform^mkClosure', _):- 'lo.comp.transform@mkClosure'(XV30747, XV30748, XV30749).
'lo.comp.transform^genRaise'('_call%6'(XV30750, XV30751, XV30752, XV30753, XV30754, XV30755), 'lo.comp.transform^genRaise', _):- 'lo.comp.transform@genRaise'(XV30750, XV30751, XV30752, XV30753, XV30754, XV30755).
'lo.comp.transform^failSafeEquation'('_call%8'(XV30756, XV30757, XV30758, XV30759, XV30760, XV30761, XV30762, XV30763), 'lo.comp.transform^failSafeEquation', _):- 'lo.comp.transform@failSafeEquation'(XV30756, XV30757, XV30758, XV30759, XV30760, XV30761, XV30762, XV30763).
'lo.comp.transform^implementFunCall'('_call%22'(XV30764, XV30765, XV30766, XV30767, XV30768, XV30769, XV30770, XV30771, XV30772, XV30773, XV30774, XV30775, XV30776, XV30777, XV30778, XV30779, XV30780, XV30781, XV30782, XV30783, XV30784, XV30785), 'lo.comp.transform^implementFunCall', _):- 'lo.comp.transform@implementFunCall'(XV30764, XV30765, XV30766, XV30767, XV30768, XV30769, XV30770, XV30771, XV30772, XV30773, XV30774, XV30775, XV30776, XV30777, XV30778, XV30779, XV30780, XV30781, XV30782, XV30783, XV30784, XV30785).
'lo.comp.transform^implementVarExp'('_call%12'(XV30786, XV30787, XV30788, XV30789, XV30790, XV30791, XV30792, XV30793, XV30794, XV30795, XV30796, XV30797), 'lo.comp.transform^implementVarExp', _):- 'lo.comp.transform@implementVarExp'(XV30786, XV30787, XV30788, XV30789, XV30790, XV30791, XV30792, XV30793, XV30794, XV30795, XV30796, XV30797).
'lo.comp.transform^trVarExp'('_call%13'(XV30798, XV30799, XV30800, XV30801, XV30802, XV30803, XV30804, XV30805, XV30806, XV30807, XV30808, XV30809, XV30810), 'lo.comp.transform^trVarExp', _):- 'lo.comp.transform@trVarExp'(XV30798, XV30799, XV30800, XV30801, XV30802, XV30803, XV30804, XV30805, XV30806, XV30807, XV30808, XV30809, XV30810).
'lo.comp.transform^implementPtnCall'('_call%18'(XV30811, XV30812, XV30813, XV30814, XV30815, XV30816, XV30817, XV30818, XV30819, XV30820, XV30821, XV30822, XV30823, XV30824, XV30825, XV30826, XV30827, XV30828), 'lo.comp.transform^implementPtnCall', _):- 'lo.comp.transform@implementPtnCall'(XV30811, XV30812, XV30813, XV30814, XV30815, XV30816, XV30817, XV30818, XV30819, XV30820, XV30821, XV30822, XV30823, XV30824, XV30825, XV30826, XV30827, XV30828).
'lo.comp.transform^trPtnCallOp'('_call%20'(XV30829, XV30830, XV30831, XV30832, XV30833, XV30834, XV30835, XV30836, XV30837, XV30838, XV30839, XV30840, XV30841, XV30842, XV30843, XV30844, XV30845, XV30846, XV30847, XV30848), 'lo.comp.transform^trPtnCallOp', _):- 'lo.comp.transform@trPtnCallOp'(XV30829, XV30830, XV30831, XV30832, XV30833, XV30834, XV30835, XV30836, XV30837, XV30838, XV30839, XV30840, XV30841, XV30842, XV30843, XV30844, XV30845, XV30846, XV30847, XV30848).
'lo.comp.transform^implementVarPtn'('_call%12'(XV30849, XV30850, XV30851, XV30852, XV30853, XV30854, XV30855, XV30856, XV30857, XV30858, XV30859, XV30860), 'lo.comp.transform^implementVarPtn', _):- 'lo.comp.transform@implementVarPtn'(XV30849, XV30850, XV30851, XV30852, XV30853, XV30854, XV30855, XV30856, XV30857, XV30858, XV30859, XV30860).
'lo.comp.transform^trVarPtn'('_call%13'(XV30861, XV30862, XV30863, XV30864, XV30865, XV30866, XV30867, XV30868, XV30869, XV30870, XV30871, XV30872, XV30873), 'lo.comp.transform^trVarPtn', _):- 'lo.comp.transform@trVarPtn'(XV30861, XV30862, XV30863, XV30864, XV30865, XV30866, XV30867, XV30868, XV30869, XV30870, XV30871, XV30872, XV30873).
'lo.comp.transform^trExpCallOp'('_call%20'(XV30874, XV30875, XV30876, XV30877, XV30878, XV30879, XV30880, XV30881, XV30882, XV30883, XV30884, XV30885, XV30886, XV30887, XV30888, XV30889, XV30890, XV30891, XV30892, XV30893), 'lo.comp.transform^trExpCallOp', _):- 'lo.comp.transform@trExpCallOp'(XV30874, XV30875, XV30876, XV30877, XV30878, XV30879, XV30880, XV30881, XV30882, XV30883, XV30884, XV30885, XV30886, XV30887, XV30888, XV30889, XV30890, XV30891, XV30892, XV30893).
'lo.comp.transform^implementDotExp'('_call%18'(XV30894, XV30895, XV30896, XV30897, XV30898, XV30899, XV30900, XV30901, XV30902, XV30903, XV30904, XV30905, XV30906, XV30907, XV30908, XV30909, XV30910, XV30911), 'lo.comp.transform^implementDotExp', _):- 'lo.comp.transform@implementDotExp'(XV30894, XV30895, XV30896, XV30897, XV30898, XV30899, XV30900, XV30901, XV30902, XV30903, XV30904, XV30905, XV30906, XV30907, XV30908, XV30909, XV30910, XV30911).
'lo.comp.transform^trDotExp'('_call%17'(XV30912, XV30913, XV30914, XV30915, XV30916, XV30917, XV30918, XV30919, XV30920, XV30921, XV30922, XV30923, XV30924, XV30925, XV30926, XV30927, XV30928), 'lo.comp.transform^trDotExp', _):- 'lo.comp.transform@trDotExp'(XV30912, XV30913, XV30914, XV30915, XV30916, XV30917, XV30918, XV30919, XV30920, XV30921, XV30922, XV30923, XV30924, XV30925, XV30926, XV30927, XV30928).
'lo.comp.transform@one290'(XRp0, XRp, XEx0, XEx, XOpts, XMap, XPst0, XPost, XPre0, XPre, XQ0, XQ, XA, XP):- 'lo.comp.transform@trPtn'(XP, XA, XQ, XQ0, XPre, XPre0, XPost, XPst0, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    !.
'lo.comp.transform^trPtns'('_call%15'(XV30929, XV30930, XV30931, XV30932, XV30933, XV30934, XV30935, XV30936, XV30937, XV30938, XV30939, XV30940, XV30941, XV30942, XV30943), 'lo.comp.transform^trPtns', _):- 'lo.comp.transform@trPtns'(XV30929, XV30930, XV30931, XV30932, XV30933, XV30934, XV30935, XV30936, XV30937, XV30938, XV30939, XV30940, XV30941, XV30942, XV30943).
'lo.comp.transform^pushTerminals'('_call%13'(XV30944, XV30945, XV30946, XV30947, XV30948, XV30949, XV30950, XV30951, XV30952, XV30953, XV30954, XV30955, XV30956), 'lo.comp.transform^pushTerminals', _):- 'lo.comp.transform@pushTerminals'(XV30944, XV30945, XV30946, XV30947, XV30948, XV30949, XV30950, XV30951, XV30952, XV30953, XV30954, XV30955, XV30956).
'lo.comp.transform@cond405'(XXd39343, XQ, XQ0, XStrmx):- 'var'(XStrmx),
    !,
    'lo.comp.transutils@genVarbl'("DjOut", XStrmx),
    XQ0 = 'lo.core#,..'(XStrmx, XQ).
'lo.comp.transform@cond405'(XXd39343, XQ, XQ0, XStrmx):- XQ0 = XQ.
'lo.comp.transform^dcgDisj'('_call%14'(XV30957, XV30958, XV30959, XV30960, XV30961, XV30962, XV30963, XV30964, XV30965, XV30966, XV30967, XV30968, XV30969, XV30970), 'lo.comp.transform^dcgDisj', _):- 'lo.comp.transform@dcgDisj'(XV30957, XV30958, XV30959, XV30960, XV30961, XV30962, XV30963, XV30964, XV30965, XV30966, XV30967, XV30968, XV30969, XV30970).
'lo.comp.transform@cond406'(XStrmx):- 'var'(XStrmx),
    !,
    'lo.comp.transutils@genVarbl'("CndOut", XStrmx).
'lo.comp.transform@cond406'(XStrmx).
'lo.comp.transform^dcgConditional'('_call%15'(XV30971, XV30972, XV30973, XV30974, XV30975, XV30976, XV30977, XV30978, XV30979, XV30980, XV30981, XV30982, XV30983, XV30984, XV30985), 'lo.comp.transform^dcgConditional', _):- 'lo.comp.transform@dcgConditional'(XV30971, XV30972, XV30973, XV30974, XV30975, XV30976, XV30977, XV30978, XV30979, XV30980, XV30981, XV30982, XV30983, XV30984, XV30985).
'lo.comp.transform@cond407'(XStrmx):- 'var'(XStrmx),
    !,
    'lo.comp.transutils@genVarbl'("DjOut", XStrmx).
'lo.comp.transform@cond407'(XStrmx).
'lo.comp.transform^dcgOne'('_call%13'(XV30986, XV30987, XV30988, XV30989, XV30990, XV30991, XV30992, XV30993, XV30994, XV30995, XV30996, XV30997, XV30998), 'lo.comp.transform^dcgOne', _):- 'lo.comp.transform@dcgOne'(XV30986, XV30987, XV30988, XV30989, XV30990, XV30991, XV30992, XV30993, XV30994, XV30995, XV30996, XV30997, XV30998).
'lo.comp.transform^dcgNeg'('_call%12'(XV30999, XV31000, XV31001, XV31002, XV31003, XV31004, XV31005, XV31006, XV31007, XV31008, XV31009, XV31010), 'lo.comp.transform^dcgNeg', _):- 'lo.comp.transform@dcgNeg'(XV30999, XV31000, XV31001, XV31002, XV31003, XV31004, XV31005, XV31006, XV31007, XV31008, XV31009, XV31010).
'lo.comp.transform^dcgAhead'('_call%12'(XV31011, XV31012, XV31013, XV31014, XV31015, XV31016, XV31017, XV31018, XV31019, XV31020, XV31021, XV31022), 'lo.comp.transform^dcgAhead', _):- 'lo.comp.transform@dcgAhead'(XV31011, XV31012, XV31013, XV31014, XV31015, XV31016, XV31017, XV31018, XV31019, XV31020, XV31021, XV31022).
'lo.comp.transform^trGoal'('_call%11'(XV31023, XV31024, XV31025, XV31026, XV31027, XV31028, XV31029, XV31030, XV31031, XV31032, XV31033), 'lo.comp.transform^trGoal', _):- 'lo.comp.transform@trGoal'(XV31023, XV31024, XV31025, XV31026, XV31027, XV31028, XV31029, XV31030, XV31031, XV31032, XV31033).
'lo.comp.transform^implementGoalCall'('_call%14'(XV31034, XV31035, XV31036, XV31037, XV31038, XV31039, XV31040, XV31041, XV31042, XV31043, XV31044, XV31045, XV31046, XV31047), 'lo.comp.transform^implementGoalCall', _):- 'lo.comp.transform@implementGoalCall'(XV31034, XV31035, XV31036, XV31037, XV31038, XV31039, XV31040, XV31041, XV31042, XV31043, XV31044, XV31045, XV31046, XV31047).
'lo.comp.transform^trGoalDot'('_call%13'(XV31048, XV31049, XV31050, XV31051, XV31052, XV31053, XV31054, XV31055, XV31056, XV31057, XV31058, XV31059, XV31060), 'lo.comp.transform^trGoalDot', _):- 'lo.comp.transform@trGoalDot'(XV31048, XV31049, XV31050, XV31051, XV31052, XV31053, XV31054, XV31055, XV31056, XV31057, XV31058, XV31059, XV31060).
'lo.comp.transform^trGoalCall'('_call%12'(XV31061, XV31062, XV31063, XV31064, XV31065, XV31066, XV31067, XV31068, XV31069, XV31070, XV31071, XV31072), 'lo.comp.transform^trGoalCall', _):- 'lo.comp.transform@trGoalCall'(XV31061, XV31062, XV31063, XV31064, XV31065, XV31066, XV31067, XV31068, XV31069, XV31070, XV31071, XV31072).
'lo.comp.transform@one291'(XRp0, XRp, XEx0, XEx, XOpts, XMap, XPst0, XPost, XPre0, XPre, XQ0, XQ, XA, XP):- 'lo.comp.transform@trExp'(XP, XA, XQ, XQ0, XPre, XPre0, XPost, XPst0, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    !.
'lo.comp.transform^trExps'('_call%15'(XV31073, XV31074, XV31075, XV31076, XV31077, XV31078, XV31079, XV31080, XV31081, XV31082, XV31083, XV31084, XV31085, XV31086, XV31087), 'lo.comp.transform^trExps', _):- 'lo.comp.transform@trExps'(XV31073, XV31074, XV31075, XV31076, XV31077, XV31078, XV31079, XV31080, XV31081, XV31082, XV31083, XV31084, XV31085, XV31086, XV31087).
'lo.comp.transform@cond408'(XXd39451, XQ0, XQ1, XStrmx):- 'var'(XStrmx),
    !,
    'lo.comp.transutils@genVarbl'("Stx", XStrmx),
    XQ1 = 'lo.core#,..'(XStrmx, XQ0).
'lo.comp.transform@cond408'(XXd39451, XQ0, XQ1, XStrmx):- XQ0 = XQ1.
'lo.comp.transform^dcgBody'('_call%13'(XV31088, XV31089, XV31090, XV31091, XV31092, XV31093, XV31094, XV31095, XV31096, XV31097, XV31098, XV31099, XV31100), 'lo.comp.transform^dcgBody', _):- 'lo.comp.transform@dcgBody'(XV31088, XV31089, XV31090, XV31091, XV31092, XV31093, XV31094, XV31095, XV31096, XV31097, XV31098, XV31099, XV31100).
'lo.comp.transform^trLambda'('_call%9'(XV31101, XV31102, XV31103, XV31104, XV31105, XV31106, XV31107, XV31108, XV31109), 'lo.comp.transform^trLambda', _):- 'lo.comp.transform@trLambda'(XV31101, XV31102, XV31103, XV31104, XV31105, XV31106, XV31107, XV31108, XV31109).
'lo.comp.transform^trExp'('_call%14'(XV31110, XV31111, XV31112, XV31113, XV31114, XV31115, XV31116, XV31117, XV31118, XV31119, XV31120, XV31121, XV31122, XV31123), 'lo.comp.transform^trExp', _):- 'lo.comp.transform@trExp'(XV31110, XV31111, XV31112, XV31113, XV31114, XV31115, XV31116, XV31117, XV31118, XV31119, XV31120, XV31121, XV31122, XV31123).
'lo.comp.transform^trPtn'('_call%14'(XV31124, XV31125, XV31126, XV31127, XV31128, XV31129, XV31130, XV31131, XV31132, XV31133, XV31134, XV31135, XV31136, XV31137), 'lo.comp.transform^trPtn', _):- 'lo.comp.transform@trPtn'(XV31124, XV31125, XV31126, XV31127, XV31128, XV31129, XV31130, XV31131, XV31132, XV31133, XV31134, XV31135, XV31136, XV31137).
'lo.comp.transform@neg328'(XTp):- 'lo.comp.types@isClassType'(XTp),
    !,
    fail.
'lo.comp.transform@neg328'(XTp).
'lo.comp.transform^collectMtd'('_call%6'(XV31138, XV31139, XV31140, XV31141, XV31142, XV31143), 'lo.comp.transform^collectMtd', _):- 'lo.comp.transform@collectMtd'(XV31138, XV31139, XV31140, XV31141, XV31142, XV31143).
'lo.comp.transform^collectMtds'('_call%7'(XV31144, XV31145, XV31146, XV31147, XV31148, XV31149, XV31150), 'lo.comp.transform^collectMtds', _):- 'lo.comp.transform@collectMtds'(XV31144, XV31145, XV31146, XV31147, XV31148, XV31149, XV31150).
'lo.comp.transform^collectLabelVars'('_call%5'(XV31151, XV31152, XV31153, XV31154, XV31155), 'lo.comp.transform^collectLabelVars', _):- 'lo.comp.transform@collectLabelVars'(XV31151, XV31152, XV31153, XV31154, XV31155).
'lo.comp.transform^makeLblTerm'('_call%3'(XV31156, XV31157, XV31158), 'lo.comp.transform^makeLblTerm', _):- 'lo.comp.transform@makeLblTerm'(XV31156, XV31157, XV31158).
'lo.comp.transform@cond409'(XXd39536, XXd39535, XLblTerm, XLbVr, XXd39534, XXe5003, XXV5365, XLc, XLblGl, XExtra):- XExtra = 'lo.core#[]',
    !,
    XLblGl = 'lo.core#[]'.
'lo.comp.transform@cond409'(XXd39536, XXd39535, XLblTerm, XLbVr, XXd39534, XXe5003, XXV5365, XLc, XLblGl, XExtra):- ocall('_coerce%1'(XXV5365),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('_call%2'(XLc, XXe5003),XXV5365,XXV5365),
    XLblGl = 'lo.core#,..'('lo.comp.term#unfy'('lo.core#some'(XXe5003), XLbVr, XLblTerm), 'lo.core#[]').
'lo.comp.transform^makeClassMtdMap'('_call%14'(XV31159, XV31160, XV31161, XV31162, XV31163, XV31164, XV31165, XV31166, XV31167, XV31168, XV31169, XV31170, XV31171, XV31172), 'lo.comp.transform^makeClassMtdMap', _):- 'lo.comp.transform@makeClassMtdMap'(XV31159, XV31160, XV31161, XV31162, XV31163, XV31164, XV31165, XV31166, XV31167, XV31168, XV31169, XV31170, XV31171, XV31172).
'lo.comp.transform^inheritClause'('_call%7'(XV31173, XV31174, XV31175, XV31176, XV31177, XV31178, XV31179), 'lo.comp.transform^inheritClause', _):- 'lo.comp.transform@inheritClause'(XV31173, XV31174, XV31175, XV31176, XV31177, XV31178, XV31179).
'lo.comp.transform@neg329'(XTp):- 'lo.comp.types@isPredType'(XTp),
    !,
    fail.
'lo.comp.transform@neg329'(XTp).
'lo.comp.transform^makeInheritFields'('_call%11'(XV31180, XV31181, XV31182, XV31183, XV31184, XV31185, XV31186, XV31187, XV31188, XV31189, XV31190), 'lo.comp.transform^makeInheritFields', _):- 'lo.comp.transform@makeInheritFields'(XV31180, XV31181, XV31182, XV31183, XV31184, XV31185, XV31186, XV31187, XV31188, XV31189, XV31190).
'lo.comp.transform^makeInheritanceMap'('_call%15'(XV31191, XV31192, XV31193, XV31194, XV31195, XV31196, XV31197, XV31198, XV31199, XV31200, XV31201, XV31202, XV31203, XV31204, XV31205), 'lo.comp.transform^makeInheritanceMap', _):- 'lo.comp.transform@makeInheritanceMap'(XV31191, XV31192, XV31193, XV31194, XV31195, XV31196, XV31197, XV31198, XV31199, XV31200, XV31201, XV31202, XV31203, XV31204, XV31205).
'lo.comp.transform^genClassMap'('_call%13'(XV31206, XV31207, XV31208, XV31209, XV31210, XV31211, XV31212, XV31213, XV31214, XV31215, XV31216, XV31217, XV31218), 'lo.comp.transform^genClassMap', _):- 'lo.comp.transform@genClassMap'(XV31206, XV31207, XV31208, XV31209, XV31210, XV31211, XV31212, XV31213, XV31214, XV31215, XV31216, XV31217, XV31218).
'lo.comp.transform^findClassBody'('_call%2'(XV31219, XV31220), 'lo.comp.transform^findClassBody', _):- 'lo.comp.transform@findClassBody'(XV31219, XV31220).
'lo.comp.transform^transformDefn'('_call%12'(XV31221, XV31222, XV31223, XV31224, XV31225, XV31226, XV31227, XV31228, XV31229, XV31230, XV31231, XV31232), 'lo.comp.transform^transformDefn', _):- 'lo.comp.transform@transformDefn'(XV31221, XV31222, XV31223, XV31224, XV31225, XV31226, XV31227, XV31228, XV31229, XV31230, XV31231, XV31232).
'lo.comp.transform^transformGrammarRule'('_call%13'(XV31233, XV31234, XV31235, XV31236, XV31237, XV31238, XV31239, XV31240, XV31241, XV31242, XV31243, XV31244, XV31245), 'lo.comp.transform^transformGrammarRule', _):- 'lo.comp.transform@transformGrammarRule'(XV31233, XV31234, XV31235, XV31236, XV31237, XV31238, XV31239, XV31240, XV31241, XV31242, XV31243, XV31244, XV31245).
'lo.comp.transform^transformGrammarRules'('_call%13'(XV31246, XV31247, XV31248, XV31249, XV31250, XV31251, XV31252, XV31253, XV31254, XV31255, XV31256, XV31257, XV31258), 'lo.comp.transform^transformGrammarRules', _):- 'lo.comp.transform@transformGrammarRules'(XV31246, XV31247, XV31248, XV31249, XV31250, XV31251, XV31252, XV31253, XV31254, XV31255, XV31256, XV31257, XV31258).
'lo.comp.transform@cond410'(XXd39574, XXd39573, XXe5012, XXV5374, XXd39572, XClosure, XClosureCons, XExtra):- XExtra = 'lo.core#[]',
    !,
    XClosureCons = 'lo.comp.term#enum'(XClosure).
'lo.comp.transform@cond410'(XXd39574, XXd39573, XXe5012, XXV5374, XXd39572, XClosure, XClosureCons, XExtra):- ocall('size%1'(XXV5374),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('_call%2'(XExtra, XXe5012),XXV5374,XXV5374),
    XClosureCons = 'lo.comp.term#cons'('lo.comp.term#strct'(XClosure, XXe5012), XExtra).
'lo.comp.transform^closureEntry'('_call%7'(XV31259, XV31260, XV31261, XV31262, XV31263, XV31264, XV31265), 'lo.comp.transform^closureEntry', _):- 'lo.comp.transform@closureEntry'(XV31259, XV31260, XV31261, XV31262, XV31263, XV31264, XV31265).
'lo.comp.transform^transformGrammar'('_call%9'(XV31266, XV31267, XV31268, XV31269, XV31270, XV31271, XV31272, XV31273, XV31274), 'lo.comp.transform^transformGrammar', _):- 'lo.comp.transform@transformGrammar'(XV31266, XV31267, XV31268, XV31269, XV31270, XV31271, XV31272, XV31273, XV31274).
'lo.comp.transform^transformClause'('_call%13'(XV31275, XV31276, XV31277, XV31278, XV31279, XV31280, XV31281, XV31282, XV31283, XV31284, XV31285, XV31286, XV31287), 'lo.comp.transform^transformClause', _):- 'lo.comp.transform@transformClause'(XV31275, XV31276, XV31277, XV31278, XV31279, XV31280, XV31281, XV31282, XV31283, XV31284, XV31285, XV31286, XV31287).
'lo.comp.transform^transformClauses'('_call%13'(XV31288, XV31289, XV31290, XV31291, XV31292, XV31293, XV31294, XV31295, XV31296, XV31297, XV31298, XV31299, XV31300), 'lo.comp.transform^transformClauses', _):- 'lo.comp.transform@transformClauses'(XV31288, XV31289, XV31290, XV31291, XV31292, XV31293, XV31294, XV31295, XV31296, XV31297, XV31298, XV31299, XV31300).
'lo.comp.transform^transformPredicate'('_call%9'(XV31301, XV31302, XV31303, XV31304, XV31305, XV31306, XV31307, XV31308, XV31309), 'lo.comp.transform^transformPredicate', _):- 'lo.comp.transform@transformPredicate'(XV31301, XV31302, XV31303, XV31304, XV31305, XV31306, XV31307, XV31308, XV31309).
'lo.comp.transform^transformEqn'('_call%13'(XV31310, XV31311, XV31312, XV31313, XV31314, XV31315, XV31316, XV31317, XV31318, XV31319, XV31320, XV31321, XV31322), 'lo.comp.transform^transformEqn', _):- 'lo.comp.transform@transformEqn'(XV31310, XV31311, XV31312, XV31313, XV31314, XV31315, XV31316, XV31317, XV31318, XV31319, XV31320, XV31321, XV31322).
'lo.comp.transform^transformEquations'('_call%13'(XV31323, XV31324, XV31325, XV31326, XV31327, XV31328, XV31329, XV31330, XV31331, XV31332, XV31333, XV31334, XV31335), 'lo.comp.transform^transformEquations', _):- 'lo.comp.transform@transformEquations'(XV31323, XV31324, XV31325, XV31326, XV31327, XV31328, XV31329, XV31330, XV31331, XV31332, XV31333, XV31334, XV31335).
'lo.comp.transform^transformFunction'('_call%9'(XV31336, XV31337, XV31338, XV31339, XV31340, XV31341, XV31342, XV31343, XV31344), 'lo.comp.transform^transformFunction', _):- 'lo.comp.transform@transformFunction'(XV31336, XV31337, XV31338, XV31339, XV31340, XV31341, XV31342, XV31343, XV31344).
'lo.comp.transform^entryClause'('_call%5'(XV31345, XV31346, XV31347, XV31348, XV31349), 'lo.comp.transform^entryClause', _):- 'lo.comp.transform@entryClause'(XV31345, XV31346, XV31347, XV31348, XV31349).
'lo.comp.transform^closureAccess'('_call%4'(XV31350, XV31351, XV31352, XV31353), 'lo.comp.transform^closureAccess', _):- 'lo.comp.transform@closureAccess'(XV31350, XV31351, XV31352, XV31353).
'lo.comp.transform^transformClassDef'('_call%11'(XV31354, XV31355, XV31356, XV31357, XV31358, XV31359, XV31360, XV31361, XV31362, XV31363, XV31364), 'lo.comp.transform^transformClassDef', _):- 'lo.comp.transform@transformClassDef'(XV31354, XV31355, XV31356, XV31357, XV31358, XV31359, XV31360, XV31361, XV31362, XV31363, XV31364).
'lo.comp.transform^transformClassDefs'('_call%11'(XV31365, XV31366, XV31367, XV31368, XV31369, XV31370, XV31371, XV31372, XV31373, XV31374, XV31375), 'lo.comp.transform^transformClassDefs', _):- 'lo.comp.transform@transformClassDefs'(XV31365, XV31366, XV31367, XV31368, XV31369, XV31370, XV31371, XV31372, XV31373, XV31374, XV31375).
'lo.comp.transform@one292'(XStmts, XDefs):- 'lo.comp.transform@findClassBody'(XDefs, XStmts),
    !.
'lo.comp.transform^transformClassBody'('_call%11'(XV31376, XV31377, XV31378, XV31379, XV31380, XV31381, XV31382, XV31383, XV31384, XV31385, XV31386), 'lo.comp.transform^transformClassBody', _):- 'lo.comp.transform@transformClassBody'(XV31376, XV31377, XV31378, XV31379, XV31380, XV31381, XV31382, XV31383, XV31384, XV31385, XV31386).
'lo.comp.transform@one293'(XRp0, XRp, XEx1, XEx, XEn0, XR0, XCMap, XFace, XDefs, XLclName, XLc, XOpts, XMap):- 'lo.comp.transform@genClassMap'(XMap, XOpts, XLc, XLclName, XDefs, XFace, XCMap, XR0, XEn0, XEx, XEx1, XRp, XRp0),
    !.
'lo.comp.transform^transformClass'('_call%14'(XV31387, XV31388, XV31389, XV31390, XV31391, XV31392, XV31393, XV31394, XV31395, XV31396, XV31397, XV31398, XV31399, XV31400), 'lo.comp.transform^transformClass', _):- 'lo.comp.transform@transformClass'(XV31387, XV31388, XV31389, XV31390, XV31391, XV31392, XV31393, XV31394, XV31395, XV31396, XV31397, XV31398, XV31399, XV31400).
'lo.comp.transform^transformImplementation'('_call%13'(XV31401, XV31402, XV31403, XV31404, XV31405, XV31406, XV31407, XV31408, XV31409, XV31410, XV31411, XV31412, XV31413), 'lo.comp.transform^transformImplementation', _):- 'lo.comp.transform@transformImplementation'(XV31401, XV31402, XV31403, XV31404, XV31405, XV31406, XV31407, XV31408, XV31409, XV31410, XV31411, XV31412, XV31413).
'lo.comp.transform^transformMdlDef'('_call%9'(XV31414, XV31415, XV31416, XV31417, XV31418, XV31419, XV31420, XV31421, XV31422), 'lo.comp.transform^transformMdlDef', _):- 'lo.comp.transform@transformMdlDef'(XV31414, XV31415, XV31416, XV31417, XV31418, XV31419, XV31420, XV31421, XV31422).
'lo.comp.transform^transformModuleDefs'('_call%9'(XV31423, XV31424, XV31425, XV31426, XV31427, XV31428, XV31429, XV31430, XV31431), 'lo.comp.transform^transformModuleDefs', _):- 'lo.comp.transform@transformModuleDefs'(XV31423, XV31424, XV31425, XV31426, XV31427, XV31428, XV31429, XV31430, XV31431).
'lo.comp.transform^isShow'('_call%1'(XV31432), 'lo.comp.transform^isShow', _):- 'lo.comp.transform@isShow'(XV31432).
'lo.comp.transform^collectShow'('_call%3'(XV31433, XV31434, XV31435), 'lo.comp.transform^collectShow', _):- 'lo.comp.transform@collectShow'(XV31433, XV31434, XV31435).
'lo.comp.transform^transformShows'('_call%9'(XV31436, XV31437, XV31438, XV31439, XV31440, XV31441, XV31442, XV31443, XV31444), 'lo.comp.transform^transformShows', _):- 'lo.comp.transform@transformShows'(XV31436, XV31437, XV31438, XV31439, XV31440, XV31441, XV31442, XV31443, XV31444).
'lo.comp.transform^isAssertion'('_call%1'(XV31445), 'lo.comp.transform^isAssertion', _):- 'lo.comp.transform@isAssertion'(XV31445).
'lo.comp.transform^collectGoal'('_call%3'(XV31446, XV31447, XV31448), 'lo.comp.transform^collectGoal', _):- 'lo.comp.transform@collectGoal'(XV31446, XV31447, XV31448).
'lo.comp.transform^transformAssertions'('_call%9'(XV31449, XV31450, XV31451, XV31452, XV31453, XV31454, XV31455, XV31456, XV31457), 'lo.comp.transform^transformAssertions', _):- 'lo.comp.transform@transformAssertions'(XV31449, XV31450, XV31451, XV31452, XV31453, XV31454, XV31455, XV31456, XV31457).
'lo.comp.transform^transformOthers'('_call%8'(XV31458, XV31459, XV31460, XV31461, XV31462, XV31463, XV31464, XV31465), 'lo.comp.transform^transformOthers', _):- 'lo.comp.transform@transformOthers'(XV31458, XV31459, XV31460, XV31461, XV31462, XV31463, XV31464, XV31465).
'lo.comp.transform^thetaInit'('_call%4'(XV31466, XV31467, XV31468, XV31469), 'lo.comp.transform^thetaInit', _):- 'lo.comp.transform@thetaInit'(XV31466, XV31467, XV31468, XV31469).
'lo.comp.transform^transformProg'('_call%5'(XV31470, XV31471, XV31472, XV31473, XV31474), 'lo.comp.transform^transformProg', _):- 'lo.comp.transform@transformProg'(XV31470, XV31471, XV31472, XV31473, XV31474).
'lo.comp.transform^trLocation'('_call%2'(XV31475, XV31476), 'lo.comp.transform^trLocation', _):- 'lo.comp.transform@trLocation'(XV31475, XV31476).
'lo.comp.transform@cond411'(XXd39649, XXd39648, XXe5034, XXV5396, XXd39647, XLclName, XLamCons, XExtra):- XExtra = 'lo.core#[]',
    !,
    XLamCons = 'lo.comp.term#enum'(XLclName).
'lo.comp.transform@cond411'(XXd39649, XXd39648, XXe5034, XXV5396, XXd39647, XLclName, XLamCons, XExtra):- ocall('size%1'(XXV5396),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('_call%2'(XExtra, XXe5034),XXV5396,XXV5396),
    XLamCons = 'lo.comp.term#cons'('lo.comp.term#strct'(XLclName, XXe5034), XExtra).
'lo.comp.transform^enumAccess'('_call%4'(XV31477, XV31478, XV31479, XV31480), 'lo.comp.transform^enumAccess', _):- 'lo.comp.transform@enumAccess'(XV31477, XV31478, XV31479, XV31480).
'lo.comp.transform^implementPkgRefPtn'('_call%12'(XV31481, XV31482, XV31483, XV31484, XV31485, XV31486, XV31487, XV31488, XV31489, XV31490, XV31491, XV31492), 'lo.comp.transform^implementPkgRefPtn', _):- 'lo.comp.transform@implementPkgRefPtn'(XV31481, XV31482, XV31483, XV31484, XV31485, XV31486, XV31487, XV31488, XV31489, XV31490, XV31491, XV31492).
'lo.comp.transform^implementPkgRefExp'('_call%12'(XV31493, XV31494, XV31495, XV31496, XV31497, XV31498, XV31499, XV31500, XV31501, XV31502, XV31503, XV31504), 'lo.comp.transform^implementPkgRefExp', _):- 'lo.comp.transform@implementPkgRefExp'(XV31493, XV31494, XV31495, XV31496, XV31497, XV31498, XV31499, XV31500, XV31501, XV31502, XV31503, XV31504).
'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc^_coerce'('_call%2'(XV31505, XV31506), 'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc^_coerce'(XLbV2535, XThV2535), _):- 'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc@_coerce'(XV31505, XV31506, XLbV2535, XThV2535).
