'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.code.pkg's'0.0.1'n11o11'()11'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.reduce'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.asm'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.clause'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.code'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.indexing'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.instructions'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.registers'e'*'s\"I1'compileMdl'FT1t'lo.comp.term*prProg't'lo.comp.code.asm*codeMdl'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.code.pkg@init'():- !.
'lo.comp.code.pkg@defltSeq'(XPrefix, XLb, 'lo.core#,..'(XCl, 'lo.core#[]'), 'lo.core#true', XLts, XLtx, 'lo.core#,..'('()2'(XLb, XCl), XEntries), XEntries, XSm, XSMx, 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XLb), XClCode)):- 'lo.comp.reduce@reduceClause'(XCl, XXd42239),
    'lo.comp.code.clause@compCl'(XXd42239, XLts, XSm, XXd42240),
    XXd42240 = 'lo.comp.code.code#assem'(X_36840, XClCode, XLtx, XSMx),
    !.
'lo.comp.code.pkg@defltSeq'(XPrefix, XLb, 'lo.core#,..'(XCl, 'lo.core#[]'), 'lo.core#false', XLts, XLtx, 'lo.core#,..'('()2'(XLbx, XCl), XEntries), XEntries, XSm, XSmx, 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XLb), 'lo.core#,..'('lo.comp.code.instructions#iTrustme', 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XLbx), XClCode)))):- 'lo.comp.code.clause@newLabel'("Cl", XXd42244),
    XLbx = XXd42244,
    'lo.comp.reduce@reduceClause'(XCl, XXd42245),
    'lo.comp.code.clause@compCl'(XXd42245, XLts, XSm, XXd42246),
    XXd42246 = 'lo.comp.code.code#assem'(X_36844, XClCode, XLtx, XSmx),
    !.
'lo.comp.code.pkg@defltSeq'(XPrefix, XLb, 'lo.core#,..'(XCl, XRest), 'lo.core#true', XLts, XLtx, 'lo.core#,..'('()2'(XLbx, XCl), XEntries), XEnx, XSm, XSmx, XXd42265):- 'lo.comp.code.clause@newLabel'("Cl", XXd42253),
    XLbx = XXd42253,
    'lo.comp.code.clause@newLabel'("Nxt", XXd42254),
    XLbnxt = XXd42254,
    'lo.comp.reduce@reduceClause'(XCl, XXd42255),
    'lo.comp.code.clause@compCl'(XXd42255, XLts, XSm, XXd42256),
    XXd42256 = 'lo.comp.code.code#assem'(X_36850, XClCode, XLt0, XSm1),
    !,
    'lo.comp.code.pkg@defltSeq'(XPrefix, XLbnxt, XRest, 'lo.core#false', XLt0, XLtx, XEntries, XEnx, XSm1, XSmx, XXd42264),
    'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XLb), 'lo.core#,..'('lo.comp.code.instructions#iTryme'(XLbnxt), 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XLbx), XClCode))), XXd42264, XXd42265).
'lo.comp.code.pkg@defltSeq'(XPrefix, XLb, 'lo.core#,..'(XCl, XRest), 'lo.core#false', XLts, XLtx, 'lo.core#,..'('()2'(XLbx, XCl), XEntries), XEnx, XSm, XSmx, XXd42278):- 'lo.comp.code.clause@newLabel'("Cl", XXd42266),
    XLbx = XXd42266,
    'lo.comp.code.clause@newLabel'("Nxt", XXd42267),
    XLbnxt = XXd42267,
    'lo.comp.reduce@reduceClause'(XCl, XXd42268),
    'lo.comp.code.clause@compCl'(XXd42268, XLts, XSm, XXd42269),
    XXd42269 = 'lo.comp.code.code#assem'(X_36856, XClCode, XLt0, XSm1),
    !,
    'lo.comp.code.pkg@defltSeq'(XPrefix, XLbnxt, XRest, 'lo.core#false', XLt0, XLtx, XEntries, XEnx, XSm1, XSmx, XXd42277),
    'lo.list@<>'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XLb), 'lo.core#,..'('lo.comp.code.instructions#iRetryme'(XLbnxt), 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XLbx), XClCode))), XXd42277, XXd42278).
'lo.comp.code.pkg@defltSeq'(X_36860, XLb, 'lo.core#[]', X_36861, XLts, XLts, XEntries, XEntries, XSm, XSm, 'lo.core#,..'('lo.comp.code.instructions#iLbl'(XLb), 'lo.core#,..'('lo.comp.code.instructions#iFayl', 'lo.core#[]'))):- !.
'lo.comp.code.pkg@defltSeq'(_, _, _, _, _, _, _, _, _, _, _):- raise_exception('error'("lo.comp.code.pkg@defltSeq", 38, 3, 160)).
'lo.comp.code.pkg@compRel'('lo.comp.term#prg'(XNm, XAr), XCls, XCode):- ocall('disp%1'(XXV5658),'lo.core$display$lo.core*list'('lo.core$display$lo.comp.term*clse'),'lo.core$display$lo.core*list'('lo.core$display$lo.comp.term*clse')),
    ocall('_call%2'(XCls, XXe5235),XXV5658,XXV5658),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Compiling "), 'lo.core#,..'(XXe5235, 'lo.core#[]'))), XXd42287),
    'lo.io@logMsg'(XXd42287),
    ocall('+%1'(XXV5659),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    ocall('_call%3'(XNm, "_", XXe5236),XXV5659,XXV5659),
    XPrefix = XXe5236,
    'lo.comp.code.clause@newLabel'(XPrefix, XXd42288),
    XL0 = XXd42288,
    'lo.comp.code.pkg@defltSeq'(XPrefix, XL0, XCls, 'lo.core#true', 'lo.core#[]', XLts, XEntries, 'lo.core#[]', 'lo.core#[]', XSrcMap, XXd42289),
    XDefltCode = XXd42289,
    'lo.comp.code.indexing@genIndex'(XEntries, XL0, XXd42290),
    XIndexCode = XXd42290,
    'lo.list@<>'(XIndexCode, XDefltCode, XXd42292),
    'lo.comp.code.clause@newLabel'(XPrefix, XXd42293),
    XCode = 'lo.comp.code.code#assem'('lo.comp.term#prg'(XNm, XAr), XXd42292, 'lo.core#,..'('lo.comp.code.code#litrl'(XXd42293, 'lo.comp.term#prg'(XNm, XAr)), XLts), XSrcMap),
    ocall('disp%1'(XXV5660),'lo.core$display$lo.comp.code.code*assem','lo.core$display$lo.comp.code.code*assem'),
    ocall('_call%2'(XCode, XXe5237),XXV5660,XXV5660),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Assembler output: "), 'lo.core#,..'(XXe5237, 'lo.core#[]'))), XXd42302),
    'lo.io@logMsg'(XXd42302),
    !.
'lo.comp.code.pkg@compRel'(_, _, _):- raise_exception('error'("lo.comp.code.pkg@compRel", 28, 3, 367)).
'lo.comp.code.pkg@pickupDefs'('lo.core#[]', XD, XD):- !.
'lo.comp.code.pkg@pickupDefs'('lo.core#,..'(XCl, XL), XD, XXd42308):- XCl = 'lo.comp.term#clse'(X_36870, XNm, X_36871, X_36872),
    ocall('present%3'(XD, XNm, XClses),'lo.collection$map$lo.index*map'('lo.core$equality$lo.comp.term*term'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.comp.term*term')),
    !,
    ocall('_put%1'(XXV5661),'lo.collection$map$lo.index*map'('lo.core$equality$lo.comp.term*term'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.comp.term*term')),
    'lo.list@<>'(XClses, 'lo.core#,..'(XCl, 'lo.core#[]'), XXd42306),
    ocall('_call%4'(XD, XNm, XXd42306, XXe5238),XXV5661,XXV5661),
    'lo.comp.code.pkg@pickupDefs'(XL, XXe5238, XXd42308).
'lo.comp.code.pkg@pickupDefs'('lo.core#,..'(XCl, XL), XD, XXd42312):- XCl = 'lo.comp.term#clse'(X_36875, XNm, X_36876, X_36877),
    !,
    ocall('_put%1'(XXV5662),'lo.collection$map$lo.index*map'('lo.core$equality$lo.comp.term*term'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.comp.term*term')),
    ocall('_call%4'(XD, XNm, 'lo.core#,..'(XCl, 'lo.core#[]'), XXe5239),XXV5662,XXV5662),
    'lo.comp.code.pkg@pickupDefs'(XL, XXe5239, XXd42312).
'lo.comp.code.pkg@pickupDefs'(_, _, _):- raise_exception('error'("lo.comp.code.pkg@pickupDefs", 23, 3, 19)).
'lo.comp.code.pkg@compileMdl'('lo.comp.term#prProg'(XSpec, XClses), 'lo.comp.code.asm#codeMdl'(XSpec, XXe5241)):- !,
    ocall('pairs%1'(XXV5664),'lo.collection$map$lo.index*map'('lo.core$equality$lo.comp.term*term'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.comp.term*term')),
    ocall('//%1'(XXV5665),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    ocall('_empty%1'(XXV5663),'lo.collection$map$lo.index*map'('lo.core$equality$lo.comp.term*term'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.comp.term*term')),
    'lo.comp.code.pkg@pickupDefs'(XClses, XXV5663, XXd42314),
    ocall('_call%2'(XXd42314, XXe5240),XXV5664,XXV5664),
    ocall('_call%3'(XXe5240, 'lo.comp.code.pkg@fun124', XXe5241),XXV5665,XXV5665).
'lo.comp.code.pkg@compileMdl'(_, _):- raise_exception('error'("lo.comp.code.pkg@compileMdl", 19, 3, 117)).
'lo.comp.code.pkg^defltSeq'('_call%11'(XV33467, XV33468, XV33469, XV33470, XV33471, XV33472, XV33473, XV33474, XV33475, XV33476, XV33477), 'lo.comp.code.pkg^defltSeq', _):- 'lo.comp.code.pkg@defltSeq'(XV33467, XV33468, XV33469, XV33470, XV33471, XV33472, XV33473, XV33474, XV33475, XV33476, XV33477).
'lo.comp.code.pkg^compRel'('_call%3'(XV33478, XV33479, XV33480), 'lo.comp.code.pkg^compRel', _):- 'lo.comp.code.pkg@compRel'(XV33478, XV33479, XV33480).
'lo.comp.code.pkg^pickupDefs'('_call%3'(XV33481, XV33482, XV33483), 'lo.comp.code.pkg^pickupDefs', _):- 'lo.comp.code.pkg@pickupDefs'(XV33481, XV33482, XV33483).
'lo.comp.code.pkg@fun124'('_call%2'('()2'(XNm, XCls), XXd42317), 'lo.comp.code.pkg@fun124', _):- !,
    'lo.comp.code.pkg@compRel'(XNm, XCls, XXd42316),
    'lo.comp.code.asm@asm'(XXd42316, XXd42317).
'lo.comp.code.pkg@fun124'(_, _, _):- raise_exception('error'("lo.comp.code.pkg@fun124", 20, 48, 34)).
'lo.comp.code.pkg^compileMdl'('_call%2'(XV33484, XV33485), 'lo.comp.code.pkg^compileMdl', _):- 'lo.comp.code.pkg@compileMdl'(XV33484, XV33485).
