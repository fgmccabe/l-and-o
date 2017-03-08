'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.dependencies's'0.0.1'n9o9'()9'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.topsort'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.ast'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.abstract'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.keywords'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.misc'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'s\"I2'defn'CT5St'lo.comp.abstract*defnKind't'lo.comp.location*location'LT2St'lo.comp.abstract*defnKind'Lt'lo.comp.ast*ast't'lo.comp.dependencies*defn''dependencies'PT8Lt'lo.comp.ast*ast'LLt'lo.comp.dependencies*defn'LT2St'lo.comp.abstract*defnKind'LT2St'lo.comp.ast*ast'Lt'lo.comp.ast*ast'Lt'lo.comp.ast*ast't'lo.comp.errors*report't'lo.comp.errors*report'\"s\"I1'defn'Yt'lo.comp.dependencies*defn'I0\"n1o1'()1's'defn'n0o0'()0'n0o0'()0'").
'lo.comp.dependencies@init'():- !.
'lo.comp.dependencies#defn'('defn%1'('lo.comp.dependencies@defn'())):- !.
'lo.topsort$depends$lo.comp.dependencies*defn'('lo.topsort$depends$lo.comp.dependencies*defn%1'('lo.topsort$depends$lo.comp.dependencies*defn')):- !.
'lo.topsort$depends$lo.comp.dependencies*defn'('defines%2'(XV32092, XV32093), XLbl2223, XThis2223):- !,
    'lo.topsort$depends$lo.comp.dependencies*defn@defines'(XV32092, XV32093, XLbl2223, XThis2223).
'lo.topsort$depends$lo.comp.dependencies*defn'('defines%1'('lo.topsort$depends$lo.comp.dependencies*defn^defines'(XLbl2224, XThis2224)), XLbl2224, XThis2224).
'lo.topsort$depends$lo.comp.dependencies*defn'('references%2'(XV32096, XV32097), XLbl2225, XThis2225):- !,
    'lo.topsort$depends$lo.comp.dependencies*defn@references'(XV32096, XV32097, XLbl2225, XThis2225).
'lo.topsort$depends$lo.comp.dependencies*defn'('references%1'('lo.topsort$depends$lo.comp.dependencies*defn^references'(XLbl2226, XThis2226)), XLbl2226, XThis2226).
'lo.topsort$depends$lo.comp.dependencies*defn@defines'('lo.comp.dependencies#defn'(XX, XK, X_35221, X_35222, X_35223), '()2'(XX, XK), XLbV2557, XThV2557).
'lo.topsort$depends$lo.comp.dependencies*defn@references'('lo.comp.dependencies#defn'(X_35224, X_35225, X_35226, XR, X_35227), XR, XLbV2557, XThV2557):- !.
'lo.topsort$depends$lo.comp.dependencies*defn@references'(_, _):- raise_exception('error'("lo.topsort$depends$lo.comp.dependencies*defn@references", 18, 5, 32)).
'lo.comp.dependencies@showRefs'('lo.core#[]', X_35228, 'lo.core#[]'):- !.
'lo.comp.dependencies@showRefs'('lo.core#,..'('()2'(XNm, XK), XR), XSep, 'lo.core#,..'('lo.core#ss'(XSep), 'lo.core#,..'('lo.core#ss'(XNm), 'lo.core#,..'('lo.core#ss'(":"), 'lo.core#,..'(XXe5087, XXd40103))))):- !,
    ocall('disp%1'(XXV5459),'lo.core$display$lo.comp.abstract*defnKind','lo.core$display$lo.comp.abstract*defnKind'),
    ocall('_call%2'(XK, XXe5087),XXV5459,XXV5459),
    'lo.comp.dependencies@showRefs'(XR, ",", XXd40103).
'lo.comp.dependencies@showRefs'(_, _, _):- raise_exception('error'("lo.comp.dependencies@showRefs", 30, 3, 20)).
'lo.comp.dependencies@dispDefn'('lo.comp.dependencies#defn'(XNm, XK, XLc, XRefs, XStmts), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(XNm), 'lo.core#,..'('lo.core#ss'(":"), 'lo.core#,..'(XXe5088, XXd40110))))):- !,
    ocall('disp%1'(XXV5460),'lo.core$display$lo.comp.abstract*defnKind','lo.core$display$lo.comp.abstract*defnKind'),
    ocall('_call%2'(XK, XXe5088),XXV5460,XXV5460),
    'lo.comp.dependencies@showRefs'(XRefs, "->", XXd40110).
'lo.comp.dependencies@dispDefn'(_, _):- raise_exception('error'("lo.comp.dependencies@dispDefn", 27, 3, 91)).
'lo.core$display$lo.comp.dependencies*defn'('lo.core$display$lo.comp.dependencies*defn%1'('lo.core$display$lo.comp.dependencies*defn')):- !.
'lo.core$display$lo.comp.dependencies*defn'('disp%2'(XV32105, XV32106), XLbl2227, XThis2227):- !,
    'lo.core$display$lo.comp.dependencies*defn@disp'(XV32105, XV32106, XLbl2227, XThis2227).
'lo.core$display$lo.comp.dependencies*defn'('disp%1'('lo.core$display$lo.comp.dependencies*defn^disp'(XLbl2228, XThis2228)), XLbl2228, XThis2228).
'lo.core$display$lo.comp.dependencies*defn@disp'(XD, XXd40115, XLbV2558, XThV2558):- !,
    'lo.comp.dependencies@dispDefn'(XD, XXd40115).
'lo.core$display$lo.comp.dependencies*defn@disp'(_, _):- raise_exception('error'("lo.core$display$lo.comp.dependencies*defn@disp", 22, 5, 22)).
'lo.core$display$lo.comp.abstract*defnKind'('lo.core$display$lo.comp.abstract*defnKind%1'('lo.core$display$lo.comp.abstract*defnKind')):- !.
'lo.core$display$lo.comp.abstract*defnKind'('disp%2'(XV32109, XV32110), XLbl2229, XThis2229):- !,
    'lo.core$display$lo.comp.abstract*defnKind@disp'(XV32109, XV32110, XLbl2229, XThis2229).
'lo.core$display$lo.comp.abstract*defnKind'('disp%1'('lo.core$display$lo.comp.abstract*defnKind^disp'(XLbl2230, XThis2230)), XLbl2230, XThis2230).
'lo.core$display$lo.comp.abstract*defnKind@disp'('lo.comp.abstract#tpe', 'lo.core#ss'("type"), XLbV2559, XThV2559):- !.
'lo.core$display$lo.comp.abstract*defnKind@disp'('lo.comp.abstract#con', 'lo.core#ss'("contract"), XLbV2559, XThV2559):- !.
'lo.core$display$lo.comp.abstract*defnKind@disp'('lo.comp.abstract#impl', 'lo.core#ss'("implementation"), XLbV2559, XThV2559):- !.
'lo.core$display$lo.comp.abstract*defnKind@disp'('lo.comp.abstract#valu', 'lo.core#ss'("var"), XLbV2559, XThV2559):- !.
'lo.core$display$lo.comp.abstract*defnKind@disp'('lo.comp.abstract#clss', 'lo.core#ss'("class"), XLbV2559, XThV2559):- !.
'lo.core$display$lo.comp.abstract*defnKind@disp'(_, _):- raise_exception('error'("lo.core$display$lo.comp.abstract*defnKind@disp", 34, 5, 23)).
'lo.comp.dependencies@ruleHead'(XS, XL):- 'lo.comp.abstract@isBinary'(XS, "=", X_35237, XL, X_35238).
'lo.comp.dependencies@ruleHead'(XS, XL):- 'lo.comp.abstract@isBinary'(XS, "=>", X_35239, XL, X_35240).
'lo.comp.dependencies@ruleHead'(XS, XL):- 'lo.comp.abstract@isBinary'(XS, "-->", X_35241, XLL, X_35242),
    'lo.comp.abstract@isBinary'(XLL, ",", X_35243, XL, X_35244).
'lo.comp.dependencies@ruleHead'(XS, XL):- 'lo.comp.abstract@isBinary'(XS, "-->", X_35245, XL, X_35246).
'lo.comp.dependencies@ruleHead'(XS, XL):- 'lo.comp.abstract@isBinary'(XS, ":-", X_35247, XLL, X_35248),
    'lo.comp.dependencies@ruleHead'(XLL, XL).
'lo.comp.dependencies@ruleHead'(XS, XL):- 'lo.comp.abstract@isBinary'(XS, "<=", X_35249, XL, X_35250).
'lo.comp.dependencies@ruleHead'(XS, XS):- 'lo.comp.abstract@isRoundTerm'(XS, X_35251, X_35252, X_35253).
'lo.comp.dependencies@headName'(XH, XXd40121):- 'lo.comp.abstract@isBinary'(XH, "@@", X_35254, XL, X_35255),
    !,
    'lo.comp.dependencies@headName'(XL, XXd40121).
'lo.comp.dependencies@headName'(XH, XNm):- 'lo.comp.abstract@isRoundTerm'(XH, X_35256, XO, X_35257),
    'lo.comp.abstract@isIden'(XO, X_35258, XNm),
    !.
'lo.comp.dependencies@headName'(XH, XNm):- 'lo.comp.abstract@isIden'(XH, X_35259, XNm),
    !.
'lo.comp.dependencies@headName'(XH, XXd40123):- 'lo.comp.abstract@isRoundTuple'(XH, X_35260, 'lo.core#,..'(XA, 'lo.core#[]')),
    !,
    'lo.comp.dependencies@headName'(XA, XXd40123).
'lo.comp.dependencies@headName'(XH, XXd40129):- 'lo.comp.abstract@isRoundTuple'(XH, X_35262, XA),
    !,
    ocall('disp%1'(XXV5461),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    'lo.list@length'(XA, XXd40125),
    ocall('_call%2'(XXd40125, XXe5089),XXV5461,XXV5461),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("()"), 'lo.core#,..'(XXe5089, 'lo.core#[]'))), XXd40129).
'lo.comp.dependencies@headName'(_, _):- raise_exception('error'("lo.comp.dependencies@headName", 114, 3, 54)).
'lo.comp.dependencies@typeRuleName'(XS, XNm):- 'lo.comp.abstract@isQuantified'(XS, X_35265, XI),
    'lo.comp.dependencies@typeRuleName'(XI, XNm).
'lo.comp.dependencies@typeRuleName'(XS, XNm):- 'lo.comp.abstract@isConstrained'(XS, X_35266, XI),
    'lo.comp.dependencies@typeRuleName'(XI, XNm).
'lo.comp.dependencies@typeRuleName'(XS, XNm):- 'lo.comp.abstract@isBinary'(XS, "<~", X_35267, XL, X_35268),
    'lo.comp.dependencies@cond417'(X_35272, XNm, X_35271, X_35270, XOp, X_35269, XL).
'lo.comp.dependencies@surfaceName'(XN, 'lo.core#ss'(XNm)):- 'lo.comp.abstract@isIden'(XN, X_35273, XNm),
    !.
'lo.comp.dependencies@surfaceName'(XS, 'lo.core#ss'(XNm)):- 'lo.comp.abstract@isSquareTerm'(XS, X_35274, XOp, X_35275),
    'lo.comp.abstract@isIden'(XOp, X_35276, XNm),
    !.
'lo.comp.dependencies@surfaceName'(XT, 'lo.core#ss'(XXd40137)):- 'lo.comp.abstract@isRoundTuple'(XT, X_35277, XEls),
    !,
    ocall('disp%1'(XXV5462),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    'lo.list@length'(XEls, XXd40133),
    ocall('_call%2'(XXd40133, XXe5090),XXV5462,XXV5462),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("()"), 'lo.core#,..'(XXe5090, 'lo.core#[]'))), XXd40137).
'lo.comp.dependencies@surfaceName'(_, _):- raise_exception('error'("lo.comp.dependencies@surfaceName", 164, 3, 44)).
'lo.comp.dependencies@surfaceNames'('lo.core#,..'(XT, X_35281), XSep, XXd40140):- 'lo.comp.abstract@isBinary'(XT, "->>", X_35282, XL, X_35283),
    !,
    'lo.comp.abstract@deComma'(XL, XXd40139),
    'lo.comp.dependencies@surfaceNames'(XXd40139, XSep, XXd40140).
'lo.comp.dependencies@surfaceNames'('lo.core#[]', X_35284, 'lo.core#[]'):- !.
'lo.comp.dependencies@surfaceNames'('lo.core#,..'(XT, XEls), XSep, 'lo.core#,..'('lo.core#ss'(XSep), 'lo.core#,..'(XXd40142, XXd40143))):- !,
    'lo.comp.dependencies@surfaceName'(XT, XXd40142),
    'lo.comp.dependencies@surfaceNames'(XEls, XSep, XXd40143).
'lo.comp.dependencies@surfaceNames'(_, _, _):- raise_exception('error'("lo.comp.dependencies@surfaceNames", 158, 3, 88)).
'lo.comp.dependencies@astImplName'(XS, XNm):- 'lo.comp.abstract@isQuantified'(XS, X_35288, XI),
    'lo.comp.dependencies@astImplName'(XI, XNm).
'lo.comp.dependencies@astImplName'(XS, XNm):- 'lo.comp.abstract@isConstrained'(XS, X_35289, XI),
    'lo.comp.dependencies@astImplName'(XI, XNm).
'lo.comp.dependencies@astImplName'(XS, XXb20285):- 'lo.comp.abstract@isSquareTerm'(XS, X_35291, XOp, XA),
    'lo.comp.abstract@isIden'(XOp, X_35292, XNm),
    'lo.comp.abstract@marker'('lo.comp.abstract#con', XXb20281),
    'lo.comp.dependencies@surfaceNames'(XA, XXb20281, XXb20282),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(XNm), XXb20282)), XXb20285).
'lo.comp.dependencies@astImplName'(XT, XXb20290):- 'lo.comp.abstract@isRoundTuple'(XT, X_35295, XA),
    ocall('disp%1'(XXV5463),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    'lo.list@length'(XA, XXd40146),
    ocall('_call%2'(XXd40146, XXe5091),XXV5463,XXV5463),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("()"), 'lo.core#,..'(XXe5091, 'lo.core#[]'))), XXb20290).
'lo.comp.dependencies@contractName'(XS, XNm):- 'lo.comp.abstract@isQuantified'(XS, X_35296, XI),
    'lo.comp.dependencies@contractName'(XI, XNm).
'lo.comp.dependencies@contractName'(XS, XNm):- 'lo.comp.abstract@isConstrained'(XS, X_35297, XR),
    'lo.comp.dependencies@contractName'(XR, XNm).
'lo.comp.dependencies@contractName'(XS, XNm):- 'lo.comp.abstract@isBinary'(XS, "<~", X_35298, XL, X_35299),
    'lo.comp.dependencies@contractName'(XL, XNm).
'lo.comp.dependencies@contractName'(XS, XNm):- 'lo.comp.abstract@isSquareTerm'(XS, X_35300, XOp, X_35301),
    'lo.comp.abstract@isIden'(XOp, X_35302, XNm).
'lo.comp.dependencies@ruleNameKind'(XS, 'lo.comp.abstract#con', XNm):- 'lo.comp.abstract@isUnary'(XS, "contract", X_35303, XI),
    'lo.comp.dependencies@contractName'(XI, XNm).
'lo.comp.dependencies@ruleNameKind'(XS, 'lo.comp.abstract#impl', XNm):- 'lo.comp.abstract@isUnary'(XS, "implementation", X_35304, XI),
    'lo.comp.abstract@isBinary'(XI, "<=", X_35305, XL, X_35306),
    'lo.comp.dependencies@astImplName'(XL, XNm).
'lo.comp.dependencies@ruleNameKind'(XS, 'lo.comp.abstract#tpe', XNm):- 'lo.comp.abstract@isUnary'(XS, "type", X_35307, XI),
    'lo.comp.dependencies@typeRuleName'(XI, XNm).
'lo.comp.dependencies@ruleNameKind'(XS, XK, XNm):- 'lo.comp.abstract@isUnary'(XS, "public", X_35308, XI),
    'lo.comp.dependencies@ruleNameKind'(XI, XK, XNm).
'lo.comp.dependencies@ruleNameKind'(XS, XK, XNm):- 'lo.comp.abstract@isUnary'(XS, "private", X_35309, XI),
    'lo.comp.dependencies@ruleNameKind'(XI, XK, XNm).
'lo.comp.dependencies@ruleNameKind'(XS, 'lo.comp.abstract#valu', XNm):- 'lo.comp.dependencies@one299'(XH, XS),
    'lo.comp.dependencies@headName'(XH, XXd40147),
    XNm = XXd40147.
'lo.comp.dependencies@collectDefines'('lo.core#[]', X_35310, X_35311, 'lo.core#[]', 'lo.core#[]').
'lo.comp.dependencies@collectDefines'('lo.core#,..'(XSt, XStmts), XNm, XK, XStx, 'lo.core#,..'(XSt, XDef)):- 'lo.comp.dependencies@ruleNameKind'(XSt, XK, XNm),
    'lo.comp.dependencies@collectDefines'(XStmts, XNm, XK, XStx, XDef).
'lo.comp.dependencies@collectDefines'(XStmts, X_35314, X_35315, XStmts, 'lo.core#[]').
'lo.comp.dependencies@isOther'(XSt):- 'lo.comp.abstract@isUnary'(XSt, "assert", X_35316, X_35317).
'lo.comp.dependencies@isOther'(XSt):- 'lo.comp.abstract@isUnary'(XSt, "show", X_35318, X_35319).
'lo.comp.dependencies@isImport'(XSt, XXb20293):- 'lo.comp.abstract@isUnary'(XSt, "import", X_35320, XP),
    'lo.comp.abstract@packageName'(XP, XXb20293).
'lo.comp.dependencies@checkPublic'('lo.comp.package#pUblic', XNm, XK, 'lo.core#,..'('()2'(XNm, XK), XE), XE).
'lo.comp.dependencies@checkPublic'('lo.comp.package#priVate', X_35322, X_35323, XE, XE).
'lo.comp.dependencies@collectDefn'(XSt, XOrigSt, XStmts, XStmts, XDefs, XDefs, XP, XPx, XA, XA, 'lo.core#,..'(XOrigSt, XI), XI, XOth, XOth, XViz, XRp, XRp):- 'lo.comp.dependencies@isImport'(XSt, XNm),
    'lo.comp.dependencies@checkPublic'(XViz, XNm, 'lo.comp.abstract#imp', XP, XPx).
'lo.comp.dependencies@collectDefn'(XSt, X_35325, XStmts, XStmts, XDefs, XDefs, XP, XP, XA, XA, XI, XI, 'lo.core#,..'(XSt, XOth), XOth, X_35327, XRp, XRp):- 'lo.comp.dependencies@isOther'(XSt).
'lo.comp.dependencies@collectDefn'(XSt, XOrigSt, XStmts, XStx, XDefs, XDx, XP, XP, XA, XAx, XI, XIx, XO, XOx, X_35328, XRp, XRpx):- 'lo.comp.abstract@isUnary'(XSt, "private", X_35329, XInner),
    'lo.comp.dependencies@collectDefn'(XInner, XOrigSt, XStmts, XStx, XDefs, XDx, XP, X_35330, XA, XAx, XI, XIx, XO, XOx, 'lo.comp.package#priVate', XRp, XRpx).
'lo.comp.dependencies@collectDefn'(XSt, XOrigSt, XStmts, XStx, XDefs, XDx, XP, XPx, XA, XAx, XI, XIx, XO, XOx, X_35331, XRp, XRpx):- 'lo.comp.abstract@isUnary'(XSt, "public", X_35332, XInner),
    'lo.comp.dependencies@collectDefn'(XInner, XOrigSt, XStmts, XStx, XDefs, XDx, XP, XPx, XA, XAx, XI, XIx, XO, XOx, 'lo.comp.package#pUblic', XRp, XRpx).
'lo.comp.dependencies@collectDefn'(XSt, XOrigSt, XStmts, XStmts, XDefs, XDefs, XP, XPx, 'lo.core#,..'('()2'(XNm, XSt), XA), XA, XI, XI, XOth, XOth, XViz, XRp, XRp):- 'lo.comp.abstract@isBinary'(XSt, ":", X_35334, XL, X_35335),
    'lo.comp.abstract@isIden'(XL, X_35336, XNm),
    'lo.comp.dependencies@checkPublic'(XViz, XNm, 'lo.comp.abstract#valu', XP, XPx).
'lo.comp.dependencies@collectDefn'(XSt, X_35337, XStmts, XStx, 'lo.core#,..'('lo.comp.dependencies#defn'(XNm, XK, XXV5464, 'lo.core#[]', 'lo.core#,..'(XSt, XDfSts)), XDefs), XDefs, XP, XPx, XA, XA, XI, XI, XOth, XOth, XViz, XRp, XRp):- ocall('loc%1'(XXV5464),XSt,XSt),
    'lo.comp.dependencies@ruleNameKind'(XSt, XK, XNm),
    'lo.comp.dependencies@collectDefines'(XStmts, XNm, XK, XStx, XDfSts),
    'lo.comp.dependencies@checkPublic'(XViz, XNm, XK, XP, XPx).
'lo.comp.dependencies@collectDefn'(XSt, X_35340, XStmts, XStmts, XDefs, XDefs, XP, XP, XA, XA, XI, XI, XO, XO, X_35341, XRp, XRpx):- ocall('disp%1'(XXV5465),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('_call%2'(XSt, XXe5092),XXV5465,XXV5465),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot fathom "), 'lo.core#,..'(XXe5092, 'lo.core#[]'))), XXd40152),
    ocall('loc%1'(XXV5466),XSt,XSt),
    'lo.comp.errors@reportError'(XXd40152, XXV5466, XRp, XRpx).
'lo.comp.dependencies@collectDefinitions'('lo.core#[]', 'lo.core#[]', 'lo.core#[]', 'lo.core#[]', 'lo.core#[]', 'lo.core#[]', XRp, XRp).
'lo.comp.dependencies@collectDefinitions'('lo.core#,..'(XSt, XStmts), XDefs, XPu, XAn, XIm, XOt, XRp, XRpx):- 'lo.comp.dependencies@collectDefn'(XSt, XSt, XStmts, XS0, XDefs, XD0, XPu, XP0, XAn, XAn0, XIm, XIm0, XOt, XOt0, 'lo.comp.package#priVate', XRp, XRp0),
    'lo.comp.dependencies@collectDefinitions'(XS0, XD0, XP0, XAn0, XIm0, XOt0, XRp0, XRpx).
'lo.comp.dependencies@allRefs'('lo.core#[]', 'lo.core#[]'):- !.
'lo.comp.dependencies@allRefs'('lo.core#,..'('lo.comp.dependencies#defn'(XNm, XK, X_35346, X_35347, X_35348), XL), 'lo.core#,..'('()2'(XNm, XK), XXd40153)):- !,
    'lo.comp.dependencies@allRefs'(XL, XXd40153).
'lo.comp.dependencies@allRefs'(_, _):- raise_exception('error'("lo.comp.dependencies@allRefs", 180, 3, 17)).
'lo.comp.dependencies@refsInTypes'('lo.core#[]', X_35350, XSo, XSo):- !.
'lo.comp.dependencies@refsInTypes'('lo.core#,..'(XT, XL), XAll, XSo, XXd40156):- !,
    'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XXd40155),
    'lo.comp.dependencies@refsInTypes'(XL, XAll, XXd40155, XXd40156).
'lo.comp.dependencies@refsInTypes'(_, _, _, _):- raise_exception('error'("lo.comp.dependencies@refsInTypes", 235, 3, 26)).
'lo.comp.dependencies@refsInConstraint'(XT, XAll, XSo, XCndV125):- 'lo.comp.abstract@isIden'(XT, X_35352, XNm),
    !,
    'lo.comp.dependencies@condExp125'(XCndV125, XXd40157, XSo, XAll, XNm).
'lo.comp.dependencies@refsInConstraint'(XT, XAll, XSo, XXd40159):- 'lo.comp.abstract@isSquareTerm'(XT, X_35354, XOp, XArgs),
    !,
    'lo.comp.dependencies@refsInConstraint'(XOp, XAll, XSo, XXd40158),
    'lo.comp.dependencies@refsInTypes'(XArgs, XAll, XXd40158, XXd40159).
'lo.comp.dependencies@refsInConstraint'(XT, XAll, XSo, XXd40161):- 'lo.comp.abstract@isBinary'(XT, ",", X_35355, XL, XR),
    !,
    'lo.comp.dependencies@refsInConstraint'(XR, XAll, XSo, XXd40160),
    'lo.comp.dependencies@refsInConstraint'(XL, XAll, XXd40160, XXd40161).
'lo.comp.dependencies@refsInConstraint'(XT, XAll, XSo, XXd40163):- 'lo.comp.abstract@isBinary'(XT, "|:", X_35356, XL, XR),
    !,
    'lo.comp.dependencies@refsInConstraint'(XR, XAll, XSo, XXd40162),
    'lo.comp.dependencies@refsInConstraint'(XL, XAll, XXd40162, XXd40163).
'lo.comp.dependencies@refsInConstraint'(XT, XAll, XSo, XXd40165):- 'lo.comp.abstract@isBinary'(XT, "<~", X_35357, XL, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XXd40164),
    'lo.comp.dependencies@refsInType'(XL, XAll, XXd40164, XXd40165).
'lo.comp.dependencies@refsInConstraint'(XT, XAll, XSo, XXd40166):- 'lo.comp.abstract@isQuantified'(XT, X_35358, XI),
    !,
    'lo.comp.dependencies@refsInConstraint'(XI, XAll, XSo, XXd40166).
'lo.comp.dependencies@refsInConstraint'(_, _, _, _):- raise_exception('error'("lo.comp.dependencies@refsInConstraint", 247, 3, 111)).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XCndV126):- 'lo.comp.abstract@isIden'(XT, X_35359, XNm),
    !,
    'lo.comp.dependencies@condExp126'(XCndV126, XXd40167, XSo, XAll, XNm).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XXd40169):- 'lo.comp.abstract@isSquareTerm'(XT, X_35361, XOp, XArgs),
    !,
    'lo.comp.dependencies@refsInType'(XOp, XAll, XSo, XXd40168),
    'lo.comp.dependencies@refsInTypes'(XArgs, XAll, XXd40168, XXd40169).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XXd40170):- 'lo.comp.abstract@isBraceTerm'(XT, X_35362, XA, 'lo.core#[]'),
    'lo.comp.abstract@isRoundTuple'(XA, X_35363, XArgs),
    !,
    'lo.comp.dependencies@refsInTypes'(XArgs, XAll, XSo, XXd40170).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XXd40172):- 'lo.comp.abstract@isBinary'(XT, ",", X_35364, XL, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XXd40171),
    'lo.comp.dependencies@refsInType'(XL, XAll, XXd40171, XXd40172).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XXd40174):- 'lo.comp.abstract@isBinary'(XT, "=>", X_35365, XL, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XXd40173),
    'lo.comp.dependencies@refsInType'(XL, XAll, XXd40173, XXd40174).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XXd40176):- 'lo.comp.abstract@isBinary'(XT, "-->", X_35366, XL, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XXd40175),
    'lo.comp.dependencies@refsInType'(XL, XAll, XXd40175, XXd40176).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XXd40178):- 'lo.comp.abstract@isBinary'(XT, "<=>", X_35367, XL, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XXd40177),
    'lo.comp.dependencies@refsInType'(XL, XAll, XXd40177, XXd40178).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XXd40180):- 'lo.comp.abstract@isBinary'(XT, "->>", X_35368, XL, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XXd40179),
    'lo.comp.dependencies@refsInType'(XL, XAll, XXd40179, XXd40180).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XXd40181):- 'lo.comp.abstract@isRoundTuple'(XT, X_35369, XL),
    !,
    'lo.comp.dependencies@refsInTypes'(XL, XAll, XSo, XXd40181).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XXd40183):- 'lo.comp.abstract@isBinary'(XT, "|:", X_35370, XL, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XXd40182),
    'lo.comp.dependencies@refsInConstraint'(XL, XAll, XXd40182, XXd40183).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XXd40184):- 'lo.comp.abstract@isBinary'(XT, "<~", X_35371, X_35372, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XXd40184).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XXd40185):- 'lo.comp.abstract@isQuantified'(XT, X_35373, XI),
    !,
    'lo.comp.dependencies@refsInType'(XI, XAll, XSo, XXd40185).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XXd40186):- 'lo.comp.abstract@isBraceTuple'(XT, X_35374, XE),
    !,
    'lo.comp.dependencies@refsInScope'(XE, XAll, XSo, XXd40186).
'lo.comp.dependencies@refsInType'(XT, X_35375, XSo, XSo):- !.
'lo.comp.dependencies@refsInType'(_, _, _, _):- raise_exception('error'("lo.comp.dependencies@refsInType", 219, 3, 105)).
'lo.comp.dependencies@refsInScope'('lo.core#[]', X_35376, XSo, XSo):- !.
'lo.comp.dependencies@refsInScope'('lo.core#,..'(XE, XL), XAll, XSo, XXd40188):- 'lo.comp.abstract@isBinary'(XE, ":", X_35378, X_35379, XT),
    !,
    'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XXd40187),
    'lo.comp.dependencies@refsInScope'(XL, XAll, XXd40187, XXd40188).
'lo.comp.dependencies@refsInScope'(_, _, _, _):- raise_exception('error'("lo.comp.dependencies@refsInScope", 239, 3, 26)).
'lo.comp.dependencies@refsInContract'(XC, XAll, XSo, XXd40189):- 'lo.comp.abstract@isQuantified'(XC, X_35380, XI),
    !,
    'lo.comp.dependencies@refsInContract'(XI, XAll, XSo, XXd40189).
'lo.comp.dependencies@refsInContract'(XC, XAll, XSo, XXd40190):- 'lo.comp.abstract@isBinary'(XC, "<~", X_35381, XL, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XXd40190).
'lo.comp.dependencies@refsInContract'(_, _, _, _):- raise_exception('error'("lo.comp.dependencies@refsInContract", 243, 3, 75)).
'lo.comp.dependencies@refsInHead'(XS, XAll, XSo, XXd40192):- 'lo.comp.abstract@isBinary'(XS, "@@", X_35382, XL, XR),
    !,
    'lo.comp.dependencies@refsInCond'(XR, XAll, XSo, XXd40191),
    'lo.comp.dependencies@refsInHead'(XL, XAll, XXd40191, XXd40192).
'lo.comp.dependencies@refsInHead'(XS, XAll, XSo, XXd40193):- 'lo.comp.abstract@isRoundTerm'(XS, X_35383, X_35384, XA),
    !,
    'lo.comp.dependencies@refsInTerms'(XA, XAll, XSo, XXd40193).
'lo.comp.dependencies@refsInHead'(XS, XAll, XSo, XSo):- 'lo.comp.abstract@isIden'(XS, X_35385, X_35386),
    !.
'lo.comp.dependencies@refsInHead'(XS, XAll, XSo, XXd40195):- 'lo.comp.abstract@isBinary'(XA, X_35387, X_35388, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XXd40194),
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XXd40194, XXd40195).
'lo.comp.dependencies@refsInHead'(XS, XAll, XSo, XXd40196):- 'lo.comp.abstract@isUnary'(XA, X_35389, X_35390, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XXd40196).
'lo.comp.dependencies@refsInHead'(_, _, _, _):- raise_exception('error'("lo.comp.dependencies@refsInHead", 212, 3, 89)).
'lo.comp.dependencies@refsInNTs'('lo.core#[]', X_35391, XSo, XSo):- !.
'lo.comp.dependencies@refsInNTs'('lo.core#,..'(XN, XL), XAll, XSo, XXd40198):- !,
    'lo.comp.dependencies@refsInNT'(XN, XAll, XSo, XXd40197),
    'lo.comp.dependencies@refsInNTs'(XL, XAll, XXd40197, XXd40198).
'lo.comp.dependencies@refsInNTs'(_, _, _, _):- raise_exception('error'("lo.comp.dependencies@refsInNTs", 294, 3, 24)).
'lo.comp.dependencies@refsInIndex'('lo.core#,..'(XA, 'lo.core#[]'), XAll, XSo, XXd40200):- 'lo.comp.abstract@isBinary'(XA, "->", X_35394, XKy, XVl),
    !,
    'lo.comp.dependencies@refsInTerm'(XKy, XAll, XSo, XXd40199),
    'lo.comp.dependencies@refsInTerm'(XVl, XAll, XXd40199, XXd40200).
'lo.comp.dependencies@refsInIndex'('lo.core#,..'(XA, 'lo.core#[]'), XAll, XSo, XXd40201):- 'lo.comp.abstract@isUnary'(XA, "\\+", X_35396, XKy),
    !,
    'lo.comp.dependencies@refsInTerm'(XKy, XAll, XSo, XXd40201).
'lo.comp.dependencies@refsInIndex'(XA, XAll, XSo, XXd40202):- !,
    'lo.comp.dependencies@refsInTerms'(XA, XAll, XSo, XXd40202).
'lo.comp.dependencies@refsInIndex'(_, _, _, _):- raise_exception('error'("lo.comp.dependencies@refsInIndex", 323, 3, 95)).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XCndV127):- 'lo.comp.abstract@isIden'(XT, X_35397, XNm),
    !,
    'lo.comp.dependencies@condExp127'(XCndV127, XXd40203, XSo, XAll, XNm).
'lo.comp.dependencies@refsInTerm'(XT, X_35399, XSo, XSo):- 'lo.comp.abstract@isScalar'(XT),
    !.
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XXd40205):- 'lo.comp.abstract@isBinary'(XT, ":", X_35400, XL, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XXd40204),
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XXd40204, XXd40205).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XXd40207):- 'lo.comp.abstract@isBinary'(XT, "::", X_35401, XL, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XXd40206),
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XXd40206, XXd40207).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XXd40209):- 'lo.comp.abstract@isBinary'(XT, "@@", X_35402, XL, XR),
    !,
    'lo.comp.dependencies@refsInCond'(XR, XAll, XSo, XXd40208),
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XXd40208, XXd40209).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XXd40210):- 'lo.comp.abstract@isUnary'(XT, "@", X_35403, XL),
    !,
    'lo.comp.dependencies@refsInCond'(XL, XAll, XSo, XXd40210).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XXd40211):- 'lo.comp.abstract@isBinary'(XT, ".", X_35404, XL, X_35405),
    !,
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XSo, XXd40211).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XXd40214):- 'lo.comp.abstract@isBinary'(XT, "|", X_35406, XL, XR),
    'lo.comp.abstract@isBinary'(XL, "?", X_35407, XLL, XLR),
    !,
    'lo.comp.dependencies@refsInCond'(XLL, XAll, XSo, XXd40212),
    'lo.comp.dependencies@refsInTerm'(XLR, XAll, XXd40212, XXd40213),
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XXd40213, XXd40214).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XXd40216):- 'lo.comp.abstract@isSquareTerm'(XT, X_35408, XOp, XArgs),
    !,
    'lo.comp.dependencies@refsInTerm'(XOp, XAll, XSo, XXd40215),
    'lo.comp.dependencies@refsInIndex'(XArgs, XAll, XXd40215, XXd40216).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XXd40217):- 'lo.comp.abstract@isRoundTuple'(XT, X_35409, XArgs),
    !,
    'lo.comp.dependencies@refsInTerms'(XArgs, XAll, XSo, XXd40217).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XXd40222):- 'lo.comp.abstract@isSquareTuple'(XT, XLc, XArgs),
    !,
    'lo.comp.dependencies@refsInTerm'('lo.comp.ast#iden'(XLc, "[]"), XAll, XSo, XXd40220),
    'lo.comp.dependencies@refsInTerm'('lo.comp.ast#iden'(XLc, ",.."), XAll, XXd40220, XXd40221),
    'lo.comp.dependencies@refsInTerms'(XArgs, XAll, XXd40221, XXd40222).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XXd40224):- 'lo.comp.abstract@isBinary'(XT, ",..", X_35410, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XSo, XXd40223),
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XXd40223, XXd40224).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XXd40226):- 'lo.comp.abstract@isBinary'(XT, "->", X_35411, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XSo, XXd40225),
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XXd40225, XXd40226).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XXd40227):- 'lo.comp.abstract@isBraceTuple'(XT, X_35412, XEls),
    !,
    'lo.comp.dependencies@thetaRefs'(XEls, XAll, XSo, XXd40227).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XXd40229):- 'lo.comp.abstract@isBinary'(XT, "=>", X_35413, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XSo, XXd40228),
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XXd40228, XXd40229).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XXd40231):- 'lo.comp.abstract@isBinary'(XT, ":-", X_35414, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XSo, XXd40230),
    'lo.comp.dependencies@refsInCond'(XR, XAll, XXd40230, XXd40231).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XXd40233):- 'lo.comp.abstract@isBinary'(XT, "-->", X_35415, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XSo, XXd40232),
    'lo.comp.dependencies@refsInNT'(XR, XAll, XXd40232, XXd40233).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XXd40235):- 'lo.comp.abstract@isRoundTerm'(XT, X_35416, XOp, XArgs),
    !,
    'lo.comp.dependencies@refsInTerm'(XOp, XAll, XSo, XXd40234),
    'lo.comp.dependencies@refsInTerms'(XArgs, XAll, XXd40234, XXd40235).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XSo):- !.
'lo.comp.dependencies@refsInTerm'(_, _, _, _):- raise_exception('error'("lo.comp.dependencies@refsInTerm", 298, 3, 108)).
'lo.comp.dependencies@refsInTerms'('lo.core#[]', X_35417, XSo, XSo):- !.
'lo.comp.dependencies@refsInTerms'('lo.core#,..'(XT, XL), XAll, XSo, XXd40237):- !,
    'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XXd40236),
    'lo.comp.dependencies@refsInTerms'(XL, XAll, XXd40236, XXd40237).
'lo.comp.dependencies@refsInTerms'(_, _, _, _):- raise_exception('error'("lo.comp.dependencies@refsInTerms", 319, 3, 26)).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XCndV128):- 'lo.comp.abstract@isIden'(XT, X_35419, XNm),
    !,
    'lo.comp.dependencies@condExp128'(XCndV128, XXd40238, XSo, XAll, XNm).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XXd40240):- 'lo.comp.abstract@isBinary'(XT, ",", X_35421, XL, XR),
    !,
    'lo.comp.dependencies@refsInCond'(XR, XAll, XSo, XXd40239),
    'lo.comp.dependencies@refsInCond'(XL, XAll, XXd40239, XXd40240).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XXd40243):- 'lo.comp.abstract@isBinary'(XT, "|", X_35422, XL, XR),
    'lo.comp.abstract@isBinary'(XL, "?", X_35423, XLL, XLR),
    !,
    'lo.comp.dependencies@refsInCond'(XLL, XAll, XSo, XXd40241),
    'lo.comp.dependencies@refsInCond'(XLR, XAll, XXd40241, XXd40242),
    'lo.comp.dependencies@refsInCond'(XR, XAll, XXd40242, XXd40243).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XXd40245):- 'lo.comp.abstract@isBinary'(XT, "|", X_35424, XL, XR),
    !,
    'lo.comp.dependencies@refsInCond'(XL, XAll, XSo, XXd40244),
    'lo.comp.dependencies@refsInCond'(XR, XAll, XXd40244, XXd40245).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XXd40247):- 'lo.comp.abstract@isBinary'(XT, "*>", X_35425, XL, XR),
    !,
    'lo.comp.dependencies@refsInCond'(XR, XAll, XSo, XXd40246),
    'lo.comp.dependencies@refsInCond'(XL, XAll, XXd40246, XXd40247).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XXd40248):- 'lo.comp.abstract@isUnary'(XT, "!", X_35426, XL),
    !,
    'lo.comp.dependencies@refsInCond'(XL, XAll, XSo, XXd40248).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XXd40249):- 'lo.comp.abstract@isUnary'(XT, "\\+", X_35427, XL),
    !,
    'lo.comp.dependencies@refsInCond'(XL, XAll, XSo, XXd40249).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XXd40251):- 'lo.comp.abstract@isBinary'(XT, "=", X_35428, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XXd40250),
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XXd40250, XXd40251).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XXd40253):- 'lo.comp.abstract@isBinary'(XT, "\\=", X_35429, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XXd40252),
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XXd40252, XXd40253).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XXd40256):- 'lo.comp.abstract@isBinary'(XT, "%%", X_35430, XL, XR),
    'lo.comp.abstract@isBinary'(XR, "~", X_35431, XLL, XLR),
    !,
    'lo.comp.dependencies@refsInTerm'(XLL, XAll, XSo, XXd40254),
    'lo.comp.dependencies@refsInTerm'(XLR, XAll, XXd40254, XXd40255),
    'lo.comp.dependencies@refsInNT'(XL, XAll, XXd40255, XXd40256).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XXd40258):- 'lo.comp.abstract@isBinary'(XT, "%%", X_35432, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XXd40257),
    'lo.comp.dependencies@refsInNT'(XL, XAll, XXd40257, XXd40258).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XXd40260):- 'lo.comp.abstract@isRoundTerm'(XT, X_35433, XOp, XArgs),
    !,
    'lo.comp.dependencies@refsInTerm'(XOp, XAll, XSo, XXd40259),
    'lo.comp.dependencies@refsInTerms'(XArgs, XAll, XXd40259, XXd40260).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XXd40261):- 'lo.comp.abstract@isRoundTuple'(XT, X_35434, XArgs),
    !,
    'lo.comp.dependencies@refsInConds'(XArgs, XAll, XSo, XXd40261).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XXd40262):- !,
    'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XXd40262).
'lo.comp.dependencies@refsInCond'(_, _, _, _):- raise_exception('error'("lo.comp.dependencies@refsInCond", 255, 3, 108)).
'lo.comp.dependencies@refsInConds'('lo.core#[]', X_35435, XSo, XSo):- !.
'lo.comp.dependencies@refsInConds'('lo.core#,..'(XT, XL), XAll, XSo, XXd40264):- !,
    'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XXd40263),
    'lo.comp.dependencies@refsInConds'(XL, XAll, XXd40263, XXd40264).
'lo.comp.dependencies@refsInConds'(_, _, _, _):- raise_exception('error'("lo.comp.dependencies@refsInConds", 271, 3, 26)).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XXd40265):- 'lo.comp.abstract@isSquareTuple'(XT, X_35437, XArgs),
    !,
    'lo.comp.dependencies@refsInTerms'(XArgs, XAll, XSo, XXd40265).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XXd40268):- 'lo.comp.abstract@isBinary'(XT, "|", X_35438, XL, XR),
    'lo.comp.abstract@isBinary'(XL, "?", X_35439, XLL, XLR),
    !,
    'lo.comp.dependencies@refsInNT'(XLL, XAll, XSo, XXd40266),
    'lo.comp.dependencies@refsInNT'(XLR, XAll, XXd40266, XXd40267),
    'lo.comp.dependencies@refsInNT'(XR, XAll, XXd40267, XXd40268).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XXd40270):- 'lo.comp.abstract@isBinary'(XT, "|", X_35440, XL, XR),
    !,
    'lo.comp.dependencies@refsInNT'(XL, XAll, XSo, XXd40269),
    'lo.comp.dependencies@refsInNT'(XR, XAll, XXd40269, XXd40270).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XXd40272):- 'lo.comp.abstract@isBinary'(XT, ",", X_35441, XL, XR),
    !,
    'lo.comp.dependencies@refsInNT'(XR, XAll, XSo, XXd40271),
    'lo.comp.dependencies@refsInNT'(XL, XAll, XXd40271, XXd40272).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XXd40273):- 'lo.comp.abstract@isUnary'(XT, "!", X_35442, XL),
    !,
    'lo.comp.dependencies@refsInNT'(XL, XAll, XSo, XXd40273).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XXd40274):- 'lo.comp.abstract@isUnary'(XT, "\\+", X_35443, XL),
    !,
    'lo.comp.dependencies@refsInNT'(XL, XAll, XSo, XXd40274).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XXd40275):- 'lo.comp.abstract@isUnary'(XT, "+", X_35444, XL),
    !,
    'lo.comp.dependencies@refsInNT'(XL, XAll, XSo, XXd40275).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XXd40277):- 'lo.comp.abstract@isBinary'(XT, "@@", X_35445, XL, XR),
    !,
    'lo.comp.dependencies@refsInCond'(XR, XAll, XSo, XXd40276),
    'lo.comp.dependencies@refsInNT'(XL, XAll, XXd40276, XXd40277).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XXd40279):- 'lo.comp.abstract@isBinary'(XT, "=", X_35446, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XXd40278),
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XXd40278, XXd40279).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XXd40281):- 'lo.comp.abstract@isBinary'(XT, "\\=", X_35447, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XXd40280),
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XXd40280, XXd40281).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XXd40282):- 'lo.comp.abstract@isUnary'(XT, "@", X_35448, XL),
    !,
    'lo.comp.dependencies@refsInCond'(XL, XAll, XSo, XXd40282).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XXd40284):- 'lo.comp.abstract@isRoundTerm'(XT, X_35449, XOp, XArgs),
    !,
    'lo.comp.dependencies@refsInTerm'(XOp, XAll, XSo, XXd40283),
    'lo.comp.dependencies@refsInTerms'(XArgs, XAll, XXd40283, XXd40284).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XSo):- 'lo.comp.abstract@isString'(XT, X_35450, X_35451),
    !.
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XXd40285):- 'lo.comp.abstract@isRoundTuple'(XT, X_35452, XEls),
    !,
    'lo.comp.dependencies@refsInNTs'(XEls, XAll, XSo, XXd40285).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XXd40286):- 'lo.comp.abstract@isBraceTuple'(XT, X_35453, XEls),
    !,
    'lo.comp.dependencies@refsInConds'(XEls, XAll, XSo, XXd40286).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XSo):- 'lo.comp.abstract@isIden'(XT, X_35454, "eof"),
    !.
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XXd40287):- !,
    'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XXd40287).
'lo.comp.dependencies@refsInNT'(_, _, _, _):- raise_exception('error'("lo.comp.dependencies@refsInNT", 275, 3, 74)).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XXd40288):- 'lo.comp.abstract@isUnary'(XS, "private", X_35455, XI),
    !,
    'lo.comp.dependencies@stmtRefs'(XI, XAll, XSo, XXd40288).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XXd40289):- 'lo.comp.abstract@isUnary'(XS, "public", X_35456, XI),
    !,
    'lo.comp.dependencies@stmtRefs'(XI, XAll, XSo, XXd40289).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XXd40290):- 'lo.comp.abstract@isUnary'(XS, "type", X_35457, XI),
    !,
    'lo.comp.dependencies@refsInType'(XI, XAll, XSo, XXd40290).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XXd40291):- 'lo.comp.abstract@isUnary'(XS, "contract", X_35458, XI),
    !,
    'lo.comp.dependencies@refsInContract'(XI, XAll, XSo, XXd40291).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XXd40293):- 'lo.comp.abstract@isUnary'(XS, "implementation", X_35459, XI),
    'lo.comp.abstract@isBinary'(XI, "<=", X_35460, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XXd40292),
    'lo.comp.dependencies@refsInConstraint'(XL, XAll, XXd40292, XXd40293).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XXd40295):- 'lo.comp.abstract@isBinary'(XS, "=>", X_35461, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XXd40294),
    'lo.comp.dependencies@refsInHead'(XL, XAll, XXd40294, XXd40295).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XXd40298):- 'lo.comp.abstract@isBinary'(XS, ":-", X_35462, XL, XR),
    'lo.comp.abstract@isBinary'(XL, "=>", X_35463, XH, XE),
    !,
    'lo.comp.dependencies@refsInCond'(XR, XAll, XSo, XXd40296),
    'lo.comp.dependencies@refsInTerm'(XE, XAll, XXd40296, XXd40297),
    'lo.comp.dependencies@refsInHead'(XH, XAll, XXd40297, XXd40298).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XXd40300):- 'lo.comp.abstract@isBinary'(XS, ":-", X_35464, XL, XR),
    !,
    'lo.comp.dependencies@refsInCond'(XR, XAll, XSo, XXd40299),
    'lo.comp.dependencies@refsInHead'(XL, XAll, XXd40299, XXd40300).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XXd40301):- 'lo.comp.abstract@isRoundTerm'(XS, X_35465, X_35466, XA),
    !,
    'lo.comp.dependencies@refsInTerms'(XA, XAll, XSo, XXd40301).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XXd40303):- 'lo.comp.abstract@isBinary'(XS, "-->", X_35467, XL, XR),
    !,
    'lo.comp.dependencies@refsInNT'(XR, XAll, XSo, XXd40302),
    'lo.comp.dependencies@refsInHead'(XL, XAll, XXd40302, XXd40303).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XXd40305):- 'lo.comp.abstract@isBinary'(XS, "<=", X_35468, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XXd40304),
    'lo.comp.dependencies@refsInHead'(XL, XAll, XXd40304, XXd40305).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XXd40306):- 'lo.comp.abstract@isBinary'(XS, "=", X_35469, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XXd40306).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XXd40307):- 'lo.comp.abstract@isUnary'(XS, "show", X_35470, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XXd40307).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XXd40308):- 'lo.comp.abstract@isUnary'(XS, "assert", X_35471, XR),
    !,
    'lo.comp.dependencies@refsInCond'(XR, XAll, XSo, XXd40308).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XXd40309):- 'lo.comp.abstract@isBinary'(XS, ":", X_35472, X_35473, XI),
    !,
    'lo.comp.dependencies@refsInType'(XI, XAll, XSo, XXd40309).
'lo.comp.dependencies@stmtRefs'(_, _, _, _):- raise_exception('error'("lo.comp.dependencies@stmtRefs", 195, 3, 70)).
'lo.comp.dependencies@thetaRefs'('lo.core#[]', X_35474, XSo, XSo):- !.
'lo.comp.dependencies@thetaRefs'('lo.core#,..'(XSt, XStmts), XAll, XSo, XXd40311):- !,
    'lo.comp.dependencies@stmtRefs'(XSt, XAll, XSo, XXd40310),
    'lo.comp.dependencies@thetaRefs'(XStmts, XAll, XXd40310, XXd40311).
'lo.comp.dependencies@thetaRefs'(_, _, _, _):- raise_exception('error'("lo.comp.dependencies@thetaRefs", 191, 3, 22)).
'lo.comp.dependencies@collectThetaRefs'('lo.core#[]', X_35476, X_35477, 'lo.core#[]'):- !.
'lo.comp.dependencies@collectThetaRefs'('lo.core#,..'('lo.comp.dependencies#defn'(XN, XK, XLc, X_35479, XS), XL), XAll, XAnnots, 'lo.core#,..'('lo.comp.dependencies#defn'(XN, XK, XLc, XXd40314, XS), XXd40316)):- ocall('in%2'('()2'(XN, XA), XAnnots),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    'lo.comp.dependencies@refsInScope'('lo.core#,..'(XA, 'lo.core#[]'), XAll, 'lo.core#[]', XXd40313),
    'lo.comp.dependencies@thetaRefs'(XS, XAll, XXd40313, XXd40314),
    'lo.comp.dependencies@collectThetaRefs'(XL, XAll, XAnnots, XXd40316).
'lo.comp.dependencies@collectThetaRefs'('lo.core#,..'('lo.comp.dependencies#defn'(XN, XK, XLc, X_35483, XS), XL), XAll, XAnnots, 'lo.core#,..'('lo.comp.dependencies#defn'(XN, XK, XLc, XXd40318, XS), XXd40320)):- !,
    'lo.comp.dependencies@thetaRefs'(XS, XAll, 'lo.core#[]', XXd40318),
    'lo.comp.dependencies@collectThetaRefs'(XL, XAll, XAnnots, XXd40320).
'lo.comp.dependencies@collectThetaRefs'(_, _, _, _):- raise_exception('error'("lo.comp.dependencies@collectThetaRefs", 184, 3, 30)).
'lo.comp.dependencies@dependencies'(XEls, XGroups, XPublic, XAnnots, XImports, XOther, XRp, XRpx):- 'lo.comp.dependencies@collectDefinitions'(XEls, XDfs, XPublic, XAnnots, XImports, XOther, XRp, XRpx),
    'lo.comp.dependencies@allRefs'(XDfs, XXd40322),
    'lo.comp.dependencies@collectThetaRefs'(XDfs, XXd40322, XAnnots, XXd40323),
    'lo.topsort@topsort'('lo.topsort$depends$lo.comp.dependencies*defn', XXd40323, XXd40324),
    XGroups = XXd40324.
'lo.comp.dependencies@showGroups'('lo.core#[]').
'lo.comp.dependencies@showGroups'('lo.core#,..'(XG, XL)):- ocall('disp%1'(XXV5467),'lo.core$display$lo.core*list'('lo.core$display$lo.comp.dependencies*defn'),'lo.core$display$lo.core*list'('lo.core$display$lo.comp.dependencies*defn')),
    ocall('_call%2'(XG, XXe5093),XXV5467,XXV5467),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Group: "), 'lo.core#,..'(XXe5093, 'lo.core#[]'))), XXd40330),
    'lo.io@logMsg'(XXd40330),
    'lo.comp.dependencies@showGroups'(XL).
'lo.topsort$depends$lo.comp.dependencies*defn^defines'('_call%4'(XV32088, XV32089, XV32090, XV32091), 'lo.topsort$depends$lo.comp.dependencies*defn^defines'(XLbV2557, XThV2557), _):- 'lo.topsort$depends$lo.comp.dependencies*defn@defines'(XV32088, XV32089, XV32090, XV32091, XLbV2557, XThV2557).
'lo.topsort$depends$lo.comp.dependencies*defn^references'('_call%2'(XV32094, XV32095), 'lo.topsort$depends$lo.comp.dependencies*defn^references'(XLbV2557, XThV2557), _):- 'lo.topsort$depends$lo.comp.dependencies*defn@references'(XV32094, XV32095, XLbV2557, XThV2557).
'lo.comp.dependencies^showRefs'('_call%3'(XV32098, XV32099, XV32100), 'lo.comp.dependencies^showRefs', _):- 'lo.comp.dependencies@showRefs'(XV32098, XV32099, XV32100).
'lo.comp.dependencies^dispDefn'('_call%2'(XV32101, XV32102), 'lo.comp.dependencies^dispDefn', _):- 'lo.comp.dependencies@dispDefn'(XV32101, XV32102).
'lo.core$display$lo.comp.dependencies*defn^disp'('_call%2'(XV32103, XV32104), 'lo.core$display$lo.comp.dependencies*defn^disp'(XLbV2558, XThV2558), _):- 'lo.core$display$lo.comp.dependencies*defn@disp'(XV32103, XV32104, XLbV2558, XThV2558).
'lo.core$display$lo.comp.abstract*defnKind^disp'('_call%2'(XV32107, XV32108), 'lo.core$display$lo.comp.abstract*defnKind^disp'(XLbV2559, XThV2559), _):- 'lo.core$display$lo.comp.abstract*defnKind@disp'(XV32107, XV32108, XLbV2559, XThV2559).
'lo.comp.dependencies^ruleHead'('_call%2'(XV32111, XV32112), 'lo.comp.dependencies^ruleHead', _):- 'lo.comp.dependencies@ruleHead'(XV32111, XV32112).
'lo.comp.dependencies^headName'('_call%2'(XV32113, XV32114), 'lo.comp.dependencies^headName', _):- 'lo.comp.dependencies@headName'(XV32113, XV32114).
'lo.comp.dependencies@cond417'(X_35272, XNm, X_35271, X_35270, XOp, X_35269, XL):- 'lo.comp.abstract@isSquareTerm'(XL, X_35269, XOp, X_35270),
    !,
    'lo.comp.abstract@isIden'(XOp, X_35271, XNm).
'lo.comp.dependencies@cond417'(X_35272, XNm, X_35271, X_35270, XOp, X_35269, XL):- 'lo.comp.abstract@isIden'(XL, X_35272, XNm).
'lo.comp.dependencies^typeRuleName'('_call%2'(XV32115, XV32116), 'lo.comp.dependencies^typeRuleName', _):- 'lo.comp.dependencies@typeRuleName'(XV32115, XV32116).
'lo.comp.dependencies^surfaceName'('_call%2'(XV32117, XV32118), 'lo.comp.dependencies^surfaceName', _):- 'lo.comp.dependencies@surfaceName'(XV32117, XV32118).
'lo.comp.dependencies^surfaceNames'('_call%3'(XV32119, XV32120, XV32121), 'lo.comp.dependencies^surfaceNames', _):- 'lo.comp.dependencies@surfaceNames'(XV32119, XV32120, XV32121).
'lo.comp.dependencies^astImplName'('_call%2'(XV32122, XV32123), 'lo.comp.dependencies^astImplName', _):- 'lo.comp.dependencies@astImplName'(XV32122, XV32123).
'lo.comp.dependencies^contractName'('_call%2'(XV32124, XV32125), 'lo.comp.dependencies^contractName', _):- 'lo.comp.dependencies@contractName'(XV32124, XV32125).
'lo.comp.dependencies@one299'(XH, XS):- 'lo.comp.dependencies@ruleHead'(XS, XH),
    !.
'lo.comp.dependencies^ruleNameKind'('_call%3'(XV32126, XV32127, XV32128), 'lo.comp.dependencies^ruleNameKind', _):- 'lo.comp.dependencies@ruleNameKind'(XV32126, XV32127, XV32128).
'lo.comp.dependencies^collectDefines'('_call%5'(XV32129, XV32130, XV32131, XV32132, XV32133), 'lo.comp.dependencies^collectDefines', _):- 'lo.comp.dependencies@collectDefines'(XV32129, XV32130, XV32131, XV32132, XV32133).
'lo.comp.dependencies^isOther'('_call%1'(XV32134), 'lo.comp.dependencies^isOther', _):- 'lo.comp.dependencies@isOther'(XV32134).
'lo.comp.dependencies^isImport'('_call%2'(XV32135, XV32136), 'lo.comp.dependencies^isImport', _):- 'lo.comp.dependencies@isImport'(XV32135, XV32136).
'lo.comp.dependencies^checkPublic'('_call%5'(XV32137, XV32138, XV32139, XV32140, XV32141), 'lo.comp.dependencies^checkPublic', _):- 'lo.comp.dependencies@checkPublic'(XV32137, XV32138, XV32139, XV32140, XV32141).
'lo.comp.dependencies^collectDefn'('_call%17'(XV32142, XV32143, XV32144, XV32145, XV32146, XV32147, XV32148, XV32149, XV32150, XV32151, XV32152, XV32153, XV32154, XV32155, XV32156, XV32157, XV32158), 'lo.comp.dependencies^collectDefn', _):- 'lo.comp.dependencies@collectDefn'(XV32142, XV32143, XV32144, XV32145, XV32146, XV32147, XV32148, XV32149, XV32150, XV32151, XV32152, XV32153, XV32154, XV32155, XV32156, XV32157, XV32158).
'lo.comp.dependencies^collectDefinitions'('_call%8'(XV32159, XV32160, XV32161, XV32162, XV32163, XV32164, XV32165, XV32166), 'lo.comp.dependencies^collectDefinitions', _):- 'lo.comp.dependencies@collectDefinitions'(XV32159, XV32160, XV32161, XV32162, XV32163, XV32164, XV32165, XV32166).
'lo.comp.dependencies^allRefs'('_call%2'(XV32167, XV32168), 'lo.comp.dependencies^allRefs', _):- 'lo.comp.dependencies@allRefs'(XV32167, XV32168).
'lo.comp.dependencies^refsInTypes'('_call%4'(XV32169, XV32170, XV32171, XV32172), 'lo.comp.dependencies^refsInTypes', _):- 'lo.comp.dependencies@refsInTypes'(XV32169, XV32170, XV32171, XV32172).
'lo.comp.dependencies@neg333'(XSo, XNm):- ocall('in%2'('()2'(XNm, 'lo.comp.abstract#con'), XSo),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    fail.
'lo.comp.dependencies@neg333'(XSo, XNm).
'lo.comp.dependencies@condExp125'('lo.core#,..'('()2'(XNm, 'lo.comp.abstract#con'), XSo), XXd40157, XSo, XAll, XNm):- ocall('in%2'('()2'(XNm, 'lo.comp.abstract#con'), XAll),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.dependencies@neg333'(XSo, XNm),
    !.
'lo.comp.dependencies@condExp125'(XSo, XXd40157, XSo, XAll, XNm).
'lo.comp.dependencies^refsInConstraint'('_call%4'(XV32173, XV32174, XV32175, XV32176), 'lo.comp.dependencies^refsInConstraint', _):- 'lo.comp.dependencies@refsInConstraint'(XV32173, XV32174, XV32175, XV32176).
'lo.comp.dependencies@neg334'(XSo, XNm):- ocall('in%2'('()2'(XNm, 'lo.comp.abstract#tpe'), XSo),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    fail.
'lo.comp.dependencies@neg334'(XSo, XNm).
'lo.comp.dependencies@condExp126'('lo.core#,..'('()2'(XNm, 'lo.comp.abstract#tpe'), XSo), XXd40167, XSo, XAll, XNm):- ocall('in%2'('()2'(XNm, 'lo.comp.abstract#tpe'), XAll),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.dependencies@neg334'(XSo, XNm),
    !.
'lo.comp.dependencies@condExp126'(XSo, XXd40167, XSo, XAll, XNm).
'lo.comp.dependencies^refsInType'('_call%4'(XV32177, XV32178, XV32179, XV32180), 'lo.comp.dependencies^refsInType', _):- 'lo.comp.dependencies@refsInType'(XV32177, XV32178, XV32179, XV32180).
'lo.comp.dependencies^refsInScope'('_call%4'(XV32181, XV32182, XV32183, XV32184), 'lo.comp.dependencies^refsInScope', _):- 'lo.comp.dependencies@refsInScope'(XV32181, XV32182, XV32183, XV32184).
'lo.comp.dependencies^refsInContract'('_call%4'(XV32185, XV32186, XV32187, XV32188), 'lo.comp.dependencies^refsInContract', _):- 'lo.comp.dependencies@refsInContract'(XV32185, XV32186, XV32187, XV32188).
'lo.comp.dependencies^refsInHead'('_call%4'(XV32189, XV32190, XV32191, XV32192), 'lo.comp.dependencies^refsInHead', _):- 'lo.comp.dependencies@refsInHead'(XV32189, XV32190, XV32191, XV32192).
'lo.comp.dependencies^refsInNTs'('_call%4'(XV32193, XV32194, XV32195, XV32196), 'lo.comp.dependencies^refsInNTs', _):- 'lo.comp.dependencies@refsInNTs'(XV32193, XV32194, XV32195, XV32196).
'lo.comp.dependencies^refsInIndex'('_call%4'(XV32197, XV32198, XV32199, XV32200), 'lo.comp.dependencies^refsInIndex', _):- 'lo.comp.dependencies@refsInIndex'(XV32197, XV32198, XV32199, XV32200).
'lo.comp.dependencies@neg335'(XSo, XNm):- ocall('in%2'('()2'(XNm, 'lo.comp.abstract#valu'), XSo),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    fail.
'lo.comp.dependencies@neg335'(XSo, XNm).
'lo.comp.dependencies@condExp127'('lo.core#,..'('()2'(XNm, 'lo.comp.abstract#valu'), XSo), XXd40203, XSo, XAll, XNm):- ocall('in%2'('()2'(XNm, 'lo.comp.abstract#valu'), XAll),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.dependencies@neg335'(XSo, XNm),
    !.
'lo.comp.dependencies@condExp127'(XSo, XXd40203, XSo, XAll, XNm).
'lo.comp.dependencies^refsInTerm'('_call%4'(XV32201, XV32202, XV32203, XV32204), 'lo.comp.dependencies^refsInTerm', _):- 'lo.comp.dependencies@refsInTerm'(XV32201, XV32202, XV32203, XV32204).
'lo.comp.dependencies^refsInTerms'('_call%4'(XV32205, XV32206, XV32207, XV32208), 'lo.comp.dependencies^refsInTerms', _):- 'lo.comp.dependencies@refsInTerms'(XV32205, XV32206, XV32207, XV32208).
'lo.comp.dependencies@neg336'(XSo, XNm):- ocall('in%2'('()2'(XNm, 'lo.comp.abstract#valu'), XSo),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    fail.
'lo.comp.dependencies@neg336'(XSo, XNm).
'lo.comp.dependencies@condExp128'('lo.core#,..'('()2'(XNm, 'lo.comp.abstract#valu'), XSo), XXd40238, XSo, XAll, XNm):- ocall('in%2'('()2'(XNm, 'lo.comp.abstract#valu'), XAll),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.dependencies@neg336'(XSo, XNm),
    !.
'lo.comp.dependencies@condExp128'(XSo, XXd40238, XSo, XAll, XNm).
'lo.comp.dependencies^refsInCond'('_call%4'(XV32209, XV32210, XV32211, XV32212), 'lo.comp.dependencies^refsInCond', _):- 'lo.comp.dependencies@refsInCond'(XV32209, XV32210, XV32211, XV32212).
'lo.comp.dependencies^refsInConds'('_call%4'(XV32213, XV32214, XV32215, XV32216), 'lo.comp.dependencies^refsInConds', _):- 'lo.comp.dependencies@refsInConds'(XV32213, XV32214, XV32215, XV32216).
'lo.comp.dependencies^refsInNT'('_call%4'(XV32217, XV32218, XV32219, XV32220), 'lo.comp.dependencies^refsInNT', _):- 'lo.comp.dependencies@refsInNT'(XV32217, XV32218, XV32219, XV32220).
'lo.comp.dependencies^stmtRefs'('_call%4'(XV32221, XV32222, XV32223, XV32224), 'lo.comp.dependencies^stmtRefs', _):- 'lo.comp.dependencies@stmtRefs'(XV32221, XV32222, XV32223, XV32224).
'lo.comp.dependencies^thetaRefs'('_call%4'(XV32225, XV32226, XV32227, XV32228), 'lo.comp.dependencies^thetaRefs', _):- 'lo.comp.dependencies@thetaRefs'(XV32225, XV32226, XV32227, XV32228).
'lo.comp.dependencies^collectThetaRefs'('_call%4'(XV32229, XV32230, XV32231, XV32232), 'lo.comp.dependencies^collectThetaRefs', _):- 'lo.comp.dependencies@collectThetaRefs'(XV32229, XV32230, XV32231, XV32232).
'lo.comp.dependencies^dependencies'('_call%8'(XV32233, XV32234, XV32235, XV32236, XV32237, XV32238, XV32239, XV32240), 'lo.comp.dependencies^dependencies', _):- 'lo.comp.dependencies@dependencies'(XV32233, XV32234, XV32235, XV32236, XV32237, XV32238, XV32239, XV32240).
'lo.comp.dependencies^showGroups'('_call%1'(XV32241), 'lo.comp.dependencies^showGroups', _):- 'lo.comp.dependencies@showGroups'(XV32241).
