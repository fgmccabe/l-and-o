'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.dependencies'e'*'n19o19'()19'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.misc'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.keywords'e'*'n2o2'import'e'public'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.abstract'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.ast'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.topsort'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I2'defn'CT5St'lo.comp.abstract*defnKind't'lo.comp.location*location'LT2St'lo.comp.abstract*defnKind'Lt'lo.comp.ast*ast't'lo.comp.dependencies*defn''dependencies'PT8Lt'lo.comp.ast*ast'LLt'lo.comp.dependencies*defn'LT2St'lo.comp.abstract*defnKind'LT2St'lo.comp.ast*ast'Lt'lo.comp.ast*ast'Lt'lo.comp.ast*ast't'lo.comp.errors*report't'lo.comp.errors*report'\"s\"I1'defn'Yt'lo.comp.dependencies*defn'I0\"n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.dependencies@init'() :- !.
'lo.comp.dependencies#defn'('defn%1'('lo.comp.dependencies@defn'())) :- !.
'lo.topsort$depends$lo.comp.dependencies*defn'('lo.topsort$depends$lo.comp.dependencies*defn%1'('lo.topsort$depends$lo.comp.dependencies*defn')) :- !.
'lo.topsort$depends$lo.comp.dependencies*defn'('defines%2'(XV3811, XV3812), XLbl297, XThis297) :- !,
    'lo.topsort$depends$lo.comp.dependencies*defn@defines'(XV3811, XV3812, XLbl297, XThis297).
'lo.topsort$depends$lo.comp.dependencies*defn'('defines%1'('lo.topsort$depends$lo.comp.dependencies*defn^defines'(XLbl298, XThis298)), XLbl298, XThis298).
'lo.topsort$depends$lo.comp.dependencies*defn'('references%2'(XV3817, XV3818), XLbl299, XThis299) :- !,
    'lo.topsort$depends$lo.comp.dependencies*defn@references'(XV3817, XV3818, XLbl299, XThis299).
'lo.topsort$depends$lo.comp.dependencies*defn'('references%1'('lo.topsort$depends$lo.comp.dependencies*defn^references'(XLbl300, XThis300)), XLbl300, XThis300).
'lo.topsort$depends$lo.comp.dependencies*defn@defines'('lo.comp.dependencies#defn'(XX, XK, X_1727, X_1728, X_1729), (XX, XK), XLbV310, XThV310).
'lo.topsort$depends$lo.comp.dependencies*defn@references'('lo.comp.dependencies#defn'(X_1730, X_1731, X_1732, XR, X_1733), XR, XLbV310, XThV310) :- !.
'lo.topsort$depends$lo.comp.dependencies*defn@references'(_, _, _, _) :- raise_exception('error'("references", 18, 5, 32)).
'lo.comp.dependencies@showRefs'('lo.core#[]', X_1734, 'lo.core#[]') :- !.
'lo.comp.dependencies@showRefs'('lo.core#,..'((XNm, XK), XR), XSep, 'lo.core#,..'('lo.core#ss'(XSep), 'lo.core#,..'('lo.core#ss'(XNm), 'lo.core#,..'('lo.core#ss'(":"), 'lo.core#,..'(XX27171, XX27174))))) :- !,
    ocall('disp%2'(XK, XX27171),'lo.core$display$lo.comp.abstract*defnKind','lo.core$display$lo.comp.abstract*defnKind'),
    'lo.comp.dependencies@showRefs'(XR, ",", XX27174).
'lo.comp.dependencies@showRefs'(_, _, _) :- raise_exception('error'("showRefs", 30, 3, 20)).
'lo.comp.dependencies@dispDefn'('lo.comp.dependencies#defn'(XNm, XK, XLc, XRefs, XStmts), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(XNm), 'lo.core#,..'('lo.core#ss'(":"), 'lo.core#,..'(XX27189, XX27192))))) :- !,
    ocall('disp%2'(XK, XX27189),'lo.core$display$lo.comp.abstract*defnKind','lo.core$display$lo.comp.abstract*defnKind'),
    'lo.comp.dependencies@showRefs'(XRefs, "->", XX27192).
'lo.comp.dependencies@dispDefn'(_, _) :- raise_exception('error'("dispDefn", 27, 3, 91)).
'lo.core$display$lo.comp.dependencies*defn'('lo.core$display$lo.comp.dependencies*defn%1'('lo.core$display$lo.comp.dependencies*defn')) :- !.
'lo.core$display$lo.comp.dependencies*defn'('disp%2'(XV3828, XV3829), XLbl301, XThis301) :- !,
    'lo.core$display$lo.comp.dependencies*defn@disp'(XV3828, XV3829, XLbl301, XThis301).
'lo.core$display$lo.comp.dependencies*defn'('disp%1'('lo.core$display$lo.comp.dependencies*defn^disp'(XLbl302, XThis302)), XLbl302, XThis302).
'lo.core$display$lo.comp.dependencies*defn@disp'(XD, XX27199, XLbV311, XThV311) :- !,
    'lo.comp.dependencies@dispDefn'(XD, XX27199).
'lo.core$display$lo.comp.dependencies*defn@disp'(_, _, _, _) :- raise_exception('error'("disp", 22, 5, 22)).
'lo.core$display$lo.comp.abstract*defnKind'('lo.core$display$lo.comp.abstract*defnKind%1'('lo.core$display$lo.comp.abstract*defnKind')) :- !.
'lo.core$display$lo.comp.abstract*defnKind'('disp%2'(XV3834, XV3835), XLbl303, XThis303) :- !,
    'lo.core$display$lo.comp.abstract*defnKind@disp'(XV3834, XV3835, XLbl303, XThis303).
'lo.core$display$lo.comp.abstract*defnKind'('disp%1'('lo.core$display$lo.comp.abstract*defnKind^disp'(XLbl304, XThis304)), XLbl304, XThis304).
'lo.core$display$lo.comp.abstract*defnKind@disp'('lo.comp.abstract#tpe', 'lo.core#ss'("type"), XLbV312, XThV312) :- !.
'lo.core$display$lo.comp.abstract*defnKind@disp'('lo.comp.abstract#con', 'lo.core#ss'("contract"), XLbV312, XThV312) :- !.
'lo.core$display$lo.comp.abstract*defnKind@disp'('lo.comp.abstract#impl', 'lo.core#ss'("implementation"), XLbV312, XThV312) :- !.
'lo.core$display$lo.comp.abstract*defnKind@disp'('lo.comp.abstract#valu', 'lo.core#ss'("var"), XLbV312, XThV312) :- !.
'lo.core$display$lo.comp.abstract*defnKind@disp'('lo.comp.abstract#clss', 'lo.core#ss'("class"), XLbV312, XThV312) :- !.
'lo.core$display$lo.comp.abstract*defnKind@disp'(_, _, _, _) :- raise_exception('error'("disp", 34, 5, 23)).
'lo.comp.dependencies@isImport'(XSt, XX27212) :- 'lo.comp.abstract@isUnary'(XSt, "import", X_1735, XP),
    'lo.comp.abstract@packageName'(XP, XX27212).
'lo.comp.dependencies@checkPublic'('lo.comp.package#pUblic', XNm, XK, 'lo.core#,..'((XNm, XK), XE), XE).
'lo.comp.dependencies@checkPublic'('lo.comp.package#priVate', X_1736, X_1737, XE, XE).
'lo.comp.dependencies@isOther'(XSt) :- 'lo.comp.abstract@isUnary'(XSt, "assert", X_1738, X_1739).
'lo.comp.dependencies@isOther'(XSt) :- 'lo.comp.abstract@isUnary'(XSt, "show", X_1740, X_1741).
'lo.comp.dependencies@contractName'(XS, XNm) :- 'lo.comp.abstract@isQuantified'(XS, X_1742, XI),
    'lo.comp.dependencies@contractName'(XI, XNm).
'lo.comp.dependencies@contractName'(XS, XNm) :- 'lo.comp.abstract@isConstrained'(XS, X_1743, XR),
    'lo.comp.dependencies@contractName'(XR, XNm).
'lo.comp.dependencies@contractName'(XS, XNm) :- 'lo.comp.abstract@isBinary'(XS, "<~", X_1744, XL, X_1745),
    'lo.comp.dependencies@contractName'(XL, XNm).
'lo.comp.dependencies@contractName'(XS, XNm) :- 'lo.comp.abstract@isSquareTerm'(XS, X_1746, XOp, X_1747),
    'lo.comp.abstract@isIden'(XOp, X_1748, XNm).
'lo.comp.dependencies@surfaceName'(XN, 'lo.core#ss'(XNm)) :- 'lo.comp.abstract@isIden'(XN, X_1749, XNm),
    !.
'lo.comp.dependencies@surfaceName'(XS, 'lo.core#ss'(XNm)) :- 'lo.comp.abstract@isSquareTerm'(XS, X_1750, XOp, X_1751),
    'lo.comp.abstract@isIden'(XOp, X_1752, XNm),
    !.
'lo.comp.dependencies@surfaceName'(XT, 'lo.core#ss'(XX27297)) :- 'lo.comp.abstract@isRoundTuple'(XT, X_1753, XEls),
    !,
    'lo.list@length'(XEls, XX27290),
    ocall('disp%2'(XX27290, XX27291),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("()"), 'lo.core#,..'(XX27291, 'lo.core#[]'))), XX27297).
'lo.comp.dependencies@surfaceName'(_, _) :- raise_exception('error'("surfaceName", 164, 3, 44)).
'lo.comp.dependencies@surfaceNames'('lo.core#,..'(XT, X_1754), XSep, XX27310) :- 'lo.comp.abstract@isBinary'(XT, "->>", X_1755, XL, X_1756),
    !,
    'lo.comp.abstract@deComma'(XL, XX27308),
    'lo.comp.dependencies@surfaceNames'(XX27308, XSep, XX27310).
'lo.comp.dependencies@surfaceNames'('lo.core#[]', X_1757, 'lo.core#[]') :- !.
'lo.comp.dependencies@surfaceNames'('lo.core#,..'(XT, XEls), XSep, 'lo.core#,..'('lo.core#ss'(XSep), 'lo.core#,..'(XX27321, XX27324))) :- !,
    'lo.comp.dependencies@surfaceName'(XT, XX27321),
    'lo.comp.dependencies@surfaceNames'(XEls, XSep, XX27324).
'lo.comp.dependencies@surfaceNames'(_, _, _) :- raise_exception('error'("surfaceNames", 158, 3, 88)).
'lo.comp.dependencies@astImplName'(XS, XNm) :- 'lo.comp.abstract@isQuantified'(XS, X_1758, XI),
    'lo.comp.dependencies@astImplName'(XI, XNm).
'lo.comp.dependencies@astImplName'(XS, XNm) :- 'lo.comp.abstract@isConstrained'(XS, X_1759, XI),
    'lo.comp.dependencies@astImplName'(XI, XNm).
'lo.comp.dependencies@astImplName'(XS, XX27350) :- 'lo.comp.abstract@isSquareTerm'(XS, X_1760, XOp, XA),
    'lo.comp.abstract@isIden'(XOp, X_1761, XNm),
    'lo.comp.abstract@marker'('lo.comp.abstract#con', XX27346),
    'lo.comp.dependencies@surfaceNames'(XA, XX27346, XX27347),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(XNm), XX27347)), XX27350).
'lo.comp.dependencies@astImplName'(XT, XX27368) :- 'lo.comp.abstract@isRoundTuple'(XT, X_1762, XA),
    'lo.list@length'(XA, XX27361),
    ocall('disp%2'(XX27361, XX27362),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("()"), 'lo.core#,..'(XX27362, 'lo.core#[]'))), XX27368).
'lo.comp.dependencies@typeRuleName'(XS, XNm) :- 'lo.comp.abstract@isQuantified'(XS, X_1763, XI),
    'lo.comp.dependencies@typeRuleName'(XI, XNm).
'lo.comp.dependencies@typeRuleName'(XS, XNm) :- 'lo.comp.abstract@isConstrained'(XS, X_1764, XI),
    'lo.comp.dependencies@typeRuleName'(XI, XNm).
'lo.comp.dependencies@typeRuleName'(XS, XNm) :- 'lo.comp.abstract@isBinary'(XS, "<~", X_1765, XL, X_1766),
    'lo.comp.dependencies@cond19'(X_1770, XNm, X_1769, X_1768, XOp, X_1767, XL).
'lo.comp.dependencies@ruleHead'(XS, XL) :- 'lo.comp.abstract@isBinary'(XS, "=", X_1771, XL, X_1772).
'lo.comp.dependencies@ruleHead'(XS, XL) :- 'lo.comp.abstract@isBinary'(XS, "=>", X_1773, XL, X_1774).
'lo.comp.dependencies@ruleHead'(XS, XL) :- 'lo.comp.abstract@isBinary'(XS, "-->", X_1775, XLL, X_1776),
    'lo.comp.abstract@isBinary'(XLL, ",", X_1777, XL, X_1778).
'lo.comp.dependencies@ruleHead'(XS, XL) :- 'lo.comp.abstract@isBinary'(XS, "-->", X_1779, XL, X_1780).
'lo.comp.dependencies@ruleHead'(XS, XL) :- 'lo.comp.abstract@isBinary'(XS, ":-", X_1781, XLL, X_1782),
    'lo.comp.dependencies@ruleHead'(XLL, XL).
'lo.comp.dependencies@ruleHead'(XS, XL) :- 'lo.comp.abstract@isBinary'(XS, "<=", X_1783, XL, X_1784).
'lo.comp.dependencies@ruleHead'(XS, XS) :- 'lo.comp.abstract@isRoundTerm'(XS, X_1785, X_1786, X_1787).
'lo.comp.dependencies@headName'(XH, XX27456) :- 'lo.comp.abstract@isBinary'(XH, "@@", X_1788, XL, X_1789),
    !,
    'lo.comp.dependencies@headName'(XL, XX27456).
'lo.comp.dependencies@headName'(XH, XNm) :- 'lo.comp.abstract@isRoundTerm'(XH, X_1790, XO, X_1791),
    'lo.comp.abstract@isIden'(XO, X_1792, XNm),
    !.
'lo.comp.dependencies@headName'(XH, XNm) :- 'lo.comp.abstract@isIden'(XH, X_1793, XNm),
    !.
'lo.comp.dependencies@headName'(XH, XX27478) :- 'lo.comp.abstract@isRoundTuple'(XH, X_1794, 'lo.core#,..'(XA, 'lo.core#[]')),
    !,
    'lo.comp.dependencies@headName'(XA, XX27478).
'lo.comp.dependencies@headName'(XH, XX27492) :- 'lo.comp.abstract@isRoundTuple'(XH, X_1795, XA),
    !,
    'lo.list@length'(XA, XX27485),
    ocall('disp%2'(XX27485, XX27486),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("()"), 'lo.core#,..'(XX27486, 'lo.core#[]'))), XX27492).
'lo.comp.dependencies@headName'(_, _) :- raise_exception('error'("headName", 114, 3, 54)).
'lo.comp.dependencies@ruleNameKind'(XS, 'lo.comp.abstract#con', XNm) :- 'lo.comp.abstract@isUnary'(XS, "contract", X_1796, XI),
    'lo.comp.dependencies@contractName'(XI, XNm).
'lo.comp.dependencies@ruleNameKind'(XS, 'lo.comp.abstract#impl', XNm) :- 'lo.comp.abstract@isUnary'(XS, "implementation", X_1797, XI),
    'lo.comp.abstract@isBinary'(XI, "<=", X_1798, XL, X_1799),
    'lo.comp.dependencies@astImplName'(XL, XNm).
'lo.comp.dependencies@ruleNameKind'(XS, 'lo.comp.abstract#tpe', XNm) :- 'lo.comp.abstract@isUnary'(XS, "type", X_1800, XI),
    'lo.comp.dependencies@typeRuleName'(XI, XNm).
'lo.comp.dependencies@ruleNameKind'(XS, XK, XNm) :- 'lo.comp.abstract@isUnary'(XS, "public", X_1801, XI),
    'lo.comp.dependencies@ruleNameKind'(XI, XK, XNm).
'lo.comp.dependencies@ruleNameKind'(XS, XK, XNm) :- 'lo.comp.abstract@isUnary'(XS, "private", X_1802, XI),
    'lo.comp.dependencies@ruleNameKind'(XI, XK, XNm).
'lo.comp.dependencies@ruleNameKind'(XS, 'lo.comp.abstract#valu', XNm) :- 'lo.comp.dependencies@one32'(XH, XS),
    'lo.comp.dependencies@headName'(XH, XX27546),
    XNm = XX27546.
'lo.comp.dependencies@collectDefines'('lo.core#[]', X_1803, X_1804, 'lo.core#[]', 'lo.core#[]').
'lo.comp.dependencies@collectDefines'('lo.core#,..'(XSt, XStmts), XNm, XK, XStx, 'lo.core#,..'(XSt, XDef)) :- 'lo.comp.dependencies@ruleNameKind'(XSt, XK, XNm),
    'lo.comp.dependencies@collectDefines'(XStmts, XNm, XK, XStx, XDef).
'lo.comp.dependencies@collectDefines'(XStmts, X_1805, X_1806, XStmts, 'lo.core#[]').
'lo.comp.dependencies@collectDefn'(XSt, XOrigSt, XStmts, XStmts, XDefs, XDefs, XP, XPx, XA, XA, 'lo.core#,..'(XOrigSt, XI), XI, XOth, XOth, XViz, XRp, XRp) :- 'lo.comp.dependencies@isImport'(XSt, XNm),
    'lo.comp.dependencies@checkPublic'(XViz, XNm, 'lo.comp.abstract#imp', XP, XPx).
'lo.comp.dependencies@collectDefn'(XSt, X_1807, XStmts, XStmts, XDefs, XDefs, XP, XP, XA, XA, XI, XI, 'lo.core#,..'(XSt, XOth), XOth, X_1808, XRp, XRp) :- 'lo.comp.dependencies@isOther'(XSt).
'lo.comp.dependencies@collectDefn'(XSt, XOrigSt, XStmts, XStx, XDefs, XDx, XP, XP, XA, XAx, XI, XIx, XO, XOx, X_1809, XRp, XRpx) :- 'lo.comp.abstract@isUnary'(XSt, "private", X_1810, XInner),
    'lo.comp.dependencies@collectDefn'(XInner, XOrigSt, XStmts, XStx, XDefs, XDx, XP, X_1811, XA, XAx, XI, XIx, XO, XOx, 'lo.comp.package#priVate', XRp, XRpx).
'lo.comp.dependencies@collectDefn'(XSt, XOrigSt, XStmts, XStx, XDefs, XDx, XP, XPx, XA, XAx, XI, XIx, XO, XOx, X_1812, XRp, XRpx) :- 'lo.comp.abstract@isUnary'(XSt, "public", X_1813, XInner),
    'lo.comp.dependencies@collectDefn'(XInner, XOrigSt, XStmts, XStx, XDefs, XDx, XP, XPx, XA, XAx, XI, XIx, XO, XOx, 'lo.comp.package#pUblic', XRp, XRpx).
'lo.comp.dependencies@collectDefn'(XSt, XOrigSt, XStmts, XStmts, XDefs, XDefs, XP, XPx, 'lo.core#,..'((XNm, XSt), XA), XA, XI, XI, XOth, XOth, XViz, XRp, XRp) :- 'lo.comp.abstract@isBinary'(XSt, ":", X_1814, XL, X_1815),
    'lo.comp.abstract@isIden'(XL, X_1816, XNm),
    'lo.comp.dependencies@checkPublic'(XViz, XNm, 'lo.comp.abstract#valu', XP, XPx).
'lo.comp.dependencies@collectDefn'(XSt, X_1817, XStmts, XStx, 'lo.core#,..'('lo.comp.dependencies#defn'(XNm, XK, XXV49, 'lo.core#[]', 'lo.core#,..'(XSt, XDfSts)), XDefs), XDefs, XP, XPx, XA, XA, XI, XI, XOth, XOth, XViz, XRp, XRp) :- ocall('loc%1'(XXV49),XSt,XSt),
    'lo.comp.dependencies@ruleNameKind'(XSt, XK, XNm),
    'lo.comp.dependencies@collectDefines'(XStmts, XNm, XK, XStx, XDfSts),
    'lo.comp.dependencies@checkPublic'(XViz, XNm, XK, XP, XPx).
'lo.comp.dependencies@collectDefn'(XSt, X_1818, XStmts, XStmts, XDefs, XDefs, XP, XP, XA, XA, XI, XI, XO, XO, X_1819, XRp, XRpx) :- ocall('disp%2'(XSt, XX27784),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot fathom "), 'lo.core#,..'(XX27784, 'lo.core#[]'))), XX27790),
    ocall('loc%1'(XXV50),XSt,XSt),
    'lo.comp.errors@reportError'(XX27790, XXV50, XRp, XRpx).
'lo.comp.dependencies@collectDefinitions'('lo.core#[]', 'lo.core#[]', 'lo.core#[]', 'lo.core#[]', 'lo.core#[]', 'lo.core#[]', XRp, XRp).
'lo.comp.dependencies@collectDefinitions'('lo.core#,..'(XSt, XStmts), XDefs, XPu, XAn, XIm, XOt, XRp, XRpx) :- 'lo.comp.dependencies@collectDefn'(XSt, XSt, XStmts, XS0, XDefs, XD0, XPu, XP0, XAn, XAn0, XIm, XIm0, XOt, XOt0, 'lo.comp.package#priVate', XRp, XRp0),
    'lo.comp.dependencies@collectDefinitions'(XS0, XD0, XP0, XAn0, XIm0, XOt0, XRp0, XRpx).
'lo.comp.dependencies@refsInScope'('lo.core#[]', X_1820, XSo, XSo) :- !.
'lo.comp.dependencies@refsInScope'('lo.core#,..'(XE, XL), XAll, XSo, XX27856) :- 'lo.comp.abstract@isBinary'(XE, ":", X_1821, X_1822, XT),
    !,
    'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XX27855),
    'lo.comp.dependencies@refsInScope'(XL, XAll, XX27855, XX27856).
'lo.comp.dependencies@refsInScope'(_, _, _, _) :- raise_exception('error'("refsInScope", 239, 3, 26)).
'lo.comp.dependencies@refsInConstraint'(XT, XAll, XSo, XCndV1) :- 'lo.comp.abstract@isIden'(XT, X_1823, XNm),
    !,
    'lo.comp.dependencies@condExp1'(XCndV1, XSo, XAll, XNm).
'lo.comp.dependencies@refsInConstraint'(XT, XAll, XSo, XX27889) :- 'lo.comp.abstract@isSquareTerm'(XT, X_1824, XOp, XArgs),
    !,
    'lo.comp.dependencies@refsInConstraint'(XOp, XAll, XSo, XX27888),
    'lo.comp.dependencies@refsInTypes'(XArgs, XAll, XX27888, XX27889).
'lo.comp.dependencies@refsInConstraint'(XT, XAll, XSo, XX27903) :- 'lo.comp.abstract@isBinary'(XT, ",", X_1825, XL, XR),
    !,
    'lo.comp.dependencies@refsInConstraint'(XR, XAll, XSo, XX27902),
    'lo.comp.dependencies@refsInConstraint'(XL, XAll, XX27902, XX27903).
'lo.comp.dependencies@refsInConstraint'(XT, XAll, XSo, XX27917) :- 'lo.comp.abstract@isBinary'(XT, "|:", X_1826, XL, XR),
    !,
    'lo.comp.dependencies@refsInConstraint'(XR, XAll, XSo, XX27916),
    'lo.comp.dependencies@refsInConstraint'(XL, XAll, XX27916, XX27917).
'lo.comp.dependencies@refsInConstraint'(XT, XAll, XSo, XX27931) :- 'lo.comp.abstract@isBinary'(XT, "<~", X_1827, XL, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XX27930),
    'lo.comp.dependencies@refsInType'(XL, XAll, XX27930, XX27931).
'lo.comp.dependencies@refsInConstraint'(XT, XAll, XSo, XX27941) :- 'lo.comp.abstract@isQuantified'(XT, X_1828, XI),
    !,
    'lo.comp.dependencies@refsInConstraint'(XI, XAll, XSo, XX27941).
'lo.comp.dependencies@refsInConstraint'(_, _, _, _) :- raise_exception('error'("refsInConstraint", 247, 3, 111)).
'lo.comp.dependencies@refsInTypes'('lo.core#[]', X_1829, XSo, XSo) :- !.
'lo.comp.dependencies@refsInTypes'('lo.core#,..'(XT, XL), XAll, XSo, XX27957) :- !,
    'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XX27956),
    'lo.comp.dependencies@refsInTypes'(XL, XAll, XX27956, XX27957).
'lo.comp.dependencies@refsInTypes'(_, _, _, _) :- raise_exception('error'("refsInTypes", 235, 3, 26)).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XCndV2) :- 'lo.comp.abstract@isIden'(XT, X_1830, XNm),
    !,
    'lo.comp.dependencies@condExp2'(XCndV2, XSo, XAll, XNm).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XX27990) :- 'lo.comp.abstract@isSquareTerm'(XT, X_1831, XOp, XArgs),
    !,
    'lo.comp.dependencies@refsInType'(XOp, XAll, XSo, XX27989),
    'lo.comp.dependencies@refsInTypes'(XArgs, XAll, XX27989, XX27990).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XX28004) :- 'lo.comp.abstract@isBraceTerm'(XT, X_1832, XA, 'lo.core#[]'),
    'lo.comp.abstract@isRoundTuple'(XA, X_1833, XArgs),
    !,
    'lo.comp.dependencies@refsInTypes'(XArgs, XAll, XSo, XX28004).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XX28018) :- 'lo.comp.abstract@isBinary'(XT, ",", X_1834, XL, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XX28017),
    'lo.comp.dependencies@refsInType'(XL, XAll, XX28017, XX28018).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XX28032) :- 'lo.comp.abstract@isBinary'(XT, "=>", X_1835, XL, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XX28031),
    'lo.comp.dependencies@refsInType'(XL, XAll, XX28031, XX28032).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XX28046) :- 'lo.comp.abstract@isBinary'(XT, "-->", X_1836, XL, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XX28045),
    'lo.comp.dependencies@refsInType'(XL, XAll, XX28045, XX28046).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XX28060) :- 'lo.comp.abstract@isBinary'(XT, "<=>", X_1837, XL, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XX28059),
    'lo.comp.dependencies@refsInType'(XL, XAll, XX28059, XX28060).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XX28074) :- 'lo.comp.abstract@isBinary'(XT, "->>", X_1838, XL, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XX28073),
    'lo.comp.dependencies@refsInType'(XL, XAll, XX28073, XX28074).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XX28084) :- 'lo.comp.abstract@isRoundTuple'(XT, X_1839, XL),
    !,
    'lo.comp.dependencies@refsInTypes'(XL, XAll, XSo, XX28084).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XX28098) :- 'lo.comp.abstract@isBinary'(XT, "|:", X_1840, XL, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XX28097),
    'lo.comp.dependencies@refsInConstraint'(XL, XAll, XX28097, XX28098).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XX28109) :- 'lo.comp.abstract@isBinary'(XT, "<~", X_1841, X_1842, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XX28109).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XX28119) :- 'lo.comp.abstract@isQuantified'(XT, X_1843, XI),
    !,
    'lo.comp.dependencies@refsInType'(XI, XAll, XSo, XX28119).
'lo.comp.dependencies@refsInType'(XT, XAll, XSo, XX28129) :- 'lo.comp.abstract@isBraceTuple'(XT, X_1844, XE),
    !,
    'lo.comp.dependencies@refsInScope'(XE, XAll, XSo, XX28129).
'lo.comp.dependencies@refsInType'(XT, X_1845, XSo, XSo) :- !.
'lo.comp.dependencies@refsInType'(_, _, _, _) :- raise_exception('error'("refsInType", 219, 3, 105)).
'lo.comp.dependencies@refsInContract'(XC, XAll, XSo, XX28143) :- 'lo.comp.abstract@isQuantified'(XC, X_1846, XI),
    !,
    'lo.comp.dependencies@refsInContract'(XI, XAll, XSo, XX28143).
'lo.comp.dependencies@refsInContract'(XC, XAll, XSo, XX28154) :- 'lo.comp.abstract@isBinary'(XC, "<~", X_1847, XL, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XX28154).
'lo.comp.dependencies@refsInContract'(_, _, _, _) :- raise_exception('error'("refsInContract", 243, 3, 75)).
'lo.comp.dependencies@refsInHead'(XS, XAll, XSo, XX28168) :- 'lo.comp.abstract@isBinary'(XS, "@@", X_1848, XL, XR),
    !,
    'lo.comp.dependencies@refsInCond'(XR, XAll, XSo, XX28167),
    'lo.comp.dependencies@refsInHead'(XL, XAll, XX28167, XX28168).
'lo.comp.dependencies@refsInHead'(XS, XAll, XSo, XX28179) :- 'lo.comp.abstract@isRoundTerm'(XS, X_1849, X_1850, XA),
    !,
    'lo.comp.dependencies@refsInTerms'(XA, XAll, XSo, XX28179).
'lo.comp.dependencies@refsInHead'(XS, XAll, XSo, XSo) :- 'lo.comp.abstract@isIden'(XS, X_1851, X_1852),
    !.
'lo.comp.dependencies@refsInHead'(XS, XAll, XSo, XX28201) :- 'lo.comp.abstract@isBinary'(XA, X_1853, X_1854, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XX28200),
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XX28200, XX28201).
'lo.comp.dependencies@refsInHead'(XS, XAll, XSo, XX28212) :- 'lo.comp.abstract@isUnary'(XA, X_1855, X_1856, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XX28212).
'lo.comp.dependencies@refsInHead'(_, _, _, _) :- raise_exception('error'("refsInHead", 212, 3, 89)).
'lo.comp.dependencies@refsInIndex'('lo.core#,..'(XA, 'lo.core#[]'), XAll, XSo, XX28228) :- 'lo.comp.abstract@isBinary'(XA, "->", X_1857, XKy, XVl),
    !,
    'lo.comp.dependencies@refsInTerm'(XKy, XAll, XSo, XX28227),
    'lo.comp.dependencies@refsInTerm'(XVl, XAll, XX28227, XX28228).
'lo.comp.dependencies@refsInIndex'('lo.core#,..'(XA, 'lo.core#[]'), XAll, XSo, XX28240) :- 'lo.comp.abstract@isUnary'(XA, "\\+", X_1858, XKy),
    !,
    'lo.comp.dependencies@refsInTerm'(XKy, XAll, XSo, XX28240).
'lo.comp.dependencies@refsInIndex'(XA, XAll, XSo, XX28247) :- !,
    'lo.comp.dependencies@refsInTerms'(XA, XAll, XSo, XX28247).
'lo.comp.dependencies@refsInIndex'(_, _, _, _) :- raise_exception('error'("refsInIndex", 323, 3, 95)).
'lo.comp.dependencies@refsInConds'('lo.core#[]', X_1859, XSo, XSo) :- !.
'lo.comp.dependencies@refsInConds'('lo.core#,..'(XT, XL), XAll, XSo, XX28263) :- !,
    'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XX28262),
    'lo.comp.dependencies@refsInConds'(XL, XAll, XX28262, XX28263).
'lo.comp.dependencies@refsInConds'(_, _, _, _) :- raise_exception('error'("refsInConds", 271, 3, 26)).
'lo.comp.dependencies@refsInNTs'('lo.core#[]', X_1860, XSo, XSo) :- !.
'lo.comp.dependencies@refsInNTs'('lo.core#,..'(XN, XL), XAll, XSo, XX28279) :- !,
    'lo.comp.dependencies@refsInNT'(XN, XAll, XSo, XX28278),
    'lo.comp.dependencies@refsInNTs'(XL, XAll, XX28278, XX28279).
'lo.comp.dependencies@refsInNTs'(_, _, _, _) :- raise_exception('error'("refsInNTs", 294, 3, 24)).
'lo.comp.dependencies@refsInTerms'('lo.core#[]', X_1861, XSo, XSo) :- !.
'lo.comp.dependencies@refsInTerms'('lo.core#,..'(XT, XL), XAll, XSo, XX28295) :- !,
    'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XX28294),
    'lo.comp.dependencies@refsInTerms'(XL, XAll, XX28294, XX28295).
'lo.comp.dependencies@refsInTerms'(_, _, _, _) :- raise_exception('error'("refsInTerms", 319, 3, 26)).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XX28305) :- 'lo.comp.abstract@isSquareTuple'(XT, X_1862, XArgs),
    !,
    'lo.comp.dependencies@refsInTerms'(XArgs, XAll, XSo, XX28305).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XX28326) :- 'lo.comp.abstract@isBinary'(XT, "|", X_1863, XL, XR),
    'lo.comp.abstract@isBinary'(XL, "?", X_1864, XLL, XLR),
    !,
    'lo.comp.dependencies@refsInNT'(XLL, XAll, XSo, XX28324),
    'lo.comp.dependencies@refsInNT'(XLR, XAll, XX28324, XX28325),
    'lo.comp.dependencies@refsInNT'(XR, XAll, XX28325, XX28326).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XX28340) :- 'lo.comp.abstract@isBinary'(XT, "|", X_1865, XL, XR),
    !,
    'lo.comp.dependencies@refsInNT'(XL, XAll, XSo, XX28339),
    'lo.comp.dependencies@refsInNT'(XR, XAll, XX28339, XX28340).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XX28354) :- 'lo.comp.abstract@isBinary'(XT, ",", X_1866, XL, XR),
    !,
    'lo.comp.dependencies@refsInNT'(XR, XAll, XSo, XX28353),
    'lo.comp.dependencies@refsInNT'(XL, XAll, XX28353, XX28354).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XX28364) :- 'lo.comp.abstract@isUnary'(XT, "!", X_1867, XL),
    !,
    'lo.comp.dependencies@refsInNT'(XL, XAll, XSo, XX28364).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XX28374) :- 'lo.comp.abstract@isUnary'(XT, "\\+", X_1868, XL),
    !,
    'lo.comp.dependencies@refsInNT'(XL, XAll, XSo, XX28374).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XX28384) :- 'lo.comp.abstract@isUnary'(XT, "+", X_1869, XL),
    !,
    'lo.comp.dependencies@refsInNT'(XL, XAll, XSo, XX28384).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XX28398) :- 'lo.comp.abstract@isBinary'(XT, "@@", X_1870, XL, XR),
    !,
    'lo.comp.dependencies@refsInCond'(XR, XAll, XSo, XX28397),
    'lo.comp.dependencies@refsInNT'(XL, XAll, XX28397, XX28398).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XX28412) :- 'lo.comp.abstract@isBinary'(XT, "=", X_1871, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XX28411),
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XX28411, XX28412).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XX28426) :- 'lo.comp.abstract@isBinary'(XT, "\\=", X_1872, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XX28425),
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XX28425, XX28426).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XX28436) :- 'lo.comp.abstract@isUnary'(XT, "@", X_1873, XL),
    !,
    'lo.comp.dependencies@refsInCond'(XL, XAll, XSo, XX28436).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XX28450) :- 'lo.comp.abstract@isRoundTerm'(XT, X_1874, XOp, XArgs),
    !,
    'lo.comp.dependencies@refsInTerm'(XOp, XAll, XSo, XX28449),
    'lo.comp.dependencies@refsInTerms'(XArgs, XAll, XX28449, XX28450).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XSo) :- 'lo.comp.abstract@isString'(XT, X_1875, X_1876),
    !.
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XX28467) :- 'lo.comp.abstract@isRoundTuple'(XT, X_1877, XEls),
    !,
    'lo.comp.dependencies@refsInNTs'(XEls, XAll, XSo, XX28467).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XX28477) :- 'lo.comp.abstract@isBraceTuple'(XT, X_1878, XEls),
    !,
    'lo.comp.dependencies@refsInConds'(XEls, XAll, XSo, XX28477).
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XSo) :- 'lo.comp.abstract@isIden'(XT, X_1879, "eof"),
    !.
'lo.comp.dependencies@refsInNT'(XT, XAll, XSo, XX28490) :- !,
    'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XX28490).
'lo.comp.dependencies@refsInNT'(_, _, _, _) :- raise_exception('error'("refsInNT", 275, 3, 74)).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XCndV3) :- 'lo.comp.abstract@isIden'(XT, X_1880, XNm),
    !,
    'lo.comp.dependencies@condExp3'(XCndV3, XSo, XAll, XNm).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XX28523) :- 'lo.comp.abstract@isBinary'(XT, ",", X_1881, XL, XR),
    !,
    'lo.comp.dependencies@refsInCond'(XR, XAll, XSo, XX28522),
    'lo.comp.dependencies@refsInCond'(XL, XAll, XX28522, XX28523).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XX28544) :- 'lo.comp.abstract@isBinary'(XT, "|", X_1882, XL, XR),
    'lo.comp.abstract@isBinary'(XL, "?", X_1883, XLL, XLR),
    !,
    'lo.comp.dependencies@refsInCond'(XLL, XAll, XSo, XX28542),
    'lo.comp.dependencies@refsInCond'(XLR, XAll, XX28542, XX28543),
    'lo.comp.dependencies@refsInCond'(XR, XAll, XX28543, XX28544).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XX28558) :- 'lo.comp.abstract@isBinary'(XT, "|", X_1884, XL, XR),
    !,
    'lo.comp.dependencies@refsInCond'(XL, XAll, XSo, XX28557),
    'lo.comp.dependencies@refsInCond'(XR, XAll, XX28557, XX28558).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XX28572) :- 'lo.comp.abstract@isBinary'(XT, "*>", X_1885, XL, XR),
    !,
    'lo.comp.dependencies@refsInCond'(XR, XAll, XSo, XX28571),
    'lo.comp.dependencies@refsInCond'(XL, XAll, XX28571, XX28572).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XX28582) :- 'lo.comp.abstract@isUnary'(XT, "!", X_1886, XL),
    !,
    'lo.comp.dependencies@refsInCond'(XL, XAll, XSo, XX28582).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XX28592) :- 'lo.comp.abstract@isUnary'(XT, "\\+", X_1887, XL),
    !,
    'lo.comp.dependencies@refsInCond'(XL, XAll, XSo, XX28592).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XX28606) :- 'lo.comp.abstract@isBinary'(XT, "=", X_1888, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XX28605),
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XX28605, XX28606).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XX28620) :- 'lo.comp.abstract@isBinary'(XT, "\\=", X_1889, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XX28619),
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XX28619, XX28620).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XX28641) :- 'lo.comp.abstract@isBinary'(XT, "%%", X_1890, XL, XR),
    'lo.comp.abstract@isBinary'(XR, "~", X_1891, XLL, XLR),
    !,
    'lo.comp.dependencies@refsInTerm'(XLL, XAll, XSo, XX28639),
    'lo.comp.dependencies@refsInTerm'(XLR, XAll, XX28639, XX28640),
    'lo.comp.dependencies@refsInNT'(XL, XAll, XX28640, XX28641).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XX28655) :- 'lo.comp.abstract@isBinary'(XT, "%%", X_1892, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XX28654),
    'lo.comp.dependencies@refsInNT'(XL, XAll, XX28654, XX28655).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XX28669) :- 'lo.comp.abstract@isRoundTerm'(XT, X_1893, XOp, XArgs),
    !,
    'lo.comp.dependencies@refsInTerm'(XOp, XAll, XSo, XX28668),
    'lo.comp.dependencies@refsInTerms'(XArgs, XAll, XX28668, XX28669).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XX28679) :- 'lo.comp.abstract@isRoundTuple'(XT, X_1894, XArgs),
    !,
    'lo.comp.dependencies@refsInConds'(XArgs, XAll, XSo, XX28679).
'lo.comp.dependencies@refsInCond'(XT, XAll, XSo, XX28686) :- !,
    'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XX28686).
'lo.comp.dependencies@refsInCond'(_, _, _, _) :- raise_exception('error'("refsInCond", 255, 3, 108)).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XCndV4) :- 'lo.comp.abstract@isIden'(XT, X_1895, XNm),
    !,
    'lo.comp.dependencies@condExp4'(XCndV4, XSo, XAll, XNm).
'lo.comp.dependencies@refsInTerm'(XT, X_1896, XSo, XSo) :- 'lo.comp.abstract@isScalar'(XT),
    !.
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XX28724) :- 'lo.comp.abstract@isBinary'(XT, ":", X_1897, XL, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XX28723),
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XX28723, XX28724).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XX28738) :- 'lo.comp.abstract@isBinary'(XT, "::", X_1898, XL, XR),
    !,
    'lo.comp.dependencies@refsInType'(XR, XAll, XSo, XX28737),
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XX28737, XX28738).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XX28752) :- 'lo.comp.abstract@isBinary'(XT, "@@", X_1899, XL, XR),
    !,
    'lo.comp.dependencies@refsInCond'(XR, XAll, XSo, XX28751),
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XX28751, XX28752).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XX28762) :- 'lo.comp.abstract@isUnary'(XT, "@", X_1900, XL),
    !,
    'lo.comp.dependencies@refsInCond'(XL, XAll, XSo, XX28762).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XX28773) :- 'lo.comp.abstract@isBinary'(XT, ".", X_1901, XL, X_1902),
    !,
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XSo, XX28773).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XX28794) :- 'lo.comp.abstract@isBinary'(XT, "|", X_1903, XL, XR),
    'lo.comp.abstract@isBinary'(XL, "?", X_1904, XLL, XLR),
    !,
    'lo.comp.dependencies@refsInCond'(XLL, XAll, XSo, XX28792),
    'lo.comp.dependencies@refsInTerm'(XLR, XAll, XX28792, XX28793),
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XX28793, XX28794).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XX28808) :- 'lo.comp.abstract@isSquareTerm'(XT, X_1905, XOp, XArgs),
    !,
    'lo.comp.dependencies@refsInTerm'(XOp, XAll, XSo, XX28807),
    'lo.comp.dependencies@refsInIndex'(XArgs, XAll, XX28807, XX28808).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XX28818) :- 'lo.comp.abstract@isRoundTuple'(XT, X_1906, XArgs),
    !,
    'lo.comp.dependencies@refsInTerms'(XArgs, XAll, XSo, XX28818).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XX28836) :- 'lo.comp.abstract@isSquareTuple'(XT, XLc, XArgs),
    !,
    'lo.comp.dependencies@refsInTerm'('lo.comp.ast#iden'(XLc, "[]"), XAll, XSo, XX28834),
    'lo.comp.dependencies@refsInTerm'('lo.comp.ast#iden'(XLc, ",.."), XAll, XX28834, XX28835),
    'lo.comp.dependencies@refsInTerms'(XArgs, XAll, XX28835, XX28836).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XX28850) :- 'lo.comp.abstract@isBinary'(XT, ",..", X_1907, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XSo, XX28849),
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XX28849, XX28850).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XX28864) :- 'lo.comp.abstract@isBinary'(XT, "->", X_1908, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XSo, XX28863),
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XX28863, XX28864).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XX28874) :- 'lo.comp.abstract@isBraceTuple'(XT, X_1909, XEls),
    !,
    'lo.comp.dependencies@thetaRefs'(XEls, XAll, XSo, XX28874).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XX28888) :- 'lo.comp.abstract@isBinary'(XT, "=>", X_1910, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XSo, XX28887),
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XX28887, XX28888).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XX28902) :- 'lo.comp.abstract@isBinary'(XT, ":-", X_1911, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XSo, XX28901),
    'lo.comp.dependencies@refsInCond'(XR, XAll, XX28901, XX28902).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XX28916) :- 'lo.comp.abstract@isBinary'(XT, "-->", X_1912, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XL, XAll, XSo, XX28915),
    'lo.comp.dependencies@refsInNT'(XR, XAll, XX28915, XX28916).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XX28930) :- 'lo.comp.abstract@isRoundTerm'(XT, X_1913, XOp, XArgs),
    !,
    'lo.comp.dependencies@refsInTerm'(XOp, XAll, XSo, XX28929),
    'lo.comp.dependencies@refsInTerms'(XArgs, XAll, XX28929, XX28930).
'lo.comp.dependencies@refsInTerm'(XT, XAll, XSo, XSo) :- !.
'lo.comp.dependencies@refsInTerm'(_, _, _, _) :- raise_exception('error'("refsInTerm", 298, 3, 108)).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XX28944) :- 'lo.comp.abstract@isUnary'(XS, "private", X_1914, XI),
    !,
    'lo.comp.dependencies@stmtRefs'(XI, XAll, XSo, XX28944).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XX28954) :- 'lo.comp.abstract@isUnary'(XS, "public", X_1915, XI),
    !,
    'lo.comp.dependencies@stmtRefs'(XI, XAll, XSo, XX28954).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XX28964) :- 'lo.comp.abstract@isUnary'(XS, "type", X_1916, XI),
    !,
    'lo.comp.dependencies@refsInType'(XI, XAll, XSo, XX28964).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XX28974) :- 'lo.comp.abstract@isUnary'(XS, "contract", X_1917, XI),
    !,
    'lo.comp.dependencies@refsInContract'(XI, XAll, XSo, XX28974).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XX28991) :- 'lo.comp.abstract@isUnary'(XS, "implementation", X_1918, XI),
    'lo.comp.abstract@isBinary'(XI, "<=", X_1919, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XX28990),
    'lo.comp.dependencies@refsInConstraint'(XL, XAll, XX28990, XX28991).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XX29005) :- 'lo.comp.abstract@isBinary'(XS, "=>", X_1920, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XX29004),
    'lo.comp.dependencies@refsInHead'(XL, XAll, XX29004, XX29005).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XX29026) :- 'lo.comp.abstract@isBinary'(XS, ":-", X_1921, XL, XR),
    'lo.comp.abstract@isBinary'(XL, "=>", X_1922, XH, XE),
    !,
    'lo.comp.dependencies@refsInCond'(XR, XAll, XSo, XX29024),
    'lo.comp.dependencies@refsInTerm'(XE, XAll, XX29024, XX29025),
    'lo.comp.dependencies@refsInHead'(XH, XAll, XX29025, XX29026).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XX29040) :- 'lo.comp.abstract@isBinary'(XS, ":-", X_1923, XL, XR),
    !,
    'lo.comp.dependencies@refsInCond'(XR, XAll, XSo, XX29039),
    'lo.comp.dependencies@refsInHead'(XL, XAll, XX29039, XX29040).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XX29051) :- 'lo.comp.abstract@isRoundTerm'(XS, X_1924, X_1925, XA),
    !,
    'lo.comp.dependencies@refsInTerms'(XA, XAll, XSo, XX29051).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XX29065) :- 'lo.comp.abstract@isBinary'(XS, "-->", X_1926, XL, XR),
    !,
    'lo.comp.dependencies@refsInNT'(XR, XAll, XSo, XX29064),
    'lo.comp.dependencies@refsInHead'(XL, XAll, XX29064, XX29065).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XX29079) :- 'lo.comp.abstract@isBinary'(XS, "<=", X_1927, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XX29078),
    'lo.comp.dependencies@refsInHead'(XL, XAll, XX29078, XX29079).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XX29090) :- 'lo.comp.abstract@isBinary'(XS, "=", X_1928, XL, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XX29090).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XX29100) :- 'lo.comp.abstract@isUnary'(XS, "show", X_1929, XR),
    !,
    'lo.comp.dependencies@refsInTerm'(XR, XAll, XSo, XX29100).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XX29110) :- 'lo.comp.abstract@isUnary'(XS, "assert", X_1930, XR),
    !,
    'lo.comp.dependencies@refsInCond'(XR, XAll, XSo, XX29110).
'lo.comp.dependencies@stmtRefs'(XS, XAll, XSo, XX29121) :- 'lo.comp.abstract@isBinary'(XS, ":", X_1931, X_1932, XI),
    !,
    'lo.comp.dependencies@refsInType'(XI, XAll, XSo, XX29121).
'lo.comp.dependencies@stmtRefs'(_, _, _, _) :- raise_exception('error'("stmtRefs", 195, 3, 70)).
'lo.comp.dependencies@thetaRefs'('lo.core#[]', X_1933, XSo, XSo) :- !.
'lo.comp.dependencies@thetaRefs'('lo.core#,..'(XSt, XStmts), XAll, XSo, XX29137) :- !,
    'lo.comp.dependencies@stmtRefs'(XSt, XAll, XSo, XX29136),
    'lo.comp.dependencies@thetaRefs'(XStmts, XAll, XX29136, XX29137).
'lo.comp.dependencies@thetaRefs'(_, _, _, _) :- raise_exception('error'("thetaRefs", 191, 3, 22)).
'lo.comp.dependencies@collectThetaRefs'('lo.core#[]', X_1934, X_1935, 'lo.core#[]') :- !.
'lo.comp.dependencies@collectThetaRefs'('lo.core#,..'('lo.comp.dependencies#defn'(XN, XK, XLc, X_1936, XS), XL), XAll, XAnnots, 'lo.core#,..'('lo.comp.dependencies#defn'(XN, XK, XLc, XX29167, XS), XX29173)) :- ocall('in%2'((XN, XA), XAnnots),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    'lo.comp.dependencies@refsInScope'('lo.core#,..'(XA, 'lo.core#[]'), XAll, 'lo.core#[]', XX29166),
    'lo.comp.dependencies@thetaRefs'(XS, XAll, XX29166, XX29167),
    'lo.comp.dependencies@collectThetaRefs'(XL, XAll, XAnnots, XX29173).
'lo.comp.dependencies@collectThetaRefs'('lo.core#,..'('lo.comp.dependencies#defn'(XN, XK, XLc, X_1937, XS), XL), XAll, XAnnots, 'lo.core#,..'('lo.comp.dependencies#defn'(XN, XK, XLc, XX29191, XS), XX29197)) :- !,
    'lo.comp.dependencies@thetaRefs'(XS, XAll, 'lo.core#[]', XX29191),
    'lo.comp.dependencies@collectThetaRefs'(XL, XAll, XAnnots, XX29197).
'lo.comp.dependencies@collectThetaRefs'(_, _, _, _) :- raise_exception('error'("collectThetaRefs", 184, 3, 30)).
'lo.comp.dependencies@allRefs'('lo.core#[]', 'lo.core#[]') :- !.
'lo.comp.dependencies@allRefs'('lo.core#,..'('lo.comp.dependencies#defn'(XNm, XK, X_1938, X_1939, X_1940), XL), 'lo.core#,..'((XNm, XK), XX29212)) :- !,
    'lo.comp.dependencies@allRefs'(XL, XX29212).
'lo.comp.dependencies@allRefs'(_, _) :- raise_exception('error'("allRefs", 180, 3, 17)).
'lo.comp.dependencies@dependencies'(XEls, XGroups, XPublic, XAnnots, XImports, XOther, XRp, XRpx) :- 'lo.comp.dependencies@collectDefinitions'(XEls, XDfs, XPublic, XAnnots, XImports, XOther, XRp, XRpx),
    'lo.comp.dependencies@allRefs'(XDfs, XX29234),
    'lo.comp.dependencies@collectThetaRefs'(XDfs, XX29234, XAnnots, XX29236),
    'lo.topsort@topsort'('lo.topsort$depends$lo.comp.dependencies*defn', XX29236, XX29237),
    XGroups = XX29237.
'lo.comp.dependencies@showGroups'('lo.core#[]').
'lo.comp.dependencies@showGroups'('lo.core#,..'(XG, XL)) :- ocall('disp%2'(XG, XX29244),'lo.core$display$lo.core*list'('lo.core$display$lo.comp.dependencies*defn'),'lo.core$display$lo.core*list'('lo.core$display$lo.comp.dependencies*defn')),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Group: "), 'lo.core#,..'(XX29244, 'lo.core#[]'))), XX29251),
    'lo.io@logMsg'(XX29251),
    'lo.comp.dependencies@showGroups'(XL).
'lo.topsort$depends$lo.comp.dependencies*defn^defines'('_call%2'(XV3809, XV3810), 'lo.topsort$depends$lo.comp.dependencies*defn^defines'(XLbV310, XThV310), _) :- 'lo.topsort$depends$lo.comp.dependencies*defn@defines'(XV3809, XV3810, XLbV310, XThV310).
'lo.topsort$depends$lo.comp.dependencies*defn^defines'('_call%2'(XV3813, XV3814), 'lo.topsort$depends$lo.comp.dependencies*defn^defines'(XLbV310, XThV310), _) :- 'lo.topsort$depends$lo.comp.dependencies*defn@defines'(XV3813, XV3814, XLbV310, XThV310).
'lo.topsort$depends$lo.comp.dependencies*defn^references'('_call%2'(XV3815, XV3816), 'lo.topsort$depends$lo.comp.dependencies*defn^references'(XLbV310, XThV310), _) :- 'lo.topsort$depends$lo.comp.dependencies*defn@references'(XV3815, XV3816, XLbV310, XThV310).
'lo.topsort$depends$lo.comp.dependencies*defn^references'('_call%2'(XV3819, XV3820), 'lo.topsort$depends$lo.comp.dependencies*defn^references'(XLbV310, XThV310), _) :- 'lo.topsort$depends$lo.comp.dependencies*defn@references'(XV3819, XV3820, XLbV310, XThV310).
'lo.comp.dependencies^showRefs'('_call%3'(XV3821, XV3822, XV3823), 'lo.comp.dependencies^showRefs', _) :- 'lo.comp.dependencies@showRefs'(XV3821, XV3822, XV3823).
'lo.comp.dependencies^dispDefn'('_call%2'(XV3824, XV3825), 'lo.comp.dependencies^dispDefn', _) :- 'lo.comp.dependencies@dispDefn'(XV3824, XV3825).
'lo.core$display$lo.comp.dependencies*defn^disp'('_call%2'(XV3826, XV3827), 'lo.core$display$lo.comp.dependencies*defn^disp'(XLbV311, XThV311), _) :- 'lo.core$display$lo.comp.dependencies*defn@disp'(XV3826, XV3827, XLbV311, XThV311).
'lo.core$display$lo.comp.dependencies*defn^disp'('_call%2'(XV3830, XV3831), 'lo.core$display$lo.comp.dependencies*defn^disp'(XLbV311, XThV311), _) :- 'lo.core$display$lo.comp.dependencies*defn@disp'(XV3830, XV3831, XLbV311, XThV311).
'lo.core$display$lo.comp.abstract*defnKind^disp'('_call%2'(XV3832, XV3833), 'lo.core$display$lo.comp.abstract*defnKind^disp'(XLbV312, XThV312), _) :- 'lo.core$display$lo.comp.abstract*defnKind@disp'(XV3832, XV3833, XLbV312, XThV312).
'lo.core$display$lo.comp.abstract*defnKind^disp'('_call%2'(XV3836, XV3837), 'lo.core$display$lo.comp.abstract*defnKind^disp'(XLbV312, XThV312), _) :- 'lo.core$display$lo.comp.abstract*defnKind@disp'(XV3836, XV3837, XLbV312, XThV312).
'lo.comp.dependencies^isImport'('_call%2'(XV3838, XV3839), 'lo.comp.dependencies^isImport', _) :- 'lo.comp.dependencies@isImport'(XV3838, XV3839).
'lo.comp.dependencies^checkPublic'('_call%5'(XV3840, XV3841, XV3842, XV3843, XV3844), 'lo.comp.dependencies^checkPublic', _) :- 'lo.comp.dependencies@checkPublic'(XV3840, XV3841, XV3842, XV3843, XV3844).
'lo.comp.dependencies^isOther'('_call%1'(XV3845), 'lo.comp.dependencies^isOther', _) :- 'lo.comp.dependencies@isOther'(XV3845).
'lo.comp.dependencies^contractName'('_call%2'(XV3846, XV3847), 'lo.comp.dependencies^contractName', _) :- 'lo.comp.dependencies@contractName'(XV3846, XV3847).
'lo.comp.dependencies^surfaceName'('_call%2'(XV3848, XV3849), 'lo.comp.dependencies^surfaceName', _) :- 'lo.comp.dependencies@surfaceName'(XV3848, XV3849).
'lo.comp.dependencies^surfaceNames'('_call%3'(XV3850, XV3851, XV3852), 'lo.comp.dependencies^surfaceNames', _) :- 'lo.comp.dependencies@surfaceNames'(XV3850, XV3851, XV3852).
'lo.comp.dependencies^astImplName'('_call%2'(XV3853, XV3854), 'lo.comp.dependencies^astImplName', _) :- 'lo.comp.dependencies@astImplName'(XV3853, XV3854).
'lo.comp.dependencies@cond19'(X_1770, XNm, X_1769, X_1768, XOp, X_1767, XL) :- 'lo.comp.abstract@isSquareTerm'(XL, X_1767, XOp, X_1768),
    !,
    'lo.comp.abstract@isIden'(XOp, X_1769, XNm).
'lo.comp.dependencies@cond19'(X_1770, XNm, X_1769, X_1768, XOp, X_1767, XL) :- 'lo.comp.abstract@isIden'(XL, X_1770, XNm).
'lo.comp.dependencies^typeRuleName'('_call%2'(XV3855, XV3856), 'lo.comp.dependencies^typeRuleName', _) :- 'lo.comp.dependencies@typeRuleName'(XV3855, XV3856).
'lo.comp.dependencies^ruleHead'('_call%2'(XV3857, XV3858), 'lo.comp.dependencies^ruleHead', _) :- 'lo.comp.dependencies@ruleHead'(XV3857, XV3858).
'lo.comp.dependencies^headName'('_call%2'(XV3859, XV3860), 'lo.comp.dependencies^headName', _) :- 'lo.comp.dependencies@headName'(XV3859, XV3860).
'lo.comp.dependencies@one32'(XH, XS) :- 'lo.comp.dependencies@ruleHead'(XS, XH),
    !.
'lo.comp.dependencies^ruleNameKind'('_call%3'(XV3861, XV3862, XV3863), 'lo.comp.dependencies^ruleNameKind', _) :- 'lo.comp.dependencies@ruleNameKind'(XV3861, XV3862, XV3863).
'lo.comp.dependencies^collectDefines'('_call%5'(XV3864, XV3865, XV3866, XV3867, XV3868), 'lo.comp.dependencies^collectDefines', _) :- 'lo.comp.dependencies@collectDefines'(XV3864, XV3865, XV3866, XV3867, XV3868).
'lo.comp.dependencies^collectDefn'('_call%17'(XV3869, XV3870, XV3871, XV3872, XV3873, XV3874, XV3875, XV3876, XV3877, XV3878, XV3879, XV3880, XV3881, XV3882, XV3883, XV3884, XV3885), 'lo.comp.dependencies^collectDefn', _) :- 'lo.comp.dependencies@collectDefn'(XV3869, XV3870, XV3871, XV3872, XV3873, XV3874, XV3875, XV3876, XV3877, XV3878, XV3879, XV3880, XV3881, XV3882, XV3883, XV3884, XV3885).
'lo.comp.dependencies^collectDefinitions'('_call%8'(XV3886, XV3887, XV3888, XV3889, XV3890, XV3891, XV3892, XV3893), 'lo.comp.dependencies^collectDefinitions', _) :- 'lo.comp.dependencies@collectDefinitions'(XV3886, XV3887, XV3888, XV3889, XV3890, XV3891, XV3892, XV3893).
'lo.comp.dependencies^refsInScope'('_call%4'(XV3894, XV3895, XV3896, XV3897), 'lo.comp.dependencies^refsInScope', _) :- 'lo.comp.dependencies@refsInScope'(XV3894, XV3895, XV3896, XV3897).
'lo.comp.dependencies@neg31'(XSo, XNm) :- ocall('in%2'((XNm, 'lo.comp.abstract#con'), XSo),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    fail.
'lo.comp.dependencies@neg31'(XSo, XNm).
'lo.comp.dependencies@condExp1'('lo.core#,..'((XNm, 'lo.comp.abstract#con'), XSo), XSo, XAll, XNm) :- ocall('in%2'((XNm, 'lo.comp.abstract#con'), XAll),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.dependencies@neg31'(XSo, XNm),
    !.
'lo.comp.dependencies@condExp1'(XSo, XSo, XAll, XNm).
'lo.comp.dependencies^refsInConstraint'('_call%4'(XV3898, XV3899, XV3900, XV3901), 'lo.comp.dependencies^refsInConstraint', _) :- 'lo.comp.dependencies@refsInConstraint'(XV3898, XV3899, XV3900, XV3901).
'lo.comp.dependencies^refsInTypes'('_call%4'(XV3902, XV3903, XV3904, XV3905), 'lo.comp.dependencies^refsInTypes', _) :- 'lo.comp.dependencies@refsInTypes'(XV3902, XV3903, XV3904, XV3905).
'lo.comp.dependencies@neg32'(XSo, XNm) :- ocall('in%2'((XNm, 'lo.comp.abstract#tpe'), XSo),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    fail.
'lo.comp.dependencies@neg32'(XSo, XNm).
'lo.comp.dependencies@condExp2'('lo.core#,..'((XNm, 'lo.comp.abstract#tpe'), XSo), XSo, XAll, XNm) :- ocall('in%2'((XNm, 'lo.comp.abstract#tpe'), XAll),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.dependencies@neg32'(XSo, XNm),
    !.
'lo.comp.dependencies@condExp2'(XSo, XSo, XAll, XNm).
'lo.comp.dependencies^refsInType'('_call%4'(XV3906, XV3907, XV3908, XV3909), 'lo.comp.dependencies^refsInType', _) :- 'lo.comp.dependencies@refsInType'(XV3906, XV3907, XV3908, XV3909).
'lo.comp.dependencies^refsInContract'('_call%4'(XV3910, XV3911, XV3912, XV3913), 'lo.comp.dependencies^refsInContract', _) :- 'lo.comp.dependencies@refsInContract'(XV3910, XV3911, XV3912, XV3913).
'lo.comp.dependencies^refsInHead'('_call%4'(XV3914, XV3915, XV3916, XV3917), 'lo.comp.dependencies^refsInHead', _) :- 'lo.comp.dependencies@refsInHead'(XV3914, XV3915, XV3916, XV3917).
'lo.comp.dependencies^refsInIndex'('_call%4'(XV3918, XV3919, XV3920, XV3921), 'lo.comp.dependencies^refsInIndex', _) :- 'lo.comp.dependencies@refsInIndex'(XV3918, XV3919, XV3920, XV3921).
'lo.comp.dependencies^refsInConds'('_call%4'(XV3922, XV3923, XV3924, XV3925), 'lo.comp.dependencies^refsInConds', _) :- 'lo.comp.dependencies@refsInConds'(XV3922, XV3923, XV3924, XV3925).
'lo.comp.dependencies^refsInNTs'('_call%4'(XV3926, XV3927, XV3928, XV3929), 'lo.comp.dependencies^refsInNTs', _) :- 'lo.comp.dependencies@refsInNTs'(XV3926, XV3927, XV3928, XV3929).
'lo.comp.dependencies^refsInTerms'('_call%4'(XV3930, XV3931, XV3932, XV3933), 'lo.comp.dependencies^refsInTerms', _) :- 'lo.comp.dependencies@refsInTerms'(XV3930, XV3931, XV3932, XV3933).
'lo.comp.dependencies^refsInNT'('_call%4'(XV3934, XV3935, XV3936, XV3937), 'lo.comp.dependencies^refsInNT', _) :- 'lo.comp.dependencies@refsInNT'(XV3934, XV3935, XV3936, XV3937).
'lo.comp.dependencies@neg33'(XSo, XNm) :- ocall('in%2'((XNm, 'lo.comp.abstract#valu'), XSo),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    fail.
'lo.comp.dependencies@neg33'(XSo, XNm).
'lo.comp.dependencies@condExp3'('lo.core#,..'((XNm, 'lo.comp.abstract#valu'), XSo), XSo, XAll, XNm) :- ocall('in%2'((XNm, 'lo.comp.abstract#valu'), XAll),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.dependencies@neg33'(XSo, XNm),
    !.
'lo.comp.dependencies@condExp3'(XSo, XSo, XAll, XNm).
'lo.comp.dependencies^refsInCond'('_call%4'(XV3938, XV3939, XV3940, XV3941), 'lo.comp.dependencies^refsInCond', _) :- 'lo.comp.dependencies@refsInCond'(XV3938, XV3939, XV3940, XV3941).
'lo.comp.dependencies@neg34'(XSo, XNm) :- ocall('in%2'((XNm, 'lo.comp.abstract#valu'), XSo),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    fail.
'lo.comp.dependencies@neg34'(XSo, XNm).
'lo.comp.dependencies@condExp4'('lo.core#,..'((XNm, 'lo.comp.abstract#valu'), XSo), XSo, XAll, XNm) :- ocall('in%2'((XNm, 'lo.comp.abstract#valu'), XAll),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.dependencies@neg34'(XSo, XNm),
    !.
'lo.comp.dependencies@condExp4'(XSo, XSo, XAll, XNm).
'lo.comp.dependencies^refsInTerm'('_call%4'(XV3942, XV3943, XV3944, XV3945), 'lo.comp.dependencies^refsInTerm', _) :- 'lo.comp.dependencies@refsInTerm'(XV3942, XV3943, XV3944, XV3945).
'lo.comp.dependencies^stmtRefs'('_call%4'(XV3946, XV3947, XV3948, XV3949), 'lo.comp.dependencies^stmtRefs', _) :- 'lo.comp.dependencies@stmtRefs'(XV3946, XV3947, XV3948, XV3949).
'lo.comp.dependencies^thetaRefs'('_call%4'(XV3950, XV3951, XV3952, XV3953), 'lo.comp.dependencies^thetaRefs', _) :- 'lo.comp.dependencies@thetaRefs'(XV3950, XV3951, XV3952, XV3953).
'lo.comp.dependencies^collectThetaRefs'('_call%4'(XV3954, XV3955, XV3956, XV3957), 'lo.comp.dependencies^collectThetaRefs', _) :- 'lo.comp.dependencies@collectThetaRefs'(XV3954, XV3955, XV3956, XV3957).
'lo.comp.dependencies^allRefs'('_call%2'(XV3958, XV3959), 'lo.comp.dependencies^allRefs', _) :- 'lo.comp.dependencies@allRefs'(XV3958, XV3959).
'lo.comp.dependencies^dependencies'('_call%8'(XV3960, XV3961, XV3962, XV3963, XV3964, XV3965, XV3966, XV3967), 'lo.comp.dependencies^dependencies', _) :- 'lo.comp.dependencies@dependencies'(XV3960, XV3961, XV3962, XV3963, XV3964, XV3965, XV3966, XV3967).
'lo.comp.dependencies^showGroups'('_call%1'(XV3968), 'lo.comp.dependencies^showGroups', _) :- 'lo.comp.dependencies@showGroups'(XV3968).
