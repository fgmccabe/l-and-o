'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.abstract'e'*'n15o15'()15'n2o2'import'e'public'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.keywords'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.ast'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I41'tpe't'lo.comp.abstract*defnKind''con't'lo.comp.abstract*defnKind''impl't'lo.comp.abstract*defnKind''valu't'lo.comp.abstract*defnKind''clss't'lo.comp.abstract*defnKind''imp't'lo.comp.abstract*defnKind''marker'FT1t'lo.comp.abstract*defnKind'S'isScalar'PT1t'lo.comp.ast*ast''isIden'PT3t'lo.comp.ast*ast't'lo.comp.location*location'S'genIden'FT1t'lo.comp.location*location't'lo.comp.ast*ast''isString'PT3t'lo.comp.ast*ast't'lo.comp.location*location'S'isInteger'PT3t'lo.comp.ast*ast't'lo.comp.location*location'i'isBinary'PT5t'lo.comp.ast*ast'St'lo.comp.location*location't'lo.comp.ast*ast't'lo.comp.ast*ast''binary'FT4t'lo.comp.location*location'St'lo.comp.ast*ast't'lo.comp.ast*ast't'lo.comp.ast*ast''isUnary'PT4t'lo.comp.ast*ast'St'lo.comp.location*location't'lo.comp.ast*ast''unary'FT3t'lo.comp.location*location'St'lo.comp.ast*ast't'lo.comp.ast*ast''isTernary'PT6t'lo.comp.ast*ast'St'lo.comp.location*location't'lo.comp.ast*ast't'lo.comp.ast*ast't'lo.comp.ast*ast''ternary'FT5t'lo.comp.location*location'St'lo.comp.ast*ast't'lo.comp.ast*ast't'lo.comp.ast*ast't'lo.comp.ast*ast''isSquareTerm'PT4t'lo.comp.ast*ast't'lo.comp.location*location't'lo.comp.ast*ast'Lt'lo.comp.ast*ast''squareTerm'FT3t'lo.comp.location*location't'lo.comp.ast*ast'Lt'lo.comp.ast*ast't'lo.comp.ast*ast''isBraceTerm'PT4t'lo.comp.ast*ast't'lo.comp.location*location't'lo.comp.ast*ast'Lt'lo.comp.ast*ast''braceTerm'FT3t'lo.comp.location*location't'lo.comp.ast*ast'Lt'lo.comp.ast*ast't'lo.comp.ast*ast''isRoundTuple'PT3t'lo.comp.ast*ast't'lo.comp.location*location'Lt'lo.comp.ast*ast''roundTuple'FT2t'lo.comp.location*location'Lt'lo.comp.ast*ast't'lo.comp.ast*ast''isSquareTuple'PT3t'lo.comp.ast*ast't'lo.comp.location*location'Lt'lo.comp.ast*ast''isBraceTuple'PT3t'lo.comp.ast*ast't'lo.comp.location*location'Lt'lo.comp.ast*ast''braceTuple'FT2t'lo.comp.location*location'Lt'lo.comp.ast*ast't'lo.comp.ast*ast''mapTuple'FT2t'lo.comp.location*location'Lt'lo.comp.ast*ast't'lo.comp.ast*ast''keyword'PT1t'lo.comp.ast*ast''isRoundTerm'PT4t'lo.comp.ast*ast't'lo.comp.location*location't'lo.comp.ast*ast'Lt'lo.comp.ast*ast''isRound'PT4t'lo.comp.ast*ast't'lo.comp.location*location't'lo.comp.ast*ast't'lo.comp.ast*ast''roundTerm'FT3t'lo.comp.location*location't'lo.comp.ast*ast'Lt'lo.comp.ast*ast't'lo.comp.ast*ast''deComma'FT1t'lo.comp.ast*ast'Lt'lo.comp.ast*ast''sameTerm'PT2t'lo.comp.ast*ast't'lo.comp.ast*ast''isQuantified'PT3t'lo.comp.ast*ast't'lo.comp.ast*ast't'lo.comp.ast*ast''isAlgebraicTypeDef'PT6t'lo.comp.ast*ast't'lo.comp.location*location'Lt'lo.comp.ast*ast'Lt'lo.comp.ast*ast't'lo.comp.ast*ast't'lo.comp.ast*ast''isContractSpec'PT6t'lo.comp.ast*ast't'lo.comp.location*location'Lt'lo.comp.ast*ast'Lt'lo.comp.ast*ast't'lo.comp.ast*ast'Lt'lo.comp.ast*ast''isConstrained'PT3t'lo.comp.ast*ast'Lt'lo.comp.ast*ast't'lo.comp.ast*ast''packageName'FT1t'lo.comp.ast*ast'S'pkgName'FT1t'lo.comp.ast*ast't'lo.repo*pkg''tupleize'PT3t'lo.comp.ast*ast'St'lo.comp.ast*ast'\"s\"I1'defnKind'Yt'lo.comp.abstract*defnKind'I0\"n6o6'()6's'tpe's'con's'impl's'valu's'clss's'imp'n0o0'()0'n0o0'()0'").
'lo.comp.abstract@init'() :- !.
'lo.comp.abstract@marker'('lo.comp.abstract#tpe', "*") :- !.
'lo.comp.abstract@marker'('lo.comp.abstract#valu', "@") :- !.
'lo.comp.abstract@marker'('lo.comp.abstract#clss', "#") :- !.
'lo.comp.abstract@marker'('lo.comp.abstract#con', "$") :- !.
'lo.comp.abstract@marker'(_, _) :- raise_exception('error'("marker", 14, 3, 15)).
'lo.comp.abstract@isScalar'('lo.comp.ast#intg'(X_743, X_744)).
'lo.comp.abstract@isScalar'('lo.comp.ast#flot'(X_745, X_746)).
'lo.comp.abstract@isScalar'('lo.comp.ast#strg'(X_747, X_748)).
'lo.comp.abstract@isIden'('lo.comp.ast#iden'(XLc, XNm), XLc, XNm).
'lo.comp.abstract@isIden'('lo.comp.ast#iden'(XLc, XNm), XLc, XNm).
'lo.comp.abstract@isIden'('lo.comp.ast#tupl'(XLc, "()", 'lo.core#,..'('lo.comp.ast#iden'(X_749, XNm), 'lo.core#[]')), XLc, XNm).
'lo.comp.abstract@genIden'(XLc, 'lo.comp.ast#iden'(XLc, XX11279)) :- !,
    '_str_gen'("N", XX11279).
'lo.comp.abstract@genIden'(_, _) :- raise_exception('error'("genIden", 28, 3, 37)).
'lo.comp.abstract@isString'('lo.comp.ast#strg'(XLc, XSt), XLc, XSt).
'lo.comp.abstract@isInteger'('lo.comp.ast#intg'(XLc, XIx), XLc, XIx).
'lo.comp.abstract@isBinary'('lo.comp.ast#appl'(XLc, 'lo.comp.ast#iden'(X_750, XNm), 'lo.comp.ast#tupl'(X_751, "()", 'lo.core#,..'(XL, 'lo.core#,..'(XR, 'lo.core#[]')))), XNm, XLc, XL, XR).
'lo.comp.abstract@binary'(XLc, XOp, XL, XR, 'lo.comp.ast#appl'(XLc, 'lo.comp.ast#iden'(XLc, XOp), 'lo.comp.ast#tupl'(XLc, "()", 'lo.core#,..'(XL, 'lo.core#,..'(XR, 'lo.core#[]'))))) :- !.
'lo.comp.abstract@binary'(_, _, _, _, _) :- raise_exception('error'("binary", 41, 3, 61)).
'lo.comp.abstract@isUnary'('lo.comp.ast#appl'(XLc, 'lo.comp.ast#iden'(X_752, XNm), 'lo.comp.ast#tupl'(X_753, "()", 'lo.core#,..'(XL, 'lo.core#[]'))), XNm, XLc, XL).
'lo.comp.abstract@unary'(XLc, XOp, XA, 'lo.comp.ast#appl'(XLc, 'lo.comp.ast#iden'(XLc, XOp), 'lo.comp.ast#tupl'(XLc, "()", 'lo.core#,..'(XA, 'lo.core#[]')))) :- !.
'lo.comp.abstract@unary'(_, _, _, _) :- raise_exception('error'("unary", 48, 3, 56)).
'lo.comp.abstract@isTernary'('lo.comp.ast#appl'(XLc, 'lo.comp.ast#iden'(X_754, XNm), 'lo.comp.ast#tupl'(X_755, "()", 'lo.core#,..'(XL, 'lo.core#,..'(XM, 'lo.core#,..'(XR, 'lo.core#[]'))))), XNm, XLc, XL, XM, XR).
'lo.comp.abstract@ternary'(XLc, XNm, XL, XM, XR, 'lo.comp.ast#appl'(XLc, 'lo.comp.ast#iden'(XLc, XNm), 'lo.comp.ast#tupl'(XLc, "()", 'lo.core#,..'(XL, 'lo.core#,..'(XM, 'lo.core#,..'(XR, 'lo.core#[]')))))) :- !.
'lo.comp.abstract@ternary'(_, _, _, _, _, _) :- raise_exception('error'("ternary", 54, 3, 66)).
'lo.comp.abstract@isSquareTerm'('lo.comp.ast#appl'(XLc, XOp, 'lo.comp.ast#tupl'(X_756, "[]", XA)), XLc, XOp, XA).
'lo.comp.abstract@squareTerm'(XLc, XOp, XEls, 'lo.comp.ast#appl'(XLc, XOp, 'lo.comp.ast#tupl'(XLc, "[]", XEls))) :- !.
'lo.comp.abstract@squareTerm'(_, _, _, _) :- raise_exception('error'("squareTerm", 62, 3, 54)).
'lo.comp.abstract@isBraceTerm'('lo.comp.ast#appl'(XLc, XOp, 'lo.comp.ast#tupl'(X_757, "{}", XA)), XLc, XOp, XA).
'lo.comp.abstract@braceTerm'(XLc, XOp, XEls, 'lo.comp.ast#appl'(XLc, XOp, 'lo.comp.ast#tupl'(XLc, "{}", XEls))) :- !.
'lo.comp.abstract@braceTerm'(_, _, _, _) :- raise_exception('error'("braceTerm", 69, 3, 53)).
'lo.comp.abstract@isRoundTuple'('lo.comp.ast#tupl'(XLc, "()", XA), XLc, XA).
'lo.comp.abstract@roundTuple'(XLc, XEls, 'lo.comp.ast#tupl'(XLc, "()", XEls)) :- !.
'lo.comp.abstract@roundTuple'(_, _, _) :- raise_exception('error'("roundTuple", 81, 3, 39)).
'lo.comp.abstract@isSquareTuple'('lo.comp.ast#tupl'(XLc, "[]", XA), XLc, XA).
'lo.comp.abstract@isBraceTuple'('lo.comp.ast#tupl'(XLc, "{}", XA), XLc, XA).
'lo.comp.abstract@braceTuple'(XLc, XEls, 'lo.comp.ast#tupl'(XLc, "{}", XEls)) :- !.
'lo.comp.abstract@braceTuple'(_, _, _) :- raise_exception('error'("braceTuple", 93, 3, 39)).
'lo.comp.abstract@mapTuple'(XLc, XEls, 'lo.comp.ast#tupl'(XLc, "{}", XEls)) :- !.
'lo.comp.abstract@mapTuple'(_, _, _) :- raise_exception('error'("mapTuple", 97, 3, 37)).
'lo.comp.abstract@keyword'('lo.comp.ast#iden'(X_758, XNm)) :- 'lo.comp.keywords@isKeyword'(XNm).
'lo.comp.abstract@isRoundTerm'('lo.comp.ast#appl'(XLc, XOp, 'lo.comp.ast#tupl'(X_759, "()", XA)), XLc, XOp, XA) :- 'lo.comp.abstract@neg22'(XOp).
'lo.comp.abstract@isRound'('lo.comp.ast#appl'(XLc, XOp, XA), XLc, XOp, XA) :- 'lo.comp.abstract@neg23'(XOp),
    'lo.comp.abstract@isRoundTuple'(XA, X_760, X_761).
'lo.comp.abstract@roundTerm'(XLc, XOp, XEls, 'lo.comp.ast#appl'(XLc, XOp, 'lo.comp.ast#tupl'(XLc, "()", XEls))) :- !.
'lo.comp.abstract@roundTerm'(_, _, _, _) :- raise_exception('error'("roundTerm", 109, 3, 53)).
'lo.comp.abstract@deComma'(XT, 'lo.core#,..'(XL, XX11494)) :- 'lo.comp.abstract@isBinary'(XT, ",", X_762, XL, XR),
    !,
    'lo.comp.abstract@deComma'(XR, XX11494).
'lo.comp.abstract@deComma'(XT, 'lo.core#,..'(XT, 'lo.core#[]')) :- !.
'lo.comp.abstract@deComma'(_, _) :- raise_exception('error'("deComma", 117, 3, 56)).
'lo.comp.abstract@sameTerms'('lo.core#[]', 'lo.core#[]').
'lo.comp.abstract@sameTerms'('lo.core#,..'(XA, XL1), 'lo.core#,..'(XB, XL2)) :- 'lo.comp.abstract@sameTerm'(XA, XB),
    'lo.comp.abstract@sameTerms'(XL1, XL2).
'lo.comp.abstract@sameTerm'('lo.comp.ast#iden'(X_763, XNm), 'lo.comp.ast#iden'(X_764, XNm)).
'lo.comp.abstract@sameTerm'('lo.comp.ast#intg'(X_765, XIx), 'lo.comp.ast#intg'(X_766, XIx)).
'lo.comp.abstract@sameTerm'('lo.comp.ast#flot'(X_767, XDx), 'lo.comp.ast#flot'(X_768, XDx)).
'lo.comp.abstract@sameTerm'('lo.comp.ast#strg'(X_769, XS), 'lo.comp.ast#strg'(X_770, XS)).
'lo.comp.abstract@sameTerm'('lo.comp.ast#tupl'(X_771, XT, XA), 'lo.comp.ast#tupl'(X_772, XT, XB)) :- 'lo.comp.abstract@sameTerms'(XA, XB).
'lo.comp.abstract@sameTerm'('lo.comp.ast#appl'(X_773, XOA, XAA), 'lo.comp.ast#appl'(X_774, XOB, XBA)) :- 'lo.comp.abstract@sameTerm'(XOA, XOB),
    'lo.comp.abstract@sameTerm'(XAA, XBA).
'lo.comp.abstract@isQuantified'(XT, XV, XB) :- 'lo.comp.abstract@isUnary'(XT, "all", X_775, XR),
    'lo.comp.abstract@isBinary'(XR, "~~", X_776, XV, XB).
'lo.comp.abstract@getQuantifiers'(XT, XX11570, XB) :- 'lo.comp.abstract@isQuantified'(XT, XV, XB),
    'lo.comp.abstract@deComma'(XV, XX11570).
'lo.comp.abstract@getQuantifiers'(XT, 'lo.core#[]', XT).
'lo.comp.abstract@getConstraints'(XT, XX11580, XR) :- 'lo.comp.abstract@isBinary'(XT, "|:", X_777, XL, XR),
    'lo.comp.abstract@deComma'(XL, XX11580).
'lo.comp.abstract@getConstraints'(XT, 'lo.core#[]', XT).
'lo.comp.abstract@isAlgebraicTypeDef'(XStmt, XLc, XQuants, XConstraints, XHead, XBody) :- 'lo.comp.abstract@isUnary'(XStmt, "type", XLc, XTerm),
    'lo.comp.abstract@getQuantifiers'(XTerm, XQuants, XInner),
    'lo.comp.abstract@getConstraints'(XInner, XConstraints, XTpStmt),
    'lo.comp.abstract@isBinary'(XTpStmt, "::=", X_778, XHead, XBody).
'lo.comp.abstract@contractSpec'(XS, XX11610, XConstraints, XCon) :- 'lo.comp.abstract@isQuantified'(XS, XV, XB),
    'lo.comp.abstract@contractSpec'(XB, X_779, XConstraints, XCon),
    'lo.comp.abstract@deComma'(XV, XX11610).
'lo.comp.abstract@contractSpec'(XS, 'lo.core#[]', XX11623, XCon) :- 'lo.comp.abstract@isBinary'(XS, "|:", X_780, XL, XCon),
    'lo.comp.abstract@deComma'(XL, XX11623).
'lo.comp.abstract@contractSpec'(XS, 'lo.core#[]', 'lo.core#[]', XS).
'lo.comp.abstract@isContractSpec'(XSt, XLc, XQuants, XConstraints, XCon, XBody) :- 'lo.comp.abstract@isUnary'(XSt, "contract", XLc, XI),
    'lo.comp.abstract@isBinary'(XI, "<~", X_781, XL, XR),
    'lo.comp.abstract@isBraceTuple'(XR, X_782, XBody),
    'lo.comp.abstract@contractSpec'(XL, XQuants, XConstraints, XCon).
'lo.comp.abstract@isConstrained'(XT, XX11655, XR) :- 'lo.comp.abstract@isBinary'(XT, "|:", X_783, XL, XR),
    'lo.comp.abstract@deComma'(XL, XX11655).
'lo.comp.abstract@packageName'(XT, XPkg) :- 'lo.comp.abstract@isIden'(XT, X_784, XPkg),
    !.
'lo.comp.abstract@packageName'(XT, XPkg) :- 'lo.comp.abstract@isString'(XT, X_785, XPkg),
    !.
'lo.comp.abstract@packageName'(XT, XX11682) :- 'lo.comp.abstract@isBinary'(XT, ".", X_786, XL, XR),
    !,
    'lo.comp.abstract@packageName'(XL, XX11677),
    ocall('+%3'(XX11677, ".", XX11678),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    'lo.comp.abstract@packageName'(XR, XX11681),
    ocall('+%3'(XX11678, XX11681, XX11682),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string').
'lo.comp.abstract@packageName'(_, _) :- raise_exception('error'("packageName", 180, 3, 42)).
'lo.comp.abstract@packageVersion'(XT, XPkg) :- 'lo.comp.abstract@isIden'(XT, X_787, XPkg),
    !.
'lo.comp.abstract@packageVersion'(XT, XPkg) :- 'lo.comp.abstract@isString'(XT, X_788, XPkg),
    !.
'lo.comp.abstract@packageVersion'(XT, XX11704) :- 'lo.comp.abstract@isInteger'(XT, X_789, XIx),
    !,
    ocall('disp%2'(XIx, XX11699),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XX11699, 'lo.core#[]')), XX11704).
'lo.comp.abstract@packageVersion'(XT, XX11716) :- 'lo.comp.abstract@isBinary'(XT, ".", X_790, XL, XR),
    !,
    'lo.comp.abstract@packageVersion'(XL, XX11711),
    ocall('+%3'(XX11711, ".", XX11712),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    'lo.comp.abstract@packageVersion'(XR, XX11715),
    ocall('+%3'(XX11712, XX11715, XX11716),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string').
'lo.comp.abstract@packageVersion'(_, _) :- raise_exception('error'("packageVersion", 191, 3, 45)).
'lo.comp.abstract@pkgName'(XT, 'lo.repo#pkg'(XX11724, 'lo.repo#vers'(XX11726))) :- 'lo.comp.abstract@isBinary'(XT, "#", X_791, XL, XR),
    !,
    'lo.comp.abstract@packageName'(XL, XX11724),
    'lo.comp.abstract@packageVersion'(XR, XX11726).
'lo.comp.abstract@pkgName'(XT, 'lo.repo#pkg'(XX11731, 'lo.repo#defltVersion')) :- !,
    'lo.comp.abstract@packageName'(XT, XX11731).
'lo.comp.abstract@pkgName'(_, _) :- raise_exception('error'("pkgName", 186, 3, 87)).
'lo.comp.abstract@tupleize'(XP, XOp, 'lo.comp.ast#tupl'(XLc, XOp, 'lo.core#,..'(XL, XX11740))) :- 'lo.comp.abstract@isBinary'(XP, ",", XLc, XL, XR),
    'lo.comp.abstract@deComma'(XR, XX11740).
'lo.comp.abstract@tupleize'(XP, XOp, 'lo.comp.ast#tupl'(XXV25, XOp, 'lo.core#,..'(XP, 'lo.core#[]'))) :- ocall('loc%1'(XXV25),XP,XP).
'lo.comp.abstract@tpe'('lo.comp.abstract#tpe') :- !.
'lo.comp.abstract@con'('lo.comp.abstract#con') :- !.
'lo.comp.abstract@impl'('lo.comp.abstract#impl') :- !.
'lo.comp.abstract@valu'('lo.comp.abstract#valu') :- !.
'lo.comp.abstract@clss'('lo.comp.abstract#clss') :- !.
'lo.comp.abstract@imp'('lo.comp.abstract#imp') :- !.
'lo.comp.abstract^marker'('_call%2'(XV1832, XV1833), 'lo.comp.abstract^marker', _) :- 'lo.comp.abstract@marker'(XV1832, XV1833).
'lo.comp.abstract^isScalar'('_call%1'(XV1834), 'lo.comp.abstract^isScalar', _) :- 'lo.comp.abstract@isScalar'(XV1834).
'lo.comp.abstract^isIden'('_call%3'(XV1835, XV1836, XV1837), 'lo.comp.abstract^isIden', _) :- 'lo.comp.abstract@isIden'(XV1835, XV1836, XV1837).
'lo.comp.abstract^genIden'('_call%2'(XV1838, XV1839), 'lo.comp.abstract^genIden', _) :- 'lo.comp.abstract@genIden'(XV1838, XV1839).
'lo.comp.abstract^isString'('_call%3'(XV1840, XV1841, XV1842), 'lo.comp.abstract^isString', _) :- 'lo.comp.abstract@isString'(XV1840, XV1841, XV1842).
'lo.comp.abstract^isInteger'('_call%3'(XV1843, XV1844, XV1845), 'lo.comp.abstract^isInteger', _) :- 'lo.comp.abstract@isInteger'(XV1843, XV1844, XV1845).
'lo.comp.abstract^isBinary'('_call%5'(XV1846, XV1847, XV1848, XV1849, XV1850), 'lo.comp.abstract^isBinary', _) :- 'lo.comp.abstract@isBinary'(XV1846, XV1847, XV1848, XV1849, XV1850).
'lo.comp.abstract^binary'('_call%5'(XV1851, XV1852, XV1853, XV1854, XV1855), 'lo.comp.abstract^binary', _) :- 'lo.comp.abstract@binary'(XV1851, XV1852, XV1853, XV1854, XV1855).
'lo.comp.abstract^isUnary'('_call%4'(XV1856, XV1857, XV1858, XV1859), 'lo.comp.abstract^isUnary', _) :- 'lo.comp.abstract@isUnary'(XV1856, XV1857, XV1858, XV1859).
'lo.comp.abstract^unary'('_call%4'(XV1860, XV1861, XV1862, XV1863), 'lo.comp.abstract^unary', _) :- 'lo.comp.abstract@unary'(XV1860, XV1861, XV1862, XV1863).
'lo.comp.abstract^isTernary'('_call%6'(XV1864, XV1865, XV1866, XV1867, XV1868, XV1869), 'lo.comp.abstract^isTernary', _) :- 'lo.comp.abstract@isTernary'(XV1864, XV1865, XV1866, XV1867, XV1868, XV1869).
'lo.comp.abstract^ternary'('_call%6'(XV1870, XV1871, XV1872, XV1873, XV1874, XV1875), 'lo.comp.abstract^ternary', _) :- 'lo.comp.abstract@ternary'(XV1870, XV1871, XV1872, XV1873, XV1874, XV1875).
'lo.comp.abstract^isSquareTerm'('_call%4'(XV1876, XV1877, XV1878, XV1879), 'lo.comp.abstract^isSquareTerm', _) :- 'lo.comp.abstract@isSquareTerm'(XV1876, XV1877, XV1878, XV1879).
'lo.comp.abstract^squareTerm'('_call%4'(XV1880, XV1881, XV1882, XV1883), 'lo.comp.abstract^squareTerm', _) :- 'lo.comp.abstract@squareTerm'(XV1880, XV1881, XV1882, XV1883).
'lo.comp.abstract^isBraceTerm'('_call%4'(XV1884, XV1885, XV1886, XV1887), 'lo.comp.abstract^isBraceTerm', _) :- 'lo.comp.abstract@isBraceTerm'(XV1884, XV1885, XV1886, XV1887).
'lo.comp.abstract^braceTerm'('_call%4'(XV1888, XV1889, XV1890, XV1891), 'lo.comp.abstract^braceTerm', _) :- 'lo.comp.abstract@braceTerm'(XV1888, XV1889, XV1890, XV1891).
'lo.comp.abstract^isRoundTuple'('_call%3'(XV1892, XV1893, XV1894), 'lo.comp.abstract^isRoundTuple', _) :- 'lo.comp.abstract@isRoundTuple'(XV1892, XV1893, XV1894).
'lo.comp.abstract^roundTuple'('_call%3'(XV1895, XV1896, XV1897), 'lo.comp.abstract^roundTuple', _) :- 'lo.comp.abstract@roundTuple'(XV1895, XV1896, XV1897).
'lo.comp.abstract^isSquareTuple'('_call%3'(XV1898, XV1899, XV1900), 'lo.comp.abstract^isSquareTuple', _) :- 'lo.comp.abstract@isSquareTuple'(XV1898, XV1899, XV1900).
'lo.comp.abstract^isBraceTuple'('_call%3'(XV1901, XV1902, XV1903), 'lo.comp.abstract^isBraceTuple', _) :- 'lo.comp.abstract@isBraceTuple'(XV1901, XV1902, XV1903).
'lo.comp.abstract^braceTuple'('_call%3'(XV1904, XV1905, XV1906), 'lo.comp.abstract^braceTuple', _) :- 'lo.comp.abstract@braceTuple'(XV1904, XV1905, XV1906).
'lo.comp.abstract^mapTuple'('_call%3'(XV1907, XV1908, XV1909), 'lo.comp.abstract^mapTuple', _) :- 'lo.comp.abstract@mapTuple'(XV1907, XV1908, XV1909).
'lo.comp.abstract^keyword'('_call%1'(XV1910), 'lo.comp.abstract^keyword', _) :- 'lo.comp.abstract@keyword'(XV1910).
'lo.comp.abstract@neg22'(XOp) :- 'lo.comp.abstract@keyword'(XOp),
    !,
    fail.
'lo.comp.abstract@neg22'(XOp).
'lo.comp.abstract^isRoundTerm'('_call%4'(XV1911, XV1912, XV1913, XV1914), 'lo.comp.abstract^isRoundTerm', _) :- 'lo.comp.abstract@isRoundTerm'(XV1911, XV1912, XV1913, XV1914).
'lo.comp.abstract@neg23'(XOp) :- 'lo.comp.abstract@keyword'(XOp),
    !,
    fail.
'lo.comp.abstract@neg23'(XOp).
'lo.comp.abstract^isRound'('_call%4'(XV1915, XV1916, XV1917, XV1918), 'lo.comp.abstract^isRound', _) :- 'lo.comp.abstract@isRound'(XV1915, XV1916, XV1917, XV1918).
'lo.comp.abstract^roundTerm'('_call%4'(XV1919, XV1920, XV1921, XV1922), 'lo.comp.abstract^roundTerm', _) :- 'lo.comp.abstract@roundTerm'(XV1919, XV1920, XV1921, XV1922).
'lo.comp.abstract^deComma'('_call%2'(XV1923, XV1924), 'lo.comp.abstract^deComma', _) :- 'lo.comp.abstract@deComma'(XV1923, XV1924).
'lo.comp.abstract^sameTerms'('_call%2'(XV1925, XV1926), 'lo.comp.abstract^sameTerms', _) :- 'lo.comp.abstract@sameTerms'(XV1925, XV1926).
'lo.comp.abstract^sameTerm'('_call%2'(XV1927, XV1928), 'lo.comp.abstract^sameTerm', _) :- 'lo.comp.abstract@sameTerm'(XV1927, XV1928).
'lo.comp.abstract^isQuantified'('_call%3'(XV1929, XV1930, XV1931), 'lo.comp.abstract^isQuantified', _) :- 'lo.comp.abstract@isQuantified'(XV1929, XV1930, XV1931).
'lo.comp.abstract^getQuantifiers'('_call%3'(XV1932, XV1933, XV1934), 'lo.comp.abstract^getQuantifiers', _) :- 'lo.comp.abstract@getQuantifiers'(XV1932, XV1933, XV1934).
'lo.comp.abstract^getConstraints'('_call%3'(XV1935, XV1936, XV1937), 'lo.comp.abstract^getConstraints', _) :- 'lo.comp.abstract@getConstraints'(XV1935, XV1936, XV1937).
'lo.comp.abstract^isAlgebraicTypeDef'('_call%6'(XV1938, XV1939, XV1940, XV1941, XV1942, XV1943), 'lo.comp.abstract^isAlgebraicTypeDef', _) :- 'lo.comp.abstract@isAlgebraicTypeDef'(XV1938, XV1939, XV1940, XV1941, XV1942, XV1943).
'lo.comp.abstract^contractSpec'('_call%4'(XV1944, XV1945, XV1946, XV1947), 'lo.comp.abstract^contractSpec', _) :- 'lo.comp.abstract@contractSpec'(XV1944, XV1945, XV1946, XV1947).
'lo.comp.abstract^isContractSpec'('_call%6'(XV1948, XV1949, XV1950, XV1951, XV1952, XV1953), 'lo.comp.abstract^isContractSpec', _) :- 'lo.comp.abstract@isContractSpec'(XV1948, XV1949, XV1950, XV1951, XV1952, XV1953).
'lo.comp.abstract^isConstrained'('_call%3'(XV1954, XV1955, XV1956), 'lo.comp.abstract^isConstrained', _) :- 'lo.comp.abstract@isConstrained'(XV1954, XV1955, XV1956).
'lo.comp.abstract^packageName'('_call%2'(XV1957, XV1958), 'lo.comp.abstract^packageName', _) :- 'lo.comp.abstract@packageName'(XV1957, XV1958).
'lo.comp.abstract^packageVersion'('_call%2'(XV1959, XV1960), 'lo.comp.abstract^packageVersion', _) :- 'lo.comp.abstract@packageVersion'(XV1959, XV1960).
'lo.comp.abstract^pkgName'('_call%2'(XV1961, XV1962), 'lo.comp.abstract^pkgName', _) :- 'lo.comp.abstract@pkgName'(XV1961, XV1962).
'lo.comp.abstract^tupleize'('_call%3'(XV1963, XV1964, XV1965), 'lo.comp.abstract^tupleize', _) :- 'lo.comp.abstract@tupleize'(XV1963, XV1964, XV1965).
