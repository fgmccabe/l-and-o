'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.typecheck'e'*'n29o29'()29'n2o2'import'e'private'n2o2'pkg's'lo.comp.resolve'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.imports'e'*'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.misc'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.macro'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.dict'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.dependencies'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.unify'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.freshen'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.parseType'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.canon'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.keywords'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.abstract'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.ast'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I1'checkProgram':k'r'|FT5t'lo.comp.ast*ast't'lo.repo*version'k'r't'lo.comp.errors*report't'lo.comp.errors*report't'lo.comp.canon*canonPkg'c'lo.repo$repository'T1k'r'T0\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.typecheck@init'() :- !.
'lo.comp.typecheck@findImport'(XSt, X_2031, XX30892) :- 'lo.comp.abstract@isUnary'(XSt, "private", X_2032, XI),
    !,
    'lo.comp.typecheck@findImport'(XI, 'lo.comp.package#priVate', XX30892).
'lo.comp.typecheck@findImport'(XSt, X_2033, XX30900) :- 'lo.comp.abstract@isUnary'(XSt, "public", X_2034, XI),
    !,
    'lo.comp.typecheck@findImport'(XI, 'lo.comp.package#pUblic', XX30900).
'lo.comp.typecheck@findImport'(XSt, XViz, (XViz, XX30908)) :- 'lo.comp.abstract@isUnary'(XSt, "import", X_2035, XP),
    !,
    'lo.comp.abstract@pkgName'(XP, XX30908).
'lo.comp.typecheck@findImport'(_, _, _) :- raise_exception('error'("findImport", 83, 3, 74)).
'lo.comp.typecheck@findAllImports'('lo.core#[]', 'lo.core#[]').
'lo.comp.typecheck@findAllImports'('lo.core#,..'(XSt, XMore), 'lo.core#,..'(XX30916, XImports)) :- 'lo.comp.typecheck@findAllImports'(XMore, XImports),
    'lo.comp.typecheck@findImport'(XSt, 'lo.comp.package#priVate', XX30916).
'lo.comp.typecheck@second'((Xa, Xb), Xb) :- !.
'lo.comp.typecheck@second'(_, _) :- raise_exception('error'("second", 91, 3, 18)).
'lo.comp.typecheck@formMethods'('lo.core#[]', X_2036, X_2037, X_2038, XEnv, XEnv).
'lo.comp.typecheck@formMethods'('lo.core#,..'((XNm, XTp), XM), XLc, XQ, XCon, XEnv, XEv) :- 'lo.comp.types@moveQuants'(XTp, XFQ, XQTp),
    'lo.list@merge'(XFQ, XQ, XX30945),
    'lo.comp.types@moveQuants'(XMTp, XX30945, 'lo.comp.types#constrained'(XQTp, XCon)),
    'lo.comp.dict@declareVar'(XNm, 'lo.comp.dict#vr'('lo.comp.canon#mtd'(XLc, XNm), XMTp), XEnv, XX30960),
    'lo.comp.typecheck@formMethods'(XM, XLc, XQ, XCon, XX30960, XEv).
'lo.comp.typecheck@declareMethods'(XSpec, XMtdsTp, XLc, XEnv, XEv) :- 'lo.comp.types@moveConQuants'(XSpec, XQ, XCon),
    'lo.comp.types@moveQuants'(XMtdsTp, X_2039, 'lo.comp.types#faceType'(XMethods)),
    'lo.comp.typecheck@formMethods'(XMethods, XLc, XQ, XCon, XEnv, XEv).
'lo.comp.typecheck@defineContract'(XLc, 'lo.comp.types#conEntry'(XN, XNm, XSpec, XFace), XE0, XEx) :- 'lo.comp.dict@declareContract'(XN, 'lo.comp.types#conEntry'(XN, XNm, XSpec, XFace), XE0, XX30998),
    'lo.comp.typecheck@declareMethods'(XSpec, XFace, XLc, XX30998, XEx).
'lo.comp.typecheck@importContracts'('lo.core#[]', X_2040, XEnv, XEnv) :- !.
'lo.comp.typecheck@importContracts'('lo.core#,..'(XC, XL), XLc, XE, XX31016) :- 'lo.comp.typecheck@defineContract'(XLc, XC, XE, XE1),
    !,
    'lo.comp.typecheck@importContracts'(XL, XLc, XE1, XX31016).
'lo.comp.typecheck@importContracts'(_, _, _, _) :- raise_exception('error'("importContracts", 127, 3, 30)).
'lo.comp.typecheck@pickTypeTemplate'('lo.comp.types#univType'(XB, XTp), XX31021) :- !,
    'lo.comp.typecheck@pickTypeTemplate'(XTp, XX31021).
'lo.comp.typecheck@pickTypeTemplate'('lo.comp.types#typeRule'(XLhs, X_2041), XX31026) :- !,
    'lo.comp.typecheck@pickTypeTemplate'(XLhs, XX31026).
'lo.comp.typecheck@pickTypeTemplate'('lo.comp.types#constrained'(XTp, X_2042), XX31031) :- !,
    'lo.comp.typecheck@pickTypeTemplate'(XTp, XX31031).
'lo.comp.typecheck@pickTypeTemplate'('lo.comp.types#tipe'(XNm), 'lo.comp.types#tipe'(XNm)) :- !.
'lo.comp.typecheck@pickTypeTemplate'('lo.comp.types#typeExp'(XOp, X_2043), XOp) :- !.
'lo.comp.typecheck@pickTypeTemplate'(_, _) :- raise_exception('error'("pickTypeTemplate", 120, 3, 56)).
'lo.comp.typecheck@importTypes'('lo.core#[]', X_2044, XEnv, XEnv) :- !.
'lo.comp.typecheck@importTypes'('lo.core#,..'((XNm, XRule), XMore), XLc, XEnv, XX31060) :- !,
    'lo.comp.typecheck@pickTypeTemplate'(XRule, XX31055),
    'lo.comp.dict@declareType'(XNm, 'lo.comp.dict#tpDef'(XLc, XX31055, XRule), XEnv, XX31059),
    'lo.comp.typecheck@importTypes'(XMore, XLc, XX31059, XX31060).
'lo.comp.typecheck@importTypes'(_, _, _, _) :- raise_exception('error'("importTypes", 116, 3, 26)).
'lo.comp.typecheck@declareFields'('lo.core#[]', X_2045, XEnv, XEnv) :- !.
'lo.comp.typecheck@declareFields'('lo.core#,..'((XNm, XTp), XMore), XLc, XEnv, XX31081) :- !,
    'lo.comp.dict@declareVar'(XNm, 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, XNm), XTp), XEnv, XX31080),
    'lo.comp.typecheck@declareFields'(XMore, XLc, XX31080, XX31081).
'lo.comp.typecheck@declareFields'(_, _, _, _) :- raise_exception('error'("declareFields", 111, 3, 30)).
'lo.comp.typecheck@importDefs'('lo.comp.package#pkgSpec'(X_2046, XExported, XTypes, X_2047, XCons, X_2048, X_2049), XLc, XEnv, XEx) :- 'lo.comp.typecheck@declareFields'(XExported, XLc, XEnv, XX31101),
    'lo.comp.typecheck@importTypes'(XTypes, XLc, XX31101, XX31102),
    'lo.comp.typecheck@importContracts'(XCons, XLc, XX31102, XX31103),
    XEx = XX31103.
'lo.comp.typecheck@importAllDefs'('lo.core#[]', X_2050, XEnv, XEnv).
'lo.comp.typecheck@importAllDefs'('lo.core#,..'(XSpec, XSpecs), XLc, XEnv, XEx) :- 'lo.comp.typecheck@importDefs'(XSpec, XLc, XEnv, XEv0),
    'lo.comp.typecheck@importAllDefs'(XSpecs, XLc, XEv0, XEx).
'lo.comp.typecheck@processImportGroup'(Xlo_repo_repository_r4, XStmts, XImports, XImportClosure, XLc, XRepo, XEnv, XEx, XRp, XRpx) :- 'lo.comp.typecheck@findAllImports'(XStmts, XImports),
    ocall('//%3'(XImports, 'lo.comp.typecheck^second', XX31137),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    XImportedPkgs = XX31137,
    'lo.comp.imports@importClosure'(Xlo_repo_repository_r4, XImportedPkgs, 'lo.core#[]', XRepo, XRp, XRpx, XX31146),
    XImportClosure = XX31146,
    'lo.comp.typecheck@importAllDefs'(XImportClosure, XLc, XEnv, XEx).
'lo.comp.typecheck@defineType'(XN, XLc, XSt, XEnv, XEx, XPath, XRp, XRpx) :- 'lo.comp.typecheck@cond22'(XX31196, XPath, XTpCore, XSt, XEx, XRpx, XRp, XLc, XX31177, XX31169, XX31165, X_2051, XOLc, XEnv, XN).
'lo.comp.typecheck@defineTypes'('lo.core#[]', XEnv, XEnv, X_2052, XRp, XRp).
'lo.comp.typecheck@defineTypes'('lo.core#,..'('lo.comp.dependencies#defn'(XN, 'lo.comp.abstract#tpe', XLc, X_2053, 'lo.core#,..'(XStmt, 'lo.core#[]')), XMore), XEnv, XEx, XPath, XRp, XRpx) :- 'lo.comp.typecheck@defineType'(XN, XLc, XStmt, XEnv, XE0, XPath, XRp, XRp0),
    'lo.comp.typecheck@defineTypes'(XMore, XE0, XEx, XPath, XRp0, XRpx).
'lo.comp.typecheck@defineTypes'('lo.core#,..'('lo.comp.dependencies#defn'(XN, 'lo.comp.abstract#tpe', XLc, X_2054, 'lo.core#,..'(X_2055, X_2056)), XMore), XEnv, XEx, XPath, XRp, XRpx) :- ocall('disp%2'(XN, XX31249),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("multiple type definition statements for "), 'lo.core#,..'(XX31249, 'lo.core#[]'))), XX31255),
    'lo.comp.errors@reportError'(XX31255, XLc, XRp, XRp0),
    'lo.comp.typecheck@defineTypes'(XMore, XEnv, XEx, XPath, XRp0, XRpx).
'lo.comp.typecheck@parseTypeDefinition'(XN, XLc, XSt, 'lo.core#,..'('lo.comp.canon#typeDef'(XLc, XN, XType, XFaceRule), XDefs), XDefs, XEnv, XPath, XRp, XRpx) :- 'lo.comp.abstract@isUnary'(XSt, "type", X_2057, XInSt),
    ocall('_empty%1'(XXV59),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.parseType@parseTypeRule'(XInSt, XXV59, XEnv, XFaceRule, XPath, XRp, XRpx),
    'lo.comp.dict@isType'(XN, XEnv, X_2058, XType).
'lo.comp.typecheck@parseTypeDefs'('lo.core#[]', XDefs, XDefs, X_2059, X_2060, XRp, XRp).
'lo.comp.typecheck@parseTypeDefs'('lo.core#,..'('lo.comp.dependencies#defn'(XN, 'lo.comp.abstract#tpe', XLc, X_2061, 'lo.core#,..'(XStmt, 'lo.core#[]')), XMore), XDefs, XDx, XTmpEnv, XPath, XRp, XRpx) :- 'lo.comp.typecheck@parseTypeDefinition'(XN, XLc, XStmt, XDefs, XD0, XTmpEnv, XPath, XRp, XRp0),
    'lo.comp.typecheck@parseTypeDefs'(XMore, XD0, XDx, XTmpEnv, XPath, XRp0, XRpx).
'lo.comp.typecheck@declareTypes'('lo.core#[]', XDefs, XDefs, XEnv, XEnv).
'lo.comp.typecheck@declareTypes'('lo.core#,..'('lo.comp.canon#typeDef'(XLc, XN, XType, XFaceRule), XMore), 'lo.core#,..'('lo.comp.canon#typeDef'(XLc, XN, XType, XFaceRule), XDefs), XDx, XEnv, XEx) :- 'lo.comp.dict@declareType'(XN, 'lo.comp.dict#tpDef'(XLc, XType, XFaceRule), XEnv, XX31365),
    'lo.comp.typecheck@declareTypes'(XMore, XDefs, XDx, XX31365, XEx).
'lo.comp.typecheck@typeGroup'(XGrp, XDefs, XDx, XEnv, XEx, XPath, XRp, XRpx) :- 'lo.comp.typecheck@defineTypes'(XGrp, XEnv, XTmpEnv, XPath, XRp, XRp0),
    'lo.comp.typecheck@parseTypeDefs'(XGrp, XTpDefs, 'lo.core#[]', XTmpEnv, XPath, XRp0, XRpx),
    'lo.comp.typecheck@declareTypes'(XTpDefs, XDefs, XDx, XEnv, XEx).
'lo.comp.typecheck@parseAnnotations'('lo.core#[]', X_2062, X_2063, XEnv, XEnv, X_2064, XRp, XRp).
'lo.comp.typecheck@parseAnnotations'('lo.core#,..'('lo.comp.dependencies#defn'(XNm, 'lo.comp.abstract#valu', X_2065, X_2066, X_2067), XMore), XFields, XAnnots, XEnv, XEx, XPath, XRp, XRpx) :- 'lo.comp.typecheck@one36'(XAnnots, XAnnot, XNm),
    'lo.comp.abstract@isBinary'(XAnnot, ":", XLc, X_2068, XT),
    'lo.comp.parseType@parseType'(XT, XEnv, XTp, XRp, XRp0),
    'lo.comp.dict@declareVar'(XNm, 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, XNm), XTp), XEnv, XX31439),
    'lo.comp.typecheck@parseAnnotations'(XMore, XFields, XAnnots, XX31439, XEx, XPath, XRp0, XRpx).
'lo.comp.typecheck@parseAnnotations'('lo.core#,..'('lo.comp.dependencies#defn'(XN, 'lo.comp.abstract#valu', XLc, X_2069, X_2070), XMore), XFields, XAnnots, XEnv, XEx, XPath, XRp, XRpx) :- 'lo.comp.typecheck@one37'(XFields, XTp, XN),
    'lo.comp.dict@declareVar'(XN, 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, XN), XTp), XEnv, XX31473),
    'lo.comp.typecheck@parseAnnotations'(XMore, XFields, XAnnots, XX31473, XEx, XPath, XRp, XRpx).
'lo.comp.typecheck@parseAnnotations'('lo.core#,..'('lo.comp.dependencies#defn'(XN, 'lo.comp.abstract#valu', XLc, X_2071, X_2072), XMore), XFields, XAnnots, XEnv, XEx, XPath, XRp, XRpx) :- ocall('disp%2'(XN, XX31495),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("no type annotation for variable "), 'lo.core#,..'(XX31495, 'lo.core#[]'))), XX31501),
    'lo.comp.errors@reportError'(XX31501, XLc, XRp, XRp0),
    'lo.comp.typecheck@parseAnnotations'(XMore, XFields, XAnnots, XEnv, XEx, XPath, XRp0, XRpx).
'lo.comp.typecheck@pickupVarType'(XN, X_2073, XEnv, XRp, XRp, XTp) :- 'lo.comp.dict@isVar'(XN, XEnv, 'lo.comp.dict#vr'(X_2074, XTp)),
    !.
'lo.comp.typecheck@pickupVarType'(XN, XLc, X_2075, XRp, XRpx, XX31541) :- ocall('disp%2'(XN, XX31530),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XX31530, 'lo.core#,..'('lo.core#ss'(" not declared"), 'lo.core#[]'))), XX31537),
    'lo.comp.errors@reportError'(XX31537, XLc, XRp, XRpx),
    !,
    'lo.comp.types@newVar'("_", XX31541).
'lo.comp.typecheck@pickupVarType'(_, _, _, _, _, _) :- raise_exception('error'("pickupVarType", 245, 3, 61)).
'lo.comp.typecheck@pickupThisType'(XEnv, XX31550) :- 'lo.comp.dict@isVar'("this", XEnv, 'lo.comp.dict#vr'(X_2076, XThisType)),
    !,
    ocall('_empty%1'(XXV60),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_put%4'(XXV60, "this", XThisType, XX31550),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.typecheck@pickupThisType'(X_2077, XXV61) :- !,
    ocall('_empty%1'(XXV61),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.typecheck@pickupThisType'(_, _) :- raise_exception('error'("pickupThisType", 905, 3, 80)).
'lo.comp.typecheck@declareTypeVars'('lo.core#[]', X_2078, XEnv, XEnv).
'lo.comp.typecheck@declareTypeVars'('lo.core#,..'(("this", X_2079), XVars), XLc, XEnv, XEx) :- 'lo.comp.typecheck@declareTypeVars'(XVars, XLc, XEnv, XEx).
'lo.comp.typecheck@declareTypeVars'('lo.core#,..'((XNm, XTp), XVars), XLc, XEnv, XEx) :- 'lo.comp.dict@declareTypeExists'(XNm, XLc, XTp, XEnv, XX31583),
    'lo.comp.typecheck@declareTypeVars'(XVars, XLc, XX31583, XEx).
'lo.comp.typecheck@declareConstraints'('lo.core#[]', XEnv, XEnv).
'lo.comp.typecheck@declareConstraints'('lo.core#,..'(XC, XL), XE, XEx) :- 'lo.comp.dict@declareConstraint'(XC, XE, XX31596),
    'lo.comp.typecheck@declareConstraints'(XL, XX31596, XEx).
'lo.comp.typecheck@splitHead'(XT, XLc, XNm, XA) :- 'lo.comp.abstract@isRound'(XT, XLc, XN, XA),
    'lo.comp.abstract@isIden'(XN, X_2080, XNm).
'lo.comp.typecheck@splitHead'(XT, XLc, XNm, XX31614) :- 'lo.comp.abstract@isIden'(XT, XLc, XNm),
    'lo.comp.abstract@roundTuple'(XLc, 'lo.core#[]', XX31614).
'lo.comp.typecheck@genTpVars'('lo.core#[]', 'lo.core#[]') :- !.
'lo.comp.typecheck@genTpVars'('lo.core#,..'(X_2081, XI), 'lo.core#,..'(XX31623, XX31625)) :- !,
    'lo.comp.types@newVar'("__", XX31623),
    'lo.comp.typecheck@genTpVars'(XI, XX31625).
'lo.comp.typecheck@genTpVars'(_, _) :- raise_exception('error'("genTpVars", 694, 3, 17)).
'lo.comp.typecheck@checkType'(XLc, XActual, XExpected, XEnv, XRp, XRpx) :- 'lo.comp.typecheck@cond23'(XLc, XX31650, XX31643, XX31639, XRpx, XRp, XEnv, XExpected, XActual).
'lo.comp.typecheck@manageConstraints'('lo.comp.types#constrained'(XT, XC), XCons, XLc, XV, XMTp, XExp) :- 'lo.comp.typecheck@manageConstraints'(XT, 'lo.core#,..'(XC, XCons), XLc, XV, XMTp, XExp).
'lo.comp.typecheck@manageConstraints'(XTp, 'lo.core#[]', X_2082, XV, XTp, XV).
'lo.comp.typecheck@manageConstraints'(XTp, XCons, XLc, XV, XTp, 'lo.comp.canon#over'(XLc, XV, XX31684)) :- 'lo.list@reverse'(XCons, XX31684).
'lo.comp.typecheck@typeOfVar'(XLc, 'lo.comp.dict#vr'(XV, XVT), XTp, XEnv, XTerm, XRp, XRpx) :- 'lo.comp.typecheck@pickupThisType'(XEnv, XX31699),
    ocall('freshen%3'(XVT, XX31699, XX31700),'lo.comp.freshen$freshen$lo.comp.types*tipe','lo.comp.freshen$freshen$lo.comp.types*tipe'),
    (X_2083, XFTp) = XX31700,
    'lo.comp.canon@relocate'(XV, XLc, XX31707),
    'lo.comp.typecheck@manageConstraints'(XFTp, 'lo.core#[]', XLc, XX31707, XMTp, XTerm),
    'lo.comp.typecheck@checkType'(XLc, XMTp, XTp, XEnv, XRp, XRpx).
'lo.comp.typecheck@findType'(XNm, X_2084, XEnv, XRp, XRp, XTp) :- 'lo.comp.dict@isType'(XNm, XEnv, X_2085, XTp),
    !.
'lo.comp.typecheck@findType'(XNm, XLc, X_2086, XRp, XRpx, 'lo.comp.types#anonType') :- ocall('disp%2'(XNm, XX31733),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("type "), 'lo.core#,..'(XX31733, 'lo.core#,..'('lo.core#ss'(" not known"), 'lo.core#[]')))), XX31741),
    'lo.comp.errors@reportError'(XX31741, XLc, XRp, XRpx),
    !.
'lo.comp.typecheck@findType'(_, _, _, _, _, _) :- raise_exception('error'("findType", 909, 3, 57)).
'lo.comp.typecheck@checkGrammarType'(XLc, XEnv, XTp, XElTp, XRp, XRpx) :- 'lo.comp.dict@isContract'("stream", XEnv, 'lo.comp.types#conEntry'(X_2087, X_2088, XSpec, X_2089)),
    'lo.comp.typecheck@pickupThisType'(XEnv, XX31769),
    ocall('freshen%3'(XSpec, XX31769, XX31770),'lo.comp.freshen$freshen$lo.comp.types*constraint','lo.comp.freshen$freshen$lo.comp.types*constraint'),
    (X_2090, 'lo.comp.types#conTract'(X_2091, 'lo.core#,..'(XArg, 'lo.core#[]'), 'lo.core#,..'(XDep, 'lo.core#[]'))) = XX31770,
    'lo.comp.typecheck@checkType'(XLc, XArg, XTp, XEnv, XRp, XRp0),
    'lo.comp.typecheck@checkType'(XLc, XDep, XElTp, XEnv, XRp0, XRpx).
'lo.comp.typecheck@makeIntList'('lo.core#[]', X_2092, 'lo.core#[]') :- !.
'lo.comp.typecheck@makeIntList'('lo.core#,..'(XCode, XL), XLc, 'lo.core#,..'('lo.comp.ast#intg'(XLc, XCode), XX31796)) :- !,
    'lo.comp.typecheck@makeIntList'(XL, XLc, XX31796).
'lo.comp.typecheck@makeIntList'(_, _, _) :- raise_exception('error'("makeIntList", 856, 3, 23)).
'lo.comp.typecheck@explodeStringLit'(XLc, XStr, XX31803) :- !,
    'explode'(XStr, XX31801),
    'lo.comp.typecheck@makeIntList'(XX31801, XLc, XX31803).
'lo.comp.typecheck@explodeStringLit'(_, _, _) :- raise_exception('error'("explodeStringLit", 853, 3, 56)).
'lo.comp.typecheck@streamVar'("stream_X") :- !.
'lo.comp.typecheck@fieldInFace'(XFields, X_2093, XNm, X_2094, XTp, XRp, XRp) :- 'lo.comp.typecheck@one38'(XFields, XTp, XNm).
'lo.comp.typecheck@fieldInFace'(X_2095, XTp, XNm, XLc, 'lo.comp.types#anonType', XRp, XRpx) :- ocall('disp%2'(XNm, XX31824),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('disp%2'(XTp, XX31828),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("field "), 'lo.core#,..'(XX31824, 'lo.core#,..'('lo.core#ss'(" not declared in "), 'lo.core#,..'(XX31828, 'lo.core#[]'))))), XX31836),
    'lo.comp.errors@reportError'(XX31836, XLc, XRp, XRpx).
'lo.comp.typecheck@isMapSequence'(XEls) :- ocall('in%2'(XE, XEls),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.abstract@isBinary'(XE, "->", X_2096, X_2097, X_2098).
'lo.comp.typecheck@isMapType'(XTp, XLc, XEnv) :- 'lo.comp.dict@isType'("map", XEnv, X_2099, XMpTp),
    'lo.comp.types@deRef'(XTp, XX31858),
    'lo.comp.types#typeExp'(XMpOp, X_2100) = XX31858,
    'lo.comp.types@deRef'(XMpOp, XX31860),
    XX31860 = XMpTp.
'lo.comp.typecheck@macroMapEntries'(XLc, 'lo.core#[]', XRp, XRp, 'lo.comp.ast#iden'(XLc, "_empty")) :- !.
'lo.comp.typecheck@macroMapEntries'(X_2101, 'lo.core#,..'(XE, XL), XRp, XRpx, XX31892) :- 'lo.comp.abstract@isBinary'(XE, "->", XLc, XKy, XVl),
    !,
    'lo.comp.typecheck@macroMapEntries'(XLc, XL, XRp, XRpx, XX31885),
    'lo.comp.abstract@roundTerm'(XLc, 'lo.comp.ast#iden'(XLc, "_put"), 'lo.core#,..'(XX31885, 'lo.core#,..'(XKy, 'lo.core#,..'(XVl, 'lo.core#[]'))), XX31892).
'lo.comp.typecheck@macroMapEntries'(XLc, 'lo.core#,..'(XE, XL), XRp, XRpx, XX31915) :- ocall('disp%2'(XE, XX31901),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("invalid entry in map "), 'lo.core#,..'(XX31901, 'lo.core#[]'))), XX31907),
    ocall('loc%1'(XXV62),XE,XE),
    'lo.comp.errors@reportError'(XX31907, XXV62, XRp, XRp0),
    !,
    'lo.comp.typecheck@macroMapEntries'(XLc, XL, XRp0, XRpx, XX31915).
'lo.comp.typecheck@macroMapEntries'(_, _, _, _, _) :- raise_exception('error'("macroMapEntries", 671, 3, 49)).
'lo.comp.typecheck@isListSequence'('lo.core#,..'(XE, X_2102)) :- 'lo.comp.typecheck@neg36'(X_2105, X_2104, X_2103, XE).
'lo.comp.typecheck@isListType'(XTp, XLc, XEnv) :- 'lo.comp.dict@isType'("list", XEnv, X_2106, XT),
    'lo.comp.types@deRef'(XTp, XX31933),
    'lo.comp.types#typeExp'(XLsOp, X_2107) = XX31933,
    'lo.comp.types@deRef'(XLsOp, XX31935),
    XX31935 = XLsTp.
'lo.comp.typecheck@macroSequenceTerm'('lo.core#[]', XLc, XV, 'lo.core#,..'(XX31942, 'lo.core#[]')) :- 'lo.comp.abstract@unary'(XLc, "_eof", XV, XX31942).
'lo.comp.typecheck@macroSequenceTerm'('lo.core#,..'(XE, XL), X_2108, XV, 'lo.core#,..'(XX31954, XM)) :- ocall('loc%1'(XXV63),XE,XE),
    'lo.comp.abstract@genIden'(XLc, XX31959),
    XNX = XX31959,
    'lo.comp.typecheck@macroSequenceTerm'(XL, XLc, XNX, XM),
    'lo.comp.abstract@ternary'(XXV63, "_hdtl", XV, XE, XNX, XX31954).
'lo.comp.typecheck@splitGrHead'(XTerm, XNm, XArgs, XHed) :- 'lo.comp.abstract@isBinary'(XTerm, ",", X_2109, XL, XR),
    'lo.comp.abstract@isSquareTuple'(XR, X_2110, XHed),
    'lo.comp.typecheck@splitHead'(XL, X_2111, XNm, XArgs).
'lo.comp.typecheck@splitGrHead'(XTerm, XNm, XArgs, XHed) :- 'lo.comp.abstract@isRoundTuple'(XTerm, X_2112, 'lo.core#,..'(XT, 'lo.core#[]')),
    'lo.comp.typecheck@splitGrHead'(XT, XNm, XArgs, XHed).
'lo.comp.typecheck@splitGrHead'(XTerm, XNm, XArgs, 'lo.core#[]') :- 'lo.comp.typecheck@splitHead'(XTerm, X_2113, XNm, XArgs).
'lo.comp.typecheck@isPublicVar'(XNm, X_2114, XPublic) :- ocall('in%2'((XNm, 'lo.comp.abstract#valu'), XPublic),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.typecheck@isPublicVar'(XNm, XFields, X_2115) :- ocall('in%2'((XNm, X_2116), XFields),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.typecheck@isPublicType'(XNm, XPublic) :- ocall('in%2'((XNm, 'lo.comp.abstract#tpe'), XPublic),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.typecheck@isPublicImplementation'(XNm, XPublic) :- ocall('in%2'((XNm, 'lo.comp.abstract#impl'), XPublic),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.typecheck@exportDef'('lo.comp.canon#funDef'(X_2117, XNm, XTp, X_2118, X_2119), XFields, XPublic, 'lo.core#,..'((XNm, XTp), XEx), XEx, XTypes, XTypes, XEnums, XEnums, XCons, XCons, XImpl, XImpl) :- 'lo.comp.typecheck@isPublicVar'(XNm, XFields, XPublic).
'lo.comp.typecheck@exportDef'('lo.comp.canon#relDef'(X_2120, XNm, XTp, X_2121, X_2122), XFields, XPublic, 'lo.core#,..'((XNm, XTp), XEx), XEx, XTypes, XTypes, XEnums, XEnums, XCons, XCons, XImpl, XImpl) :- 'lo.comp.typecheck@isPublicVar'(XNm, XFields, XPublic).
'lo.comp.typecheck@exportDef'('lo.comp.canon#classDef'(X_2123, XNm, XTp, X_2124, X_2125, X_2126), XFields, XPublic, 'lo.core#,..'((XNm, XTp), XEx), XEx, XTypes, XTypes, 'lo.core#,..'(XNm, XClx), XClx, XCons, XCons, XImpl, XImpl) :- 'lo.comp.typecheck@isPublicVar'(XNm, XFields, XPublic).
'lo.comp.typecheck@exportDef'('lo.comp.canon#typeDef'(X_2127, XNm, X_2128, XFaceRule), X_2129, XPublic, XExports, XExports, 'lo.core#,..'((XNm, XFaceRule), XTx), XTx, XEnums, XEnums, XCons, XCons, XImpl, XImpl) :- 'lo.comp.typecheck@isPublicType'(XNm, XPublic).
'lo.comp.typecheck@exportDef'('lo.comp.canon#varDef'(X_2130, XNm, XTp, X_2131, X_2132, X_2133), XFields, XPublic, 'lo.core#,..'((XNm, XTp), XEx), XEx, XTypes, XTypes, XEnums, XEnums, XCons, XCons, XImpl, XImpl) :- 'lo.comp.typecheck@isPublicVar'(XNm, XFields, XPublic).
'lo.comp.typecheck@exportDef'('lo.comp.canon#grammDef'(X_2134, XNm, XTp, X_2135, X_2136), XFields, XPublic, 'lo.core#,..'((XNm, XTp), XEx), XEx, XTypes, XTypes, XEnums, XEnums, XCons, XCons, XImpl, XImpl) :- 'lo.comp.typecheck@isPublicVar'(XNm, XFields, XPublic).
'lo.comp.typecheck@exportDef'('lo.comp.canon#cnDefn'(X_2137, XNm, XContract), X_2138, XPublic, XEx, XEx, XTypes, XTypes, XEnums, XEnums, 'lo.core#,..'(XContract, XCons), XCons, XImpl, XImpl) :- ocall('in%2'((XNm, 'lo.comp.abstract#con'), XPublic),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.typecheck@exportDef'('lo.comp.canon#implDef'(X_2139, XINm, XImplName, XSpec, X_2140, X_2141, X_2142, X_2143), X_2144, XPublic, XEx, XEx, XTps, XTps, XEnums, XEnums, XCons, XCons, 'lo.core#,..'('lo.comp.types#implEntry'(XImplName, XSpec), XIx), XIx) :- 'lo.comp.typecheck@one39'(XPublic, XINm).
'lo.comp.typecheck@exportDef'(X_2145, X_2146, X_2147, XEx, XEx, XTps, XTps, XEnums, XEnums, XCons, XCons, XImpls, XImpls).
'lo.comp.typecheck@computeExport'('lo.core#[]', X_2148, X_2149, 'lo.core#[]', 'lo.core#[]', 'lo.core#[]', 'lo.core#[]', 'lo.core#[]').
'lo.comp.typecheck@computeExport'('lo.core#,..'(XDef, XDefs), XFields, XPublic, XExports, XTypes, XEnums, XContracts, XImpls) :- 'lo.comp.typecheck@one40'(XIx, XImpls, XCx, XContracts, XClx, XEnums, XTx, XTypes, XEx, XExports, XPublic, XFields, XDef),
    'lo.comp.typecheck@computeExport'(XDefs, XFields, XPublic, XEx, XTx, XClx, XCx, XIx).
'lo.comp.typecheck@collectEquations'('lo.core#,..'(XEqn, XStmts), XSx, XNm, 'lo.core#,..'(XEqn, XEx)) :- XEqn = 'lo.comp.canon#equation'(X_2150, XNm, X_2151, X_2152, X_2153),
    'lo.comp.typecheck@collectEquations'(XStmts, XSx, XNm, XEx).
'lo.comp.typecheck@collectEquations'('lo.core#,..'(XRl, XStmts), 'lo.core#,..'(XRl, XSx), XNm, XEqns) :- 'lo.comp.typecheck@neg37'(X_2157, X_2156, X_2155, XNm, X_2154, XRl),
    'lo.comp.typecheck@collectEquations'(XStmts, XSx, XNm, XEqns).
'lo.comp.typecheck@collectEquations'('lo.core#[]', 'lo.core#[]', X_2158, 'lo.core#[]').
'lo.comp.typecheck@collectMoreClauses'('lo.core#,..'(XCl, XStmts), XSx, XNm, 'lo.core#,..'(XCl, XEx)) :- XCl = 'lo.comp.canon#clause'(X_2159, XNm, X_2160, X_2161),
    'lo.comp.typecheck@collectMoreClauses'(XStmts, XSx, XNm, XEx).
'lo.comp.typecheck@collectMoreClauses'('lo.core#,..'(XRl, XStmts), 'lo.core#,..'(XRl, XSx), XNm, XEqns) :- 'lo.comp.typecheck@neg38'(X_2164, X_2163, XNm, X_2162, XRl),
    'lo.comp.typecheck@collectMoreClauses'(XStmts, XSx, XNm, XEqns).
'lo.comp.typecheck@collectMoreClauses'('lo.core#[]', 'lo.core#[]', X_2165, 'lo.core#[]').
'lo.comp.typecheck@collectClauses'('lo.core#[]', 'lo.core#[]', X_2166, 'lo.core#[]').
'lo.comp.typecheck@collectClauses'('lo.core#,..'(XCl, XStmts), XSx, XNm, 'lo.core#,..'(XCl, XEx)) :- XCl = 'lo.comp.canon#clause'(X_2167, XNm, X_2168, X_2169),
    'lo.comp.typecheck@collectMoreClauses'(XStmts, XSx, XNm, XEx).
'lo.comp.typecheck@isRuleForClass'('lo.comp.canon#clRule'(XLc, XNm, X_2170, X_2171, X_2172, XFace), XLc, XNm, XFace).
'lo.comp.typecheck@collectClassRules'('lo.core#,..'(XCl, XStmts), XSx, XNm, 'lo.core#,..'(XCl, XEx)) :- 'lo.comp.typecheck@isRuleForClass'(XCl, X_2173, XNm, X_2174),
    'lo.comp.typecheck@collectClassRules'(XStmts, XSx, XNm, XEx).
'lo.comp.typecheck@collectClassRules'('lo.core#,..'(XRl, XStmts), 'lo.core#,..'(XRl, XSx), XNm, XEqns) :- 'lo.comp.typecheck@collectClassRules'(XStmts, XSx, XNm, XEqns).
'lo.comp.typecheck@collectClassRules'('lo.core#[]', 'lo.core#[]', X_2175, 'lo.core#[]').
'lo.comp.typecheck@isGrammarRule'('lo.comp.canon#grRule'(XLc, XNm, X_2176, X_2177, X_2178), XLc, XNm).
'lo.comp.typecheck@collectGrammarRules'('lo.core#,..'(XRl, XStmts), XSx, XNm, 'lo.core#,..'(XRl, XEx)) :- 'lo.comp.typecheck@isGrammarRule'(XRl, X_2179, XNm),
    'lo.comp.typecheck@collectGrammarRules'(XStmts, XSx, XNm, XEx).
'lo.comp.typecheck@collectGrammarRules'('lo.core#,..'(XRl, XStmts), 'lo.core#,..'(XRl, XSx), XNm, XEqns) :- 'lo.comp.typecheck@collectGrammarRules'(XStmts, XSx, XNm, XEqns).
'lo.comp.typecheck@collectGrammarRules'('lo.core#[]', 'lo.core#[]', X_2180, 'lo.core#[]').
'lo.comp.typecheck@collectPrograms'('lo.core#[]', X_2181, X_2182, XDefs, XDefs).
'lo.comp.typecheck@collectPrograms'('lo.core#,..'(XEqn, XStmts), XEnv, XCx, 'lo.core#,..'('lo.comp.canon#funDef'(XLc, XNm, XTp, XCx, 'lo.core#,..'(XEqn, XEqns)), XDefs), XDx) :- XEqn = 'lo.comp.canon#equation'(XLc, XNm, X_2183, X_2184, X_2185),
    'lo.comp.typecheck@collectEquations'(XStmts, XS0, XNm, XEqns),
    'lo.comp.dict@isVar'(XNm, XEnv, 'lo.comp.dict#vr'(X_2186, XTp)),
    'lo.comp.typecheck@collectPrograms'(XS0, XEnv, XCx, XDefs, XDx).
'lo.comp.typecheck@collectPrograms'('lo.core#,..'(XCl, XStmts), XEnv, XCx, 'lo.core#,..'('lo.comp.canon#relDef'(XLc, XNm, XTp, XCx, 'lo.core#,..'(XCl, XClses)), XDefs), XDx) :- XCl = 'lo.comp.canon#clause'(XLc, XNm, X_2187, X_2188),
    'lo.comp.typecheck@collectClauses'(XStmts, XS0, XNm, XClses),
    'lo.comp.dict@isVar'(XNm, XEnv, 'lo.comp.dict#vr'(X_2189, XTp)),
    'lo.comp.typecheck@collectPrograms'(XS0, XEnv, XCx, XDefs, XDx).
'lo.comp.typecheck@collectPrograms'('lo.core#,..'('lo.comp.canon#vrDef'(XLc, XNm, XValue, XCond), XStmts), XEnv, XCx, 'lo.core#,..'('lo.comp.canon#varDef'(XLc, XNm, XTp, XCx, XValue, XCond), XDefs), XDx) :- 'lo.comp.dict@isVar'(XNm, XEnv, 'lo.comp.dict#vr'(X_2190, XTp)),
    'lo.comp.typecheck@collectPrograms'(XStmts, XEnv, XCx, XDefs, XDx).
'lo.comp.typecheck@collectPrograms'('lo.core#,..'(XCl, XStmts), XEnv, XCx, 'lo.core#,..'('lo.comp.canon#classDef'(XLc, XNm, XTp, XCx, 'lo.core#,..'(XCl, XRules), XFace), XDefs), XDx) :- 'lo.comp.typecheck@isRuleForClass'(XCl, XLc, XNm, XFace),
    'lo.comp.typecheck@collectClassRules'(XStmts, XS0, XNm, XRules),
    'lo.comp.dict@isVar'(XNm, XEnv, 'lo.comp.dict#vr'(X_2191, XTp)),
    'lo.comp.typecheck@collectPrograms'(XS0, XEnv, XCx, XDefs, XDx).
'lo.comp.typecheck@collectPrograms'('lo.core#,..'(XRl, XStmts), XEnv, XCx, 'lo.core#,..'('lo.comp.canon#grammDef'(XLc, XNm, XTp, XCx, 'lo.core#,..'(XRl, XRules)), XDefs), XDx) :- 'lo.comp.typecheck@isGrammarRule'(XRl, XLc, XNm),
    'lo.comp.typecheck@collectGrammarRules'(XStmts, XS0, XNm, XRules),
    'lo.comp.dict@isVar'(XNm, XEnv, 'lo.comp.dict#vr'(X_2192, XTp)),
    'lo.comp.typecheck@collectPrograms'(XS0, XEnv, XCx, XDefs, XDx).
'lo.comp.typecheck@contractGroup'('lo.core#,..'('lo.comp.dependencies#defn'(XN, 'lo.comp.abstract#con', XLc, X_2193, 'lo.core#,..'(XConStmt, 'lo.core#[]')), X_2194), 'lo.core#,..'('lo.comp.canon#cnDefn'(XLc, XN, XContract), XDefs), XDefs, XEnv, XEx, XPath, XRp, XRpx) :- 'lo.comp.parseType@parseContract'(XConStmt, XEnv, XPath, XContract, XRp, XRpx),
    'lo.comp.typecheck@defineContract'(XLc, XContract, XEnv, XEx).
'lo.comp.typecheck@sameLength'(XL1, XL2, X_2195, XRp, XRp) :- 'lo.list@length'(XL1, XX32671),
    'lo.list@length'(XL2, XX32673),
    XX32671 = XX32673.
'lo.comp.typecheck@sameLength'(XL1, X_2196, XLc, XRp, XRpx) :- 'lo.list@length'(XL1, XX32681),
    ocall('disp%2'(XX32681, XX32682),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("expecting "), 'lo.core#,..'(XX32682, 'lo.core#,..'('lo.core#ss'(" elements"), 'lo.core#[]')))), XX32690),
    'lo.comp.errors@reportError'(XX32690, XLc, XRp, XRpx).
'lo.comp.typecheck@pushFace'('lo.core#[]', X_2197, XE, XE) :- !.
'lo.comp.typecheck@pushFace'('lo.core#,..'((XN, XT), XL), XLc, XE, XX32714) :- !,
    'lo.comp.dict@declareVar'(XN, 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, XN), XT), XE, XX32713),
    'lo.comp.typecheck@pushFace'(XL, XLc, XX32713, XX32714).
'lo.comp.typecheck@pushFace'(_, _, _, _) :- raise_exception('error'("pushFace", 925, 3, 21)).
'lo.comp.typecheck@checkOther'(XSt, 'lo.core#,..'('lo.comp.canon#integrity'(XLc, XCond), XMore), XMore, XEnv, XRp, XRpx) :- 'lo.comp.abstract@isUnary'(XSt, "assert", XLc, XC),
    'lo.comp.typecheck@checkCond'(XC, XEnv, X_2198, XCond, XRp, XRpx).
'lo.comp.typecheck@checkOther'(XSt, 'lo.core#,..'('lo.comp.canon#expShow'(XLc, XShow), XMore), XMore, XEnv, XRp, XRpx) :- 'lo.comp.abstract@isUnary'(XSt, "show", XLc, XE),
    'lo.comp.typecheck@findType'("string", XLc, XEnv, XRp, XRp0, XX32752),
    'lo.comp.typecheck@typeOfTerm'(XE, XX32752, XEnv, X_2199, XShow, XRp0, XRpx).
'lo.comp.typecheck@checkOthers'('lo.core#[]', 'lo.core#[]', X_2200, X_2201, XRp, XRp).
'lo.comp.typecheck@checkOthers'('lo.core#,..'(XSt, XStmts), XAss, XEnv, XPath, XRp, XRpx) :- 'lo.comp.typecheck@one41'(XRp0, XRp, XEnv, XMore, XAss, XSt),
    'lo.comp.typecheck@checkOthers'(XStmts, XMore, XEnv, XPath, XRp0, XRpx).
'lo.comp.typecheck@checkOthers'('lo.core#,..'(XSt, XStmts), XAss, XEnv, XPath, XRp, XRpx) :- ocall('disp%2'(XSt, XX32794),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot understand statement: "), 'lo.core#,..'(XX32794, 'lo.core#[]'))), XX32800),
    ocall('loc%1'(XXV64),XSt,XSt),
    'lo.comp.errors@reportError'(XX32800, XXV64, XRp, XRp0),
    'lo.comp.typecheck@checkOthers'(XStmts, XAss, XEnv, XPath, XRp0, XRpx).
'lo.comp.typecheck@implementationGroup'('lo.core#,..'('lo.comp.dependencies#defn'(XINm, 'lo.comp.abstract#impl', X_2202, X_2203, 'lo.core#,..'(XStmt, 'lo.core#[]')), 'lo.core#[]'), 'lo.core#,..'(XImpl, XDefs), XDefs, XEnv, XEx, XPath, XRp, XRpx) :- 'lo.comp.abstract@isUnary'(XStmt, "implementation", XLc, XI),
    'lo.comp.abstract@isBinary'(XI, "<=", X_2204, XSq, XBody),
    'lo.comp.abstract@isBraceTuple'(XBody, X_2205, XEls),
    'lo.comp.parseType@parseContractConstraint'(XSq, XEnv, XNm, XSpec, XRp, XRp0),
    'lo.comp.dict@isContract'(XNm, XEnv, 'lo.comp.types#conEntry'(X_2206, XCNm, XFullSpec, XConFace)),
    'lo.comp.types@moveConQuants'(XFullSpec, X_2207, 'lo.comp.types#conTract'(XConNm, XOArgs, XODeps)),
    'lo.comp.types@moveConQuants'(XSpec, XSQ, XASpec),
    'lo.comp.types@moveConConstraints'(XASpec, XAC, 'lo.comp.types#conTract'(XConNm, XAArgs, XADeps)),
    'lo.comp.typecheck@sameLength'(XOArgs, XAArgs, XLc, XRp0, XRp1),
    'lo.comp.typecheck@sameLength'(XODeps, XADeps, XLc, XRp1, XRp2),
    ocall('_empty%1'(XXV65),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.types@bindAT'(XOArgs, XAArgs, XXV65, XX32884),
    'lo.comp.types@bindAT'(XODeps, XADeps, XX32884, XX32885),
    XQQ = XX32885,
    'lo.comp.types@moveQuants'(XConFace, X_2208, XCFace),
    ocall('freshen%3'(XCFace, XQQ, XX32893),'lo.comp.freshen$freshen$lo.comp.types*tipe','lo.comp.freshen$freshen$lo.comp.types*tipe'),
    (X_2209, XF) = XX32893,
    'lo.comp.types@moveConstraints'(XCF, XAC, XF),
    'lo.comp.types@moveQuants'(XFT, XSQ, XCF),
    ocall('_empty%1'(XXV66),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('skolemize%3'(XFT, XXV66, XX32906),'lo.comp.freshen$freshen$lo.comp.types*tipe','lo.comp.freshen$freshen$lo.comp.types*tipe'),
    (XIQ, XSF) = XX32906,
    'lo.comp.types@moveConstraints'(XSF, XSC, XFace),
    'lo.comp.dict@pushScope'(XEnv, XX32913),
    'lo.comp.typecheck@declareConstraints'(XSC, XX32913, XE0),
    ocall('pairs%2'(XIQ, XX32916),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.typecheck@declareTypeVars'(XX32916, XLc, XE0, XSEnv),
    'lo.comp.types@implementationName'('lo.comp.types#conTract'(XCNm, XAArgs, XADeps), XX32926),
    XX32926 = XImplName,
    'lo.comp.typecheck@checkThetaTerm'(XFace, XLc, XEls, XSEnv, XThDefs, XOthers, XTypes, XImplName, XRp0, XRpx),
    XImpl = 'lo.comp.canon#implDef'(XLc, XINm, XImplName, XSpec, XSC, 'lo.comp.canon#tpl'('lo.core#[]'), 'lo.comp.canon#theta'(XThDefs, XOthers), XFace),
    'lo.comp.dict@declareImplementation'(XNm, XImplName, 'lo.comp.types#implEntry'(XImplName, XSpec), XEnv, XX32958),
    XEx = XX32958.
'lo.comp.typecheck@implementationGroup'('lo.core#,..'('lo.comp.dependencies#defn'(XINm, 'lo.comp.abstract#impl', XLc, X_2210, XStmt), 'lo.core#[]'), XDefs, XDefs, XEnv, XE, XPath, XRp, XRpx) :- ocall('disp%2'(XStmt, XX32976),'lo.core$display$lo.core*list'('lo.core$display$lo.comp.ast*ast'),'lo.core$display$lo.core*list'('lo.core$display$lo.comp.ast*ast')),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("could not check implementation statement "), 'lo.core#,..'(XX32976, 'lo.core#[]'))), XX32983),
    'lo.comp.errors@reportError'(XX32983, XLc, XRp, XRpx).
'lo.comp.typecheck@checkDefn'(XLc, XL, XC, XR, XTp, 'lo.core#,..'('lo.comp.canon#vrDef'(XLc, XNm, XValue, XCond), XDx), XDx, XE, XRp, XRpx) :- 'lo.comp.abstract@isIden'(XL, X_2211, XNm),
    'lo.comp.dict@pushScope'(XE, XX33008),
    'lo.comp.typecheck@checkCond'(XC, XX33008, XE1, XCond, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerm'(XR, XTp, XE1, X_2212, XValue, XRp0, XRpx).
'lo.comp.typecheck@checkClassHead'(XTerm, XTp, XTp, XEnv, XEnv, XNm, 'lo.comp.canon#v'(XLc, XNm), XRp, XRp) :- 'lo.comp.abstract@isIden'(XTerm, XLc, XNm).
'lo.comp.typecheck@checkClassHead'(XTerm, 'lo.comp.types#classType'(XAT, XTp), XTp, XEnv, XEv, XNm, 'lo.comp.canon#apply'(XLc, 'lo.comp.canon#v'(XLc, XNm), XArgs), XRp, XRpx) :- 'lo.comp.typecheck@splitHead'(XTerm, XLc, XNm, XA),
    'lo.comp.typecheck@typeOfArg'(XA, XAT, XEnv, XEv, XArgs, XRp, XRpx).
'lo.comp.typecheck@checkClassRule'(X_2213, XH, XG, XR, XClassTp, 'lo.core#,..'('lo.comp.canon#clRule'(XLc, XNm, XHd, 'lo.comp.canon#theta'(XClDefs, XClOthers), XCond, 'lo.comp.types#faceType'(XTypes)), XDefs), XDefs, XE, XPth, XRp, XRpx) :- 'lo.comp.abstract@isBraceTuple'(XR, XLc, XEls),
    'lo.comp.dict@pushScope'(XE, XX33090),
    'lo.comp.typecheck@checkClassHead'(XH, XClassTp, XSuperTp, XX33090, XE1, XNm, XHd, XRp, XRp0),
    'lo.comp.typecheck@checkCond'(XG, XE1, XE2, XCond, XRp0, XRp1),
    'lo.comp.abstract@marker'('lo.comp.abstract#clss', XX33111),
    'lo.comp.misc@subPath'(XPth, XX33111, XNm, XX33113),
    'lo.comp.typecheck@checkThetaTerm'(XSuperTp, XLc, XEls, XE1, XClDefs, XClOthers, XTypes, XX33113, XRp0, XRpx).
'lo.comp.typecheck@checkClassRule'(XLc, XH, XG, XR, XClassTp, 'lo.core#,..'('lo.comp.canon#clRule'(XLc, XNm, XHd, XRepl, XCond, XSuperFace), XDefs), XDefs, XE, XPth, XRp, XRpx) :- 'lo.comp.dict@pushScope'(XE, XX33139),
    'lo.comp.typecheck@checkClassHead'(XH, XClassTp, X_2214, XX33139, XE1, XNm, XHd, XRp, XRp0),
    'lo.comp.typecheck@checkCond'(XG, XE1, XE2, XCond, XRp0, XRp1),
    'lo.comp.types@newVar'("SpTp", XX33152),
    XSuperTp = XX33152,
    'lo.comp.typecheck@typeOfTerm'(XR, XSuperTp, XE2, X_2215, XRepl, XRp1, XRpx),
    'lo.comp.unify@faceOfType'(XSuperTp, XE2, XSuperFace).
'lo.comp.typecheck@checkThetaTerm'(XClassTp, XLc, XEls, XEnv, XDefs, XOthers, XTypes, XClassPath, XRp, XRpx) :- 'lo.comp.types@deRef'(XClassTp, XX33174),
    'lo.comp.unify@faceOfType'(XX33174, XEnv, XFace),
    'lo.comp.types@moveConstraints'(XFace, X_2216, 'lo.comp.types#faceType'(XFields)),
    'lo.comp.dict@declareVar'("this", 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, "this"), XClassTp), XEnv, XX33192),
    'lo.comp.typecheck@thetaEnv'('lo.repo$repository$lo.repo*coreRepo', XClassPath, 'lo.repo#coreRepo', XLc, XEls, XFields, XX33192, X_OEnv, XDefs, XPublic, X_Imports, X_2217, XOthers, XRp, XRpx),
    'lo.comp.typecheck@computeExport'(XDefs, XFields, XPublic, X_2218, XTypes, X_2219, 'lo.core#[]', 'lo.core#[]').
'lo.comp.typecheck@typeOfIndex'(XLc, XMp, XArg, XTp, XEnv, XEv, XExp, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XArg, "->", X_2220, XKy, XVl),
    'lo.comp.abstract@ternary'(XLc, "_put", XMp, XKy, XVl, XX33226),
    'lo.comp.typecheck@typeOfTerm'(XX33226, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfIndex'(XLc, XMp, XArg, XTp, XEnv, XEv, XExp, XRp, XRpx) :- 'lo.comp.abstract@isUnary'(XArg, "\\+", X_2221, XKy),
    'lo.comp.abstract@binary'(XLc, "_remove", XMp, XKy, XX33248),
    'lo.comp.typecheck@typeOfTerm'(XX33248, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfIndex'(XLc, XMp, XArg, XTp, XEnv, XEv, XExp, XRp, XRpx) :- 'lo.comp.abstract@binary'(XLc, "find", XMp, XArg, XX33267),
    'lo.comp.typecheck@typeOfTerm'(XX33267, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfCall'(XLc, XFun, XA, 'lo.comp.types#funType'(XArgTp, XResTp), XTp, XEnv, XEv, 'lo.comp.canon#apply'(XLc, XFun, XArgs), XRp, XRpx) :- 'lo.comp.typecheck@checkType'(XLc, XResTp, XTp, XEnv, XRp, XRp0),
    'lo.comp.typecheck@typeOfArg'(XA, XArgTp, XEnv, XEv, XArgs, XRp0, XRpx).
'lo.comp.typecheck@typeOfCall'(XLc, XFun, XA, 'lo.comp.types#classType'(XArgTp, XResTp), XTp, XEnv, XEv, 'lo.comp.canon#apply'(XLc, XFun, XArgs), XRp, XRpx) :- 'lo.comp.typecheck@checkType'(XLc, XResTp, XTp, XEnv, XRp, XRp0),
    'lo.comp.typecheck@typeOfArg'(XA, XArgTp, XEnv, XEv, XArgs, XRp0, XRpx).
'lo.comp.typecheck@typeOfCall'(XLc, XFun, X_2222, XFtp, X_2223, XEnv, XEnv, XFun, XRp, XRpx) :- ocall('disp%2'(XFun, XX33341),'lo.core$display$lo.comp.canon*canonTerm','lo.core$display$lo.comp.canon*canonTerm'),
    ocall('disp%2'(XFtp, XX33345),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XX33341, 'lo.core#,..'('lo.core#ss'(" : "), 'lo.core#,..'(XX33345, 'lo.core#,..'('lo.core#ss'(" is not a function"), 'lo.core#[]'))))), XX33354),
    'lo.comp.errors@reportError'(XX33354, XLc, XRp, XRpx).
'lo.comp.typecheck@checkGrammarRule'(XLc, XL, XR, 'lo.comp.types#grammarType'(XAT, XTp), 'lo.core#,..'('lo.comp.canon#grRule'(XLc, XNm, XArgs, 'lo.comp.canon#grTerms'(XPB), XBody), XDefs), XDefs, XE, XRp, XRpx) :- 'lo.comp.typecheck@splitGrHead'(XL, XNm, XA, XP),
    'lo.comp.types@newVar'("_E", XX33382),
    XElTp = XX33382,
    'lo.comp.dict@pushScope'(XE, XX33390),
    'lo.comp.dict@declareVar'("stream$X", 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, "stream$X"), XTp), XX33390, XX33391),
    'lo.comp.typecheck@typeOfArg'(XA, XAT, XX33391, XE2, XArgs, XRp, XRp0),
    'lo.comp.typecheck@checkNonTerminal'(XR, XTp, XElTp, XE2, XE3, XBody, XRp0, XRp1),
    'lo.comp.typecheck@typeOfTerm'('lo.comp.ast#iden'(XLc, "_hdtl"), 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'(XStrmTp, 'lo.core#,..'(XElTp, 'lo.core#,..'(XStrmTp, 'lo.core#[]'))))), XE3, X_2224, XHDTL, XRp1, XRp2),
    'lo.comp.typecheck@checkTerminals'(XP, XHDTL, XPB, XElTp, XE3, X_2225, XRp2, XRpx).
'lo.comp.typecheck@checkClause'(XLc, XL, XR, 'lo.comp.types#predType'(XAT), 'lo.core#,..'('lo.comp.canon#clause'(XLc, XNm, XArgs, XBody), XDefs), XDefs, XE, XRp, XRpx) :- 'lo.comp.typecheck@splitHead'(XL, X_2226, XNm, XA),
    'lo.comp.dict@pushScope'(XE, XX33451),
    'lo.comp.typecheck@typeOfArg'(XA, XAT, XX33451, XE0, XArgs, XRp, XRp0),
    'lo.comp.typecheck@checkCond'(XR, XE0, X_2227, XBody, XRp0, XRpx).
'lo.comp.typecheck@checkSequenceTerm'(XLc, XEls, XTp, XEnv, XEv, XExp, XRp, XRpx) :- 'lo.comp.abstract@genIden'(XLc, XX33472),
    XSeq = XX33472,
    'lo.comp.typecheck@macroSequenceTerm'(XEls, XLc, XSeq, XTsts),
    'lo.comp.abstract@roundTuple'(XLc, XTsts, XX33481),
    'lo.comp.abstract@binary'(XLc, "@@", XSeq, XX33481, XX33482),
    'lo.comp.typecheck@typeOfTerm'(XX33482, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfListTerm'('lo.core#[]', XLc, X_2228, XListTp, XEnv, XEv, XExp, XRp, XRpx) :- 'lo.comp.typecheck@typeOfTerm'('lo.comp.ast#iden'(XLc, "[]"), XListTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfListTerm'('lo.core#,..'(XLast, 'lo.core#[]'), X_2229, XElTp, XListTp, XEnv, XEv, 'lo.comp.canon#apply'(XLc, XOp, 'lo.comp.canon#tpl'('lo.core#,..'(XHd, 'lo.core#,..'(XTl, 'lo.core#[]')))), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XLast, ",..", XLc, XL, XR),
    'lo.comp.types@newVar'("_", XX33531),
    'lo.comp.typecheck@knownType'('lo.comp.ast#iden'(XLc, ",.."), XX33531, XEnv, XE0, XOp, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerm'(XL, XElTp, XE0, XE1, XHd, XRp0, XRp1),
    'lo.comp.typecheck@typeOfTerm'(XR, XListTp, XE1, XEv, XTl, XRp1, XRpx).
'lo.comp.typecheck@typeOfListTerm'('lo.core#,..'(XEl, XMore), X_2230, XElTp, XListTp, XEnv, XEv, 'lo.comp.canon#apply'(XLc, XOp, 'lo.comp.canon#tpl'('lo.core#,..'(XHd, 'lo.core#,..'(XTl, 'lo.core#[]')))), XRp, XRpx) :- ocall('loc%1'(XXV67),XEl,XEl),
    XLc = XXV67,
    'lo.comp.types@newVar'("_", XX33574),
    'lo.comp.typecheck@knownType'('lo.comp.ast#iden'(XLc, ",.."), XX33574, XEnv, XE0, XOp, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerm'(XEl, XElTp, XE0, XE1, XHd, XRp0, XRp1),
    'lo.comp.typecheck@typeOfListTerm'(XMore, XLc, XElTp, XListTp, XE1, XEv, XTl, XRp1, XRpx).
'lo.comp.typecheck@checkSquareTuple'(XLc, XEls, XTp, XEnv, XEv, XExp, XRp, XRpx) :- 'lo.comp.typecheck@cond25'(XElTp, XX33640, XListTp, XX33646, XRpx, XExp, XEv, XX33628, XRp3, XRp2, XX33615, XX33614, XX33613, XRp1, XRp, XEls, XTp, XLc, XEnv).
'lo.comp.typecheck@recordAccessExp'(XLc, XRc, XFld, XET, XEnv, XEv, 'lo.comp.canon#dot'(XLc, XRec, XFld), XRp, XRpx) :- 'lo.comp.types@newVar'("_R", XX33687),
    XAT = XX33687,
    'lo.comp.typecheck@knownType'(XRc, XAT, XEnv, XEv, XRec, XRp, XRp0),
    'lo.comp.types@deRef'(XAT, XX33696),
    'lo.comp.unify@faceOfType'(XX33696, XEnv, XFace),
    'lo.comp.types@moveConstraints'(XFace, X_2231, 'lo.comp.types#faceType'(XFields)),
    'lo.comp.typecheck@one42'(XRp1, XRp0, XFTp, XLc, XFld, XAT, XFields),
    ocall('_empty%1'(XXV68),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('freshen%3'(XFTp, XXV68, XX33715),'lo.comp.freshen$freshen$lo.comp.types*tipe','lo.comp.freshen$freshen$lo.comp.types*tipe'),
    (X_2232, XTp) = XX33715,
    'lo.comp.typecheck@checkType'(XLc, XTp, XET, XEnv, XRp1, XRpx).
'lo.comp.typecheck@knownType'(XV, XTp, XEnv, XEnv, XTerm, XRp, XRpx) :- 'lo.comp.typecheck@cond27'(XEx, XEnv, XSpec, XTp, XTerm, XRp, XRpx, XEv, XX33747, XX33755, XNm, XLc, XV).
'lo.comp.typecheck@checkNonTerminals'('lo.core#[]', X_2233, X_2234, XEnv, XEnv, 'lo.comp.canon#grTest'('lo.comp.canon#trueCond'), XRp, XRp).
'lo.comp.typecheck@checkNonTerminals'('lo.core#,..'(XN, XEls), XTp, XElTp, XEnv, XEv, 'lo.comp.canon#grConj'(XLhs, XRhs), XRp, XRpx) :- 'lo.comp.typecheck@checkNonTerminal'(XN, XTp, XElTp, XEnv, XE0, XLhs, XRp, XRp0),
    'lo.comp.typecheck@checkNonTerminals'(XEls, XTp, XElTp, XE0, XEv, XRhs, XRp0, XRpx).
'lo.comp.typecheck@checkTerminals'('lo.core#[]', X_2235, 'lo.core#[]', X_2236, XEnv, XEnv, XRp, XRp).
'lo.comp.typecheck@checkTerminals'('lo.core#,..'(XT, XL), XV, 'lo.core#,..'((XXV69, XV, XTT), XM), XElTp, XEnv, XEv, XRp, XRpx) :- ocall('loc%1'(XXV69),XT,XT),
    'lo.comp.typecheck@typeOfTerm'(XT, XElTp, XEnv, XE0, XTT, XRp, XRp0),
    'lo.comp.typecheck@checkTerminals'(XL, XV, XM, XElTp, XE0, XEv, XRp0, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTpl, XStrmTp, XElTp, XE, XEnv, 'lo.comp.canon#grTerms'(XTerms), XRp, XRpx) :- 'lo.comp.abstract@isSquareTuple'(XTpl, XLc, XEls),
    'lo.comp.typecheck@typeOfTerm'('lo.comp.ast#iden'(XLc, "_hdtl"), 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'(XStrmTp, 'lo.core#,..'(XElTp, 'lo.core#,..'(XStrmTp, 'lo.core#[]'))))), XE, XE0, XHDTL, XRp, XRp0),
    'lo.comp.typecheck@checkTerminals'(XEls, XHDTL, XTerms, XElTp, XE0, XEnv, XRp0, XRpx).
'lo.comp.typecheck@checkNonTerminal'('lo.comp.ast#strg'(XLc, XText), XStrmTp, XElTp, XE, XEnv, 'lo.comp.canon#grTerms'(XTerms), XRp, XRpx) :- 'lo.comp.typecheck@typeOfTerm'('lo.comp.ast#iden'(XLc, "_hdtl"), 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'(XStrmTp, 'lo.core#,..'(XElTp, 'lo.core#,..'(XStrmTp, 'lo.core#[]'))))), XE, XE0, XHDTL, XRp, XRp0),
    'lo.comp.typecheck@explodeStringLit'(XLc, XText, XX33909),
    'lo.comp.typecheck@checkTerminals'(XX33909, XHDTL, XTerms, XElTp, XE0, XEnv, XRp0, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, XTp, XElTp, XEnv, XEx, XGrNT, XRp, XRpx) :- 'lo.comp.abstract@isRoundTuple'(XTerm, XLc, XEls),
    'lo.comp.typecheck@one43'(XRpx, XRp, XGrNT, XEx, XEnv, XElTp, XTp, XEls).
'lo.comp.typecheck@checkNonTerminal'(XTerm, XTp, XElTp, XEnv, XEx, 'lo.comp.canon#grConj'(XLhs, XRhs), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, ",", XLc, XL, XR),
    'lo.comp.typecheck@checkNonTerminal'(XL, XTp, XElTp, XEnv, XE1, XLhs, XRp, XRp0),
    'lo.comp.typecheck@checkNonTerminal'(XR, XTp, XElTp, XE1, XEx, XRhs, XRp0, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, XTp, XElTp, XEnv, XEx, 'lo.comp.canon#grCond'(XTest, XEither, XOr), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, "|", X_2237, XL, XR),
    'lo.comp.abstract@isBinary'(XL, "?", X_2238, XT, XTh),
    'lo.comp.typecheck@checkNonTerminal'(XT, XTp, XElTp, XEnv, XE0, XTest, XRp, XRp0),
    'lo.comp.typecheck@checkNonTerminal'(XTh, XTp, XElTp, XE0, XE1, XEither, XRp0, XRp1),
    'lo.comp.typecheck@checkNonTerminal'(XR, XTp, XElTp, XE1, XEx, XOr, XRp1, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, XTp, XElTp, XEnv, XEx, 'lo.comp.canon#grDisj'(XEither, XOr), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, "|", X_2239, XL, XR),
    'lo.comp.typecheck@neg39'(X_2242, X_2241, X_2240, XL),
    'lo.comp.typecheck@checkNonTerminal'(XL, XTp, XElTp, XEnv, XE1, XEither, XRp, XRp0),
    'lo.comp.typecheck@checkNonTerminal'(XR, XTp, XElTp, XE1, XEx, XOr, XRp0, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, XTp, XElTp, XEnv, XEx, 'lo.comp.canon#grOne'(XTest), XRp, XRpx) :- 'lo.comp.abstract@isUnary'(XTerm, "!", X_2243, XN),
    'lo.comp.typecheck@checkNonTerminal'(XN, XTp, XElTp, XEnv, XEx, XTest, XRp, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, XTp, XElTp, XEnv, XEnv, 'lo.comp.canon#grNeg'(XTest), XRp, XRpx) :- 'lo.comp.abstract@isUnary'(XTerm, "\\+", XLc, XN),
    'lo.comp.typecheck@checkNonTerminal'(XN, XTp, XElTp, XEnv, X_2244, XTest, XRp, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, XTp, XElTp, XEnv, XEnv, 'lo.comp.canon#grAhed'(XTest), XRp, XRpx) :- 'lo.comp.abstract@isUnary'(XTerm, "+", XLc, XN),
    'lo.comp.typecheck@checkNonTerminal'(XN, XTp, XElTp, XEnv, X_2245, XTest, XRp, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, X_2246, X_2247, XEnv, XEv, 'lo.comp.canon#grTest'('lo.comp.canon#unifyCond'(XLc, XLhs, XRhs)), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, "=", XLc, XL, XR),
    'lo.comp.types@newVar'("_", XX34120),
    XTV = XX34120,
    'lo.comp.typecheck@typeOfTerm'(XL, XTV, XEnv, XE0, XLhs, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerm'(XR, XTV, XE0, XEv, XRhs, XRp0, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, X_2248, X_2249, XEnv, XEv, 'lo.comp.canon#grTest'('lo.comp.canon#negCond'('lo.comp.canon#unifyCond'(XLc, XLhs, XRhs))), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, "\\=", XLc, XL, XR),
    'lo.comp.types@newVar'("_", XX34153),
    XTV = XX34153,
    'lo.comp.typecheck@typeOfTerm'(XL, XTV, XEnv, XE0, XLhs, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerm'(XR, XTV, XE0, XEv, XRhs, XRp0, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, XTp, X_2250, XEnv, XEv, 'lo.comp.canon#grDip'('lo.comp.canon#v'(XLc, XNV), XCond), XRp, XRpx) :- 'lo.comp.abstract@isUnary'(XTerm, "@", XLc, XTest),
    'lo.comp.abstract@isRoundTerm'(XTest, X_2251, XOp, XArgs),
    '_str_gen'("_", XX34188),
    XNV = XX34188,
    'lo.comp.abstract@binary'(XLc, ".", 'lo.comp.ast#iden'(XLc, XNV), XOp, XX34195),
    'lo.comp.dict@declareVar'(XNV, 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, XNV), XTp), XEnv, XX34207),
    'lo.comp.typecheck@checkCond'('lo.comp.ast#appl'(XLc, XX34195, 'lo.comp.ast#tupl'(XLc, "()", XArgs)), XX34207, XEv, XCond, XRp, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, XTp, X_2252, XEnv, XEv, XNT, XRp, XRpx) :- 'lo.comp.abstract@isRound'(XTerm, XLc, XF, XA),
    'lo.comp.types@newVar'("_G", XX34225),
    XGrTp = XX34225,
    'lo.comp.typecheck@knownType'(XF, XGrTp, XEnv, XE0, XOp, XRp, XRp0),
    'lo.comp.typecheck@cond29'(XX34299, XX34290, XX34286, XF, XLc, XTp, XEnv, XRp0, XRp1, XA, XE0, XEv, XArg, XRpx, XNT, XOp, XX34258, XX34262, XX34266, XX34276, XStrmTp, XArgTp, XX34234, XGrTp).
'lo.comp.typecheck@checkNonTerminal'(XTerm, XStreamTp, XElTp, XEnv, XEv, 'lo.comp.canon#grDip'('lo.comp.canon#v'(XLc, XNV), XCond), XRp, XRpx) :- 'lo.comp.abstract@isIden'(XTerm, XLc, "eof"),
    '_str_gen'("_", XX34323),
    XNV = XX34323,
    'lo.comp.abstract@unary'(XLc, "_eof", 'lo.comp.ast#iden'(XLc, XNV), XX34328),
    'lo.comp.dict@declareVar'(XNV, 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, XNV), XStreamTp), XEnv, XX34336),
    'lo.comp.typecheck@checkCond'(XX34328, XX34336, XEv, XCond, XRp, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, X_2253, X_2254, XEnv, XEx, 'lo.comp.canon#grTest'(XCond), XRp, XRpx) :- 'lo.comp.abstract@isBraceTuple'(XTerm, XLc, XEls),
    'lo.comp.typecheck@checkConds'(XEls, XEnv, XEx, XCond, XRp, XRpx).
'lo.comp.typecheck@checkConds'('lo.core#[]', XEnv, XEnv, 'lo.comp.canon#trueCond', XRp, XRp).
'lo.comp.typecheck@checkConds'('lo.core#,..'(XC, 'lo.core#[]'), XEv, XEnv, XCond, XRp, XRpx) :- 'lo.comp.typecheck@checkCond'(XC, XEv, XEnv, XCond, XRp, XRpx).
'lo.comp.typecheck@checkConds'('lo.core#,..'(XC, XL), XEv, XEnv, 'lo.comp.canon#conjCond'(XC0, XCm), XRp, XRpx) :- 'lo.comp.typecheck@checkCond'(XC, XEv, XE0, XC0, XRp, XRp0),
    'lo.comp.typecheck@checkConds'(XL, XE0, XEnv, XCm, XRp0, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEnv, 'lo.comp.canon#trueCond', XRp, XRp) :- 'lo.comp.abstract@isIden'(XTerm, XLc, "true").
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEnv, 'lo.comp.canon#falseCond', XRp, XRp) :- 'lo.comp.abstract@isIden'(XTerm, XLc, "false").
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEx, 'lo.comp.canon#conjCond'(XLhs, XRhs), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, ",", X_2255, XL, XR),
    'lo.comp.typecheck@checkCond'(XL, XEnv, XE1, XLhs, XRp, XRp0),
    'lo.comp.typecheck@checkCond'(XR, XE1, XEx, XRhs, XRp0, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEx, 'lo.comp.canon#condCond'(XTest, XEither, XOr), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, "|", X_2256, XL, XR),
    'lo.comp.abstract@isBinary'(XL, "?", X_2257, XT, XTh),
    'lo.comp.typecheck@checkCond'(XT, XEnv, XE0, XTest, XRp, XRp0),
    'lo.comp.typecheck@checkCond'(XTh, XE0, XE1, XEither, XRp0, XRp1),
    'lo.comp.typecheck@checkCond'(XR, XE1, XEx, XOr, XRp1, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEx, 'lo.comp.canon#disjCond'(XEither, XOr), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, "|", X_2258, XL, XR),
    'lo.comp.typecheck@checkCond'(XL, XEnv, XE1, XEither, XRp, XRp0),
    'lo.comp.typecheck@checkCond'(XR, XE1, XEx, XOr, XRp0, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEx, 'lo.comp.canon#oneCond'(XTest), XRp, XRpx) :- 'lo.comp.abstract@isUnary'(XTerm, "!", X_2259, XN),
    'lo.comp.typecheck@checkCond'(XN, XEnv, XEx, XTest, XRp, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEnv, 'lo.comp.canon#negCond'(XTest), XRp, XRpx) :- 'lo.comp.abstract@isUnary'(XTerm, "\\+", X_2260, XN),
    'lo.comp.typecheck@checkCond'(XN, XEnv, X_2261, XTest, XRp, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEnv, 'lo.comp.canon#forallCond'(XGen, XTest), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, "*>", X_2262, XL, XR),
    'lo.comp.typecheck@checkCond'(XL, XEnv, XE0, XGen, XRp, XRp0),
    'lo.comp.typecheck@checkCond'(XR, XE0, X_2263, XTest, XRp0, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEx, XCond, XRp, XRpx) :- 'lo.comp.abstract@isRoundTuple'(XTerm, X_2264, XC),
    'lo.comp.typecheck@checkConds'(XC, XEnv, XEx, XCond, XRp, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEv, 'lo.comp.canon#negCond'('lo.comp.canon#unifyCond'(XLc, XLhs, XRhs)), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, "\\=", XLc, XL, XR),
    'lo.comp.types@newVar'("_#", XX34586),
    XTV = XX34586,
    'lo.comp.typecheck@typeOfTerm'(XL, XTV, XEnv, XE0, XLhs, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerm'(XR, XTV, XE0, XEv, XRhs, XRp0, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEv, 'lo.comp.canon#unifyCond'(XLc, XLhs, XRhs), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, "=", XLc, XL, XR),
    'lo.comp.types@newVar'("_#", XX34615),
    XTV = XX34615,
    'lo.comp.typecheck@typeOfTerm'(XL, XTV, XEnv, XE0, XLhs, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerm'(XR, XTV, XE0, XEv, XRhs, XRp0, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEv, 'lo.comp.canon#phraseCond'(XLc, XNT, XStrm, XRest), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, "%%", XLc, XL, XR),
    'lo.comp.abstract@isBinary'(XR, "~", X_2265, XS, XM),
    'lo.comp.types@newVar'("_S", XX34649),
    XStrmTp = XX34649,
    'lo.comp.types@newVar'("_E", XX34651),
    XElTp = XX34651,
    'lo.comp.typecheck@checkGrammarType'(XLc, XEnv, XStrmTp, XElTp, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerm'(XS, XStrmTp, XEnv, XE0, XStrm, XRp0, XRp1),
    'lo.comp.typecheck@typeOfTerm'(XM, XStrmTp, XE0, XE1, XRest, XRp1, XRp2),
    'lo.comp.typecheck@streamVar'(XX34675),
    'lo.comp.typecheck@streamVar'(XX34677),
    'lo.comp.dict@declareVar'(XX34675, 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, XX34677), XStrmTp), XE1, XX34682),
    'lo.comp.typecheck@checkNonTerminal'(XL, XStrmTp, XElTp, XX34682, XEv, XNT, XRp2, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEv, 'lo.comp.canon#phraseCond'(XLc, XNT, XStrm, XRest), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, "%%", XLc, XL, XR),
    'lo.comp.types@newVar'("_S", XX34702),
    XStrmTp = XX34702,
    'lo.comp.types@newVar'("_E", XX34704),
    XElTp = XX34704,
    'lo.comp.typecheck@checkGrammarType'(XLc, XEnv, XStrmTp, XElTp, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerm'(XR, XStrmTp, XEnv, XE0, XStrm, XRp0, XRp1),
    '_str_gen'("_", XX34720),
    XM = 'lo.comp.ast#iden'(XLc, XX34720),
    'lo.comp.typecheck@typeOfTerm'(XM, XStrmTp, XE0, XE1, XRest, XRp1, XRp2),
    'lo.comp.abstract@binary'(XLc, ",", XL, 'lo.comp.ast#iden'(XLc, "eof"), XX34733),
    'lo.comp.dict@declareVar'("stream_X", 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, "stream_X"), XStrmTp), XE1, XX34741),
    'lo.comp.typecheck@checkNonTerminal'(XX34733, XStrmTp, XElTp, XX34741, XEv, XNT, XRp2, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEv, XCall, XRp, XRpx) :- 'lo.comp.abstract@isRound'(XTerm, XLc, XF, XA),
    'lo.comp.types@newVar'("_P", XX34757),
    XPrTp = XX34757,
    'lo.comp.typecheck@knownType'(XF, XPrTp, XEnv, XE0, XPred, XRp, XRp0),
    'lo.comp.typecheck@cond30'(XX34795, XX34786, XX34782, XPred, XLc, XCall, XRpx, XRp0, XArg, XEv, XE0, XA, XArgTp, XX34766, XPrTp).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEv, 'lo.comp.canon#isTrue'(XExp), XRp, XRpx) :- 'lo.comp.typecheck@findType'("logical", XLc, XEnv, XRp, XRp0, XX34815),
    'lo.comp.typecheck@knownType'(XTerm, XX34815, XEnv, XEv, XExp, XRp0, XRpx).
'lo.comp.typecheck@typeOfTerm'(XV, X_2266, XEnv, XEnv, 'lo.comp.canon#v'(XLc, XX34826), XRp, XRp) :- 'lo.comp.abstract@isIden'(XV, XLc, "_"),
    '_str_gen'("_", XX34826).
'lo.comp.typecheck@typeOfTerm'(XV, XTp, XEnv, XEv, XTerm, XRp, XRpx) :- 'lo.comp.abstract@isIden'(XV, XLc, XNm),
    'lo.comp.typecheck@cond32'(XX34857, XX34863, XX34869, XX34879, XEv, XRpx, XRp, XTerm, XTp, XLc, XSpec, XEnv, XNm).
'lo.comp.typecheck@typeOfTerm'('lo.comp.ast#intg'(XLc, XIx), XTp, XEnv, XEnv, 'lo.comp.canon#int'(XIx), XRp, XRpx) :- 'lo.comp.typecheck@findType'("integer", XLc, XEnv, XRp, XRp0, XX34901),
    'lo.comp.typecheck@checkType'(XLc, XX34901, XTp, XEnv, XRp0, XRpx).
'lo.comp.typecheck@typeOfTerm'('lo.comp.ast#flot'(XLc, XDx), XTp, XEnv, XEnv, 'lo.comp.canon#flt'(XDx), XRp, XRpx) :- 'lo.comp.typecheck@findType'("float", XLc, XEnv, XRp, XRp0, XX34921),
    'lo.comp.typecheck@checkType'(XLc, XX34921, XTp, XEnv, XRp0, XRpx).
'lo.comp.typecheck@typeOfTerm'('lo.comp.ast#strg'(XLc, XSx), XTp, XEnv, XEnv, 'lo.comp.canon#str'(XSx), XRp, XRpx) :- 'lo.comp.typecheck@findType'("string", XLc, XEnv, XRp, XRp0, XX34941),
    'lo.comp.typecheck@checkType'(XLc, XX34941, XTp, XEnv, XRp0, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEv, XExp, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, ":", XLc, XL, XR),
    'lo.comp.typecheck@cond33'(XX34983, XX34977, XRpx, XExp, XEv, XL, XRp1, XTp, XLc, XRp0, XRp, XRT, XEnv, XR).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEv, XExp, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, "::", XLc, XL, XR),
    'lo.comp.abstract@unary'(XLc, "_coerce", XL, XX35008),
    'lo.comp.abstract@binary'(XLc, ":", XX35008, XR, XX35010),
    'lo.comp.typecheck@typeOfTerm'(XX35010, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XP, XTp, XEnv, XEx, 'lo.comp.canon#whre'(XPtn, XCond), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XP, "@@", X_2267, XL, XR),
    'lo.comp.typecheck@typeOfTerm'(XL, XTp, XEnv, XE0, XPtn, XRp, XRp0),
    'lo.comp.typecheck@checkCond'(XR, XE0, XEx, XCond, XRp0, XRpx).
'lo.comp.typecheck@typeOfTerm'(XCall, XTp, XEnv, XEv, 'lo.comp.canon#whre'(XV, XCond), XRp, XRpx) :- 'lo.comp.abstract@isUnary'(XCall, "@", XLc, XTest),
    'lo.comp.abstract@isRoundTerm'(XTest, X_2268, X_2269, X_2270),
    '_str_gen'("_", XX35061),
    XNV = 'lo.comp.ast#iden'(XLc, XX35061),
    'lo.comp.typecheck@typeOfTerm'(XNV, XTp, XEnv, XE0, XV, XRp, XRp0),
    'lo.comp.abstract@binary'(XLc, ".", XNV, XTest, XX35073),
    'lo.comp.typecheck@checkCond'(XX35073, XE0, XEv, XCond, XRp0, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEv, XExp, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, ".", XLc, XL, XF),
    'lo.comp.abstract@isIden'(XF, XFLc, XFld),
    'lo.comp.typecheck@recordAccessExp'(XFLc, XL, XFld, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEv, 'lo.comp.canon#condExp'(XTest, XThen, XElse), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, "|", XLc, XL, XEl),
    'lo.comp.abstract@isBinary'(XL, "?", X_2271, XTst, XTh),
    'lo.comp.typecheck@checkCond'(XTst, XEnv, XE0, XTest, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerm'(XTh, XTp, XE0, XE1, XThen, XRp0, XRp1),
    'lo.comp.typecheck@typeOfTerm'(XEl, XTp, XE1, XEv, XElse, XRp1, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEv, XExp, XRp, XRpx) :- 'lo.comp.abstract@isSquareTuple'(XTerm, XLc, XEls),
    'lo.comp.typecheck@checkSquareTuple'(XLc, XEls, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'('lo.comp.ast#tupl'(X_2272, "()", 'lo.core#,..'(XInner, 'lo.core#[]')), XTp, XEnv, XEv, XExp, XRp, XRpx) :- 'lo.comp.typecheck@neg40'(X_2274, X_2273, XInner),
    'lo.comp.typecheck@typeOfTerm'(XInner, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEv, 'lo.comp.canon#tpl'(XEls), XRp, XRpx) :- 'lo.comp.abstract@isRoundTuple'(XTerm, XLc, XA),
    'lo.comp.typecheck@genTpVars'(XA, XX35192),
    XArgTps = XX35192,
    'lo.comp.typecheck@checkType'(XLc, 'lo.comp.types#tupleType'(XArgTps), XTp, XEnv, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerms'(XA, XArgTps, XEnv, XEv, XLc, XEls, XRp0, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEnv, 'lo.comp.canon#int'(XX35214), XRp, XRpx) :- 'lo.comp.abstract@isUnary'(XTerm, "-", XLc, 'lo.comp.ast#intg'(X_2275, XIx)),
    'lo.comp.typecheck@findType'("integer", XLc, XEnv, XRp, XRp0, XX35229),
    'lo.comp.typecheck@checkType'(XLc, XX35229, XTp, XEnv, XRp0, XRpx),
    ocall('zero%1'(XXV70),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('-%3'(XXV70, XIx, XX35214),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer').
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEnv, 'lo.comp.canon#flt'(XX35240), XRp, XRpx) :- 'lo.comp.abstract@isUnary'(XTerm, "-", XLc, 'lo.comp.ast#flot'(X_2276, XDx)),
    'lo.comp.typecheck@findType'("float", XLc, XEnv, XRp, XRp0, XX35255),
    'lo.comp.typecheck@checkType'(XLc, XX35255, XTp, XEnv, XRp0, XRpx),
    ocall('zero%1'(XXV71),'lo.core$additive$lo.core*float','lo.core$additive$lo.core*float'),
    ocall('-%3'(XXV71, XDx, XX35240),'lo.core$additive$lo.core*float','lo.core$additive$lo.core*float').
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEnv, XExp, XRp, XRpx) :- 'lo.comp.abstract@isUnary'(XTerm, "-", XLc, XArg),
    'lo.comp.abstract@binary'(XLc, "-", 'lo.comp.ast#iden'(XLc, "zero"), XArg, XX35274),
    'lo.comp.typecheck@typeOfTerm'(XX35274, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEnv, 'lo.comp.canon#lambda'(XEqn), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, "=>", XLc, XLhs, XRhs),
    'lo.comp.types@deRef'(XTp, XX35303),
    'lo.comp.typecheck@checkEquation'(XLc, 'lo.comp.ast#appl'(XLc, 'lo.comp.ast#iden'(XLc, "fun"), XLhs), 'lo.comp.ast#iden'(XLc, "true"), XRhs, XX35303, 'lo.core#,..'(XEqn, X_2277), X_2278, XEnv, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEnv, 'lo.comp.canon#lambda'(XEqn), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, ":-", XLc, XL, XG),
    'lo.comp.abstract@isBinary'(XL, "=>", X_2279, XH, XR),
    'lo.comp.types@deRef'(XTp, XX35336),
    'lo.comp.typecheck@checkEquation'(XLc, 'lo.comp.ast#appl'(XLc, 'lo.comp.ast#iden'(XLc, "fun"), XH), XG, XR, XX35336, 'lo.core#,..'(XEqn, X_2280), X_2281, XEnv, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEnv, 'lo.comp.canon#lambda'(XClse), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, ":-", XLc, XLhs, XRhs),
    'lo.comp.types@deRef'(XTp, XX35364),
    'lo.comp.typecheck@checkClause'(XLc, 'lo.comp.ast#appl'(XLc, 'lo.comp.ast#iden'(XLc, "pred"), XLhs), XRhs, XX35364, 'lo.core#,..'(XClse, 'lo.core#[]'), 'lo.core#[]', XEnv, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEnv, 'lo.comp.canon#lambda'(XRle), XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XTerm, "-->", XLc, XLhs, XRhs),
    'lo.comp.types@deRef'(XTp, XX35392),
    'lo.comp.typecheck@checkGrammarRule'(XLc, 'lo.comp.ast#appl'(XLc, 'lo.comp.ast#iden'(XLc, "grm"), XLhs), XR, XX35392, 'lo.core#,..'(XRle, 'lo.core#[]'), 'lo.core#[]', XEnv, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEv, XExp, XRp, XRpx) :- 'lo.comp.abstract@isRound'(XTerm, XLc, XF, XA),
    'lo.comp.types@newVar'("F", XX35412),
    XFnTp = XX35412,
    'lo.comp.typecheck@knownType'(XF, XFnTp, XEnv, XE0, XFun, XRp, XRp0),
    'lo.comp.types@deRef'(XFnTp, XX35424),
    'lo.comp.typecheck@typeOfCall'(XLc, XFun, XA, XX35424, XTp, XE0, XEv, XExp, XRp0, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEv, XExp, XRp, XRpx) :- 'lo.comp.abstract@isSquareTerm'(XTerm, XLc, XF, 'lo.core#,..'(XA, 'lo.core#[]')),
    'lo.comp.typecheck@typeOfIndex'(XLc, XF, XA, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEnv, 'lo.comp.canon#theta'(XDefs, XOthers), XRp, XRpx) :- 'lo.comp.abstract@isBraceTuple'(XTerm, XLc, XEls),
    '_str_gen'("theta", XX35472),
    'lo.comp.typecheck@checkThetaTerm'(XTp, XLc, XEls, XEnv, XDefs, XOthers, XTypes, XX35472, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEnv, 'lo.comp.canon#v'(XLc, XX35480), XRp, XRpx) :- ocall('disp%2'(XTerm, XX35486),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('disp%2'(XTp, XX35490),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("illegal expression: "), 'lo.core#,..'(XX35486, 'lo.core#,..'('lo.core#ss'(", expecting a "), 'lo.core#,..'(XX35490, 'lo.core#[]'))))), XX35498),
    ocall('loc%1'(XXV72),XTerm,XTerm),
    'lo.comp.errors@reportError'(XX35498, XXV72, XRp, XRpx),
    '_str_gen'("_", XX35480).
'lo.comp.typecheck@typeOfTerms'('lo.core#[]', 'lo.core#[]', XEnv, XEnv, X_2282, 'lo.core#[]', XRp, XRp).
'lo.comp.typecheck@typeOfTerms'('lo.core#[]', 'lo.core#,..'(XT, X_2283), XEnv, XEnv, XLc, 'lo.core#[]', XRp, XRpx) :- ocall('disp%2'(XT, XX35522),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("insufficient arguments, expecting a "), 'lo.core#,..'(XX35522, 'lo.core#[]'))), XX35528),
    'lo.comp.errors@reportError'(XX35528, XLc, XRp, XRpx).
'lo.comp.typecheck@typeOfTerms'('lo.core#,..'(XA, X_2284), 'lo.core#[]', XEnv, XEnv, X_2285, 'lo.core#[]', XRp, XRpx) :- ocall('disp%2'(XA, XX35544),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("too many arguments: "), 'lo.core#,..'(XX35544, 'lo.core#[]'))), XX35550),
    ocall('loc%1'(XXV73),XA,XA),
    'lo.comp.errors@reportError'(XX35550, XXV73, XRp, XRpx).
'lo.comp.typecheck@typeOfTerms'('lo.core#,..'(XA, XAs), 'lo.core#,..'(XElTp, XElTypes), XEnv, XEv, X_2286, 'lo.core#,..'(XTerm, XEls), XRp, XRpx) :- 'lo.comp.typecheck@typeOfTerm'(XA, XElTp, XEnv, XE0, XTerm, XRp, XRp0),
    ocall('loc%1'(XXV74),XA,XA),
    'lo.comp.typecheck@typeOfTerms'(XAs, XElTypes, XE0, XEv, XXV74, XEls, XRp0, XRpx).
'lo.comp.typecheck@typeOfArg'(XTerm, XTp, XEnv, XEv, XExp, XRp, XRpx) :- 'lo.comp.typecheck@cond34'(XExp, XRpx, XEls, XEv, XRp0, XRp, XEnv, XTp, XX35595, XArgTps, XA, XLc, XTerm).
'lo.comp.typecheck@checkEquation'(XLc, XH, XG, XR, 'lo.comp.types#funType'(XAT, XRT), 'lo.core#,..'('lo.comp.canon#equation'(XLc, XNm, XArgs, XExp, XCond), XDefs), XDefs, XE, XRp, XRpx) :- 'lo.comp.typecheck@splitHead'(XH, X_2287, XNm, XA),
    'lo.comp.dict@pushScope'(XE, XX35647),
    'lo.comp.typecheck@typeOfArg'(XA, XAT, XX35647, XE0, XArgs, XRp, XRp0),
    'lo.comp.typecheck@checkCond'(XG, XE0, XE1, XCond, XRp0, XRp1),
    'lo.comp.typecheck@typeOfTerm'(XR, XRT, XE1, X_2288, XExp, XRp1, XRpx).
'lo.comp.typecheck@checkEquation'(XLc, X_2289, X_2290, X_2291, XProgramType, XDefs, XDefs, X_2292, XRp, XRpx) :- ocall('disp%2'(XProgramType, XX35677),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("equation not consistent with expected type: "), 'lo.core#,..'(XX35677, 'lo.core#[]'))), XX35683),
    'lo.comp.errors@reportError'(XX35683, XLc, XRp, XRpx).
'lo.comp.typecheck@processStmt'(XSt, XProgramType, XDefs, XDefx, XE, X_2293, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XSt, "=>", XLc, XL, XR),
    'lo.comp.typecheck@checkEquation'(XLc, XL, 'lo.comp.ast#iden'(XLc, "true"), XR, XProgramType, XDefs, XDefx, XE, XRp, XRpx).
'lo.comp.typecheck@processStmt'(XSt, XProgramType, XDefs, XDefx, XE, X_2294, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XSt, ":-", XLc, XL, XG),
    'lo.comp.abstract@isBinary'(XL, "=>", X_2295, XH, XR),
    'lo.comp.typecheck@checkEquation'(XLc, XH, XG, XR, XProgramType, XDefs, XDefx, XE, XRp, XRpx).
'lo.comp.typecheck@processStmt'(XSt, XProgramType, XDefs, XDefx, XE, XPth, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XSt, ":-", XLc, XL, XG),
    'lo.comp.abstract@isBinary'(XL, "<=", X_2296, XH, XR),
    'lo.comp.typecheck@checkClassRule'(XLc, XH, XG, XR, XProgramType, XDefs, XDefx, XE, XPth, XRp, XRpx).
'lo.comp.typecheck@processStmt'(XSt, XProgramType, XDefs, XDfx, XE, X_2297, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XSt, ":-", XLc, XL, XR),
    'lo.comp.typecheck@checkClause'(XLc, XL, XR, XProgramType, XDefs, XDfx, XE, XRp, XRpx).
'lo.comp.typecheck@processStmt'(XSt, 'lo.comp.types#predType'(XAT), 'lo.core#,..'('lo.comp.canon#clause'(XLc, XNm, XArgs, 'lo.comp.canon#trueCond'), XDefs), XDefs, XE, X_2298, XRp, XRpx) :- 'lo.comp.typecheck@splitHead'(XSt, XLc, XNm, XA),
    'lo.comp.dict@pushScope'(XE, XX35806),
    'lo.comp.typecheck@typeOfArg'(XA, XAT, XX35806, XE0, XArgs, XRp, XRpx).
'lo.comp.typecheck@processStmt'(XSt, XTp, XDefs, XDx, XEnv, X_2299, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XSt, "=", XLc, XL, XR),
    'lo.comp.typecheck@checkDefn'(XLc, XL, 'lo.comp.ast#iden'(XLc, "true"), XR, XTp, XDefs, XDx, XEnv, XRp, XRpx).
'lo.comp.typecheck@processStmt'(XSt, XProgramType, XDefs, XDefx, XE, XPth, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XSt, "<=", XLc, XH, XR),
    'lo.comp.typecheck@checkClassRule'(XLc, XH, 'lo.comp.ast#iden'(XLc, "true"), XR, XProgramType, XDefs, XDefx, XE, XPth, XRp, XRpx).
'lo.comp.typecheck@processStmt'(XSt, XTp, XDefs, XDx, XE, X_2300, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XSt, "-->", XLc, XL, XR),
    'lo.comp.typecheck@checkGrammarRule'(XLc, XL, XR, XTp, XDefs, XDx, XE, XRp, XRpx).
'lo.comp.typecheck@processStmt'(XSt, XTp, XDefs, XDefs, X_2301, X_2302, XRp, XRpx) :- ocall('disp%2'(XSt, XX35889),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('disp%2'(XTp, XX35893),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Statement "), 'lo.core#,..'(XX35889, 'lo.core#,..'('lo.core#ss'(" not consistent with expected type "), 'lo.core#,..'(XX35893, 'lo.core#[]'))))), XX35901),
    ocall('loc%1'(XXV75),XSt,XSt),
    'lo.comp.errors@reportError'(XX35901, XXV75, XRp, XRpx).
'lo.comp.typecheck@processStmts'('lo.core#[]', X_2303, XDefs, XDefs, X_2304, X_2305, XRp, XRp).
'lo.comp.typecheck@processStmts'('lo.core#,..'(XSt, XMore), XProgramType, XDefs, XDx, XEnv, XPath, XRp, XRpx) :- 'lo.comp.typecheck@one44'(XRp0, XRp, XPath, XEnv, XD0, XDefs, XProgramType, XSt),
    'lo.comp.typecheck@processStmts'(XMore, XProgramType, XD0, XDx, XEnv, XPath, XRp0, XRpx).
'lo.comp.typecheck@checkVarRules'('lo.core#[]', X_2306, XDefs, XDefs, X_2307, XRp, XRp).
'lo.comp.typecheck@checkVarRules'('lo.core#,..'('lo.comp.dependencies#defn'(XN, 'lo.comp.abstract#valu', XLc, X_2308, XStmts), XMore), XEnv, XDefs, XDx, XPath, XRp, XRpx) :- 'lo.comp.typecheck@pickupVarType'(XN, XLc, XEnv, XRp, XRp0, XX35967),
    'lo.comp.typecheck@pickupThisType'(XEnv, XX35969),
    ocall('skolemize%3'(XX35967, XX35969, XX35970),'lo.comp.freshen$freshen$lo.comp.types*tipe','lo.comp.freshen$freshen$lo.comp.types*tipe'),
    (XQ, XPT) = XX35970,
    ocall('pairs%2'(XQ, XX35973),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.typecheck@declareTypeVars'(XX35973, XLc, XEnv, XSEnv),
    'lo.comp.types@moveConstraints'(XPT, XCx, XProgramType),
    'lo.comp.typecheck@declareConstraints'(XCx, XSEnv, XStmtEnv),
    'lo.comp.typecheck@processStmts'(XStmts, XProgramType, XRules, 'lo.core#[]', XStmtEnv, XPath, XRp0, XRp1),
    'lo.comp.typecheck@collectPrograms'(XRules, XEnv, XCx, XDefs, XD0),
    'lo.comp.typecheck@checkVarRules'(XMore, XEnv, XD0, XDx, XPath, XRp1, XRpx).
'lo.comp.typecheck@varGroup'(XGrp, XFields, XAnnots, XDefs, XDx, XBase, XEnv, XPath, XRp, XRpx) :- 'lo.comp.typecheck@one45'(XRp0, XRp, XPath, XEnv, XBase, XAnnots, XFields, XGrp),
    'lo.comp.typecheck@checkVarRules'(XGrp, XEnv, XDefs, XDx, XPath, XRp0, XRpx).
'lo.comp.typecheck@checkGroup'(XGrp, 'lo.core#,..'('lo.comp.dependencies#defn'(X_2309, 'lo.comp.abstract#tpe', X_2310, X_2311, X_2312), X_2313), X_2314, X_2315, XDefs, XDx, XEnv, XEx, XPath, XRp, XRpx) :- 'lo.comp.typecheck@typeGroup'(XGrp, XDefs, XDx, XEnv, XEx, XPath, XRp, XRpx).
'lo.comp.typecheck@checkGroup'(XGrp, 'lo.core#,..'('lo.comp.dependencies#defn'(X_2316, 'lo.comp.abstract#valu', X_2317, X_2318, X_2319), X_2320), XFields, XAnnots, XDefs, XDx, XEnv, XEx, XPath, XRp, XRpx) :- 'lo.comp.typecheck@varGroup'(XGrp, XFields, XAnnots, XDefs, XDx, XEnv, XEx, XPath, XRp, XRpx).
'lo.comp.typecheck@checkGroup'(XGrp, 'lo.core#,..'('lo.comp.dependencies#defn'(X_2321, 'lo.comp.abstract#con', X_2322, X_2323, X_2324), X_2325), X_2326, X_2327, XDefs, XDx, XEnv, XEx, XPath, XRp, XRpx) :- 'lo.comp.typecheck@contractGroup'(XGrp, XDefs, XDx, XEnv, XEx, XPath, XRp, XRpx).
'lo.comp.typecheck@checkGroup'(XGrp, 'lo.core#,..'('lo.comp.dependencies#defn'(X_2328, 'lo.comp.abstract#impl', X_2329, X_2330, X_2331), X_2332), X_2333, X_2334, XDefs, XDx, XEnv, XEx, XPath, XRp, XRpx) :- 'lo.comp.typecheck@implementationGroup'(XGrp, XDefs, XDx, XEnv, XEx, XPath, XRp, XRpx).
'lo.comp.typecheck@checkGroups'('lo.core#[]', X_2335, X_2336, 'lo.core#[]', XE, XE, X_2337, XRp, XRp).
'lo.comp.typecheck@checkGroups'('lo.core#,..'(XGp, XMore), XFields, XAnnots, XDefs, XEnv, XE, XPath, XRp, XRpx) :- 'lo.comp.typecheck@checkGroup'(XGp, XGp, XFields, XAnnots, XDefs, XD0, XEnv, XE0, XPath, XRp, XRp0),
    'lo.comp.typecheck@checkGroups'(XMore, XFields, XAnnots, XD0, XE0, XE, XPath, XRp0, XRpx).
'lo.comp.typecheck@thetaEnv'(Xlo_repo_repository_r5, XPath, XRepo, XLc, XEls, XFields, XBase, XTheEnv, XDefs, XPublic, XImports, XImportClosure, XOthers, XRp, XRpx) :- 'lo.comp.macro@macroRewrite'(XEls, XStmts, XRp, XRp0),
    'lo.comp.dependencies@dependencies'(XStmts, XGroups, XPublic, XAnnots, XImps, XOtrs, XRp0, XRp1),
    'lo.comp.typecheck@processImportGroup'(Xlo_repo_repository_r5, XImps, XImports, XImportClosure, XLc, XRepo, XBase, XIBase, XRp1, XRp2),
    'lo.comp.typecheck@pushFace'(XFields, XLc, XIBase, XX36220),
    'lo.comp.typecheck@checkGroups'(XGroups, XFields, XAnnots, XDefs, XX36220, XTheEnv, XPath, XRp2, XRp3),
    'lo.comp.typecheck@checkOthers'(XOtrs, XOthers, XTheEnv, XPath, XRp3, XRpx).
'lo.comp.typecheck@findImportedImplementations'('lo.core#,..'('lo.comp.package#pkgSpec'(X_2338, X_2339, X_2340, X_2341, X_2342, XImpls, X_2343), XSpecs), XD0, XD1) :- 'lo.list@<>'(XImpls, XD0, XX36246),
    'lo.comp.typecheck@findImportedImplementations'(XSpecs, XX36246, XD1).
'lo.comp.typecheck@findImportedImplementations'('lo.core#[]', XD, XD).
'lo.comp.typecheck@checkProgram'(Xlo_repo_repository_r6, XProg, XV, XRepo, XRp, XRpx, 'lo.comp.canon#canonPkg'('lo.comp.package#pkgSpec'('lo.repo#pkg'(XPkgNm, XV), XExports, XTypes, XEnums, XContracts, XImpls, XImports), XImportClosure, XODefs, XOOthers)) :- 'lo.comp.abstract@isBraceTerm'(XProg, XLc, XPk, XEls),
    'lo.comp.abstract@packageName'(XPk, XX36263),
    XPkgNm = XX36263,
    'lo.comp.dict@stdDict'(XX36270),
    'lo.comp.dict@pushScope'(XX36270, XX36271),
    'lo.comp.typecheck@thetaEnv'(Xlo_repo_repository_r6, XPkgNm, XRepo, XLc, XEls, 'lo.core#[]', XX36271, X_2344, XDefs, XPublic, XImports, XImportClosure, XOthers, XRp, XRp0),
    'lo.comp.typecheck@cond35'(XX36295, XRpx, XOthers, XOOthers, XRp1, XODefs, XODict, XDefs, XOverDict, XImportClosure, XRp0),
    'lo.comp.typecheck@computeExport'(XODefs, 'lo.core#[]', XPublic, XExports, XTypes, XEnums, XContracts, XImpls),
    !.
'lo.comp.typecheck@checkProgram'(_, _, _, _, _, _) :- raise_exception('error'("checkProgram", 25, 3, 626)).
'lo.comp.typecheck@exportViz'(XNm, XK, XP, 'lo.comp.package#pUblic') :- ocall('in%2'((XNm, XK), XP),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !.
'lo.comp.typecheck@exportViz'(X_2345, X_2346, X_2347, 'lo.comp.package#priVate') :- !.
'lo.comp.typecheck@exportViz'(_, _, _, _) :- raise_exception('error'("exportViz", 960, 3, 42)).
'lo.comp.typecheck@computeImports'(X_2348, 'lo.core#[]', 'lo.core#[]') :- !.
'lo.comp.typecheck@computeImports'(XPub, 'lo.core#,..'('lo.comp.package#pkgSpec'('lo.repo#pkg'(XPk, XV), X_2349, X_2350, X_2351, X_2352, X_2353, X_2354), XPkgs), 'lo.core#,..'((XX36355, 'lo.repo#pkg'(XPk, XV)), XX36361)) :- !,
    'lo.comp.typecheck@exportViz'(XPk, 'lo.comp.abstract#imp', XPub, XX36355),
    'lo.comp.typecheck@computeImports'(XPub, XPkgs, XX36361).
'lo.comp.typecheck@computeImports'(_, _, _) :- raise_exception('error'("computeImports", 39, 3, 24)).
'lo.comp.typecheck@addPublicImports'('lo.core#[]', X_2355, 'lo.core#[]').
'lo.comp.typecheck@addPublicImports'('lo.core#,..'(('lo.comp.package#pUblic', XPkg), XI), XLc, 'lo.core#,..'((XLc, 'lo.comp.package#pUblic', XPkg), XRest)) :- 'lo.comp.typecheck@addPublicImports'(XI, XLc, XRest).
'lo.comp.typecheck@addPublicImports'('lo.core#,..'(('lo.comp.package#priVate', X_2356), XI), XLc, XRest) :- 'lo.comp.typecheck@addPublicImports'(XI, XLc, XRest).
'lo.comp.typecheck@mkTypes'(0, 'lo.core#[]') :- !.
'lo.comp.typecheck@mkTypes'(XN, 'lo.core#,..'(XX36390, XX36394)) :- !,
    'lo.comp.types@newVar'("_", XX36390),
    ocall('-%3'(XN, 1, XX36392),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.typecheck@mkTypes'(XX36392, XX36394).
'lo.comp.typecheck@mkTypes'(_, _) :- raise_exception('error'("mkTypes", 915, 3, 16)).
'lo.comp.typecheck@pickVar'('lo.comp.dict#vr'(XTrm, X_2357), XTrm) :- !.
'lo.comp.typecheck@pickVar'(_, _) :- raise_exception('error'("pickVar", 922, 3, 25)).
'lo.comp.typecheck@computeNewVars'(XEnv, XX36407) :- !,
    'lo.comp.dict@topVars'(XEnv, XX36402),
    ocall('values%2'(XX36402, XX36403),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('//%3'(XX36403, 'lo.comp.typecheck^pickVar', XX36407),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list').
'lo.comp.typecheck@computeNewVars'(_, _) :- raise_exception('error'("computeNewVars", 919, 3, 54)).
'lo.comp.typecheck^findImport'('_call%3'(XV4143, XV4144, XV4145), 'lo.comp.typecheck^findImport', _) :- 'lo.comp.typecheck@findImport'(XV4143, XV4144, XV4145).
'lo.comp.typecheck^findAllImports'('_call%2'(XV4146, XV4147), 'lo.comp.typecheck^findAllImports', _) :- 'lo.comp.typecheck@findAllImports'(XV4146, XV4147).
'lo.comp.typecheck^second'('_call%2'(XV4148, XV4149), 'lo.comp.typecheck^second', _) :- 'lo.comp.typecheck@second'(XV4148, XV4149).
'lo.comp.typecheck^formMethods'('_call%6'(XV4150, XV4151, XV4152, XV4153, XV4154, XV4155), 'lo.comp.typecheck^formMethods', _) :- 'lo.comp.typecheck@formMethods'(XV4150, XV4151, XV4152, XV4153, XV4154, XV4155).
'lo.comp.typecheck^declareMethods'('_call%5'(XV4156, XV4157, XV4158, XV4159, XV4160), 'lo.comp.typecheck^declareMethods', _) :- 'lo.comp.typecheck@declareMethods'(XV4156, XV4157, XV4158, XV4159, XV4160).
'lo.comp.typecheck^defineContract'('_call%4'(XV4161, XV4162, XV4163, XV4164), 'lo.comp.typecheck^defineContract', _) :- 'lo.comp.typecheck@defineContract'(XV4161, XV4162, XV4163, XV4164).
'lo.comp.typecheck^importContracts'('_call%4'(XV4165, XV4166, XV4167, XV4168), 'lo.comp.typecheck^importContracts', _) :- 'lo.comp.typecheck@importContracts'(XV4165, XV4166, XV4167, XV4168).
'lo.comp.typecheck^pickTypeTemplate'('_call%2'(XV4169, XV4170), 'lo.comp.typecheck^pickTypeTemplate', _) :- 'lo.comp.typecheck@pickTypeTemplate'(XV4169, XV4170).
'lo.comp.typecheck^importTypes'('_call%4'(XV4171, XV4172, XV4173, XV4174), 'lo.comp.typecheck^importTypes', _) :- 'lo.comp.typecheck@importTypes'(XV4171, XV4172, XV4173, XV4174).
'lo.comp.typecheck^declareFields'('_call%4'(XV4175, XV4176, XV4177, XV4178), 'lo.comp.typecheck^declareFields', _) :- 'lo.comp.typecheck@declareFields'(XV4175, XV4176, XV4177, XV4178).
'lo.comp.typecheck^importDefs'('_call%4'(XV4179, XV4180, XV4181, XV4182), 'lo.comp.typecheck^importDefs', _) :- 'lo.comp.typecheck@importDefs'(XV4179, XV4180, XV4181, XV4182).
'lo.comp.typecheck^importAllDefs'('_call%4'(XV4183, XV4184, XV4185, XV4186), 'lo.comp.typecheck^importAllDefs', _) :- 'lo.comp.typecheck@importAllDefs'(XV4183, XV4184, XV4185, XV4186).
'lo.comp.typecheck^processImportGroup'('_call%9'(XV4187, XV4188, XV4189, XV4190, XV4191, XV4192, XV4193, XV4194, XV4195), 'lo.comp.typecheck^processImportGroup', _) :- 'lo.comp.typecheck@processImportGroup'(XV4187, XV4188, XV4189, XV4190, XV4191, XV4192, XV4193, XV4194, XV4195).
'lo.comp.typecheck@cond22'(XX31196, XPath, XTpCore, XSt, XEx, XRpx, XRp, XLc, XX31177, XX31169, XX31165, X_2051, XOLc, XEnv, XN) :- 'lo.comp.dict@isType'(XN, XEnv, XOLc, X_2051),
    !,
    ocall('disp%2'(XN, XX31165),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('disp%2'(XOLc, XX31169),'lo.core$display$lo.comp.location*location','lo.core$display$lo.comp.location*location'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("type "), 'lo.core#,..'(XX31165, 'lo.core#,..'('lo.core#ss'(" already defined at "), 'lo.core#,..'(XX31169, 'lo.core#[]'))))), XX31177),
    'lo.comp.errors@reportError'(XX31177, XLc, XRp, XRpx),
    XEx = XEnv.
'lo.comp.typecheck@cond22'(XX31196, XPath, XTpCore, XSt, XEx, XRpx, XRp, XLc, XX31177, XX31169, XX31165, X_2051, XOLc, XEnv, XN) :- 'lo.comp.parseType@parseTypeCore'(XSt, XTpCore, XPath),
    XRp = XRpx,
    'lo.comp.dict@declareType'(XN, 'lo.comp.dict#tpDef'(XLc, XTpCore, 'lo.comp.types#faceType'('lo.core#[]')), XEnv, XX31196),
    XEx = XX31196.
'lo.comp.typecheck^defineType'('_call%8'(XV4196, XV4197, XV4198, XV4199, XV4200, XV4201, XV4202, XV4203), 'lo.comp.typecheck^defineType', _) :- 'lo.comp.typecheck@defineType'(XV4196, XV4197, XV4198, XV4199, XV4200, XV4201, XV4202, XV4203).
'lo.comp.typecheck^defineTypes'('_call%6'(XV4204, XV4205, XV4206, XV4207, XV4208, XV4209), 'lo.comp.typecheck^defineTypes', _) :- 'lo.comp.typecheck@defineTypes'(XV4204, XV4205, XV4206, XV4207, XV4208, XV4209).
'lo.comp.typecheck^parseTypeDefinition'('_call%9'(XV4210, XV4211, XV4212, XV4213, XV4214, XV4215, XV4216, XV4217, XV4218), 'lo.comp.typecheck^parseTypeDefinition', _) :- 'lo.comp.typecheck@parseTypeDefinition'(XV4210, XV4211, XV4212, XV4213, XV4214, XV4215, XV4216, XV4217, XV4218).
'lo.comp.typecheck^parseTypeDefs'('_call%7'(XV4219, XV4220, XV4221, XV4222, XV4223, XV4224, XV4225), 'lo.comp.typecheck^parseTypeDefs', _) :- 'lo.comp.typecheck@parseTypeDefs'(XV4219, XV4220, XV4221, XV4222, XV4223, XV4224, XV4225).
'lo.comp.typecheck^declareTypes'('_call%5'(XV4226, XV4227, XV4228, XV4229, XV4230), 'lo.comp.typecheck^declareTypes', _) :- 'lo.comp.typecheck@declareTypes'(XV4226, XV4227, XV4228, XV4229, XV4230).
'lo.comp.typecheck^typeGroup'('_call%8'(XV4231, XV4232, XV4233, XV4234, XV4235, XV4236, XV4237, XV4238), 'lo.comp.typecheck^typeGroup', _) :- 'lo.comp.typecheck@typeGroup'(XV4231, XV4232, XV4233, XV4234, XV4235, XV4236, XV4237, XV4238).
'lo.comp.typecheck@one36'(XAnnots, XAnnot, XNm) :- ocall('in%2'((XNm, XAnnot), XAnnots),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !.
'lo.comp.typecheck@one37'(XFields, XTp, XN) :- ocall('in%2'((XN, XTp), XFields),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !.
'lo.comp.typecheck^parseAnnotations'('_call%8'(XV4239, XV4240, XV4241, XV4242, XV4243, XV4244, XV4245, XV4246), 'lo.comp.typecheck^parseAnnotations', _) :- 'lo.comp.typecheck@parseAnnotations'(XV4239, XV4240, XV4241, XV4242, XV4243, XV4244, XV4245, XV4246).
'lo.comp.typecheck^pickupVarType'('_call%6'(XV4247, XV4248, XV4249, XV4250, XV4251, XV4252), 'lo.comp.typecheck^pickupVarType', _) :- 'lo.comp.typecheck@pickupVarType'(XV4247, XV4248, XV4249, XV4250, XV4251, XV4252).
'lo.comp.typecheck^pickupThisType'('_call%2'(XV4253, XV4254), 'lo.comp.typecheck^pickupThisType', _) :- 'lo.comp.typecheck@pickupThisType'(XV4253, XV4254).
'lo.comp.typecheck^declareTypeVars'('_call%4'(XV4255, XV4256, XV4257, XV4258), 'lo.comp.typecheck^declareTypeVars', _) :- 'lo.comp.typecheck@declareTypeVars'(XV4255, XV4256, XV4257, XV4258).
'lo.comp.typecheck^declareConstraints'('_call%3'(XV4259, XV4260, XV4261), 'lo.comp.typecheck^declareConstraints', _) :- 'lo.comp.typecheck@declareConstraints'(XV4259, XV4260, XV4261).
'lo.comp.typecheck^splitHead'('_call%4'(XV4262, XV4263, XV4264, XV4265), 'lo.comp.typecheck^splitHead', _) :- 'lo.comp.typecheck@splitHead'(XV4262, XV4263, XV4264, XV4265).
'lo.comp.typecheck^genTpVars'('_call%2'(XV4266, XV4267), 'lo.comp.typecheck^genTpVars', _) :- 'lo.comp.typecheck@genTpVars'(XV4266, XV4267).
'lo.comp.typecheck@cond23'(XLc, XX31650, XX31643, XX31639, XRpx, XRp, XEnv, XExpected, XActual) :- 'lo.comp.unify@sameType'(XActual, XExpected, XEnv),
    !,
    XRp = XRpx.
'lo.comp.typecheck@cond23'(XLc, XX31650, XX31643, XX31639, XRpx, XRp, XEnv, XExpected, XActual) :- ocall('disp%2'(XActual, XX31639),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('disp%2'(XExpected, XX31643),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XX31639, 'lo.core#,..'('lo.core#ss'(" not consistent with "), 'lo.core#,..'(XX31643, 'lo.core#[]')))), XX31650),
    'lo.comp.errors@reportError'(XX31650, XLc, XRp, XRpx).
'lo.comp.typecheck^checkType'('_call%6'(XV4268, XV4269, XV4270, XV4271, XV4272, XV4273), 'lo.comp.typecheck^checkType', _) :- 'lo.comp.typecheck@checkType'(XV4268, XV4269, XV4270, XV4271, XV4272, XV4273).
'lo.comp.typecheck^manageConstraints'('_call%6'(XV4274, XV4275, XV4276, XV4277, XV4278, XV4279), 'lo.comp.typecheck^manageConstraints', _) :- 'lo.comp.typecheck@manageConstraints'(XV4274, XV4275, XV4276, XV4277, XV4278, XV4279).
'lo.comp.typecheck^typeOfVar'('_call%7'(XV4280, XV4281, XV4282, XV4283, XV4284, XV4285, XV4286), 'lo.comp.typecheck^typeOfVar', _) :- 'lo.comp.typecheck@typeOfVar'(XV4280, XV4281, XV4282, XV4283, XV4284, XV4285, XV4286).
'lo.comp.typecheck^findType'('_call%6'(XV4287, XV4288, XV4289, XV4290, XV4291, XV4292), 'lo.comp.typecheck^findType', _) :- 'lo.comp.typecheck@findType'(XV4287, XV4288, XV4289, XV4290, XV4291, XV4292).
'lo.comp.typecheck^checkGrammarType'('_call%6'(XV4293, XV4294, XV4295, XV4296, XV4297, XV4298), 'lo.comp.typecheck^checkGrammarType', _) :- 'lo.comp.typecheck@checkGrammarType'(XV4293, XV4294, XV4295, XV4296, XV4297, XV4298).
'lo.comp.typecheck^makeIntList'('_call%3'(XV4299, XV4300, XV4301), 'lo.comp.typecheck^makeIntList', _) :- 'lo.comp.typecheck@makeIntList'(XV4299, XV4300, XV4301).
'lo.comp.typecheck^explodeStringLit'('_call%3'(XV4302, XV4303, XV4304), 'lo.comp.typecheck^explodeStringLit', _) :- 'lo.comp.typecheck@explodeStringLit'(XV4302, XV4303, XV4304).
'lo.comp.typecheck@one38'(XFields, XTp, XNm) :- ocall('in%2'((XNm, XTp), XFields),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !.
'lo.comp.typecheck^fieldInFace'('_call%7'(XV4305, XV4306, XV4307, XV4308, XV4309, XV4310, XV4311), 'lo.comp.typecheck^fieldInFace', _) :- 'lo.comp.typecheck@fieldInFace'(XV4305, XV4306, XV4307, XV4308, XV4309, XV4310, XV4311).
'lo.comp.typecheck^isMapSequence'('_call%1'(XV4312), 'lo.comp.typecheck^isMapSequence', _) :- 'lo.comp.typecheck@isMapSequence'(XV4312).
'lo.comp.typecheck^isMapType'('_call%3'(XV4313, XV4314, XV4315), 'lo.comp.typecheck^isMapType', _) :- 'lo.comp.typecheck@isMapType'(XV4313, XV4314, XV4315).
'lo.comp.typecheck^macroMapEntries'('_call%5'(XV4316, XV4317, XV4318, XV4319, XV4320), 'lo.comp.typecheck^macroMapEntries', _) :- 'lo.comp.typecheck@macroMapEntries'(XV4316, XV4317, XV4318, XV4319, XV4320).
'lo.comp.typecheck@neg36'(X_2105, X_2104, X_2103, XE) :- 'lo.comp.abstract@isBinary'(XE, "->", X_2103, X_2104, X_2105),
    !,
    fail.
'lo.comp.typecheck@neg36'(X_2105, X_2104, X_2103, XE).
'lo.comp.typecheck^isListSequence'('_call%1'(XV4321), 'lo.comp.typecheck^isListSequence', _) :- 'lo.comp.typecheck@isListSequence'(XV4321).
'lo.comp.typecheck^isListType'('_call%3'(XV4322, XV4323, XV4324), 'lo.comp.typecheck^isListType', _) :- 'lo.comp.typecheck@isListType'(XV4322, XV4323, XV4324).
'lo.comp.typecheck^macroSequenceTerm'('_call%4'(XV4325, XV4326, XV4327, XV4328), 'lo.comp.typecheck^macroSequenceTerm', _) :- 'lo.comp.typecheck@macroSequenceTerm'(XV4325, XV4326, XV4327, XV4328).
'lo.comp.typecheck^splitGrHead'('_call%4'(XV4329, XV4330, XV4331, XV4332), 'lo.comp.typecheck^splitGrHead', _) :- 'lo.comp.typecheck@splitGrHead'(XV4329, XV4330, XV4331, XV4332).
'lo.comp.typecheck^isPublicVar'('_call%3'(XV4333, XV4334, XV4335), 'lo.comp.typecheck^isPublicVar', _) :- 'lo.comp.typecheck@isPublicVar'(XV4333, XV4334, XV4335).
'lo.comp.typecheck^isPublicType'('_call%2'(XV4336, XV4337), 'lo.comp.typecheck^isPublicType', _) :- 'lo.comp.typecheck@isPublicType'(XV4336, XV4337).
'lo.comp.typecheck^isPublicImplementation'('_call%2'(XV4338, XV4339), 'lo.comp.typecheck^isPublicImplementation', _) :- 'lo.comp.typecheck@isPublicImplementation'(XV4338, XV4339).
'lo.comp.typecheck@one39'(XPublic, XINm) :- 'lo.comp.typecheck@isPublicImplementation'(XINm, XPublic),
    !.
'lo.comp.typecheck^exportDef'('_call%13'(XV4340, XV4341, XV4342, XV4343, XV4344, XV4345, XV4346, XV4347, XV4348, XV4349, XV4350, XV4351, XV4352), 'lo.comp.typecheck^exportDef', _) :- 'lo.comp.typecheck@exportDef'(XV4340, XV4341, XV4342, XV4343, XV4344, XV4345, XV4346, XV4347, XV4348, XV4349, XV4350, XV4351, XV4352).
'lo.comp.typecheck@one40'(XIx, XImpls, XCx, XContracts, XClx, XEnums, XTx, XTypes, XEx, XExports, XPublic, XFields, XDef) :- 'lo.comp.typecheck@exportDef'(XDef, XFields, XPublic, XExports, XEx, XTypes, XTx, XEnums, XClx, XContracts, XCx, XImpls, XIx),
    !.
'lo.comp.typecheck^computeExport'('_call%8'(XV4353, XV4354, XV4355, XV4356, XV4357, XV4358, XV4359, XV4360), 'lo.comp.typecheck^computeExport', _) :- 'lo.comp.typecheck@computeExport'(XV4353, XV4354, XV4355, XV4356, XV4357, XV4358, XV4359, XV4360).
'lo.comp.typecheck@neg37'(X_2157, X_2156, X_2155, XNm, X_2154, XRl) :- XRl = 'lo.comp.canon#equation'(X_2154, XNm, X_2155, X_2156, X_2157),
    !,
    fail.
'lo.comp.typecheck@neg37'(X_2157, X_2156, X_2155, XNm, X_2154, XRl).
'lo.comp.typecheck^collectEquations'('_call%4'(XV4361, XV4362, XV4363, XV4364), 'lo.comp.typecheck^collectEquations', _) :- 'lo.comp.typecheck@collectEquations'(XV4361, XV4362, XV4363, XV4364).
'lo.comp.typecheck@neg38'(X_2164, X_2163, XNm, X_2162, XRl) :- XRl = 'lo.comp.canon#clause'(X_2162, XNm, X_2163, X_2164),
    !,
    fail.
'lo.comp.typecheck@neg38'(X_2164, X_2163, XNm, X_2162, XRl).
'lo.comp.typecheck^collectMoreClauses'('_call%4'(XV4365, XV4366, XV4367, XV4368), 'lo.comp.typecheck^collectMoreClauses', _) :- 'lo.comp.typecheck@collectMoreClauses'(XV4365, XV4366, XV4367, XV4368).
'lo.comp.typecheck^collectClauses'('_call%4'(XV4369, XV4370, XV4371, XV4372), 'lo.comp.typecheck^collectClauses', _) :- 'lo.comp.typecheck@collectClauses'(XV4369, XV4370, XV4371, XV4372).
'lo.comp.typecheck^isRuleForClass'('_call%4'(XV4373, XV4374, XV4375, XV4376), 'lo.comp.typecheck^isRuleForClass', _) :- 'lo.comp.typecheck@isRuleForClass'(XV4373, XV4374, XV4375, XV4376).
'lo.comp.typecheck^collectClassRules'('_call%4'(XV4377, XV4378, XV4379, XV4380), 'lo.comp.typecheck^collectClassRules', _) :- 'lo.comp.typecheck@collectClassRules'(XV4377, XV4378, XV4379, XV4380).
'lo.comp.typecheck^isGrammarRule'('_call%3'(XV4381, XV4382, XV4383), 'lo.comp.typecheck^isGrammarRule', _) :- 'lo.comp.typecheck@isGrammarRule'(XV4381, XV4382, XV4383).
'lo.comp.typecheck^collectGrammarRules'('_call%4'(XV4384, XV4385, XV4386, XV4387), 'lo.comp.typecheck^collectGrammarRules', _) :- 'lo.comp.typecheck@collectGrammarRules'(XV4384, XV4385, XV4386, XV4387).
'lo.comp.typecheck^collectPrograms'('_call%5'(XV4388, XV4389, XV4390, XV4391, XV4392), 'lo.comp.typecheck^collectPrograms', _) :- 'lo.comp.typecheck@collectPrograms'(XV4388, XV4389, XV4390, XV4391, XV4392).
'lo.comp.typecheck^contractGroup'('_call%8'(XV4393, XV4394, XV4395, XV4396, XV4397, XV4398, XV4399, XV4400), 'lo.comp.typecheck^contractGroup', _) :- 'lo.comp.typecheck@contractGroup'(XV4393, XV4394, XV4395, XV4396, XV4397, XV4398, XV4399, XV4400).
'lo.comp.typecheck^sameLength'('_call%5'(XV4401, XV4402, XV4403, XV4404, XV4405), 'lo.comp.typecheck^sameLength', _) :- 'lo.comp.typecheck@sameLength'(XV4401, XV4402, XV4403, XV4404, XV4405).
'lo.comp.typecheck^pushFace'('_call%4'(XV4406, XV4407, XV4408, XV4409), 'lo.comp.typecheck^pushFace', _) :- 'lo.comp.typecheck@pushFace'(XV4406, XV4407, XV4408, XV4409).
'lo.comp.typecheck^checkOther'('_call%6'(XV4410, XV4411, XV4412, XV4413, XV4414, XV4415), 'lo.comp.typecheck^checkOther', _) :- 'lo.comp.typecheck@checkOther'(XV4410, XV4411, XV4412, XV4413, XV4414, XV4415).
'lo.comp.typecheck@one41'(XRp0, XRp, XEnv, XMore, XAss, XSt) :- 'lo.comp.typecheck@checkOther'(XSt, XAss, XMore, XEnv, XRp, XRp0),
    !.
'lo.comp.typecheck^checkOthers'('_call%6'(XV4416, XV4417, XV4418, XV4419, XV4420, XV4421), 'lo.comp.typecheck^checkOthers', _) :- 'lo.comp.typecheck@checkOthers'(XV4416, XV4417, XV4418, XV4419, XV4420, XV4421).
'lo.comp.typecheck^implementationGroup'('_call%8'(XV4422, XV4423, XV4424, XV4425, XV4426, XV4427, XV4428, XV4429), 'lo.comp.typecheck^implementationGroup', _) :- 'lo.comp.typecheck@implementationGroup'(XV4422, XV4423, XV4424, XV4425, XV4426, XV4427, XV4428, XV4429).
'lo.comp.typecheck^checkDefn'('_call%10'(XV4430, XV4431, XV4432, XV4433, XV4434, XV4435, XV4436, XV4437, XV4438, XV4439), 'lo.comp.typecheck^checkDefn', _) :- 'lo.comp.typecheck@checkDefn'(XV4430, XV4431, XV4432, XV4433, XV4434, XV4435, XV4436, XV4437, XV4438, XV4439).
'lo.comp.typecheck^checkClassHead'('_call%9'(XV4440, XV4441, XV4442, XV4443, XV4444, XV4445, XV4446, XV4447, XV4448), 'lo.comp.typecheck^checkClassHead', _) :- 'lo.comp.typecheck@checkClassHead'(XV4440, XV4441, XV4442, XV4443, XV4444, XV4445, XV4446, XV4447, XV4448).
'lo.comp.typecheck^checkClassRule'('_call%11'(XV4449, XV4450, XV4451, XV4452, XV4453, XV4454, XV4455, XV4456, XV4457, XV4458, XV4459), 'lo.comp.typecheck^checkClassRule', _) :- 'lo.comp.typecheck@checkClassRule'(XV4449, XV4450, XV4451, XV4452, XV4453, XV4454, XV4455, XV4456, XV4457, XV4458, XV4459).
'lo.comp.typecheck^checkThetaTerm'('_call%10'(XV4460, XV4461, XV4462, XV4463, XV4464, XV4465, XV4466, XV4467, XV4468, XV4469), 'lo.comp.typecheck^checkThetaTerm', _) :- 'lo.comp.typecheck@checkThetaTerm'(XV4460, XV4461, XV4462, XV4463, XV4464, XV4465, XV4466, XV4467, XV4468, XV4469).
'lo.comp.typecheck^typeOfIndex'('_call%9'(XV4470, XV4471, XV4472, XV4473, XV4474, XV4475, XV4476, XV4477, XV4478), 'lo.comp.typecheck^typeOfIndex', _) :- 'lo.comp.typecheck@typeOfIndex'(XV4470, XV4471, XV4472, XV4473, XV4474, XV4475, XV4476, XV4477, XV4478).
'lo.comp.typecheck^typeOfCall'('_call%10'(XV4479, XV4480, XV4481, XV4482, XV4483, XV4484, XV4485, XV4486, XV4487, XV4488), 'lo.comp.typecheck^typeOfCall', _) :- 'lo.comp.typecheck@typeOfCall'(XV4479, XV4480, XV4481, XV4482, XV4483, XV4484, XV4485, XV4486, XV4487, XV4488).
'lo.comp.typecheck^checkGrammarRule'('_call%9'(XV4489, XV4490, XV4491, XV4492, XV4493, XV4494, XV4495, XV4496, XV4497), 'lo.comp.typecheck^checkGrammarRule', _) :- 'lo.comp.typecheck@checkGrammarRule'(XV4489, XV4490, XV4491, XV4492, XV4493, XV4494, XV4495, XV4496, XV4497).
'lo.comp.typecheck^checkClause'('_call%9'(XV4498, XV4499, XV4500, XV4501, XV4502, XV4503, XV4504, XV4505, XV4506), 'lo.comp.typecheck^checkClause', _) :- 'lo.comp.typecheck@checkClause'(XV4498, XV4499, XV4500, XV4501, XV4502, XV4503, XV4504, XV4505, XV4506).
'lo.comp.typecheck^checkSequenceTerm'('_call%8'(XV4507, XV4508, XV4509, XV4510, XV4511, XV4512, XV4513, XV4514), 'lo.comp.typecheck^checkSequenceTerm', _) :- 'lo.comp.typecheck@checkSequenceTerm'(XV4507, XV4508, XV4509, XV4510, XV4511, XV4512, XV4513, XV4514).
'lo.comp.typecheck^typeOfListTerm'('_call%9'(XV4515, XV4516, XV4517, XV4518, XV4519, XV4520, XV4521, XV4522, XV4523), 'lo.comp.typecheck^typeOfListTerm', _) :- 'lo.comp.typecheck@typeOfListTerm'(XV4515, XV4516, XV4517, XV4518, XV4519, XV4520, XV4521, XV4522, XV4523).
'lo.comp.typecheck@or13'(XEnv, XLc, XTp, XEls) :- 'lo.comp.typecheck@isMapSequence'(XEls).
'lo.comp.typecheck@or13'(XEnv, XLc, XTp, XEls) :- 'lo.comp.typecheck@isMapType'(XTp, XLc, XEnv).
'lo.comp.typecheck@or14'(XEnv, XLc, XTp, XEls) :- 'lo.comp.typecheck@isListSequence'(XEls).
'lo.comp.typecheck@or14'(XEnv, XLc, XTp, XEls) :- 'lo.comp.typecheck@isListType'(XTp, XLc, XEnv).
'lo.comp.typecheck@cond24'(XRpx, XExp, XEv, XRp2, XX33646, XRp1, XRp, XListTp, XX33640, XElTp, XEls, XTp, XLc, XEnv) :- 'lo.comp.typecheck@or14'(XEnv, XLc, XTp, XEls),
    !,
    'lo.comp.types@newVar'("El", XX33640),
    XElTp = XX33640,
    'lo.comp.typecheck@findType'("list", XLc, XEnv, XRp, XRp1, XX33646),
    XListTp = 'lo.comp.types#typeExp'(XX33646, 'lo.core#,..'(XElTp, 'lo.core#[]')),
    'lo.comp.typecheck@checkType'(XLc, XListTp, XTp, XEnv, XRp1, XRp2),
    'lo.comp.typecheck@typeOfListTerm'(XEls, XLc, XElTp, XListTp, XEnv, XEv, XExp, XRp2, XRpx).
'lo.comp.typecheck@cond24'(XRpx, XExp, XEv, XRp2, XX33646, XRp1, XRp, XListTp, XX33640, XElTp, XEls, XTp, XLc, XEnv) :- 'lo.comp.typecheck@checkSequenceTerm'(XLc, XEls, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@cond25'(XElTp, XX33640, XListTp, XX33646, XRpx, XExp, XEv, XX33628, XRp3, XRp2, XX33615, XX33614, XX33613, XRp1, XRp, XEls, XTp, XLc, XEnv) :- 'lo.comp.typecheck@or13'(XEnv, XLc, XTp, XEls),
    !,
    'lo.comp.typecheck@findType'("map", XLc, XEnv, XRp, XRp1, XX33613),
    'lo.comp.types@newVar'("K", XX33614),
    'lo.comp.types@newVar'("V", XX33615),
    'lo.comp.typecheck@checkType'(XLc, 'lo.comp.types#typeExp'(XX33613, 'lo.core#,..'(XX33614, 'lo.core#,..'(XX33615, 'lo.core#[]'))), XTp, XEnv, XRp1, XRp2),
    'lo.comp.typecheck@macroMapEntries'(XLc, XEls, XRp2, XRp3, XX33628),
    'lo.comp.typecheck@typeOfTerm'(XX33628, XTp, XEnv, XEv, XExp, XRp3, XRpx).
'lo.comp.typecheck@cond25'(XElTp, XX33640, XListTp, XX33646, XRpx, XExp, XEv, XX33628, XRp3, XRp2, XX33615, XX33614, XX33613, XRp1, XRp, XEls, XTp, XLc, XEnv) :- 'lo.comp.typecheck@cond24'(XRpx, XExp, XEv, XRp2, XX33646, XRp1, XRp, XListTp, XX33640, XElTp, XEls, XTp, XLc, XEnv).
'lo.comp.typecheck^checkSquareTuple'('_call%8'(XV4524, XV4525, XV4526, XV4527, XV4528, XV4529, XV4530, XV4531), 'lo.comp.typecheck^checkSquareTuple', _) :- 'lo.comp.typecheck@checkSquareTuple'(XV4524, XV4525, XV4526, XV4527, XV4528, XV4529, XV4530, XV4531).
'lo.comp.typecheck@one42'(XRp1, XRp0, XFTp, XLc, XFld, XAT, XFields) :- 'lo.comp.typecheck@fieldInFace'(XFields, XAT, XFld, XLc, XFTp, XRp0, XRp1),
    !.
'lo.comp.typecheck^recordAccessExp'('_call%9'(XV4532, XV4533, XV4534, XV4535, XV4536, XV4537, XV4538, XV4539, XV4540), 'lo.comp.typecheck^recordAccessExp', _) :- 'lo.comp.typecheck@recordAccessExp'(XV4532, XV4533, XV4534, XV4535, XV4536, XV4537, XV4538, XV4539, XV4540).
'lo.comp.typecheck@cond26'(XX33755, XX33747, XV, XEv, XRpx, XRp, XTerm, XTp, XLc, XSpec, XEnv, XNm) :- 'lo.comp.dict@isVar'(XNm, XEnv, XSpec),
    !,
    'lo.comp.typecheck@typeOfVar'(XLc, XSpec, XTp, XEnv, XTerm, XRp, XRpx),
    XEnv = XEv.
'lo.comp.typecheck@cond26'(XX33755, XX33747, XV, XEv, XRpx, XRp, XTerm, XTp, XLc, XSpec, XEnv, XNm) :- ocall('disp%2'(XV, XX33747),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("variable "), 'lo.core#,..'(XX33747, 'lo.core#,..'('lo.core#ss'(" not declared"), 'lo.core#[]')))), XX33755),
    'lo.comp.errors@reportError'(XX33755, XLc, XRp, XRpx),
    XTerm = 'lo.comp.canon#v'(XLc, XNm).
'lo.comp.typecheck@cond27'(XEx, XEnv, XSpec, XTp, XTerm, XRp, XRpx, XEv, XX33747, XX33755, XNm, XLc, XV) :- 'lo.comp.abstract@isIden'(XV, XLc, XNm),
    !,
    'lo.comp.typecheck@cond26'(XX33755, XX33747, XV, XEv, XRpx, XRp, XTerm, XTp, XLc, XSpec, XEnv, XNm).
'lo.comp.typecheck@cond27'(XEx, XEnv, XSpec, XTp, XTerm, XRp, XRpx, XEv, XX33747, XX33755, XNm, XLc, XV) :- 'lo.comp.typecheck@typeOfTerm'(XV, XTp, XEnv, XEx, XTerm, XRp, XRpx).
'lo.comp.typecheck^knownType'('_call%7'(XV4541, XV4542, XV4543, XV4544, XV4545, XV4546, XV4547), 'lo.comp.typecheck^knownType', _) :- 'lo.comp.typecheck@knownType'(XV4541, XV4542, XV4543, XV4544, XV4545, XV4546, XV4547).
'lo.comp.typecheck^checkNonTerminals'('_call%8'(XV4548, XV4549, XV4550, XV4551, XV4552, XV4553, XV4554, XV4555), 'lo.comp.typecheck^checkNonTerminals', _) :- 'lo.comp.typecheck@checkNonTerminals'(XV4548, XV4549, XV4550, XV4551, XV4552, XV4553, XV4554, XV4555).
'lo.comp.typecheck^checkTerminals'('_call%8'(XV4556, XV4557, XV4558, XV4559, XV4560, XV4561, XV4562, XV4563), 'lo.comp.typecheck^checkTerminals', _) :- 'lo.comp.typecheck@checkTerminals'(XV4556, XV4557, XV4558, XV4559, XV4560, XV4561, XV4562, XV4563).
'lo.comp.typecheck@one43'(XRpx, XRp, XGrNT, XEx, XEnv, XElTp, XTp, XEls) :- 'lo.comp.typecheck@checkNonTerminals'(XEls, XTp, XElTp, XEnv, XEx, XGrNT, XRp, XRpx),
    !.
'lo.comp.typecheck@neg39'(X_2242, X_2241, X_2240, XL) :- 'lo.comp.abstract@isBinary'(XL, "?", X_2240, X_2241, X_2242),
    !,
    fail.
'lo.comp.typecheck@neg39'(X_2242, X_2241, X_2240, XL).
'lo.comp.typecheck@cond28'(XX34276, XX34266, XX34262, XGrTp, XX34258, XOp, XNT, XRpx, XArg, XEv, XE0, XArgTp, XA, XRp1, XRp0, XEnv, XTp, XStrmTp, XLc) :- 'lo.comp.typecheck@checkType'(XLc, XStrmTp, XTp, XEnv, XRp0, XRp1),
    !,
    'lo.comp.typecheck@typeOfArg'(XA, XArgTp, XE0, XEv, XArg, XRp1, XRpx),
    XNT = 'lo.comp.canon#grCall'(XLc, XOp, XArg).
'lo.comp.typecheck@cond28'(XX34276, XX34266, XX34262, XGrTp, XX34258, XOp, XNT, XRpx, XArg, XEv, XE0, XArgTp, XA, XRp1, XRp0, XEnv, XTp, XStrmTp, XLc) :- ocall('disp%2'(XOp, XX34258),'lo.core$display$lo.comp.canon*canonTerm','lo.core$display$lo.comp.canon*canonTerm'),
    ocall('disp%2'(XGrTp, XX34262),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('disp%2'(XTp, XX34266),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("type of "), 'lo.core#,..'(XX34258, 'lo.core#,..'('lo.core#ss'(" : "), 'lo.core#,..'(XX34262, 'lo.core#,..'('lo.core#ss'(" not consistent with stream type "), 'lo.core#,..'(XX34266, 'lo.core#[]'))))))), XX34276),
    'lo.comp.errors@reportError'(XX34276, XLc, XRp0, XRpx),
    XNT = 'lo.comp.canon#grTerms'('lo.core#[]'),
    XEv = XE0.
'lo.comp.typecheck@cond29'(XX34299, XX34290, XX34286, XF, XLc, XTp, XEnv, XRp0, XRp1, XA, XE0, XEv, XArg, XRpx, XNT, XOp, XX34258, XX34262, XX34266, XX34276, XStrmTp, XArgTp, XX34234, XGrTp) :- 'lo.comp.types@deRef'(XGrTp, XX34234),
    XX34234 = 'lo.comp.types#grammarType'(XArgTp, XStrmTp),
    !,
    'lo.comp.typecheck@cond28'(XX34276, XX34266, XX34262, XGrTp, XX34258, XOp, XNT, XRpx, XArg, XEv, XE0, XArgTp, XA, XRp1, XRp0, XEnv, XTp, XStrmTp, XLc).
'lo.comp.typecheck@cond29'(XX34299, XX34290, XX34286, XF, XLc, XTp, XEnv, XRp0, XRp1, XA, XE0, XEv, XArg, XRpx, XNT, XOp, XX34258, XX34262, XX34266, XX34276, XStrmTp, XArgTp, XX34234, XGrTp) :- ocall('disp%2'(XF, XX34286),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('disp%2'(XGrTp, XX34290),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XX34286, 'lo.core#,..'('lo.core#ss'(" : "), 'lo.core#,..'(XX34290, 'lo.core#,..'('lo.core#ss'(" not a grammar"), 'lo.core#[]'))))), XX34299),
    'lo.comp.errors@reportError'(XX34299, XLc, XRp0, XRpx),
    XNT = 'lo.comp.canon#grTerms'('lo.core#[]'),
    XEv = XEnv.
'lo.comp.typecheck^checkNonTerminal'('_call%8'(XV4564, XV4565, XV4566, XV4567, XV4568, XV4569, XV4570, XV4571), 'lo.comp.typecheck^checkNonTerminal', _) :- 'lo.comp.typecheck@checkNonTerminal'(XV4564, XV4565, XV4566, XV4567, XV4568, XV4569, XV4570, XV4571).
'lo.comp.typecheck^checkConds'('_call%6'(XV4572, XV4573, XV4574, XV4575, XV4576, XV4577), 'lo.comp.typecheck^checkConds', _) :- 'lo.comp.typecheck@checkConds'(XV4572, XV4573, XV4574, XV4575, XV4576, XV4577).
'lo.comp.typecheck@cond30'(XX34795, XX34786, XX34782, XPred, XLc, XCall, XRpx, XRp0, XArg, XEv, XE0, XA, XArgTp, XX34766, XPrTp) :- 'lo.comp.types@deRef'(XPrTp, XX34766),
    XX34766 = 'lo.comp.types#predType'(XArgTp),
    !,
    'lo.comp.typecheck@typeOfArg'(XA, XArgTp, XE0, XEv, XArg, XRp0, XRpx),
    XCall = 'lo.comp.canon#callCond'(XLc, XPred, XArg).
'lo.comp.typecheck@cond30'(XX34795, XX34786, XX34782, XPred, XLc, XCall, XRpx, XRp0, XArg, XEv, XE0, XA, XArgTp, XX34766, XPrTp) :- ocall('disp%2'(XPred, XX34782),'lo.core$display$lo.comp.canon*canonTerm','lo.core$display$lo.comp.canon*canonTerm'),
    ocall('disp%2'(XPrTp, XX34786),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XX34782, 'lo.core#,..'('lo.core#ss'(":"), 'lo.core#,..'(XX34786, 'lo.core#,..'('lo.core#ss'(" not a predicate"), 'lo.core#[]'))))), XX34795),
    'lo.comp.errors@reportError'(XX34795, XLc, XRp0, XRpx),
    XE0 = XEv,
    XCall = 'lo.comp.canon#trueCond'.
'lo.comp.typecheck^checkCond'('_call%6'(XV4578, XV4579, XV4580, XV4581, XV4582, XV4583), 'lo.comp.typecheck^checkCond', _) :- 'lo.comp.typecheck@checkCond'(XV4578, XV4579, XV4580, XV4581, XV4582, XV4583).
'lo.comp.typecheck@cond31'(XX34879, XEnv, XTp, XEv, XX34869, XTerm, XRpx, XRp, XLc, XX34863, XX34857, XNm) :- 'lo.comp.keywords@isKeyword'(XNm),
    !,
    ocall('disp%2'(XNm, XX34857),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("unexpected keyword: "), 'lo.core#,..'(XX34857, 'lo.core#[]'))), XX34863),
    'lo.comp.errors@reportError'(XX34863, XLc, XRp, XRpx),
    '_str_gen'("_", XX34869),
    XTerm = 'lo.comp.canon#v'(XLc, XX34869).
'lo.comp.typecheck@cond31'(XX34879, XEnv, XTp, XEv, XX34869, XTerm, XRpx, XRp, XLc, XX34863, XX34857, XNm) :- 'lo.comp.dict@declareVar'(XNm, 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, XNm), XTp), XEnv, XX34879),
    XEv = XX34879,
    XTerm = 'lo.comp.canon#v'(XLc, XNm),
    XRp = XRpx.
'lo.comp.typecheck@cond32'(XX34857, XX34863, XX34869, XX34879, XEv, XRpx, XRp, XTerm, XTp, XLc, XSpec, XEnv, XNm) :- 'lo.comp.dict@isVar'(XNm, XEnv, XSpec),
    !,
    'lo.comp.typecheck@typeOfVar'(XLc, XSpec, XTp, XEnv, XTerm, XRp, XRpx),
    XEnv = XEv.
'lo.comp.typecheck@cond32'(XX34857, XX34863, XX34869, XX34879, XEv, XRpx, XRp, XTerm, XTp, XLc, XSpec, XEnv, XNm) :- 'lo.comp.typecheck@cond31'(XX34879, XEnv, XTp, XEv, XX34869, XTerm, XRpx, XRp, XLc, XX34863, XX34857, XNm).
'lo.comp.typecheck@cond33'(XX34983, XX34977, XRpx, XExp, XEv, XL, XRp1, XTp, XLc, XRp0, XRp, XRT, XEnv, XR) :- 'lo.comp.parseType@parseType'(XR, XEnv, XRT, XRp, XRp0),
    !,
    'lo.comp.typecheck@checkType'(XLc, XRT, XTp, XEnv, XRp0, XRp1),
    'lo.comp.typecheck@typeOfTerm'(XL, XRT, XEnv, XEv, XExp, XRp1, XRpx).
'lo.comp.typecheck@cond33'(XX34983, XX34977, XRpx, XExp, XEv, XL, XRp1, XTp, XLc, XRp0, XRp, XRT, XEnv, XR) :- ocall('disp%2'(XR, XX34977),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot parse type "), 'lo.core#,..'(XX34977, 'lo.core#[]'))), XX34983),
    'lo.comp.errors@reportError'(XX34983, XLc, XRp0, XRp1),
    'lo.comp.typecheck@typeOfTerm'(XL, XTp, XEnv, XEv, XExp, XRp1, XRpx).
'lo.comp.typecheck@neg40'(X_2274, X_2273, XInner) :- 'lo.comp.abstract@isRoundTuple'(XInner, X_2273, X_2274),
    !,
    fail.
'lo.comp.typecheck@neg40'(X_2274, X_2273, XInner).
'lo.comp.typecheck^typeOfTerm'('_call%7'(XV4584, XV4585, XV4586, XV4587, XV4588, XV4589, XV4590), 'lo.comp.typecheck^typeOfTerm', _) :- 'lo.comp.typecheck@typeOfTerm'(XV4584, XV4585, XV4586, XV4587, XV4588, XV4589, XV4590).
'lo.comp.typecheck^typeOfTerms'('_call%8'(XV4591, XV4592, XV4593, XV4594, XV4595, XV4596, XV4597, XV4598), 'lo.comp.typecheck^typeOfTerms', _) :- 'lo.comp.typecheck@typeOfTerms'(XV4591, XV4592, XV4593, XV4594, XV4595, XV4596, XV4597, XV4598).
'lo.comp.typecheck@cond34'(XExp, XRpx, XEls, XEv, XRp0, XRp, XEnv, XTp, XX35595, XArgTps, XA, XLc, XTerm) :- 'lo.comp.abstract@isRoundTuple'(XTerm, XLc, XA),
    !,
    'lo.comp.typecheck@genTpVars'(XA, XX35595),
    XArgTps = XX35595,
    'lo.comp.typecheck@checkType'(XLc, 'lo.comp.types#tupleType'(XArgTps), XTp, XEnv, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerms'(XA, XArgTps, XEnv, XEv, XLc, XEls, XRp0, XRpx),
    XExp = 'lo.comp.canon#tpl'(XEls).
'lo.comp.typecheck@cond34'(XExp, XRpx, XEls, XEv, XRp0, XRp, XEnv, XTp, XX35595, XArgTps, XA, XLc, XTerm) :- 'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck^typeOfArg'('_call%7'(XV4599, XV4600, XV4601, XV4602, XV4603, XV4604, XV4605), 'lo.comp.typecheck^typeOfArg', _) :- 'lo.comp.typecheck@typeOfArg'(XV4599, XV4600, XV4601, XV4602, XV4603, XV4604, XV4605).
'lo.comp.typecheck^checkEquation'('_call%10'(XV4606, XV4607, XV4608, XV4609, XV4610, XV4611, XV4612, XV4613, XV4614, XV4615), 'lo.comp.typecheck^checkEquation', _) :- 'lo.comp.typecheck@checkEquation'(XV4606, XV4607, XV4608, XV4609, XV4610, XV4611, XV4612, XV4613, XV4614, XV4615).
'lo.comp.typecheck^processStmt'('_call%8'(XV4616, XV4617, XV4618, XV4619, XV4620, XV4621, XV4622, XV4623), 'lo.comp.typecheck^processStmt', _) :- 'lo.comp.typecheck@processStmt'(XV4616, XV4617, XV4618, XV4619, XV4620, XV4621, XV4622, XV4623).
'lo.comp.typecheck@one44'(XRp0, XRp, XPath, XEnv, XD0, XDefs, XProgramType, XSt) :- 'lo.comp.typecheck@processStmt'(XSt, XProgramType, XDefs, XD0, XEnv, XPath, XRp, XRp0),
    !.
'lo.comp.typecheck^processStmts'('_call%8'(XV4624, XV4625, XV4626, XV4627, XV4628, XV4629, XV4630, XV4631), 'lo.comp.typecheck^processStmts', _) :- 'lo.comp.typecheck@processStmts'(XV4624, XV4625, XV4626, XV4627, XV4628, XV4629, XV4630, XV4631).
'lo.comp.typecheck^checkVarRules'('_call%7'(XV4632, XV4633, XV4634, XV4635, XV4636, XV4637, XV4638), 'lo.comp.typecheck^checkVarRules', _) :- 'lo.comp.typecheck@checkVarRules'(XV4632, XV4633, XV4634, XV4635, XV4636, XV4637, XV4638).
'lo.comp.typecheck@one45'(XRp0, XRp, XPath, XEnv, XBase, XAnnots, XFields, XGrp) :- 'lo.comp.typecheck@parseAnnotations'(XGrp, XFields, XAnnots, XBase, XEnv, XPath, XRp, XRp0),
    !.
'lo.comp.typecheck^varGroup'('_call%10'(XV4639, XV4640, XV4641, XV4642, XV4643, XV4644, XV4645, XV4646, XV4647, XV4648), 'lo.comp.typecheck^varGroup', _) :- 'lo.comp.typecheck@varGroup'(XV4639, XV4640, XV4641, XV4642, XV4643, XV4644, XV4645, XV4646, XV4647, XV4648).
'lo.comp.typecheck^checkGroup'('_call%11'(XV4649, XV4650, XV4651, XV4652, XV4653, XV4654, XV4655, XV4656, XV4657, XV4658, XV4659), 'lo.comp.typecheck^checkGroup', _) :- 'lo.comp.typecheck@checkGroup'(XV4649, XV4650, XV4651, XV4652, XV4653, XV4654, XV4655, XV4656, XV4657, XV4658, XV4659).
'lo.comp.typecheck^checkGroups'('_call%9'(XV4660, XV4661, XV4662, XV4663, XV4664, XV4665, XV4666, XV4667, XV4668), 'lo.comp.typecheck^checkGroups', _) :- 'lo.comp.typecheck@checkGroups'(XV4660, XV4661, XV4662, XV4663, XV4664, XV4665, XV4666, XV4667, XV4668).
'lo.comp.typecheck^thetaEnv'('_call%14'(XV4669, XV4670, XV4671, XV4672, XV4673, XV4674, XV4675, XV4676, XV4677, XV4678, XV4679, XV4680, XV4681, XV4682), 'lo.comp.typecheck^thetaEnv', _) :- 'lo.comp.typecheck@thetaEnv'(XV4669, XV4670, XV4671, XV4672, XV4673, XV4674, XV4675, XV4676, XV4677, XV4678, XV4679, XV4680, XV4681, XV4682).
'lo.comp.typecheck^findImportedImplementations'('_call%3'(XV4683, XV4684, XV4685), 'lo.comp.typecheck^findImportedImplementations', _) :- 'lo.comp.typecheck@findImportedImplementations'(XV4683, XV4684, XV4685).
'lo.comp.typecheck@cond35'(XX36295, XRpx, XOthers, XOOthers, XRp1, XODefs, XODict, XDefs, XOverDict, XImportClosure, XRp0) :- 'lo.comp.errors@errorFree'(XRp0),
    !,
    'lo.comp.typecheck@findImportedImplementations'(XImportClosure, 'lo.core#[]', XOverDict),
    'lo.comp.resolve@overload'(XDefs, XOverDict, XODict, XODefs, XRp0, XRp1),
    'lo.comp.resolve@overloadOthers'(XOthers, XODict, XRp1, XRpx, XX36295),
    XOOthers = XX36295.
'lo.comp.typecheck@cond35'(XX36295, XRpx, XOthers, XOOthers, XRp1, XODefs, XODict, XDefs, XOverDict, XImportClosure, XRp0) :- XRpx = XRp0,
    XODefs = XDefs,
    XOOthers = XOthers.
'lo.comp.typecheck^checkProgram'('_call%6'(XV4686, XV4687, XV4688, XV4689, XV4690, XV4691), 'lo.comp.typecheck^checkProgram', _) :- 'lo.comp.typecheck@checkProgram'(XV4686, XV4687, XV4688, XV4689, XV4690, XV4691).
'lo.comp.typecheck^exportViz'('_call%4'(XV4692, XV4693, XV4694, XV4695), 'lo.comp.typecheck^exportViz', _) :- 'lo.comp.typecheck@exportViz'(XV4692, XV4693, XV4694, XV4695).
'lo.comp.typecheck^computeImports'('_call%3'(XV4696, XV4697, XV4698), 'lo.comp.typecheck^computeImports', _) :- 'lo.comp.typecheck@computeImports'(XV4696, XV4697, XV4698).
'lo.comp.typecheck^addPublicImports'('_call%3'(XV4699, XV4700, XV4701), 'lo.comp.typecheck^addPublicImports', _) :- 'lo.comp.typecheck@addPublicImports'(XV4699, XV4700, XV4701).
'lo.comp.typecheck^mkTypes'('_call%2'(XV4702, XV4703), 'lo.comp.typecheck^mkTypes', _) :- 'lo.comp.typecheck@mkTypes'(XV4702, XV4703).
'lo.comp.typecheck^pickVar'('_call%2'(XV4704, XV4705), 'lo.comp.typecheck^pickVar', _) :- 'lo.comp.typecheck@pickVar'(XV4704, XV4705).
'lo.comp.typecheck^computeNewVars'('_call%2'(XV4706, XV4707), 'lo.comp.typecheck^computeNewVars', _) :- 'lo.comp.typecheck@computeNewVars'(XV4706, XV4707).
