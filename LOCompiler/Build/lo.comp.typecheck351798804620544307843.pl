'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.typecheck's'0.0.1'n20o20'()20'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.ast'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.abstract'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.keywords'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.canon'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.parseType'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.freshen'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.unify'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.dependencies'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.dict'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.macro'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.misc'e'*'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.imports'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.resolve'e'*'s\"I1'checkProgram':k'r'|FT5t'lo.comp.ast*ast't'lo.repo*version'k'r't'lo.comp.errors*report't'lo.comp.errors*report't'lo.comp.canon*canonPkg'c'lo.repo$repository'T1k'r'T0\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.typecheck@init'():- !.
'lo.comp.typecheck@findImport'(XSt, X_35620, XXd40431):- 'lo.comp.abstract@isUnary'(XSt, "private", X_35621, XI),
    !,
    'lo.comp.typecheck@findImport'(XI, 'lo.comp.package#priVate', XXd40431).
'lo.comp.typecheck@findImport'(XSt, X_35622, XXd40432):- 'lo.comp.abstract@isUnary'(XSt, "public", X_35623, XI),
    !,
    'lo.comp.typecheck@findImport'(XI, 'lo.comp.package#pUblic', XXd40432).
'lo.comp.typecheck@findImport'(XSt, XViz, '()2'(XViz, XXd40433)):- 'lo.comp.abstract@isUnary'(XSt, "import", X_35624, XP),
    !,
    'lo.comp.abstract@pkgName'(XP, XXd40433).
'lo.comp.typecheck@findImport'(_, _, _):- raise_exception('error'("lo.comp.typecheck@findImport", 83, 3, 74)).
'lo.comp.typecheck@findAllImports'('lo.core#[]', 'lo.core#[]').
'lo.comp.typecheck@findAllImports'('lo.core#,..'(XSt, XMore), 'lo.core#,..'(XXb20372, XImports)):- 'lo.comp.typecheck@findAllImports'(XMore, XImports),
    'lo.comp.typecheck@findImport'(XSt, 'lo.comp.package#priVate', XXb20372).
'lo.comp.typecheck@second'('()2'(Xa, Xb), Xb):- !.
'lo.comp.typecheck@second'(_, _):- raise_exception('error'("lo.comp.typecheck@second", 91, 3, 18)).
'lo.comp.typecheck@declareFields'('lo.core#[]', X_35627, XEnv, XEnv):- !.
'lo.comp.typecheck@declareFields'('lo.core#,..'('()2'(XNm, XTp), XMore), XLc, XEnv, XXd40437):- !,
    'lo.comp.dict@declareVar'(XNm, 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, XNm), XTp), XEnv, XXd40436),
    'lo.comp.typecheck@declareFields'(XMore, XLc, XXd40436, XXd40437).
'lo.comp.typecheck@declareFields'(_, _, _, _):- raise_exception('error'("lo.comp.typecheck@declareFields", 111, 3, 30)).
'lo.comp.typecheck@pickTypeTemplate'('lo.comp.types#univType'(XB, XTp), XXd40438):- !,
    'lo.comp.typecheck@pickTypeTemplate'(XTp, XXd40438).
'lo.comp.typecheck@pickTypeTemplate'('lo.comp.types#typeRule'(XLhs, X_35629), XXd40439):- !,
    'lo.comp.typecheck@pickTypeTemplate'(XLhs, XXd40439).
'lo.comp.typecheck@pickTypeTemplate'('lo.comp.types#constrained'(XTp, X_35630), XXd40440):- !,
    'lo.comp.typecheck@pickTypeTemplate'(XTp, XXd40440).
'lo.comp.typecheck@pickTypeTemplate'('lo.comp.types#tipe'(XNm), 'lo.comp.types#tipe'(XNm)):- !.
'lo.comp.typecheck@pickTypeTemplate'('lo.comp.types#typeExp'(XOp, X_35631), XOp):- !.
'lo.comp.typecheck@pickTypeTemplate'(_, _):- raise_exception('error'("lo.comp.typecheck@pickTypeTemplate", 120, 3, 56)).
'lo.comp.typecheck@importTypes'('lo.core#[]', X_35632, XEnv, XEnv):- !.
'lo.comp.typecheck@importTypes'('lo.core#,..'('()2'(XNm, XRule), XMore), XLc, XEnv, XXd40445):- !,
    'lo.comp.typecheck@pickTypeTemplate'(XRule, XXd40442),
    'lo.comp.dict@declareType'(XNm, 'lo.comp.dict#tpDef'(XLc, XXd40442, XRule), XEnv, XXd40444),
    'lo.comp.typecheck@importTypes'(XMore, XLc, XXd40444, XXd40445).
'lo.comp.typecheck@importTypes'(_, _, _, _):- raise_exception('error'("lo.comp.typecheck@importTypes", 116, 3, 26)).
'lo.comp.typecheck@formMethods'('lo.core#[]', X_35634, X_35635, X_35636, XEnv, XEnv).
'lo.comp.typecheck@formMethods'('lo.core#,..'('()2'(XNm, XTp), XM), XLc, XQ, XCon, XEnv, XEv):- 'lo.comp.types@moveQuants'(XTp, XFQ, XQTp),
    'lo.list@merge'(XFQ, XQ, XXd40446),
    'lo.comp.types@moveQuants'(XMTp, XXd40446, 'lo.comp.types#constrained'(XQTp, XCon)),
    'lo.comp.dict@declareVar'(XNm, 'lo.comp.dict#vr'('lo.comp.canon#mtd'(XLc, XNm), XMTp), XEnv, XXd40450),
    'lo.comp.typecheck@formMethods'(XM, XLc, XQ, XCon, XXd40450, XEv).
'lo.comp.typecheck@declareMethods'(XSpec, XMtdsTp, XLc, XEnv, XEv):- 'lo.comp.types@moveConQuants'(XSpec, XQ, XCon),
    'lo.comp.types@moveQuants'(XMtdsTp, X_35638, 'lo.comp.types#faceType'(XMethods)),
    'lo.comp.typecheck@formMethods'(XMethods, XLc, XQ, XCon, XEnv, XEv).
'lo.comp.typecheck@defineContract'(XLc, 'lo.comp.types#conEntry'(XN, XNm, XSpec, XFace), XE0, XEx):- 'lo.comp.dict@declareContract'(XN, 'lo.comp.types#conEntry'(XN, XNm, XSpec, XFace), XE0, XXd40453),
    'lo.comp.typecheck@declareMethods'(XSpec, XFace, XLc, XXd40453, XEx).
'lo.comp.typecheck@importContracts'('lo.core#[]', X_35639, XEnv, XEnv):- !.
'lo.comp.typecheck@importContracts'('lo.core#,..'(XC, XL), XLc, XE, XXd40454):- 'lo.comp.typecheck@defineContract'(XLc, XC, XE, XE1),
    !,
    'lo.comp.typecheck@importContracts'(XL, XLc, XE1, XXd40454).
'lo.comp.typecheck@importContracts'(_, _, _, _):- raise_exception('error'("lo.comp.typecheck@importContracts", 127, 3, 30)).
'lo.comp.typecheck@importDefs'('lo.comp.package#pkgSpec'(X_35641, XExported, XTypes, X_35642, XCons, X_35643, X_35644), XLc, XEnv, XEx):- 'lo.comp.typecheck@declareFields'(XExported, XLc, XEnv, XXd40455),
    'lo.comp.typecheck@importTypes'(XTypes, XLc, XXd40455, XXd40456),
    'lo.comp.typecheck@importContracts'(XCons, XLc, XXd40456, XXd40457),
    XEx = XXd40457.
'lo.comp.typecheck@importAllDefs'('lo.core#[]', X_35645, XEnv, XEnv).
'lo.comp.typecheck@importAllDefs'('lo.core#,..'(XSpec, XSpecs), XLc, XEnv, XEx):- 'lo.comp.typecheck@importDefs'(XSpec, XLc, XEnv, XEv0),
    'lo.comp.typecheck@importAllDefs'(XSpecs, XLc, XEv0, XEx).
'lo.comp.typecheck@processImportGroup'(Xrepository64, XStmts, XImports, XImportClosure, XLc, XRepo, XEnv, XEx, XRp, XRpx):- 'lo.comp.typecheck@findAllImports'(XStmts, XImports),
    ocall('//%1'(XXV5494),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    ocall('_call%3'(XImports, 'lo.comp.typecheck^second', XXe5112),XXV5494,XXV5494),
    XImportedPkgs = XXe5112,
    'lo.comp.imports@importClosure'(Xrepository64, XImportedPkgs, 'lo.core#[]', XRepo, XRp, XRpx, XXd40458),
    XImportClosure = XXd40458,
    'lo.comp.typecheck@importAllDefs'(XImportClosure, XLc, XEnv, XEx).
'lo.comp.typecheck@pushFace'('lo.core#[]', X_35647, XE, XE):- !.
'lo.comp.typecheck@pushFace'('lo.core#,..'('()2'(XN, XT), XL), XLc, XE, XXd40462):- !,
    'lo.comp.dict@declareVar'(XN, 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, XN), XT), XE, XXd40461),
    'lo.comp.typecheck@pushFace'(XL, XLc, XXd40461, XXd40462).
'lo.comp.typecheck@pushFace'(_, _, _, _):- raise_exception('error'("lo.comp.typecheck@pushFace", 925, 3, 21)).
'lo.comp.typecheck@sameLength'(XL1, XL2, X_35649, XRp, XRp):- 'lo.list@length'(XL1, XXd40463),
    'lo.list@length'(XL2, XXd40464),
    XXd40463 = XXd40464.
'lo.comp.typecheck@sameLength'(XL1, X_35650, XLc, XRp, XRpx):- ocall('disp%1'(XXV5495),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    'lo.list@length'(XL1, XXd40466),
    ocall('_call%2'(XXd40466, XXe5113),XXV5495,XXV5495),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("expecting "), 'lo.core#,..'(XXe5113, 'lo.core#,..'('lo.core#ss'(" elements"), 'lo.core#[]')))), XXd40472),
    'lo.comp.errors@reportError'(XXd40472, XLc, XRp, XRpx).
'lo.comp.typecheck@declareConstraints'('lo.core#[]', XEnv, XEnv).
'lo.comp.typecheck@declareConstraints'('lo.core#,..'(XC, XL), XE, XEx):- 'lo.comp.dict@declareConstraint'(XC, XE, XXd40473),
    'lo.comp.typecheck@declareConstraints'(XL, XXd40473, XEx).
'lo.comp.typecheck@declareTypeVars'('lo.core#[]', X_35655, XEnv, XEnv).
'lo.comp.typecheck@declareTypeVars'('lo.core#,..'('()2'("this", X_35657), XVars), XLc, XEnv, XEx):- 'lo.comp.typecheck@declareTypeVars'(XVars, XLc, XEnv, XEx).
'lo.comp.typecheck@declareTypeVars'('lo.core#,..'('()2'(XNm, XTp), XVars), XLc, XEnv, XEx):- 'lo.comp.dict@declareTypeExists'(XNm, XLc, XTp, XEnv, XXd40474),
    'lo.comp.typecheck@declareTypeVars'(XVars, XLc, XXd40474, XEx).
'lo.comp.typecheck@isPublicImplementation'(XNm, XPublic):- ocall('in%2'('()2'(XNm, 'lo.comp.abstract#impl'), XPublic),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.typecheck@isPublicType'(XNm, XPublic):- ocall('in%2'('()2'(XNm, 'lo.comp.abstract#tpe'), XPublic),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.typecheck@isPublicVar'(XNm, X_35659, XPublic):- ocall('in%2'('()2'(XNm, 'lo.comp.abstract#valu'), XPublic),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.typecheck@isPublicVar'(XNm, XFields, X_35660):- ocall('in%2'('()2'(XNm, X_35661), XFields),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.typecheck@exportDef'('lo.comp.canon#funDef'(X_35662, XNm, XTp, X_35663, X_35664), XFields, XPublic, 'lo.core#,..'('()2'(XNm, XTp), XEx), XEx, XTypes, XTypes, XEnums, XEnums, XCons, XCons, XImpl, XImpl):- 'lo.comp.typecheck@isPublicVar'(XNm, XFields, XPublic).
'lo.comp.typecheck@exportDef'('lo.comp.canon#relDef'(X_35666, XNm, XTp, X_35667, X_35668), XFields, XPublic, 'lo.core#,..'('()2'(XNm, XTp), XEx), XEx, XTypes, XTypes, XEnums, XEnums, XCons, XCons, XImpl, XImpl):- 'lo.comp.typecheck@isPublicVar'(XNm, XFields, XPublic).
'lo.comp.typecheck@exportDef'('lo.comp.canon#classDef'(X_35670, XNm, XTp, X_35671, X_35672, X_35673), XFields, XPublic, 'lo.core#,..'('()2'(XNm, XTp), XEx), XEx, XTypes, XTypes, 'lo.core#,..'(XNm, XClx), XClx, XCons, XCons, XImpl, XImpl):- 'lo.comp.typecheck@isPublicVar'(XNm, XFields, XPublic).
'lo.comp.typecheck@exportDef'('lo.comp.canon#typeDef'(X_35676, XNm, X_35677, XFaceRule), X_35678, XPublic, XExports, XExports, 'lo.core#,..'('()2'(XNm, XFaceRule), XTx), XTx, XEnums, XEnums, XCons, XCons, XImpl, XImpl):- 'lo.comp.typecheck@isPublicType'(XNm, XPublic).
'lo.comp.typecheck@exportDef'('lo.comp.canon#varDef'(X_35680, XNm, XTp, X_35681, X_35682, X_35683), XFields, XPublic, 'lo.core#,..'('()2'(XNm, XTp), XEx), XEx, XTypes, XTypes, XEnums, XEnums, XCons, XCons, XImpl, XImpl):- 'lo.comp.typecheck@isPublicVar'(XNm, XFields, XPublic).
'lo.comp.typecheck@exportDef'('lo.comp.canon#grammDef'(X_35685, XNm, XTp, X_35686, X_35687), XFields, XPublic, 'lo.core#,..'('()2'(XNm, XTp), XEx), XEx, XTypes, XTypes, XEnums, XEnums, XCons, XCons, XImpl, XImpl):- 'lo.comp.typecheck@isPublicVar'(XNm, XFields, XPublic).
'lo.comp.typecheck@exportDef'('lo.comp.canon#cnDefn'(X_35689, XNm, XContract), X_35690, XPublic, XEx, XEx, XTypes, XTypes, XEnums, XEnums, 'lo.core#,..'(XContract, XCons), XCons, XImpl, XImpl):- ocall('in%2'('()2'(XNm, 'lo.comp.abstract#con'), XPublic),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.typecheck@exportDef'('lo.comp.canon#implDef'(X_35692, XINm, XImplName, XSpec, X_35693, X_35694, X_35695, X_35696), X_35697, XPublic, XEx, XEx, XTps, XTps, XEnums, XEnums, XCons, XCons, 'lo.core#,..'('lo.comp.types#implEntry'(XImplName, XSpec), XIx), XIx):- 'lo.comp.typecheck@one303'(XPublic, XINm).
'lo.comp.typecheck@exportDef'(X_35699, X_35700, X_35701, XEx, XEx, XTps, XTps, XEnums, XEnums, XCons, XCons, XImpls, XImpls).
'lo.comp.typecheck@computeExport'('lo.core#[]', X_35702, X_35703, 'lo.core#[]', 'lo.core#[]', 'lo.core#[]', 'lo.core#[]', 'lo.core#[]').
'lo.comp.typecheck@computeExport'('lo.core#,..'(XDef, XDefs), XFields, XPublic, XExports, XTypes, XEnums, XContracts, XImpls):- 'lo.comp.typecheck@one304'(XIx, XImpls, XCx, XContracts, XClx, XEnums, XTx, XTypes, XEx, XExports, XPublic, XFields, XDef),
    'lo.comp.typecheck@computeExport'(XDefs, XFields, XPublic, XEx, XTx, XClx, XCx, XIx).
'lo.comp.typecheck@contractGroup'('lo.core#,..'('lo.comp.dependencies#defn'(XN, 'lo.comp.abstract#con', XLc, X_35706, 'lo.core#,..'(XConStmt, 'lo.core#[]')), X_35708), 'lo.core#,..'('lo.comp.canon#cnDefn'(XLc, XN, XContract), XDefs), XDefs, XEnv, XEx, XPath, XRp, XRpx):- 'lo.comp.parseType@parseContract'(XConStmt, XEnv, XPath, XContract, XRp, XRpx),
    'lo.comp.typecheck@defineContract'(XLc, XContract, XEnv, XEx).
'lo.comp.typecheck@parseAnnotations'('lo.core#[]', X_35710, X_35711, XEnv, XEnv, X_35712, XRp, XRp).
'lo.comp.typecheck@parseAnnotations'('lo.core#,..'('lo.comp.dependencies#defn'(XNm, 'lo.comp.abstract#valu', X_35714, X_35715, X_35716), XMore), XFields, XAnnots, XEnv, XEx, XPath, XRp, XRpx):- 'lo.comp.typecheck@one305'(XAnnots, XAnnot, XNm),
    'lo.comp.abstract@isBinary'(XAnnot, ":", XLc, X_35717, XT),
    'lo.comp.parseType@parseType'(XT, XEnv, XTp, XRp, XRp0),
    'lo.comp.dict@declareVar'(XNm, 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, XNm), XTp), XEnv, XXd40477),
    'lo.comp.typecheck@parseAnnotations'(XMore, XFields, XAnnots, XXd40477, XEx, XPath, XRp0, XRpx).
'lo.comp.typecheck@parseAnnotations'('lo.core#,..'('lo.comp.dependencies#defn'(XN, 'lo.comp.abstract#valu', XLc, X_35719, X_35720), XMore), XFields, XAnnots, XEnv, XEx, XPath, XRp, XRpx):- 'lo.comp.typecheck@one306'(XFields, XTp, XN),
    'lo.comp.dict@declareVar'(XN, 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, XN), XTp), XEnv, XXd40480),
    'lo.comp.typecheck@parseAnnotations'(XMore, XFields, XAnnots, XXd40480, XEx, XPath, XRp, XRpx).
'lo.comp.typecheck@parseAnnotations'('lo.core#,..'('lo.comp.dependencies#defn'(XN, 'lo.comp.abstract#valu', XLc, X_35722, X_35723), XMore), XFields, XAnnots, XEnv, XEx, XPath, XRp, XRpx):- ocall('disp%1'(XXV5496),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('_call%2'(XN, XXe5114),XXV5496,XXV5496),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("no type annotation for variable "), 'lo.core#,..'(XXe5114, 'lo.core#[]'))), XXd40485),
    'lo.comp.errors@reportError'(XXd40485, XLc, XRp, XRp0),
    'lo.comp.typecheck@parseAnnotations'(XMore, XFields, XAnnots, XEnv, XEx, XPath, XRp0, XRpx).
'lo.comp.typecheck@pickupThisType'(XEnv, XXe5115):- 'lo.comp.dict@isVar'("this", XEnv, 'lo.comp.dict#vr'(X_35726, XThisType)),
    !,
    ocall('_put%1'(XXV5498),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_empty%1'(XXV5497),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XXV5497, "this", XThisType, XXe5115),XXV5498,XXV5498).
'lo.comp.typecheck@pickupThisType'(X_35727, XXV5499):- !,
    ocall('_empty%1'(XXV5499),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.typecheck@pickupThisType'(_, _):- raise_exception('error'("lo.comp.typecheck@pickupThisType", 905, 3, 80)).
'lo.comp.typecheck@pickupVarType'(XN, X_35728, XEnv, XRp, XRp, XTp):- 'lo.comp.dict@isVar'(XN, XEnv, 'lo.comp.dict#vr'(X_35729, XTp)),
    !.
'lo.comp.typecheck@pickupVarType'(XN, XLc, X_35730, XRp, XRpx, XXd40496):- ocall('disp%1'(XXV5500),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('_call%2'(XN, XXe5116),XXV5500,XXV5500),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XXe5116, 'lo.core#,..'('lo.core#ss'(" not declared"), 'lo.core#[]'))), XXd40495),
    'lo.comp.errors@reportError'(XXd40495, XLc, XRp, XRpx),
    !,
    'lo.comp.types@newVar'("_", XXd40496).
'lo.comp.typecheck@pickupVarType'(_, _, _, _, _, _):- raise_exception('error'("lo.comp.typecheck@pickupVarType", 245, 3, 61)).
'lo.comp.typecheck@splitHead'(XT, XLc, XNm, XA):- 'lo.comp.abstract@isRound'(XT, XLc, XN, XA),
    'lo.comp.abstract@isIden'(XN, X_35733, XNm).
'lo.comp.typecheck@splitHead'(XT, XLc, XNm, XXb20420):- 'lo.comp.abstract@isIden'(XT, XLc, XNm),
    'lo.comp.abstract@roundTuple'(XLc, 'lo.core#[]', XXb20420).
'lo.comp.typecheck@splitGrHead'(XTerm, XNm, XArgs, XHed):- 'lo.comp.abstract@isBinary'(XTerm, ",", X_35734, XL, XR),
    'lo.comp.abstract@isSquareTuple'(XR, X_35735, XHed),
    'lo.comp.typecheck@splitHead'(XL, X_35736, XNm, XArgs).
'lo.comp.typecheck@splitGrHead'(XTerm, XNm, XArgs, XHed):- 'lo.comp.abstract@isRoundTuple'(XTerm, X_35737, 'lo.core#,..'(XT, 'lo.core#[]')),
    'lo.comp.typecheck@splitGrHead'(XT, XNm, XArgs, XHed).
'lo.comp.typecheck@splitGrHead'(XTerm, XNm, XArgs, 'lo.core#[]'):- 'lo.comp.typecheck@splitHead'(XTerm, X_35739, XNm, XArgs).
'lo.comp.typecheck@manageConstraints'('lo.comp.types#constrained'(XT, XC), XCons, XLc, XV, XMTp, XExp):- 'lo.comp.typecheck@manageConstraints'(XT, 'lo.core#,..'(XC, XCons), XLc, XV, XMTp, XExp).
'lo.comp.typecheck@manageConstraints'(XTp, 'lo.core#[]', X_35741, XV, XTp, XV).
'lo.comp.typecheck@manageConstraints'(XTp, XCons, XLc, XV, XTp, 'lo.comp.canon#over'(XLc, XV, XXb20422)):- 'lo.list@reverse'(XCons, XXb20422).
'lo.comp.typecheck@checkType'(XLc, XActual, XExpected, XEnv, XRp, XRpx):- 'lo.comp.typecheck@cond420'(XLc, XXd40504, XXd40503, XXd40502, XXd40501, XXd40500, XXe5118, XXV5502, XXd40499, XXe5117, XXV5501, XRpx, XRp, XEnv, XExpected, XActual).
'lo.comp.typecheck@typeOfVar'(XLc, 'lo.comp.dict#vr'(XV, XVT), XTp, XEnv, XTerm, XRp, XRpx):- ocall('freshen%1'(XXV5503),'lo.comp.freshen$freshen$lo.comp.types*tipe','lo.comp.freshen$freshen$lo.comp.types*tipe'),
    'lo.comp.typecheck@pickupThisType'(XEnv, XXd40505),
    ocall('_call%3'(XVT, XXd40505, XXe5119),XXV5503,XXV5503),
    '()2'(X_35745, XFTp) = XXe5119,
    'lo.comp.canon@relocate'(XV, XLc, XXd40506),
    'lo.comp.typecheck@manageConstraints'(XFTp, 'lo.core#[]', XLc, XXd40506, XMTp, XTerm),
    'lo.comp.typecheck@checkType'(XLc, XMTp, XTp, XEnv, XRp, XRpx).
'lo.comp.typecheck@findType'(XNm, X_35746, XEnv, XRp, XRp, XTp):- 'lo.comp.dict@isType'(XNm, XEnv, X_35747, XTp),
    !.
'lo.comp.typecheck@findType'(XNm, XLc, X_35748, XRp, XRpx, 'lo.comp.types#anonType'):- ocall('disp%1'(XXV5504),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('_call%2'(XNm, XXe5120),XXV5504,XXV5504),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("type "), 'lo.core#,..'(XXe5120, 'lo.core#,..'('lo.core#ss'(" not known"), 'lo.core#[]')))), XXd40513),
    'lo.comp.errors@reportError'(XXd40513, XLc, XRp, XRpx),
    !.
'lo.comp.typecheck@findType'(_, _, _, _, _, _):- raise_exception('error'("lo.comp.typecheck@findType", 909, 3, 57)).
'lo.comp.typecheck@checkGrammarType'(XLc, XEnv, XTp, XElTp, XRp, XRpx):- 'lo.comp.dict@isContract'("stream", XEnv, 'lo.comp.types#conEntry'(X_35752, X_35753, XSpec, X_35754)),
    ocall('freshen%1'(XXV5505),'lo.comp.freshen$freshen$lo.comp.types*constraint','lo.comp.freshen$freshen$lo.comp.types*constraint'),
    'lo.comp.typecheck@pickupThisType'(XEnv, XXd40518),
    ocall('_call%3'(XSpec, XXd40518, XXe5121),XXV5505,XXV5505),
    '()2'(X_35755, 'lo.comp.types#conTract'(X_35756, 'lo.core#,..'(XArg, 'lo.core#[]'), 'lo.core#,..'(XDep, 'lo.core#[]'))) = XXe5121,
    'lo.comp.typecheck@checkType'(XLc, XArg, XTp, XEnv, XRp, XRp0),
    'lo.comp.typecheck@checkType'(XLc, XDep, XElTp, XEnv, XRp0, XRpx).
'lo.comp.typecheck@streamVar'("stream_X"):- !.
'lo.comp.typecheck@makeIntList'('lo.core#[]', X_35759, 'lo.core#[]'):- !.
'lo.comp.typecheck@makeIntList'('lo.core#,..'(XCode, XL), XLc, 'lo.core#,..'('lo.comp.ast#intg'(XLc, XCode), XXd40520)):- !,
    'lo.comp.typecheck@makeIntList'(XL, XLc, XXd40520).
'lo.comp.typecheck@makeIntList'(_, _, _):- raise_exception('error'("lo.comp.typecheck@makeIntList", 856, 3, 23)).
'lo.comp.typecheck@explodeStringLit'(XLc, XStr, XXd40522):- !,
    'explode'(XStr, XXc533),
    'lo.comp.typecheck@makeIntList'(XXc533, XLc, XXd40522).
'lo.comp.typecheck@explodeStringLit'(_, _, _):- raise_exception('error'("lo.comp.typecheck@explodeStringLit", 853, 3, 56)).
'lo.comp.typecheck@genTpVars'('lo.core#[]', 'lo.core#[]'):- !.
'lo.comp.typecheck@genTpVars'('lo.core#,..'(X_35763, XI), 'lo.core#,..'(XXd40523, XXd40524)):- !,
    'lo.comp.types@newVar'("__", XXd40523),
    'lo.comp.typecheck@genTpVars'(XI, XXd40524).
'lo.comp.typecheck@genTpVars'(_, _):- raise_exception('error'("lo.comp.typecheck@genTpVars", 694, 3, 17)).
'lo.comp.typecheck@macroSequenceTerm'('lo.core#[]', XLc, XV, 'lo.core#,..'(XXb20427, 'lo.core#[]')):- 'lo.comp.abstract@unary'(XLc, "_eof", XV, XXb20427).
'lo.comp.typecheck@macroSequenceTerm'('lo.core#,..'(XE, XL), X_35767, XV, 'lo.core#,..'(XXb20430, XM)):- ocall('loc%1'(XXV5506),XE,XE),
    'lo.comp.abstract@genIden'(XLc, XXd40526),
    XNX = XXd40526,
    'lo.comp.typecheck@macroSequenceTerm'(XL, XLc, XNX, XM),
    'lo.comp.abstract@ternary'(XXV5506, "_hdtl", XV, XE, XNX, XXb20430).
'lo.comp.typecheck@isListType'(XTp, XLc, XEnv):- 'lo.comp.dict@isType'("list", XEnv, X_35769, XT),
    'lo.comp.types@deRef'(XTp, XXd40528),
    'lo.comp.types#typeExp'(XLsOp, X_35770) = XXd40528,
    'lo.comp.types@deRef'(XLsOp, XXd40529),
    XXd40529 = XLsTp.
'lo.comp.typecheck@isListSequence'('lo.core#,..'(XE, X_35772)):- 'lo.comp.typecheck@neg338'(X_35775, X_35774, X_35773, XE).
'lo.comp.typecheck@macroMapEntries'(XLc, 'lo.core#[]', XRp, XRp, 'lo.comp.ast#iden'(XLc, "_empty")):- !.
'lo.comp.typecheck@macroMapEntries'(X_35776, 'lo.core#,..'(XE, XL), XRp, XRpx, XXd40536):- 'lo.comp.abstract@isBinary'(XE, "->", XLc, XKy, XVl),
    !,
    'lo.comp.typecheck@macroMapEntries'(XLc, XL, XRp, XRpx, XXd40532),
    'lo.comp.abstract@roundTerm'(XLc, 'lo.comp.ast#iden'(XLc, "_put"), 'lo.core#,..'(XXd40532, 'lo.core#,..'(XKy, 'lo.core#,..'(XVl, 'lo.core#[]'))), XXd40536).
'lo.comp.typecheck@macroMapEntries'(XLc, 'lo.core#,..'(XE, XL), XRp, XRpx, XXd40542):- ocall('disp%1'(XXV5507),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('_call%2'(XE, XXe5122),XXV5507,XXV5507),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("invalid entry in map "), 'lo.core#,..'(XXe5122, 'lo.core#[]'))), XXd40541),
    ocall('loc%1'(XXV5508),XE,XE),
    'lo.comp.errors@reportError'(XXd40541, XXV5508, XRp, XRp0),
    !,
    'lo.comp.typecheck@macroMapEntries'(XLc, XL, XRp0, XRpx, XXd40542).
'lo.comp.typecheck@macroMapEntries'(_, _, _, _, _):- raise_exception('error'("lo.comp.typecheck@macroMapEntries", 671, 3, 49)).
'lo.comp.typecheck@isMapType'(XTp, XLc, XEnv):- 'lo.comp.dict@isType'("map", XEnv, X_35784, XMpTp),
    'lo.comp.types@deRef'(XTp, XXd40544),
    'lo.comp.types#typeExp'(XMpOp, X_35785) = XXd40544,
    'lo.comp.types@deRef'(XMpOp, XXd40545),
    XXd40545 = XMpTp.
'lo.comp.typecheck@isMapSequence'(XEls):- ocall('in%2'(XE, XEls),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.abstract@isBinary'(XE, "->", X_35786, X_35787, X_35788).
'lo.comp.typecheck@fieldInFace'(XFields, X_35789, XNm, X_35790, XTp, XRp, XRp):- 'lo.comp.typecheck@one307'(XFields, XTp, XNm).
'lo.comp.typecheck@fieldInFace'(X_35791, XTp, XNm, XLc, 'lo.comp.types#anonType', XRp, XRpx):- ocall('disp%1'(XXV5509),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('disp%1'(XXV5510),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('_call%2'(XNm, XXe5123),XXV5509,XXV5509),
    ocall('_call%2'(XTp, XXe5124),XXV5510,XXV5510),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("field "), 'lo.core#,..'(XXe5123, 'lo.core#,..'('lo.core#ss'(" not declared in "), 'lo.core#,..'(XXe5124, 'lo.core#[]'))))), XXd40553),
    'lo.comp.errors@reportError'(XXd40553, XLc, XRp, XRpx).
'lo.comp.typecheck@isGrammarRule'('lo.comp.canon#grRule'(XLc, XNm, X_35796, X_35797, X_35798), XLc, XNm).
'lo.comp.typecheck@collectGrammarRules'('lo.core#,..'(XRl, XStmts), XSx, XNm, 'lo.core#,..'(XRl, XEx)):- 'lo.comp.typecheck@isGrammarRule'(XRl, X_35801, XNm),
    'lo.comp.typecheck@collectGrammarRules'(XStmts, XSx, XNm, XEx).
'lo.comp.typecheck@collectGrammarRules'('lo.core#,..'(XRl, XStmts), 'lo.core#,..'(XRl, XSx), XNm, XEqns):- 'lo.comp.typecheck@collectGrammarRules'(XStmts, XSx, XNm, XEqns).
'lo.comp.typecheck@collectGrammarRules'('lo.core#[]', 'lo.core#[]', X_35804, 'lo.core#[]').
'lo.comp.typecheck@isRuleForClass'('lo.comp.canon#clRule'(XLc, XNm, X_35805, X_35806, X_35807, XFace), XLc, XNm, XFace).
'lo.comp.typecheck@collectClassRules'('lo.core#,..'(XCl, XStmts), XSx, XNm, 'lo.core#,..'(XCl, XEx)):- 'lo.comp.typecheck@isRuleForClass'(XCl, X_35810, XNm, X_35811),
    'lo.comp.typecheck@collectClassRules'(XStmts, XSx, XNm, XEx).
'lo.comp.typecheck@collectClassRules'('lo.core#,..'(XRl, XStmts), 'lo.core#,..'(XRl, XSx), XNm, XEqns):- 'lo.comp.typecheck@collectClassRules'(XStmts, XSx, XNm, XEqns).
'lo.comp.typecheck@collectClassRules'('lo.core#[]', 'lo.core#[]', X_35814, 'lo.core#[]').
'lo.comp.typecheck@collectMoreClauses'('lo.core#,..'(XCl, XStmts), XSx, XNm, 'lo.core#,..'(XCl, XEx)):- XCl = 'lo.comp.canon#clause'(X_35817, XNm, X_35818, X_35819),
    'lo.comp.typecheck@collectMoreClauses'(XStmts, XSx, XNm, XEx).
'lo.comp.typecheck@collectMoreClauses'('lo.core#,..'(XRl, XStmts), 'lo.core#,..'(XRl, XSx), XNm, XEqns):- 'lo.comp.typecheck@neg339'(XXd40555, X_35824, X_35823, XNm, X_35822, XRl),
    'lo.comp.typecheck@collectMoreClauses'(XStmts, XSx, XNm, XEqns).
'lo.comp.typecheck@collectMoreClauses'('lo.core#[]', 'lo.core#[]', X_35825, 'lo.core#[]').
'lo.comp.typecheck@collectClauses'('lo.core#[]', 'lo.core#[]', X_35826, 'lo.core#[]').
'lo.comp.typecheck@collectClauses'('lo.core#,..'(XCl, XStmts), XSx, XNm, 'lo.core#,..'(XCl, XEx)):- XCl = 'lo.comp.canon#clause'(X_35829, XNm, X_35830, X_35831),
    'lo.comp.typecheck@collectMoreClauses'(XStmts, XSx, XNm, XEx).
'lo.comp.typecheck@collectEquations'('lo.core#,..'(XEqn, XStmts), XSx, XNm, 'lo.core#,..'(XEqn, XEx)):- XEqn = 'lo.comp.canon#equation'(X_35834, XNm, X_35835, X_35836, X_35837),
    'lo.comp.typecheck@collectEquations'(XStmts, XSx, XNm, XEx).
'lo.comp.typecheck@collectEquations'('lo.core#,..'(XRl, XStmts), 'lo.core#,..'(XRl, XSx), XNm, XEqns):- 'lo.comp.typecheck@neg340'(XXd40558, X_35843, X_35842, X_35841, XNm, X_35840, XRl),
    'lo.comp.typecheck@collectEquations'(XStmts, XSx, XNm, XEqns).
'lo.comp.typecheck@collectEquations'('lo.core#[]', 'lo.core#[]', X_35844, 'lo.core#[]').
'lo.comp.typecheck@collectPrograms'('lo.core#[]', X_35845, X_35846, XDefs, XDefs).
'lo.comp.typecheck@collectPrograms'('lo.core#,..'(XEqn, XStmts), XEnv, XCx, 'lo.core#,..'('lo.comp.canon#funDef'(XLc, XNm, XTp, XCx, 'lo.core#,..'(XEqn, XEqns)), XDefs), XDx):- XEqn = 'lo.comp.canon#equation'(XLc, XNm, X_35850, X_35851, X_35852),
    'lo.comp.typecheck@collectEquations'(XStmts, XS0, XNm, XEqns),
    'lo.comp.dict@isVar'(XNm, XEnv, 'lo.comp.dict#vr'(X_35853, XTp)),
    'lo.comp.typecheck@collectPrograms'(XS0, XEnv, XCx, XDefs, XDx).
'lo.comp.typecheck@collectPrograms'('lo.core#,..'(XCl, XStmts), XEnv, XCx, 'lo.core#,..'('lo.comp.canon#relDef'(XLc, XNm, XTp, XCx, 'lo.core#,..'(XCl, XClses)), XDefs), XDx):- XCl = 'lo.comp.canon#clause'(XLc, XNm, X_35857, X_35858),
    'lo.comp.typecheck@collectClauses'(XStmts, XS0, XNm, XClses),
    'lo.comp.dict@isVar'(XNm, XEnv, 'lo.comp.dict#vr'(X_35859, XTp)),
    'lo.comp.typecheck@collectPrograms'(XS0, XEnv, XCx, XDefs, XDx).
'lo.comp.typecheck@collectPrograms'('lo.core#,..'('lo.comp.canon#vrDef'(XLc, XNm, XValue, XCond), XStmts), XEnv, XCx, 'lo.core#,..'('lo.comp.canon#varDef'(XLc, XNm, XTp, XCx, XValue, XCond), XDefs), XDx):- 'lo.comp.dict@isVar'(XNm, XEnv, 'lo.comp.dict#vr'(X_35862, XTp)),
    'lo.comp.typecheck@collectPrograms'(XStmts, XEnv, XCx, XDefs, XDx).
'lo.comp.typecheck@collectPrograms'('lo.core#,..'(XCl, XStmts), XEnv, XCx, 'lo.core#,..'('lo.comp.canon#classDef'(XLc, XNm, XTp, XCx, 'lo.core#,..'(XCl, XRules), XFace), XDefs), XDx):- 'lo.comp.typecheck@isRuleForClass'(XCl, XLc, XNm, XFace),
    'lo.comp.typecheck@collectClassRules'(XStmts, XS0, XNm, XRules),
    'lo.comp.dict@isVar'(XNm, XEnv, 'lo.comp.dict#vr'(X_35866, XTp)),
    'lo.comp.typecheck@collectPrograms'(XS0, XEnv, XCx, XDefs, XDx).
'lo.comp.typecheck@collectPrograms'('lo.core#,..'(XRl, XStmts), XEnv, XCx, 'lo.core#,..'('lo.comp.canon#grammDef'(XLc, XNm, XTp, XCx, 'lo.core#,..'(XRl, XRules)), XDefs), XDx):- 'lo.comp.typecheck@isGrammarRule'(XRl, XLc, XNm),
    'lo.comp.typecheck@collectGrammarRules'(XStmts, XS0, XNm, XRules),
    'lo.comp.dict@isVar'(XNm, XEnv, 'lo.comp.dict#vr'(X_35870, XTp)),
    'lo.comp.typecheck@collectPrograms'(XS0, XEnv, XCx, XDefs, XDx).
'lo.comp.typecheck@defineType'(XN, XLc, XSt, XEnv, XEx, XPath, XRp, XRpx):- 'lo.comp.typecheck@cond421'(XXd40576, XXd40575, XXd40574, XPath, XTpCore, XSt, XEx, XRpx, XRp, XLc, XXd40573, XXd40572, XXd40571, XXd40570, XXd40569, XXd40568, XXe5126, XXV5512, XXd40567, XXe5125, XXV5511, XXd40566, X_35871, XOLc, XEnv, XN).
'lo.comp.typecheck@defineTypes'('lo.core#[]', XEnv, XEnv, X_35876, XRp, XRp).
'lo.comp.typecheck@defineTypes'('lo.core#,..'('lo.comp.dependencies#defn'(XN, 'lo.comp.abstract#tpe', XLc, X_35878, 'lo.core#,..'(XStmt, 'lo.core#[]')), XMore), XEnv, XEx, XPath, XRp, XRpx):- 'lo.comp.typecheck@defineType'(XN, XLc, XStmt, XEnv, XE0, XPath, XRp, XRp0),
    'lo.comp.typecheck@defineTypes'(XMore, XE0, XEx, XPath, XRp0, XRpx).
'lo.comp.typecheck@defineTypes'('lo.core#,..'('lo.comp.dependencies#defn'(XN, 'lo.comp.abstract#tpe', XLc, X_35881, 'lo.core#,..'(X_35883, X_35884)), XMore), XEnv, XEx, XPath, XRp, XRpx):- ocall('disp%1'(XXV5513),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('_call%2'(XN, XXe5127),XXV5513,XXV5513),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("multiple type definition statements for "), 'lo.core#,..'(XXe5127, 'lo.core#[]'))), XXd40581),
    'lo.comp.errors@reportError'(XXd40581, XLc, XRp, XRp0),
    'lo.comp.typecheck@defineTypes'(XMore, XEnv, XEx, XPath, XRp0, XRpx).
'lo.comp.typecheck@parseTypeDefinition'(XN, XLc, XSt, 'lo.core#,..'('lo.comp.canon#typeDef'(XLc, XN, XType, XFaceRule), XDefs), XDefs, XEnv, XPath, XRp, XRpx):- 'lo.comp.abstract@isUnary'(XSt, "type", X_35888, XInSt),
    ocall('_empty%1'(XXV5514),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.parseType@parseTypeRule'(XInSt, XXV5514, XEnv, XFaceRule, XPath, XRp, XRpx),
    'lo.comp.dict@isType'(XN, XEnv, X_35889, XType).
'lo.comp.typecheck@parseTypeDefs'('lo.core#[]', XDefs, XDefs, X_35890, X_35891, XRp, XRp).
'lo.comp.typecheck@parseTypeDefs'('lo.core#,..'('lo.comp.dependencies#defn'(XN, 'lo.comp.abstract#tpe', XLc, X_35893, 'lo.core#,..'(XStmt, 'lo.core#[]')), XMore), XDefs, XDx, XTmpEnv, XPath, XRp, XRpx):- 'lo.comp.typecheck@parseTypeDefinition'(XN, XLc, XStmt, XDefs, XD0, XTmpEnv, XPath, XRp, XRp0),
    'lo.comp.typecheck@parseTypeDefs'(XMore, XD0, XDx, XTmpEnv, XPath, XRp0, XRpx).
'lo.comp.typecheck@declareTypes'('lo.core#[]', XDefs, XDefs, XEnv, XEnv).
'lo.comp.typecheck@declareTypes'('lo.core#,..'('lo.comp.canon#typeDef'(XLc, XN, XType, XFaceRule), XMore), 'lo.core#,..'('lo.comp.canon#typeDef'(XLc, XN, XType, XFaceRule), XDefs), XDx, XEnv, XEx):- 'lo.comp.dict@declareType'(XN, 'lo.comp.dict#tpDef'(XLc, XType, XFaceRule), XEnv, XXd40584),
    'lo.comp.typecheck@declareTypes'(XMore, XDefs, XDx, XXd40584, XEx).
'lo.comp.typecheck@typeGroup'(XGrp, XDefs, XDx, XEnv, XEx, XPath, XRp, XRpx):- 'lo.comp.typecheck@defineTypes'(XGrp, XEnv, XTmpEnv, XPath, XRp, XRp0),
    'lo.comp.typecheck@parseTypeDefs'(XGrp, XTpDefs, 'lo.core#[]', XTmpEnv, XPath, XRp0, XRpx),
    'lo.comp.typecheck@declareTypes'(XTpDefs, XDefs, XDx, XEnv, XEx).
'lo.comp.typecheck@checkOther'(XSt, 'lo.core#,..'('lo.comp.canon#integrity'(XLc, XCond), XMore), XMore, XEnv, XRp, XRpx):- 'lo.comp.abstract@isUnary'(XSt, "assert", XLc, XC),
    'lo.comp.typecheck@checkCond'(XC, XEnv, X_35898, XCond, XRp, XRpx).
'lo.comp.typecheck@checkOther'(XSt, 'lo.core#,..'('lo.comp.canon#expShow'(XLc, XShow), XMore), XMore, XEnv, XRp, XRpx):- 'lo.comp.abstract@isUnary'(XSt, "show", XLc, XE),
    'lo.comp.typecheck@findType'("string", XLc, XEnv, XRp, XRp0, XXd40585),
    'lo.comp.typecheck@typeOfTerm'(XE, XXd40585, XEnv, X_35900, XShow, XRp0, XRpx).
'lo.comp.typecheck@checkOthers'('lo.core#[]', 'lo.core#[]', X_35901, X_35902, XRp, XRp).
'lo.comp.typecheck@checkOthers'('lo.core#,..'(XSt, XStmts), XAss, XEnv, XPath, XRp, XRpx):- 'lo.comp.typecheck@one308'(XRp0, XRp, XEnv, XMore, XAss, XSt),
    'lo.comp.typecheck@checkOthers'(XStmts, XMore, XEnv, XPath, XRp0, XRpx).
'lo.comp.typecheck@checkOthers'('lo.core#,..'(XSt, XStmts), XAss, XEnv, XPath, XRp, XRpx):- ocall('disp%1'(XXV5515),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('_call%2'(XSt, XXe5128),XXV5515,XXV5515),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot understand statement: "), 'lo.core#,..'(XXe5128, 'lo.core#[]'))), XXd40590),
    ocall('loc%1'(XXV5516),XSt,XSt),
    'lo.comp.errors@reportError'(XXd40590, XXV5516, XRp, XRp0),
    'lo.comp.typecheck@checkOthers'(XStmts, XAss, XEnv, XPath, XRp0, XRpx).
'lo.comp.typecheck@checkClassHead'(XTerm, XTp, XTp, XEnv, XEnv, XNm, 'lo.comp.canon#v'(XLc, XNm), XRp, XRp):- 'lo.comp.abstract@isIden'(XTerm, XLc, XNm).
'lo.comp.typecheck@checkClassHead'(XTerm, 'lo.comp.types#classType'(XAT, XTp), XTp, XEnv, XEv, XNm, 'lo.comp.canon#apply'(XLc, 'lo.comp.canon#v'(XLc, XNm), XArgs), XRp, XRpx):- 'lo.comp.typecheck@splitHead'(XTerm, XLc, XNm, XA),
    'lo.comp.typecheck@typeOfArg'(XA, XAT, XEnv, XEv, XArgs, XRp, XRpx).
'lo.comp.typecheck@checkClassRule'(X_35907, XH, XG, XR, XClassTp, 'lo.core#,..'('lo.comp.canon#clRule'(XLc, XNm, XHd, 'lo.comp.canon#theta'(XClDefs, XClOthers), XCond, 'lo.comp.types#faceType'(XTypes)), XDefs), XDefs, XE, XPth, XRp, XRpx):- 'lo.comp.abstract@isBraceTuple'(XR, XLc, XEls),
    'lo.comp.dict@pushScope'(XE, XXd40591),
    'lo.comp.typecheck@checkClassHead'(XH, XClassTp, XSuperTp, XXd40591, XE1, XNm, XHd, XRp, XRp0),
    'lo.comp.typecheck@checkCond'(XG, XE1, XE2, XCond, XRp0, XRp1),
    'lo.comp.abstract@marker'('lo.comp.abstract#clss', XXd40592),
    'lo.comp.misc@subPath'(XPth, XXd40592, XNm, XXd40593),
    'lo.comp.typecheck@checkThetaTerm'(XSuperTp, XLc, XEls, XE1, XClDefs, XClOthers, XTypes, XXd40593, XRp0, XRpx).
'lo.comp.typecheck@checkClassRule'(XLc, XH, XG, XR, XClassTp, 'lo.core#,..'('lo.comp.canon#clRule'(XLc, XNm, XHd, XRepl, XCond, XSuperFace), XDefs), XDefs, XE, XPth, XRp, XRpx):- 'lo.comp.dict@pushScope'(XE, XXd40594),
    'lo.comp.typecheck@checkClassHead'(XH, XClassTp, X_35910, XXd40594, XE1, XNm, XHd, XRp, XRp0),
    'lo.comp.typecheck@checkCond'(XG, XE1, XE2, XCond, XRp0, XRp1),
    'lo.comp.types@newVar'("SpTp", XXd40595),
    XSuperTp = XXd40595,
    'lo.comp.typecheck@typeOfTerm'(XR, XSuperTp, XE2, X_35911, XRepl, XRp1, XRpx),
    'lo.comp.unify@faceOfType'(XSuperTp, XE2, XSuperFace).
'lo.comp.typecheck@checkDefn'(XLc, XL, XC, XR, XTp, 'lo.core#,..'('lo.comp.canon#vrDef'(XLc, XNm, XValue, XCond), XDx), XDx, XE, XRp, XRpx):- 'lo.comp.abstract@isIden'(XL, X_35913, XNm),
    'lo.comp.dict@pushScope'(XE, XXd40596),
    'lo.comp.typecheck@checkCond'(XC, XXd40596, XE1, XCond, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerm'(XR, XTp, XE1, X_35914, XValue, XRp0, XRpx).
'lo.comp.typecheck@recordAccessExp'(XLc, XRc, XFld, XET, XEnv, XEv, 'lo.comp.canon#dot'(XLc, XRec, XFld), XRp, XRpx):- 'lo.comp.types@newVar'("_R", XXd40597),
    XAT = XXd40597,
    'lo.comp.typecheck@knownType'(XRc, XAT, XEnv, XEv, XRec, XRp, XRp0),
    'lo.comp.types@deRef'(XAT, XXd40598),
    'lo.comp.unify@faceOfType'(XXd40598, XEnv, XFace),
    'lo.comp.types@moveConstraints'(XFace, X_35915, 'lo.comp.types#faceType'(XFields)),
    'lo.comp.typecheck@one309'(XRp1, XRp0, XFTp, XLc, XFld, XAT, XFields),
    ocall('freshen%1'(XXV5518),'lo.comp.freshen$freshen$lo.comp.types*tipe','lo.comp.freshen$freshen$lo.comp.types*tipe'),
    ocall('_empty%1'(XXV5517),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%3'(XFTp, XXV5517, XXe5129),XXV5518,XXV5518),
    '()2'(X_35916, XTp) = XXe5129,
    'lo.comp.typecheck@checkType'(XLc, XTp, XET, XEnv, XRp1, XRpx).
'lo.comp.typecheck@typeOfListTerm'('lo.core#[]', XLc, X_35917, XListTp, XEnv, XEv, XExp, XRp, XRpx):- 'lo.comp.typecheck@typeOfTerm'('lo.comp.ast#iden'(XLc, "[]"), XListTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfListTerm'('lo.core#,..'(XLast, 'lo.core#[]'), X_35919, XElTp, XListTp, XEnv, XEv, 'lo.comp.canon#apply'(XLc, XOp, 'lo.comp.canon#tpl'('lo.core#,..'(XHd, 'lo.core#,..'(XTl, 'lo.core#[]')))), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XLast, ",..", XLc, XL, XR),
    'lo.comp.types@newVar'("_", XXd40603),
    'lo.comp.typecheck@knownType'('lo.comp.ast#iden'(XLc, ",.."), XXd40603, XEnv, XE0, XOp, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerm'(XL, XElTp, XE0, XE1, XHd, XRp0, XRp1),
    'lo.comp.typecheck@typeOfTerm'(XR, XListTp, XE1, XEv, XTl, XRp1, XRpx).
'lo.comp.typecheck@typeOfListTerm'('lo.core#,..'(XEl, XMore), X_35923, XElTp, XListTp, XEnv, XEv, 'lo.comp.canon#apply'(XLc, XOp, 'lo.comp.canon#tpl'('lo.core#,..'(XHd, 'lo.core#,..'(XTl, 'lo.core#[]')))), XRp, XRpx):- ocall('loc%1'(XXV5519),XEl,XEl),
    XLc = XXV5519,
    'lo.comp.types@newVar'("_", XXd40605),
    'lo.comp.typecheck@knownType'('lo.comp.ast#iden'(XLc, ",.."), XXd40605, XEnv, XE0, XOp, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerm'(XEl, XElTp, XE0, XE1, XHd, XRp0, XRp1),
    'lo.comp.typecheck@typeOfListTerm'(XMore, XLc, XElTp, XListTp, XE1, XEv, XTl, XRp1, XRpx).
'lo.comp.typecheck@checkSequenceTerm'(XLc, XEls, XTp, XEnv, XEv, XExp, XRp, XRpx):- 'lo.comp.abstract@genIden'(XLc, XXd40606),
    XSeq = XXd40606,
    'lo.comp.typecheck@macroSequenceTerm'(XEls, XLc, XSeq, XTsts),
    'lo.comp.abstract@roundTuple'(XLc, XTsts, XXd40607),
    'lo.comp.abstract@binary'(XLc, "@@", XSeq, XXd40607, XXd40608),
    'lo.comp.typecheck@typeOfTerm'(XXd40608, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@checkSquareTuple'(XLc, XEls, XTp, XEnv, XEv, XExp, XRp, XRpx):- 'lo.comp.typecheck@cond423'(XXd40619, XXd40618, XXd40617, XListTp, XXd40616, XElTp, XRpx, XExp, XEv, XXd40615, XRp3, XRp2, XXd40614, XXd40613, XXd40612, XXd40611, XXd40610, XXd40609, XRp1, XRp, XEnv, XLc, XTp, XEls).
'lo.comp.typecheck@typeOfTerms'('lo.core#[]', 'lo.core#[]', XEnv, XEnv, X_35929, 'lo.core#[]', XRp, XRp).
'lo.comp.typecheck@typeOfTerms'('lo.core#[]', 'lo.core#,..'(XT, X_35931), XEnv, XEnv, XLc, 'lo.core#[]', XRp, XRpx):- ocall('disp%1'(XXV5520),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('_call%2'(XT, XXe5130),XXV5520,XXV5520),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("insufficient arguments, expecting a "), 'lo.core#,..'(XXe5130, 'lo.core#[]'))), XXd40624),
    'lo.comp.errors@reportError'(XXd40624, XLc, XRp, XRpx).
'lo.comp.typecheck@typeOfTerms'('lo.core#,..'(XA, X_35935), 'lo.core#[]', XEnv, XEnv, X_35936, 'lo.core#[]', XRp, XRpx):- ocall('disp%1'(XXV5521),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('_call%2'(XA, XXe5131),XXV5521,XXV5521),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("too many arguments: "), 'lo.core#,..'(XXe5131, 'lo.core#[]'))), XXd40629),
    ocall('loc%1'(XXV5522),XA,XA),
    'lo.comp.errors@reportError'(XXd40629, XXV5522, XRp, XRpx).
'lo.comp.typecheck@typeOfTerms'('lo.core#,..'(XA, XAs), 'lo.core#,..'(XElTp, XElTypes), XEnv, XEv, X_35941, 'lo.core#,..'(XTerm, XEls), XRp, XRpx):- 'lo.comp.typecheck@typeOfTerm'(XA, XElTp, XEnv, XE0, XTerm, XRp, XRp0),
    ocall('loc%1'(XXV5523),XA,XA),
    'lo.comp.typecheck@typeOfTerms'(XAs, XElTypes, XE0, XEv, XXV5523, XEls, XRp0, XRpx).
'lo.comp.typecheck@checkEquation'(XLc, XH, XG, XR, 'lo.comp.types#funType'(XAT, XRT), 'lo.core#,..'('lo.comp.canon#equation'(XLc, XNm, XArgs, XExp, XCond), XDefs), XDefs, XE, XRp, XRpx):- 'lo.comp.typecheck@splitHead'(XH, X_35944, XNm, XA),
    'lo.comp.dict@pushScope'(XE, XXd40630),
    'lo.comp.typecheck@typeOfArg'(XA, XAT, XXd40630, XE0, XArgs, XRp, XRp0),
    'lo.comp.typecheck@checkCond'(XG, XE0, XE1, XCond, XRp0, XRp1),
    'lo.comp.typecheck@typeOfTerm'(XR, XRT, XE1, X_35945, XExp, XRp1, XRpx).
'lo.comp.typecheck@checkEquation'(XLc, X_35946, X_35947, X_35948, XProgramType, XDefs, XDefs, X_35949, XRp, XRpx):- ocall('disp%1'(XXV5524),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('_call%2'(XProgramType, XXe5132),XXV5524,XXV5524),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("equation not consistent with expected type: "), 'lo.core#,..'(XXe5132, 'lo.core#[]'))), XXd40635),
    'lo.comp.errors@reportError'(XXd40635, XLc, XRp, XRpx).
'lo.comp.typecheck@checkTerminals'('lo.core#[]', X_35952, 'lo.core#[]', X_35953, XEnv, XEnv, XRp, XRp).
'lo.comp.typecheck@checkTerminals'('lo.core#,..'(XT, XL), XV, 'lo.core#,..'('()3'(XXV5525, XV, XTT), XM), XElTp, XEnv, XEv, XRp, XRpx):- ocall('loc%1'(XXV5525),XT,XT),
    'lo.comp.typecheck@typeOfTerm'(XT, XElTp, XEnv, XE0, XTT, XRp, XRp0),
    'lo.comp.typecheck@checkTerminals'(XL, XV, XM, XElTp, XE0, XEv, XRp0, XRpx).
'lo.comp.typecheck@checkNonTerminals'('lo.core#[]', X_35956, X_35957, XEnv, XEnv, 'lo.comp.canon#grTest'('lo.comp.canon#trueCond'), XRp, XRp).
'lo.comp.typecheck@checkNonTerminals'('lo.core#,..'(XN, XEls), XTp, XElTp, XEnv, XEv, 'lo.comp.canon#grConj'(XLhs, XRhs), XRp, XRpx):- 'lo.comp.typecheck@checkNonTerminal'(XN, XTp, XElTp, XEnv, XE0, XLhs, XRp, XRp0),
    'lo.comp.typecheck@checkNonTerminals'(XEls, XTp, XElTp, XE0, XEv, XRhs, XRp0, XRpx).
'lo.comp.typecheck@checkConds'('lo.core#[]', XEnv, XEnv, 'lo.comp.canon#trueCond', XRp, XRp).
'lo.comp.typecheck@checkConds'('lo.core#,..'(XC, 'lo.core#[]'), XEv, XEnv, XCond, XRp, XRpx):- 'lo.comp.typecheck@checkCond'(XC, XEv, XEnv, XCond, XRp, XRpx).
'lo.comp.typecheck@checkConds'('lo.core#,..'(XC, XL), XEv, XEnv, 'lo.comp.canon#conjCond'(XC0, XCm), XRp, XRpx):- 'lo.comp.typecheck@checkCond'(XC, XEv, XE0, XC0, XRp, XRp0),
    'lo.comp.typecheck@checkConds'(XL, XE0, XEnv, XCm, XRp0, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTpl, XStrmTp, XElTp, XE, XEnv, 'lo.comp.canon#grTerms'(XTerms), XRp, XRpx):- 'lo.comp.abstract@isSquareTuple'(XTpl, XLc, XEls),
    'lo.comp.typecheck@typeOfTerm'('lo.comp.ast#iden'(XLc, "_hdtl"), 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'(XStrmTp, 'lo.core#,..'(XElTp, 'lo.core#,..'(XStrmTp, 'lo.core#[]'))))), XE, XE0, XHDTL, XRp, XRp0),
    'lo.comp.typecheck@checkTerminals'(XEls, XHDTL, XTerms, XElTp, XE0, XEnv, XRp0, XRpx).
'lo.comp.typecheck@checkNonTerminal'('lo.comp.ast#strg'(XLc, XText), XStrmTp, XElTp, XE, XEnv, 'lo.comp.canon#grTerms'(XTerms), XRp, XRpx):- 'lo.comp.typecheck@typeOfTerm'('lo.comp.ast#iden'(XLc, "_hdtl"), 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'(XStrmTp, 'lo.core#,..'(XElTp, 'lo.core#,..'(XStrmTp, 'lo.core#[]'))))), XE, XE0, XHDTL, XRp, XRp0),
    'lo.comp.typecheck@explodeStringLit'(XLc, XText, XXd40648),
    'lo.comp.typecheck@checkTerminals'(XXd40648, XHDTL, XTerms, XElTp, XE0, XEnv, XRp0, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, XTp, XElTp, XEnv, XEx, XGrNT, XRp, XRpx):- 'lo.comp.abstract@isRoundTuple'(XTerm, XLc, XEls),
    'lo.comp.typecheck@one310'(XRpx, XRp, XGrNT, XEx, XEnv, XElTp, XTp, XEls).
'lo.comp.typecheck@checkNonTerminal'(XTerm, XTp, XElTp, XEnv, XEx, 'lo.comp.canon#grConj'(XLhs, XRhs), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, ",", XLc, XL, XR),
    'lo.comp.typecheck@checkNonTerminal'(XL, XTp, XElTp, XEnv, XE1, XLhs, XRp, XRp0),
    'lo.comp.typecheck@checkNonTerminal'(XR, XTp, XElTp, XE1, XEx, XRhs, XRp0, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, XTp, XElTp, XEnv, XEx, 'lo.comp.canon#grCond'(XTest, XEither, XOr), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, "|", X_35967, XL, XR),
    'lo.comp.abstract@isBinary'(XL, "?", X_35968, XT, XTh),
    'lo.comp.typecheck@checkNonTerminal'(XT, XTp, XElTp, XEnv, XE0, XTest, XRp, XRp0),
    'lo.comp.typecheck@checkNonTerminal'(XTh, XTp, XElTp, XE0, XE1, XEither, XRp0, XRp1),
    'lo.comp.typecheck@checkNonTerminal'(XR, XTp, XElTp, XE1, XEx, XOr, XRp1, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, XTp, XElTp, XEnv, XEx, 'lo.comp.canon#grDisj'(XEither, XOr), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, "|", X_35969, XL, XR),
    'lo.comp.typecheck@neg341'(X_35972, X_35971, X_35970, XL),
    'lo.comp.typecheck@checkNonTerminal'(XL, XTp, XElTp, XEnv, XE1, XEither, XRp, XRp0),
    'lo.comp.typecheck@checkNonTerminal'(XR, XTp, XElTp, XE1, XEx, XOr, XRp0, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, XTp, XElTp, XEnv, XEx, 'lo.comp.canon#grOne'(XTest), XRp, XRpx):- 'lo.comp.abstract@isUnary'(XTerm, "!", X_35973, XN),
    'lo.comp.typecheck@checkNonTerminal'(XN, XTp, XElTp, XEnv, XEx, XTest, XRp, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, XTp, XElTp, XEnv, XEnv, 'lo.comp.canon#grNeg'(XTest), XRp, XRpx):- 'lo.comp.abstract@isUnary'(XTerm, "\\+", XLc, XN),
    'lo.comp.typecheck@checkNonTerminal'(XN, XTp, XElTp, XEnv, X_35974, XTest, XRp, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, XTp, XElTp, XEnv, XEnv, 'lo.comp.canon#grAhed'(XTest), XRp, XRpx):- 'lo.comp.abstract@isUnary'(XTerm, "+", XLc, XN),
    'lo.comp.typecheck@checkNonTerminal'(XN, XTp, XElTp, XEnv, X_35975, XTest, XRp, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, X_35976, X_35977, XEnv, XEv, 'lo.comp.canon#grTest'('lo.comp.canon#unifyCond'(XLc, XLhs, XRhs)), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, "=", XLc, XL, XR),
    'lo.comp.types@newVar'("_", XXd40649),
    XTV = XXd40649,
    'lo.comp.typecheck@typeOfTerm'(XL, XTV, XEnv, XE0, XLhs, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerm'(XR, XTV, XE0, XEv, XRhs, XRp0, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, X_35978, X_35979, XEnv, XEv, 'lo.comp.canon#grTest'('lo.comp.canon#negCond'('lo.comp.canon#unifyCond'(XLc, XLhs, XRhs))), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, "\\=", XLc, XL, XR),
    'lo.comp.types@newVar'("_", XXd40650),
    XTV = XXd40650,
    'lo.comp.typecheck@typeOfTerm'(XL, XTV, XEnv, XE0, XLhs, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerm'(XR, XTV, XE0, XEv, XRhs, XRp0, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, XTp, X_35980, XEnv, XEv, 'lo.comp.canon#grDip'('lo.comp.canon#v'(XLc, XNV), XCond), XRp, XRpx):- 'lo.comp.abstract@isUnary'(XTerm, "@", XLc, XTest),
    'lo.comp.abstract@isRoundTerm'(XTest, X_35981, XOp, XArgs),
    '_str_gen'("_", XXc534),
    XNV = XXc534,
    'lo.comp.abstract@binary'(XLc, ".", 'lo.comp.ast#iden'(XLc, XNV), XOp, XXd40652),
    'lo.comp.dict@declareVar'(XNV, 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, XNV), XTp), XEnv, XXd40657),
    'lo.comp.typecheck@checkCond'('lo.comp.ast#appl'(XLc, XXd40652, 'lo.comp.ast#tupl'(XLc, "()", XArgs)), XXd40657, XEv, XCond, XRp, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, XTp, X_35982, XEnv, XEv, XNT, XRp, XRpx):- 'lo.comp.abstract@isRound'(XTerm, XLc, XF, XA),
    'lo.comp.types@newVar'("_G", XXd40658),
    XGrTp = XXd40658,
    'lo.comp.typecheck@knownType'(XF, XGrTp, XEnv, XE0, XOp, XRp, XRp0),
    'lo.comp.typecheck@cond425'(XXd40682, XXd40681, XXd40680, XXd40679, XXd40678, XXd40677, XXd40676, XXd40675, XXe5137, XXV5530, XXd40674, XXe5136, XXV5529, XF, XXd40673, XXd40672, XXd40671, XXd40670, XXd40669, XXd40668, XXd40667, XXd40666, XXd40665, XXe5135, XXV5528, XXd40664, XXe5134, XXV5527, XXd40663, XXe5133, XXV5526, XXd40662, XXd40661, XOp, XNT, XRpx, XArg, XEv, XE0, XA, XRp1, XRp0, XEnv, XTp, XLc, XXd40660, XStrmTp, XArgTp, XXd40659, XGrTp).
'lo.comp.typecheck@checkNonTerminal'(XTerm, XStreamTp, XElTp, XEnv, XEv, 'lo.comp.canon#grDip'('lo.comp.canon#v'(XLc, XNV), XCond), XRp, XRpx):- 'lo.comp.abstract@isIden'(XTerm, XLc, "eof"),
    '_str_gen'("_", XXc535),
    XNV = XXc535,
    'lo.comp.abstract@unary'(XLc, "_eof", 'lo.comp.ast#iden'(XLc, XNV), XXd40684),
    'lo.comp.dict@declareVar'(XNV, 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, XNV), XStreamTp), XEnv, XXd40687),
    'lo.comp.typecheck@checkCond'(XXd40684, XXd40687, XEv, XCond, XRp, XRpx).
'lo.comp.typecheck@checkNonTerminal'(XTerm, X_35993, X_35994, XEnv, XEx, 'lo.comp.canon#grTest'(XCond), XRp, XRpx):- 'lo.comp.abstract@isBraceTuple'(XTerm, XLc, XEls),
    'lo.comp.typecheck@checkConds'(XEls, XEnv, XEx, XCond, XRp, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEnv, 'lo.comp.canon#trueCond', XRp, XRp):- 'lo.comp.abstract@isIden'(XTerm, XLc, "true").
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEnv, 'lo.comp.canon#falseCond', XRp, XRp):- 'lo.comp.abstract@isIden'(XTerm, XLc, "false").
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEx, 'lo.comp.canon#conjCond'(XLhs, XRhs), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, ",", X_35995, XL, XR),
    'lo.comp.typecheck@checkCond'(XL, XEnv, XE1, XLhs, XRp, XRp0),
    'lo.comp.typecheck@checkCond'(XR, XE1, XEx, XRhs, XRp0, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEx, 'lo.comp.canon#condCond'(XTest, XEither, XOr), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, "|", X_35996, XL, XR),
    'lo.comp.abstract@isBinary'(XL, "?", X_35997, XT, XTh),
    'lo.comp.typecheck@checkCond'(XT, XEnv, XE0, XTest, XRp, XRp0),
    'lo.comp.typecheck@checkCond'(XTh, XE0, XE1, XEither, XRp0, XRp1),
    'lo.comp.typecheck@checkCond'(XR, XE1, XEx, XOr, XRp1, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEx, 'lo.comp.canon#disjCond'(XEither, XOr), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, "|", X_35998, XL, XR),
    'lo.comp.typecheck@checkCond'(XL, XEnv, XE1, XEither, XRp, XRp0),
    'lo.comp.typecheck@checkCond'(XR, XE1, XEx, XOr, XRp0, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEx, 'lo.comp.canon#oneCond'(XTest), XRp, XRpx):- 'lo.comp.abstract@isUnary'(XTerm, "!", X_35999, XN),
    'lo.comp.typecheck@checkCond'(XN, XEnv, XEx, XTest, XRp, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEnv, 'lo.comp.canon#negCond'(XTest), XRp, XRpx):- 'lo.comp.abstract@isUnary'(XTerm, "\\+", X_36000, XN),
    'lo.comp.typecheck@checkCond'(XN, XEnv, X_36001, XTest, XRp, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEnv, 'lo.comp.canon#forallCond'(XGen, XTest), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, "*>", X_36002, XL, XR),
    'lo.comp.typecheck@checkCond'(XL, XEnv, XE0, XGen, XRp, XRp0),
    'lo.comp.typecheck@checkCond'(XR, XE0, X_36003, XTest, XRp0, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEx, XCond, XRp, XRpx):- 'lo.comp.abstract@isRoundTuple'(XTerm, X_36004, XC),
    'lo.comp.typecheck@checkConds'(XC, XEnv, XEx, XCond, XRp, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEv, 'lo.comp.canon#negCond'('lo.comp.canon#unifyCond'(XLc, XLhs, XRhs)), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, "\\=", XLc, XL, XR),
    'lo.comp.types@newVar'("_#", XXd40688),
    XTV = XXd40688,
    'lo.comp.typecheck@typeOfTerm'(XL, XTV, XEnv, XE0, XLhs, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerm'(XR, XTV, XE0, XEv, XRhs, XRp0, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEv, 'lo.comp.canon#unifyCond'(XLc, XLhs, XRhs), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, "=", XLc, XL, XR),
    'lo.comp.types@newVar'("_#", XXd40689),
    XTV = XXd40689,
    'lo.comp.typecheck@typeOfTerm'(XL, XTV, XEnv, XE0, XLhs, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerm'(XR, XTV, XE0, XEv, XRhs, XRp0, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEv, 'lo.comp.canon#phraseCond'(XLc, XNT, XStrm, XRest), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, "%%", XLc, XL, XR),
    'lo.comp.abstract@isBinary'(XR, "~", X_36005, XS, XM),
    'lo.comp.types@newVar'("_S", XXd40690),
    XStrmTp = XXd40690,
    'lo.comp.types@newVar'("_E", XXd40691),
    XElTp = XXd40691,
    'lo.comp.typecheck@checkGrammarType'(XLc, XEnv, XStrmTp, XElTp, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerm'(XS, XStrmTp, XEnv, XE0, XStrm, XRp0, XRp1),
    'lo.comp.typecheck@typeOfTerm'(XM, XStrmTp, XE0, XE1, XRest, XRp1, XRp2),
    'lo.comp.typecheck@streamVar'(XstreamVar11),
    'lo.comp.typecheck@streamVar'(XstreamVar12),
    'lo.comp.dict@declareVar'(XstreamVar11, 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, XstreamVar12), XStrmTp), XE1, XXd40694),
    'lo.comp.typecheck@checkNonTerminal'(XL, XStrmTp, XElTp, XXd40694, XEv, XNT, XRp2, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEv, 'lo.comp.canon#phraseCond'(XLc, XNT, XStrm, XRest), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, "%%", XLc, XL, XR),
    'lo.comp.types@newVar'("_S", XXd40695),
    XStrmTp = XXd40695,
    'lo.comp.types@newVar'("_E", XXd40696),
    XElTp = XXd40696,
    'lo.comp.typecheck@checkGrammarType'(XLc, XEnv, XStrmTp, XElTp, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerm'(XR, XStrmTp, XEnv, XE0, XStrm, XRp0, XRp1),
    '_str_gen'("_", XXc536),
    XM = 'lo.comp.ast#iden'(XLc, XXc536),
    'lo.comp.typecheck@typeOfTerm'(XM, XStrmTp, XE0, XE1, XRest, XRp1, XRp2),
    'lo.comp.abstract@binary'(XLc, ",", XL, 'lo.comp.ast#iden'(XLc, "eof"), XXd40699),
    'lo.comp.dict@declareVar'("stream_X", 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, "stream_X"), XStrmTp), XE1, XXd40702),
    'lo.comp.typecheck@checkNonTerminal'(XXd40699, XStrmTp, XElTp, XXd40702, XEv, XNT, XRp2, XRpx).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEv, XCall, XRp, XRpx):- 'lo.comp.abstract@isRound'(XTerm, XLc, XF, XA),
    'lo.comp.types@newVar'("_P", XXd40703),
    XPrTp = XXd40703,
    'lo.comp.typecheck@knownType'(XF, XPrTp, XEnv, XE0, XPred, XRp, XRp0),
    'lo.comp.typecheck@cond426'(XXd40714, XXd40713, XXd40712, XXd40711, XXd40710, XXd40709, XXd40708, XXe5139, XXV5532, XXd40707, XXe5138, XXV5531, XXd40706, XPred, XLc, XCall, XRpx, XRp0, XArg, XEv, XE0, XA, XXd40705, XArgTp, XXd40704, XPrTp).
'lo.comp.typecheck@checkCond'(XTerm, XEnv, XEv, 'lo.comp.canon#isTrue'(XExp), XRp, XRpx):- 'lo.comp.typecheck@findType'("logical", XLc, XEnv, XRp, XRp0, XXd40715),
    'lo.comp.typecheck@knownType'(XTerm, XXd40715, XEnv, XEv, XExp, XRp0, XRpx).
'lo.comp.typecheck@checkClause'(XLc, XL, XR, 'lo.comp.types#predType'(XAT), 'lo.core#,..'('lo.comp.canon#clause'(XLc, XNm, XArgs, XBody), XDefs), XDefs, XE, XRp, XRpx):- 'lo.comp.typecheck@splitHead'(XL, X_36011, XNm, XA),
    'lo.comp.dict@pushScope'(XE, XXd40716),
    'lo.comp.typecheck@typeOfArg'(XA, XAT, XXd40716, XE0, XArgs, XRp, XRp0),
    'lo.comp.typecheck@checkCond'(XR, XE0, X_36012, XBody, XRp0, XRpx).
'lo.comp.typecheck@typeOfCall'(XLc, XFun, XA, 'lo.comp.types#funType'(XArgTp, XResTp), XTp, XEnv, XEv, 'lo.comp.canon#apply'(XLc, XFun, XArgs), XRp, XRpx):- 'lo.comp.typecheck@checkType'(XLc, XResTp, XTp, XEnv, XRp, XRp0),
    'lo.comp.typecheck@typeOfArg'(XA, XArgTp, XEnv, XEv, XArgs, XRp0, XRpx).
'lo.comp.typecheck@typeOfCall'(XLc, XFun, XA, 'lo.comp.types#classType'(XArgTp, XResTp), XTp, XEnv, XEv, 'lo.comp.canon#apply'(XLc, XFun, XArgs), XRp, XRpx):- 'lo.comp.typecheck@checkType'(XLc, XResTp, XTp, XEnv, XRp, XRp0),
    'lo.comp.typecheck@typeOfArg'(XA, XArgTp, XEnv, XEv, XArgs, XRp0, XRpx).
'lo.comp.typecheck@typeOfCall'(XLc, XFun, X_36013, XFtp, X_36014, XEnv, XEnv, XFun, XRp, XRpx):- ocall('disp%1'(XXV5533),'lo.core$display$lo.comp.canon*canonTerm','lo.core$display$lo.comp.canon*canonTerm'),
    ocall('disp%1'(XXV5534),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('_call%2'(XFun, XXe5140),XXV5533,XXV5533),
    ocall('_call%2'(XFtp, XXe5141),XXV5534,XXV5534),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XXe5140, 'lo.core#,..'('lo.core#ss'(" : "), 'lo.core#,..'(XXe5141, 'lo.core#,..'('lo.core#ss'(" is not a function"), 'lo.core#[]'))))), XXd40724),
    'lo.comp.errors@reportError'(XXd40724, XLc, XRp, XRpx).
'lo.comp.typecheck@knownType'(XV, XTp, XEnv, XEnv, XTerm, XRp, XRpx):- 'lo.comp.typecheck@cond428'(XEx, XXd40732, XXd40731, XXd40730, XXd40729, XXd40728, XXd40727, XXd40726, XXe5142, XXV5535, XXd40725, XEv, XRpx, XRp, XTerm, XTp, XSpec, XEnv, XNm, XLc, XV).
'lo.comp.typecheck@typeOfIndex'(XLc, XMp, XArg, XTp, XEnv, XEv, XExp, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XArg, "->", X_36022, XKy, XVl),
    'lo.comp.abstract@ternary'(XLc, "_put", XMp, XKy, XVl, XXd40733),
    'lo.comp.typecheck@typeOfTerm'(XXd40733, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfIndex'(XLc, XMp, XArg, XTp, XEnv, XEv, XExp, XRp, XRpx):- 'lo.comp.abstract@isUnary'(XArg, "\\+", X_36023, XKy),
    'lo.comp.abstract@binary'(XLc, "_remove", XMp, XKy, XXd40734),
    'lo.comp.typecheck@typeOfTerm'(XXd40734, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfIndex'(XLc, XMp, XArg, XTp, XEnv, XEv, XExp, XRp, XRpx):- 'lo.comp.abstract@binary'(XLc, "find", XMp, XArg, XXd40735),
    'lo.comp.typecheck@typeOfTerm'(XXd40735, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XV, X_36024, XEnv, XEnv, 'lo.comp.canon#v'(XLc, XXa103), XRp, XRp):- 'lo.comp.abstract@isIden'(XV, XLc, "_"),
    '_str_gen'("_", XXa103).
'lo.comp.typecheck@typeOfTerm'(XV, XTp, XEnv, XEv, XTerm, XRp, XRpx):- 'lo.comp.abstract@isIden'(XV, XLc, XNm),
    'lo.comp.typecheck@cond430'(XXd40745, XXd40744, XXd40743, XXd40742, XXd40741, XXc537, XXd40740, XXd40739, XXd40738, XXd40737, XXe5143, XXV5536, XXd40736, XEv, XRpx, XRp, XTerm, XTp, XLc, XSpec, XEnv, XNm).
'lo.comp.typecheck@typeOfTerm'('lo.comp.ast#intg'(XLc, XIx), XTp, XEnv, XEnv, 'lo.comp.canon#int'(XIx), XRp, XRpx):- 'lo.comp.typecheck@findType'("integer", XLc, XEnv, XRp, XRp0, XXd40746),
    'lo.comp.typecheck@checkType'(XLc, XXd40746, XTp, XEnv, XRp0, XRpx).
'lo.comp.typecheck@typeOfTerm'('lo.comp.ast#flot'(XLc, XDx), XTp, XEnv, XEnv, 'lo.comp.canon#flt'(XDx), XRp, XRpx):- 'lo.comp.typecheck@findType'("float", XLc, XEnv, XRp, XRp0, XXd40747),
    'lo.comp.typecheck@checkType'(XLc, XXd40747, XTp, XEnv, XRp0, XRpx).
'lo.comp.typecheck@typeOfTerm'('lo.comp.ast#strg'(XLc, XSx), XTp, XEnv, XEnv, 'lo.comp.canon#str'(XSx), XRp, XRpx):- 'lo.comp.typecheck@findType'("string", XLc, XEnv, XRp, XRp0, XXd40748),
    'lo.comp.typecheck@checkType'(XLc, XXd40748, XTp, XEnv, XRp0, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEv, XExp, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, ":", XLc, XL, XR),
    'lo.comp.typecheck@cond431'(XXd40753, XXd40752, XXd40751, XXd40750, XXe5144, XXV5537, XXd40749, XRpx, XExp, XEv, XL, XRp1, XTp, XLc, XRp0, XRp, XRT, XEnv, XR).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEv, XExp, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, "::", XLc, XL, XR),
    'lo.comp.abstract@unary'(XLc, "_coerce", XL, XXd40754),
    'lo.comp.abstract@binary'(XLc, ":", XXd40754, XR, XXd40755),
    'lo.comp.typecheck@typeOfTerm'(XXd40755, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XP, XTp, XEnv, XEx, 'lo.comp.canon#whre'(XPtn, XCond), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XP, "@@", X_36029, XL, XR),
    'lo.comp.typecheck@typeOfTerm'(XL, XTp, XEnv, XE0, XPtn, XRp, XRp0),
    'lo.comp.typecheck@checkCond'(XR, XE0, XEx, XCond, XRp0, XRpx).
'lo.comp.typecheck@typeOfTerm'(XCall, XTp, XEnv, XEv, 'lo.comp.canon#whre'(XV, XCond), XRp, XRpx):- 'lo.comp.abstract@isUnary'(XCall, "@", XLc, XTest),
    'lo.comp.abstract@isRoundTerm'(XTest, X_36030, X_36031, X_36032),
    '_str_gen'("_", XXc538),
    XNV = 'lo.comp.ast#iden'(XLc, XXc538),
    'lo.comp.typecheck@typeOfTerm'(XNV, XTp, XEnv, XE0, XV, XRp, XRp0),
    'lo.comp.abstract@binary'(XLc, ".", XNV, XTest, XXd40757),
    'lo.comp.typecheck@checkCond'(XXd40757, XE0, XEv, XCond, XRp0, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEv, XExp, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, ".", XLc, XL, XF),
    'lo.comp.abstract@isIden'(XF, XFLc, XFld),
    'lo.comp.typecheck@recordAccessExp'(XFLc, XL, XFld, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEv, 'lo.comp.canon#condExp'(XTest, XThen, XElse), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, "|", XLc, XL, XEl),
    'lo.comp.abstract@isBinary'(XL, "?", X_36033, XTst, XTh),
    'lo.comp.typecheck@checkCond'(XTst, XEnv, XE0, XTest, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerm'(XTh, XTp, XE0, XE1, XThen, XRp0, XRp1),
    'lo.comp.typecheck@typeOfTerm'(XEl, XTp, XE1, XEv, XElse, XRp1, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEv, XExp, XRp, XRpx):- 'lo.comp.abstract@isSquareTuple'(XTerm, XLc, XEls),
    'lo.comp.typecheck@checkSquareTuple'(XLc, XEls, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'('lo.comp.ast#tupl'(X_36034, "()", 'lo.core#,..'(XInner, 'lo.core#[]')), XTp, XEnv, XEv, XExp, XRp, XRpx):- 'lo.comp.typecheck@neg342'(X_36037, X_36036, XInner),
    'lo.comp.typecheck@typeOfTerm'(XInner, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEv, 'lo.comp.canon#tpl'(XEls), XRp, XRpx):- 'lo.comp.abstract@isRoundTuple'(XTerm, XLc, XA),
    'lo.comp.typecheck@genTpVars'(XA, XXd40758),
    XArgTps = XXd40758,
    'lo.comp.typecheck@checkType'(XLc, 'lo.comp.types#tupleType'(XArgTps), XTp, XEnv, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerms'(XA, XArgTps, XEnv, XEv, XLc, XEls, XRp0, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEnv, 'lo.comp.canon#int'(XXe5145), XRp, XRpx):- 'lo.comp.abstract@isUnary'(XTerm, "-", XLc, 'lo.comp.ast#intg'(X_36038, XIx)),
    'lo.comp.typecheck@findType'("integer", XLc, XEnv, XRp, XRp0, XXd40761),
    'lo.comp.typecheck@checkType'(XLc, XXd40761, XTp, XEnv, XRp0, XRpx),
    ocall('-%1'(XXV5539),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('zero%1'(XXV5538),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XXV5538, XIx, XXe5145),XXV5539,XXV5539).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEnv, 'lo.comp.canon#flt'(XXe5146), XRp, XRpx):- 'lo.comp.abstract@isUnary'(XTerm, "-", XLc, 'lo.comp.ast#flot'(X_36039, XDx)),
    'lo.comp.typecheck@findType'("float", XLc, XEnv, XRp, XRp0, XXd40763),
    'lo.comp.typecheck@checkType'(XLc, XXd40763, XTp, XEnv, XRp0, XRpx),
    ocall('-%1'(XXV5541),'lo.core$additive$lo.core*float','lo.core$additive$lo.core*float'),
    ocall('zero%1'(XXV5540),'lo.core$additive$lo.core*float','lo.core$additive$lo.core*float'),
    ocall('_call%3'(XXV5540, XDx, XXe5146),XXV5541,XXV5541).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEnv, XExp, XRp, XRpx):- 'lo.comp.abstract@isUnary'(XTerm, "-", XLc, XArg),
    'lo.comp.abstract@binary'(XLc, "-", 'lo.comp.ast#iden'(XLc, "zero"), XArg, XXd40765),
    'lo.comp.typecheck@typeOfTerm'(XXd40765, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEnv, 'lo.comp.canon#lambda'(XEqn), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, "=>", XLc, XLhs, XRhs),
    'lo.comp.types@deRef'(XTp, XXd40769),
    'lo.comp.typecheck@checkEquation'(XLc, 'lo.comp.ast#appl'(XLc, 'lo.comp.ast#iden'(XLc, "fun"), XLhs), 'lo.comp.ast#iden'(XLc, "true"), XRhs, XXd40769, 'lo.core#,..'(XEqn, X_36041), X_36042, XEnv, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEnv, 'lo.comp.canon#lambda'(XEqn), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, ":-", XLc, XL, XG),
    'lo.comp.abstract@isBinary'(XL, "=>", X_36043, XH, XR),
    'lo.comp.types@deRef'(XTp, XXd40773),
    'lo.comp.typecheck@checkEquation'(XLc, 'lo.comp.ast#appl'(XLc, 'lo.comp.ast#iden'(XLc, "fun"), XH), XG, XR, XXd40773, 'lo.core#,..'(XEqn, X_36045), X_36046, XEnv, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEnv, 'lo.comp.canon#lambda'(XClse), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, ":-", XLc, XLhs, XRhs),
    'lo.comp.types@deRef'(XTp, XXd40777),
    'lo.comp.typecheck@checkClause'(XLc, 'lo.comp.ast#appl'(XLc, 'lo.comp.ast#iden'(XLc, "pred"), XLhs), XRhs, XXd40777, 'lo.core#,..'(XClse, 'lo.core#[]'), 'lo.core#[]', XEnv, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEnv, 'lo.comp.canon#lambda'(XRle), XRp, XRpx):- 'lo.comp.abstract@isBinary'(XTerm, "-->", XLc, XLhs, XRhs),
    'lo.comp.types@deRef'(XTp, XXd40781),
    'lo.comp.typecheck@checkGrammarRule'(XLc, 'lo.comp.ast#appl'(XLc, 'lo.comp.ast#iden'(XLc, "grm"), XLhs), XR, XXd40781, 'lo.core#,..'(XRle, 'lo.core#[]'), 'lo.core#[]', XEnv, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEv, XExp, XRp, XRpx):- 'lo.comp.abstract@isRound'(XTerm, XLc, XF, XA),
    'lo.comp.types@newVar'("F", XXd40783),
    XFnTp = XXd40783,
    'lo.comp.typecheck@knownType'(XF, XFnTp, XEnv, XE0, XFun, XRp, XRp0),
    'lo.comp.types@deRef'(XFnTp, XXd40784),
    'lo.comp.typecheck@typeOfCall'(XLc, XFun, XA, XXd40784, XTp, XE0, XEv, XExp, XRp0, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEv, XExp, XRp, XRpx):- 'lo.comp.abstract@isSquareTerm'(XTerm, XLc, XF, 'lo.core#,..'(XA, 'lo.core#[]')),
    'lo.comp.typecheck@typeOfIndex'(XLc, XF, XA, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEnv, 'lo.comp.canon#theta'(XDefs, XOthers), XRp, XRpx):- 'lo.comp.abstract@isBraceTuple'(XTerm, XLc, XEls),
    '_str_gen'("theta", XXc539),
    'lo.comp.typecheck@checkThetaTerm'(XTp, XLc, XEls, XEnv, XDefs, XOthers, XTypes, XXc539, XRp, XRpx).
'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEnv, 'lo.comp.canon#v'(XLc, XXa104), XRp, XRpx):- ocall('disp%1'(XXV5542),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('disp%1'(XXV5543),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('_call%2'(XTerm, XXe5147),XXV5542,XXV5542),
    ocall('_call%2'(XTp, XXe5148),XXV5543,XXV5543),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("illegal expression: "), 'lo.core#,..'(XXe5147, 'lo.core#,..'('lo.core#ss'(", expecting a "), 'lo.core#,..'(XXe5148, 'lo.core#[]'))))), XXd40793),
    ocall('loc%1'(XXV5544),XTerm,XTerm),
    'lo.comp.errors@reportError'(XXd40793, XXV5544, XRp, XRpx),
    '_str_gen'("_", XXa104).
'lo.comp.typecheck@typeOfArg'(XTerm, XTp, XEnv, XEv, XExp, XRp, XRpx):- 'lo.comp.typecheck@cond432'(XXd40796, XExp, XRpx, XEls, XEv, XRp0, XRp, XEnv, XTp, XXd40795, XXd40794, XArgTps, XA, XLc, XTerm).
'lo.comp.typecheck@checkGrammarRule'(XLc, XL, XR, 'lo.comp.types#grammarType'(XAT, XTp), 'lo.core#,..'('lo.comp.canon#grRule'(XLc, XNm, XArgs, 'lo.comp.canon#grTerms'(XPB), XBody), XDefs), XDefs, XE, XRp, XRpx):- 'lo.comp.typecheck@splitGrHead'(XL, XNm, XA, XP),
    'lo.comp.types@newVar'("_E", XXd40797),
    XElTp = XXd40797,
    'lo.comp.dict@pushScope'(XE, XXd40800),
    'lo.comp.dict@declareVar'("stream$X", 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, "stream$X"), XTp), XXd40800, XXd40801),
    'lo.comp.typecheck@typeOfArg'(XA, XAT, XXd40801, XE2, XArgs, XRp, XRp0),
    'lo.comp.typecheck@checkNonTerminal'(XR, XTp, XElTp, XE2, XE3, XBody, XRp0, XRp1),
    'lo.comp.typecheck@typeOfTerm'('lo.comp.ast#iden'(XLc, "_hdtl"), 'lo.comp.types#predType'('lo.comp.types#tupleType'('lo.core#,..'(XStrmTp, 'lo.core#,..'(XElTp, 'lo.core#,..'(XStrmTp, 'lo.core#[]'))))), XE3, X_36058, XHDTL, XRp1, XRp2),
    'lo.comp.typecheck@checkTerminals'(XP, XHDTL, XPB, XElTp, XE3, X_36059, XRp2, XRpx).
'lo.comp.typecheck@processStmt'(XSt, XProgramType, XDefs, XDefx, XE, X_36060, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XSt, "=>", XLc, XL, XR),
    'lo.comp.typecheck@checkEquation'(XLc, XL, 'lo.comp.ast#iden'(XLc, "true"), XR, XProgramType, XDefs, XDefx, XE, XRp, XRpx).
'lo.comp.typecheck@processStmt'(XSt, XProgramType, XDefs, XDefx, XE, X_36061, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XSt, ":-", XLc, XL, XG),
    'lo.comp.abstract@isBinary'(XL, "=>", X_36062, XH, XR),
    'lo.comp.typecheck@checkEquation'(XLc, XH, XG, XR, XProgramType, XDefs, XDefx, XE, XRp, XRpx).
'lo.comp.typecheck@processStmt'(XSt, XProgramType, XDefs, XDefx, XE, XPth, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XSt, ":-", XLc, XL, XG),
    'lo.comp.abstract@isBinary'(XL, "<=", X_36063, XH, XR),
    'lo.comp.typecheck@checkClassRule'(XLc, XH, XG, XR, XProgramType, XDefs, XDefx, XE, XPth, XRp, XRpx).
'lo.comp.typecheck@processStmt'(XSt, XProgramType, XDefs, XDfx, XE, X_36064, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XSt, ":-", XLc, XL, XR),
    'lo.comp.typecheck@checkClause'(XLc, XL, XR, XProgramType, XDefs, XDfx, XE, XRp, XRpx).
'lo.comp.typecheck@processStmt'(XSt, 'lo.comp.types#predType'(XAT), 'lo.core#,..'('lo.comp.canon#clause'(XLc, XNm, XArgs, 'lo.comp.canon#trueCond'), XDefs), XDefs, XE, X_36066, XRp, XRpx):- 'lo.comp.typecheck@splitHead'(XSt, XLc, XNm, XA),
    'lo.comp.dict@pushScope'(XE, XXd40809),
    'lo.comp.typecheck@typeOfArg'(XA, XAT, XXd40809, XE0, XArgs, XRp, XRpx).
'lo.comp.typecheck@processStmt'(XSt, XTp, XDefs, XDx, XEnv, X_36067, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XSt, "=", XLc, XL, XR),
    'lo.comp.typecheck@checkDefn'(XLc, XL, 'lo.comp.ast#iden'(XLc, "true"), XR, XTp, XDefs, XDx, XEnv, XRp, XRpx).
'lo.comp.typecheck@processStmt'(XSt, XProgramType, XDefs, XDefx, XE, XPth, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XSt, "<=", XLc, XH, XR),
    'lo.comp.typecheck@checkClassRule'(XLc, XH, 'lo.comp.ast#iden'(XLc, "true"), XR, XProgramType, XDefs, XDefx, XE, XPth, XRp, XRpx).
'lo.comp.typecheck@processStmt'(XSt, XTp, XDefs, XDx, XE, X_36068, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XSt, "-->", XLc, XL, XR),
    'lo.comp.typecheck@checkGrammarRule'(XLc, XL, XR, XTp, XDefs, XDx, XE, XRp, XRpx).
'lo.comp.typecheck@processStmt'(XSt, XTp, XDefs, XDefs, X_36069, X_36070, XRp, XRpx):- ocall('disp%1'(XXV5545),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('disp%1'(XXV5546),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('_call%2'(XSt, XXe5149),XXV5545,XXV5545),
    ocall('_call%2'(XTp, XXe5150),XXV5546,XXV5546),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Statement "), 'lo.core#,..'(XXe5149, 'lo.core#,..'('lo.core#ss'(" not consistent with expected type "), 'lo.core#,..'(XXe5150, 'lo.core#[]'))))), XXd40819),
    ocall('loc%1'(XXV5547),XSt,XSt),
    'lo.comp.errors@reportError'(XXd40819, XXV5547, XRp, XRpx).
'lo.comp.typecheck@processStmts'('lo.core#[]', X_36075, XDefs, XDefs, X_36076, X_36077, XRp, XRp).
'lo.comp.typecheck@processStmts'('lo.core#,..'(XSt, XMore), XProgramType, XDefs, XDx, XEnv, XPath, XRp, XRpx):- 'lo.comp.typecheck@one311'(XRp0, XRp, XPath, XEnv, XD0, XDefs, XProgramType, XSt),
    'lo.comp.typecheck@processStmts'(XMore, XProgramType, XD0, XDx, XEnv, XPath, XRp0, XRpx).
'lo.comp.typecheck@checkVarRules'('lo.core#[]', X_36079, XDefs, XDefs, X_36080, XRp, XRp).
'lo.comp.typecheck@checkVarRules'('lo.core#,..'('lo.comp.dependencies#defn'(XN, 'lo.comp.abstract#valu', XLc, X_36082, XStmts), XMore), XEnv, XDefs, XDx, XPath, XRp, XRpx):- ocall('skolemize%1'(XXV5548),'lo.comp.freshen$freshen$lo.comp.types*tipe','lo.comp.freshen$freshen$lo.comp.types*tipe'),
    'lo.comp.typecheck@pickupVarType'(XN, XLc, XEnv, XRp, XRp0, XXd40820),
    'lo.comp.typecheck@pickupThisType'(XEnv, XXd40821),
    ocall('_call%3'(XXd40820, XXd40821, XXe5151),XXV5548,XXV5548),
    '()2'(XQ, XPT) = XXe5151,
    ocall('pairs%1'(XXV5549),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%2'(XQ, XXe5152),XXV5549,XXV5549),
    'lo.comp.typecheck@declareTypeVars'(XXe5152, XLc, XEnv, XSEnv),
    'lo.comp.types@moveConstraints'(XPT, XCx, XProgramType),
    'lo.comp.typecheck@declareConstraints'(XCx, XSEnv, XStmtEnv),
    'lo.comp.typecheck@processStmts'(XStmts, XProgramType, XRules, 'lo.core#[]', XStmtEnv, XPath, XRp0, XRp1),
    'lo.comp.typecheck@collectPrograms'(XRules, XEnv, XCx, XDefs, XD0),
    'lo.comp.typecheck@checkVarRules'(XMore, XEnv, XD0, XDx, XPath, XRp1, XRpx).
'lo.comp.typecheck@varGroup'(XGrp, XFields, XAnnots, XDefs, XDx, XBase, XEnv, XPath, XRp, XRpx):- 'lo.comp.typecheck@one312'(XRp0, XRp, XPath, XEnv, XBase, XAnnots, XFields, XGrp),
    'lo.comp.typecheck@checkVarRules'(XGrp, XEnv, XDefs, XDx, XPath, XRp0, XRpx).
'lo.comp.typecheck@checkThetaTerm'(XClassTp, XLc, XEls, XEnv, XDefs, XOthers, XTypes, XClassPath, XRp, XRpx):- 'lo.comp.types@deRef'(XClassTp, XXd40823),
    'lo.comp.unify@faceOfType'(XXd40823, XEnv, XFace),
    'lo.comp.types@moveConstraints'(XFace, X_36083, 'lo.comp.types#faceType'(XFields)),
    'lo.comp.dict@declareVar'("this", 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, "this"), XClassTp), XEnv, XXd40827),
    'lo.comp.typecheck@thetaEnv'('lo.repo$repository$lo.repo*coreRepo', XClassPath, 'lo.repo#coreRepo', XLc, XEls, XFields, XXd40827, X_OEnv, XDefs, XPublic, X_Imports, X_36084, XOthers, XRp, XRpx),
    'lo.comp.typecheck@computeExport'(XDefs, XFields, XPublic, X_36085, XTypes, X_36086, 'lo.core#[]', 'lo.core#[]').
'lo.comp.typecheck@implementationGroup'('lo.core#,..'('lo.comp.dependencies#defn'(XINm, 'lo.comp.abstract#impl', X_36088, X_36089, 'lo.core#,..'(XStmt, 'lo.core#[]')), 'lo.core#[]'), 'lo.core#,..'(XImpl, XDefs), XDefs, XEnv, XEx, XPath, XRp, XRpx):- 'lo.comp.abstract@isUnary'(XStmt, "implementation", XLc, XI),
    'lo.comp.abstract@isBinary'(XI, "<=", X_36092, XSq, XBody),
    'lo.comp.abstract@isBraceTuple'(XBody, X_36093, XEls),
    'lo.comp.parseType@parseContractConstraint'(XSq, XEnv, XNm, XSpec, XRp, XRp0),
    'lo.comp.dict@isContract'(XNm, XEnv, 'lo.comp.types#conEntry'(X_36094, XCNm, XFullSpec, XConFace)),
    'lo.comp.types@moveConQuants'(XFullSpec, X_36095, 'lo.comp.types#conTract'(XConNm, XOArgs, XODeps)),
    'lo.comp.types@moveConQuants'(XSpec, XSQ, XASpec),
    'lo.comp.types@moveConConstraints'(XASpec, XAC, 'lo.comp.types#conTract'(XConNm, XAArgs, XADeps)),
    'lo.comp.typecheck@sameLength'(XOArgs, XAArgs, XLc, XRp0, XRp1),
    'lo.comp.typecheck@sameLength'(XODeps, XADeps, XLc, XRp1, XRp2),
    ocall('_empty%1'(XXV5550),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.types@bindAT'(XOArgs, XAArgs, XXV5550, XXd40832),
    'lo.comp.types@bindAT'(XODeps, XADeps, XXd40832, XXd40833),
    XQQ = XXd40833,
    'lo.comp.types@moveQuants'(XConFace, X_36096, XCFace),
    ocall('freshen%1'(XXV5551),'lo.comp.freshen$freshen$lo.comp.types*tipe','lo.comp.freshen$freshen$lo.comp.types*tipe'),
    ocall('_call%3'(XCFace, XQQ, XXe5153),XXV5551,XXV5551),
    '()2'(X_36097, XF) = XXe5153,
    'lo.comp.types@moveConstraints'(XCF, XAC, XF),
    'lo.comp.types@moveQuants'(XFT, XSQ, XCF),
    ocall('skolemize%1'(XXV5553),'lo.comp.freshen$freshen$lo.comp.types*tipe','lo.comp.freshen$freshen$lo.comp.types*tipe'),
    ocall('_empty%1'(XXV5552),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%3'(XFT, XXV5552, XXe5154),XXV5553,XXV5553),
    '()2'(XIQ, XSF) = XXe5154,
    'lo.comp.types@moveConstraints'(XSF, XSC, XFace),
    'lo.comp.dict@pushScope'(XEnv, XXd40835),
    'lo.comp.typecheck@declareConstraints'(XSC, XXd40835, XE0),
    ocall('pairs%1'(XXV5554),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%2'(XIQ, XXe5155),XXV5554,XXV5554),
    'lo.comp.typecheck@declareTypeVars'(XXe5155, XLc, XE0, XSEnv),
    'lo.comp.types@implementationName'('lo.comp.types#conTract'(XCNm, XAArgs, XADeps), XXd40838),
    XXd40838 = XImplName,
    'lo.comp.typecheck@checkThetaTerm'(XFace, XLc, XEls, XSEnv, XThDefs, XOthers, XTypes, XImplName, XRp0, XRpx),
    XImpl = 'lo.comp.canon#implDef'(XLc, XINm, XImplName, XSpec, XSC, 'lo.comp.canon#tpl'('lo.core#[]'), 'lo.comp.canon#theta'(XThDefs, XOthers), XFace),
    'lo.comp.dict@declareImplementation'(XNm, XImplName, 'lo.comp.types#implEntry'(XImplName, XSpec), XEnv, XXd40843),
    XEx = XXd40843.
'lo.comp.typecheck@implementationGroup'('lo.core#,..'('lo.comp.dependencies#defn'(XINm, 'lo.comp.abstract#impl', XLc, X_36099, XStmt), 'lo.core#[]'), XDefs, XDefs, XEnv, XE, XPath, XRp, XRpx):- ocall('disp%1'(XXV5555),'lo.core$display$lo.core*list'('lo.core$display$lo.comp.ast*ast'),'lo.core$display$lo.core*list'('lo.core$display$lo.comp.ast*ast')),
    ocall('_call%2'(XStmt, XXe5156),XXV5555,XXV5555),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("could not check implementation statement "), 'lo.core#,..'(XXe5156, 'lo.core#[]'))), XXd40849),
    'lo.comp.errors@reportError'(XXd40849, XLc, XRp, XRpx).
'lo.comp.typecheck@checkGroup'(XGrp, 'lo.core#,..'('lo.comp.dependencies#defn'(X_36103, 'lo.comp.abstract#tpe', X_36104, X_36105, X_36106), X_36107), X_36108, X_36109, XDefs, XDx, XEnv, XEx, XPath, XRp, XRpx):- 'lo.comp.typecheck@typeGroup'(XGrp, XDefs, XDx, XEnv, XEx, XPath, XRp, XRpx).
'lo.comp.typecheck@checkGroup'(XGrp, 'lo.core#,..'('lo.comp.dependencies#defn'(X_36111, 'lo.comp.abstract#valu', X_36112, X_36113, X_36114), X_36115), XFields, XAnnots, XDefs, XDx, XEnv, XEx, XPath, XRp, XRpx):- 'lo.comp.typecheck@varGroup'(XGrp, XFields, XAnnots, XDefs, XDx, XEnv, XEx, XPath, XRp, XRpx).
'lo.comp.typecheck@checkGroup'(XGrp, 'lo.core#,..'('lo.comp.dependencies#defn'(X_36117, 'lo.comp.abstract#con', X_36118, X_36119, X_36120), X_36121), X_36122, X_36123, XDefs, XDx, XEnv, XEx, XPath, XRp, XRpx):- 'lo.comp.typecheck@contractGroup'(XGrp, XDefs, XDx, XEnv, XEx, XPath, XRp, XRpx).
'lo.comp.typecheck@checkGroup'(XGrp, 'lo.core#,..'('lo.comp.dependencies#defn'(X_36125, 'lo.comp.abstract#impl', X_36126, X_36127, X_36128), X_36129), X_36130, X_36131, XDefs, XDx, XEnv, XEx, XPath, XRp, XRpx):- 'lo.comp.typecheck@implementationGroup'(XGrp, XDefs, XDx, XEnv, XEx, XPath, XRp, XRpx).
'lo.comp.typecheck@checkGroups'('lo.core#[]', X_36132, X_36133, 'lo.core#[]', XE, XE, X_36134, XRp, XRp).
'lo.comp.typecheck@checkGroups'('lo.core#,..'(XGp, XMore), XFields, XAnnots, XDefs, XEnv, XE, XPath, XRp, XRpx):- 'lo.comp.typecheck@checkGroup'(XGp, XGp, XFields, XAnnots, XDefs, XD0, XEnv, XE0, XPath, XRp, XRp0),
    'lo.comp.typecheck@checkGroups'(XMore, XFields, XAnnots, XD0, XE0, XE, XPath, XRp0, XRpx).
'lo.comp.typecheck@thetaEnv'(Xrepository65, XPath, XRepo, XLc, XEls, XFields, XBase, XTheEnv, XDefs, XPublic, XImports, XImportClosure, XOthers, XRp, XRpx):- 'lo.comp.macro@macroRewrite'(XEls, XStmts, XRp, XRp0),
    'lo.comp.dependencies@dependencies'(XStmts, XGroups, XPublic, XAnnots, XImps, XOtrs, XRp0, XRp1),
    'lo.comp.typecheck@processImportGroup'(Xrepository65, XImps, XImports, XImportClosure, XLc, XRepo, XBase, XIBase, XRp1, XRp2),
    'lo.comp.typecheck@pushFace'(XFields, XLc, XIBase, XXd40850),
    'lo.comp.typecheck@checkGroups'(XGroups, XFields, XAnnots, XDefs, XXd40850, XTheEnv, XPath, XRp2, XRp3),
    'lo.comp.typecheck@checkOthers'(XOtrs, XOthers, XTheEnv, XPath, XRp3, XRpx).
'lo.comp.typecheck@findImportedImplementations'('lo.core#,..'('lo.comp.package#pkgSpec'(X_36137, X_36138, X_36139, X_36140, X_36141, XImpls, X_36142), XSpecs), XD0, XD1):- 'lo.list@<>'(XImpls, XD0, XXd40851),
    'lo.comp.typecheck@findImportedImplementations'(XSpecs, XXd40851, XD1).
'lo.comp.typecheck@findImportedImplementations'('lo.core#[]', XD, XD).
'lo.comp.typecheck@checkProgram'(Xrepository66, XProg, XV, XRepo, XRp, XRpx, 'lo.comp.canon#canonPkg'('lo.comp.package#pkgSpec'('lo.repo#pkg'(XPkgNm, XV), XExports, XTypes, XEnums, XContracts, XImpls, XImports), XImportClosure, XODefs, XOOthers)):- 'lo.comp.abstract@isBraceTerm'(XProg, XLc, XPk, XEls),
    'lo.comp.abstract@packageName'(XPk, XXd40852),
    XPkgNm = XXd40852,
    'lo.comp.dict@stdDict'(XstdDict6),
    'lo.comp.dict@pushScope'(XstdDict6, XXd40853),
    'lo.comp.typecheck@thetaEnv'(Xrepository66, XPkgNm, XRepo, XLc, XEls, 'lo.core#[]', XXd40853, X_36143, XDefs, XPublic, XImports, XImportClosure, XOthers, XRp, XRp0),
    'lo.comp.typecheck@cond433'(XXd40854, XRpx, XOthers, XOOthers, XRp1, XODefs, XODict, XDefs, XOverDict, XImportClosure, XRp0),
    'lo.comp.typecheck@computeExport'(XODefs, 'lo.core#[]', XPublic, XExports, XTypes, XEnums, XContracts, XImpls),
    !.
'lo.comp.typecheck@checkProgram'(_, _, _, _, _, _, _):- raise_exception('error'("lo.comp.typecheck@checkProgram", 25, 3, 626)).
'lo.comp.typecheck@exportViz'(XNm, XK, XP, 'lo.comp.package#pUblic'):- ocall('in%2'('()2'(XNm, XK), XP),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !.
'lo.comp.typecheck@exportViz'(X_36144, X_36145, X_36146, 'lo.comp.package#priVate'):- !.
'lo.comp.typecheck@exportViz'(_, _, _, _):- raise_exception('error'("lo.comp.typecheck@exportViz", 960, 3, 42)).
'lo.comp.typecheck@computeImports'(X_36147, 'lo.core#[]', 'lo.core#[]'):- !.
'lo.comp.typecheck@computeImports'(XPub, 'lo.core#,..'('lo.comp.package#pkgSpec'('lo.repo#pkg'(XPk, XV), X_36149, X_36150, X_36151, X_36152, X_36153, X_36154), XPkgs), 'lo.core#,..'('()2'(XXd40858, 'lo.repo#pkg'(XPk, XV)), XXd40860)):- !,
    'lo.comp.typecheck@exportViz'(XPk, 'lo.comp.abstract#imp', XPub, XXd40858),
    'lo.comp.typecheck@computeImports'(XPub, XPkgs, XXd40860).
'lo.comp.typecheck@computeImports'(_, _, _):- raise_exception('error'("lo.comp.typecheck@computeImports", 39, 3, 24)).
'lo.comp.typecheck@addPublicImports'('lo.core#[]', X_36156, 'lo.core#[]').
'lo.comp.typecheck@addPublicImports'('lo.core#,..'('()2'('lo.comp.package#pUblic', XPkg), XI), XLc, 'lo.core#,..'('()3'(XLc, 'lo.comp.package#pUblic', XPkg), XRest)):- 'lo.comp.typecheck@addPublicImports'(XI, XLc, XRest).
'lo.comp.typecheck@addPublicImports'('lo.core#,..'('()2'('lo.comp.package#priVate', X_36160), XI), XLc, XRest):- 'lo.comp.typecheck@addPublicImports'(XI, XLc, XRest).
'lo.comp.typecheck@mkTypes'(0, 'lo.core#[]'):- !.
'lo.comp.typecheck@mkTypes'(XN, 'lo.core#,..'(XXd40862, XXd40863)):- !,
    ocall('-%1'(XXV5556),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.types@newVar'("_", XXd40862),
    ocall('_call%3'(XN, 1, XXe5157),XXV5556,XXV5556),
    'lo.comp.typecheck@mkTypes'(XXe5157, XXd40863).
'lo.comp.typecheck@mkTypes'(_, _):- raise_exception('error'("lo.comp.typecheck@mkTypes", 915, 3, 16)).
'lo.comp.typecheck@pickVar'('lo.comp.dict#vr'(XTrm, X_36162), XTrm):- !.
'lo.comp.typecheck@pickVar'(_, _):- raise_exception('error'("lo.comp.typecheck@pickVar", 922, 3, 25)).
'lo.comp.typecheck@computeNewVars'(XEnv, XXe5159):- !,
    ocall('values%1'(XXV5557),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('//%1'(XXV5558),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    'lo.comp.dict@topVars'(XEnv, XXd40865),
    ocall('_call%2'(XXd40865, XXe5158),XXV5557,XXV5557),
    ocall('_call%3'(XXe5158, 'lo.comp.typecheck^pickVar', XXe5159),XXV5558,XXV5558).
'lo.comp.typecheck@computeNewVars'(_, _):- raise_exception('error'("lo.comp.typecheck@computeNewVars", 919, 3, 54)).
'lo.comp.typecheck^findImport'('_call%3'(XV32427, XV32428, XV32429), 'lo.comp.typecheck^findImport', _):- 'lo.comp.typecheck@findImport'(XV32427, XV32428, XV32429).
'lo.comp.typecheck^findAllImports'('_call%2'(XV32430, XV32431), 'lo.comp.typecheck^findAllImports', _):- 'lo.comp.typecheck@findAllImports'(XV32430, XV32431).
'lo.comp.typecheck^second'('_call%2'(XV32432, XV32433), 'lo.comp.typecheck^second', _):- 'lo.comp.typecheck@second'(XV32432, XV32433).
'lo.comp.typecheck^declareFields'('_call%4'(XV32434, XV32435, XV32436, XV32437), 'lo.comp.typecheck^declareFields', _):- 'lo.comp.typecheck@declareFields'(XV32434, XV32435, XV32436, XV32437).
'lo.comp.typecheck^pickTypeTemplate'('_call%2'(XV32438, XV32439), 'lo.comp.typecheck^pickTypeTemplate', _):- 'lo.comp.typecheck@pickTypeTemplate'(XV32438, XV32439).
'lo.comp.typecheck^importTypes'('_call%4'(XV32440, XV32441, XV32442, XV32443), 'lo.comp.typecheck^importTypes', _):- 'lo.comp.typecheck@importTypes'(XV32440, XV32441, XV32442, XV32443).
'lo.comp.typecheck^formMethods'('_call%6'(XV32444, XV32445, XV32446, XV32447, XV32448, XV32449), 'lo.comp.typecheck^formMethods', _):- 'lo.comp.typecheck@formMethods'(XV32444, XV32445, XV32446, XV32447, XV32448, XV32449).
'lo.comp.typecheck^declareMethods'('_call%5'(XV32450, XV32451, XV32452, XV32453, XV32454), 'lo.comp.typecheck^declareMethods', _):- 'lo.comp.typecheck@declareMethods'(XV32450, XV32451, XV32452, XV32453, XV32454).
'lo.comp.typecheck^defineContract'('_call%4'(XV32455, XV32456, XV32457, XV32458), 'lo.comp.typecheck^defineContract', _):- 'lo.comp.typecheck@defineContract'(XV32455, XV32456, XV32457, XV32458).
'lo.comp.typecheck^importContracts'('_call%4'(XV32459, XV32460, XV32461, XV32462), 'lo.comp.typecheck^importContracts', _):- 'lo.comp.typecheck@importContracts'(XV32459, XV32460, XV32461, XV32462).
'lo.comp.typecheck^importDefs'('_call%4'(XV32463, XV32464, XV32465, XV32466), 'lo.comp.typecheck^importDefs', _):- 'lo.comp.typecheck@importDefs'(XV32463, XV32464, XV32465, XV32466).
'lo.comp.typecheck^importAllDefs'('_call%4'(XV32467, XV32468, XV32469, XV32470), 'lo.comp.typecheck^importAllDefs', _):- 'lo.comp.typecheck@importAllDefs'(XV32467, XV32468, XV32469, XV32470).
'lo.comp.typecheck^processImportGroup'('_call%10'(XV32471, XV32472, XV32473, XV32474, XV32475, XV32476, XV32477, XV32478, XV32479, XV32480), 'lo.comp.typecheck^processImportGroup', _):- 'lo.comp.typecheck@processImportGroup'(XV32471, XV32472, XV32473, XV32474, XV32475, XV32476, XV32477, XV32478, XV32479, XV32480).
'lo.comp.typecheck^pushFace'('_call%4'(XV32481, XV32482, XV32483, XV32484), 'lo.comp.typecheck^pushFace', _):- 'lo.comp.typecheck@pushFace'(XV32481, XV32482, XV32483, XV32484).
'lo.comp.typecheck^sameLength'('_call%5'(XV32485, XV32486, XV32487, XV32488, XV32489), 'lo.comp.typecheck^sameLength', _):- 'lo.comp.typecheck@sameLength'(XV32485, XV32486, XV32487, XV32488, XV32489).
'lo.comp.typecheck^declareConstraints'('_call%3'(XV32490, XV32491, XV32492), 'lo.comp.typecheck^declareConstraints', _):- 'lo.comp.typecheck@declareConstraints'(XV32490, XV32491, XV32492).
'lo.comp.typecheck^declareTypeVars'('_call%4'(XV32493, XV32494, XV32495, XV32496), 'lo.comp.typecheck^declareTypeVars', _):- 'lo.comp.typecheck@declareTypeVars'(XV32493, XV32494, XV32495, XV32496).
'lo.comp.typecheck^isPublicImplementation'('_call%2'(XV32497, XV32498), 'lo.comp.typecheck^isPublicImplementation', _):- 'lo.comp.typecheck@isPublicImplementation'(XV32497, XV32498).
'lo.comp.typecheck^isPublicType'('_call%2'(XV32499, XV32500), 'lo.comp.typecheck^isPublicType', _):- 'lo.comp.typecheck@isPublicType'(XV32499, XV32500).
'lo.comp.typecheck^isPublicVar'('_call%3'(XV32501, XV32502, XV32503), 'lo.comp.typecheck^isPublicVar', _):- 'lo.comp.typecheck@isPublicVar'(XV32501, XV32502, XV32503).
'lo.comp.typecheck@one303'(XPublic, XINm):- 'lo.comp.typecheck@isPublicImplementation'(XINm, XPublic),
    !.
'lo.comp.typecheck^exportDef'('_call%13'(XV32504, XV32505, XV32506, XV32507, XV32508, XV32509, XV32510, XV32511, XV32512, XV32513, XV32514, XV32515, XV32516), 'lo.comp.typecheck^exportDef', _):- 'lo.comp.typecheck@exportDef'(XV32504, XV32505, XV32506, XV32507, XV32508, XV32509, XV32510, XV32511, XV32512, XV32513, XV32514, XV32515, XV32516).
'lo.comp.typecheck@one304'(XIx, XImpls, XCx, XContracts, XClx, XEnums, XTx, XTypes, XEx, XExports, XPublic, XFields, XDef):- 'lo.comp.typecheck@exportDef'(XDef, XFields, XPublic, XExports, XEx, XTypes, XTx, XEnums, XClx, XContracts, XCx, XImpls, XIx),
    !.
'lo.comp.typecheck^computeExport'('_call%8'(XV32517, XV32518, XV32519, XV32520, XV32521, XV32522, XV32523, XV32524), 'lo.comp.typecheck^computeExport', _):- 'lo.comp.typecheck@computeExport'(XV32517, XV32518, XV32519, XV32520, XV32521, XV32522, XV32523, XV32524).
'lo.comp.typecheck^contractGroup'('_call%8'(XV32525, XV32526, XV32527, XV32528, XV32529, XV32530, XV32531, XV32532), 'lo.comp.typecheck^contractGroup', _):- 'lo.comp.typecheck@contractGroup'(XV32525, XV32526, XV32527, XV32528, XV32529, XV32530, XV32531, XV32532).
'lo.comp.typecheck@one305'(XAnnots, XAnnot, XNm):- ocall('in%2'('()2'(XNm, XAnnot), XAnnots),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !.
'lo.comp.typecheck@one306'(XFields, XTp, XN):- ocall('in%2'('()2'(XN, XTp), XFields),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !.
'lo.comp.typecheck^parseAnnotations'('_call%8'(XV32533, XV32534, XV32535, XV32536, XV32537, XV32538, XV32539, XV32540), 'lo.comp.typecheck^parseAnnotations', _):- 'lo.comp.typecheck@parseAnnotations'(XV32533, XV32534, XV32535, XV32536, XV32537, XV32538, XV32539, XV32540).
'lo.comp.typecheck^pickupThisType'('_call%2'(XV32541, XV32542), 'lo.comp.typecheck^pickupThisType', _):- 'lo.comp.typecheck@pickupThisType'(XV32541, XV32542).
'lo.comp.typecheck^pickupVarType'('_call%6'(XV32543, XV32544, XV32545, XV32546, XV32547, XV32548), 'lo.comp.typecheck^pickupVarType', _):- 'lo.comp.typecheck@pickupVarType'(XV32543, XV32544, XV32545, XV32546, XV32547, XV32548).
'lo.comp.typecheck^splitHead'('_call%4'(XV32549, XV32550, XV32551, XV32552), 'lo.comp.typecheck^splitHead', _):- 'lo.comp.typecheck@splitHead'(XV32549, XV32550, XV32551, XV32552).
'lo.comp.typecheck^splitGrHead'('_call%4'(XV32553, XV32554, XV32555, XV32556), 'lo.comp.typecheck^splitGrHead', _):- 'lo.comp.typecheck@splitGrHead'(XV32553, XV32554, XV32555, XV32556).
'lo.comp.typecheck^manageConstraints'('_call%6'(XV32557, XV32558, XV32559, XV32560, XV32561, XV32562), 'lo.comp.typecheck^manageConstraints', _):- 'lo.comp.typecheck@manageConstraints'(XV32557, XV32558, XV32559, XV32560, XV32561, XV32562).
'lo.comp.typecheck@cond420'(XLc, XXd40504, XXd40503, XXd40502, XXd40501, XXd40500, XXe5118, XXV5502, XXd40499, XXe5117, XXV5501, XRpx, XRp, XEnv, XExpected, XActual):- 'lo.comp.unify@sameType'(XActual, XExpected, XEnv),
    !,
    XRp = XRpx.
'lo.comp.typecheck@cond420'(XLc, XXd40504, XXd40503, XXd40502, XXd40501, XXd40500, XXe5118, XXV5502, XXd40499, XXe5117, XXV5501, XRpx, XRp, XEnv, XExpected, XActual):- ocall('disp%1'(XXV5501),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('disp%1'(XXV5502),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('_call%2'(XActual, XXe5117),XXV5501,XXV5501),
    ocall('_call%2'(XExpected, XXe5118),XXV5502,XXV5502),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XXe5117, 'lo.core#,..'('lo.core#ss'(" not consistent with "), 'lo.core#,..'(XXe5118, 'lo.core#[]')))), XXd40504),
    'lo.comp.errors@reportError'(XXd40504, XLc, XRp, XRpx).
'lo.comp.typecheck^checkType'('_call%6'(XV32563, XV32564, XV32565, XV32566, XV32567, XV32568), 'lo.comp.typecheck^checkType', _):- 'lo.comp.typecheck@checkType'(XV32563, XV32564, XV32565, XV32566, XV32567, XV32568).
'lo.comp.typecheck^typeOfVar'('_call%7'(XV32569, XV32570, XV32571, XV32572, XV32573, XV32574, XV32575), 'lo.comp.typecheck^typeOfVar', _):- 'lo.comp.typecheck@typeOfVar'(XV32569, XV32570, XV32571, XV32572, XV32573, XV32574, XV32575).
'lo.comp.typecheck^findType'('_call%6'(XV32576, XV32577, XV32578, XV32579, XV32580, XV32581), 'lo.comp.typecheck^findType', _):- 'lo.comp.typecheck@findType'(XV32576, XV32577, XV32578, XV32579, XV32580, XV32581).
'lo.comp.typecheck^checkGrammarType'('_call%6'(XV32582, XV32583, XV32584, XV32585, XV32586, XV32587), 'lo.comp.typecheck^checkGrammarType', _):- 'lo.comp.typecheck@checkGrammarType'(XV32582, XV32583, XV32584, XV32585, XV32586, XV32587).
'lo.comp.typecheck^makeIntList'('_call%3'(XV32588, XV32589, XV32590), 'lo.comp.typecheck^makeIntList', _):- 'lo.comp.typecheck@makeIntList'(XV32588, XV32589, XV32590).
'lo.comp.typecheck^explodeStringLit'('_call%3'(XV32591, XV32592, XV32593), 'lo.comp.typecheck^explodeStringLit', _):- 'lo.comp.typecheck@explodeStringLit'(XV32591, XV32592, XV32593).
'lo.comp.typecheck^genTpVars'('_call%2'(XV32594, XV32595), 'lo.comp.typecheck^genTpVars', _):- 'lo.comp.typecheck@genTpVars'(XV32594, XV32595).
'lo.comp.typecheck^macroSequenceTerm'('_call%4'(XV32596, XV32597, XV32598, XV32599), 'lo.comp.typecheck^macroSequenceTerm', _):- 'lo.comp.typecheck@macroSequenceTerm'(XV32596, XV32597, XV32598, XV32599).
'lo.comp.typecheck^isListType'('_call%3'(XV32600, XV32601, XV32602), 'lo.comp.typecheck^isListType', _):- 'lo.comp.typecheck@isListType'(XV32600, XV32601, XV32602).
'lo.comp.typecheck@neg338'(X_35775, X_35774, X_35773, XE):- 'lo.comp.abstract@isBinary'(XE, "->", X_35773, X_35774, X_35775),
    !,
    fail.
'lo.comp.typecheck@neg338'(X_35775, X_35774, X_35773, XE).
'lo.comp.typecheck^isListSequence'('_call%1'(XV32603), 'lo.comp.typecheck^isListSequence', _):- 'lo.comp.typecheck@isListSequence'(XV32603).
'lo.comp.typecheck^macroMapEntries'('_call%5'(XV32604, XV32605, XV32606, XV32607, XV32608), 'lo.comp.typecheck^macroMapEntries', _):- 'lo.comp.typecheck@macroMapEntries'(XV32604, XV32605, XV32606, XV32607, XV32608).
'lo.comp.typecheck^isMapType'('_call%3'(XV32609, XV32610, XV32611), 'lo.comp.typecheck^isMapType', _):- 'lo.comp.typecheck@isMapType'(XV32609, XV32610, XV32611).
'lo.comp.typecheck^isMapSequence'('_call%1'(XV32612), 'lo.comp.typecheck^isMapSequence', _):- 'lo.comp.typecheck@isMapSequence'(XV32612).
'lo.comp.typecheck@one307'(XFields, XTp, XNm):- ocall('in%2'('()2'(XNm, XTp), XFields),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !.
'lo.comp.typecheck^fieldInFace'('_call%7'(XV32613, XV32614, XV32615, XV32616, XV32617, XV32618, XV32619), 'lo.comp.typecheck^fieldInFace', _):- 'lo.comp.typecheck@fieldInFace'(XV32613, XV32614, XV32615, XV32616, XV32617, XV32618, XV32619).
'lo.comp.typecheck^isGrammarRule'('_call%3'(XV32620, XV32621, XV32622), 'lo.comp.typecheck^isGrammarRule', _):- 'lo.comp.typecheck@isGrammarRule'(XV32620, XV32621, XV32622).
'lo.comp.typecheck^collectGrammarRules'('_call%4'(XV32623, XV32624, XV32625, XV32626), 'lo.comp.typecheck^collectGrammarRules', _):- 'lo.comp.typecheck@collectGrammarRules'(XV32623, XV32624, XV32625, XV32626).
'lo.comp.typecheck^isRuleForClass'('_call%4'(XV32627, XV32628, XV32629, XV32630), 'lo.comp.typecheck^isRuleForClass', _):- 'lo.comp.typecheck@isRuleForClass'(XV32627, XV32628, XV32629, XV32630).
'lo.comp.typecheck^collectClassRules'('_call%4'(XV32631, XV32632, XV32633, XV32634), 'lo.comp.typecheck^collectClassRules', _):- 'lo.comp.typecheck@collectClassRules'(XV32631, XV32632, XV32633, XV32634).
'lo.comp.typecheck@neg339'(XXd40555, X_35824, X_35823, XNm, X_35822, XRl):- XRl = 'lo.comp.canon#clause'(X_35822, XNm, X_35823, X_35824),
    !,
    fail.
'lo.comp.typecheck@neg339'(XXd40555, X_35824, X_35823, XNm, X_35822, XRl).
'lo.comp.typecheck^collectMoreClauses'('_call%4'(XV32635, XV32636, XV32637, XV32638), 'lo.comp.typecheck^collectMoreClauses', _):- 'lo.comp.typecheck@collectMoreClauses'(XV32635, XV32636, XV32637, XV32638).
'lo.comp.typecheck^collectClauses'('_call%4'(XV32639, XV32640, XV32641, XV32642), 'lo.comp.typecheck^collectClauses', _):- 'lo.comp.typecheck@collectClauses'(XV32639, XV32640, XV32641, XV32642).
'lo.comp.typecheck@neg340'(XXd40558, X_35843, X_35842, X_35841, XNm, X_35840, XRl):- XRl = 'lo.comp.canon#equation'(X_35840, XNm, X_35841, X_35842, X_35843),
    !,
    fail.
'lo.comp.typecheck@neg340'(XXd40558, X_35843, X_35842, X_35841, XNm, X_35840, XRl).
'lo.comp.typecheck^collectEquations'('_call%4'(XV32643, XV32644, XV32645, XV32646), 'lo.comp.typecheck^collectEquations', _):- 'lo.comp.typecheck@collectEquations'(XV32643, XV32644, XV32645, XV32646).
'lo.comp.typecheck^collectPrograms'('_call%5'(XV32647, XV32648, XV32649, XV32650, XV32651), 'lo.comp.typecheck^collectPrograms', _):- 'lo.comp.typecheck@collectPrograms'(XV32647, XV32648, XV32649, XV32650, XV32651).
'lo.comp.typecheck@cond421'(XXd40576, XXd40575, XXd40574, XPath, XTpCore, XSt, XEx, XRpx, XRp, XLc, XXd40573, XXd40572, XXd40571, XXd40570, XXd40569, XXd40568, XXe5126, XXV5512, XXd40567, XXe5125, XXV5511, XXd40566, X_35871, XOLc, XEnv, XN):- 'lo.comp.dict@isType'(XN, XEnv, XOLc, X_35871),
    !,
    ocall('disp%1'(XXV5511),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('disp%1'(XXV5512),'lo.core$display$lo.comp.location*location','lo.core$display$lo.comp.location*location'),
    ocall('_call%2'(XN, XXe5125),XXV5511,XXV5511),
    ocall('_call%2'(XOLc, XXe5126),XXV5512,XXV5512),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("type "), 'lo.core#,..'(XXe5125, 'lo.core#,..'('lo.core#ss'(" already defined at "), 'lo.core#,..'(XXe5126, 'lo.core#[]'))))), XXd40573),
    'lo.comp.errors@reportError'(XXd40573, XLc, XRp, XRpx),
    XEx = XEnv.
'lo.comp.typecheck@cond421'(XXd40576, XXd40575, XXd40574, XPath, XTpCore, XSt, XEx, XRpx, XRp, XLc, XXd40573, XXd40572, XXd40571, XXd40570, XXd40569, XXd40568, XXe5126, XXV5512, XXd40567, XXe5125, XXV5511, XXd40566, X_35871, XOLc, XEnv, XN):- 'lo.comp.parseType@parseTypeCore'(XSt, XTpCore, XPath),
    XRp = XRpx,
    'lo.comp.dict@declareType'(XN, 'lo.comp.dict#tpDef'(XLc, XTpCore, 'lo.comp.types#faceType'('lo.core#[]')), XEnv, XXd40576),
    XEx = XXd40576.
'lo.comp.typecheck^defineType'('_call%8'(XV32652, XV32653, XV32654, XV32655, XV32656, XV32657, XV32658, XV32659), 'lo.comp.typecheck^defineType', _):- 'lo.comp.typecheck@defineType'(XV32652, XV32653, XV32654, XV32655, XV32656, XV32657, XV32658, XV32659).
'lo.comp.typecheck^defineTypes'('_call%6'(XV32660, XV32661, XV32662, XV32663, XV32664, XV32665), 'lo.comp.typecheck^defineTypes', _):- 'lo.comp.typecheck@defineTypes'(XV32660, XV32661, XV32662, XV32663, XV32664, XV32665).
'lo.comp.typecheck^parseTypeDefinition'('_call%9'(XV32666, XV32667, XV32668, XV32669, XV32670, XV32671, XV32672, XV32673, XV32674), 'lo.comp.typecheck^parseTypeDefinition', _):- 'lo.comp.typecheck@parseTypeDefinition'(XV32666, XV32667, XV32668, XV32669, XV32670, XV32671, XV32672, XV32673, XV32674).
'lo.comp.typecheck^parseTypeDefs'('_call%7'(XV32675, XV32676, XV32677, XV32678, XV32679, XV32680, XV32681), 'lo.comp.typecheck^parseTypeDefs', _):- 'lo.comp.typecheck@parseTypeDefs'(XV32675, XV32676, XV32677, XV32678, XV32679, XV32680, XV32681).
'lo.comp.typecheck^declareTypes'('_call%5'(XV32682, XV32683, XV32684, XV32685, XV32686), 'lo.comp.typecheck^declareTypes', _):- 'lo.comp.typecheck@declareTypes'(XV32682, XV32683, XV32684, XV32685, XV32686).
'lo.comp.typecheck^typeGroup'('_call%8'(XV32687, XV32688, XV32689, XV32690, XV32691, XV32692, XV32693, XV32694), 'lo.comp.typecheck^typeGroup', _):- 'lo.comp.typecheck@typeGroup'(XV32687, XV32688, XV32689, XV32690, XV32691, XV32692, XV32693, XV32694).
'lo.comp.typecheck^checkOther'('_call%6'(XV32695, XV32696, XV32697, XV32698, XV32699, XV32700), 'lo.comp.typecheck^checkOther', _):- 'lo.comp.typecheck@checkOther'(XV32695, XV32696, XV32697, XV32698, XV32699, XV32700).
'lo.comp.typecheck@one308'(XRp0, XRp, XEnv, XMore, XAss, XSt):- 'lo.comp.typecheck@checkOther'(XSt, XAss, XMore, XEnv, XRp, XRp0),
    !.
'lo.comp.typecheck^checkOthers'('_call%6'(XV32701, XV32702, XV32703, XV32704, XV32705, XV32706), 'lo.comp.typecheck^checkOthers', _):- 'lo.comp.typecheck@checkOthers'(XV32701, XV32702, XV32703, XV32704, XV32705, XV32706).
'lo.comp.typecheck^checkClassHead'('_call%9'(XV32707, XV32708, XV32709, XV32710, XV32711, XV32712, XV32713, XV32714, XV32715), 'lo.comp.typecheck^checkClassHead', _):- 'lo.comp.typecheck@checkClassHead'(XV32707, XV32708, XV32709, XV32710, XV32711, XV32712, XV32713, XV32714, XV32715).
'lo.comp.typecheck^checkClassRule'('_call%11'(XV32716, XV32717, XV32718, XV32719, XV32720, XV32721, XV32722, XV32723, XV32724, XV32725, XV32726), 'lo.comp.typecheck^checkClassRule', _):- 'lo.comp.typecheck@checkClassRule'(XV32716, XV32717, XV32718, XV32719, XV32720, XV32721, XV32722, XV32723, XV32724, XV32725, XV32726).
'lo.comp.typecheck^checkDefn'('_call%10'(XV32727, XV32728, XV32729, XV32730, XV32731, XV32732, XV32733, XV32734, XV32735, XV32736), 'lo.comp.typecheck^checkDefn', _):- 'lo.comp.typecheck@checkDefn'(XV32727, XV32728, XV32729, XV32730, XV32731, XV32732, XV32733, XV32734, XV32735, XV32736).
'lo.comp.typecheck@one309'(XRp1, XRp0, XFTp, XLc, XFld, XAT, XFields):- 'lo.comp.typecheck@fieldInFace'(XFields, XAT, XFld, XLc, XFTp, XRp0, XRp1),
    !.
'lo.comp.typecheck^recordAccessExp'('_call%9'(XV32737, XV32738, XV32739, XV32740, XV32741, XV32742, XV32743, XV32744, XV32745), 'lo.comp.typecheck^recordAccessExp', _):- 'lo.comp.typecheck@recordAccessExp'(XV32737, XV32738, XV32739, XV32740, XV32741, XV32742, XV32743, XV32744, XV32745).
'lo.comp.typecheck^typeOfListTerm'('_call%9'(XV32746, XV32747, XV32748, XV32749, XV32750, XV32751, XV32752, XV32753, XV32754), 'lo.comp.typecheck^typeOfListTerm', _):- 'lo.comp.typecheck@typeOfListTerm'(XV32746, XV32747, XV32748, XV32749, XV32750, XV32751, XV32752, XV32753, XV32754).
'lo.comp.typecheck^checkSequenceTerm'('_call%8'(XV32755, XV32756, XV32757, XV32758, XV32759, XV32760, XV32761, XV32762), 'lo.comp.typecheck^checkSequenceTerm', _):- 'lo.comp.typecheck@checkSequenceTerm'(XV32755, XV32756, XV32757, XV32758, XV32759, XV32760, XV32761, XV32762).
'lo.comp.typecheck@or193'(XEnv, XLc, XTp, XEls):- 'lo.comp.typecheck@isMapSequence'(XEls).
'lo.comp.typecheck@or193'(XEnv, XLc, XTp, XEls):- 'lo.comp.typecheck@isMapType'(XTp, XLc, XEnv).
'lo.comp.typecheck@or194'(XEnv, XLc, XTp, XEls):- 'lo.comp.typecheck@isListSequence'(XEls).
'lo.comp.typecheck@or194'(XEnv, XLc, XTp, XEls):- 'lo.comp.typecheck@isListType'(XTp, XLc, XEnv).
'lo.comp.typecheck@cond422'(XRpx, XExp, XEv, XRp2, XXd40619, XXd40618, XXd40617, XRp1, XRp, XListTp, XXd40616, XElTp, XEnv, XLc, XTp, XEls):- 'lo.comp.typecheck@or194'(XEnv, XLc, XTp, XEls),
    !,
    'lo.comp.types@newVar'("El", XXd40616),
    XElTp = XXd40616,
    'lo.comp.typecheck@findType'("list", XLc, XEnv, XRp, XRp1, XXd40617),
    XListTp = 'lo.comp.types#typeExp'(XXd40617, 'lo.core#,..'(XElTp, 'lo.core#[]')),
    'lo.comp.typecheck@checkType'(XLc, XListTp, XTp, XEnv, XRp1, XRp2),
    'lo.comp.typecheck@typeOfListTerm'(XEls, XLc, XElTp, XListTp, XEnv, XEv, XExp, XRp2, XRpx).
'lo.comp.typecheck@cond422'(XRpx, XExp, XEv, XRp2, XXd40619, XXd40618, XXd40617, XRp1, XRp, XListTp, XXd40616, XElTp, XEnv, XLc, XTp, XEls):- 'lo.comp.typecheck@checkSequenceTerm'(XLc, XEls, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck@cond423'(XXd40619, XXd40618, XXd40617, XListTp, XXd40616, XElTp, XRpx, XExp, XEv, XXd40615, XRp3, XRp2, XXd40614, XXd40613, XXd40612, XXd40611, XXd40610, XXd40609, XRp1, XRp, XEnv, XLc, XTp, XEls):- 'lo.comp.typecheck@or193'(XEnv, XLc, XTp, XEls),
    !,
    'lo.comp.typecheck@findType'("map", XLc, XEnv, XRp, XRp1, XXd40609),
    'lo.comp.types@newVar'("K", XXd40610),
    'lo.comp.types@newVar'("V", XXd40611),
    'lo.comp.typecheck@checkType'(XLc, 'lo.comp.types#typeExp'(XXd40609, 'lo.core#,..'(XXd40610, 'lo.core#,..'(XXd40611, 'lo.core#[]'))), XTp, XEnv, XRp1, XRp2),
    'lo.comp.typecheck@macroMapEntries'(XLc, XEls, XRp2, XRp3, XXd40615),
    'lo.comp.typecheck@typeOfTerm'(XXd40615, XTp, XEnv, XEv, XExp, XRp3, XRpx).
'lo.comp.typecheck@cond423'(XXd40619, XXd40618, XXd40617, XListTp, XXd40616, XElTp, XRpx, XExp, XEv, XXd40615, XRp3, XRp2, XXd40614, XXd40613, XXd40612, XXd40611, XXd40610, XXd40609, XRp1, XRp, XEnv, XLc, XTp, XEls):- 'lo.comp.typecheck@cond422'(XRpx, XExp, XEv, XRp2, XXd40619, XXd40618, XXd40617, XRp1, XRp, XListTp, XXd40616, XElTp, XEnv, XLc, XTp, XEls).
'lo.comp.typecheck^checkSquareTuple'('_call%8'(XV32763, XV32764, XV32765, XV32766, XV32767, XV32768, XV32769, XV32770), 'lo.comp.typecheck^checkSquareTuple', _):- 'lo.comp.typecheck@checkSquareTuple'(XV32763, XV32764, XV32765, XV32766, XV32767, XV32768, XV32769, XV32770).
'lo.comp.typecheck^typeOfTerms'('_call%8'(XV32771, XV32772, XV32773, XV32774, XV32775, XV32776, XV32777, XV32778), 'lo.comp.typecheck^typeOfTerms', _):- 'lo.comp.typecheck@typeOfTerms'(XV32771, XV32772, XV32773, XV32774, XV32775, XV32776, XV32777, XV32778).
'lo.comp.typecheck^checkEquation'('_call%10'(XV32779, XV32780, XV32781, XV32782, XV32783, XV32784, XV32785, XV32786, XV32787, XV32788), 'lo.comp.typecheck^checkEquation', _):- 'lo.comp.typecheck@checkEquation'(XV32779, XV32780, XV32781, XV32782, XV32783, XV32784, XV32785, XV32786, XV32787, XV32788).
'lo.comp.typecheck^checkTerminals'('_call%8'(XV32789, XV32790, XV32791, XV32792, XV32793, XV32794, XV32795, XV32796), 'lo.comp.typecheck^checkTerminals', _):- 'lo.comp.typecheck@checkTerminals'(XV32789, XV32790, XV32791, XV32792, XV32793, XV32794, XV32795, XV32796).
'lo.comp.typecheck^checkNonTerminals'('_call%8'(XV32797, XV32798, XV32799, XV32800, XV32801, XV32802, XV32803, XV32804), 'lo.comp.typecheck^checkNonTerminals', _):- 'lo.comp.typecheck@checkNonTerminals'(XV32797, XV32798, XV32799, XV32800, XV32801, XV32802, XV32803, XV32804).
'lo.comp.typecheck^checkConds'('_call%6'(XV32805, XV32806, XV32807, XV32808, XV32809, XV32810), 'lo.comp.typecheck^checkConds', _):- 'lo.comp.typecheck@checkConds'(XV32805, XV32806, XV32807, XV32808, XV32809, XV32810).
'lo.comp.typecheck@one310'(XRpx, XRp, XGrNT, XEx, XEnv, XElTp, XTp, XEls):- 'lo.comp.typecheck@checkNonTerminals'(XEls, XTp, XElTp, XEnv, XEx, XGrNT, XRp, XRpx),
    !.
'lo.comp.typecheck@neg341'(X_35972, X_35971, X_35970, XL):- 'lo.comp.abstract@isBinary'(XL, "?", X_35970, X_35971, X_35972),
    !,
    fail.
'lo.comp.typecheck@neg341'(X_35972, X_35971, X_35970, XL).
'lo.comp.typecheck@cond424'(XXd40673, XXd40672, XXd40671, XXd40670, XXd40669, XXd40668, XXd40667, XXd40666, XXd40665, XXe5135, XXV5528, XXd40664, XXe5134, XXV5527, XGrTp, XXd40663, XXe5133, XXV5526, XXd40662, XXd40661, XOp, XNT, XRpx, XArg, XEv, XE0, XArgTp, XA, XRp1, XRp0, XEnv, XTp, XStrmTp, XLc):- 'lo.comp.typecheck@checkType'(XLc, XStrmTp, XTp, XEnv, XRp0, XRp1),
    !,
    'lo.comp.typecheck@typeOfArg'(XA, XArgTp, XE0, XEv, XArg, XRp1, XRpx),
    XNT = 'lo.comp.canon#grCall'(XLc, XOp, XArg).
'lo.comp.typecheck@cond424'(XXd40673, XXd40672, XXd40671, XXd40670, XXd40669, XXd40668, XXd40667, XXd40666, XXd40665, XXe5135, XXV5528, XXd40664, XXe5134, XXV5527, XGrTp, XXd40663, XXe5133, XXV5526, XXd40662, XXd40661, XOp, XNT, XRpx, XArg, XEv, XE0, XArgTp, XA, XRp1, XRp0, XEnv, XTp, XStrmTp, XLc):- ocall('disp%1'(XXV5526),'lo.core$display$lo.comp.canon*canonTerm','lo.core$display$lo.comp.canon*canonTerm'),
    ocall('disp%1'(XXV5527),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('disp%1'(XXV5528),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('_call%2'(XOp, XXe5133),XXV5526,XXV5526),
    ocall('_call%2'(XGrTp, XXe5134),XXV5527,XXV5527),
    ocall('_call%2'(XTp, XXe5135),XXV5528,XXV5528),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("type of "), 'lo.core#,..'(XXe5133, 'lo.core#,..'('lo.core#ss'(" : "), 'lo.core#,..'(XXe5134, 'lo.core#,..'('lo.core#ss'(" not consistent with stream type "), 'lo.core#,..'(XXe5135, 'lo.core#[]'))))))), XXd40672),
    'lo.comp.errors@reportError'(XXd40672, XLc, XRp0, XRpx),
    XNT = 'lo.comp.canon#grTerms'('lo.core#[]'),
    XEv = XE0.
'lo.comp.typecheck@cond425'(XXd40682, XXd40681, XXd40680, XXd40679, XXd40678, XXd40677, XXd40676, XXd40675, XXe5137, XXV5530, XXd40674, XXe5136, XXV5529, XF, XXd40673, XXd40672, XXd40671, XXd40670, XXd40669, XXd40668, XXd40667, XXd40666, XXd40665, XXe5135, XXV5528, XXd40664, XXe5134, XXV5527, XXd40663, XXe5133, XXV5526, XXd40662, XXd40661, XOp, XNT, XRpx, XArg, XEv, XE0, XA, XRp1, XRp0, XEnv, XTp, XLc, XXd40660, XStrmTp, XArgTp, XXd40659, XGrTp):- 'lo.comp.types@deRef'(XGrTp, XXd40659),
    XXd40659 = 'lo.comp.types#grammarType'(XArgTp, XStrmTp),
    !,
    'lo.comp.typecheck@cond424'(XXd40673, XXd40672, XXd40671, XXd40670, XXd40669, XXd40668, XXd40667, XXd40666, XXd40665, XXe5135, XXV5528, XXd40664, XXe5134, XXV5527, XGrTp, XXd40663, XXe5133, XXV5526, XXd40662, XXd40661, XOp, XNT, XRpx, XArg, XEv, XE0, XArgTp, XA, XRp1, XRp0, XEnv, XTp, XStrmTp, XLc).
'lo.comp.typecheck@cond425'(XXd40682, XXd40681, XXd40680, XXd40679, XXd40678, XXd40677, XXd40676, XXd40675, XXe5137, XXV5530, XXd40674, XXe5136, XXV5529, XF, XXd40673, XXd40672, XXd40671, XXd40670, XXd40669, XXd40668, XXd40667, XXd40666, XXd40665, XXe5135, XXV5528, XXd40664, XXe5134, XXV5527, XXd40663, XXe5133, XXV5526, XXd40662, XXd40661, XOp, XNT, XRpx, XArg, XEv, XE0, XA, XRp1, XRp0, XEnv, XTp, XLc, XXd40660, XStrmTp, XArgTp, XXd40659, XGrTp):- ocall('disp%1'(XXV5529),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('disp%1'(XXV5530),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('_call%2'(XF, XXe5136),XXV5529,XXV5529),
    ocall('_call%2'(XGrTp, XXe5137),XXV5530,XXV5530),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XXe5136, 'lo.core#,..'('lo.core#ss'(" : "), 'lo.core#,..'(XXe5137, 'lo.core#,..'('lo.core#ss'(" not a grammar"), 'lo.core#[]'))))), XXd40681),
    'lo.comp.errors@reportError'(XXd40681, XLc, XRp0, XRpx),
    XNT = 'lo.comp.canon#grTerms'('lo.core#[]'),
    XEv = XEnv.
'lo.comp.typecheck^checkNonTerminal'('_call%8'(XV32811, XV32812, XV32813, XV32814, XV32815, XV32816, XV32817, XV32818), 'lo.comp.typecheck^checkNonTerminal', _):- 'lo.comp.typecheck@checkNonTerminal'(XV32811, XV32812, XV32813, XV32814, XV32815, XV32816, XV32817, XV32818).
'lo.comp.typecheck@cond426'(XXd40714, XXd40713, XXd40712, XXd40711, XXd40710, XXd40709, XXd40708, XXe5139, XXV5532, XXd40707, XXe5138, XXV5531, XXd40706, XPred, XLc, XCall, XRpx, XRp0, XArg, XEv, XE0, XA, XXd40705, XArgTp, XXd40704, XPrTp):- 'lo.comp.types@deRef'(XPrTp, XXd40704),
    XXd40704 = 'lo.comp.types#predType'(XArgTp),
    !,
    'lo.comp.typecheck@typeOfArg'(XA, XArgTp, XE0, XEv, XArg, XRp0, XRpx),
    XCall = 'lo.comp.canon#callCond'(XLc, XPred, XArg).
'lo.comp.typecheck@cond426'(XXd40714, XXd40713, XXd40712, XXd40711, XXd40710, XXd40709, XXd40708, XXe5139, XXV5532, XXd40707, XXe5138, XXV5531, XXd40706, XPred, XLc, XCall, XRpx, XRp0, XArg, XEv, XE0, XA, XXd40705, XArgTp, XXd40704, XPrTp):- ocall('disp%1'(XXV5531),'lo.core$display$lo.comp.canon*canonTerm','lo.core$display$lo.comp.canon*canonTerm'),
    ocall('disp%1'(XXV5532),'lo.core$display$lo.comp.types*tipe','lo.core$display$lo.comp.types*tipe'),
    ocall('_call%2'(XPred, XXe5138),XXV5531,XXV5531),
    ocall('_call%2'(XPrTp, XXe5139),XXV5532,XXV5532),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XXe5138, 'lo.core#,..'('lo.core#ss'(":"), 'lo.core#,..'(XXe5139, 'lo.core#,..'('lo.core#ss'(" not a predicate"), 'lo.core#[]'))))), XXd40714),
    'lo.comp.errors@reportError'(XXd40714, XLc, XRp0, XRpx),
    XE0 = XEv,
    XCall = 'lo.comp.canon#trueCond'.
'lo.comp.typecheck^checkCond'('_call%6'(XV32819, XV32820, XV32821, XV32822, XV32823, XV32824), 'lo.comp.typecheck^checkCond', _):- 'lo.comp.typecheck@checkCond'(XV32819, XV32820, XV32821, XV32822, XV32823, XV32824).
'lo.comp.typecheck^checkClause'('_call%9'(XV32825, XV32826, XV32827, XV32828, XV32829, XV32830, XV32831, XV32832, XV32833), 'lo.comp.typecheck^checkClause', _):- 'lo.comp.typecheck@checkClause'(XV32825, XV32826, XV32827, XV32828, XV32829, XV32830, XV32831, XV32832, XV32833).
'lo.comp.typecheck^typeOfCall'('_call%10'(XV32834, XV32835, XV32836, XV32837, XV32838, XV32839, XV32840, XV32841, XV32842, XV32843), 'lo.comp.typecheck^typeOfCall', _):- 'lo.comp.typecheck@typeOfCall'(XV32834, XV32835, XV32836, XV32837, XV32838, XV32839, XV32840, XV32841, XV32842, XV32843).
'lo.comp.typecheck@cond427'(XXd40732, XXd40731, XXd40730, XXd40729, XXd40728, XXd40727, XXd40726, XXe5142, XXV5535, XV, XXd40725, XEv, XRpx, XRp, XTerm, XTp, XLc, XSpec, XEnv, XNm):- 'lo.comp.dict@isVar'(XNm, XEnv, XSpec),
    !,
    'lo.comp.typecheck@typeOfVar'(XLc, XSpec, XTp, XEnv, XTerm, XRp, XRpx),
    XEnv = XEv.
'lo.comp.typecheck@cond427'(XXd40732, XXd40731, XXd40730, XXd40729, XXd40728, XXd40727, XXd40726, XXe5142, XXV5535, XV, XXd40725, XEv, XRpx, XRp, XTerm, XTp, XLc, XSpec, XEnv, XNm):- ocall('disp%1'(XXV5535),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('_call%2'(XV, XXe5142),XXV5535,XXV5535),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("variable "), 'lo.core#,..'(XXe5142, 'lo.core#,..'('lo.core#ss'(" not declared"), 'lo.core#[]')))), XXd40731),
    'lo.comp.errors@reportError'(XXd40731, XLc, XRp, XRpx),
    XTerm = 'lo.comp.canon#v'(XLc, XNm).
'lo.comp.typecheck@cond428'(XEx, XXd40732, XXd40731, XXd40730, XXd40729, XXd40728, XXd40727, XXd40726, XXe5142, XXV5535, XXd40725, XEv, XRpx, XRp, XTerm, XTp, XSpec, XEnv, XNm, XLc, XV):- 'lo.comp.abstract@isIden'(XV, XLc, XNm),
    !,
    'lo.comp.typecheck@cond427'(XXd40732, XXd40731, XXd40730, XXd40729, XXd40728, XXd40727, XXd40726, XXe5142, XXV5535, XV, XXd40725, XEv, XRpx, XRp, XTerm, XTp, XLc, XSpec, XEnv, XNm).
'lo.comp.typecheck@cond428'(XEx, XXd40732, XXd40731, XXd40730, XXd40729, XXd40728, XXd40727, XXd40726, XXe5142, XXV5535, XXd40725, XEv, XRpx, XRp, XTerm, XTp, XSpec, XEnv, XNm, XLc, XV):- 'lo.comp.typecheck@typeOfTerm'(XV, XTp, XEnv, XEx, XTerm, XRp, XRpx).
'lo.comp.typecheck^knownType'('_call%7'(XV32844, XV32845, XV32846, XV32847, XV32848, XV32849, XV32850), 'lo.comp.typecheck^knownType', _):- 'lo.comp.typecheck@knownType'(XV32844, XV32845, XV32846, XV32847, XV32848, XV32849, XV32850).
'lo.comp.typecheck^typeOfIndex'('_call%9'(XV32851, XV32852, XV32853, XV32854, XV32855, XV32856, XV32857, XV32858, XV32859), 'lo.comp.typecheck^typeOfIndex', _):- 'lo.comp.typecheck@typeOfIndex'(XV32851, XV32852, XV32853, XV32854, XV32855, XV32856, XV32857, XV32858, XV32859).
'lo.comp.typecheck@cond429'(XXd40745, XXd40744, XEnv, XXd40743, XTp, XXd40742, XEv, XXd40741, XXc537, XTerm, XRpx, XRp, XLc, XXd40740, XXd40739, XXd40738, XXd40737, XXe5143, XXV5536, XXd40736, XNm):- 'lo.comp.keywords@isKeyword'(XNm),
    !,
    ocall('disp%1'(XXV5536),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('_call%2'(XNm, XXe5143),XXV5536,XXV5536),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("unexpected keyword: "), 'lo.core#,..'(XXe5143, 'lo.core#[]'))), XXd40740),
    'lo.comp.errors@reportError'(XXd40740, XLc, XRp, XRpx),
    '_str_gen'("_", XXc537),
    XTerm = 'lo.comp.canon#v'(XLc, XXc537).
'lo.comp.typecheck@cond429'(XXd40745, XXd40744, XEnv, XXd40743, XTp, XXd40742, XEv, XXd40741, XXc537, XTerm, XRpx, XRp, XLc, XXd40740, XXd40739, XXd40738, XXd40737, XXe5143, XXV5536, XXd40736, XNm):- 'lo.comp.dict@declareVar'(XNm, 'lo.comp.dict#vr'('lo.comp.canon#v'(XLc, XNm), XTp), XEnv, XXd40744),
    XEv = XXd40744,
    XTerm = 'lo.comp.canon#v'(XLc, XNm),
    XRp = XRpx.
'lo.comp.typecheck@cond430'(XXd40745, XXd40744, XXd40743, XXd40742, XXd40741, XXc537, XXd40740, XXd40739, XXd40738, XXd40737, XXe5143, XXV5536, XXd40736, XEv, XRpx, XRp, XTerm, XTp, XLc, XSpec, XEnv, XNm):- 'lo.comp.dict@isVar'(XNm, XEnv, XSpec),
    !,
    'lo.comp.typecheck@typeOfVar'(XLc, XSpec, XTp, XEnv, XTerm, XRp, XRpx),
    XEnv = XEv.
'lo.comp.typecheck@cond430'(XXd40745, XXd40744, XXd40743, XXd40742, XXd40741, XXc537, XXd40740, XXd40739, XXd40738, XXd40737, XXe5143, XXV5536, XXd40736, XEv, XRpx, XRp, XTerm, XTp, XLc, XSpec, XEnv, XNm):- 'lo.comp.typecheck@cond429'(XXd40745, XXd40744, XEnv, XXd40743, XTp, XXd40742, XEv, XXd40741, XXc537, XTerm, XRpx, XRp, XLc, XXd40740, XXd40739, XXd40738, XXd40737, XXe5143, XXV5536, XXd40736, XNm).
'lo.comp.typecheck@cond431'(XXd40753, XXd40752, XXd40751, XXd40750, XXe5144, XXV5537, XXd40749, XRpx, XExp, XEv, XL, XRp1, XTp, XLc, XRp0, XRp, XRT, XEnv, XR):- 'lo.comp.parseType@parseType'(XR, XEnv, XRT, XRp, XRp0),
    !,
    'lo.comp.typecheck@checkType'(XLc, XRT, XTp, XEnv, XRp0, XRp1),
    'lo.comp.typecheck@typeOfTerm'(XL, XRT, XEnv, XEv, XExp, XRp1, XRpx).
'lo.comp.typecheck@cond431'(XXd40753, XXd40752, XXd40751, XXd40750, XXe5144, XXV5537, XXd40749, XRpx, XExp, XEv, XL, XRp1, XTp, XLc, XRp0, XRp, XRT, XEnv, XR):- ocall('disp%1'(XXV5537),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('_call%2'(XR, XXe5144),XXV5537,XXV5537),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot parse type "), 'lo.core#,..'(XXe5144, 'lo.core#[]'))), XXd40753),
    'lo.comp.errors@reportError'(XXd40753, XLc, XRp0, XRp1),
    'lo.comp.typecheck@typeOfTerm'(XL, XTp, XEnv, XEv, XExp, XRp1, XRpx).
'lo.comp.typecheck@neg342'(X_36037, X_36036, XInner):- 'lo.comp.abstract@isRoundTuple'(XInner, X_36036, X_36037),
    !,
    fail.
'lo.comp.typecheck@neg342'(X_36037, X_36036, XInner).
'lo.comp.typecheck^typeOfTerm'('_call%7'(XV32860, XV32861, XV32862, XV32863, XV32864, XV32865, XV32866), 'lo.comp.typecheck^typeOfTerm', _):- 'lo.comp.typecheck@typeOfTerm'(XV32860, XV32861, XV32862, XV32863, XV32864, XV32865, XV32866).
'lo.comp.typecheck@cond432'(XXd40796, XExp, XRpx, XEls, XEv, XRp0, XRp, XEnv, XTp, XXd40795, XXd40794, XArgTps, XA, XLc, XTerm):- 'lo.comp.abstract@isRoundTuple'(XTerm, XLc, XA),
    !,
    'lo.comp.typecheck@genTpVars'(XA, XXd40794),
    XArgTps = XXd40794,
    'lo.comp.typecheck@checkType'(XLc, 'lo.comp.types#tupleType'(XArgTps), XTp, XEnv, XRp, XRp0),
    'lo.comp.typecheck@typeOfTerms'(XA, XArgTps, XEnv, XEv, XLc, XEls, XRp0, XRpx),
    XExp = 'lo.comp.canon#tpl'(XEls).
'lo.comp.typecheck@cond432'(XXd40796, XExp, XRpx, XEls, XEv, XRp0, XRp, XEnv, XTp, XXd40795, XXd40794, XArgTps, XA, XLc, XTerm):- 'lo.comp.typecheck@typeOfTerm'(XTerm, XTp, XEnv, XEv, XExp, XRp, XRpx).
'lo.comp.typecheck^typeOfArg'('_call%7'(XV32867, XV32868, XV32869, XV32870, XV32871, XV32872, XV32873), 'lo.comp.typecheck^typeOfArg', _):- 'lo.comp.typecheck@typeOfArg'(XV32867, XV32868, XV32869, XV32870, XV32871, XV32872, XV32873).
'lo.comp.typecheck^checkGrammarRule'('_call%9'(XV32874, XV32875, XV32876, XV32877, XV32878, XV32879, XV32880, XV32881, XV32882), 'lo.comp.typecheck^checkGrammarRule', _):- 'lo.comp.typecheck@checkGrammarRule'(XV32874, XV32875, XV32876, XV32877, XV32878, XV32879, XV32880, XV32881, XV32882).
'lo.comp.typecheck^processStmt'('_call%8'(XV32883, XV32884, XV32885, XV32886, XV32887, XV32888, XV32889, XV32890), 'lo.comp.typecheck^processStmt', _):- 'lo.comp.typecheck@processStmt'(XV32883, XV32884, XV32885, XV32886, XV32887, XV32888, XV32889, XV32890).
'lo.comp.typecheck@one311'(XRp0, XRp, XPath, XEnv, XD0, XDefs, XProgramType, XSt):- 'lo.comp.typecheck@processStmt'(XSt, XProgramType, XDefs, XD0, XEnv, XPath, XRp, XRp0),
    !.
'lo.comp.typecheck^processStmts'('_call%8'(XV32891, XV32892, XV32893, XV32894, XV32895, XV32896, XV32897, XV32898), 'lo.comp.typecheck^processStmts', _):- 'lo.comp.typecheck@processStmts'(XV32891, XV32892, XV32893, XV32894, XV32895, XV32896, XV32897, XV32898).
'lo.comp.typecheck^checkVarRules'('_call%7'(XV32899, XV32900, XV32901, XV32902, XV32903, XV32904, XV32905), 'lo.comp.typecheck^checkVarRules', _):- 'lo.comp.typecheck@checkVarRules'(XV32899, XV32900, XV32901, XV32902, XV32903, XV32904, XV32905).
'lo.comp.typecheck@one312'(XRp0, XRp, XPath, XEnv, XBase, XAnnots, XFields, XGrp):- 'lo.comp.typecheck@parseAnnotations'(XGrp, XFields, XAnnots, XBase, XEnv, XPath, XRp, XRp0),
    !.
'lo.comp.typecheck^varGroup'('_call%10'(XV32906, XV32907, XV32908, XV32909, XV32910, XV32911, XV32912, XV32913, XV32914, XV32915), 'lo.comp.typecheck^varGroup', _):- 'lo.comp.typecheck@varGroup'(XV32906, XV32907, XV32908, XV32909, XV32910, XV32911, XV32912, XV32913, XV32914, XV32915).
'lo.comp.typecheck^checkThetaTerm'('_call%10'(XV32916, XV32917, XV32918, XV32919, XV32920, XV32921, XV32922, XV32923, XV32924, XV32925), 'lo.comp.typecheck^checkThetaTerm', _):- 'lo.comp.typecheck@checkThetaTerm'(XV32916, XV32917, XV32918, XV32919, XV32920, XV32921, XV32922, XV32923, XV32924, XV32925).
'lo.comp.typecheck^implementationGroup'('_call%8'(XV32926, XV32927, XV32928, XV32929, XV32930, XV32931, XV32932, XV32933), 'lo.comp.typecheck^implementationGroup', _):- 'lo.comp.typecheck@implementationGroup'(XV32926, XV32927, XV32928, XV32929, XV32930, XV32931, XV32932, XV32933).
'lo.comp.typecheck^checkGroup'('_call%11'(XV32934, XV32935, XV32936, XV32937, XV32938, XV32939, XV32940, XV32941, XV32942, XV32943, XV32944), 'lo.comp.typecheck^checkGroup', _):- 'lo.comp.typecheck@checkGroup'(XV32934, XV32935, XV32936, XV32937, XV32938, XV32939, XV32940, XV32941, XV32942, XV32943, XV32944).
'lo.comp.typecheck^checkGroups'('_call%9'(XV32945, XV32946, XV32947, XV32948, XV32949, XV32950, XV32951, XV32952, XV32953), 'lo.comp.typecheck^checkGroups', _):- 'lo.comp.typecheck@checkGroups'(XV32945, XV32946, XV32947, XV32948, XV32949, XV32950, XV32951, XV32952, XV32953).
'lo.comp.typecheck^thetaEnv'('_call%15'(XV32954, XV32955, XV32956, XV32957, XV32958, XV32959, XV32960, XV32961, XV32962, XV32963, XV32964, XV32965, XV32966, XV32967, XV32968), 'lo.comp.typecheck^thetaEnv', _):- 'lo.comp.typecheck@thetaEnv'(XV32954, XV32955, XV32956, XV32957, XV32958, XV32959, XV32960, XV32961, XV32962, XV32963, XV32964, XV32965, XV32966, XV32967, XV32968).
'lo.comp.typecheck^findImportedImplementations'('_call%3'(XV32969, XV32970, XV32971), 'lo.comp.typecheck^findImportedImplementations', _):- 'lo.comp.typecheck@findImportedImplementations'(XV32969, XV32970, XV32971).
'lo.comp.typecheck@cond433'(XXd40854, XRpx, XOthers, XOOthers, XRp1, XODefs, XODict, XDefs, XOverDict, XImportClosure, XRp0):- 'lo.comp.errors@errorFree'(XRp0),
    !,
    'lo.comp.typecheck@findImportedImplementations'(XImportClosure, 'lo.core#[]', XOverDict),
    'lo.comp.resolve@overload'(XDefs, XOverDict, XODict, XODefs, XRp0, XRp1),
    'lo.comp.resolve@overloadOthers'(XOthers, XODict, XRp1, XRpx, XXd40854),
    XOOthers = XXd40854.
'lo.comp.typecheck@cond433'(XXd40854, XRpx, XOthers, XOOthers, XRp1, XODefs, XODict, XDefs, XOverDict, XImportClosure, XRp0):- XRpx = XRp0,
    XODefs = XDefs,
    XOOthers = XOthers.
'lo.comp.typecheck^checkProgram'('_call%7'(XV32972, XV32973, XV32974, XV32975, XV32976, XV32977, XV32978), 'lo.comp.typecheck^checkProgram', _):- 'lo.comp.typecheck@checkProgram'(XV32972, XV32973, XV32974, XV32975, XV32976, XV32977, XV32978).
'lo.comp.typecheck^exportViz'('_call%4'(XV32979, XV32980, XV32981, XV32982), 'lo.comp.typecheck^exportViz', _):- 'lo.comp.typecheck@exportViz'(XV32979, XV32980, XV32981, XV32982).
'lo.comp.typecheck^computeImports'('_call%3'(XV32983, XV32984, XV32985), 'lo.comp.typecheck^computeImports', _):- 'lo.comp.typecheck@computeImports'(XV32983, XV32984, XV32985).
'lo.comp.typecheck^addPublicImports'('_call%3'(XV32986, XV32987, XV32988), 'lo.comp.typecheck^addPublicImports', _):- 'lo.comp.typecheck@addPublicImports'(XV32986, XV32987, XV32988).
'lo.comp.typecheck^mkTypes'('_call%2'(XV32989, XV32990), 'lo.comp.typecheck^mkTypes', _):- 'lo.comp.typecheck@mkTypes'(XV32989, XV32990).
'lo.comp.typecheck^pickVar'('_call%2'(XV32991, XV32992), 'lo.comp.typecheck^pickVar', _):- 'lo.comp.typecheck@pickVar'(XV32991, XV32992).
'lo.comp.typecheck^computeNewVars'('_call%2'(XV32993, XV32994), 'lo.comp.typecheck^computeNewVars', _):- 'lo.comp.typecheck@computeNewVars'(XV32993, XV32994).
