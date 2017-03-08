'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.imports's'0.0.1'n17o17'()17'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'n2o2'import'e'private'n2o2'pkg's'lo.resources'e'*'n2o2'import'e'private'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.dict'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.misc'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.decode'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.token'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.lexer'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.grammar'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.ast'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.abstract'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.canon'e'*'s\"I4'importPkg':k'r'|PT5t'lo.repo*pkg'k'r't'lo.comp.package*pkgSpec't'lo.comp.errors*report't'lo.comp.errors*report'c'lo.repo$repository'T1k'r'T0'consistentPkg'PT2t'lo.repo*pkg't'lo.repo*pkg''importPkgs':k'r'|FT5Lt'lo.repo*pkg'Lt'lo.comp.package*pkgSpec'k'r't'lo.comp.errors*report't'lo.comp.errors*report'Lt'lo.comp.package*pkgSpec'c'lo.repo$repository'T1k'r'T0'importClosure':k'r'|FT5Lt'lo.repo*pkg'Lt'lo.comp.package*pkgSpec'k'r't'lo.comp.errors*report't'lo.comp.errors*report'Lt'lo.comp.package*pkgSpec'c'lo.repo$repository'T1k'r'T0\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.imports@init'():- !.
'lo.comp.imports@decodeVers'('lo.comp.term#enum'("*"), 'lo.repo#defltVersion'):- !.
'lo.comp.imports@decodeVers'('lo.comp.term#strng'(XV), 'lo.repo#vers'(XV)):- !.
'lo.comp.imports@decodeVers'(_, _):- raise_exception('error'("lo.comp.imports@decodeVers", 83, 3, 35)).
'lo.comp.imports@decodePkg'('lo.comp.term#cons'('lo.comp.term#strct'("pkg", 2), 'lo.core#,..'('lo.comp.term#strng'(XPk), 'lo.core#,..'(XV, 'lo.core#[]'))), 'lo.repo#pkg'(XPk, XXd39173)):- !,
    'lo.comp.imports@decodeVers'(XV, XXd39173).
'lo.comp.imports@decodePkg'(_, _):- raise_exception('error'("lo.comp.imports@decodePkg", 80, 3, 69)).
'lo.comp.imports@decodeViz'('lo.comp.term#enum'("public"), 'lo.comp.package#pUblic'):- !.
'lo.comp.imports@decodeViz'('lo.comp.term#enum'("private"), 'lo.comp.package#priVate'):- !.
'lo.comp.imports@decodeViz'(_, _):- raise_exception('error'("lo.comp.imports@decodeViz", 76, 3, 35)).
'lo.comp.imports@decodeImports'('lo.core#[]', 'lo.core#[]'):- !.
'lo.comp.imports@decodeImports'('lo.core#,..'('lo.comp.term#cons'('lo.comp.term#strct'("import", 2), 'lo.core#,..'(XVz, 'lo.core#,..'(XPk, 'lo.core#[]'))), XIs), 'lo.core#,..'('()2'(XXd39175, XXd39176), XXd39177)):- !,
    'lo.comp.imports@decodeViz'(XVz, XXd39175),
    'lo.comp.imports@decodePkg'(XPk, XXd39176),
    'lo.comp.imports@decodeImports'(XIs, XXd39177).
'lo.comp.imports@decodeImports'(_, _):- raise_exception('error'("lo.comp.imports@decodeImports", 69, 3, 23)).
'lo.comp.imports@decodeSignature'('lo.comp.term#strng'(XS), XT):- 'explode'(XS, XXc526),
    'lo.comp.decode@decodeType'(XXc526, XStx1821, XT),
    XStx1821 = X_33733,
    ocall('_eof%1'(X_33733),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    X_33732 = XStx1821,
    !.
'lo.comp.imports@decodeSignature'(_, _):- raise_exception('error'("lo.comp.imports@decodeSignature", 104, 3, 61)).
'lo.comp.imports@pickupEnums'('lo.core#[]', XCls, XCls).
'lo.comp.imports@pickupEnums'('lo.core#,..'('lo.comp.term#strng'(XNm), XRest), 'lo.core#,..'(XNm, XMore), XCls):- 'lo.comp.imports@pickupEnums'(XRest, XMore, XCls).
'lo.comp.imports@findContracts'('lo.core#[]', XC, XC).
'lo.comp.imports@findContracts'('lo.core#,..'('lo.comp.term#cons'(X_33737, 'lo.core#,..'('lo.comp.term#strng'(XNm), 'lo.core#,..'('lo.comp.term#strng'(XConNm), 'lo.core#,..'('lo.comp.term#strng'(XConSig), 'lo.core#,..'(XFaceSig, 'lo.core#[]'))))), XM), 'lo.core#,..'('lo.comp.types#conEntry'(XNm, XConNm, XCon, XXb19122), XC), XCx):- 'lo.comp.decode@decodeConstraint'(XConSig, XCon),
    'lo.comp.imports@findContracts'(XM, XC, XCx),
    'lo.comp.imports@decodeSignature'(XFaceSig, XXb19122).
'lo.comp.imports@pickupImplementations'('lo.core#[]', XI, XI).
'lo.comp.imports@pickupImplementations'('lo.core#,..'('lo.comp.term#cons'(X_33744, 'lo.core#,..'('lo.comp.term#strng'(XNm), 'lo.core#,..'('lo.comp.term#strng'(XSig), 'lo.core#[]'))), XM), 'lo.core#,..'('lo.comp.types#implEntry'(XNm, XSpec), XI), XRI):- 'lo.comp.decode@decodeConstraint'(XSig, XSpec),
    'lo.comp.imports@pickupImplementations'(XM, XI, XRI).
'lo.comp.imports@pickupPkgSpec'(XTerm, XXb19133, XXb19134, XFields, XTypes, XEnums, XContracts, XImpls, XRp, XRp):- 'lo.comp.abstract@isUnary'(XTerm, "#pkg", X_33748, XE),
    'lo.comp.abstract@isString'(XE, X_33749, XEnc),
    'lo.comp.decode@decodeValue'(XEnc, XXd39191),
    'lo.comp.term#cons'(X_33750, 'lo.core#,..'(XPk, 'lo.core#,..'('lo.comp.term#cons'(X_33753, XIs), 'lo.core#,..'(XEx, 'lo.core#,..'(XTp, 'lo.core#,..'('lo.comp.term#cons'(X_33757, XC), 'lo.core#,..'('lo.comp.term#cons'(X_33759, XCo), 'lo.core#,..'('lo.comp.term#cons'(X_33761, XIm), 'lo.core#[]')))))))) = XXd39191,
    'lo.comp.imports@decodeSignature'(XEx, XXd39193),
    'lo.comp.types#faceType'(XFields) = XXd39193,
    'lo.comp.imports@decodeSignature'(XTp, XXd39195),
    'lo.comp.types#faceType'(XTypes) = XXd39195,
    'lo.comp.imports@pickupEnums'(XC, XEnums, 'lo.core#[]'),
    'lo.comp.imports@findContracts'(XCo, XContracts, 'lo.core#[]'),
    'lo.comp.imports@pickupImplementations'(XIm, XImpls, 'lo.core#[]'),
    'lo.comp.imports@decodePkg'(XPk, XXb19133),
    'lo.comp.imports@decodeImports'(XIs, XXb19134).
'lo.comp.imports@importPkg'(Xrepository61, 'lo.repo#pkg'(XPkg, XVers), XRepo, 'lo.comp.package#pkgSpec'(XIPkg, XExport, XTypes, XEnums, XContracts, XImpls, XImports), XRp, XRpx):- ocall('loadFromRepo%4'(XRepo, 'lo.repo#pkg'(XPkg, XVers), "prolog", XStrm),Xrepository61,Xrepository61),
    'lo.comp.imports@one280'(X_33762, X_33763, XStx1822, XRp0, XRp, XA, XXd39197, XPkg, XXc527, XStrm),
    'lo.comp.imports@one281'(XRpx, XRp0, XImpls, XContracts, XEnums, XTypes, XExport, XImports, XIPkg, XA).
'lo.comp.imports@consistentVersion'('lo.repo#defltVersion', X_33764).
'lo.comp.imports@consistentVersion'('lo.repo#vers'(XV), 'lo.repo#vers'(XV)).
'lo.comp.imports@consistentPkg'('lo.repo#pkg'(XP, XV1), 'lo.repo#pkg'(XP, XV2)):- 'lo.comp.imports@one282'(XV2, XV1).
'lo.comp.imports@alreadyImported'(XPkg, XSpecs):- ocall('in%2'('lo.comp.package#pkgSpec'(XPk, X_33765, X_33766, X_33767, X_33768, X_33769, X_33770), XSpecs),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.imports@consistentPkg'(XPkg, XPk).
'lo.comp.imports@importPkgs'(Xrepository62, 'lo.core#[]', XSpecs, X_33771, XRp, XRp, XSpecs):- !.
'lo.comp.imports@importPkgs'(Xrepository62, 'lo.core#,..'(XPkg, XL), XSoFar, XRepo, XRp, XRpx, XXd39199):- 'lo.comp.imports@alreadyImported'(XPkg, XSoFar),
    !,
    'lo.comp.imports@importPkgs'(Xrepository62, XL, XSoFar, XRepo, XRp, XRpx, XXd39199).
'lo.comp.imports@importPkgs'(Xrepository62, 'lo.core#,..'(XPkg, XL), XSoFar, XRepo, XRp, XRpx, XXd39201):- 'lo.comp.imports@importPkg'(Xrepository62, XPkg, XRepo, XSpec, XRp, XRp0),
    !,
    'lo.comp.imports@importPkgs'(Xrepository62, XL, 'lo.core#,..'(XSpec, XSoFar), XRepo, XRp0, XRpx, XXd39201).
'lo.comp.imports@importPkgs'(_, _, _, _, _, _, _):- raise_exception('error'("lo.comp.imports@importPkgs", 39, 3, 37)).
'lo.comp.imports@importsOfSpec'('lo.comp.package#pkgSpec'(X_33775, X_33776, X_33777, X_33778, X_33779, X_33780, XImps), XImps):- !.
'lo.comp.imports@importsOfSpec'(_, _):- raise_exception('error'("lo.comp.imports@importsOfSpec", 61, 3, 48)).
'lo.comp.imports@publicImports'('lo.core#[]', 'lo.core#[]'):- !.
'lo.comp.imports@publicImports'('lo.core#,..'('()2'('lo.comp.package#pUblic', XPkg), XL), 'lo.core#,..'(XPkg, XXd39202)):- !,
    'lo.comp.imports@publicImports'(XL, XXd39202).
'lo.comp.imports@publicImports'('lo.core#,..'('()2'('lo.comp.package#priVate', X_33784), XL), XXd39204):- !,
    'lo.comp.imports@publicImports'(XL, XXd39204).
'lo.comp.imports@publicImports'(_, _):- raise_exception('error'("lo.comp.imports@publicImports", 64, 3, 23)).
'lo.comp.imports@importClosure'(Xrepository63, 'lo.core#[]', XSpecs, X_33785, XRp, XRp, XSpecs):- !.
'lo.comp.imports@importClosure'(Xrepository63, 'lo.core#,..'(XPkg, XL), XSoFar, XRepo, XRp, XRpx, XXd39205):- 'lo.comp.imports@alreadyImported'(XPkg, XSoFar),
    !,
    'lo.comp.imports@importClosure'(Xrepository63, XL, XSoFar, XRepo, XRp, XRpx, XXd39205).
'lo.comp.imports@importClosure'(Xrepository63, 'lo.core#,..'(XPkg, XL), XSoFar, XRepo, XRp, XRpx, XXd39210):- 'lo.comp.imports@importPkg'(Xrepository63, XPkg, XRepo, XSpec, XRp, XRp0),
    !,
    'lo.comp.imports@importsOfSpec'(XSpec, XXd39206),
    'lo.comp.imports@publicImports'(XXd39206, XXd39207),
    'lo.comp.imports@importClosure'(Xrepository63, XXd39207, 'lo.core#,..'(XSpec, XSoFar), XRepo, XRp0, XRp1, XXd39209),
    'lo.comp.imports@importClosure'(Xrepository63, XL, XXd39209, XRepo, XRp1, XRpx, XXd39210).
'lo.comp.imports@importClosure'(_, _, _, _, _, _, _):- raise_exception('error'("lo.comp.imports@importClosure", 45, 3, 40)).
'lo.comp.imports@decodeTypes'(XTxt, XF):- 'explode'(XTxt, XXc528),
    'lo.comp.decode@decodeType'(XXc528, XStx1823, 'lo.comp.types#faceType'(XF)),
    XStx1823 = X_33790,
    ocall('_eof%1'(X_33790),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    X_33789 = XStx1823,
    !.
'lo.comp.imports@decodeTypes'(_, _):- raise_exception('error'("lo.comp.imports@decodeTypes", 73, 3, 64)).
'lo.comp.imports@splitOnHash'(XS, XXd39212):- !,
    'lo.comp.misc@pathSuffix'(XS, "#", XXd39212).
'lo.comp.imports@splitOnHash'(_, _):- raise_exception('error'("lo.comp.imports@splitOnHash", 107, 3, 35)).
'lo.comp.imports^decodeVers'('_call%2'(XV30506, XV30507), 'lo.comp.imports^decodeVers', _):- 'lo.comp.imports@decodeVers'(XV30506, XV30507).
'lo.comp.imports^decodePkg'('_call%2'(XV30508, XV30509), 'lo.comp.imports^decodePkg', _):- 'lo.comp.imports@decodePkg'(XV30508, XV30509).
'lo.comp.imports^decodeViz'('_call%2'(XV30510, XV30511), 'lo.comp.imports^decodeViz', _):- 'lo.comp.imports@decodeViz'(XV30510, XV30511).
'lo.comp.imports^decodeImports'('_call%2'(XV30512, XV30513), 'lo.comp.imports^decodeImports', _):- 'lo.comp.imports@decodeImports'(XV30512, XV30513).
'lo.comp.imports^decodeSignature'('_call%2'(XV30514, XV30515), 'lo.comp.imports^decodeSignature', _):- 'lo.comp.imports@decodeSignature'(XV30514, XV30515).
'lo.comp.imports^pickupEnums'('_call%3'(XV30516, XV30517, XV30518), 'lo.comp.imports^pickupEnums', _):- 'lo.comp.imports@pickupEnums'(XV30516, XV30517, XV30518).
'lo.comp.imports^findContracts'('_call%3'(XV30519, XV30520, XV30521), 'lo.comp.imports^findContracts', _):- 'lo.comp.imports@findContracts'(XV30519, XV30520, XV30521).
'lo.comp.imports^pickupImplementations'('_call%3'(XV30522, XV30523, XV30524), 'lo.comp.imports^pickupImplementations', _):- 'lo.comp.imports@pickupImplementations'(XV30522, XV30523, XV30524).
'lo.comp.imports^pickupPkgSpec'('_call%10'(XV30525, XV30526, XV30527, XV30528, XV30529, XV30530, XV30531, XV30532, XV30533, XV30534), 'lo.comp.imports^pickupPkgSpec', _):- 'lo.comp.imports@pickupPkgSpec'(XV30525, XV30526, XV30527, XV30528, XV30529, XV30530, XV30531, XV30532, XV30533, XV30534).
'lo.comp.imports@one280'(X_33762, X_33763, XStx1822, XRp0, XRp, XA, XXd39197, XPkg, XXc527, XStrm):- 'explode'(XStrm, XXc527),
    'lo.comp.lexer@getNTokens'(XXc527, XPkg, 5, XXd39197),
    'lo.comp.grammar@parse'(XXd39197, XStx1822, XA, XRp, XRp0),
    XStx1822 = X_33763,
    ocall('_eof%1'(X_33763),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    X_33762 = XStx1822,
    !.
'lo.comp.imports@one281'(XRpx, XRp0, XImpls, XContracts, XEnums, XTypes, XExport, XImports, XIPkg, XA):- 'lo.comp.imports@pickupPkgSpec'(XA, XIPkg, XImports, XExport, XTypes, XEnums, XContracts, XImpls, XRp0, XRpx),
    !.
'lo.comp.imports^importPkg'('_call%6'(XV30535, XV30536, XV30537, XV30538, XV30539, XV30540), 'lo.comp.imports^importPkg', _):- 'lo.comp.imports@importPkg'(XV30535, XV30536, XV30537, XV30538, XV30539, XV30540).
'lo.comp.imports^consistentVersion'('_call%2'(XV30541, XV30542), 'lo.comp.imports^consistentVersion', _):- 'lo.comp.imports@consistentVersion'(XV30541, XV30542).
'lo.comp.imports@one282'(XV2, XV1):- 'lo.comp.imports@consistentVersion'(XV1, XV2),
    !.
'lo.comp.imports^consistentPkg'('_call%2'(XV30543, XV30544), 'lo.comp.imports^consistentPkg', _):- 'lo.comp.imports@consistentPkg'(XV30543, XV30544).
'lo.comp.imports^alreadyImported'('_call%2'(XV30545, XV30546), 'lo.comp.imports^alreadyImported', _):- 'lo.comp.imports@alreadyImported'(XV30545, XV30546).
'lo.comp.imports^importPkgs'('_call%7'(XV30547, XV30548, XV30549, XV30550, XV30551, XV30552, XV30553), 'lo.comp.imports^importPkgs', _):- 'lo.comp.imports@importPkgs'(XV30547, XV30548, XV30549, XV30550, XV30551, XV30552, XV30553).
'lo.comp.imports^importsOfSpec'('_call%2'(XV30554, XV30555), 'lo.comp.imports^importsOfSpec', _):- 'lo.comp.imports@importsOfSpec'(XV30554, XV30555).
'lo.comp.imports^publicImports'('_call%2'(XV30556, XV30557), 'lo.comp.imports^publicImports', _):- 'lo.comp.imports@publicImports'(XV30556, XV30557).
'lo.comp.imports^importClosure'('_call%7'(XV30558, XV30559, XV30560, XV30561, XV30562, XV30563, XV30564), 'lo.comp.imports^importClosure', _):- 'lo.comp.imports@importClosure'(XV30558, XV30559, XV30560, XV30561, XV30562, XV30563, XV30564).
'lo.comp.imports^decodeTypes'('_call%2'(XV30565, XV30566), 'lo.comp.imports^decodeTypes', _):- 'lo.comp.imports@decodeTypes'(XV30565, XV30566).
'lo.comp.imports^splitOnHash'('_call%2'(XV30567, XV30568), 'lo.comp.imports^splitOnHash', _):- 'lo.comp.imports@splitOnHash'(XV30567, XV30568).
