'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.imports'e'*'n26o26'()26'n2o2'import'e'private'n2o2'pkg's'lo.comp.canon'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.abstract'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.ast'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.grammar'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.lexer'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.token'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.decode'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.misc'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.dict'e'*'n2o2'import'e'private'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.resources'e'*'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I4'importPkg':k'r'|PT5t'lo.repo*pkg'k'r't'lo.comp.package*pkgSpec't'lo.comp.errors*report't'lo.comp.errors*report'c'lo.repo$repository'T1k'r'T0'consistentPkg'PT2t'lo.repo*pkg't'lo.repo*pkg''importPkgs':k'r'|FT5Lt'lo.repo*pkg'Lt'lo.comp.package*pkgSpec'k'r't'lo.comp.errors*report't'lo.comp.errors*report'Lt'lo.comp.package*pkgSpec'c'lo.repo$repository'T1k'r'T0'importClosure':k'r'|FT5Lt'lo.repo*pkg'Lt'lo.comp.package*pkgSpec'k'r't'lo.comp.errors*report't'lo.comp.errors*report'Lt'lo.comp.package*pkgSpec'c'lo.repo$repository'T1k'r'T0\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.imports@init'() :- !.
'lo.comp.imports@decodeVers'('lo.comp.term#enum'("*"), 'lo.repo#defltVersion') :- !.
'lo.comp.imports@decodeVers'('lo.comp.term#strng'(XV), 'lo.repo#vers'(XV)) :- !.
'lo.comp.imports@decodeVers'(_, _) :- raise_exception('error'("decodeVers", 83, 3, 35)).
'lo.comp.imports@decodePkg'('lo.comp.term#cons'('lo.comp.term#strct'("pkg", 2), 'lo.core#,..'('lo.comp.term#strng'(XPk), 'lo.core#,..'(XV, 'lo.core#[]'))), 'lo.repo#pkg'(XPk, XX12753)) :- !,
    'lo.comp.imports@decodeVers'(XV, XX12753).
'lo.comp.imports@decodePkg'(_, _) :- raise_exception('error'("decodePkg", 80, 3, 69)).
'lo.comp.imports@decodeViz'('lo.comp.term#enum'("public"), 'lo.comp.package#pUblic') :- !.
'lo.comp.imports@decodeViz'('lo.comp.term#enum'("private"), 'lo.comp.package#priVate') :- !.
'lo.comp.imports@decodeViz'(_, _) :- raise_exception('error'("decodeViz", 76, 3, 35)).
'lo.comp.imports@decodeImports'('lo.core#[]', 'lo.core#[]') :- !.
'lo.comp.imports@decodeImports'('lo.core#,..'('lo.comp.term#cons'('lo.comp.term#strct'("import", 2), 'lo.core#,..'(XVz, 'lo.core#,..'(XPk, 'lo.core#[]'))), XIs), 'lo.core#,..'((XX12771, XX12773), XX12775)) :- !,
    'lo.comp.imports@decodeViz'(XVz, XX12771),
    'lo.comp.imports@decodePkg'(XPk, XX12773),
    'lo.comp.imports@decodeImports'(XIs, XX12775).
'lo.comp.imports@decodeImports'(_, _) :- raise_exception('error'("decodeImports", 69, 3, 23)).
'lo.comp.imports@decodeSignature'('lo.comp.term#strng'(XS), XT) :- 'explode'(XS, XX12780),
    'lo.comp.decode@decodeType'(XX12780, XStx350, XT),
    ocall('_eof%1'(XStx350),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    !.
'lo.comp.imports@decodeSignature'(_, _) :- raise_exception('error'("decodeSignature", 104, 3, 61)).
'lo.comp.imports@pickupEnums'('lo.core#[]', XCls, XCls).
'lo.comp.imports@pickupEnums'('lo.core#,..'('lo.comp.term#strng'(XNm), XRest), 'lo.core#,..'(XNm, XMore), XCls) :- 'lo.comp.imports@pickupEnums'(XRest, XMore, XCls).
'lo.comp.imports@findContracts'('lo.core#[]', XC, XC).
'lo.comp.imports@findContracts'('lo.core#,..'('lo.comp.term#cons'(X_848, 'lo.core#,..'('lo.comp.term#strng'(XNm), 'lo.core#,..'('lo.comp.term#strng'(XConNm), 'lo.core#,..'('lo.comp.term#strng'(XConSig), 'lo.core#,..'(XFaceSig, 'lo.core#[]'))))), XM), 'lo.core#,..'('lo.comp.types#conEntry'(XNm, XConNm, XCon, XX12822), XC), XCx) :- 'lo.comp.decode@decodeConstraint'(XConSig, XCon),
    'lo.comp.imports@findContracts'(XM, XC, XCx),
    'lo.comp.imports@decodeSignature'(XFaceSig, XX12822).
'lo.comp.imports@pickupImplementations'('lo.core#[]', XI, XI).
'lo.comp.imports@pickupImplementations'('lo.core#,..'('lo.comp.term#cons'(X_849, 'lo.core#,..'('lo.comp.term#strng'(XNm), 'lo.core#,..'('lo.comp.term#strng'(XSig), 'lo.core#[]'))), XM), 'lo.core#,..'('lo.comp.types#implEntry'(XNm, XSpec), XI), XRI) :- 'lo.comp.decode@decodeConstraint'(XSig, XSpec),
    'lo.comp.imports@pickupImplementations'(XM, XI, XRI).
'lo.comp.imports@pickupPkgSpec'(XTerm, XX12859, XX12861, XFields, XTypes, XEnums, XContracts, XImpls, XRp, XRp) :- 'lo.comp.abstract@isUnary'(XTerm, "#pkg", X_850, XE),
    'lo.comp.abstract@isString'(XE, X_851, XEnc),
    'lo.comp.decode@decodeValue'(XEnc, XX12901),
    'lo.comp.term#cons'(X_852, 'lo.core#,..'(XPk, 'lo.core#,..'('lo.comp.term#cons'(X_853, XIs), 'lo.core#,..'(XEx, 'lo.core#,..'(XTp, 'lo.core#,..'('lo.comp.term#cons'(X_854, XC), 'lo.core#,..'('lo.comp.term#cons'(X_855, XCo), 'lo.core#,..'('lo.comp.term#cons'(X_856, XIm), 'lo.core#[]')))))))) = XX12901,
    'lo.comp.imports@decodeSignature'(XEx, XX12905),
    'lo.comp.types#faceType'(XFields) = XX12905,
    'lo.comp.imports@decodeSignature'(XTp, XX12909),
    'lo.comp.types#faceType'(XTypes) = XX12909,
    'lo.comp.imports@pickupEnums'(XC, XEnums, 'lo.core#[]'),
    'lo.comp.imports@findContracts'(XCo, XContracts, 'lo.core#[]'),
    'lo.comp.imports@pickupImplementations'(XIm, XImpls, 'lo.core#[]'),
    'lo.comp.imports@decodePkg'(XPk, XX12859),
    'lo.comp.imports@decodeImports'(XIs, XX12861).
'lo.comp.imports@importPkg'(Xlo_repo_repository_r1, 'lo.repo#pkg'(XPkg, XVers), XRepo, 'lo.comp.package#pkgSpec'(XIPkg, XExport, XTypes, XEnums, XContracts, XImpls, XImports), XRp, XRpx) :- ocall('loadFromRepo%4'(XRepo, 'lo.repo#pkg'(XPkg, XVers), "prolog", XStrm),Xlo_repo_repository_r1,Xlo_repo_repository_r1),
    'lo.comp.imports@one10'(XStx351, XRp0, XRp, XA, XX12943, XX12943, XPkg, XX12941, XStrm),
    'lo.comp.imports@one11'(XRpx, XRp0, XImpls, XContracts, XEnums, XTypes, XExport, XImports, XIPkg, XA).
'lo.comp.imports@consistentVersion'('lo.repo#defltVersion', X_857).
'lo.comp.imports@consistentVersion'('lo.repo#vers'(XV), 'lo.repo#vers'(XV)).
'lo.comp.imports@consistentPkg'('lo.repo#pkg'(XP, XV1), 'lo.repo#pkg'(XP, XV2)) :- 'lo.comp.imports@one12'(XV2, XV1).
'lo.comp.imports@alreadyImported'(XPkg, XSpecs) :- ocall('in%2'('lo.comp.package#pkgSpec'(XPk, X_858, X_859, X_860, X_861, X_862, X_863), XSpecs),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.imports@consistentPkg'(XPkg, XPk).
'lo.comp.imports@importPkgs'(Xlo_repo_repository_r2, 'lo.core#[]', XSpecs, X_864, XRp, XRp, XSpecs) :- !.
'lo.comp.imports@importPkgs'(Xlo_repo_repository_r2, 'lo.core#,..'(XPkg, XL), XSoFar, XRepo, XRp, XRpx, XX13010) :- 'lo.comp.imports@alreadyImported'(XPkg, XSoFar),
    !,
    'lo.comp.imports@importPkgs'(Xlo_repo_repository_r2, XL, XSoFar, XRepo, XRp, XRpx, XX13010).
'lo.comp.imports@importPkgs'(Xlo_repo_repository_r2, 'lo.core#,..'(XPkg, XL), XSoFar, XRepo, XRp, XRpx, XX13033) :- 'lo.comp.imports@importPkg'(Xlo_repo_repository_r2, XPkg, XRepo, XSpec, XRp, XRp0),
    !,
    'lo.comp.imports@importPkgs'(Xlo_repo_repository_r2, XL, 'lo.core#,..'(XSpec, XSoFar), XRepo, XRp0, XRpx, XX13033).
'lo.comp.imports@importPkgs'(_, _, _, _, _, _) :- raise_exception('error'("importPkgs", 39, 3, 37)).
'lo.comp.imports@publicImports'('lo.core#[]', 'lo.core#[]') :- !.
'lo.comp.imports@publicImports'('lo.core#,..'(('lo.comp.package#pUblic', XPkg), XL), 'lo.core#,..'(XPkg, XX13042)) :- !,
    'lo.comp.imports@publicImports'(XL, XX13042).
'lo.comp.imports@publicImports'('lo.core#,..'(('lo.comp.package#priVate', X_865), XL), XX13049) :- !,
    'lo.comp.imports@publicImports'(XL, XX13049).
'lo.comp.imports@publicImports'(_, _) :- raise_exception('error'("publicImports", 64, 3, 23)).
'lo.comp.imports@importsOfSpec'('lo.comp.package#pkgSpec'(X_866, X_867, X_868, X_869, X_870, X_871, XImps), XImps) :- !.
'lo.comp.imports@importsOfSpec'(_, _) :- raise_exception('error'("importsOfSpec", 61, 3, 48)).
'lo.comp.imports@importClosure'(Xlo_repo_repository_r3, 'lo.core#[]', XSpecs, X_872, XRp, XRp, XSpecs) :- !.
'lo.comp.imports@importClosure'(Xlo_repo_repository_r3, 'lo.core#,..'(XPkg, XL), XSoFar, XRepo, XRp, XRpx, XX13082) :- 'lo.comp.imports@alreadyImported'(XPkg, XSoFar),
    !,
    'lo.comp.imports@importClosure'(Xlo_repo_repository_r3, XL, XSoFar, XRepo, XRp, XRpx, XX13082).
'lo.comp.imports@importClosure'(Xlo_repo_repository_r3, 'lo.core#,..'(XPkg, XL), XSoFar, XRepo, XRp, XRpx, XX13113) :- 'lo.comp.imports@importPkg'(Xlo_repo_repository_r3, XPkg, XRepo, XSpec, XRp, XRp0),
    !,
    'lo.comp.imports@importsOfSpec'(XSpec, XX13101),
    'lo.comp.imports@publicImports'(XX13101, XX13102),
    'lo.comp.imports@importClosure'(Xlo_repo_repository_r3, XX13102, 'lo.core#,..'(XSpec, XSoFar), XRepo, XRp0, XRp1, XX13109),
    'lo.comp.imports@importClosure'(Xlo_repo_repository_r3, XL, XX13109, XRepo, XRp1, XRpx, XX13113).
'lo.comp.imports@importClosure'(_, _, _, _, _, _) :- raise_exception('error'("importClosure", 45, 3, 40)).
'lo.comp.imports@decodeTypes'(XTxt, XF) :- 'explode'(XTxt, XX13116),
    'lo.comp.decode@decodeType'(XX13116, XStx352, 'lo.comp.types#faceType'(XF)),
    ocall('_eof%1'(XStx352),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    !.
'lo.comp.imports@decodeTypes'(_, _) :- raise_exception('error'("decodeTypes", 73, 3, 64)).
'lo.comp.imports@splitOnHash'(XS, XX13124) :- !,
    'lo.comp.misc@pathSuffix'(XS, "#", XX13124).
'lo.comp.imports@splitOnHash'(_, _) :- raise_exception('error'("splitOnHash", 107, 3, 35)).
'lo.comp.imports^decodeVers'('_call%2'(XV2074, XV2075), 'lo.comp.imports^decodeVers', _) :- 'lo.comp.imports@decodeVers'(XV2074, XV2075).
'lo.comp.imports^decodePkg'('_call%2'(XV2076, XV2077), 'lo.comp.imports^decodePkg', _) :- 'lo.comp.imports@decodePkg'(XV2076, XV2077).
'lo.comp.imports^decodeViz'('_call%2'(XV2078, XV2079), 'lo.comp.imports^decodeViz', _) :- 'lo.comp.imports@decodeViz'(XV2078, XV2079).
'lo.comp.imports^decodeImports'('_call%2'(XV2080, XV2081), 'lo.comp.imports^decodeImports', _) :- 'lo.comp.imports@decodeImports'(XV2080, XV2081).
'lo.comp.imports^decodeSignature'('_call%2'(XV2082, XV2083), 'lo.comp.imports^decodeSignature', _) :- 'lo.comp.imports@decodeSignature'(XV2082, XV2083).
'lo.comp.imports^pickupEnums'('_call%3'(XV2084, XV2085, XV2086), 'lo.comp.imports^pickupEnums', _) :- 'lo.comp.imports@pickupEnums'(XV2084, XV2085, XV2086).
'lo.comp.imports^findContracts'('_call%3'(XV2087, XV2088, XV2089), 'lo.comp.imports^findContracts', _) :- 'lo.comp.imports@findContracts'(XV2087, XV2088, XV2089).
'lo.comp.imports^pickupImplementations'('_call%3'(XV2090, XV2091, XV2092), 'lo.comp.imports^pickupImplementations', _) :- 'lo.comp.imports@pickupImplementations'(XV2090, XV2091, XV2092).
'lo.comp.imports^pickupPkgSpec'('_call%10'(XV2093, XV2094, XV2095, XV2096, XV2097, XV2098, XV2099, XV2100, XV2101, XV2102), 'lo.comp.imports^pickupPkgSpec', _) :- 'lo.comp.imports@pickupPkgSpec'(XV2093, XV2094, XV2095, XV2096, XV2097, XV2098, XV2099, XV2100, XV2101, XV2102).
'lo.comp.imports@one10'(XStx351, XRp0, XRp, XA, XX12943, XX12943, XPkg, XX12941, XStrm) :- 'explode'(XStrm, XX12941),
    'lo.comp.lexer@getNTokens'(XX12941, XPkg, 5, XX12943),
    'lo.comp.grammar@parse'(XX12943, XStx351, XA, XRp, XRp0),
    ocall('_eof%1'(XStx351),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    !.
'lo.comp.imports@one11'(XRpx, XRp0, XImpls, XContracts, XEnums, XTypes, XExport, XImports, XIPkg, XA) :- 'lo.comp.imports@pickupPkgSpec'(XA, XIPkg, XImports, XExport, XTypes, XEnums, XContracts, XImpls, XRp0, XRpx),
    !.
'lo.comp.imports^importPkg'('_call%5'(XV2103, XV2104, XV2105, XV2106, XV2107), 'lo.comp.imports^importPkg', _) :- 'lo.comp.imports@importPkg'(XV2103, XV2104, XV2105, XV2106, XV2107).
'lo.comp.imports^consistentVersion'('_call%2'(XV2108, XV2109), 'lo.comp.imports^consistentVersion', _) :- 'lo.comp.imports@consistentVersion'(XV2108, XV2109).
'lo.comp.imports@one12'(XV2, XV1) :- 'lo.comp.imports@consistentVersion'(XV1, XV2),
    !.
'lo.comp.imports^consistentPkg'('_call%2'(XV2110, XV2111), 'lo.comp.imports^consistentPkg', _) :- 'lo.comp.imports@consistentPkg'(XV2110, XV2111).
'lo.comp.imports^alreadyImported'('_call%2'(XV2112, XV2113), 'lo.comp.imports^alreadyImported', _) :- 'lo.comp.imports@alreadyImported'(XV2112, XV2113).
'lo.comp.imports^importPkgs'('_call%6'(XV2114, XV2115, XV2116, XV2117, XV2118, XV2119), 'lo.comp.imports^importPkgs', _) :- 'lo.comp.imports@importPkgs'(XV2114, XV2115, XV2116, XV2117, XV2118, XV2119).
'lo.comp.imports^publicImports'('_call%2'(XV2120, XV2121), 'lo.comp.imports^publicImports', _) :- 'lo.comp.imports@publicImports'(XV2120, XV2121).
'lo.comp.imports^importsOfSpec'('_call%2'(XV2122, XV2123), 'lo.comp.imports^importsOfSpec', _) :- 'lo.comp.imports@importsOfSpec'(XV2122, XV2123).
'lo.comp.imports^importClosure'('_call%6'(XV2124, XV2125, XV2126, XV2127, XV2128, XV2129), 'lo.comp.imports^importClosure', _) :- 'lo.comp.imports@importClosure'(XV2124, XV2125, XV2126, XV2127, XV2128, XV2129).
'lo.comp.imports^decodeTypes'('_call%2'(XV2130, XV2131), 'lo.comp.imports^decodeTypes', _) :- 'lo.comp.imports@decodeTypes'(XV2130, XV2131).
'lo.comp.imports^splitOnHash'('_call%2'(XV2132, XV2133), 'lo.comp.imports^splitOnHash', _) :- 'lo.comp.imports@splitOnHash'(XV2132, XV2133).
