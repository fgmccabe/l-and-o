'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.grapher'e'*'n24o24'()24'n2o2'import'e'private'n2o2'pkg's'lo.comp.misc'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.imports'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.catalog'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.canon'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.ast'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.abstract'e'*'n2o2'import'e'public'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.grammar'e'*'n2o2'import'e'private'n2o2'pkg's'lo.topsort'e'*'n2o2'import'e'private'n2o2'pkg's'lo.resources'e'*'n2o2'import'e'private'n2o2'pkg's'lo.repo.file'e'*'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I1'makeGraph'FT5Lt'lo.repo*pkg't'lo.repo.file*fileRepo't'lo.comp.catalog*catalog't'lo.comp.errors*report't'lo.comp.errors*report'LLT2t'lo.repo*pkg'Lt'lo.repo*pkg'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.grapher@init'() :- !.
'lo.comp.grapher@projectOutPkgs'('lo.core#[]', 'lo.core#[]') :- !.
'lo.comp.grapher@projectOutPkgs'('lo.core#,..'((X_1552, XPkg), XL), 'lo.core#,..'(XPkg, XX23306)) :- !,
    'lo.comp.grapher@projectOutPkgs'(XL, XX23306).
'lo.comp.grapher@projectOutPkgs'(_, _) :- raise_exception('error'("projectOutPkgs", 66, 3, 24)).
'lo.comp.grapher@scanPkgName'(XT, X_1553, 'lo.repo#pkg'(XX23315, 'lo.repo#vers'(XX23322))) :- 'lo.comp.abstract@isBinary'(XT, "#", X_1554, XP, XV),
    !,
    'lo.comp.abstract@packageName'(XP, XX23315),
    ocall('disp%2'(XV, XX23317),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XX23317, 'lo.core#[]')), XX23322).
'lo.comp.grapher@scanPkgName'(XT, XV, 'lo.repo#pkg'(XX23328, XV)) :- !,
    'lo.comp.abstract@packageName'(XT, XX23328).
'lo.comp.grapher@scanPkgName'(_, _, _) :- raise_exception('error'("scanPkgName", 92, 3, 81)).
'lo.comp.grapher@scanStmt'(XSt, XImp, XX23338) :- 'lo.comp.abstract@isUnary'(XSt, "public", X_1555, XEl),
    !,
    'lo.comp.grapher@scanStmt'(XEl, XImp, XX23338).
'lo.comp.grapher@scanStmt'(XSt, XImp, XX23346) :- 'lo.comp.abstract@isUnary'(XSt, "private", X_1556, XEl),
    !,
    'lo.comp.grapher@scanStmt'(XEl, XImp, XX23346).
'lo.comp.grapher@scanStmt'(XSt, XImps, 'lo.core#,..'(XX23354, XImps)) :- 'lo.comp.abstract@isUnary'(XSt, "import", X_1557, XP),
    !,
    'lo.comp.grapher@scanPkgName'(XP, 'lo.repo#defltVersion', XX23354).
'lo.comp.grapher@scanStmt'(XSt, XImps, XImps) :- !.
'lo.comp.grapher@scanStmt'(_, _, _) :- raise_exception('error'("scanStmt", 83, 3, 69)).
'lo.comp.grapher@scanTheta'('lo.core#[]', XImps, XImps) :- !.
'lo.comp.grapher@scanTheta'('lo.core#,..'(XSt, XStmts), XImports, XX23371) :- !,
    'lo.comp.grapher@scanStmt'(XSt, XImports, XX23370),
    'lo.comp.grapher@scanTheta'(XStmts, XX23370, XX23371).
'lo.comp.grapher@scanTheta'(_, _, _) :- raise_exception('error'("scanTheta", 79, 3, 26)).
'lo.comp.grapher@scanTerm'(XT, XX23375) :- 'lo.comp.abstract@isBraceTerm'(XT, X_1558, XP, XTh),
    'lo.comp.grapher@scanTheta'(XTh, 'lo.core#[]', XX23375).
'lo.comp.grapher@scanFile'(XFl, XPkg, XRepo, XCat, XSoFar, XRp, XRpx, XX23403) :- 'lo.comp.grammar@parseFile'(XFl, XRp, XRp0, XX23391),
    XTerm = XX23391,
    'lo.comp.grapher@scanTerm'(XTerm, XImps),
    !,
    'lo.comp.grapher@scanPkgs'(XImps, XRepo, XCat, 'lo.core#,..'((XPkg, XImps), XSoFar), XRp0, XRpx, XX23403).
'lo.comp.grapher@scanFile'(_, _, _, _, _, _, _, _) :- raise_exception('error'("scanFile", 70, 3, 154)).
'lo.comp.grapher@checkPkg'('lo.comp.package#pkgSpec'(XPkg, X_1559, X_1560, X_1561, X_1562, X_1563, XImports), XRepo, XCat, XSoFar, XRp, XRpx, XX23433) :- 'lo.comp.catalog@resolveCatalog'(XCat, XPkg, X_1564, XVPkg),
    'lo.comp.grapher@projectOutPkgs'(XImports, XX23423),
    XImported = XX23423,
    !,
    'lo.comp.grapher@scanPkgs'(XImported, XRepo, XCat, 'lo.core#,..'((XVPkg, XImported), XSoFar), XRp, XRpx, XX23433).
'lo.comp.grapher@checkPkg'(_, _, _, _, _, _, _) :- raise_exception('error'("checkPkg", 61, 3, 204)).
'lo.comp.grapher@scanPkg'(XRq, X_1565, X_1566, XSoFar, XRp, XRp, XSoFar) :- ocall('in%2'((XPkg, X_1567), XSoFar),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.imports@consistentPkg'(XRq, XPkg),
    !.
'lo.comp.grapher@scanPkg'(XPkg, XRepo, XCat, XSoFar, XRp, XRpx, XX23467) :- 'lo.repo.file@packagePrologOk'(XRepo, XPkg),
    'lo.comp.imports@importPkg'('lo.repo$repository$lo.repo.file*fileRepo', XPkg, XRepo, XSpec, XRp, XRp0),
    !,
    'lo.comp.grapher@checkPkg'(XSpec, XRepo, XCat, XSoFar, XRp0, XRpx, XX23467).
'lo.comp.grapher@scanPkg'(XPkg, XRepo, XCat, XSoFar, XRp, XRpx, XX23485) :- 'lo.comp.catalog@resolveCatalog'(XCat, XPkg, XSrcUri, XVPkg),
    !,
    'lo.comp.grapher@scanFile'(XSrcUri, XVPkg, XRepo, XCat, XSoFar, XRp, XRpx, XX23485).
'lo.comp.grapher@scanPkg'(XPkg, XRepo, XCat, XSoFar, XRp, XRpx, XSoFar) :- ocall('disp%2'(XPkg, XX23494),'lo.core$display$lo.repo*pkg','lo.core$display$lo.repo*pkg'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot locate package "), 'lo.core#,..'(XX23494, 'lo.core#[]'))), XX23500),
    'lo.comp.errors@reportError'(XX23500, Xstd, XRp, XRpx),
    !.
'lo.comp.grapher@scanPkg'(_, _, _, _, _, _, _) :- raise_exception('error'("scanPkg", 49, 3, 87)).
'lo.comp.grapher@scanPkgs'('lo.core#[]', X_1568, X_1569, XSoFar, XRp, XRp, XSoFar) :- !.
'lo.comp.grapher@scanPkgs'('lo.core#,..'(XPkg, XImports), XRepo, XCat, XSoFar, XRp, XRpx, XX23532) :- !,
    'lo.comp.grapher@scanPkg'(XPkg, XRepo, XCat, XSoFar, XRp, XRp0, XX23529),
    'lo.comp.grapher@scanPkgs'(XImports, XRepo, XCat, XX23529, XRp0, XRpx, XX23532).
'lo.comp.grapher@scanPkgs'(_, _, _, _, _, _, _) :- raise_exception('error'("scanPkgs", 45, 3, 37)).
'lo.comp.grapher@makeGraph'(XPkgs, XRepo, XCat, XRp, XRpx, XX23546) :- !,
    'lo.comp.grapher@scanPkgs'(XPkgs, XRepo, XCat, 'lo.core#[]', XRp, XRpx, XX23545),
    'lo.topsort@topsort'('lo.topsort$depends$()2', XX23545, XX23546).
'lo.comp.grapher@makeGraph'(_, _, _, _, _, _) :- raise_exception('error'("makeGraph", 19, 3, 77)).
'lo.topsort$depends$()2'('lo.topsort$depends$()2%1'('lo.topsort$depends$()2')) :- !.
'lo.topsort$depends$()2'('defines%2'(XV3388, XV3389), XLbl285, XThis285) :- !,
    'lo.topsort$depends$()2@defines'(XV3388, XV3389, XLbl285, XThis285).
'lo.topsort$depends$()2'('defines%1'('lo.topsort$depends$()2^defines'(XLbl286, XThis286)), XLbl286, XThis286).
'lo.topsort$depends$()2'('references%2'(XV3394, XV3395), XLbl287, XThis287) :- !,
    'lo.topsort$depends$()2@references'(XV3394, XV3395, XLbl287, XThis287).
'lo.topsort$depends$()2'('references%1'('lo.topsort$depends$()2^references'(XLbl288, XThis288)), XLbl288, XThis288).
'lo.topsort$depends$()2@defines'((XPk, X_1570), XRq, XLbV303, XThV303) :- 'lo.comp.imports@consistentPkg'(XRq, XPk).
'lo.topsort$depends$()2@references'((XPk, XI), XI, XLbV303, XThV303) :- !.
'lo.topsort$depends$()2@references'(_, _, _, _) :- raise_exception('error'("references", 24, 5, 23)).
'lo.comp.grapher@showPkg'((XP, XI)) :- ocall('disp%2'(XP, XX23558),'lo.core$display$lo.repo*pkg','lo.core$display$lo.repo*pkg'),
    ocall('disp%2'(XI, XX23562),'lo.core$display$lo.core*list'('lo.core$display$lo.repo*pkg'),'lo.core$display$lo.core*list'('lo.core$display$lo.repo*pkg')),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XX23558, 'lo.core#,..'('lo.core#ss'(" --> "), 'lo.core#,..'(XX23562, 'lo.core#[]')))), XX23570),
    'lo.io@logMsg'(XX23570).
'lo.comp.grapher@showGroup'('lo.core#[]').
'lo.comp.grapher@showGroup'('lo.core#,..'(XP, XL)) :- 'lo.comp.grapher@showPkg'(XP),
    'lo.comp.grapher@showGroup'(XL).
'lo.comp.grapher@showGroups'('lo.core#[]').
'lo.comp.grapher@showGroups'('lo.core#,..'(XG, XL)) :- 'lo.io@logMsg'("---"),
    'lo.comp.grapher@showGroup'(XG),
    'lo.comp.grapher@showGroups'(XL).
'lo.comp.grapher^projectOutPkgs'('_call%2'(XV3338, XV3339), 'lo.comp.grapher^projectOutPkgs', _) :- 'lo.comp.grapher@projectOutPkgs'(XV3338, XV3339).
'lo.comp.grapher^scanPkgName'('_call%3'(XV3340, XV3341, XV3342), 'lo.comp.grapher^scanPkgName', _) :- 'lo.comp.grapher@scanPkgName'(XV3340, XV3341, XV3342).
'lo.comp.grapher^scanStmt'('_call%3'(XV3343, XV3344, XV3345), 'lo.comp.grapher^scanStmt', _) :- 'lo.comp.grapher@scanStmt'(XV3343, XV3344, XV3345).
'lo.comp.grapher^scanTheta'('_call%3'(XV3346, XV3347, XV3348), 'lo.comp.grapher^scanTheta', _) :- 'lo.comp.grapher@scanTheta'(XV3346, XV3347, XV3348).
'lo.comp.grapher^scanTerm'('_call%2'(XV3349, XV3350), 'lo.comp.grapher^scanTerm', _) :- 'lo.comp.grapher@scanTerm'(XV3349, XV3350).
'lo.comp.grapher^scanFile'('_call%8'(XV3351, XV3352, XV3353, XV3354, XV3355, XV3356, XV3357, XV3358), 'lo.comp.grapher^scanFile', _) :- 'lo.comp.grapher@scanFile'(XV3351, XV3352, XV3353, XV3354, XV3355, XV3356, XV3357, XV3358).
'lo.comp.grapher^checkPkg'('_call%7'(XV3359, XV3360, XV3361, XV3362, XV3363, XV3364, XV3365), 'lo.comp.grapher^checkPkg', _) :- 'lo.comp.grapher@checkPkg'(XV3359, XV3360, XV3361, XV3362, XV3363, XV3364, XV3365).
'lo.comp.grapher^scanPkg'('_call%7'(XV3366, XV3367, XV3368, XV3369, XV3370, XV3371, XV3372), 'lo.comp.grapher^scanPkg', _) :- 'lo.comp.grapher@scanPkg'(XV3366, XV3367, XV3368, XV3369, XV3370, XV3371, XV3372).
'lo.comp.grapher^scanPkgs'('_call%7'(XV3373, XV3374, XV3375, XV3376, XV3377, XV3378, XV3379), 'lo.comp.grapher^scanPkgs', _) :- 'lo.comp.grapher@scanPkgs'(XV3373, XV3374, XV3375, XV3376, XV3377, XV3378, XV3379).
'lo.comp.grapher^makeGraph'('_call%6'(XV3380, XV3381, XV3382, XV3383, XV3384, XV3385), 'lo.comp.grapher^makeGraph', _) :- 'lo.comp.grapher@makeGraph'(XV3380, XV3381, XV3382, XV3383, XV3384, XV3385).
'lo.topsort$depends$()2^defines'('_call%2'(XV3386, XV3387), 'lo.topsort$depends$()2^defines'(XLbV303, XThV303), _) :- 'lo.topsort$depends$()2@defines'(XV3386, XV3387, XLbV303, XThV303).
'lo.topsort$depends$()2^defines'('_call%2'(XV3390, XV3391), 'lo.topsort$depends$()2^defines'(XLbV303, XThV303), _) :- 'lo.topsort$depends$()2@defines'(XV3390, XV3391, XLbV303, XThV303).
'lo.topsort$depends$()2^references'('_call%2'(XV3392, XV3393), 'lo.topsort$depends$()2^references'(XLbV303, XThV303), _) :- 'lo.topsort$depends$()2@references'(XV3392, XV3393, XLbV303, XThV303).
'lo.topsort$depends$()2^references'('_call%2'(XV3396, XV3397), 'lo.topsort$depends$()2^references'(XLbV303, XThV303), _) :- 'lo.topsort$depends$()2@references'(XV3396, XV3397, XLbV303, XThV303).
'lo.comp.grapher^showPkg'('_call%1'(XV3398), 'lo.comp.grapher^showPkg', _) :- 'lo.comp.grapher@showPkg'(XV3398).
'lo.comp.grapher^showGroup'('_call%1'(XV3399), 'lo.comp.grapher^showGroup', _) :- 'lo.comp.grapher@showGroup'(XV3399).
'lo.comp.grapher^showGroups'('_call%1'(XV3400), 'lo.comp.grapher^showGroups', _) :- 'lo.comp.grapher@showGroups'(XV3400).
