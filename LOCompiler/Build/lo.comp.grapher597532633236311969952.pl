'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.grapher's'0.0.1'n14o14'()14'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'n2o2'import'e'private'n2o2'pkg's'lo.repo.file'e'*'n2o2'import'e'private'n2o2'pkg's'lo.resources'e'*'n2o2'import'e'private'n2o2'pkg's'lo.topsort'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.grammar'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.abstract'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.ast'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.canon'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.catalog'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.imports'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.misc'e'*'s\"I1'makeGraph'FT5Lt'lo.repo*pkg't'lo.repo.file*fileRepo't'lo.comp.catalog*catalog't'lo.comp.errors*report't'lo.comp.errors*report'LLT2t'lo.repo*pkg'Lt'lo.repo*pkg'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.grapher@init'():- !.
'lo.comp.grapher@scanPkgName'(XT, X_34978, 'lo.repo#pkg'(XXd39802, 'lo.repo#vers'(XXd39805))):- 'lo.comp.abstract@isBinary'(XT, "#", X_34979, XP, XV),
    !,
    ocall('disp%1'(XXV5438),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    'lo.comp.abstract@packageName'(XP, XXd39802),
    ocall('_call%2'(XV, XXe5072),XXV5438,XXV5438),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XXe5072, 'lo.core#[]')), XXd39805).
'lo.comp.grapher@scanPkgName'(XT, XV, 'lo.repo#pkg'(XXd39808, XV)):- !,
    'lo.comp.abstract@packageName'(XT, XXd39808).
'lo.comp.grapher@scanPkgName'(_, _, _):- raise_exception('error'("lo.comp.grapher@scanPkgName", 92, 3, 81)).
'lo.comp.grapher@scanStmt'(XSt, XImp, XXd39810):- 'lo.comp.abstract@isUnary'(XSt, "public", X_34981, XEl),
    !,
    'lo.comp.grapher@scanStmt'(XEl, XImp, XXd39810).
'lo.comp.grapher@scanStmt'(XSt, XImp, XXd39811):- 'lo.comp.abstract@isUnary'(XSt, "private", X_34982, XEl),
    !,
    'lo.comp.grapher@scanStmt'(XEl, XImp, XXd39811).
'lo.comp.grapher@scanStmt'(XSt, XImps, 'lo.core#,..'(XXd39812, XImps)):- 'lo.comp.abstract@isUnary'(XSt, "import", X_34983, XP),
    !,
    'lo.comp.grapher@scanPkgName'(XP, 'lo.repo#defltVersion', XXd39812).
'lo.comp.grapher@scanStmt'(XSt, XImps, XImps):- !.
'lo.comp.grapher@scanStmt'(_, _, _):- raise_exception('error'("lo.comp.grapher@scanStmt", 83, 3, 69)).
'lo.comp.grapher@scanTheta'('lo.core#[]', XImps, XImps):- !.
'lo.comp.grapher@scanTheta'('lo.core#,..'(XSt, XStmts), XImports, XXd39815):- !,
    'lo.comp.grapher@scanStmt'(XSt, XImports, XXd39814),
    'lo.comp.grapher@scanTheta'(XStmts, XXd39814, XXd39815).
'lo.comp.grapher@scanTheta'(_, _, _):- raise_exception('error'("lo.comp.grapher@scanTheta", 79, 3, 26)).
'lo.comp.grapher@scanTerm'(XT, XXb20088):- 'lo.comp.abstract@isBraceTerm'(XT, X_34986, XP, XTh),
    'lo.comp.grapher@scanTheta'(XTh, 'lo.core#[]', XXb20088).
'lo.comp.grapher@projectOutPkgs'('lo.core#[]', 'lo.core#[]'):- !.
'lo.comp.grapher@projectOutPkgs'('lo.core#,..'('()2'(X_34988, XPkg), XL), 'lo.core#,..'(XPkg, XXd39816)):- !,
    'lo.comp.grapher@projectOutPkgs'(XL, XXd39816).
'lo.comp.grapher@projectOutPkgs'(_, _):- raise_exception('error'("lo.comp.grapher@projectOutPkgs", 66, 3, 24)).
'lo.comp.grapher@checkPkg'('lo.comp.package#pkgSpec'(XPkg, X_34990, X_34991, X_34992, X_34993, X_34994, XImports), XRepo, XCat, XSoFar, XRp, XRpx, XXd39820):- 'lo.comp.catalog@resolveCatalog'(XCat, XPkg, X_34995, XVPkg),
    'lo.comp.grapher@projectOutPkgs'(XImports, XXd39818),
    XImported = XXd39818,
    !,
    'lo.comp.grapher@scanPkgs'(XImported, XRepo, XCat, 'lo.core#,..'('()2'(XVPkg, XImported), XSoFar), XRp, XRpx, XXd39820).
'lo.comp.grapher@checkPkg'(_, _, _, _, _, _, _):- raise_exception('error'("lo.comp.grapher@checkPkg", 61, 3, 204)).
'lo.comp.grapher@scanFile'(XFl, XPkg, XRepo, XCat, XSoFar, XRp, XRpx, XXd39823):- 'lo.comp.grammar@parseFile'(XFl, XRp, XRp0, XXd39821),
    XTerm = XXd39821,
    'lo.comp.grapher@scanTerm'(XTerm, XImps),
    !,
    'lo.comp.grapher@scanPkgs'(XImps, XRepo, XCat, 'lo.core#,..'('()2'(XPkg, XImps), XSoFar), XRp0, XRpx, XXd39823).
'lo.comp.grapher@scanFile'(_, _, _, _, _, _, _, _):- raise_exception('error'("lo.comp.grapher@scanFile", 70, 3, 154)).
'lo.comp.grapher@scanPkg'(XRq, X_34998, X_34999, XSoFar, XRp, XRp, XSoFar):- ocall('in%2'('()2'(XPkg, X_35000), XSoFar),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.imports@consistentPkg'(XRq, XPkg),
    !.
'lo.comp.grapher@scanPkg'(XPkg, XRepo, XCat, XSoFar, XRp, XRpx, XXd39824):- 'lo.repo.file@packagePrologOk'(XRepo, XPkg),
    'lo.comp.imports@importPkg'('lo.repo$repository$lo.repo.file*fileRepo', XPkg, XRepo, XSpec, XRp, XRp0),
    !,
    'lo.comp.grapher@checkPkg'(XSpec, XRepo, XCat, XSoFar, XRp0, XRpx, XXd39824).
'lo.comp.grapher@scanPkg'(XPkg, XRepo, XCat, XSoFar, XRp, XRpx, XXd39825):- 'lo.comp.catalog@resolveCatalog'(XCat, XPkg, XSrcUri, XVPkg),
    !,
    'lo.comp.grapher@scanFile'(XSrcUri, XVPkg, XRepo, XCat, XSoFar, XRp, XRpx, XXd39825).
'lo.comp.grapher@scanPkg'(XPkg, XRepo, XCat, XSoFar, XRp, XRpx, XSoFar):- ocall('disp%1'(XXV5439),'lo.core$display$lo.repo*pkg','lo.core$display$lo.repo*pkg'),
    ocall('_call%2'(XPkg, XXe5073),XXV5439,XXV5439),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot locate package "), 'lo.core#,..'(XXe5073, 'lo.core#[]'))), XXd39830),
    'lo.comp.errors@reportError'(XXd39830, Xstd, XRp, XRpx),
    !.
'lo.comp.grapher@scanPkg'(_, _, _, _, _, _, _):- raise_exception('error'("lo.comp.grapher@scanPkg", 49, 3, 87)).
'lo.comp.grapher@scanPkgs'('lo.core#[]', X_35003, X_35004, XSoFar, XRp, XRp, XSoFar):- !.
'lo.comp.grapher@scanPkgs'('lo.core#,..'(XPkg, XImports), XRepo, XCat, XSoFar, XRp, XRpx, XXd39832):- !,
    'lo.comp.grapher@scanPkg'(XPkg, XRepo, XCat, XSoFar, XRp, XRp0, XXd39831),
    'lo.comp.grapher@scanPkgs'(XImports, XRepo, XCat, XXd39831, XRp0, XRpx, XXd39832).
'lo.comp.grapher@scanPkgs'(_, _, _, _, _, _, _):- raise_exception('error'("lo.comp.grapher@scanPkgs", 45, 3, 37)).
'lo.comp.grapher@makeGraph'(XPkgs, XRepo, XCat, XRp, XRpx, XXd39834):- !,
    'lo.comp.grapher@scanPkgs'(XPkgs, XRepo, XCat, 'lo.core#[]', XRp, XRpx, XXd39833),
    'lo.topsort@topsort'('lo.topsort$depends$()2', XXd39833, XXd39834).
'lo.comp.grapher@makeGraph'(_, _, _, _, _, _):- raise_exception('error'("lo.comp.grapher@makeGraph", 19, 3, 77)).
'lo.topsort$depends$()2'('lo.topsort$depends$()2%1'('lo.topsort$depends$()2')):- !.
'lo.topsort$depends$()2'('defines%2'(XV31762, XV31763), XLbl2219, XThis2219):- !,
    'lo.topsort$depends$()2@defines'(XV31762, XV31763, XLbl2219, XThis2219).
'lo.topsort$depends$()2'('defines%1'('lo.topsort$depends$()2^defines'(XLbl2220, XThis2220)), XLbl2220, XThis2220).
'lo.topsort$depends$()2'('references%2'(XV31766, XV31767), XLbl2221, XThis2221):- !,
    'lo.topsort$depends$()2@references'(XV31766, XV31767, XLbl2221, XThis2221).
'lo.topsort$depends$()2'('references%1'('lo.topsort$depends$()2^references'(XLbl2222, XThis2222)), XLbl2222, XThis2222).
'lo.topsort$depends$()2@defines'('()2'(XPk, X_35006), XRq, XLbV2552, XThV2552):- 'lo.comp.imports@consistentPkg'(XRq, XPk).
'lo.topsort$depends$()2@references'('()2'(XPk, XI), XI, XLbV2552, XThV2552):- !.
'lo.topsort$depends$()2@references'(_, _):- raise_exception('error'("lo.topsort$depends$()2@references", 24, 5, 23)).
'lo.comp.grapher@showPkg'('()2'(XP, XI)):- ocall('disp%1'(XXV5440),'lo.core$display$lo.repo*pkg','lo.core$display$lo.repo*pkg'),
    ocall('disp%1'(XXV5441),'lo.core$display$lo.core*list'('lo.core$display$lo.repo*pkg'),'lo.core$display$lo.core*list'('lo.core$display$lo.repo*pkg')),
    ocall('_call%2'(XP, XXe5074),XXV5440,XXV5440),
    ocall('_call%2'(XI, XXe5075),XXV5441,XXV5441),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XXe5074, 'lo.core#,..'('lo.core#ss'(" --> "), 'lo.core#,..'(XXe5075, 'lo.core#[]')))), XXd39841),
    'lo.io@logMsg'(XXd39841).
'lo.comp.grapher@showGroup'('lo.core#[]').
'lo.comp.grapher@showGroup'('lo.core#,..'(XP, XL)):- 'lo.comp.grapher@showPkg'(XP),
    'lo.comp.grapher@showGroup'(XL).
'lo.comp.grapher@showGroups'('lo.core#[]').
'lo.comp.grapher@showGroups'('lo.core#,..'(XG, XL)):- 'lo.io@logMsg'("---"),
    'lo.comp.grapher@showGroup'(XG),
    'lo.comp.grapher@showGroups'(XL).
'lo.comp.grapher^scanPkgName'('_call%3'(XV31710, XV31711, XV31712), 'lo.comp.grapher^scanPkgName', _):- 'lo.comp.grapher@scanPkgName'(XV31710, XV31711, XV31712).
'lo.comp.grapher^scanStmt'('_call%3'(XV31713, XV31714, XV31715), 'lo.comp.grapher^scanStmt', _):- 'lo.comp.grapher@scanStmt'(XV31713, XV31714, XV31715).
'lo.comp.grapher^scanTheta'('_call%3'(XV31716, XV31717, XV31718), 'lo.comp.grapher^scanTheta', _):- 'lo.comp.grapher@scanTheta'(XV31716, XV31717, XV31718).
'lo.comp.grapher^scanTerm'('_call%2'(XV31719, XV31720), 'lo.comp.grapher^scanTerm', _):- 'lo.comp.grapher@scanTerm'(XV31719, XV31720).
'lo.comp.grapher^projectOutPkgs'('_call%2'(XV31721, XV31722), 'lo.comp.grapher^projectOutPkgs', _):- 'lo.comp.grapher@projectOutPkgs'(XV31721, XV31722).
'lo.comp.grapher^checkPkg'('_call%7'(XV31723, XV31724, XV31725, XV31726, XV31727, XV31728, XV31729), 'lo.comp.grapher^checkPkg', _):- 'lo.comp.grapher@checkPkg'(XV31723, XV31724, XV31725, XV31726, XV31727, XV31728, XV31729).
'lo.comp.grapher^scanFile'('_call%8'(XV31730, XV31731, XV31732, XV31733, XV31734, XV31735, XV31736, XV31737), 'lo.comp.grapher^scanFile', _):- 'lo.comp.grapher@scanFile'(XV31730, XV31731, XV31732, XV31733, XV31734, XV31735, XV31736, XV31737).
'lo.comp.grapher^scanPkg'('_call%7'(XV31738, XV31739, XV31740, XV31741, XV31742, XV31743, XV31744), 'lo.comp.grapher^scanPkg', _):- 'lo.comp.grapher@scanPkg'(XV31738, XV31739, XV31740, XV31741, XV31742, XV31743, XV31744).
'lo.comp.grapher^scanPkgs'('_call%7'(XV31745, XV31746, XV31747, XV31748, XV31749, XV31750, XV31751), 'lo.comp.grapher^scanPkgs', _):- 'lo.comp.grapher@scanPkgs'(XV31745, XV31746, XV31747, XV31748, XV31749, XV31750, XV31751).
'lo.comp.grapher^makeGraph'('_call%6'(XV31752, XV31753, XV31754, XV31755, XV31756, XV31757), 'lo.comp.grapher^makeGraph', _):- 'lo.comp.grapher@makeGraph'(XV31752, XV31753, XV31754, XV31755, XV31756, XV31757).
'lo.topsort$depends$()2^defines'('_call%4'(XV31758, XV31759, XV31760, XV31761), 'lo.topsort$depends$()2^defines'(XLbV2552, XThV2552), _):- 'lo.topsort$depends$()2@defines'(XV31758, XV31759, XV31760, XV31761, XLbV2552, XThV2552).
'lo.topsort$depends$()2^references'('_call%2'(XV31764, XV31765), 'lo.topsort$depends$()2^references'(XLbV2552, XThV2552), _):- 'lo.topsort$depends$()2@references'(XV31764, XV31765, XLbV2552, XThV2552).
'lo.comp.grapher^showPkg'('_call%1'(XV31768), 'lo.comp.grapher^showPkg', _):- 'lo.comp.grapher@showPkg'(XV31768).
'lo.comp.grapher^showGroup'('_call%1'(XV31769), 'lo.comp.grapher^showGroup', _):- 'lo.comp.grapher@showGroup'(XV31769).
'lo.comp.grapher^showGroups'('_call%1'(XV31770), 'lo.comp.grapher^showGroups', _):- 'lo.comp.grapher@showGroups'(XV31770).
