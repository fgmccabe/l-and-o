'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.freevars's'0.0.1'n5o5'()5'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.canon'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'s\"I1'freeVarsInRule'FT3t'lo.comp.canon*canonRule'Lt'lo.comp.term*term'Lt'lo.comp.term*term'Lt'lo.comp.term*term'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.freevars@init'():- !.
'lo.comp.freevars@filterDef'('lo.comp.canon#funDef'(X_33174, XNm, X_33175, X_33176, X_33177), XVars, XXd38665):- !,
    'lo.list@subtract'('lo.comp.term#varbl'(XNm), XVars, XXd38665).
'lo.comp.freevars@filterDef'('lo.comp.canon#relDef'(X_33178, XNm, X_33179, X_33180, X_33181), XVars, XXd38667):- !,
    'lo.list@subtract'('lo.comp.term#varbl'(XNm), XVars, XXd38667).
'lo.comp.freevars@filterDef'('lo.comp.canon#grammDef'(X_33182, XNm, X_33183, X_33184, X_33185), XVars, XXd38669):- !,
    'lo.list@subtract'('lo.comp.term#varbl'(XNm), XVars, XXd38669).
'lo.comp.freevars@filterDef'('lo.comp.canon#classDef'(X_33186, XNm, X_33187, X_33188, X_33189, X_33190), XVars, XXd38671):- !,
    'lo.list@subtract'('lo.comp.term#varbl'(XNm), XVars, XXd38671).
'lo.comp.freevars@filterDef'('lo.comp.canon#varDef'(X_33191, XNm, X_33192, X_33193, X_33194, X_33195), XVars, XXd38673):- !,
    'lo.list@subtract'('lo.comp.term#varbl'(XNm), XVars, XXd38673).
'lo.comp.freevars@filterDef'(X_33196, XVars, XVars):- !.
'lo.comp.freevars@filterDef'(_, _, _):- raise_exception('error'("lo.comp.freevars@filterDef", 78, 3, 62)).
'lo.comp.freevars@filterDefs'('lo.core#[]', XVars, XVars):- !.
'lo.comp.freevars@filterDefs'('lo.core#,..'(XD, XL), XVars, XXd38675):- !,
    'lo.comp.freevars@filterDef'(XD, XVars, XXd38674),
    'lo.comp.freevars@filterDefs'(XL, XXd38674, XXd38675).
'lo.comp.freevars@filterDefs'(_, _, _):- raise_exception('error'("lo.comp.freevars@filterDefs", 74, 3, 27)).
'lo.comp.freevars@freeVarsInTerminals'('lo.core#[]', X_33198, XF, XF):- !.
'lo.comp.freevars@freeVarsInTerminals'('lo.core#,..'('()3'(X_33200, X_33201, XT), XL), XQ, XF, XXd38677):- !,
    'lo.comp.freevars@freeVars'(XT, XQ, XF, XXd38676),
    'lo.comp.freevars@freeVarsInTerminals'(XL, XQ, XXd38676, XXd38677).
'lo.comp.freevars@freeVarsInTerminals'(_, _, _, _):- raise_exception('error'("lo.comp.freevars@freeVarsInTerminals", 63, 3, 32)).
'lo.comp.freevars@freeVarsList'('lo.core#[]', X_33202, XFV, XFV):- !.
'lo.comp.freevars@freeVarsList'('lo.core#,..'(XT, XL), XQ, XF, XXd38679):- !,
    'lo.comp.freevars@freeVars'(XT, XQ, XF, XXd38678),
    'lo.comp.freevars@freeVarsList'(XL, XQ, XXd38678, XXd38679).
'lo.comp.freevars@freeVarsList'(_, _, _, _):- raise_exception('error'("lo.comp.freevars@freeVarsList", 19, 3, 27)).
'lo.comp.freevars@freeVarsInRules'('lo.core#[]', X_33204, XFV, XFV):- !.
'lo.comp.freevars@freeVarsInRules'('lo.core#,..'(XRl, XL), XQ, XF, XXd38681):- !,
    'lo.comp.freevars@freeVarsInRule'(XRl, XQ, XF, XXd38680),
    'lo.comp.freevars@freeVarsInRules'(XL, XQ, XXd38680, XXd38681).
'lo.comp.freevars@freeVarsInRules'(_, _, _, _):- raise_exception('error'("lo.comp.freevars@freeVarsInRules", 15, 3, 30)).
'lo.comp.freevars@freeVarsInOther'('lo.comp.canon#integrity'(X_33206, XC), XQ, XF, XXd38682):- !,
    'lo.comp.freevars@freeVarsInGoal'(XC, XQ, XF, XXd38682).
'lo.comp.freevars@freeVarsInOther'('lo.comp.canon#expShow'(X_33207, XE), XQ, XF, XXd38683):- !,
    'lo.comp.freevars@freeVars'(XE, XQ, XF, XXd38683).
'lo.comp.freevars@freeVarsInOther'(_, _, _, _):- raise_exception('error'("lo.comp.freevars@freeVarsInOther", 96, 3, 60)).
'lo.comp.freevars@freeVarsInDef'('lo.comp.canon#funDef'(X_33208, X_33209, X_33210, X_33211, XRls), XQ, XF, XXd38684):- !,
    'lo.comp.freevars@freeVarsInRules'(XRls, XQ, XF, XXd38684).
'lo.comp.freevars@freeVarsInDef'('lo.comp.canon#relDef'(X_33212, X_33213, X_33214, X_33215, XRls), XQ, XF, XXd38685):- !,
    'lo.comp.freevars@freeVarsInRules'(XRls, XQ, XF, XXd38685).
'lo.comp.freevars@freeVarsInDef'('lo.comp.canon#grammDef'(X_33216, X_33217, X_33218, X_33219, XRls), XQ, XF, XXd38686):- !,
    'lo.comp.freevars@freeVarsInRules'(XRls, XQ, XF, XXd38686).
'lo.comp.freevars@freeVarsInDef'('lo.comp.canon#classDef'(X_33220, X_33221, X_33222, X_33223, XRls, X_33224), XQ, XF, XXd38687):- !,
    'lo.comp.freevars@freeVarsInRules'(XRls, XQ, XF, XXd38687).
'lo.comp.freevars@freeVarsInDef'('lo.comp.canon#varDef'(X_33225, X_33226, X_33227, X_33228, XL, XC), XQ, XF, XXd38689):- !,
    'lo.comp.freevars@freeVarsInGoal'(XC, XQ, XF, XXd38688),
    'lo.comp.freevars@freeVars'(XL, XQ, XXd38688, XXd38689).
'lo.comp.freevars@freeVarsInDef'('lo.comp.canon#otherDef'(X_33229, XOt), XQ, XF, XXd38690):- !,
    'lo.comp.freevars@freeVarsInOther'(XOt, XQ, XF, XXd38690).
'lo.comp.freevars@freeVarsInDef'('lo.comp.canon#implDef'(X_33230, X_33231, X_33232, X_33233, X_33234, XL, XR, X_33235), XQ, XF, XXd38692):- !,
    'lo.comp.freevars@freeVars'(XL, XQ, XF, XXd38691),
    'lo.comp.freevars@freeVars'(XR, XQ, XXd38691, XXd38692).
'lo.comp.freevars@freeVarsInDef'(X_33236, X_33237, XF, XF):- !.
'lo.comp.freevars@freeVarsInDef'(_, _, _, _):- raise_exception('error'("lo.comp.freevars@freeVarsInDef", 86, 3, 66)).
'lo.comp.freevars@findVarsInDefs'('lo.core#[]', X_33238, XF, XF):- !.
'lo.comp.freevars@findVarsInDefs'('lo.core#,..'(XD, XL), XQ, XF, XXd38694):- !,
    'lo.comp.freevars@freeVarsInDef'(XD, XQ, XF, XXd38693),
    'lo.comp.freevars@findVarsInDefs'(XL, XQ, XXd38693, XXd38694).
'lo.comp.freevars@findVarsInDefs'(_, _, _, _):- raise_exception('error'("lo.comp.freevars@findVarsInDefs", 70, 3, 27)).
'lo.comp.freevars@freeVarsInDefs'(XDefs, XQ, XF, XXd38696):- !,
    'lo.comp.freevars@filterDefs'(XDefs, XQ, XXd38695),
    'lo.comp.freevars@findVarsInDefs'(XDefs, XXd38695, XF, XXd38696).
'lo.comp.freevars@freeVarsInDefs'(_, _, _, _):- raise_exception('error'("lo.comp.freevars@freeVarsInDefs", 67, 3, 69)).
'lo.comp.freevars@freeVars'('lo.comp.canon#v'(X_33240, XLb), XQ, XF, 'lo.core#,..'('lo.comp.term#varbl'(XLb), XF)):- ocall('in%2'('lo.comp.term#varbl'(XLb), XQ),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !.
'lo.comp.freevars@freeVars'('lo.comp.canon#v'(X_33242, X_33243), X_33244, XF, XF):- !.
'lo.comp.freevars@freeVars'('lo.comp.canon#int'(X_33245), X_33246, XF, XF):- !.
'lo.comp.freevars@freeVars'('lo.comp.canon#flt'(X_33247), X_33248, XF, XF):- !.
'lo.comp.freevars@freeVars'('lo.comp.canon#str'(X_33249), X_33250, XF, XF):- !.
'lo.comp.freevars@freeVars'('lo.comp.canon#tpl'(XEls), XQ, XF, XXd38700):- !,
    'lo.comp.freevars@freeVarsList'(XEls, XQ, XF, XXd38700).
'lo.comp.freevars@freeVars'('lo.comp.canon#apply'(X_33251, XOp, XA), XQ, XF, XXd38702):- !,
    'lo.comp.freevars@freeVars'(XA, XQ, XF, XXd38701),
    'lo.comp.freevars@freeVars'(XOp, XQ, XXd38701, XXd38702).
'lo.comp.freevars@freeVars'('lo.comp.canon#dot'(X_33252, XRc, X_33253), XQ, XF, XXd38703):- !,
    'lo.comp.freevars@freeVars'(XRc, XQ, XF, XXd38703).
'lo.comp.freevars@freeVars'('lo.comp.canon#whre'(XT, XC), XQ, XF, XXd38705):- !,
    'lo.comp.freevars@freeVarsInGoal'(XC, XQ, XF, XXd38704),
    'lo.comp.freevars@freeVars'(XT, XQ, XXd38704, XXd38705).
'lo.comp.freevars@freeVars'('lo.comp.canon#condExp'(XC, XT, XE), XQ, XF, XXd38708):- !,
    'lo.comp.freevars@freeVarsInGoal'(XC, XQ, XF, XXd38706),
    'lo.comp.freevars@freeVars'(XE, XQ, XXd38706, XXd38707),
    'lo.comp.freevars@freeVars'(XT, XQ, XXd38707, XXd38708).
'lo.comp.freevars@freeVars'('lo.comp.canon#lambda'(XRl), XQ, XF, XXd38709):- !,
    'lo.comp.freevars@freeVarsInRule'(XRl, XQ, XF, XXd38709).
'lo.comp.freevars@freeVars'('lo.comp.canon#theta'(XDefs, X_33254), XQ, XF, XXd38711):- !,
    'lo.comp.freevars@filterDefs'(XDefs, XQ, XXd38710),
    'lo.comp.freevars@freeVarsInDefs'(XDefs, XXd38710, XF, XXd38711).
'lo.comp.freevars@freeVars'(_, _, _, _):- raise_exception('error'("lo.comp.freevars@freeVars", 23, 3, 58)).
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#trueCond', X_33255, XF, XF):- !.
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#falseCond', X_33256, XF, XF):- !.
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#conjCond'(XL, XR), XQ, XF, XXd38713):- !,
    'lo.comp.freevars@freeVarsInGoal'(XR, XQ, XF, XXd38712),
    'lo.comp.freevars@freeVarsInGoal'(XL, XQ, XXd38712, XXd38713).
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#disjCond'(XL, XR), XQ, XF, XXd38715):- !,
    'lo.comp.freevars@freeVarsInGoal'(XR, XQ, XF, XXd38714),
    'lo.comp.freevars@freeVarsInGoal'(XL, XQ, XXd38714, XXd38715).
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#forallCond'(XL, XR), XQ, XF, XXd38717):- !,
    'lo.comp.freevars@freeVarsInGoal'(XR, XQ, XF, XXd38716),
    'lo.comp.freevars@freeVarsInGoal'(XL, XQ, XXd38716, XXd38717).
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#condCond'(XT, XL, XR), XQ, XF, XXd38720):- !,
    'lo.comp.freevars@freeVarsInGoal'(XR, XQ, XF, XXd38718),
    'lo.comp.freevars@freeVarsInGoal'(XL, XQ, XXd38718, XXd38719),
    'lo.comp.freevars@freeVarsInGoal'(XT, XQ, XXd38719, XXd38720).
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#unifyCond'(X_33257, XL, XR), XQ, XF, XXd38722):- !,
    'lo.comp.freevars@freeVars'(XR, XQ, XF, XXd38721),
    'lo.comp.freevars@freeVars'(XL, XQ, XXd38721, XXd38722).
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#oneCond'(XL), XQ, XF, XXd38723):- !,
    'lo.comp.freevars@freeVarsInGoal'(XL, XQ, XF, XXd38723).
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#negCond'(XL), XQ, XF, XXd38724):- !,
    'lo.comp.freevars@freeVarsInGoal'(XL, XQ, XF, XXd38724).
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#callCond'(X_33258, XOp, XA), XQ, XF, XXd38726):- !,
    'lo.comp.freevars@freeVars'(XA, XQ, XF, XXd38725),
    'lo.comp.freevars@freeVars'(XOp, XQ, XXd38725, XXd38726).
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#isTrue'(XA), XQ, XF, XXd38727):- !,
    'lo.comp.freevars@freeVars'(XA, XQ, XF, XXd38727).
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#phraseCond'(X_33259, XNT, XL, XR), XQ, XF, XXd38730):- !,
    'lo.comp.freevars@freeVars'(XR, XQ, XF, XXd38728),
    'lo.comp.freevars@freeVars'(XL, XQ, XXd38728, XXd38729),
    'lo.comp.freevars@freeVarsInNT'(XNT, XQ, XXd38729, XXd38730).
'lo.comp.freevars@freeVarsInGoal'(_, _, _, _):- raise_exception('error'("lo.comp.freevars@freeVarsInGoal", 37, 3, 33)).
'lo.comp.freevars@freeVarsInNT'('lo.comp.canon#grTerms'(XTerms), XQ, XF, XXd38731):- !,
    'lo.comp.freevars@freeVarsInTerminals'(XTerms, XQ, XF, XXd38731).
'lo.comp.freevars@freeVarsInNT'('lo.comp.canon#grCall'(X_33260, XOp, XA), XQ, XF, XXd38733):- !,
    'lo.comp.freevars@freeVars'(XA, XQ, XF, XXd38732),
    'lo.comp.freevars@freeVars'(XOp, XQ, XXd38732, XXd38733).
'lo.comp.freevars@freeVarsInNT'('lo.comp.canon#grTest'(XC), XQ, XF, XXd38734):- !,
    'lo.comp.freevars@freeVarsInGoal'(XC, XQ, XF, XXd38734).
'lo.comp.freevars@freeVarsInNT'('lo.comp.canon#grDip'(XT, XC), XQ, XF, XXd38736):- !,
    'lo.comp.freevars@freeVars'(XT, XQ, XF, XXd38735),
    'lo.comp.freevars@freeVarsInGoal'(XC, XQ, XXd38735, XXd38736).
'lo.comp.freevars@freeVarsInNT'('lo.comp.canon#grOne'(XC), XQ, XF, XXd38737):- !,
    'lo.comp.freevars@freeVarsInNT'(XC, XQ, XF, XXd38737).
'lo.comp.freevars@freeVarsInNT'('lo.comp.canon#grNeg'(XC), XQ, XF, XXd38738):- !,
    'lo.comp.freevars@freeVarsInNT'(XC, XQ, XF, XXd38738).
'lo.comp.freevars@freeVarsInNT'('lo.comp.canon#grAhed'(XC), XQ, XF, XXd38739):- !,
    'lo.comp.freevars@freeVarsInNT'(XC, XQ, XF, XXd38739).
'lo.comp.freevars@freeVarsInNT'('lo.comp.canon#grDisj'(XL, XR), XQ, XF, XXd38741):- !,
    'lo.comp.freevars@freeVarsInNT'(XL, XQ, XF, XXd38740),
    'lo.comp.freevars@freeVarsInNT'(XR, XQ, XXd38740, XXd38741).
'lo.comp.freevars@freeVarsInNT'('lo.comp.canon#grConj'(XL, XR), XQ, XF, XXd38743):- !,
    'lo.comp.freevars@freeVarsInNT'(XL, XQ, XF, XXd38742),
    'lo.comp.freevars@freeVarsInNT'(XR, XQ, XXd38742, XXd38743).
'lo.comp.freevars@freeVarsInNT'('lo.comp.canon#grCond'(XT, XL, XR), XQ, XF, XXd38746):- !,
    'lo.comp.freevars@freeVarsInNT'(XT, XQ, XF, XXd38744),
    'lo.comp.freevars@freeVarsInNT'(XL, XQ, XXd38744, XXd38745),
    'lo.comp.freevars@freeVarsInNT'(XR, XQ, XXd38745, XXd38746).
'lo.comp.freevars@freeVarsInNT'(_, _, _, _):- raise_exception('error'("lo.comp.freevars@freeVarsInNT", 51, 3, 66)).
'lo.comp.freevars@freeVarsInRule'('lo.comp.canon#equation'(X_33261, X_33262, XA, XExp, XCond), XQ, XF, XXd38749):- !,
    'lo.comp.freevars@freeVars'(XA, XQ, XF, XXd38747),
    'lo.comp.freevars@freeVars'(XExp, XQ, XXd38747, XXd38748),
    'lo.comp.freevars@freeVarsInGoal'(XCond, XQ, XXd38748, XXd38749).
'lo.comp.freevars@freeVarsInRule'('lo.comp.canon#clause'(X_33263, X_33264, XA, XBody), XQ, XF, XXd38751):- !,
    'lo.comp.freevars@freeVars'(XA, XQ, XF, XXd38750),
    'lo.comp.freevars@freeVarsInGoal'(XBody, XQ, XXd38750, XXd38751).
'lo.comp.freevars@freeVarsInRule'('lo.comp.canon#grRule'(X_33265, X_33266, XA, XPB, XBody), XQ, XF, XXd38754):- !,
    'lo.comp.freevars@freeVars'(XA, XQ, XF, XXd38752),
    'lo.comp.freevars@freeVarsInNT'(XBody, XQ, XXd38752, XXd38753),
    'lo.comp.freevars@freeVarsInNT'(XPB, XQ, XXd38753, XXd38754).
'lo.comp.freevars@freeVarsInRule'(_, _, _, _):- raise_exception('error'("lo.comp.freevars@freeVarsInRule", 10, 3, 102)).
'lo.comp.freevars^filterDef'('_call%3'(XV29805, XV29806, XV29807), 'lo.comp.freevars^filterDef', _):- 'lo.comp.freevars@filterDef'(XV29805, XV29806, XV29807).
'lo.comp.freevars^filterDefs'('_call%3'(XV29808, XV29809, XV29810), 'lo.comp.freevars^filterDefs', _):- 'lo.comp.freevars@filterDefs'(XV29808, XV29809, XV29810).
'lo.comp.freevars^freeVarsInTerminals'('_call%4'(XV29811, XV29812, XV29813, XV29814), 'lo.comp.freevars^freeVarsInTerminals', _):- 'lo.comp.freevars@freeVarsInTerminals'(XV29811, XV29812, XV29813, XV29814).
'lo.comp.freevars^freeVarsList'('_call%4'(XV29815, XV29816, XV29817, XV29818), 'lo.comp.freevars^freeVarsList', _):- 'lo.comp.freevars@freeVarsList'(XV29815, XV29816, XV29817, XV29818).
'lo.comp.freevars^freeVarsInRules'('_call%4'(XV29819, XV29820, XV29821, XV29822), 'lo.comp.freevars^freeVarsInRules', _):- 'lo.comp.freevars@freeVarsInRules'(XV29819, XV29820, XV29821, XV29822).
'lo.comp.freevars^freeVarsInOther'('_call%4'(XV29823, XV29824, XV29825, XV29826), 'lo.comp.freevars^freeVarsInOther', _):- 'lo.comp.freevars@freeVarsInOther'(XV29823, XV29824, XV29825, XV29826).
'lo.comp.freevars^freeVarsInDef'('_call%4'(XV29827, XV29828, XV29829, XV29830), 'lo.comp.freevars^freeVarsInDef', _):- 'lo.comp.freevars@freeVarsInDef'(XV29827, XV29828, XV29829, XV29830).
'lo.comp.freevars^findVarsInDefs'('_call%4'(XV29831, XV29832, XV29833, XV29834), 'lo.comp.freevars^findVarsInDefs', _):- 'lo.comp.freevars@findVarsInDefs'(XV29831, XV29832, XV29833, XV29834).
'lo.comp.freevars^freeVarsInDefs'('_call%4'(XV29835, XV29836, XV29837, XV29838), 'lo.comp.freevars^freeVarsInDefs', _):- 'lo.comp.freevars@freeVarsInDefs'(XV29835, XV29836, XV29837, XV29838).
'lo.comp.freevars^freeVars'('_call%4'(XV29839, XV29840, XV29841, XV29842), 'lo.comp.freevars^freeVars', _):- 'lo.comp.freevars@freeVars'(XV29839, XV29840, XV29841, XV29842).
'lo.comp.freevars^freeVarsInGoal'('_call%4'(XV29843, XV29844, XV29845, XV29846), 'lo.comp.freevars^freeVarsInGoal', _):- 'lo.comp.freevars@freeVarsInGoal'(XV29843, XV29844, XV29845, XV29846).
'lo.comp.freevars^freeVarsInNT'('_call%4'(XV29847, XV29848, XV29849, XV29850), 'lo.comp.freevars^freeVarsInNT', _):- 'lo.comp.freevars@freeVarsInNT'(XV29847, XV29848, XV29849, XV29850).
'lo.comp.freevars^freeVarsInRule'('_call%4'(XV29851, XV29852, XV29853, XV29854), 'lo.comp.freevars^freeVarsInRule', _):- 'lo.comp.freevars@freeVarsInRule'(XV29851, XV29852, XV29853, XV29854).
