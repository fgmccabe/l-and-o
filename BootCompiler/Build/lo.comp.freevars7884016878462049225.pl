'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.freevars'e'*'n15o15'()15'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'public'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.canon'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I1'freeVarsInRule'FT3t'lo.comp.canon*canonRule'Lt'lo.comp.term*term'Lt'lo.comp.term*term'Lt'lo.comp.term*term'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.freevars@init'() :- !.
'lo.comp.freevars@filterDef'('lo.comp.canon#funDef'(X_443, XNm, X_444, X_445, X_446), XVars, XX5232) :- !,
    'lo.list@subtract'('lo.comp.term#varbl'(XNm), XVars, XX5232).
'lo.comp.freevars@filterDef'('lo.comp.canon#relDef'(X_447, XNm, X_448, X_449, X_450), XVars, XX5243) :- !,
    'lo.list@subtract'('lo.comp.term#varbl'(XNm), XVars, XX5243).
'lo.comp.freevars@filterDef'('lo.comp.canon#grammDef'(X_451, XNm, X_452, X_453, X_454), XVars, XX5254) :- !,
    'lo.list@subtract'('lo.comp.term#varbl'(XNm), XVars, XX5254).
'lo.comp.freevars@filterDef'('lo.comp.canon#classDef'(X_455, XNm, X_456, X_457, X_458, X_459), XVars, XX5266) :- !,
    'lo.list@subtract'('lo.comp.term#varbl'(XNm), XVars, XX5266).
'lo.comp.freevars@filterDef'('lo.comp.canon#varDef'(X_460, XNm, X_461, X_462, X_463, X_464), XVars, XX5278) :- !,
    'lo.list@subtract'('lo.comp.term#varbl'(XNm), XVars, XX5278).
'lo.comp.freevars@filterDef'(X_465, XVars, XVars) :- !.
'lo.comp.freevars@filterDef'(_, _, _) :- raise_exception('error'("filterDef", 78, 3, 62)).
'lo.comp.freevars@filterDefs'('lo.core#[]', XVars, XVars) :- !.
'lo.comp.freevars@filterDefs'('lo.core#,..'(XD, XL), XVars, XX5293) :- !,
    'lo.comp.freevars@filterDef'(XD, XVars, XX5292),
    'lo.comp.freevars@filterDefs'(XL, XX5292, XX5293).
'lo.comp.freevars@filterDefs'(_, _, _) :- raise_exception('error'("filterDefs", 74, 3, 27)).
'lo.comp.freevars@freeVarsInTerminals'('lo.core#[]', X_466, XF, XF) :- !.
'lo.comp.freevars@freeVarsInTerminals'('lo.core#,..'((X_467, X_468, XT), XL), XQ, XF, XX5311) :- !,
    'lo.comp.freevars@freeVars'(XT, XQ, XF, XX5310),
    'lo.comp.freevars@freeVarsInTerminals'(XL, XQ, XX5310, XX5311).
'lo.comp.freevars@freeVarsInTerminals'(_, _, _, _) :- raise_exception('error'("freeVarsInTerminals", 63, 3, 32)).
'lo.comp.freevars@freeVarsInNT'('lo.comp.canon#grTerms'(XTerms), XQ, XF, XX5319) :- !,
    'lo.comp.freevars@freeVarsInTerminals'(XTerms, XQ, XF, XX5319).
'lo.comp.freevars@freeVarsInNT'('lo.comp.canon#grCall'(X_469, XOp, XA), XQ, XF, XX5332) :- !,
    'lo.comp.freevars@freeVars'(XA, XQ, XF, XX5331),
    'lo.comp.freevars@freeVars'(XOp, XQ, XX5331, XX5332).
'lo.comp.freevars@freeVarsInNT'('lo.comp.canon#grTest'(XC), XQ, XF, XX5340) :- !,
    'lo.comp.freevars@freeVarsInGoal'(XC, XQ, XF, XX5340).
'lo.comp.freevars@freeVarsInNT'('lo.comp.canon#grDip'(XT, XC), XQ, XF, XX5352) :- !,
    'lo.comp.freevars@freeVars'(XT, XQ, XF, XX5351),
    'lo.comp.freevars@freeVarsInGoal'(XC, XQ, XX5351, XX5352).
'lo.comp.freevars@freeVarsInNT'('lo.comp.canon#grOne'(XC), XQ, XF, XX5360) :- !,
    'lo.comp.freevars@freeVarsInNT'(XC, XQ, XF, XX5360).
'lo.comp.freevars@freeVarsInNT'('lo.comp.canon#grNeg'(XC), XQ, XF, XX5368) :- !,
    'lo.comp.freevars@freeVarsInNT'(XC, XQ, XF, XX5368).
'lo.comp.freevars@freeVarsInNT'('lo.comp.canon#grAhed'(XC), XQ, XF, XX5376) :- !,
    'lo.comp.freevars@freeVarsInNT'(XC, XQ, XF, XX5376).
'lo.comp.freevars@freeVarsInNT'('lo.comp.canon#grDisj'(XL, XR), XQ, XF, XX5388) :- !,
    'lo.comp.freevars@freeVarsInNT'(XL, XQ, XF, XX5387),
    'lo.comp.freevars@freeVarsInNT'(XR, XQ, XX5387, XX5388).
'lo.comp.freevars@freeVarsInNT'('lo.comp.canon#grConj'(XL, XR), XQ, XF, XX5400) :- !,
    'lo.comp.freevars@freeVarsInNT'(XL, XQ, XF, XX5399),
    'lo.comp.freevars@freeVarsInNT'(XR, XQ, XX5399, XX5400).
'lo.comp.freevars@freeVarsInNT'('lo.comp.canon#grCond'(XT, XL, XR), XQ, XF, XX5416) :- !,
    'lo.comp.freevars@freeVarsInNT'(XT, XQ, XF, XX5414),
    'lo.comp.freevars@freeVarsInNT'(XL, XQ, XX5414, XX5415),
    'lo.comp.freevars@freeVarsInNT'(XR, XQ, XX5415, XX5416).
'lo.comp.freevars@freeVarsInNT'(_, _, _, _) :- raise_exception('error'("freeVarsInNT", 51, 3, 66)).
'lo.comp.freevars@freeVarsInOther'('lo.comp.canon#integrity'(X_470, XC), XQ, XF, XX5425) :- !,
    'lo.comp.freevars@freeVarsInGoal'(XC, XQ, XF, XX5425).
'lo.comp.freevars@freeVarsInOther'('lo.comp.canon#expShow'(X_471, XE), XQ, XF, XX5434) :- !,
    'lo.comp.freevars@freeVars'(XE, XQ, XF, XX5434).
'lo.comp.freevars@freeVarsInOther'(_, _, _, _) :- raise_exception('error'("freeVarsInOther", 96, 3, 60)).
'lo.comp.freevars@freeVarsInRules'('lo.core#[]', X_472, XFV, XFV) :- !.
'lo.comp.freevars@freeVarsInRules'('lo.core#,..'(XRl, XL), XQ, XF, XX5450) :- !,
    'lo.comp.freevars@freeVarsInRule'(XRl, XQ, XF, XX5449),
    'lo.comp.freevars@freeVarsInRules'(XL, XQ, XX5449, XX5450).
'lo.comp.freevars@freeVarsInRules'(_, _, _, _) :- raise_exception('error'("freeVarsInRules", 15, 3, 30)).
'lo.comp.freevars@freeVarsInDef'('lo.comp.canon#funDef'(X_473, X_474, X_475, X_476, XRls), XQ, XF, XX5462) :- !,
    'lo.comp.freevars@freeVarsInRules'(XRls, XQ, XF, XX5462).
'lo.comp.freevars@freeVarsInDef'('lo.comp.canon#relDef'(X_477, X_478, X_479, X_480, XRls), XQ, XF, XX5474) :- !,
    'lo.comp.freevars@freeVarsInRules'(XRls, XQ, XF, XX5474).
'lo.comp.freevars@freeVarsInDef'('lo.comp.canon#grammDef'(X_481, X_482, X_483, X_484, XRls), XQ, XF, XX5486) :- !,
    'lo.comp.freevars@freeVarsInRules'(XRls, XQ, XF, XX5486).
'lo.comp.freevars@freeVarsInDef'('lo.comp.canon#classDef'(X_485, X_486, X_487, X_488, XRls, X_489), XQ, XF, XX5499) :- !,
    'lo.comp.freevars@freeVarsInRules'(XRls, XQ, XF, XX5499).
'lo.comp.freevars@freeVarsInDef'('lo.comp.canon#varDef'(X_490, X_491, X_492, X_493, XL, XC), XQ, XF, XX5515) :- !,
    'lo.comp.freevars@freeVarsInGoal'(XC, XQ, XF, XX5514),
    'lo.comp.freevars@freeVars'(XL, XQ, XX5514, XX5515).
'lo.comp.freevars@freeVarsInDef'('lo.comp.canon#otherDef'(X_494, XOt), XQ, XF, XX5524) :- !,
    'lo.comp.freevars@freeVarsInOther'(XOt, XQ, XF, XX5524).
'lo.comp.freevars@freeVarsInDef'('lo.comp.canon#implDef'(X_495, X_496, X_497, X_498, X_499, XL, XR, X_500), XQ, XF, XX5542) :- !,
    'lo.comp.freevars@freeVars'(XL, XQ, XF, XX5541),
    'lo.comp.freevars@freeVars'(XR, XQ, XX5541, XX5542).
'lo.comp.freevars@freeVarsInDef'(X_501, X_502, XF, XF) :- !.
'lo.comp.freevars@freeVarsInDef'(_, _, _, _) :- raise_exception('error'("freeVarsInDef", 86, 3, 66)).
'lo.comp.freevars@findVarsInDefs'('lo.core#[]', X_503, XF, XF) :- !.
'lo.comp.freevars@findVarsInDefs'('lo.core#,..'(XD, XL), XQ, XF, XX5562) :- !,
    'lo.comp.freevars@freeVarsInDef'(XD, XQ, XF, XX5561),
    'lo.comp.freevars@findVarsInDefs'(XL, XQ, XX5561, XX5562).
'lo.comp.freevars@findVarsInDefs'(_, _, _, _) :- raise_exception('error'("findVarsInDefs", 70, 3, 27)).
'lo.comp.freevars@freeVarsInDefs'(XDefs, XQ, XF, XX5571) :- !,
    'lo.comp.freevars@filterDefs'(XDefs, XQ, XX5569),
    'lo.comp.freevars@findVarsInDefs'(XDefs, XX5569, XF, XX5571).
'lo.comp.freevars@freeVarsInDefs'(_, _, _, _) :- raise_exception('error'("freeVarsInDefs", 67, 3, 69)).
'lo.comp.freevars@freeVarsList'('lo.core#[]', X_504, XFV, XFV) :- !.
'lo.comp.freevars@freeVarsList'('lo.core#,..'(XT, XL), XQ, XF, XX5587) :- !,
    'lo.comp.freevars@freeVars'(XT, XQ, XF, XX5586),
    'lo.comp.freevars@freeVarsList'(XL, XQ, XX5586, XX5587).
'lo.comp.freevars@freeVarsList'(_, _, _, _) :- raise_exception('error'("freeVarsList", 19, 3, 27)).
'lo.comp.freevars@freeVars'('lo.comp.canon#v'(X_505, XLb), XQ, XF, 'lo.core#,..'('lo.comp.term#varbl'(XLb), XF)) :- ocall('in%2'('lo.comp.term#varbl'(XLb), XQ),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !.
'lo.comp.freevars@freeVars'('lo.comp.canon#v'(X_506, X_507), X_508, XF, XF) :- !.
'lo.comp.freevars@freeVars'('lo.comp.canon#int'(X_509), X_510, XF, XF) :- !.
'lo.comp.freevars@freeVars'('lo.comp.canon#flt'(X_511), X_512, XF, XF) :- !.
'lo.comp.freevars@freeVars'('lo.comp.canon#str'(X_513), X_514, XF, XF) :- !.
'lo.comp.freevars@freeVars'('lo.comp.canon#tpl'(XEls), XQ, XF, XX5629) :- !,
    'lo.comp.freevars@freeVarsList'(XEls, XQ, XF, XX5629).
'lo.comp.freevars@freeVars'('lo.comp.canon#apply'(X_515, XOp, XA), XQ, XF, XX5642) :- !,
    'lo.comp.freevars@freeVars'(XA, XQ, XF, XX5641),
    'lo.comp.freevars@freeVars'(XOp, XQ, XX5641, XX5642).
'lo.comp.freevars@freeVars'('lo.comp.canon#dot'(X_516, XRc, X_517), XQ, XF, XX5652) :- !,
    'lo.comp.freevars@freeVars'(XRc, XQ, XF, XX5652).
'lo.comp.freevars@freeVars'('lo.comp.canon#whre'(XT, XC), XQ, XF, XX5664) :- !,
    'lo.comp.freevars@freeVarsInGoal'(XC, XQ, XF, XX5663),
    'lo.comp.freevars@freeVars'(XT, XQ, XX5663, XX5664).
'lo.comp.freevars@freeVars'('lo.comp.canon#condExp'(XC, XT, XE), XQ, XF, XX5680) :- !,
    'lo.comp.freevars@freeVarsInGoal'(XC, XQ, XF, XX5678),
    'lo.comp.freevars@freeVars'(XE, XQ, XX5678, XX5679),
    'lo.comp.freevars@freeVars'(XT, XQ, XX5679, XX5680).
'lo.comp.freevars@freeVars'('lo.comp.canon#lambda'(XRl), XQ, XF, XX5688) :- !,
    'lo.comp.freevars@freeVarsInRule'(XRl, XQ, XF, XX5688).
'lo.comp.freevars@freeVars'('lo.comp.canon#theta'(XDefs, X_518), XQ, XF, XX5699) :- !,
    'lo.comp.freevars@filterDefs'(XDefs, XQ, XX5697),
    'lo.comp.freevars@freeVarsInDefs'(XDefs, XX5697, XF, XX5699).
'lo.comp.freevars@freeVars'(_, _, _, _) :- raise_exception('error'("freeVars", 23, 3, 58)).
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#trueCond', X_519, XF, XF) :- !.
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#falseCond', X_520, XF, XF) :- !.
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#conjCond'(XL, XR), XQ, XF, XX5719) :- !,
    'lo.comp.freevars@freeVarsInGoal'(XR, XQ, XF, XX5718),
    'lo.comp.freevars@freeVarsInGoal'(XL, XQ, XX5718, XX5719).
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#disjCond'(XL, XR), XQ, XF, XX5731) :- !,
    'lo.comp.freevars@freeVarsInGoal'(XR, XQ, XF, XX5730),
    'lo.comp.freevars@freeVarsInGoal'(XL, XQ, XX5730, XX5731).
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#forallCond'(XL, XR), XQ, XF, XX5743) :- !,
    'lo.comp.freevars@freeVarsInGoal'(XR, XQ, XF, XX5742),
    'lo.comp.freevars@freeVarsInGoal'(XL, XQ, XX5742, XX5743).
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#condCond'(XT, XL, XR), XQ, XF, XX5759) :- !,
    'lo.comp.freevars@freeVarsInGoal'(XR, XQ, XF, XX5757),
    'lo.comp.freevars@freeVarsInGoal'(XL, XQ, XX5757, XX5758),
    'lo.comp.freevars@freeVarsInGoal'(XT, XQ, XX5758, XX5759).
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#unifyCond'(X_521, XL, XR), XQ, XF, XX5772) :- !,
    'lo.comp.freevars@freeVars'(XR, XQ, XF, XX5771),
    'lo.comp.freevars@freeVars'(XL, XQ, XX5771, XX5772).
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#oneCond'(XL), XQ, XF, XX5780) :- !,
    'lo.comp.freevars@freeVarsInGoal'(XL, XQ, XF, XX5780).
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#negCond'(XL), XQ, XF, XX5788) :- !,
    'lo.comp.freevars@freeVarsInGoal'(XL, XQ, XF, XX5788).
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#callCond'(X_522, XOp, XA), XQ, XF, XX5801) :- !,
    'lo.comp.freevars@freeVars'(XA, XQ, XF, XX5800),
    'lo.comp.freevars@freeVars'(XOp, XQ, XX5800, XX5801).
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#isTrue'(XA), XQ, XF, XX5809) :- !,
    'lo.comp.freevars@freeVars'(XA, XQ, XF, XX5809).
'lo.comp.freevars@freeVarsInGoal'('lo.comp.canon#phraseCond'(X_523, XNT, XL, XR), XQ, XF, XX5826) :- !,
    'lo.comp.freevars@freeVars'(XR, XQ, XF, XX5824),
    'lo.comp.freevars@freeVars'(XL, XQ, XX5824, XX5825),
    'lo.comp.freevars@freeVarsInNT'(XNT, XQ, XX5825, XX5826).
'lo.comp.freevars@freeVarsInGoal'(_, _, _, _) :- raise_exception('error'("freeVarsInGoal", 37, 3, 33)).
'lo.comp.freevars@freeVarsInRule'('lo.comp.canon#equation'(X_524, X_525, XA, XExp, XCond), XQ, XF, XX5844) :- !,
    'lo.comp.freevars@freeVars'(XA, XQ, XF, XX5842),
    'lo.comp.freevars@freeVars'(XExp, XQ, XX5842, XX5843),
    'lo.comp.freevars@freeVarsInGoal'(XCond, XQ, XX5843, XX5844).
'lo.comp.freevars@freeVarsInRule'('lo.comp.canon#clause'(X_526, X_527, XA, XBody), XQ, XF, XX5858) :- !,
    'lo.comp.freevars@freeVars'(XA, XQ, XF, XX5857),
    'lo.comp.freevars@freeVarsInGoal'(XBody, XQ, XX5857, XX5858).
'lo.comp.freevars@freeVarsInRule'('lo.comp.canon#grRule'(X_528, X_529, XA, XPB, XBody), XQ, XF, XX5876) :- !,
    'lo.comp.freevars@freeVars'(XA, XQ, XF, XX5874),
    'lo.comp.freevars@freeVarsInNT'(XBody, XQ, XX5874, XX5875),
    'lo.comp.freevars@freeVarsInNT'(XPB, XQ, XX5875, XX5876).
'lo.comp.freevars@freeVarsInRule'(_, _, _, _) :- raise_exception('error'("freeVarsInRule", 10, 3, 102)).
'lo.comp.freevars^filterDef'('_call%3'(XV962, XV963, XV964), 'lo.comp.freevars^filterDef', _) :- 'lo.comp.freevars@filterDef'(XV962, XV963, XV964).
'lo.comp.freevars^filterDefs'('_call%3'(XV965, XV966, XV967), 'lo.comp.freevars^filterDefs', _) :- 'lo.comp.freevars@filterDefs'(XV965, XV966, XV967).
'lo.comp.freevars^freeVarsInTerminals'('_call%4'(XV968, XV969, XV970, XV971), 'lo.comp.freevars^freeVarsInTerminals', _) :- 'lo.comp.freevars@freeVarsInTerminals'(XV968, XV969, XV970, XV971).
'lo.comp.freevars^freeVarsInNT'('_call%4'(XV972, XV973, XV974, XV975), 'lo.comp.freevars^freeVarsInNT', _) :- 'lo.comp.freevars@freeVarsInNT'(XV972, XV973, XV974, XV975).
'lo.comp.freevars^freeVarsInOther'('_call%4'(XV976, XV977, XV978, XV979), 'lo.comp.freevars^freeVarsInOther', _) :- 'lo.comp.freevars@freeVarsInOther'(XV976, XV977, XV978, XV979).
'lo.comp.freevars^freeVarsInRules'('_call%4'(XV980, XV981, XV982, XV983), 'lo.comp.freevars^freeVarsInRules', _) :- 'lo.comp.freevars@freeVarsInRules'(XV980, XV981, XV982, XV983).
'lo.comp.freevars^freeVarsInDef'('_call%4'(XV984, XV985, XV986, XV987), 'lo.comp.freevars^freeVarsInDef', _) :- 'lo.comp.freevars@freeVarsInDef'(XV984, XV985, XV986, XV987).
'lo.comp.freevars^findVarsInDefs'('_call%4'(XV988, XV989, XV990, XV991), 'lo.comp.freevars^findVarsInDefs', _) :- 'lo.comp.freevars@findVarsInDefs'(XV988, XV989, XV990, XV991).
'lo.comp.freevars^freeVarsInDefs'('_call%4'(XV992, XV993, XV994, XV995), 'lo.comp.freevars^freeVarsInDefs', _) :- 'lo.comp.freevars@freeVarsInDefs'(XV992, XV993, XV994, XV995).
'lo.comp.freevars^freeVarsList'('_call%4'(XV996, XV997, XV998, XV999), 'lo.comp.freevars^freeVarsList', _) :- 'lo.comp.freevars@freeVarsList'(XV996, XV997, XV998, XV999).
'lo.comp.freevars^freeVars'('_call%4'(XV1000, XV1001, XV1002, XV1003), 'lo.comp.freevars^freeVars', _) :- 'lo.comp.freevars@freeVars'(XV1000, XV1001, XV1002, XV1003).
'lo.comp.freevars^freeVarsInGoal'('_call%4'(XV1004, XV1005, XV1006, XV1007), 'lo.comp.freevars^freeVarsInGoal', _) :- 'lo.comp.freevars@freeVarsInGoal'(XV1004, XV1005, XV1006, XV1007).
'lo.comp.freevars^freeVarsInRule'('_call%4'(XV1008, XV1009, XV1010, XV1011), 'lo.comp.freevars^freeVarsInRule', _) :- 'lo.comp.freevars@freeVarsInRule'(XV1008, XV1009, XV1010, XV1011).
