'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.genprolog'e'*'n15o15'()15'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'n2o2'import'e'public'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.encode'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s'I0's'I0'n0o0'()0'n0o0'()0'n1o1'()1'n2o2'()2's'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string's\"c'lo.coerce$coercion'T2t'lo.comp.term*prProg'ST0\"").
'lo.comp.genprolog@init'() :- !.
'lo.comp.genprolog@appStr'(XS, XOx, XX41025) :- !,
    'explode'(XS, XX41023),
    'lo.list@<>'(XX41023, XOx, XX41025).
'lo.comp.genprolog@appStr'(_, _, _) :- raise_exception('error'("appStr", 60, 3, 30)).
'lo.comp.genprolog@quoteConcat'('lo.core#[]', X_2550, XOx, XOx) :- !.
'lo.comp.genprolog@quoteConcat'('lo.core#,..'(XQt, XL), XQt, XOx, 'lo.core#,..'(92, 'lo.core#,..'(XQt, XX41039))) :- !,
    'lo.comp.genprolog@quoteConcat'(XL, XQt, XOx, XX41039).
'lo.comp.genprolog@quoteConcat'('lo.core#,..'(34, XL), XQt, XOx, 'lo.core#,..'(92, 'lo.core#,..'(34, XX41049))) :- !,
    'lo.comp.genprolog@quoteConcat'(XL, XQt, XOx, XX41049).
'lo.comp.genprolog@quoteConcat'('lo.core#,..'(92, XL), XQt, XOx, 'lo.core#,..'(92, 'lo.core#,..'(92, XX41059))) :- !,
    'lo.comp.genprolog@quoteConcat'(XL, XQt, XOx, XX41059).
'lo.comp.genprolog@quoteConcat'('lo.core#,..'(XC, XL), XQt, XOx, 'lo.core#,..'(XC, XX41071)) :- !,
    'lo.comp.genprolog@quoteConcat'(XL, XQt, XOx, XX41071).
'lo.comp.genprolog@quoteConcat'(_, _, _, _) :- raise_exception('error'("quoteConcat", 69, 3, 26)).
'lo.comp.genprolog@appQuoted'(XS, XQt, XOx, 'lo.core#,..'(XQt, XX41083)) :- !,
    'explode'(XS, XX41078),
    'lo.comp.genprolog@quoteConcat'(XX41078, XQt, 'lo.core#,..'(XQt, XOx), XX41083).
'lo.comp.genprolog@appQuoted'(_, _, _, _) :- raise_exception('error'("appQuoted", 63, 3, 65)).
'lo.comp.genprolog@appId'(XNm, XOx, XX41089) :- !,
    'lo.comp.genprolog@appQuoted'(XNm, 39, XOx, XX41089).
'lo.comp.genprolog@appId'(_, _, _) :- raise_exception('error'("appId", 66, 3, 36)).
'lo.comp.genprolog@convertArg'(XT, XOx, 'lo.core#,..'(44, 'lo.core#,..'(32, XX41094))) :- !,
    'lo.comp.genprolog@convertTerm'(XT, XOx, XX41094).
'lo.comp.genprolog@convertArg'(_, _, _) :- raise_exception('error'("convertArg", 34, 3, 50)).
'lo.comp.genprolog@convertArgs'('lo.core#[]', XOx, XOx) :- !.
'lo.comp.genprolog@convertArgs'('lo.core#,..'(XA, XL), XOx, XX41110) :- !,
    ocall('foldRight%4'('lo.comp.genprolog^convertArg', XOx, XL, XX41108),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    'lo.comp.genprolog@convertTerm'(XA, XX41108, XX41110).
'lo.comp.genprolog@convertArgs'(_, _, _) :- raise_exception('error'("convertArgs", 30, 3, 24)).
'lo.comp.genprolog@convertTerm'('lo.comp.term#anon', XOx, 'lo.core#,..'(95, XOx)) :- !.
'lo.comp.genprolog@convertTerm'('lo.comp.term#varbl'(XNm), XOx, 'lo.core#,..'(88, XX41120)) :- !,
    'lo.comp.genprolog@appStr'(XNm, XOx, XX41120).
'lo.comp.genprolog@convertTerm'('lo.comp.term#intgr'(XIx), XOx, XX41129) :- !,
    ocall('_coerce%2'(XIx, XX41126),'lo.coerce$coercion$lo.core*integer$lo.core*string','lo.coerce$coercion$lo.core*integer$lo.core*string'),
    'lo.comp.genprolog@appStr'(XX41126, XOx, XX41129).
'lo.comp.genprolog@convertTerm'('lo.comp.term#flot'(XDx), XOx, XX41137) :- !,
    ocall('_coerce%2'(XDx, XX41134),'lo.coerce$coercion$lo.core*float$lo.core*string','lo.coerce$coercion$lo.core*float$lo.core*string'),
    'lo.comp.genprolog@appStr'(XX41134, XOx, XX41137).
'lo.comp.genprolog@convertTerm'('lo.comp.term#strng'(XSx), XOx, XX41143) :- !,
    'lo.comp.genprolog@appQuoted'(XSx, 34, XOx, XX41143).
'lo.comp.genprolog@convertTerm'('lo.comp.term#enum'(XEx), XOx, XX41149) :- !,
    'lo.comp.genprolog@appId'(XEx, XOx, XX41149).
'lo.comp.genprolog@convertTerm'('lo.comp.term#strct'(XNm, XAr), XOx, XX41156) :- !,
    'lo.comp.genprolog@appId'(XNm, XOx, XX41156).
'lo.comp.genprolog@convertTerm'('lo.comp.term#prg'(XNm, XAr), XOx, XX41163) :- !,
    'lo.comp.genprolog@appId'(XNm, XOx, XX41163).
'lo.comp.genprolog@convertTerm'('lo.comp.term#cons'(XOp, XArgs), XOx, XX41174) :- !,
    'lo.comp.genprolog@convertArgs'(XArgs, 'lo.core#,..'(41, XOx), XX41172),
    'lo.comp.genprolog@convertTerm'(XOp, 'lo.core#,..'(40, XX41172), XX41174).
'lo.comp.genprolog@convertTerm'(_, _, _) :- raise_exception('error'("convertTerm", 49, 3, 34)).
'lo.comp.genprolog@genPrologSig'(XSpec, 'lo.comp.term#cons'('lo.comp.term#strct'("#pkg", 1), 'lo.core#,..'(XX41178, 'lo.core#[]'))) :- !,
    'lo.comp.encode@packageSig'(XSpec, XX41178).
'lo.comp.genprolog@genPrologSig'(_, _) :- raise_exception('error'("genPrologSig", 17, 3, 62)).
'lo.comp.genprolog@convertCallOp'('lo.comp.term#prg'(XNm, X_2551), XOx, XX41188) :- !,
    'lo.comp.genprolog@appId'(XNm, XOx, XX41188).
'lo.comp.genprolog@convertCallOp'(_, _, _) :- raise_exception('error'("convertCallOp", 27, 3, 43)).
'lo.comp.genprolog@convertHead'(XNm, XArgs, XOx, XX41198) :- !,
    'lo.comp.genprolog@convertArgs'(XArgs, 'lo.core#,..'(41, XOx), XX41196),
    'lo.comp.genprolog@convertCallOp'(XNm, 'lo.core#,..'(40, XX41196), XX41198).
'lo.comp.genprolog@convertHead'(_, _, _, _) :- raise_exception('error'("convertHead", 24, 3, 81)).
'lo.comp.genprolog@convertPred'('lo.comp.term#call'(X_2552, XOp, XArgs), XOx, XX41210) :- !,
    'lo.comp.genprolog@convertArgs'(XArgs, 'lo.core#,..'(41, XOx), XX41208),
    'lo.comp.genprolog@convertCallOp'(XOp, 'lo.core#,..'(40, XX41208), XX41210).
'lo.comp.genprolog@convertPred'('lo.comp.term#ecall'(X_2553, XOp, XArgs), XOx, XX41222) :- !,
    'lo.comp.genprolog@convertArgs'(XArgs, 'lo.core#,..'(41, XOx), XX41220),
    'lo.comp.genprolog@appId'(XOp, 'lo.core#,..'(40, XX41220), XX41222).
'lo.comp.genprolog@convertPred'('lo.comp.term#unfy'(X_2554, XL, XR), XOx, XX41235) :- !,
    'lo.comp.genprolog@convertTerm'(XR, XOx, XX41231),
    'lo.comp.genprolog@convertTerm'(XL, 'lo.core#,..'(32, 'lo.core#,..'(61, 'lo.core#,..'(32, XX41231))), XX41235).
'lo.comp.genprolog@convertPred'('lo.comp.term#ocall'(X_2555, XCall, XLb, XTh), XOx, XX41252) :- !,
    'lo.comp.genprolog@convertTerm'(XTh, 'lo.core#,..'(41, XOx), XX41247),
    'lo.comp.genprolog@convertTerm'(XLb, 'lo.core#,..'(44, XX41247), XX41249),
    'lo.comp.genprolog@convertTerm'(XCall, 'lo.core#,..'(44, XX41249), XX41251),
    'lo.comp.genprolog@appStr'("ocall(", XX41251, XX41252).
'lo.comp.genprolog@convertPred'('lo.comp.term#neck', XOx, 'lo.core#,..'(33, XOx)) :- !.
'lo.comp.genprolog@convertPred'('lo.comp.term#fail', XOx, XX41260) :- !,
    'lo.comp.genprolog@appStr'("fail", XOx, XX41260).
'lo.comp.genprolog@convertPred'('lo.comp.term#except'(X_2556, XE), XOx, XX41270) :- !,
    'lo.comp.genprolog@convertTerm'(XE, 'lo.core#,..'(41, XOx), XX41268),
    'lo.comp.genprolog@appStr'("raise_exception", 'lo.core#,..'(40, XX41268), XX41270).
'lo.comp.genprolog@convertPred'(_, _, _) :- raise_exception('error'("convertPred", 40, 3, 89)).
'lo.comp.genprolog@convertCall'(XC, XOx, 'lo.core#,..'(44, 'lo.core#,..'(10, 'lo.core#,..'(32, 'lo.core#,..'(32, 'lo.core#,..'(32, 'lo.core#,..'(32, XX41275))))))) :- !,
    'lo.comp.genprolog@convertPred'(XC, XOx, XX41275).
'lo.comp.genprolog@convertCall'(_, _, _) :- raise_exception('error'("convertCall", 37, 3, 67)).
'lo.comp.genprolog@convertClause'('lo.comp.term#clse'(X_2557, XNm, XArgs, 'lo.core#[]'), XOx, XX41293) :- !,
    'lo.comp.genprolog@convertHead'(XNm, XArgs, 'lo.core#,..'(46, 'lo.core#,..'(10, XOx)), XX41293).
'lo.comp.genprolog@convertClause'('lo.comp.term#clse'(X_2558, XNm, XArgs, 'lo.core#,..'(XC, XRest)), XOx, XX41316) :- !,
    ocall('foldRight%4'('lo.comp.genprolog^convertCall', 'lo.core#,..'(46, 'lo.core#,..'(10, XOx)), XRest, XX41310),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    'lo.comp.genprolog@convertPred'(XC, XX41310, XX41312),
    'lo.comp.genprolog@convertHead'(XNm, XArgs, 'lo.core#,..'(58, 'lo.core#,..'(45, 'lo.core#,..'(32, XX41312))), XX41316).
'lo.comp.genprolog@convertClause'(_, _, _) :- raise_exception('error'("convertClause", 20, 3, 76)).
'lo.comp.genprolog@prog2code'('lo.comp.term#prProg'(XSpec, XClses), XOx, XX41330) :- !,
    'lo.comp.genprolog@genPrologSig'(XSpec, XX41322),
    ocall('foldRight%4'('lo.comp.genprolog^convertClause', XOx, XClses, XX41326),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    'lo.comp.genprolog@convertTerm'(XX41322, 'lo.core#,..'(46, 'lo.core#,..'(10, XX41326)), XX41330).
'lo.comp.genprolog@prog2code'(_, _, _) :- raise_exception('error'("prog2code", 14, 3, 114)).
'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string'('lo.coerce$coercion$lo.comp.term*prProg$lo.core*string%1'('lo.coerce$coercion$lo.comp.term*prProg$lo.core*string')) :- !.
'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string'('_coerce%2'(XV4974, XV4975), XLbl317, XThis317) :- !,
    'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string@_coerce'(XV4974, XV4975, XLbl317, XThis317).
'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string'('_coerce%1'('lo.coerce$coercion$lo.comp.term*prProg$lo.core*string^_coerce'(XLbl318, XThis318)), XLbl318, XThis318).
'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string@_coerce'(XP, XX41335, XLbV424, XThV424) :- !,
    'lo.comp.genprolog@prog2code'(XP, 'lo.core#[]', XX41334),
    'implode'(XX41334, XX41335).
'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string@_coerce'(_, _, _, _) :- raise_exception('error'("_coerce", 10, 5, 38)).
'lo.comp.genprolog^appStr'('_call%3'(XV4928, XV4929, XV4930), 'lo.comp.genprolog^appStr', _) :- 'lo.comp.genprolog@appStr'(XV4928, XV4929, XV4930).
'lo.comp.genprolog^quoteConcat'('_call%4'(XV4931, XV4932, XV4933, XV4934), 'lo.comp.genprolog^quoteConcat', _) :- 'lo.comp.genprolog@quoteConcat'(XV4931, XV4932, XV4933, XV4934).
'lo.comp.genprolog^appQuoted'('_call%4'(XV4935, XV4936, XV4937, XV4938), 'lo.comp.genprolog^appQuoted', _) :- 'lo.comp.genprolog@appQuoted'(XV4935, XV4936, XV4937, XV4938).
'lo.comp.genprolog^appId'('_call%3'(XV4939, XV4940, XV4941), 'lo.comp.genprolog^appId', _) :- 'lo.comp.genprolog@appId'(XV4939, XV4940, XV4941).
'lo.comp.genprolog^convertArg'('_call%3'(XV4942, XV4943, XV4944), 'lo.comp.genprolog^convertArg', _) :- 'lo.comp.genprolog@convertArg'(XV4942, XV4943, XV4944).
'lo.comp.genprolog^convertArgs'('_call%3'(XV4945, XV4946, XV4947), 'lo.comp.genprolog^convertArgs', _) :- 'lo.comp.genprolog@convertArgs'(XV4945, XV4946, XV4947).
'lo.comp.genprolog^convertTerm'('_call%3'(XV4948, XV4949, XV4950), 'lo.comp.genprolog^convertTerm', _) :- 'lo.comp.genprolog@convertTerm'(XV4948, XV4949, XV4950).
'lo.comp.genprolog^genPrologSig'('_call%2'(XV4951, XV4952), 'lo.comp.genprolog^genPrologSig', _) :- 'lo.comp.genprolog@genPrologSig'(XV4951, XV4952).
'lo.comp.genprolog^convertCallOp'('_call%3'(XV4953, XV4954, XV4955), 'lo.comp.genprolog^convertCallOp', _) :- 'lo.comp.genprolog@convertCallOp'(XV4953, XV4954, XV4955).
'lo.comp.genprolog^convertHead'('_call%4'(XV4956, XV4957, XV4958, XV4959), 'lo.comp.genprolog^convertHead', _) :- 'lo.comp.genprolog@convertHead'(XV4956, XV4957, XV4958, XV4959).
'lo.comp.genprolog^convertPred'('_call%3'(XV4960, XV4961, XV4962), 'lo.comp.genprolog^convertPred', _) :- 'lo.comp.genprolog@convertPred'(XV4960, XV4961, XV4962).
'lo.comp.genprolog^convertCall'('_call%3'(XV4963, XV4964, XV4965), 'lo.comp.genprolog^convertCall', _) :- 'lo.comp.genprolog@convertCall'(XV4963, XV4964, XV4965).
'lo.comp.genprolog^convertClause'('_call%3'(XV4966, XV4967, XV4968), 'lo.comp.genprolog^convertClause', _) :- 'lo.comp.genprolog@convertClause'(XV4966, XV4967, XV4968).
'lo.comp.genprolog^prog2code'('_call%3'(XV4969, XV4970, XV4971), 'lo.comp.genprolog^prog2code', _) :- 'lo.comp.genprolog@prog2code'(XV4969, XV4970, XV4971).
'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string^_coerce'('_call%2'(XV4972, XV4973), 'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string^_coerce'(XLbV424, XThV424), _) :- 'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string@_coerce'(XV4972, XV4973, XLbV424, XThV424).
'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string^_coerce'('_call%2'(XV4976, XV4977), 'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string^_coerce'(XLbV424, XThV424), _) :- 'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string@_coerce'(XV4976, XV4977, XLbV424, XThV424).
