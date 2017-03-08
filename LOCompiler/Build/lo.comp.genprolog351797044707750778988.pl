'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.genprolog's'0.0.1'n5o5'()5'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.encode'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'s'I0's'I0'n0o0'()0'n0o0'()0'n1o1'()1'n2o2'()2's'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string's\"c'lo.coerce$coercion'T2t'lo.comp.term*prProg'ST0\"").
'lo.comp.genprolog@init'():- !.
'lo.comp.genprolog@appStr'(XS, XOx, XXd41080):- !,
    'explode'(XS, XXc544),
    'lo.list@<>'(XXc544, XOx, XXd41080).
'lo.comp.genprolog@appStr'(_, _, _):- raise_exception('error'("lo.comp.genprolog@appStr", 60, 3, 30)).
'lo.comp.genprolog@quoteConcat'('lo.core#[]', X_36246, XOx, XOx):- !.
'lo.comp.genprolog@quoteConcat'('lo.core#,..'(XQt, XL), XQt, XOx, 'lo.core#,..'(92, 'lo.core#,..'(XQt, XXd41081))):- !,
    'lo.comp.genprolog@quoteConcat'(XL, XQt, XOx, XXd41081).
'lo.comp.genprolog@quoteConcat'('lo.core#,..'(34, XL), XQt, XOx, 'lo.core#,..'(92, 'lo.core#,..'(34, XXd41084))):- !,
    'lo.comp.genprolog@quoteConcat'(XL, XQt, XOx, XXd41084).
'lo.comp.genprolog@quoteConcat'('lo.core#,..'(92, XL), XQt, XOx, 'lo.core#,..'(92, 'lo.core#,..'(92, XXd41087))):- !,
    'lo.comp.genprolog@quoteConcat'(XL, XQt, XOx, XXd41087).
'lo.comp.genprolog@quoteConcat'('lo.core#,..'(XC, XL), XQt, XOx, 'lo.core#,..'(XC, XXd41090)):- !,
    'lo.comp.genprolog@quoteConcat'(XL, XQt, XOx, XXd41090).
'lo.comp.genprolog@quoteConcat'(_, _, _, _):- raise_exception('error'("lo.comp.genprolog@quoteConcat", 69, 3, 26)).
'lo.comp.genprolog@appQuoted'(XS, XQt, XOx, 'lo.core#,..'(XQt, XXd41093)):- !,
    'explode'(XS, XXc545),
    'lo.comp.genprolog@quoteConcat'(XXc545, XQt, 'lo.core#,..'(XQt, XOx), XXd41093).
'lo.comp.genprolog@appQuoted'(_, _, _, _):- raise_exception('error'("lo.comp.genprolog@appQuoted", 63, 3, 65)).
'lo.comp.genprolog@appId'(XNm, XOx, XXd41095):- !,
    'lo.comp.genprolog@appQuoted'(XNm, 39, XOx, XXd41095).
'lo.comp.genprolog@appId'(_, _, _):- raise_exception('error'("lo.comp.genprolog@appId", 66, 3, 36)).
'lo.comp.genprolog@convertArg'(XT, XOx, 'lo.core#,..'(44, 'lo.core#,..'(32, XXd41096))):- !,
    'lo.comp.genprolog@convertTerm'(XT, XOx, XXd41096).
'lo.comp.genprolog@convertArg'(_, _, _):- raise_exception('error'("lo.comp.genprolog@convertArg", 34, 3, 50)).
'lo.comp.genprolog@convertArgs'('lo.core#[]', XOx, XOx):- !.
'lo.comp.genprolog@convertArgs'('lo.core#,..'(XA, XL), XOx, XXd41099):- !,
    ocall('foldRight%1'(XXV5568),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_call%4'('lo.comp.genprolog^convertArg', XOx, XL, XXe5168),XXV5568,XXV5568),
    'lo.comp.genprolog@convertTerm'(XA, XXe5168, XXd41099).
'lo.comp.genprolog@convertArgs'(_, _, _):- raise_exception('error'("lo.comp.genprolog@convertArgs", 30, 3, 24)).
'lo.comp.genprolog@convertTerm'('lo.comp.term#anon', XOx, 'lo.core#,..'(95, XOx)):- !.
'lo.comp.genprolog@convertTerm'('lo.comp.term#varbl'(XNm), XOx, 'lo.core#,..'(88, XXd41101)):- !,
    'lo.comp.genprolog@appStr'(XNm, XOx, XXd41101).
'lo.comp.genprolog@convertTerm'('lo.comp.term#intgr'(XIx), XOx, XXd41103):- !,
    ocall('_coerce%1'(XXV5569),'lo.coerce$coercion$lo.core*integer$lo.core*string','lo.coerce$coercion$lo.core*integer$lo.core*string'),
    ocall('_call%2'(XIx, XXe5169),XXV5569,XXV5569),
    'lo.comp.genprolog@appStr'(XXe5169, XOx, XXd41103).
'lo.comp.genprolog@convertTerm'('lo.comp.term#flot'(XDx), XOx, XXd41104):- !,
    ocall('_coerce%1'(XXV5570),'lo.coerce$coercion$lo.core*float$lo.core*string','lo.coerce$coercion$lo.core*float$lo.core*string'),
    ocall('_call%2'(XDx, XXe5170),XXV5570,XXV5570),
    'lo.comp.genprolog@appStr'(XXe5170, XOx, XXd41104).
'lo.comp.genprolog@convertTerm'('lo.comp.term#strng'(XSx), XOx, XXd41105):- !,
    'lo.comp.genprolog@appQuoted'(XSx, 34, XOx, XXd41105).
'lo.comp.genprolog@convertTerm'('lo.comp.term#enum'(XEx), XOx, XXd41106):- !,
    'lo.comp.genprolog@appId'(XEx, XOx, XXd41106).
'lo.comp.genprolog@convertTerm'('lo.comp.term#strct'(XNm, XAr), XOx, XXd41107):- !,
    'lo.comp.genprolog@appId'(XNm, XOx, XXd41107).
'lo.comp.genprolog@convertTerm'('lo.comp.term#prg'(XNm, XAr), XOx, XXd41108):- !,
    'lo.comp.genprolog@appId'(XNm, XOx, XXd41108).
'lo.comp.genprolog@convertTerm'('lo.comp.term#cons'(XOp, XArgs), XOx, XXd41112):- !,
    'lo.comp.genprolog@convertArgs'(XArgs, 'lo.core#,..'(41, XOx), XXd41110),
    'lo.comp.genprolog@convertTerm'(XOp, 'lo.core#,..'(40, XXd41110), XXd41112).
'lo.comp.genprolog@convertTerm'(_, _, _):- raise_exception('error'("lo.comp.genprolog@convertTerm", 49, 3, 34)).
'lo.comp.genprolog@convertCallOp'('lo.comp.term#prg'(XNm, X_36267), XOx, XXd41113):- !,
    'lo.comp.genprolog@appId'(XNm, XOx, XXd41113).
'lo.comp.genprolog@convertCallOp'(_, _, _):- raise_exception('error'("lo.comp.genprolog@convertCallOp", 27, 3, 43)).
'lo.comp.genprolog@convertPred'('lo.comp.term#call'(X_36268, XOp, XArgs), XOx, XXd41117):- !,
    'lo.comp.genprolog@convertArgs'(XArgs, 'lo.core#,..'(41, XOx), XXd41115),
    'lo.comp.genprolog@convertCallOp'(XOp, 'lo.core#,..'(40, XXd41115), XXd41117).
'lo.comp.genprolog@convertPred'('lo.comp.term#ecall'(X_36271, XOp, XArgs), XOx, XXd41121):- !,
    'lo.comp.genprolog@convertArgs'(XArgs, 'lo.core#,..'(41, XOx), XXd41119),
    'lo.comp.genprolog@appId'(XOp, 'lo.core#,..'(40, XXd41119), XXd41121).
'lo.comp.genprolog@convertPred'('lo.comp.term#unfy'(X_36274, XL, XR), XOx, XXd41126):- !,
    'lo.comp.genprolog@convertTerm'(XR, XOx, XXd41122),
    'lo.comp.genprolog@convertTerm'(XL, 'lo.core#,..'(32, 'lo.core#,..'(61, 'lo.core#,..'(32, XXd41122))), XXd41126).
'lo.comp.genprolog@convertPred'('lo.comp.term#ocall'(X_36278, XCall, XLb, XTh), XOx, XXd41133):- !,
    'lo.comp.genprolog@convertTerm'(XTh, 'lo.core#,..'(41, XOx), XXd41128),
    'lo.comp.genprolog@convertTerm'(XLb, 'lo.core#,..'(44, XXd41128), XXd41130),
    'lo.comp.genprolog@convertTerm'(XCall, 'lo.core#,..'(44, XXd41130), XXd41132),
    'lo.comp.genprolog@appStr'("ocall(", XXd41132, XXd41133).
'lo.comp.genprolog@convertPred'('lo.comp.term#neck', XOx, 'lo.core#,..'(33, XOx)):- !.
'lo.comp.genprolog@convertPred'('lo.comp.term#fail', XOx, XXd41135):- !,
    'lo.comp.genprolog@appStr'("fail", XOx, XXd41135).
'lo.comp.genprolog@convertPred'('lo.comp.term#except'(X_36283, XE), XOx, XXd41139):- !,
    'lo.comp.genprolog@convertTerm'(XE, 'lo.core#,..'(41, XOx), XXd41137),
    'lo.comp.genprolog@appStr'("raise_exception", 'lo.core#,..'(40, XXd41137), XXd41139).
'lo.comp.genprolog@convertPred'(_, _, _):- raise_exception('error'("lo.comp.genprolog@convertPred", 40, 3, 89)).
'lo.comp.genprolog@convertCall'(XC, XOx, 'lo.core#,..'(44, 'lo.core#,..'(10, 'lo.core#,..'(32, 'lo.core#,..'(32, 'lo.core#,..'(32, 'lo.core#,..'(32, XXd41140))))))):- !,
    'lo.comp.genprolog@convertPred'(XC, XOx, XXd41140).
'lo.comp.genprolog@convertCall'(_, _, _):- raise_exception('error'("lo.comp.genprolog@convertCall", 37, 3, 67)).
'lo.comp.genprolog@convertHead'(XNm, XArgs, XOx, XXd41150):- !,
    'lo.comp.genprolog@convertArgs'(XArgs, 'lo.core#,..'(41, XOx), XXd41148),
    'lo.comp.genprolog@convertCallOp'(XNm, 'lo.core#,..'(40, XXd41148), XXd41150).
'lo.comp.genprolog@convertHead'(_, _, _, _):- raise_exception('error'("lo.comp.genprolog@convertHead", 24, 3, 81)).
'lo.comp.genprolog@convertClause'('lo.comp.term#clse'(X_36294, XNm, XArgs, 'lo.core#[]'), XOx, XXd41153):- !,
    'lo.comp.genprolog@convertHead'(XNm, XArgs, 'lo.core#,..'(46, 'lo.core#,..'(10, XOx)), XXd41153).
'lo.comp.genprolog@convertClause'('lo.comp.term#clse'(X_36297, XNm, XArgs, 'lo.core#,..'(XC, XRest)), XOx, XXd41160):- !,
    ocall('foldRight%1'(XXV5571),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_call%4'('lo.comp.genprolog^convertCall', 'lo.core#,..'(46, 'lo.core#,..'(10, XOx)), XRest, XXe5171),XXV5571,XXV5571),
    'lo.comp.genprolog@convertPred'(XC, XXe5171, XXd41156),
    'lo.comp.genprolog@convertHead'(XNm, XArgs, 'lo.core#,..'(58, 'lo.core#,..'(45, 'lo.core#,..'(32, XXd41156))), XXd41160).
'lo.comp.genprolog@convertClause'(_, _, _):- raise_exception('error'("lo.comp.genprolog@convertClause", 20, 3, 76)).
'lo.comp.genprolog@genPrologSig'(XSpec, 'lo.comp.term#cons'('lo.comp.term#strct'("#pkg", 1), 'lo.core#,..'(XXd41162, 'lo.core#[]'))):- !,
    'lo.comp.encode@packageSig'(XSpec, XXd41162).
'lo.comp.genprolog@genPrologSig'(_, _):- raise_exception('error'("lo.comp.genprolog@genPrologSig", 17, 3, 62)).
'lo.comp.genprolog@prog2code'('lo.comp.term#prProg'(XSpec, XClses), XOx, XXd41168):- !,
    ocall('foldRight%1'(XXV5572),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    'lo.comp.genprolog@genPrologSig'(XSpec, XXd41165),
    ocall('_call%4'('lo.comp.genprolog^convertClause', XOx, XClses, XXe5172),XXV5572,XXV5572),
    'lo.comp.genprolog@convertTerm'(XXd41165, 'lo.core#,..'(46, 'lo.core#,..'(10, XXe5172)), XXd41168).
'lo.comp.genprolog@prog2code'(_, _, _):- raise_exception('error'("lo.comp.genprolog@prog2code", 14, 3, 114)).
'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string'('lo.coerce$coercion$lo.comp.term*prProg$lo.core*string%1'('lo.coerce$coercion$lo.comp.term*prProg$lo.core*string')):- !.
'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string'('_coerce%2'(XV33130, XV33131), XLbl2231, XThis2231):- !,
    'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string@_coerce'(XV33130, XV33131, XLbl2231, XThis2231).
'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string'('_coerce%1'('lo.coerce$coercion$lo.comp.term*prProg$lo.core*string^_coerce'(XLbl2232, XThis2232)), XLbl2232, XThis2232).
'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string@_coerce'(XP, XXc546, XLbV2560, XThV2560):- !,
    'lo.comp.genprolog@prog2code'(XP, 'lo.core#[]', XXd41169),
    'implode'(XXd41169, XXc546).
'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.comp.term*prProg$lo.core*string@_coerce", 10, 5, 38)).
'lo.comp.genprolog^appStr'('_call%3'(XV33084, XV33085, XV33086), 'lo.comp.genprolog^appStr', _):- 'lo.comp.genprolog@appStr'(XV33084, XV33085, XV33086).
'lo.comp.genprolog^quoteConcat'('_call%4'(XV33087, XV33088, XV33089, XV33090), 'lo.comp.genprolog^quoteConcat', _):- 'lo.comp.genprolog@quoteConcat'(XV33087, XV33088, XV33089, XV33090).
'lo.comp.genprolog^appQuoted'('_call%4'(XV33091, XV33092, XV33093, XV33094), 'lo.comp.genprolog^appQuoted', _):- 'lo.comp.genprolog@appQuoted'(XV33091, XV33092, XV33093, XV33094).
'lo.comp.genprolog^appId'('_call%3'(XV33095, XV33096, XV33097), 'lo.comp.genprolog^appId', _):- 'lo.comp.genprolog@appId'(XV33095, XV33096, XV33097).
'lo.comp.genprolog^convertArg'('_call%3'(XV33098, XV33099, XV33100), 'lo.comp.genprolog^convertArg', _):- 'lo.comp.genprolog@convertArg'(XV33098, XV33099, XV33100).
'lo.comp.genprolog^convertArgs'('_call%3'(XV33101, XV33102, XV33103), 'lo.comp.genprolog^convertArgs', _):- 'lo.comp.genprolog@convertArgs'(XV33101, XV33102, XV33103).
'lo.comp.genprolog^convertTerm'('_call%3'(XV33104, XV33105, XV33106), 'lo.comp.genprolog^convertTerm', _):- 'lo.comp.genprolog@convertTerm'(XV33104, XV33105, XV33106).
'lo.comp.genprolog^convertCallOp'('_call%3'(XV33107, XV33108, XV33109), 'lo.comp.genprolog^convertCallOp', _):- 'lo.comp.genprolog@convertCallOp'(XV33107, XV33108, XV33109).
'lo.comp.genprolog^convertPred'('_call%3'(XV33110, XV33111, XV33112), 'lo.comp.genprolog^convertPred', _):- 'lo.comp.genprolog@convertPred'(XV33110, XV33111, XV33112).
'lo.comp.genprolog^convertCall'('_call%3'(XV33113, XV33114, XV33115), 'lo.comp.genprolog^convertCall', _):- 'lo.comp.genprolog@convertCall'(XV33113, XV33114, XV33115).
'lo.comp.genprolog^convertHead'('_call%4'(XV33116, XV33117, XV33118, XV33119), 'lo.comp.genprolog^convertHead', _):- 'lo.comp.genprolog@convertHead'(XV33116, XV33117, XV33118, XV33119).
'lo.comp.genprolog^convertClause'('_call%3'(XV33120, XV33121, XV33122), 'lo.comp.genprolog^convertClause', _):- 'lo.comp.genprolog@convertClause'(XV33120, XV33121, XV33122).
'lo.comp.genprolog^genPrologSig'('_call%2'(XV33123, XV33124), 'lo.comp.genprolog^genPrologSig', _):- 'lo.comp.genprolog@genPrologSig'(XV33123, XV33124).
'lo.comp.genprolog^prog2code'('_call%3'(XV33125, XV33126, XV33127), 'lo.comp.genprolog^prog2code', _):- 'lo.comp.genprolog@prog2code'(XV33125, XV33126, XV33127).
'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string^_coerce'('_call%2'(XV33128, XV33129), 'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string^_coerce'(XLbV2560, XThV2560), _):- 'lo.coerce$coercion$lo.comp.term*prProg$lo.core*string@_coerce'(XV33128, XV33129, XLbV2560, XThV2560).
