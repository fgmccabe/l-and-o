'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.abstract's'0.0.1'n5o5'()5'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.ast'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.keywords'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'s\"I42'tpe't'lo.comp.abstract*defnKind''con't'lo.comp.abstract*defnKind''impl't'lo.comp.abstract*defnKind''valu't'lo.comp.abstract*defnKind''clss't'lo.comp.abstract*defnKind''imp't'lo.comp.abstract*defnKind''marker'FT1t'lo.comp.abstract*defnKind'S'isScalar'PT1t'lo.comp.ast*ast''isIden'PT3t'lo.comp.ast*ast't'lo.comp.location*location'S'genIden'FT1t'lo.comp.location*location't'lo.comp.ast*ast''isString'PT3t'lo.comp.ast*ast't'lo.comp.location*location'S'isInteger'PT3t'lo.comp.ast*ast't'lo.comp.location*location'i'isBinary'PT5t'lo.comp.ast*ast'St'lo.comp.location*location't'lo.comp.ast*ast't'lo.comp.ast*ast''binary'FT4t'lo.comp.location*location'St'lo.comp.ast*ast't'lo.comp.ast*ast't'lo.comp.ast*ast''isUnary'PT4t'lo.comp.ast*ast'St'lo.comp.location*location't'lo.comp.ast*ast''unary'FT3t'lo.comp.location*location'St'lo.comp.ast*ast't'lo.comp.ast*ast''isTernary'PT6t'lo.comp.ast*ast'St'lo.comp.location*location't'lo.comp.ast*ast't'lo.comp.ast*ast't'lo.comp.ast*ast''ternary'FT5t'lo.comp.location*location'St'lo.comp.ast*ast't'lo.comp.ast*ast't'lo.comp.ast*ast't'lo.comp.ast*ast''isSquareTerm'PT4t'lo.comp.ast*ast't'lo.comp.location*location't'lo.comp.ast*ast'Lt'lo.comp.ast*ast''squareTerm'FT3t'lo.comp.location*location't'lo.comp.ast*ast'Lt'lo.comp.ast*ast't'lo.comp.ast*ast''isBraceTerm'PT4t'lo.comp.ast*ast't'lo.comp.location*location't'lo.comp.ast*ast'Lt'lo.comp.ast*ast''braceTerm'FT3t'lo.comp.location*location't'lo.comp.ast*ast'Lt'lo.comp.ast*ast't'lo.comp.ast*ast''isIden'PT3t'lo.comp.ast*ast't'lo.comp.location*location'S'isRoundTuple'PT3t'lo.comp.ast*ast't'lo.comp.location*location'Lt'lo.comp.ast*ast''roundTuple'FT2t'lo.comp.location*location'Lt'lo.comp.ast*ast't'lo.comp.ast*ast''isSquareTuple'PT3t'lo.comp.ast*ast't'lo.comp.location*location'Lt'lo.comp.ast*ast''isBraceTuple'PT3t'lo.comp.ast*ast't'lo.comp.location*location'Lt'lo.comp.ast*ast''braceTuple'FT2t'lo.comp.location*location'Lt'lo.comp.ast*ast't'lo.comp.ast*ast''mapTuple'FT2t'lo.comp.location*location'Lt'lo.comp.ast*ast't'lo.comp.ast*ast''keyword'PT1t'lo.comp.ast*ast''isRoundTerm'PT4t'lo.comp.ast*ast't'lo.comp.location*location't'lo.comp.ast*ast'Lt'lo.comp.ast*ast''isRound'PT4t'lo.comp.ast*ast't'lo.comp.location*location't'lo.comp.ast*ast't'lo.comp.ast*ast''roundTerm'FT3t'lo.comp.location*location't'lo.comp.ast*ast'Lt'lo.comp.ast*ast't'lo.comp.ast*ast''deComma'FT1t'lo.comp.ast*ast'Lt'lo.comp.ast*ast''sameTerm'PT2t'lo.comp.ast*ast't'lo.comp.ast*ast''isQuantified'PT3t'lo.comp.ast*ast't'lo.comp.ast*ast't'lo.comp.ast*ast''isAlgebraicTypeDef'PT6t'lo.comp.ast*ast't'lo.comp.location*location'Lt'lo.comp.ast*ast'Lt'lo.comp.ast*ast't'lo.comp.ast*ast't'lo.comp.ast*ast''isContractSpec'PT6t'lo.comp.ast*ast't'lo.comp.location*location'Lt'lo.comp.ast*ast'Lt'lo.comp.ast*ast't'lo.comp.ast*ast'Lt'lo.comp.ast*ast''isConstrained'PT3t'lo.comp.ast*ast'Lt'lo.comp.ast*ast't'lo.comp.ast*ast''packageName'FT1t'lo.comp.ast*ast'S'pkgName'FT1t'lo.comp.ast*ast't'lo.repo*pkg''tupleize'PT3t'lo.comp.ast*ast'St'lo.comp.ast*ast'\"s\"I1'defnKind'Yt'lo.comp.abstract*defnKind'I0\"n6o6'()6's'tpe's'con's'impl's'valu's'clss's'imp'n0o0'()0'n0o0'()0'").
'lo.comp.abstract@init'():- !.
'lo.comp.abstract#tpe'('tpe%1'('lo.comp.abstract@tpe')):- !.
'lo.comp.abstract#con'('con%1'('lo.comp.abstract@con')):- !.
'lo.comp.abstract#impl'('impl%1'('lo.comp.abstract@impl')):- !.
'lo.comp.abstract#valu'('valu%1'('lo.comp.abstract@valu')):- !.
'lo.comp.abstract#clss'('clss%1'('lo.comp.abstract@clss')):- !.
'lo.comp.abstract#imp'('imp%1'('lo.comp.abstract@imp')):- !.
'lo.comp.abstract@marker'('lo.comp.abstract#tpe', "*"):- !.
'lo.comp.abstract@marker'('lo.comp.abstract#valu', "@"):- !.
'lo.comp.abstract@marker'('lo.comp.abstract#clss', "#"):- !.
'lo.comp.abstract@marker'('lo.comp.abstract#con', "$"):- !.
'lo.comp.abstract@marker'(_, _):- raise_exception('error'("lo.comp.abstract@marker", 14, 3, 15)).
'lo.comp.abstract@isScalar'('lo.comp.ast#intg'(X_33568, X_33569)).
'lo.comp.abstract@isScalar'('lo.comp.ast#flot'(X_33570, X_33571)).
'lo.comp.abstract@isScalar'('lo.comp.ast#strg'(X_33572, X_33573)).
'lo.comp.abstract@isIden'('lo.comp.ast#iden'(XLc, XNm), XLc, XNm).
'lo.comp.abstract@genIden'(XLc, 'lo.comp.ast#iden'(XLc, XXc525)):- !,
    '_str_gen'("N", XXc525).
'lo.comp.abstract@genIden'(_, _):- raise_exception('error'("lo.comp.abstract@genIden", 28, 3, 37)).
'lo.comp.abstract@isString'('lo.comp.ast#strg'(XLc, XSt), XLc, XSt).
'lo.comp.abstract@isInteger'('lo.comp.ast#intg'(XLc, XIx), XLc, XIx).
'lo.comp.abstract@isBinary'('lo.comp.ast#appl'(XLc, 'lo.comp.ast#iden'(X_33574, XNm), 'lo.comp.ast#tupl'(X_33575, "()", 'lo.core#,..'(XL, 'lo.core#,..'(XR, 'lo.core#[]')))), XNm, XLc, XL, XR).
'lo.comp.abstract@binary'(XLc, XOp, XL, XR, 'lo.comp.ast#appl'(XLc, 'lo.comp.ast#iden'(XLc, XOp), 'lo.comp.ast#tupl'(XLc, "()", 'lo.core#,..'(XL, 'lo.core#,..'(XR, 'lo.core#[]'))))):- !.
'lo.comp.abstract@binary'(_, _, _, _, _):- raise_exception('error'("lo.comp.abstract@binary", 41, 3, 61)).
'lo.comp.abstract@isUnary'('lo.comp.ast#appl'(XLc, 'lo.comp.ast#iden'(X_33580, XNm), 'lo.comp.ast#tupl'(X_33581, "()", 'lo.core#,..'(XL, 'lo.core#[]'))), XNm, XLc, XL).
'lo.comp.abstract@unary'(XLc, XOp, XA, 'lo.comp.ast#appl'(XLc, 'lo.comp.ast#iden'(XLc, XOp), 'lo.comp.ast#tupl'(XLc, "()", 'lo.core#,..'(XA, 'lo.core#[]')))):- !.
'lo.comp.abstract@unary'(_, _, _, _):- raise_exception('error'("lo.comp.abstract@unary", 48, 3, 56)).
'lo.comp.abstract@isTernary'('lo.comp.ast#appl'(XLc, 'lo.comp.ast#iden'(X_33584, XNm), 'lo.comp.ast#tupl'(X_33585, "()", 'lo.core#,..'(XL, 'lo.core#,..'(XM, 'lo.core#,..'(XR, 'lo.core#[]'))))), XNm, XLc, XL, XM, XR).
'lo.comp.abstract@ternary'(XLc, XNm, XL, XM, XR, 'lo.comp.ast#appl'(XLc, 'lo.comp.ast#iden'(XLc, XNm), 'lo.comp.ast#tupl'(XLc, "()", 'lo.core#,..'(XL, 'lo.core#,..'(XM, 'lo.core#,..'(XR, 'lo.core#[]')))))):- !.
'lo.comp.abstract@ternary'(_, _, _, _, _, _):- raise_exception('error'("lo.comp.abstract@ternary", 54, 3, 66)).
'lo.comp.abstract@isSquareTerm'('lo.comp.ast#appl'(XLc, XOp, 'lo.comp.ast#tupl'(X_33592, "[]", XA)), XLc, XOp, XA).
'lo.comp.abstract@squareTerm'(XLc, XOp, XEls, 'lo.comp.ast#appl'(XLc, XOp, 'lo.comp.ast#tupl'(XLc, "[]", XEls))):- !.
'lo.comp.abstract@squareTerm'(_, _, _, _):- raise_exception('error'("lo.comp.abstract@squareTerm", 62, 3, 54)).
'lo.comp.abstract@isBraceTerm'('lo.comp.ast#appl'(XLc, XOp, 'lo.comp.ast#tupl'(X_33593, "{}", XA)), XLc, XOp, XA).
'lo.comp.abstract@braceTerm'(XLc, XOp, XEls, 'lo.comp.ast#appl'(XLc, XOp, 'lo.comp.ast#tupl'(XLc, "{}", XEls))):- !.
'lo.comp.abstract@braceTerm'(_, _, _, _):- raise_exception('error'("lo.comp.abstract@braceTerm", 69, 3, 53)).
'lo.comp.abstract@isIden'('lo.comp.ast#iden'(XLc, XNm), XLc, XNm).
'lo.comp.abstract@isIden'('lo.comp.ast#tupl'(XLc, "()", 'lo.core#,..'('lo.comp.ast#iden'(X_33595, XNm), 'lo.core#[]')), XLc, XNm).
'lo.comp.abstract@isRoundTuple'('lo.comp.ast#tupl'(XLc, "()", XA), XLc, XA).
'lo.comp.abstract@roundTuple'(XLc, XEls, 'lo.comp.ast#tupl'(XLc, "()", XEls)):- !.
'lo.comp.abstract@roundTuple'(_, _, _):- raise_exception('error'("lo.comp.abstract@roundTuple", 81, 3, 39)).
'lo.comp.abstract@isSquareTuple'('lo.comp.ast#tupl'(XLc, "[]", XA), XLc, XA).
'lo.comp.abstract@isBraceTuple'('lo.comp.ast#tupl'(XLc, "{}", XA), XLc, XA).
'lo.comp.abstract@braceTuple'(XLc, XEls, 'lo.comp.ast#tupl'(XLc, "{}", XEls)):- !.
'lo.comp.abstract@braceTuple'(_, _, _):- raise_exception('error'("lo.comp.abstract@braceTuple", 93, 3, 39)).
'lo.comp.abstract@mapTuple'(XLc, XEls, 'lo.comp.ast#tupl'(XLc, "{}", XEls)):- !.
'lo.comp.abstract@mapTuple'(_, _, _):- raise_exception('error'("lo.comp.abstract@mapTuple", 97, 3, 37)).
'lo.comp.abstract@keyword'('lo.comp.ast#iden'(X_33596, XNm)):- 'lo.comp.keywords@isKeyword'(XNm).
'lo.comp.abstract@isRoundTerm'('lo.comp.ast#appl'(XLc, XOp, 'lo.comp.ast#tupl'(X_33597, "()", XA)), XLc, XOp, XA):- 'lo.comp.abstract@neg324'(XOp).
'lo.comp.abstract@isRound'('lo.comp.ast#appl'(XLc, XOp, XA), XLc, XOp, XA):- 'lo.comp.abstract@neg325'(XOp),
    'lo.comp.abstract@isRoundTuple'(XA, X_33598, X_33599).
'lo.comp.abstract@roundTerm'(XLc, XOp, XEls, 'lo.comp.ast#appl'(XLc, XOp, 'lo.comp.ast#tupl'(XLc, "()", XEls))):- !.
'lo.comp.abstract@roundTerm'(_, _, _, _):- raise_exception('error'("lo.comp.abstract@roundTerm", 109, 3, 53)).
'lo.comp.abstract@deComma'(XT, 'lo.core#,..'(XL, XXd39060)):- 'lo.comp.abstract@isBinary'(XT, ",", X_33600, XL, XR),
    !,
    'lo.comp.abstract@deComma'(XR, XXd39060).
'lo.comp.abstract@deComma'(XT, 'lo.core#,..'(XT, 'lo.core#[]')):- !.
'lo.comp.abstract@deComma'(_, _):- raise_exception('error'("lo.comp.abstract@deComma", 117, 3, 56)).
'lo.comp.abstract@sameTerms'('lo.core#[]', 'lo.core#[]').
'lo.comp.abstract@sameTerms'('lo.core#,..'(XA, XL1), 'lo.core#,..'(XB, XL2)):- 'lo.comp.abstract@sameTerm'(XA, XB),
    'lo.comp.abstract@sameTerms'(XL1, XL2).
'lo.comp.abstract@sameTerm'('lo.comp.ast#iden'(X_33605, XNm), 'lo.comp.ast#iden'(X_33606, XNm)).
'lo.comp.abstract@sameTerm'('lo.comp.ast#intg'(X_33607, XIx), 'lo.comp.ast#intg'(X_33608, XIx)).
'lo.comp.abstract@sameTerm'('lo.comp.ast#flot'(X_33609, XDx), 'lo.comp.ast#flot'(X_33610, XDx)).
'lo.comp.abstract@sameTerm'('lo.comp.ast#strg'(X_33611, XS), 'lo.comp.ast#strg'(X_33612, XS)).
'lo.comp.abstract@sameTerm'('lo.comp.ast#tupl'(X_33613, XT, XA), 'lo.comp.ast#tupl'(X_33614, XT, XB)):- 'lo.comp.abstract@sameTerms'(XA, XB).
'lo.comp.abstract@sameTerm'('lo.comp.ast#appl'(X_33615, XOA, XAA), 'lo.comp.ast#appl'(X_33616, XOB, XBA)):- 'lo.comp.abstract@sameTerm'(XOA, XOB),
    'lo.comp.abstract@sameTerm'(XAA, XBA).
'lo.comp.abstract@isQuantified'(XT, XV, XB):- 'lo.comp.abstract@isUnary'(XT, "all", X_33617, XR),
    'lo.comp.abstract@isBinary'(XR, "~~", X_33618, XV, XB).
'lo.comp.abstract@getQuantifiers'(XT, XXb19057, XB):- 'lo.comp.abstract@isQuantified'(XT, XV, XB),
    'lo.comp.abstract@deComma'(XV, XXb19057).
'lo.comp.abstract@getQuantifiers'(XT, 'lo.core#[]', XT).
'lo.comp.abstract@getConstraints'(XT, XXb19058, XR):- 'lo.comp.abstract@isBinary'(XT, "|:", X_33619, XL, XR),
    'lo.comp.abstract@deComma'(XL, XXb19058).
'lo.comp.abstract@getConstraints'(XT, 'lo.core#[]', XT).
'lo.comp.abstract@isAlgebraicTypeDef'(XStmt, XLc, XQuants, XConstraints, XHead, XBody):- 'lo.comp.abstract@isUnary'(XStmt, "type", XLc, XTerm),
    'lo.comp.abstract@getQuantifiers'(XTerm, XQuants, XInner),
    'lo.comp.abstract@getConstraints'(XInner, XConstraints, XTpStmt),
    'lo.comp.abstract@isBinary'(XTpStmt, "::=", X_33620, XHead, XBody).
'lo.comp.abstract@contractSpec'(XS, XXb19059, XConstraints, XCon):- 'lo.comp.abstract@isQuantified'(XS, XV, XB),
    'lo.comp.abstract@contractSpec'(XB, X_33621, XConstraints, XCon),
    'lo.comp.abstract@deComma'(XV, XXb19059).
'lo.comp.abstract@contractSpec'(XS, 'lo.core#[]', XXb19060, XCon):- 'lo.comp.abstract@isBinary'(XS, "|:", X_33622, XL, XCon),
    'lo.comp.abstract@deComma'(XL, XXb19060).
'lo.comp.abstract@contractSpec'(XS, 'lo.core#[]', 'lo.core#[]', XS).
'lo.comp.abstract@isContractSpec'(XSt, XLc, XQuants, XConstraints, XCon, XBody):- 'lo.comp.abstract@isUnary'(XSt, "contract", XLc, XI),
    'lo.comp.abstract@isBinary'(XI, "<~", X_33623, XL, XR),
    'lo.comp.abstract@isBraceTuple'(XR, X_33624, XBody),
    'lo.comp.abstract@contractSpec'(XL, XQuants, XConstraints, XCon).
'lo.comp.abstract@isConstrained'(XT, XXb19061, XR):- 'lo.comp.abstract@isBinary'(XT, "|:", X_33625, XL, XR),
    'lo.comp.abstract@deComma'(XL, XXb19061).
'lo.comp.abstract@packageName'(XT, XPkg):- 'lo.comp.abstract@isIden'(XT, X_33626, XPkg),
    !.
'lo.comp.abstract@packageName'(XT, XPkg):- 'lo.comp.abstract@isString'(XT, X_33627, XPkg),
    !.
'lo.comp.abstract@packageName'(XT, XXe4902):- 'lo.comp.abstract@isBinary'(XT, ".", X_33628, XL, XR),
    !,
    ocall('+%1'(XXV5249),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    ocall('+%1'(XXV5250),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    'lo.comp.abstract@packageName'(XL, XXd39063),
    ocall('_call%3'(XXd39063, ".", XXe4901),XXV5249,XXV5249),
    'lo.comp.abstract@packageName'(XR, XXd39064),
    ocall('_call%3'(XXe4901, XXd39064, XXe4902),XXV5250,XXV5250).
'lo.comp.abstract@packageName'(_, _):- raise_exception('error'("lo.comp.abstract@packageName", 180, 3, 42)).
'lo.comp.abstract@packageVersion'(XT, XPkg):- 'lo.comp.abstract@isIden'(XT, X_33629, XPkg),
    !.
'lo.comp.abstract@packageVersion'(XT, XPkg):- 'lo.comp.abstract@isString'(XT, X_33630, XPkg),
    !.
'lo.comp.abstract@packageVersion'(XT, XXd39067):- 'lo.comp.abstract@isInteger'(XT, X_33631, XIx),
    !,
    ocall('disp%1'(XXV5251),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    ocall('_call%2'(XIx, XXe4903),XXV5251,XXV5251),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XXe4903, 'lo.core#[]')), XXd39067).
'lo.comp.abstract@packageVersion'(XT, XXe4905):- 'lo.comp.abstract@isBinary'(XT, ".", X_33633, XL, XR),
    !,
    ocall('+%1'(XXV5252),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    ocall('+%1'(XXV5253),'lo.core$additive$lo.core*string','lo.core$additive$lo.core*string'),
    'lo.comp.abstract@packageVersion'(XL, XXd39068),
    ocall('_call%3'(XXd39068, ".", XXe4904),XXV5252,XXV5252),
    'lo.comp.abstract@packageVersion'(XR, XXd39069),
    ocall('_call%3'(XXe4904, XXd39069, XXe4905),XXV5253,XXV5253).
'lo.comp.abstract@packageVersion'(_, _):- raise_exception('error'("lo.comp.abstract@packageVersion", 191, 3, 45)).
'lo.comp.abstract@pkgName'(XT, 'lo.repo#pkg'(XXd39070, 'lo.repo#vers'(XXd39071))):- 'lo.comp.abstract@isBinary'(XT, "#", X_33634, XL, XR),
    !,
    'lo.comp.abstract@packageName'(XL, XXd39070),
    'lo.comp.abstract@packageVersion'(XR, XXd39071).
'lo.comp.abstract@pkgName'(XT, 'lo.repo#pkg'(XXd39074, 'lo.repo#defltVersion')):- !,
    'lo.comp.abstract@packageName'(XT, XXd39074).
'lo.comp.abstract@pkgName'(_, _):- raise_exception('error'("lo.comp.abstract@pkgName", 186, 3, 87)).
'lo.comp.abstract@tupleize'(XP, XOp, 'lo.comp.ast#tupl'(XLc, XOp, 'lo.core#,..'(XL, XXb19062))):- 'lo.comp.abstract@isBinary'(XP, ",", XLc, XL, XR),
    'lo.comp.abstract@deComma'(XR, XXb19062).
'lo.comp.abstract@tupleize'(XP, XOp, 'lo.comp.ast#tupl'(XXV5254, XOp, 'lo.core#,..'(XP, 'lo.core#[]'))):- ocall('loc%1'(XXV5254),XP,XP).
'lo.comp.abstract^marker'('_call%2'(XV30261, XV30262), 'lo.comp.abstract^marker', _):- 'lo.comp.abstract@marker'(XV30261, XV30262).
'lo.comp.abstract^isScalar'('_call%1'(XV30263), 'lo.comp.abstract^isScalar', _):- 'lo.comp.abstract@isScalar'(XV30263).
'lo.comp.abstract^isIden'('_call%3'(XV30264, XV30265, XV30266), 'lo.comp.abstract^isIden', _):- 'lo.comp.abstract@isIden'(XV30264, XV30265, XV30266).
'lo.comp.abstract^genIden'('_call%2'(XV30267, XV30268), 'lo.comp.abstract^genIden', _):- 'lo.comp.abstract@genIden'(XV30267, XV30268).
'lo.comp.abstract^isString'('_call%3'(XV30269, XV30270, XV30271), 'lo.comp.abstract^isString', _):- 'lo.comp.abstract@isString'(XV30269, XV30270, XV30271).
'lo.comp.abstract^isInteger'('_call%3'(XV30272, XV30273, XV30274), 'lo.comp.abstract^isInteger', _):- 'lo.comp.abstract@isInteger'(XV30272, XV30273, XV30274).
'lo.comp.abstract^isBinary'('_call%5'(XV30275, XV30276, XV30277, XV30278, XV30279), 'lo.comp.abstract^isBinary', _):- 'lo.comp.abstract@isBinary'(XV30275, XV30276, XV30277, XV30278, XV30279).
'lo.comp.abstract^binary'('_call%5'(XV30280, XV30281, XV30282, XV30283, XV30284), 'lo.comp.abstract^binary', _):- 'lo.comp.abstract@binary'(XV30280, XV30281, XV30282, XV30283, XV30284).
'lo.comp.abstract^isUnary'('_call%4'(XV30285, XV30286, XV30287, XV30288), 'lo.comp.abstract^isUnary', _):- 'lo.comp.abstract@isUnary'(XV30285, XV30286, XV30287, XV30288).
'lo.comp.abstract^unary'('_call%4'(XV30289, XV30290, XV30291, XV30292), 'lo.comp.abstract^unary', _):- 'lo.comp.abstract@unary'(XV30289, XV30290, XV30291, XV30292).
'lo.comp.abstract^isTernary'('_call%6'(XV30293, XV30294, XV30295, XV30296, XV30297, XV30298), 'lo.comp.abstract^isTernary', _):- 'lo.comp.abstract@isTernary'(XV30293, XV30294, XV30295, XV30296, XV30297, XV30298).
'lo.comp.abstract^ternary'('_call%6'(XV30299, XV30300, XV30301, XV30302, XV30303, XV30304), 'lo.comp.abstract^ternary', _):- 'lo.comp.abstract@ternary'(XV30299, XV30300, XV30301, XV30302, XV30303, XV30304).
'lo.comp.abstract^isSquareTerm'('_call%4'(XV30305, XV30306, XV30307, XV30308), 'lo.comp.abstract^isSquareTerm', _):- 'lo.comp.abstract@isSquareTerm'(XV30305, XV30306, XV30307, XV30308).
'lo.comp.abstract^squareTerm'('_call%4'(XV30309, XV30310, XV30311, XV30312), 'lo.comp.abstract^squareTerm', _):- 'lo.comp.abstract@squareTerm'(XV30309, XV30310, XV30311, XV30312).
'lo.comp.abstract^isBraceTerm'('_call%4'(XV30313, XV30314, XV30315, XV30316), 'lo.comp.abstract^isBraceTerm', _):- 'lo.comp.abstract@isBraceTerm'(XV30313, XV30314, XV30315, XV30316).
'lo.comp.abstract^braceTerm'('_call%4'(XV30317, XV30318, XV30319, XV30320), 'lo.comp.abstract^braceTerm', _):- 'lo.comp.abstract@braceTerm'(XV30317, XV30318, XV30319, XV30320).
'lo.comp.abstract^isIden'('_call%3'(XV30321, XV30322, XV30323), 'lo.comp.abstract^isIden', _):- 'lo.comp.abstract@isIden'(XV30321, XV30322, XV30323).
'lo.comp.abstract^isRoundTuple'('_call%3'(XV30324, XV30325, XV30326), 'lo.comp.abstract^isRoundTuple', _):- 'lo.comp.abstract@isRoundTuple'(XV30324, XV30325, XV30326).
'lo.comp.abstract^roundTuple'('_call%3'(XV30327, XV30328, XV30329), 'lo.comp.abstract^roundTuple', _):- 'lo.comp.abstract@roundTuple'(XV30327, XV30328, XV30329).
'lo.comp.abstract^isSquareTuple'('_call%3'(XV30330, XV30331, XV30332), 'lo.comp.abstract^isSquareTuple', _):- 'lo.comp.abstract@isSquareTuple'(XV30330, XV30331, XV30332).
'lo.comp.abstract^isBraceTuple'('_call%3'(XV30333, XV30334, XV30335), 'lo.comp.abstract^isBraceTuple', _):- 'lo.comp.abstract@isBraceTuple'(XV30333, XV30334, XV30335).
'lo.comp.abstract^braceTuple'('_call%3'(XV30336, XV30337, XV30338), 'lo.comp.abstract^braceTuple', _):- 'lo.comp.abstract@braceTuple'(XV30336, XV30337, XV30338).
'lo.comp.abstract^mapTuple'('_call%3'(XV30339, XV30340, XV30341), 'lo.comp.abstract^mapTuple', _):- 'lo.comp.abstract@mapTuple'(XV30339, XV30340, XV30341).
'lo.comp.abstract^keyword'('_call%1'(XV30342), 'lo.comp.abstract^keyword', _):- 'lo.comp.abstract@keyword'(XV30342).
'lo.comp.abstract@neg324'(XOp):- 'lo.comp.abstract@keyword'(XOp),
    !,
    fail.
'lo.comp.abstract@neg324'(XOp).
'lo.comp.abstract^isRoundTerm'('_call%4'(XV30343, XV30344, XV30345, XV30346), 'lo.comp.abstract^isRoundTerm', _):- 'lo.comp.abstract@isRoundTerm'(XV30343, XV30344, XV30345, XV30346).
'lo.comp.abstract@neg325'(XOp):- 'lo.comp.abstract@keyword'(XOp),
    !,
    fail.
'lo.comp.abstract@neg325'(XOp).
'lo.comp.abstract^isRound'('_call%4'(XV30347, XV30348, XV30349, XV30350), 'lo.comp.abstract^isRound', _):- 'lo.comp.abstract@isRound'(XV30347, XV30348, XV30349, XV30350).
'lo.comp.abstract^roundTerm'('_call%4'(XV30351, XV30352, XV30353, XV30354), 'lo.comp.abstract^roundTerm', _):- 'lo.comp.abstract@roundTerm'(XV30351, XV30352, XV30353, XV30354).
'lo.comp.abstract^deComma'('_call%2'(XV30355, XV30356), 'lo.comp.abstract^deComma', _):- 'lo.comp.abstract@deComma'(XV30355, XV30356).
'lo.comp.abstract^sameTerms'('_call%2'(XV30357, XV30358), 'lo.comp.abstract^sameTerms', _):- 'lo.comp.abstract@sameTerms'(XV30357, XV30358).
'lo.comp.abstract^sameTerm'('_call%2'(XV30359, XV30360), 'lo.comp.abstract^sameTerm', _):- 'lo.comp.abstract@sameTerm'(XV30359, XV30360).
'lo.comp.abstract^isQuantified'('_call%3'(XV30361, XV30362, XV30363), 'lo.comp.abstract^isQuantified', _):- 'lo.comp.abstract@isQuantified'(XV30361, XV30362, XV30363).
'lo.comp.abstract^getQuantifiers'('_call%3'(XV30364, XV30365, XV30366), 'lo.comp.abstract^getQuantifiers', _):- 'lo.comp.abstract@getQuantifiers'(XV30364, XV30365, XV30366).
'lo.comp.abstract^getConstraints'('_call%3'(XV30367, XV30368, XV30369), 'lo.comp.abstract^getConstraints', _):- 'lo.comp.abstract@getConstraints'(XV30367, XV30368, XV30369).
'lo.comp.abstract^isAlgebraicTypeDef'('_call%6'(XV30370, XV30371, XV30372, XV30373, XV30374, XV30375), 'lo.comp.abstract^isAlgebraicTypeDef', _):- 'lo.comp.abstract@isAlgebraicTypeDef'(XV30370, XV30371, XV30372, XV30373, XV30374, XV30375).
'lo.comp.abstract^contractSpec'('_call%4'(XV30376, XV30377, XV30378, XV30379), 'lo.comp.abstract^contractSpec', _):- 'lo.comp.abstract@contractSpec'(XV30376, XV30377, XV30378, XV30379).
'lo.comp.abstract^isContractSpec'('_call%6'(XV30380, XV30381, XV30382, XV30383, XV30384, XV30385), 'lo.comp.abstract^isContractSpec', _):- 'lo.comp.abstract@isContractSpec'(XV30380, XV30381, XV30382, XV30383, XV30384, XV30385).
'lo.comp.abstract^isConstrained'('_call%3'(XV30386, XV30387, XV30388), 'lo.comp.abstract^isConstrained', _):- 'lo.comp.abstract@isConstrained'(XV30386, XV30387, XV30388).
'lo.comp.abstract^packageName'('_call%2'(XV30389, XV30390), 'lo.comp.abstract^packageName', _):- 'lo.comp.abstract@packageName'(XV30389, XV30390).
'lo.comp.abstract^packageVersion'('_call%2'(XV30391, XV30392), 'lo.comp.abstract^packageVersion', _):- 'lo.comp.abstract@packageVersion'(XV30391, XV30392).
'lo.comp.abstract^pkgName'('_call%2'(XV30393, XV30394), 'lo.comp.abstract^pkgName', _):- 'lo.comp.abstract@pkgName'(XV30393, XV30394).
'lo.comp.abstract^tupleize'('_call%3'(XV30395, XV30396, XV30397), 'lo.comp.abstract^tupleize', _):- 'lo.comp.abstract@tupleize'(XV30395, XV30396, XV30397).
