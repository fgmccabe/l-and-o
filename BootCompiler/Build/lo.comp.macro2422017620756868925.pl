'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.macro'e'*'n15o15'()15'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'public'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.abstract'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.ast'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I1'macroRewrite'PT4Lt'lo.comp.ast*ast'Lt'lo.comp.ast*ast't'lo.comp.errors*report't'lo.comp.errors*report'\"s'I0'n3o3'()3's'markPublic's'markPrivate's'noMark'n0o0'()0'n0o0'()0'").
'lo.comp.macro@init'() :- !.
'lo.comp.macro@checkSoFar'(X_1696, X_1697, 'lo.core#[]', 'lo.core#[]', XRp, XRp).
'lo.comp.macro@checkSoFar'(XNm, XT, 'lo.core#,..'(XP, XL), XL, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XP, ":", XLc, XNN, XRR),
    'lo.comp.abstract@isIden'(XNN, X_1698, XNm),
    'lo.comp.macro@cond18'(XLc, XX26479, XX26469, XX26465, XX26461, XNm, XRpx, XRp, XT, XRR).
'lo.comp.macro@checkSoFar'(XNm, XT, 'lo.core#,..'(XP, XL), 'lo.core#,..'(XP, XLL), XRp, XRpx) :- 'lo.comp.macro@checkSoFar'(XNm, XT, XL, XLL, XRp, XRpx).
'lo.comp.macro@pickupFields'('lo.core#[]', XFace, XFace, XRp, XRp).
'lo.comp.macro@pickupFields'('lo.core#,..'(XT, XM), XSF, XFace, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XT, ":", XLc, XL, XR),
    'lo.comp.abstract@isIden'(XL, X_1699, XNm),
    'lo.comp.macro@checkSoFar'(XNm, XR, XSF, XSF0, XRp, XRp0),
    'lo.comp.macro@pickupFields'(XM, 'lo.core#,..'(XT, XSF0), XFace, XRp0, XRpx).
'lo.comp.macro@pickupFields'('lo.core#,..'(XT, XM), XSF, XFace, XRp, XRpx) :- ocall('disp%2'(XT, XX26540),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("invalid type field: "), 'lo.core#,..'(XX26540, 'lo.core#[]'))), XX26546),
    ocall('loc%1'(XXV47),XT,XT),
    'lo.comp.errors@reportError'(XX26546, XXV47, XRp, XRp0),
    'lo.comp.macro@pickupFields'(XM, XSF, XFace, XRp0, XRpx).
'lo.comp.macro@algebraicFace'(XT, XSoFar, XFace, XRp, XRpx) :- 'lo.comp.abstract@isBinary'(XT, "|", X_1700, XL, XR),
    'lo.comp.macro@algebraicFace'(XL, XSoFar, XSF, XRp, XRp0),
    'lo.comp.macro@algebraicFace'(XR, XSF, XFace, XRp0, XRpx).
'lo.comp.macro@algebraicFace'(XT, XSoFar, XSoFar, XRp, XRp) :- 'lo.comp.abstract@isRoundTerm'(XT, X_1701, X_1702, X_1703).
'lo.comp.macro@algebraicFace'(XT, XFace, XFace, XRp, XRp) :- 'lo.comp.abstract@isIden'(XT, X_1704, X_1705).
'lo.comp.macro@algebraicFace'(XT, XSoFar, XFace, XRp, XRpx) :- 'lo.comp.abstract@isBraceTerm'(XT, X_1706, X_1707, XArgs),
    'lo.comp.macro@pickupFields'(XArgs, XSoFar, XFace, XRp, XRpx).
'lo.comp.macro@listComma'('lo.core#,..'(XT, 'lo.core#[]'), X_1708, XT).
'lo.comp.macro@listComma'('lo.core#,..'(XT, XR), XLc, XX26617) :- 'lo.comp.macro@listComma'(XR, XLc, XRR),
    'lo.comp.abstract@binary'(XLc, ",", XT, XRR, XX26617).
'lo.comp.macro@wrapConstraints'('lo.core#[]', X_1709, XTp, XTp).
'lo.comp.macro@wrapConstraints'(XCon, XLc, XTp, XX26631) :- 'lo.comp.macro@listComma'(XCon, XLc, XCTp),
    'lo.comp.abstract@binary'(XLc, "|:", XCTp, XTp, XX26631).
'lo.comp.macro@wrapQuants'('lo.core#[]', X_1710, XRule, XRule).
'lo.comp.macro@wrapQuants'(XQ, XLc, XRl, XX26647) :- 'lo.comp.macro@listComma'(XQ, XLc, XQV),
    'lo.comp.abstract@binary'(XLc, "~~", XQV, XRl, XX26646),
    'lo.comp.abstract@unary'(XLc, "all", XX26646, XX26647).
'lo.comp.macro@typeRule'(XLc, XQuants, XConstraints, XHd, XBody, XX26658) :- 'lo.comp.abstract@binary'(XLc, "<~", XHd, XBody, XX26664),
    'lo.comp.macro@wrapConstraints'(XConstraints, XLc, XX26664, XConRl),
    'lo.comp.macro@wrapQuants'(XQuants, XLc, XConRl, XRule),
    'lo.comp.abstract@unary'(XLc, "type", XRule, XX26658).
'lo.comp.macro@mark'('lo.comp.macro#markPublic', XLc, XStmt, XX26675) :- 'lo.comp.abstract@unary'(XLc, "public", XStmt, XX26675).
'lo.comp.macro@mark'('lo.comp.macro#markPrivate', XLc, XStmt, XX26681) :- 'lo.comp.abstract@unary'(XLc, "private", XStmt, XX26681).
'lo.comp.macro@mark'('lo.comp.macro#noMark', X_1711, XStmt, XStmt).
'lo.comp.macro@bodyRule'(XLc, XHd, XEls, XX26694) :- 'lo.comp.abstract@braceTuple'(XLc, XEls, XX26693),
    'lo.comp.abstract@binary'(XLc, "<=", XHd, XX26693, XX26694).
'lo.comp.macro@classType'(XLc, XArgs, XRes, XX26701) :- 'lo.comp.abstract@isRoundTuple'(XA, XLc, XArgs),
    'lo.comp.abstract@binary'(XLc, "<=>", XA, XRes, XX26701).
'lo.comp.macro@genAnonArgs'('lo.core#[]', 'lo.core#[]').
'lo.comp.macro@genAnonArgs'('lo.core#,..'(XT, XM), 'lo.core#,..'('lo.comp.ast#iden'(XXV48, "_"), XA)) :- ocall('loc%1'(XXV48),XT,XT),
    'lo.comp.macro@genAnonArgs'(XM, XA).
'lo.comp.macro@genFieldArgs'('lo.core#[]', 'lo.core#[]', 'lo.core#[]').
'lo.comp.macro@genFieldArgs'('lo.core#,..'(XF, XM), 'lo.core#,..'(XV, XR), 'lo.core#,..'(XX26728, XAR)) :- 'lo.comp.abstract@isBinary'(XF, ":", XLc, XL, X_1712),
    '_str_gen'("_", XX26737),
    XV = 'lo.comp.ast#iden'(XLc, XX26737),
    'lo.comp.macro@genFieldArgs'(XM, XR, XAR),
    'lo.comp.abstract@binary'(XLc, "=", XL, XV, XX26728).
'lo.comp.macro@convertConstructor'(XNm, XTp, XMark, XQuants, XConstraints, 'lo.core#,..'(XTpRule, 'lo.core#,..'(XBodyRule, XTail)), XTail) :- 'lo.comp.abstract@isIden'(XNm, XLc, X_1713),
    'lo.comp.macro@wrapConstraints'(XConstraints, XLc, XTp, XTpCon),
    'lo.comp.macro@wrapQuants'(XQuants, XLc, XTpCon, XQTp),
    'lo.comp.abstract@binary'(XLc, ":", XNm, XQTp, XX26769),
    'lo.comp.macro@mark'(XMark, XLc, XX26769, XTpRule),
    'lo.comp.macro@bodyRule'(XLc, XNm, 'lo.core#[]', XBodyRule).
'lo.comp.macro@convertConstructor'(XCon, XTp, XMark, XQuants, XConstraints, 'lo.core#,..'(XTpRule, 'lo.core#,..'(XBodyRule, XTail)), XTail) :- 'lo.comp.abstract@isRoundTerm'(XCon, XLc, XOp, XArgTypes),
    'lo.comp.macro@classType'(XLc, XArgTypes, XTp, XClassType),
    'lo.comp.macro@wrapConstraints'(XConstraints, XLc, XClassType, XCTp),
    'lo.comp.macro@wrapQuants'(XQuants, XLc, XCTp, XQTp),
    'lo.comp.abstract@binary'(XLc, ":", XOp, XQTp, XX26807),
    'lo.comp.macro@mark'(XMark, XLc, XX26807, XTpRule),
    'lo.comp.macro@genAnonArgs'(XArgTypes, XArgs),
    'lo.comp.abstract@roundTerm'(XLc, XOp, XArgs, XX26815),
    'lo.comp.macro@bodyRule'(XLc, XX26815, 'lo.core#[]', XBodyRule).
'lo.comp.macro@convertConstructor'(XCon, XTp, XMark, XQuants, XConstraints, 'lo.core#,..'(XTpRule, 'lo.core#,..'(XBodyRule, XTail)), XTail) :- 'lo.comp.abstract@isBraceTerm'(XCon, X_1714, XOp, XFldTps),
    'lo.comp.abstract@braceTuple'(XLc, XFldTps, XX26838),
    'lo.comp.abstract@binary'(XLc, "<=>", XX26838, XTp, XX26840),
    'lo.comp.macro@wrapConstraints'(XConstraints, XLc, XX26840, XCTp),
    'lo.comp.macro@wrapQuants'(XQuants, XLc, XCTp, XQTp),
    'lo.comp.abstract@binary'(XLc, ":", XOp, XQTp, XX26851),
    'lo.comp.macro@mark'(XMark, XLc, XX26851, XTpRule),
    'lo.comp.macro@genFieldArgs'(XFldTps, XArgs, XDefs),
    'lo.comp.abstract@roundTerm'(XLc, XOp, XArgs, XX26860),
    'lo.comp.macro@bodyRule'(XLc, XX26860, XDefs, XBodyRule).
'lo.comp.macro@convertConstructors'(XPair, XHead, XMark, XQuants, XConstraints, XElements, XTail) :- 'lo.comp.abstract@isBinary'(XPair, "|", X_1715, XL, XR),
    'lo.comp.macro@convertConstructors'(XL, XHead, XMark, XQuants, XConstraints, XElements, XL1),
    'lo.comp.macro@convertConstructors'(XR, XHead, XMark, XQuants, XConstraints, XL1, XTail).
'lo.comp.macro@convertConstructors'(XTerm, XHead, XMark, XQuants, XConstraints, XElements, XTail) :- 'lo.comp.macro@convertConstructor'(XTerm, XHead, XMark, XQuants, XConstraints, XElements, XTail).
'lo.comp.macro@convertAlgebraic'(XLc, XMark, XQuants, XConstraints, XHead, XBody, 'lo.core#,..'(XTypeRule, XElements), XTail, XRp, XRpx) :- 'lo.comp.macro@algebraicFace'(XBody, 'lo.core#[]', XEls, XRp, XRpx),
    'lo.comp.abstract@isBraceTuple'(XFace, XLc, XEls),
    'lo.comp.macro@typeRule'(XLc, XQuants, XConstraints, XHead, XFace, XFaceRule),
    'lo.comp.macro@mark'(XMark, XLc, XFaceRule, XTypeRule),
    'lo.comp.macro@convertConstructors'(XBody, XHead, XMark, XQuants, XConstraints, XElements, XTail).
'lo.comp.macro@generateAnnotations'('lo.core#[]', X_1716, X_1717, XStmts, XStmts).
'lo.comp.macro@generateAnnotations'('lo.core#,..'(XDef, XEls), XQuants, XConstraints, 'lo.core#,..'(XX26952, XStmts), XS0) :- 'lo.comp.abstract@isBinary'(XDef, ":", XLc, XN, XTp),
    'lo.comp.macro@wrapConstraints'(XConstraints, XLc, XTp, XCTp),
    'lo.comp.macro@wrapQuants'(XQuants, XLc, XCTp, XMTp),
    'lo.comp.macro@generateAnnotations'(XEls, XQuants, XConstraints, XStmts, XS0),
    'lo.comp.abstract@binary'(XLc, ":", XN, XMTp, XX26952).
'lo.comp.macro@generateAnnotations'('lo.core#,..'(X_1718, XEls), XQuants, XConstraints, XStmts, XS0) :- 'lo.comp.macro@generateAnnotations'(XEls, XQuants, XConstraints, XStmts, XS0).
'lo.comp.macro@macroRewrite'('lo.core#[]', 'lo.core#[]', XRp, XRp).
'lo.comp.macro@macroRewrite'('lo.core#,..'(XSt, XMore), XStmts, XRp, XRpx) :- 'lo.comp.abstract@isUnary'(XSt, "public", X_1719, XInner),
    'lo.comp.abstract@isAlgebraicTypeDef'(XInner, XLc, XQuants, XConstraints, XHead, XBody),
    'lo.comp.macro@convertAlgebraic'(XLc, 'lo.comp.macro#markPublic', XQuants, XConstraints, XHead, XBody, XStmts, XS0, XRp, XRp0),
    'lo.comp.macro@macroRewrite'(XMore, XS0, XRp0, XRpx).
'lo.comp.macro@macroRewrite'('lo.core#,..'(XSt, XMore), XStmts, XRp, XRpx) :- 'lo.comp.abstract@isUnary'(XSt, "private", X_1720, XInner),
    'lo.comp.abstract@isAlgebraicTypeDef'(XInner, XLc, XQuants, XConstraints, XHead, XBody),
    'lo.comp.macro@convertAlgebraic'(XLc, 'lo.comp.macro#markPrivate', XQuants, XConstraints, XHead, XBody, XStmts, XS0, XRp, XRp0),
    'lo.comp.macro@macroRewrite'(XMore, XS0, XRp0, XRpx).
'lo.comp.macro@macroRewrite'('lo.core#,..'(XSt, XMore), XStmts, XRp, XRpx) :- 'lo.comp.abstract@isAlgebraicTypeDef'(XSt, XLc, XQuants, XConstraints, XHead, XBody),
    'lo.comp.macro@convertAlgebraic'(XLc, 'lo.comp.macro#noMark', XQuants, XConstraints, XHead, XBody, XStmts, XS0, XRp, XRp0),
    'lo.comp.macro@macroRewrite'(XMore, XS0, XRp0, XRpx).
'lo.comp.macro@macroRewrite'('lo.core#,..'(XSt, XMore), 'lo.core#,..'(XSt, XStmts), XRp, XRpx) :- 'lo.comp.abstract@isContractSpec'(XSt, X_1721, XQuants, XConstraints, XCon, XEls),
    'lo.comp.macro@generateAnnotations'(XEls, XQuants, 'lo.core#,..'(XCon, XConstraints), XStmts, XS0),
    'lo.comp.macro@macroRewrite'(XMore, XS0, XRp, XRpx).
'lo.comp.macro@macroRewrite'('lo.core#,..'(XSt, XMore), 'lo.core#,..'(XSt, XStmts), XRp, XRpx) :- 'lo.comp.macro@macroRewrite'(XMore, XStmts, XRp, XRpx).
'lo.comp.macro@hasType'(XLc, XNm, XTp, XX27118) :- 'lo.comp.abstract@binary'(XLc, ":", 'lo.comp.ast#iden'(XLc, XNm), XTp, XX27118).
'lo.comp.macro@funType'(XLc, XArgs, XRes, XX27125) :- 'lo.comp.abstract@isRoundTuple'(XA, XLc, XArgs),
    'lo.comp.abstract@binary'(XLc, "=>", XA, XRes, XX27125).
'lo.comp.macro@genericType'(XLc, XNm, XArgs, XX27135) :- 'lo.comp.abstract@squareTerm'(XLc, XNm, XArgs, XX27135).
'lo.comp.macro@cond18'(XLc, XX26479, XX26469, XX26465, XX26461, XNm, XRpx, XRp, XT, XRR) :- 'lo.comp.abstract@sameTerm'(XRR, XT),
    !,
    XRp = XRpx.
'lo.comp.macro@cond18'(XLc, XX26479, XX26469, XX26465, XX26461, XNm, XRpx, XRp, XT, XRR) :- ocall('disp%2'(XNm, XX26461),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('disp%2'(XT, XX26465),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('disp%2'(XRR, XX26469),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("field "), 'lo.core#,..'(XX26461, 'lo.core#,..'('lo.core#ss'(" : "), 'lo.core#,..'(XX26465, 'lo.core#,..'('lo.core#ss'(" must be identical to: "), 'lo.core#,..'(XX26469, 'lo.core#[]'))))))), XX26479),
    'lo.comp.errors@reportError'(XX26479, XLc, XRp, XRpx).
'lo.comp.macro^checkSoFar'('_call%6'(XV3714, XV3715, XV3716, XV3717, XV3718, XV3719), 'lo.comp.macro^checkSoFar', _) :- 'lo.comp.macro@checkSoFar'(XV3714, XV3715, XV3716, XV3717, XV3718, XV3719).
'lo.comp.macro^pickupFields'('_call%5'(XV3720, XV3721, XV3722, XV3723, XV3724), 'lo.comp.macro^pickupFields', _) :- 'lo.comp.macro@pickupFields'(XV3720, XV3721, XV3722, XV3723, XV3724).
'lo.comp.macro^algebraicFace'('_call%5'(XV3725, XV3726, XV3727, XV3728, XV3729), 'lo.comp.macro^algebraicFace', _) :- 'lo.comp.macro@algebraicFace'(XV3725, XV3726, XV3727, XV3728, XV3729).
'lo.comp.macro^listComma'('_call%3'(XV3730, XV3731, XV3732), 'lo.comp.macro^listComma', _) :- 'lo.comp.macro@listComma'(XV3730, XV3731, XV3732).
'lo.comp.macro^wrapConstraints'('_call%4'(XV3733, XV3734, XV3735, XV3736), 'lo.comp.macro^wrapConstraints', _) :- 'lo.comp.macro@wrapConstraints'(XV3733, XV3734, XV3735, XV3736).
'lo.comp.macro^wrapQuants'('_call%4'(XV3737, XV3738, XV3739, XV3740), 'lo.comp.macro^wrapQuants', _) :- 'lo.comp.macro@wrapQuants'(XV3737, XV3738, XV3739, XV3740).
'lo.comp.macro^typeRule'('_call%6'(XV3741, XV3742, XV3743, XV3744, XV3745, XV3746), 'lo.comp.macro^typeRule', _) :- 'lo.comp.macro@typeRule'(XV3741, XV3742, XV3743, XV3744, XV3745, XV3746).
'lo.comp.macro@markPublic'('lo.comp.macro#markPublic') :- !.
'lo.comp.macro@markPrivate'('lo.comp.macro#markPrivate') :- !.
'lo.comp.macro@noMark'('lo.comp.macro#noMark') :- !.
'lo.comp.macro^mark'('_call%4'(XV3747, XV3748, XV3749, XV3750), 'lo.comp.macro^mark', _) :- 'lo.comp.macro@mark'(XV3747, XV3748, XV3749, XV3750).
'lo.comp.macro^bodyRule'('_call%4'(XV3751, XV3752, XV3753, XV3754), 'lo.comp.macro^bodyRule', _) :- 'lo.comp.macro@bodyRule'(XV3751, XV3752, XV3753, XV3754).
'lo.comp.macro^classType'('_call%4'(XV3755, XV3756, XV3757, XV3758), 'lo.comp.macro^classType', _) :- 'lo.comp.macro@classType'(XV3755, XV3756, XV3757, XV3758).
'lo.comp.macro^genAnonArgs'('_call%2'(XV3759, XV3760), 'lo.comp.macro^genAnonArgs', _) :- 'lo.comp.macro@genAnonArgs'(XV3759, XV3760).
'lo.comp.macro^genFieldArgs'('_call%3'(XV3761, XV3762, XV3763), 'lo.comp.macro^genFieldArgs', _) :- 'lo.comp.macro@genFieldArgs'(XV3761, XV3762, XV3763).
'lo.comp.macro^convertConstructor'('_call%7'(XV3764, XV3765, XV3766, XV3767, XV3768, XV3769, XV3770), 'lo.comp.macro^convertConstructor', _) :- 'lo.comp.macro@convertConstructor'(XV3764, XV3765, XV3766, XV3767, XV3768, XV3769, XV3770).
'lo.comp.macro^convertConstructors'('_call%7'(XV3771, XV3772, XV3773, XV3774, XV3775, XV3776, XV3777), 'lo.comp.macro^convertConstructors', _) :- 'lo.comp.macro@convertConstructors'(XV3771, XV3772, XV3773, XV3774, XV3775, XV3776, XV3777).
'lo.comp.macro^convertAlgebraic'('_call%10'(XV3778, XV3779, XV3780, XV3781, XV3782, XV3783, XV3784, XV3785, XV3786, XV3787), 'lo.comp.macro^convertAlgebraic', _) :- 'lo.comp.macro@convertAlgebraic'(XV3778, XV3779, XV3780, XV3781, XV3782, XV3783, XV3784, XV3785, XV3786, XV3787).
'lo.comp.macro^generateAnnotations'('_call%5'(XV3788, XV3789, XV3790, XV3791, XV3792), 'lo.comp.macro^generateAnnotations', _) :- 'lo.comp.macro@generateAnnotations'(XV3788, XV3789, XV3790, XV3791, XV3792).
'lo.comp.macro^macroRewrite'('_call%4'(XV3793, XV3794, XV3795, XV3796), 'lo.comp.macro^macroRewrite', _) :- 'lo.comp.macro@macroRewrite'(XV3793, XV3794, XV3795, XV3796).
'lo.comp.macro^hasType'('_call%4'(XV3797, XV3798, XV3799, XV3800), 'lo.comp.macro^hasType', _) :- 'lo.comp.macro@hasType'(XV3797, XV3798, XV3799, XV3800).
'lo.comp.macro^funType'('_call%4'(XV3801, XV3802, XV3803, XV3804), 'lo.comp.macro^funType', _) :- 'lo.comp.macro@funType'(XV3801, XV3802, XV3803, XV3804).
'lo.comp.macro^genericType'('_call%4'(XV3805, XV3806, XV3807, XV3808), 'lo.comp.macro^genericType', _) :- 'lo.comp.macro@genericType'(XV3805, XV3806, XV3807, XV3808).
