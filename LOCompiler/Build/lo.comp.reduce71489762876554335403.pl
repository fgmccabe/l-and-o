'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.reduce's'0.0.1'n3o3'()3'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.misc'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'s\"I1'reduceClause'FT1t'lo.comp.term*clse't'lo.comp.term*clse'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.reduce@init'():- !.
'lo.comp.reduce@reduceTerm'('lo.comp.term#anon', XQ, 'lo.comp.term#anon', XPre, XPre, XPost, XPost, XQ).
'lo.comp.reduce@reduceTerm'('lo.comp.term#varbl'(XId), XQ, 'lo.comp.term#varbl'(XId), XPre, XPre, XPost, XPost, XQ).
'lo.comp.reduce@reduceTerm'(XT, XQ, XT, XPre, XPre, XPost, XPost, XQ):- 'lo.comp.term@isGroundTerm'(XT).
'lo.comp.reduce@reduceTerm'('lo.comp.term#cons'(XOp, XEls), XQ, XVV, XPre, XPrx, 'lo.core#,..'('lo.comp.term#unfy'('lo.core#none', XVV, 'lo.comp.term#cons'(XOp, XNEls)), XPost), XPsx, XQx):- 'lo.comp.term@genVr'("$%", XXd37571),
    XVV = XXd37571,
    'lo.comp.reduce@reduceTerms'(XEls, XQ, XNEls, XPre, XPrx, XPost, XPsx, XQx).
'lo.comp.reduce@reduceTerms'('lo.core#[]', XQ, 'lo.core#[]', XPre, XPre, XPost, XPost, XQ).
'lo.comp.reduce@reduceTerms'('lo.core#,..'(XA, XL), XQ, 'lo.core#,..'(XNA, XNL), XPr, XRpx, XPo, XPsx, XQx):- 'lo.comp.reduce@reduceTerm'(XA, XQ, XNA, XPr, XPr0, XPo, XPo0, XQ0),
    'lo.comp.reduce@reduceTerms'(XL, XQ0, XNL, XPr0, XPrx, XPo0, XPsx, XQx).
'lo.comp.reduce@reduceArg'('lo.comp.term#anon', XQ, 'lo.comp.term#anon', XPre, XPre, XPost, XPost, XQ).
'lo.comp.reduce@reduceArg'('lo.comp.term#varbl'(XId), XQ, 'lo.comp.term#varbl'(XId), XPre, XPre, XPost, XPost, XQ).
'lo.comp.reduce@reduceArg'('lo.comp.term#intgr'(XIx), XQ, 'lo.comp.term#intgr'(XIx), XPre, XPre, XPost, XPost, XQ).
'lo.comp.reduce@reduceArg'('lo.comp.term#flot'(XDx), XQ, 'lo.comp.term#flot'(XDx), XPre, XPre, XPost, XPost, XQ).
'lo.comp.reduce@reduceArg'('lo.comp.term#enum'(XId), XQ, 'lo.comp.term#enum'(XId), XPre, XPre, XPost, XPost, XQ).
'lo.comp.reduce@reduceArg'('lo.comp.term#strng'(XSx), XQ, 'lo.comp.term#strng'(XSx), XPre, XPre, XPost, XPost, XQ).
'lo.comp.reduce@reduceArg'('lo.comp.term#cons'(XOp, XEls), XQ, 'lo.comp.term#cons'(XOp, XNEls), XPre, XPrx, XPost, XPsx, XQx):- 'lo.comp.reduce@reduceTerms'(XEls, XQ, XNEls, XPre, XPrx, XPost, XPsx, XQx).
'lo.comp.reduce@reduceArg'('lo.comp.term#prg'(XNm, XAr), XQ, 'lo.comp.term#prg'(XNm, XAr), XPre, XPre, XPost, XPost, XQ).
'lo.comp.reduce@reduceArg'('lo.comp.term#strct'(XNm, XAr), XQ, 'lo.comp.term#strct'(XNm, XAr), XPre, XPre, XPost, XPost, XQ).
'lo.comp.reduce@reduceArgs'('lo.core#[]', XQ, 'lo.core#[]', 'lo.core#[]', 'lo.core#[]', XQ).
'lo.comp.reduce@reduceArgs'('lo.core#,..'(XA, XL), XQ, 'lo.core#,..'(XNA, XNL), XPr, XPo, XQx):- 'lo.comp.reduce@reduceArg'(XA, XQ, XNA, XPr, XPr0, XPo, XPo0, XQ0),
    'lo.comp.reduce@reduceArgs'(XL, XQ0, XNL, XPr0, XPo0, XQx).
'lo.comp.reduce@reduceBildTerm'('lo.comp.term#anon', XQ, 'lo.comp.term#anon', XPre, XPre, XPost, XPost, XQ).
'lo.comp.reduce@reduceBildTerm'('lo.comp.term#varbl'(XId), XQ, 'lo.comp.term#varbl'(XId), XPre, XPre, XPost, XPost, XQ).
'lo.comp.reduce@reduceBildTerm'(XT, XQ, XT, XPre, XPre, XPost, XPost, XQ):- 'lo.comp.term@isGroundTerm'(XT).
'lo.comp.reduce@reduceBildTerm'('lo.comp.term#cons'(XOp, XEls), XQ, XVV, XPre, XPrx, XPost, XPsx, XQx):- 'lo.comp.term@genVr'("$%", XXd37572),
    XVV = XXd37572,
    'lo.comp.reduce@reduceBildTerms'(XEls, XQ, XNEls, XPre, 'lo.core#,..'('lo.comp.term#unfy'('lo.core#none', XVV, 'lo.comp.term#cons'(XOp, XNEls)), XPrx), XPost, XPsx, XQx).
'lo.comp.reduce@reduceBildTerms'('lo.core#[]', XQ, 'lo.core#[]', XPre, XPre, XPost, XPost, XQ).
'lo.comp.reduce@reduceBildTerms'('lo.core#,..'(XA, XL), XQ, 'lo.core#,..'(XNA, XNL), XPr, XPrx, XPo, XPsx, XQx):- 'lo.comp.reduce@reduceBildTerm'(XA, XQ, XNA, XPr, XPr0, XPo, XPo0, XQ0),
    'lo.comp.reduce@reduceBildTerms'(XL, XQ0, XNL, XPr0, XPrx, XPo0, XPsx, XQx).
'lo.comp.reduce@reduceBildArg'('lo.comp.term#anon', XQ, 'lo.comp.term#anon', XPre, XPre, XPost, XPost, XQ).
'lo.comp.reduce@reduceBildArg'('lo.comp.term#varbl'(XId), XQ, 'lo.comp.term#varbl'(XId), XPre, XPre, XPost, XPost, XQ).
'lo.comp.reduce@reduceBildArg'(XT, XQ, XT, XPre, XPre, XPost, XPost, XQ):- 'lo.comp.term@isGroundTerm'(XT).
'lo.comp.reduce@reduceBildArg'('lo.comp.term#cons'(XOp, XEls), XQ, 'lo.comp.term#cons'(XOp, XNEls), XPre, XPrx, XPost, XPsx, XQx):- 'lo.comp.reduce@reduceBildTerms'(XEls, XQ, XNEls, XPre, XPrx, XPost, XPsx, XQx).
'lo.comp.reduce@reduceBildArgs'('lo.core#[]', XQ, 'lo.core#[]', XPre, XPre, XPost, XPost, XQ).
'lo.comp.reduce@reduceBildArgs'('lo.core#,..'(XA, XL), XQ, 'lo.core#,..'(XNA, XNL), XPr, XPrx, XPo, XPox, XQx):- 'lo.comp.reduce@reduceBildArg'(XA, XQ, XNA, XPr, XPr0, XPo, XPo0, XQ0),
    'lo.comp.reduce@reduceBildArgs'(XL, XQ0, XNL, XPr0, XPrx, XPo0, XPox, XQx).
'lo.comp.reduce@reducePred'('lo.comp.term#call'(XTLc, XOp, XArgs), XQ, 'lo.comp.term#call'(XTLc, XNOp, XNArgs), XPre, XPrx, XPost, XPsx, XQx):- 'lo.comp.reduce@reduceBildArg'(XOp, XQ, XNOp, XPre, XPr0, XPost, XPs0, XQ0),
    'lo.comp.reduce@reduceBildArgs'(XArgs, XQ0, XNArgs, XPr0, XPrx, XPs0, XPsx, XQx).
'lo.comp.reduce@reducePred'('lo.comp.term#ecall'(XTLc, XOp, XArgs), XQ, 'lo.comp.term#ecall'(XTLc, XOp, XNArgs), XPr, XPrx, XPs, XPsx, XQx):- 'lo.comp.reduce@reduceBildArgs'(XArgs, XQ, XNArgs, XPr, XPrx, XPs, XPsx, XQx).
'lo.comp.reduce@reducePred'('lo.comp.term#ocall'(XTLc, XCl, XLb, XTh), XQ, 'lo.comp.term#ocall'(XTLc, XNCl, XNLb, XNTh), XPre, XPrx, XPost, XPsx, XQx):- 'lo.comp.reduce@reduceBildArg'(XCl, XQ, XNCl, XPre, XPr0, XPost, XPs0, XQ0),
    'lo.comp.reduce@reduceBildArg'(XLb, XQ0, XNLb, XPr0, XPr1, XPs0, XPs1, XQ1),
    'lo.comp.reduce@reduceBildArg'(XTh, XQ1, XNTh, XPr1, XPrx, XPs1, XPsx, XQx).
'lo.comp.reduce@reducePred'('lo.comp.term#unfy'(XTLc, XL, XR), XQ, 'lo.comp.term#unfy'(XTLc, XNL, XNR), XPre, XPrx, XPost, XPsx, XQx):- 'lo.comp.reduce@reduceBildArg'(XL, XQ, XNL, XPre, XPr0, XPost, XPs0, XQ0),
    'lo.comp.reduce@reduceBildArg'(XR, XQ0, XNR, XPr0, XPrx, XPs0, XPsx, XQx).
'lo.comp.reduce@reducePred'('lo.comp.term#neck', XQ, 'lo.comp.term#neck', XPr, XPr, XPs, XPs, XQ).
'lo.comp.reduce@reducePred'('lo.comp.term#fail', XQ, 'lo.comp.term#fail', XPr, XPr, XPs, XPs, XQ).
'lo.comp.reduce@reducePred'('lo.comp.term#except'(XTLc, XE), XQ, 'lo.comp.term#except'(XTLc, XNE), XPre, XPrx, XPos, XPsx, XQx):- 'lo.comp.reduce@reduceBildArg'(XE, XQ, XNE, XPre, XPrx, XPost, XPsx, XQx).
'lo.comp.reduce@reduceBody'('lo.core#[]', XQ, 'lo.core#[]', XQ).
'lo.comp.reduce@reduceBody'('lo.core#,..'(XP, XL), XQ, XPre, XNQ):- 'lo.comp.reduce@reducePred'(XP, XQ, XNP, XPre, XPs, XPs, 'lo.core#,..'(XNP, XPsx), XQ1),
    'lo.comp.reduce@reduceBody'(XL, XQ1, XPsx, XNQ).
'lo.comp.reduce@reduceClause'('lo.comp.term#clse'(XQ, XNm, XArgs, XBody), 'lo.comp.term#clse'(XNQ, XNm, XNArgs, XXd37578)):- 'lo.comp.reduce@one275'(XQ1, XPost, XGuard, XNArgs, XQ, XArgs),
    'lo.comp.reduce@reduceBody'(XBody, XQ1, XNBody, XNQ),
    !,
    'lo.list@<>'(XPost, XNBody, XXd37577),
    'lo.list@<>'(XGuard, XXd37577, XXd37578).
'lo.comp.reduce@reduceClause'(_, _):- raise_exception('error'("lo.comp.reduce@reduceClause", 10, 3, 155)).
'lo.comp.reduce^reduceTerm'('_call%8'(XV29611, XV29612, XV29613, XV29614, XV29615, XV29616, XV29617, XV29618), 'lo.comp.reduce^reduceTerm', _):- 'lo.comp.reduce@reduceTerm'(XV29611, XV29612, XV29613, XV29614, XV29615, XV29616, XV29617, XV29618).
'lo.comp.reduce^reduceTerms'('_call%8'(XV29619, XV29620, XV29621, XV29622, XV29623, XV29624, XV29625, XV29626), 'lo.comp.reduce^reduceTerms', _):- 'lo.comp.reduce@reduceTerms'(XV29619, XV29620, XV29621, XV29622, XV29623, XV29624, XV29625, XV29626).
'lo.comp.reduce^reduceArg'('_call%8'(XV29627, XV29628, XV29629, XV29630, XV29631, XV29632, XV29633, XV29634), 'lo.comp.reduce^reduceArg', _):- 'lo.comp.reduce@reduceArg'(XV29627, XV29628, XV29629, XV29630, XV29631, XV29632, XV29633, XV29634).
'lo.comp.reduce^reduceArgs'('_call%6'(XV29635, XV29636, XV29637, XV29638, XV29639, XV29640), 'lo.comp.reduce^reduceArgs', _):- 'lo.comp.reduce@reduceArgs'(XV29635, XV29636, XV29637, XV29638, XV29639, XV29640).
'lo.comp.reduce^reduceBildTerm'('_call%8'(XV29641, XV29642, XV29643, XV29644, XV29645, XV29646, XV29647, XV29648), 'lo.comp.reduce^reduceBildTerm', _):- 'lo.comp.reduce@reduceBildTerm'(XV29641, XV29642, XV29643, XV29644, XV29645, XV29646, XV29647, XV29648).
'lo.comp.reduce^reduceBildTerms'('_call%8'(XV29649, XV29650, XV29651, XV29652, XV29653, XV29654, XV29655, XV29656), 'lo.comp.reduce^reduceBildTerms', _):- 'lo.comp.reduce@reduceBildTerms'(XV29649, XV29650, XV29651, XV29652, XV29653, XV29654, XV29655, XV29656).
'lo.comp.reduce^reduceBildArg'('_call%8'(XV29657, XV29658, XV29659, XV29660, XV29661, XV29662, XV29663, XV29664), 'lo.comp.reduce^reduceBildArg', _):- 'lo.comp.reduce@reduceBildArg'(XV29657, XV29658, XV29659, XV29660, XV29661, XV29662, XV29663, XV29664).
'lo.comp.reduce^reduceBildArgs'('_call%8'(XV29665, XV29666, XV29667, XV29668, XV29669, XV29670, XV29671, XV29672), 'lo.comp.reduce^reduceBildArgs', _):- 'lo.comp.reduce@reduceBildArgs'(XV29665, XV29666, XV29667, XV29668, XV29669, XV29670, XV29671, XV29672).
'lo.comp.reduce^reducePred'('_call%8'(XV29673, XV29674, XV29675, XV29676, XV29677, XV29678, XV29679, XV29680), 'lo.comp.reduce^reducePred', _):- 'lo.comp.reduce@reducePred'(XV29673, XV29674, XV29675, XV29676, XV29677, XV29678, XV29679, XV29680).
'lo.comp.reduce^reduceBody'('_call%4'(XV29681, XV29682, XV29683, XV29684), 'lo.comp.reduce^reduceBody', _):- 'lo.comp.reduce@reduceBody'(XV29681, XV29682, XV29683, XV29684).
'lo.comp.reduce@one275'(XQ1, XPost, XGuard, XNArgs, XQ, XArgs):- 'lo.comp.reduce@reduceArgs'(XArgs, XQ, XNArgs, XGuard, XPost, XQ1),
    !.
'lo.comp.reduce^reduceClause'('_call%2'(XV29685, XV29686), 'lo.comp.reduce^reduceClause', _):- 'lo.comp.reduce@reduceClause'(XV29685, XV29686).
