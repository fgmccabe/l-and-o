'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.grammar's'0.0.1'n10o10'()10'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'n2o2'import'e'private'n2o2'pkg's'lo.resources'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.token'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.operators'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.ast'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.abstract'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.lexer'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'s\"I5'term'GT5it'lo.comp.ast*ast't'lo.comp.grammar*tokMark't'lo.comp.errors*report't'lo.comp.errors*report'Lt'lo.comp.token*token''parse'GT3t'lo.comp.ast*ast't'lo.comp.errors*report't'lo.comp.errors*report'Lt'lo.comp.token*token''parseFile'FT3t'lo.uri*uri't'lo.comp.errors*report't'lo.comp.errors*report't'lo.comp.ast*ast''parseSrc'FT4t'lo.uri*uri'Lit'lo.comp.errors*report't'lo.comp.errors*report't'lo.comp.ast*ast''nextTerm'PT5Lt'lo.comp.token*token't'lo.comp.ast*ast'Lt'lo.comp.token*token't'lo.comp.errors*report't'lo.comp.errors*report'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.grammar@init'():- !.
'lo.comp.grammar@tupleize'(XTerm, 'lo.core#,..'(XL, XXd39076)):- 'lo.comp.abstract@isBinary'(XTerm, ",", X_33637, XL, XR),
    !,
    'lo.comp.grammar@tupleize'(XR, XXd39076).
'lo.comp.grammar@tupleize'(XT, 'lo.core#,..'(XT, 'lo.core#[]')):- !.
'lo.comp.grammar@tupleize'(_, _):- raise_exception('error'("lo.comp.grammar@tupleize", 170, 3, 63)).
'lo.comp.grammar@checkFor'(XStIn1900, XNStrm1679, XTk, XLc, X_33640, XRp, XRp):- ocall('_hdtl%3'(XStIn1900, 'lo.comp.token#tok'(XTk, XLc), XNStrm1679),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@checkFor'(XStIn1901, XStIn1901, XTk, XLc, XMsg, XRp, XRpx):- 'lo.comp.grammar@Hed114'(XStIn1901, XNStrm1680, XNStrm1680, XXd39080, XLc, X_33641, XHedStrm114),
    'lo.comp.errors@reportError'(XMsg, XLc, XRp, XRpx).
'lo.comp.grammar#otherMark'('otherMark%1'('lo.comp.grammar@otherMark')):- !.
'lo.comp.grammar#endBrce'('endBrce%1'('lo.comp.grammar@endBrce')):- !.
'lo.comp.grammar@followsInfix'(XStIn1902, XNStrm1681, XPr):- ocall('_hdtl%3'(XStIn1902, 'lo.comp.token#tok'('lo.comp.token#idTok'(XOp), X_33642), XNStrm1681),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.grammar@cond403'(X_33644, XPr, X_33643, XOpPr, XOp).
'lo.comp.grammar@followsInfix'(XStIn1903, XNStrm1682, X_33645):- ocall('_hdtl%3'(XStIn1903, 'lo.comp.token#tok'('lo.comp.token#lpar', X_33646), XNStrm1682),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@followsInfix'(XStIn1904, XNStrm1683, X_33647):- ocall('_hdtl%3'(XStIn1904, 'lo.comp.token#tok'('lo.comp.token#lbra', X_33648), XNStrm1683),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@followsInfix'(XStIn1905, XNStrm1684, X_33649):- ocall('_hdtl%3'(XStIn1905, 'lo.comp.token#tok'('lo.comp.token#lbrce', X_33650), XNStrm1684),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@followsInfix'(XStIn1906, XNStrm1685, X_33651):- ocall('_hdtl%3'(XStIn1906, 'lo.comp.token#tok'('lo.comp.token#lqpar', X_33652), XNStrm1685),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@followsInfix'(XStIn1907, XNStrm1686, X_33653):- ocall('_hdtl%3'(XStIn1907, 'lo.comp.token#tok'('lo.comp.token#stringTok'(X_33654), X_33655), XNStrm1686),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@followsInfix'(XStIn1908, XNStrm1687, X_33656):- ocall('_hdtl%3'(XStIn1908, 'lo.comp.token#tok'('lo.comp.token#idQTok'(X_33657), X_33658), XNStrm1687),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@followsInfix'(XStIn1909, XNStrm1688, X_33659):- ocall('_hdtl%3'(XStIn1909, 'lo.comp.token#tok'('lo.comp.token#intTok'(X_33660), X_33661), XNStrm1688),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@followsInfix'(XStIn1910, XNStrm1689, X_33662):- ocall('_hdtl%3'(XStIn1910, 'lo.comp.token#tok'('lo.comp.token#fltTok'(X_33663), X_33664), XNStrm1689),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@legalInfixOp'(XStIn1911, XNStrm1690, XOp, XPr, XLeftPr, XOPr, XRPr):- ocall('_hdtl%3'(XStIn1911, 'lo.comp.token#tok'('lo.comp.token#idTok'(XOp), X_33665), XNStrm1690),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.operators@infixOp'(XOp, XLPr, XOPr, XRPr),
    'lo.core@=<'('lo.core$comp$lo.core*integer', XOPr, XPr),
    ocall('>=%2'(XLPr, XLeftPr),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer'),
    'lo.comp.grammar@Hed115'(XNStrm1690, XStx1782, XRPr).
'lo.comp.grammar@checkForTerminator'(XStIn1912, XNStrm1691, X_33666, XRp, XRp):- ocall('_hdtl%3'(XStIn1912, 'lo.comp.token#tok'('lo.comp.token#period', X_33667), XNStrm1691),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@checkForTerminator'(XStIn1913, XStIn1913, X_33668, XRp, XRp):- XStIn1913 = X_33669,
    ocall('_eof%1'(X_33669),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@checkForTerminator'(XStIn1914, XStIn1914, X_33670, XRp, XRp):- 'lo.comp.grammar@Hed116'(XStIn1914, XNStrm1692, XNStrm1692, XXd39098, X_33671, XHedStrm116).
'lo.comp.grammar@checkForTerminator'(XStIn1915, XStIn1915, 'lo.comp.grammar#endBrce', XRp, XRp).
'lo.comp.grammar@checkForTerminator'(XStIn1916, XStx1783, X_33672, XRp, XRpx):- 'lo.comp.grammar@Hed117'(XStIn1916, XNStrm1693, XNStrm1693, XXd39099, XLc, X_33673, XHedStrm117),
    'lo.comp.grammar@checkFor'(XStIn1916, XStx1783, 'lo.comp.token#period', XLc, "missing terminator", XRp, XRpx).
'lo.comp.grammar@termRight'(XStIn1917, XStx1786, XPr, XLeft, XTerm, XLeftPr, X_33674, XMark, XRp, XRpx):- 'lo.comp.grammar@legalInfixOp'(XStIn1917, XStx1784, XOp, XPr, XLeftPr, XInfPr, XRightPr),
    'lo.comp.grammar@term'(XStx1784, XStx1785, XRightPr, XRight, XLMark, XRp, XRp0),
    ocall('loc%1'(XXV5257),XLeft,XLeft),
    ocall('merge%1'(XXV5256),XXV5257,XXV5257),
    ocall('loc%1'(XXV5255),XRight,XRight),
    ocall('_call%2'(XXV5255, XXe4906),XXV5256,XXV5256),
    'lo.comp.abstract@binary'(XXe4906, XOp, XLeft, XRight, XXd39100),
    XM = XXd39100,
    'lo.comp.grammar@termRight'(XStx1785, XStx1786, XPr, XM, XTerm, XInfPr, XLMark, XMark, XRp0, XRpx).
'lo.comp.grammar@termRight'(XStIn1918, XStx1787, XPr, XLeft, XTerm, XLeftPr, X_33676, XMark, XRp, XRpx):- ocall('_hdtl%3'(XStIn1918, 'lo.comp.token#tok'('lo.comp.token#idTok'(XOp), XLc), XNStrm1694),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.operators@postfixOp'(XOp, XLPr, XPostPr),
    'lo.core@=<'('lo.core$comp$lo.core*integer', XPostPr, XPr),
    ocall('>=%2'(XLPr, XLeftPr),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer'),
    ocall('loc%1'(XXV5259),XLeft,XLeft),
    ocall('merge%1'(XXV5258),XXV5259,XXV5259),
    ocall('_call%2'(XLc, XXe4907),XXV5258,XXV5258),
    'lo.comp.abstract@unary'(XXe4907, XOp, XLeft, XXd39103),
    XM = XXd39103,
    'lo.comp.grammar@termRight'(XNStrm1694, XStx1787, XPr, XM, XTerm, XPostPr, 'lo.comp.grammar#otherMark', XMark, XRp, XRpx).
'lo.comp.grammar@termRight'(XStIn1919, XStIn1919, X_33678, XTerm, XTerm, X_33679, XMark, XMark, XRp, XRp).
'lo.comp.grammar@interpolateSegment'(XLc, XText, "", XRp, XRpx, XXd39105):- 'lo.comp.lexer@subTokenize'(XLc, XText, XXd39104),
    'lo.comp.grammar@term'(XXd39104, XStx1788, 2000, XArg, X_33681, XRp, XRpx),
    XStx1788 = X_33682,
    ocall('_eof%1'(X_33682),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    X_33680 = XStx1788,
    !,
    'lo.comp.abstract@unary'(XLc, "disp", XArg, XXd39105).
'lo.comp.grammar@interpolateSegment'(XLc, XText, XFmt, XRp, XRpx, XXd39108):- 'lo.comp.lexer@subTokenize'(XLc, XText, XXd39106),
    'lo.comp.grammar@term'(XXd39106, XStx1789, 2000, XArg, X_33684, XRp, XRpx),
    XStx1789 = X_33685,
    ocall('_eof%1'(X_33685),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    X_33683 = XStx1789,
    !,
    'lo.comp.abstract@binary'(XLc, "frmt", XArg, 'lo.comp.ast#strg'(XLc, XFmt), XXd39108).
'lo.comp.grammar@interpolateSegment'(_, _, _, _, _, _):- raise_exception('error'("lo.comp.grammar@interpolateSegment", 204, 3, 116)).
'lo.comp.grammar@parseSegments'('lo.core#[]', XRp, XRp, 'lo.core#[]'):- !.
'lo.comp.grammar@parseSegments'('lo.core#,..'('lo.comp.token#segment'(XLc, XStr), XMore), XRp, XRpx, 'lo.core#,..'(XXd39110, XXd39111)):- !,
    'lo.comp.abstract@unary'(XLc, "ss", 'lo.comp.ast#strg'(XLc, XStr), XXd39110),
    'lo.comp.grammar@parseSegments'(XMore, XRp, XRpx, XXd39111).
'lo.comp.grammar@parseSegments'('lo.core#,..'('lo.comp.token#interpolate'(XLc, XExp, XFmt), XMore), XRp, XRpx, 'lo.core#,..'(XXd39113, XXd39114)):- !,
    'lo.comp.grammar@interpolateSegment'(XLc, XExp, XFmt, XRp, XRp0, XXd39113),
    'lo.comp.grammar@parseSegments'(XMore, XRp0, XRpx, XXd39114).
'lo.comp.grammar@parseSegments'(_, _, _, _):- raise_exception('error'("lo.comp.grammar@parseSegments", 197, 3, 29)).
'lo.comp.grammar@parseString'(XLc, 'lo.core#[]', XRp, XRp, 'lo.comp.ast#strg'(XLc, "")):- !.
'lo.comp.grammar@parseString'(X_33690, 'lo.core#,..'('lo.comp.token#segment'(XLc, XStr), 'lo.core#[]'), XRp, XRp, 'lo.comp.ast#strg'(XLc, XStr)):- !.
'lo.comp.grammar@parseString'(XLc, XSegments, XRp, XRpx, XXd39121):- !,
    'lo.comp.grammar@parseSegments'(XSegments, XRp, XRpx, XXd39118),
    'lo.comp.abstract@unary'(XLc, "ssSeq", 'lo.comp.ast#tupl'(XLc, "[]", XXd39118), XXd39120),
    'lo.comp.abstract@unary'(XLc, "formatSS", XXd39120, XXd39121).
'lo.comp.grammar@parseString'(_, _, _, _, _):- raise_exception('error'("lo.comp.grammar@parseString", 191, 3, 39)).
'lo.comp.grammar@terms'(XStIn1920, XStx1791, 'lo.core#,..'(XT, XR), XRp, XRpx):- 'lo.comp.grammar@parse'(XStIn1920, XStx1790, XT, XRp, XRp0),
    'lo.comp.grammar@terms'(XStx1790, XStx1791, XR, XRp0, XRpx).
'lo.comp.grammar@terms'(XStIn1921, XStIn1921, 'lo.core#[]', XRp, XRp).
'lo.comp.grammar@termArgs'(XStIn1922, XStx1792, XLft, XTerm, X_33693, XMark, XRp, XRpx):- ocall('_hdtl%3'(XStIn1922, 'lo.comp.token#tok'('lo.comp.token#lpar', XLc0), XNStrm1695),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm1695, 'lo.comp.token#tok'('lo.comp.token#rpar', XLc1), XNStrm1696),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('loc%1'(XXV5261),XLft,XLft),
    ocall('merge%1'(XXV5260),XXV5261,XXV5261),
    ocall('_call%2'(XLc1, XXe4908),XXV5260,XXV5260),
    XLc = XXe4908,
    ocall('merge%1'(XXV5262),XLc0,XLc0),
    ocall('_call%2'(XLc1, XXe4909),XXV5262,XXV5262),
    'lo.comp.grammar@termArgs'(XNStrm1696, XStx1792, 'lo.comp.ast#appl'(XLc, XLft, 'lo.comp.ast#tupl'(XXe4909, "()", 'lo.core#[]')), XTerm, 'lo.comp.grammar#otherMark', XMark, XRp, XRpx).
'lo.comp.grammar@termArgs'(XStIn1923, XStx1795, XLft, XTerm, X_33695, XMark, XRp, XRpx):- ocall('_hdtl%3'(XStIn1923, 'lo.comp.token#tok'('lo.comp.token#lpar', XLc0), XNStrm1697),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.grammar@term'(XNStrm1697, XStx1793, 2000, XSeq, X_33696, XRp, XRp0),
    'lo.comp.grammar@checkFor'(XStx1793, XStx1794, 'lo.comp.token#rpar', XLc1, "missing close paren", XRp0, XRp1),
    ocall('loc%1'(XXV5264),XLft,XLft),
    ocall('merge%1'(XXV5263),XXV5264,XXV5264),
    ocall('merge%1'(XXV5265),XLc0,XLc0),
    ocall('_call%2'(XLc1, XXe4910),XXV5263,XXV5263),
    ocall('_call%2'(XLc1, XXe4911),XXV5265,XXV5265),
    'lo.comp.grammar@tupleize'(XSeq, XXd39127),
    'lo.comp.grammar@termArgs'(XStx1794, XStx1795, 'lo.comp.ast#appl'(XXe4910, XLft, 'lo.comp.ast#tupl'(XXe4911, "()", XXd39127)), XTerm, 'lo.comp.grammar#otherMark', XMark, XRp1, XRpx).
'lo.comp.grammar@termArgs'(XStIn1924, XStx1796, XLft, XTerm, X_33697, XMark, XRp, XRpx):- ocall('_hdtl%3'(XStIn1924, 'lo.comp.token#tok'('lo.comp.token#lbra', XLc0), XNStrm1698),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm1698, 'lo.comp.token#tok'('lo.comp.token#rbra', XLc1), XNStrm1699),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('loc%1'(XXV5267),XLft,XLft),
    ocall('merge%1'(XXV5266),XXV5267,XXV5267),
    ocall('merge%1'(XXV5268),XLc0,XLc0),
    ocall('_call%2'(XLc1, XXe4912),XXV5266,XXV5266),
    ocall('_call%2'(XLc1, XXe4913),XXV5268,XXV5268),
    'lo.comp.grammar@termArgs'(XNStrm1699, XStx1796, 'lo.comp.ast#appl'(XXe4912, XLft, 'lo.comp.ast#tupl'(XXe4913, "[]", 'lo.core#[]')), XTerm, 'lo.comp.grammar#otherMark', XMark, XRp, XRpx).
'lo.comp.grammar@termArgs'(XStIn1925, XStx1798, XLft, 'lo.comp.ast#appl'(XLc, XLft, 'lo.comp.ast#tupl'(XTpLc, "[]", XXb19074)), X_33698, 'lo.comp.grammar#otherMark', XRp, XRpx):- ocall('_hdtl%3'(XStIn1925, 'lo.comp.token#tok'('lo.comp.token#lbra', XLc0), XNStrm1700),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.grammar@term'(XNStrm1700, XStx1797, 2000, XSeq, X_33699, XRp, XRp0),
    'lo.comp.grammar@checkFor'(XStx1797, XStx1798, 'lo.comp.token#rbra', XLc1, "missing close bracket", XRp0, XRpx),
    ocall('merge%1'(XXV5269),XLc0,XLc0),
    ocall('_call%2'(XLc1, XXe4914),XXV5269,XXV5269),
    XTpLc = XXe4914,
    ocall('loc%1'(XXV5271),XLft,XLft),
    ocall('merge%1'(XXV5270),XXV5271,XXV5271),
    ocall('_call%2'(XLc1, XXe4915),XXV5270,XXV5270),
    XLc = XXe4915,
    'lo.comp.grammar@tupleize'(XSeq, XXb19074).
'lo.comp.grammar@termArgs'(XStIn1926, XNStrm1702, XLft, 'lo.comp.ast#appl'(XLc, XLft, 'lo.comp.ast#tupl'(XTpLc, "{}", 'lo.core#[]')), X_33702, 'lo.comp.grammar#endBrce', XRp, XRp):- ocall('_hdtl%3'(XStIn1926, 'lo.comp.token#tok'('lo.comp.token#lbrce', XLc0), XNStrm1701),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm1701, 'lo.comp.token#tok'('lo.comp.token#rbrce', XLc1), XNStrm1702),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('loc%1'(XXV5273),XLft,XLft),
    ocall('merge%1'(XXV5272),XXV5273,XXV5273),
    ocall('_call%2'(XLc1, XXe4916),XXV5272,XXV5272),
    XLc = XXe4916,
    ocall('merge%1'(XXV5274),XLc0,XLc0),
    ocall('_call%2'(XLc1, XXe4917),XXV5274,XXV5274),
    XTpLc = XXe4917.
'lo.comp.grammar@termArgs'(XStIn1927, XStx1800, XLft, 'lo.comp.ast#appl'(XLc, XLft, 'lo.comp.ast#tupl'(XTpLc, "{}", XEls)), X_33705, 'lo.comp.grammar#endBrce', XRp, XRpx):- ocall('_hdtl%3'(XStIn1927, 'lo.comp.token#tok'('lo.comp.token#lbrce', XLc0), XNStrm1703),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.grammar@terms'(XNStrm1703, XStx1799, XEls, XRp, XRp0),
    'lo.comp.grammar@checkFor'(XStx1799, XStx1800, 'lo.comp.token#rbrce', XLc1, "missing close brace", XRp0, XRpx),
    ocall('loc%1'(XXV5276),XLft,XLft),
    ocall('merge%1'(XXV5275),XXV5276,XXV5276),
    ocall('_call%2'(XLc1, XXe4918),XXV5275,XXV5275),
    XLc = XXe4918,
    ocall('merge%1'(XXV5277),XLc0,XLc0),
    ocall('_call%2'(XLc1, XXe4919),XXV5277,XXV5277),
    XTpLc = XXe4919.
'lo.comp.grammar@termArgs'(XStIn1928, XStx1801, XLft, XTerm, X_33708, XMark, XRp, XRpx):- ocall('_hdtl%3'(XStIn1928, 'lo.comp.token#tok'('lo.comp.token#idTok'("."), XLc), XNStrm1704),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm1704, 'lo.comp.token#tok'('lo.comp.token#idTok'(XFld), XLcF), XNStrm1705),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('loc%1'(XXV5279),XLft,XLft),
    ocall('merge%1'(XXV5278),XXV5279,XXV5279),
    ocall('_call%2'(XLcF, XXe4920),XXV5278,XXV5278),
    'lo.comp.abstract@binary'(XXe4920, ".", XLft, 'lo.comp.ast#iden'(XLcF, XFld), XXd39143),
    'lo.comp.grammar@termArgs'(XNStrm1705, XStx1801, XXd39143, XTerm, 'lo.comp.grammar#otherMark', XMark, XRp, XRpx).
'lo.comp.grammar@termArgs'(XStIn1929, XStIn1929, XTerm, XTerm, XMark, XMark, XRp, XRp).
'lo.comp.grammar@term00'(XStIn1930, XNStrm1706, 'lo.comp.ast#iden'(XLc, XNm), 'lo.comp.grammar#otherMark', XRp, XRp):- ocall('_hdtl%3'(XStIn1930, 'lo.comp.token#tok'('lo.comp.token#idTok'(XNm), XLc), XNStrm1706),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@term00'(XStIn1931, XNStrm1707, 'lo.comp.ast#iden'(XLc, XNm), 'lo.comp.grammar#otherMark', XRp, XRp):- ocall('_hdtl%3'(XStIn1931, 'lo.comp.token#tok'('lo.comp.token#idQTok'(XNm), XLc), XNStrm1707),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@term00'(XStIn1932, XNStrm1709, 'lo.comp.ast#tupl'(XLc, "()", 'lo.core#[]'), 'lo.comp.grammar#otherMark', XRp, XRp):- ocall('_hdtl%3'(XStIn1932, 'lo.comp.token#tok'('lo.comp.token#lpar', XLc0), XNStrm1708),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm1708, 'lo.comp.token#tok'('lo.comp.token#rpar', XLc1), XNStrm1709),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('merge%1'(XXV5280),XLc0,XLc0),
    ocall('_call%2'(XLc1, XXe4921),XXV5280,XXV5280),
    XLc = XXe4921.
'lo.comp.grammar@term00'(XStIn1933, XStx1803, 'lo.comp.ast#tupl'(XLc, "()", XXb19084), 'lo.comp.grammar#otherMark', XRp, XRpx):- ocall('_hdtl%3'(XStIn1933, 'lo.comp.token#tok'('lo.comp.token#lpar', XLc0), XNStrm1710),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.grammar@term'(XNStrm1710, XStx1802, 2000, XSeq, X_33710, XRp, XRp0),
    'lo.comp.grammar@checkFor'(XStx1802, XStx1803, 'lo.comp.token#rpar', XLc1, "missing close paren", XRp0, XRpx),
    ocall('merge%1'(XXV5281),XLc0,XLc0),
    ocall('_call%2'(XLc1, XXe4922),XXV5281,XXV5281),
    XLc = XXe4922,
    'lo.comp.grammar@tupleize'(XSeq, XXb19084).
'lo.comp.grammar@term0'(XStIn1934, XNStrm1711, XXb19086, 'lo.comp.grammar#otherMark', XRp, XRpx):- ocall('_hdtl%3'(XStIn1934, 'lo.comp.token#tok'('lo.comp.token#stringTok'(XSegments), XLc), XNStrm1711),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.grammar@parseString'(XLc, XSegments, XRp, XRpx, XXb19086).
'lo.comp.grammar@term0'(XStIn1935, XNStrm1712, 'lo.comp.ast#intg'(XLc, XIx), 'lo.comp.grammar#otherMark', XRp, XRp):- ocall('_hdtl%3'(XStIn1935, 'lo.comp.token#tok'('lo.comp.token#intTok'(XIx), XLc), XNStrm1712),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@term0'(XStIn1936, XNStrm1713, 'lo.comp.ast#flot'(XLc, XDx), 'lo.comp.grammar#otherMark', XRp, XRp):- ocall('_hdtl%3'(XStIn1936, 'lo.comp.token#tok'('lo.comp.token#fltTok'(XDx), XLc), XNStrm1713),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@term0'(XStIn1937, XNStrm1715, 'lo.comp.ast#tupl'(XLc, "[]", 'lo.core#[]'), 'lo.comp.grammar#otherMark', XRp, XRp):- ocall('_hdtl%3'(XStIn1937, 'lo.comp.token#tok'('lo.comp.token#lbra', XLc0), XNStrm1714),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm1714, 'lo.comp.token#tok'('lo.comp.token#rbra', XLc1), XNStrm1715),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('merge%1'(XXV5282),XLc0,XLc0),
    ocall('_call%2'(XLc1, XXe4923),XXV5282,XXV5282),
    XLc = XXe4923.
'lo.comp.grammar@term0'(XStIn1938, XStx1805, 'lo.comp.ast#tupl'(XLc, "[]", XXb19090), 'lo.comp.grammar#otherMark', XRp, XRpx):- ocall('_hdtl%3'(XStIn1938, 'lo.comp.token#tok'('lo.comp.token#lbra', XLc0), XNStrm1716),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.grammar@term'(XNStrm1716, XStx1804, 2000, XSeq, X_33713, XRp, XRp0),
    'lo.comp.grammar@checkFor'(XStx1804, XStx1805, 'lo.comp.token#rbra', XLc1, "missing close bracket", XRp0, XRpx),
    ocall('merge%1'(XXV5283),XLc0,XLc0),
    ocall('_call%2'(XLc1, XXe4924),XXV5283,XXV5283),
    XLc = XXe4924,
    'lo.comp.grammar@tupleize'(XSeq, XXb19090).
'lo.comp.grammar@term0'(XStIn1939, XNStrm1718, 'lo.comp.ast#tupl'(XLc, "{}", 'lo.core#[]'), 'lo.comp.grammar#endBrce', XRp, XRp):- ocall('_hdtl%3'(XStIn1939, 'lo.comp.token#tok'('lo.comp.token#lbrce', XLc0), XNStrm1717),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm1717, 'lo.comp.token#tok'('lo.comp.token#rbrce', XLc1), XNStrm1718),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('merge%1'(XXV5284),XLc0,XLc0),
    ocall('_call%2'(XLc1, XXe4925),XXV5284,XXV5284),
    XLc = XXe4925.
'lo.comp.grammar@term0'(XStIn1940, XStx1807, 'lo.comp.ast#tupl'(XLc, "{}", XSeq), 'lo.comp.grammar#endBrce', XRp, XRpx):- ocall('_hdtl%3'(XStIn1940, 'lo.comp.token#tok'('lo.comp.token#lbrce', XLc0), XNStrm1719),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.grammar@terms'(XNStrm1719, XStx1806, XSeq, XRp, XRp0),
    'lo.comp.grammar@checkFor'(XStx1806, XStx1807, 'lo.comp.token#rbrce', XLc1, "missing close brace", XRp0, XRpx),
    ocall('merge%1'(XXV5285),XLc0,XLc0),
    ocall('_call%2'(XLc1, XXe4926),XXV5285,XXV5285),
    XLc = XXe4926.
'lo.comp.grammar@term0'(XStIn1941, XStx1809, XXb19094, 'lo.comp.grammar#otherMark', XRp, XRpx):- ocall('_hdtl%3'(XStIn1941, 'lo.comp.token#tok'('lo.comp.token#lqpar', XLc0), XNStrm1720),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.grammar@term'(XNStrm1720, XStx1808, 2000, XA, X_33717, XRp, XRp0),
    'lo.comp.grammar@checkFor'(XStx1808, XStx1809, 'lo.comp.token#rqpar', XLc1, "missing close quote", XRp0, XRpx),
    ocall('merge%1'(XXV5286),XLc0,XLc0),
    ocall('_call%2'(XLc1, XXe4927),XXV5286,XXV5286),
    XLc = XXe4927,
    'lo.comp.abstract@unary'(XLc, "<||>", XA, XXb19094).
'lo.comp.grammar@term0'(XStIn1942, XStx1811, XTerm, XMark, XRp, XRpx):- 'lo.comp.grammar@term00'(XStIn1942, XStx1810, XLeft, XLMark, XRp, XRp0),
    'lo.comp.grammar@termArgs'(XStx1810, XStx1811, XLeft, XTerm, XLMark, XMark, XRp0, XRpx).
'lo.comp.grammar@termLeft'(XStIn1943, XStx1812, XPr, XLeft, XOPr, XMark, XRp, XRpx):- ocall('_hdtl%3'(XStIn1943, 'lo.comp.token#tok'('lo.comp.token#idTok'(XOp), XLc), XNStrm1721),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.grammar@Neg54'(XNStrm1721, XNStrm1722, XNStrm1722, XXd39166, X_33719, XNegStrm54),
    'lo.comp.operators@prefixOp'(XOp, XOPr, XORight),
    'lo.core@=<'('lo.core$comp$lo.core*integer', XOPr, XPr),
    'lo.comp.grammar@term'(XNStrm1721, XStx1812, XORight, XArg, XMark, XRp, XRpx),
    ocall('merge%1'(XXV5288),XLc,XLc),
    ocall('loc%1'(XXV5287),XArg,XArg),
    ocall('_call%2'(XXV5287, XXe4928),XXV5288,XXV5288),
    'lo.comp.abstract@unary'(XXe4928, XOp, XArg, XXd39167),
    XLeft = XXd39167.
'lo.comp.grammar@termLeft'(XStIn1944, XStx1813, X_33721, XTerm, 0, XMark, XRp, XRpx):- 'lo.comp.grammar@term0'(XStIn1944, XStx1813, XTerm, XMark, XRp, XRpx).
'lo.comp.grammar@term'(XStIn1945, XStx1815, XPr, XTerm, XMark, XRp, XRpx):- 'lo.comp.grammar@termLeft'(XStIn1945, XStx1814, XPr, XLeft, XLftPr, XLLend, XRp, XRp0),
    'lo.comp.grammar@termRight'(XStx1814, XStx1815, XPr, XLeft, XTerm, XLftPr, XLLend, XMark, XRp0, XRpx).
'lo.comp.grammar@parse'(XStIn1946, XStx1817, XTerm, XRp, XRpx):- 'lo.comp.grammar@One38'(XStIn1946, XDjOut126, XStx1816, XRp0, XRp, XMark, XTerm),
    'lo.comp.grammar@checkForTerminator'(XDjOut126, XStx1817, XMark, XRp0, XRpx).
'lo.comp.grammar@parseFile'(XU, XRp, XRpx, XTerm):- 'lo.comp.lexer@tokenizeFile'(XU, XXd39168),
    'lo.comp.grammar@parse'(XXd39168, XStx1818, XTerm, XRp, XRp0),
    XRest = XStx1818,
    'lo.comp.grammar@or192'(XXV5289, XXd39169, X_33723, XTk, XRpx, XRp0, XRest),
    !.
'lo.comp.grammar@parseFile'(_, _, _, _):- raise_exception('error'("lo.comp.grammar@parseFile", 14, 3, 175)).
'lo.comp.grammar@parseSrc'(XU, XCodes, XRp, XRpx, XTerm):- 'lo.uri@getUriPath'(XU, XXd39170),
    'lo.comp.lexer@tokenCodes'(XCodes, XXd39170, XXd39171),
    'lo.comp.grammar@parse'(XXd39171, XStx1819, XTerm, XRp, XRpx),
    XStx1819 = X_33725,
    ocall('_eof%1'(X_33725),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    X_33724 = XStx1819,
    !.
'lo.comp.grammar@parseSrc'(_, _, _, _, _):- raise_exception('error'("lo.comp.grammar@parseSrc", 19, 3, 93)).
'lo.comp.grammar@nextTerm'(XToks, XTerm, XRest, XRp, XRpx):- 'lo.comp.grammar@parse'(XToks, XStx1820, XTerm, XRp, XRpx),
    XRest = XStx1820.
'lo.comp.grammar^tupleize'('_call%2'(XV30398, XV30399), 'lo.comp.grammar^tupleize', _):- 'lo.comp.grammar@tupleize'(XV30398, XV30399).
'lo.comp.grammar@Hed114'(XHedStrm114, XNStrm1680, XNStrm1680, XXd39080, XLc, X_33641, XHedStrm114):- ocall('_hdtl%3'(XHedStrm114, 'lo.comp.token#tok'(X_33641, XLc), XNStrm1680),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar^checkFor'('_call%7'(XV30400, XV30401, XV30402, XV30403, XV30404, XV30405, XV30406), 'lo.comp.grammar^checkFor', _):- 'lo.comp.grammar@checkFor'(XV30400, XV30401, XV30402, XV30403, XV30404, XV30405, XV30406).
'lo.comp.grammar@neg326'(X_33644, XOp):- 'lo.comp.operators@isOperator'(XOp, X_33644),
    !,
    fail.
'lo.comp.grammar@neg326'(X_33644, XOp).
'lo.comp.grammar@cond403'(X_33644, XPr, X_33643, XOpPr, XOp):- 'lo.comp.operators@prefixOp'(XOp, XOpPr, X_33643),
    !,
    'lo.core@=<'('lo.core$comp$lo.core*integer', XOpPr, XPr).
'lo.comp.grammar@cond403'(X_33644, XPr, X_33643, XOpPr, XOp):- 'lo.comp.grammar@neg326'(X_33644, XOp).
'lo.comp.grammar^followsInfix'('_call%3'(XV30407, XV30408, XV30409), 'lo.comp.grammar^followsInfix', _):- 'lo.comp.grammar@followsInfix'(XV30407, XV30408, XV30409).
'lo.comp.grammar@Hed115'(XHedStrm115, XStx1782, XRPr):- 'lo.comp.grammar@followsInfix'(XHedStrm115, XStx1782, XRPr).
'lo.comp.grammar^legalInfixOp'('_call%7'(XV30410, XV30411, XV30412, XV30413, XV30414, XV30415, XV30416), 'lo.comp.grammar^legalInfixOp', _):- 'lo.comp.grammar@legalInfixOp'(XV30410, XV30411, XV30412, XV30413, XV30414, XV30415, XV30416).
'lo.comp.grammar@Hed116'(XHedStrm116, XNStrm1692, XNStrm1692, XXd39098, X_33671, XHedStrm116):- ocall('_hdtl%3'(XHedStrm116, 'lo.comp.token#tok'('lo.comp.token#rbrce', X_33671), XNStrm1692),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@Hed117'(XHedStrm117, XNStrm1693, XNStrm1693, XXd39099, XLc, X_33673, XHedStrm117):- ocall('_hdtl%3'(XHedStrm117, 'lo.comp.token#tok'(X_33673, XLc), XNStrm1693),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar^checkForTerminator'('_call%5'(XV30417, XV30418, XV30419, XV30420, XV30421), 'lo.comp.grammar^checkForTerminator', _):- 'lo.comp.grammar@checkForTerminator'(XV30417, XV30418, XV30419, XV30420, XV30421).
'lo.comp.grammar^termRight'('_call%10'(XV30422, XV30423, XV30424, XV30425, XV30426, XV30427, XV30428, XV30429, XV30430, XV30431), 'lo.comp.grammar^termRight', _):- 'lo.comp.grammar@termRight'(XV30422, XV30423, XV30424, XV30425, XV30426, XV30427, XV30428, XV30429, XV30430, XV30431).
'lo.comp.grammar^interpolateSegment'('_call%6'(XV30432, XV30433, XV30434, XV30435, XV30436, XV30437), 'lo.comp.grammar^interpolateSegment', _):- 'lo.comp.grammar@interpolateSegment'(XV30432, XV30433, XV30434, XV30435, XV30436, XV30437).
'lo.comp.grammar^parseSegments'('_call%4'(XV30438, XV30439, XV30440, XV30441), 'lo.comp.grammar^parseSegments', _):- 'lo.comp.grammar@parseSegments'(XV30438, XV30439, XV30440, XV30441).
'lo.comp.grammar^parseString'('_call%5'(XV30442, XV30443, XV30444, XV30445, XV30446), 'lo.comp.grammar^parseString', _):- 'lo.comp.grammar@parseString'(XV30442, XV30443, XV30444, XV30445, XV30446).
'lo.comp.grammar^terms'('_call%5'(XV30447, XV30448, XV30449, XV30450, XV30451), 'lo.comp.grammar^terms', _):- 'lo.comp.grammar@terms'(XV30447, XV30448, XV30449, XV30450, XV30451).
'lo.comp.grammar^termArgs'('_call%8'(XV30452, XV30453, XV30454, XV30455, XV30456, XV30457, XV30458, XV30459), 'lo.comp.grammar^termArgs', _):- 'lo.comp.grammar@termArgs'(XV30452, XV30453, XV30454, XV30455, XV30456, XV30457, XV30458, XV30459).
'lo.comp.grammar^term00'('_call%6'(XV30460, XV30461, XV30462, XV30463, XV30464, XV30465), 'lo.comp.grammar^term00', _):- 'lo.comp.grammar@term00'(XV30460, XV30461, XV30462, XV30463, XV30464, XV30465).
'lo.comp.grammar^term0'('_call%6'(XV30466, XV30467, XV30468, XV30469, XV30470, XV30471), 'lo.comp.grammar^term0', _):- 'lo.comp.grammar@term0'(XV30466, XV30467, XV30468, XV30469, XV30470, XV30471).
'lo.comp.grammar@Neg54'(XNegStrm54, XNStrm1722, XNStrm1722, XXd39166, X_33719, XNegStrm54):- ocall('_hdtl%3'(XNegStrm54, 'lo.comp.token#tok'('lo.comp.token#rpar', X_33719), XNStrm1722),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    !,
    fail.
'lo.comp.grammar@Neg54'(XNegStrm54, XNStrm1722, XNStrm1722, XXd39166, X_33719, XNegStrm54).
'lo.comp.grammar^termLeft'('_call%8'(XV30472, XV30473, XV30474, XV30475, XV30476, XV30477, XV30478, XV30479), 'lo.comp.grammar^termLeft', _):- 'lo.comp.grammar@termLeft'(XV30472, XV30473, XV30474, XV30475, XV30476, XV30477, XV30478, XV30479).
'lo.comp.grammar^term'('_call%7'(XV30480, XV30481, XV30482, XV30483, XV30484, XV30485, XV30486), 'lo.comp.grammar^term', _):- 'lo.comp.grammar@term'(XV30480, XV30481, XV30482, XV30483, XV30484, XV30485, XV30486).
'lo.comp.grammar@One38'(XOneStm38, XStx1816, XStx1816, XRp0, XRp, XMark, XTerm):- 'lo.comp.grammar@term'(XOneStm38, XStx1816, 2000, XTerm, XMark, XRp, XRp0),
    !.
'lo.comp.grammar^parse'('_call%5'(XV30487, XV30488, XV30489, XV30490, XV30491), 'lo.comp.grammar^parse', _):- 'lo.comp.grammar@parse'(XV30487, XV30488, XV30489, XV30490, XV30491).
'lo.comp.grammar@or192'(XXV5289, XXd39169, X_33723, XTk, XRpx, XRp0, XRest):- XRest = 'lo.core#[]',
    XRp0 = XRpx.
'lo.comp.grammar@or192'(XXV5289, XXd39169, X_33723, XTk, XRpx, XRp0, XRest):- XRest = 'lo.core#,..'(XTk, X_33723),
    ocall('loc%1'(XXV5289),XTk,XTk),
    'lo.comp.errors@reportError'("extra tokens found in file", XXV5289, XRp0, XRpx).
'lo.comp.grammar^parseFile'('_call%4'(XV30492, XV30493, XV30494, XV30495), 'lo.comp.grammar^parseFile', _):- 'lo.comp.grammar@parseFile'(XV30492, XV30493, XV30494, XV30495).
'lo.comp.grammar^parseSrc'('_call%5'(XV30496, XV30497, XV30498, XV30499, XV30500), 'lo.comp.grammar^parseSrc', _):- 'lo.comp.grammar@parseSrc'(XV30496, XV30497, XV30498, XV30499, XV30500).
'lo.comp.grammar^nextTerm'('_call%5'(XV30501, XV30502, XV30503, XV30504, XV30505), 'lo.comp.grammar^nextTerm', _):- 'lo.comp.grammar@nextTerm'(XV30501, XV30502, XV30503, XV30504, XV30505).
