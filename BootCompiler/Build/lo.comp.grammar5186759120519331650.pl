'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.grammar'e'*'n20o20'()20'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.lexer'e'*'n2o2'import'e'public'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.abstract'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.ast'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.operators'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.token'e'*'n2o2'import'e'private'n2o2'pkg's'lo.resources'e'*'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I5'term'GT5it'lo.comp.ast*ast't'lo.comp.grammar*tokMark't'lo.comp.errors*report't'lo.comp.errors*report'Lt'lo.comp.token*token''parse'GT3t'lo.comp.ast*ast't'lo.comp.errors*report't'lo.comp.errors*report'Lt'lo.comp.token*token''parseFile'FT3t'lo.uri*uri't'lo.comp.errors*report't'lo.comp.errors*report't'lo.comp.ast*ast''parseSrc'FT4t'lo.uri*uri'Lit'lo.comp.errors*report't'lo.comp.errors*report't'lo.comp.ast*ast''nextTerm'PT5Lt'lo.comp.token*token't'lo.comp.ast*ast'Lt'lo.comp.token*token't'lo.comp.errors*report't'lo.comp.errors*report'\"s'I0'n2o2'()2's'otherMark's'endBrce'n0o0'()0'n0o0'()0'").
'lo.comp.grammar@init'() :- !.
'lo.comp.grammar@tupleize'(XTerm, 'lo.core#,..'(XL, XX11762)) :- 'lo.comp.abstract@isBinary'(XTerm, ",", X_792, XL, XR),
    !,
    'lo.comp.grammar@tupleize'(XR, XX11762).
'lo.comp.grammar@tupleize'(XT, 'lo.core#,..'(XT, 'lo.core#[]')) :- !.
'lo.comp.grammar@tupleize'(_, _) :- raise_exception('error'("tupleize", 170, 3, 63)).
'lo.comp.grammar@checkFor'(XStIn326, XNStrm270, XTk, XLc, X_793, XRp, XRp) :- ocall('_hdtl%3'(XStIn326, 'lo.comp.token#tok'(XTk, XLc), XNStrm270),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@checkFor'(XStIn327, XStIn327, XTk, XLc, XMsg, XRp, XRpx) :- 'lo.comp.grammar@Hed21'(XStIn327, XNStrm271, XNStrm271, XLc, X_794, XHedStrm21),
    'lo.comp.errors@reportError'(XMsg, XLc, XRp, XRpx).
'lo.comp.grammar@followsInfix'(XStIn328, XNStrm272, XPr) :- ocall('_hdtl%3'(XStIn328, 'lo.comp.token#tok'('lo.comp.token#idTok'(XOp), X_795), XNStrm272),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.grammar@cond5'(X_797, XPr, X_796, XOpPr, XOp).
'lo.comp.grammar@followsInfix'(XStIn329, XNStrm273, X_798) :- ocall('_hdtl%3'(XStIn329, 'lo.comp.token#tok'('lo.comp.token#lpar', X_799), XNStrm273),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@followsInfix'(XStIn330, XNStrm274, X_800) :- ocall('_hdtl%3'(XStIn330, 'lo.comp.token#tok'('lo.comp.token#lbra', X_801), XNStrm274),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@followsInfix'(XStIn331, XNStrm275, X_802) :- ocall('_hdtl%3'(XStIn331, 'lo.comp.token#tok'('lo.comp.token#lbrce', X_803), XNStrm275),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@followsInfix'(XStIn332, XNStrm276, X_804) :- ocall('_hdtl%3'(XStIn332, 'lo.comp.token#tok'('lo.comp.token#lqpar', X_805), XNStrm276),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@followsInfix'(XStIn333, XNStrm277, X_806) :- ocall('_hdtl%3'(XStIn333, 'lo.comp.token#tok'('lo.comp.token#stringTok'(X_807), X_808), XNStrm277),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@followsInfix'(XStIn334, XNStrm278, X_809) :- ocall('_hdtl%3'(XStIn334, 'lo.comp.token#tok'('lo.comp.token#idQTok'(X_810), X_811), XNStrm278),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@followsInfix'(XStIn335, XNStrm279, X_812) :- ocall('_hdtl%3'(XStIn335, 'lo.comp.token#tok'('lo.comp.token#intTok'(X_813), X_814), XNStrm279),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@followsInfix'(XStIn336, XNStrm280, X_815) :- ocall('_hdtl%3'(XStIn336, 'lo.comp.token#tok'('lo.comp.token#fltTok'(X_816), X_817), XNStrm280),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@legalInfixOp'(XStIn337, XNStrm281, XOp, XPr, XLeftPr, XOPr, XRPr) :- ocall('_hdtl%3'(XStIn337, 'lo.comp.token#tok'('lo.comp.token#idTok'(XOp), X_818), XNStrm281),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.operators@infixOp'(XOp, XLPr, XOPr, XRPr),
    'lo.core@=<'('lo.core$comp$lo.core*integer', XOPr, XPr),
    ocall('>=%2'(XLPr, XLeftPr),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer'),
    'lo.comp.grammar@Hed22'(XNStrm281, XRPr).
'lo.comp.grammar@checkForTerminator'(XStIn338, XNStrm282, X_819, XRp, XRp) :- ocall('_hdtl%3'(XStIn338, 'lo.comp.token#tok'('lo.comp.token#period', X_820), XNStrm282),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@checkForTerminator'(XStIn339, XStIn339, X_821, XRp, XRp) :- ocall('_eof%1'(XStIn339),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@checkForTerminator'(XStIn340, XStIn340, X_822, XRp, XRp) :- 'lo.comp.grammar@Hed23'(XStIn340, XNStrm283, XNStrm283, X_823, XHedStrm23).
'lo.comp.grammar@checkForTerminator'(XStIn341, XStIn341, 'lo.comp.grammar#endBrce', XRp, XRp).
'lo.comp.grammar@checkForTerminator'(XStIn342, XStx312, X_824, XRp, XRpx) :- 'lo.comp.grammar@Hed24'(XStIn342, XNStrm284, XNStrm284, XLc, X_825, XHedStrm24),
    'lo.comp.grammar@checkFor'(XStIn342, XStx312, 'lo.comp.token#period', XLc, "missing terminator", XRp, XRpx).
'lo.comp.grammar@termRight'(XStIn343, XStx315, XPr, XLeft, XTerm, XLeftPr, X_826, XMark, XRp, XRpx) :- 'lo.comp.grammar@legalInfixOp'(XStIn343, XStx313, XOp, XPr, XLeftPr, XInfPr, XRightPr),
    'lo.comp.grammar@term'(XStx313, XStx314, XRightPr, XRight, XLMark, XRp, XRp0),
    ocall('loc%1'(XXV27),XLeft,XLeft),
    ocall('loc%1'(XXV26),XRight,XRight),
    ocall('merge%2'(XXV26, XX11952),XXV27,XXV27),
    'lo.comp.abstract@binary'(XX11952, XOp, XLeft, XRight, XX11957),
    XM = XX11957,
    'lo.comp.grammar@termRight'(XStx314, XStx315, XPr, XM, XTerm, XInfPr, XLMark, XMark, XRp0, XRpx).
'lo.comp.grammar@termRight'(XStIn344, XStx316, XPr, XLeft, XTerm, XLeftPr, X_827, XMark, XRp, XRpx) :- ocall('_hdtl%3'(XStIn344, 'lo.comp.token#tok'('lo.comp.token#idTok'(XOp), XLc), XNStrm285),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.operators@postfixOp'(XOp, XLPr, XPostPr),
    'lo.core@=<'('lo.core$comp$lo.core*integer', XPostPr, XPr),
    ocall('>=%2'(XLPr, XLeftPr),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer'),
    ocall('loc%1'(XXV28),XLeft,XLeft),
    ocall('merge%2'(XLc, XX11992),XXV28,XXV28),
    'lo.comp.abstract@unary'(XX11992, XOp, XLeft, XX11996),
    XM = XX11996,
    'lo.comp.grammar@termRight'(XNStrm285, XStx316, XPr, XM, XTerm, XPostPr, 'lo.comp.grammar#otherMark', XMark, XRp, XRpx).
'lo.comp.grammar@termRight'(XStIn345, XStIn345, X_828, XTerm, XTerm, X_829, XMark, XMark, XRp, XRp).
'lo.comp.grammar@termArgs'(XStIn346, XStx317, XLft, XTerm, X_830, XMark, XRp, XRpx) :- ocall('_hdtl%3'(XStIn346, 'lo.comp.token#tok'('lo.comp.token#lpar', XLc0), XNStrm286),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm286, 'lo.comp.token#tok'('lo.comp.token#rpar', XLc1), XNStrm287),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('loc%1'(XXV29),XLft,XLft),
    ocall('merge%2'(XLc1, XX12033),XXV29,XXV29),
    XLc = XX12033,
    ocall('merge%2'(XLc1, XX12038),XLc0,XLc0),
    'lo.comp.grammar@termArgs'(XNStrm287, XStx317, 'lo.comp.ast#appl'(XLc, XLft, 'lo.comp.ast#tupl'(XX12038, "()", 'lo.core#[]')), XTerm, 'lo.comp.grammar#otherMark', XMark, XRp, XRpx).
'lo.comp.grammar@termArgs'(XStIn347, XStx320, XLft, XTerm, X_831, XMark, XRp, XRpx) :- ocall('_hdtl%3'(XStIn347, 'lo.comp.token#tok'('lo.comp.token#lpar', XLc0), XNStrm288),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.grammar@term'(XNStrm288, XStx318, 2000, XSeq, X_832, XRp, XRp0),
    'lo.comp.grammar@checkFor'(XStx318, XStx319, 'lo.comp.token#rpar', XLc1, "missing close paren", XRp0, XRp1),
    ocall('loc%1'(XXV30),XLft,XLft),
    ocall('merge%2'(XLc1, XX12069),XXV30,XXV30),
    ocall('merge%2'(XLc1, XX12073),XLc0,XLc0),
    'lo.comp.grammar@tupleize'(XSeq, XX12076),
    'lo.comp.grammar@termArgs'(XStx319, XStx320, 'lo.comp.ast#appl'(XX12069, XLft, 'lo.comp.ast#tupl'(XX12073, "()", XX12076)), XTerm, 'lo.comp.grammar#otherMark', XMark, XRp1, XRpx).
'lo.comp.grammar@termArgs'(XStIn348, XStx321, XLft, XTerm, X_833, XMark, XRp, XRpx) :- ocall('_hdtl%3'(XStIn348, 'lo.comp.token#tok'('lo.comp.token#lbra', XLc0), XNStrm289),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm289, 'lo.comp.token#tok'('lo.comp.token#rbra', XLc1), XNStrm290),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('loc%1'(XXV31),XLft,XLft),
    ocall('merge%2'(XLc1, XX12103),XXV31,XXV31),
    ocall('merge%2'(XLc1, XX12107),XLc0,XLc0),
    'lo.comp.grammar@termArgs'(XNStrm290, XStx321, 'lo.comp.ast#appl'(XX12103, XLft, 'lo.comp.ast#tupl'(XX12107, "[]", 'lo.core#[]')), XTerm, 'lo.comp.grammar#otherMark', XMark, XRp, XRpx).
'lo.comp.grammar@termArgs'(XStIn349, XStx323, XLft, 'lo.comp.ast#appl'(XLc, XLft, 'lo.comp.ast#tupl'(XTpLc, "[]", XX12122)), X_834, 'lo.comp.grammar#otherMark', XRp, XRpx) :- ocall('_hdtl%3'(XStIn349, 'lo.comp.token#tok'('lo.comp.token#lbra', XLc0), XNStrm291),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.grammar@term'(XNStrm291, XStx322, 2000, XSeq, X_835, XRp, XRp0),
    'lo.comp.grammar@checkFor'(XStx322, XStx323, 'lo.comp.token#rbra', XLc1, "missing close bracket", XRp0, XRpx),
    ocall('merge%2'(XLc1, XX12145),XLc0,XLc0),
    XTpLc = XX12145,
    ocall('loc%1'(XXV32),XLft,XLft),
    ocall('merge%2'(XLc1, XX12149),XXV32,XXV32),
    XLc = XX12149,
    'lo.comp.grammar@tupleize'(XSeq, XX12122).
'lo.comp.grammar@termArgs'(XStIn350, XNStrm293, XLft, 'lo.comp.ast#appl'(XLc, XLft, 'lo.comp.ast#tupl'(XTpLc, "{}", 'lo.core#[]')), X_836, 'lo.comp.grammar#endBrce', XRp, XRp) :- ocall('_hdtl%3'(XStIn350, 'lo.comp.token#tok'('lo.comp.token#lbrce', XLc0), XNStrm292),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm292, 'lo.comp.token#tok'('lo.comp.token#rbrce', XLc1), XNStrm293),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('loc%1'(XXV33),XLft,XLft),
    ocall('merge%2'(XLc1, XX12176),XXV33,XXV33),
    XLc = XX12176,
    ocall('merge%2'(XLc1, XX12180),XLc0,XLc0),
    XTpLc = XX12180.
'lo.comp.grammar@termArgs'(XStIn351, XStx325, XLft, 'lo.comp.ast#appl'(XLc, XLft, 'lo.comp.ast#tupl'(XTpLc, "{}", XEls)), X_837, 'lo.comp.grammar#endBrce', XRp, XRpx) :- ocall('_hdtl%3'(XStIn351, 'lo.comp.token#tok'('lo.comp.token#lbrce', XLc0), XNStrm294),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.grammar@terms'(XNStrm294, XStx324, XEls, XRp, XRp0),
    'lo.comp.grammar@checkFor'(XStx324, XStx325, 'lo.comp.token#rbrce', XLc1, "missing close brace", XRp0, XRpx),
    ocall('loc%1'(XXV34),XLft,XLft),
    ocall('merge%2'(XLc1, XX12208),XXV34,XXV34),
    XLc = XX12208,
    ocall('merge%2'(XLc1, XX12212),XLc0,XLc0),
    XTpLc = XX12212.
'lo.comp.grammar@termArgs'(XStIn352, XStx326, XLft, XTerm, X_838, XMark, XRp, XRpx) :- ocall('_hdtl%3'(XStIn352, 'lo.comp.token#tok'('lo.comp.token#idTok'("."), XLc), XNStrm295),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm295, 'lo.comp.token#tok'('lo.comp.token#idTok'(XFld), XLcF), XNStrm296),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('loc%1'(XXV35),XLft,XLft),
    ocall('merge%2'(XLcF, XX12234),XXV35,XXV35),
    'lo.comp.abstract@binary'(XX12234, ".", XLft, 'lo.comp.ast#iden'(XLcF, XFld), XX12240),
    'lo.comp.grammar@termArgs'(XNStrm296, XStx326, XX12240, XTerm, 'lo.comp.grammar#otherMark', XMark, XRp, XRpx).
'lo.comp.grammar@termArgs'(XStIn353, XStIn353, XTerm, XTerm, XMark, XMark, XRp, XRp).
'lo.comp.grammar@term00'(XStIn354, XNStrm297, 'lo.comp.ast#iden'(XLc, XNm), 'lo.comp.grammar#otherMark', XRp, XRp) :- ocall('_hdtl%3'(XStIn354, 'lo.comp.token#tok'('lo.comp.token#idTok'(XNm), XLc), XNStrm297),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@term00'(XStIn355, XNStrm298, 'lo.comp.ast#iden'(XLc, XNm), 'lo.comp.grammar#otherMark', XRp, XRp) :- ocall('_hdtl%3'(XStIn355, 'lo.comp.token#tok'('lo.comp.token#idQTok'(XNm), XLc), XNStrm298),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@term00'(XStIn356, XNStrm300, 'lo.comp.ast#tupl'(XLc, "()", 'lo.core#[]'), 'lo.comp.grammar#otherMark', XRp, XRp) :- ocall('_hdtl%3'(XStIn356, 'lo.comp.token#tok'('lo.comp.token#lpar', XLc0), XNStrm299),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm299, 'lo.comp.token#tok'('lo.comp.token#rpar', XLc1), XNStrm300),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('merge%2'(XLc1, XX12298),XLc0,XLc0),
    XLc = XX12298.
'lo.comp.grammar@term00'(XStIn357, XStx328, 'lo.comp.ast#tupl'(XLc, "()", XX12302), 'lo.comp.grammar#otherMark', XRp, XRpx) :- ocall('_hdtl%3'(XStIn357, 'lo.comp.token#tok'('lo.comp.token#lpar', XLc0), XNStrm301),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.grammar@term'(XNStrm301, XStx327, 2000, XSeq, X_839, XRp, XRp0),
    'lo.comp.grammar@checkFor'(XStx327, XStx328, 'lo.comp.token#rpar', XLc1, "missing close paren", XRp0, XRpx),
    ocall('merge%2'(XLc1, XX12323),XLc0,XLc0),
    XLc = XX12323,
    'lo.comp.grammar@tupleize'(XSeq, XX12302).
'lo.comp.grammar@terms'(XStIn358, XStx330, 'lo.core#,..'(XT, XR), XRp, XRpx) :- 'lo.comp.grammar@parse'(XStIn358, XStx329, XT, XRp, XRp0),
    'lo.comp.grammar@terms'(XStx329, XStx330, XR, XRp0, XRpx).
'lo.comp.grammar@terms'(XStIn359, XStIn359, 'lo.core#[]', XRp, XRp).
'lo.comp.grammar@interpolateSegment'(XLc, XText, "", XRp, XRpx, XX12354) :- 'lo.comp.lexer@subTokenize'(XLc, XText, XX12345),
    'lo.comp.grammar@term'(XX12345, XStx331, 2000, XArg, X_840, XRp, XRpx),
    ocall('_eof%1'(XStx331),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    !,
    'lo.comp.abstract@unary'(XLc, "disp", XArg, XX12354).
'lo.comp.grammar@interpolateSegment'(XLc, XText, XFmt, XRp, XRpx, XX12374) :- 'lo.comp.lexer@subTokenize'(XLc, XText, XX12362),
    'lo.comp.grammar@term'(XX12362, XStx332, 2000, XArg, X_841, XRp, XRpx),
    ocall('_eof%1'(XStx332),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    !,
    'lo.comp.abstract@binary'(XLc, "frmt", XArg, 'lo.comp.ast#strg'(XLc, XFmt), XX12374).
'lo.comp.grammar@interpolateSegment'(_, _, _, _, _, _) :- raise_exception('error'("interpolateSegment", 204, 3, 116)).
'lo.comp.grammar@parseSegments'('lo.core#[]', XRp, XRp, 'lo.core#[]') :- !.
'lo.comp.grammar@parseSegments'('lo.core#,..'('lo.comp.token#segment'(XLc, XStr), XMore), XRp, XRpx, 'lo.core#,..'(XX12390, XX12394)) :- !,
    'lo.comp.abstract@unary'(XLc, "ss", 'lo.comp.ast#strg'(XLc, XStr), XX12390),
    'lo.comp.grammar@parseSegments'(XMore, XRp, XRpx, XX12394).
'lo.comp.grammar@parseSegments'('lo.core#,..'('lo.comp.token#interpolate'(XLc, XExp, XFmt), XMore), XRp, XRpx, 'lo.core#,..'(XX12409, XX12413)) :- !,
    'lo.comp.grammar@interpolateSegment'(XLc, XExp, XFmt, XRp, XRp0, XX12409),
    'lo.comp.grammar@parseSegments'(XMore, XRp0, XRpx, XX12413).
'lo.comp.grammar@parseSegments'(_, _, _, _) :- raise_exception('error'("parseSegments", 197, 3, 29)).
'lo.comp.grammar@parseString'(XLc, 'lo.core#[]', XRp, XRp, 'lo.comp.ast#strg'(XLc, "")) :- !.
'lo.comp.grammar@parseString'(X_842, 'lo.core#,..'('lo.comp.token#segment'(XLc, XStr), 'lo.core#[]'), XRp, XRp, 'lo.comp.ast#strg'(XLc, XStr)) :- !.
'lo.comp.grammar@parseString'(XLc, XSegments, XRp, XRpx, XX12445) :- !,
    'lo.comp.grammar@parseSegments'(XSegments, XRp, XRpx, XX12442),
    'lo.comp.abstract@unary'(XLc, "ssSeq", 'lo.comp.ast#tupl'(XLc, "[]", XX12442), XX12444),
    'lo.comp.abstract@unary'(XLc, "formatSS", XX12444, XX12445).
'lo.comp.grammar@parseString'(_, _, _, _, _) :- raise_exception('error'("parseString", 191, 3, 39)).
'lo.comp.grammar@term0'(XStIn360, XNStrm302, XX12450, 'lo.comp.grammar#otherMark', XRp, XRpx) :- ocall('_hdtl%3'(XStIn360, 'lo.comp.token#tok'('lo.comp.token#stringTok'(XSegments), XLc), XNStrm302),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.grammar@parseString'(XLc, XSegments, XRp, XRpx, XX12450).
'lo.comp.grammar@term0'(XStIn361, XNStrm303, 'lo.comp.ast#intg'(XLc, XIx), 'lo.comp.grammar#otherMark', XRp, XRp) :- ocall('_hdtl%3'(XStIn361, 'lo.comp.token#tok'('lo.comp.token#intTok'(XIx), XLc), XNStrm303),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@term0'(XStIn362, XNStrm304, 'lo.comp.ast#flot'(XLc, XDx), 'lo.comp.grammar#otherMark', XRp, XRp) :- ocall('_hdtl%3'(XStIn362, 'lo.comp.token#tok'('lo.comp.token#fltTok'(XDx), XLc), XNStrm304),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@term0'(XStIn363, XNStrm306, 'lo.comp.ast#tupl'(XLc, "[]", 'lo.core#[]'), 'lo.comp.grammar#otherMark', XRp, XRp) :- ocall('_hdtl%3'(XStIn363, 'lo.comp.token#tok'('lo.comp.token#lbra', XLc0), XNStrm305),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm305, 'lo.comp.token#tok'('lo.comp.token#rbra', XLc1), XNStrm306),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('merge%2'(XLc1, XX12507),XLc0,XLc0),
    XLc = XX12507.
'lo.comp.grammar@term0'(XStIn364, XStx334, 'lo.comp.ast#tupl'(XLc, "[]", XX12511), 'lo.comp.grammar#otherMark', XRp, XRpx) :- ocall('_hdtl%3'(XStIn364, 'lo.comp.token#tok'('lo.comp.token#lbra', XLc0), XNStrm307),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.grammar@term'(XNStrm307, XStx333, 2000, XSeq, X_843, XRp, XRp0),
    'lo.comp.grammar@checkFor'(XStx333, XStx334, 'lo.comp.token#rbra', XLc1, "missing close bracket", XRp0, XRpx),
    ocall('merge%2'(XLc1, XX12532),XLc0,XLc0),
    XLc = XX12532,
    'lo.comp.grammar@tupleize'(XSeq, XX12511).
'lo.comp.grammar@term0'(XStIn365, XNStrm309, 'lo.comp.ast#tupl'(XLc, "{}", 'lo.core#[]'), 'lo.comp.grammar#endBrce', XRp, XRp) :- ocall('_hdtl%3'(XStIn365, 'lo.comp.token#tok'('lo.comp.token#lbrce', XLc0), XNStrm308),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm308, 'lo.comp.token#tok'('lo.comp.token#rbrce', XLc1), XNStrm309),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('merge%2'(XLc1, XX12554),XLc0,XLc0),
    XLc = XX12554.
'lo.comp.grammar@term0'(XStIn366, XStx336, 'lo.comp.ast#tupl'(XLc, "{}", XSeq), 'lo.comp.grammar#endBrce', XRp, XRpx) :- ocall('_hdtl%3'(XStIn366, 'lo.comp.token#tok'('lo.comp.token#lbrce', XLc0), XNStrm310),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.grammar@terms'(XNStrm310, XStx335, XSeq, XRp, XRp0),
    'lo.comp.grammar@checkFor'(XStx335, XStx336, 'lo.comp.token#rbrce', XLc1, "missing close brace", XRp0, XRpx),
    ocall('merge%2'(XLc1, XX12577),XLc0,XLc0),
    XLc = XX12577.
'lo.comp.grammar@term0'(XStIn367, XStx338, XX12581, 'lo.comp.grammar#otherMark', XRp, XRpx) :- ocall('_hdtl%3'(XStIn367, 'lo.comp.token#tok'('lo.comp.token#lqpar', XLc0), XNStrm311),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.grammar@term'(XNStrm311, XStx337, 2000, XA, X_844, XRp, XRp0),
    'lo.comp.grammar@checkFor'(XStx337, XStx338, 'lo.comp.token#rqpar', XLc1, "missing close quote", XRp0, XRpx),
    ocall('merge%2'(XLc1, XX12601),XLc0,XLc0),
    XLc = XX12601,
    'lo.comp.abstract@unary'(XLc, "<||>", XA, XX12581).
'lo.comp.grammar@term0'(XStIn368, XStx340, XTerm, XMark, XRp, XRpx) :- 'lo.comp.grammar@term00'(XStIn368, XStx339, XLeft, XLMark, XRp, XRp0),
    'lo.comp.grammar@termArgs'(XStx339, XStx340, XLeft, XTerm, XLMark, XMark, XRp0, XRpx).
'lo.comp.grammar@termLeft'(XStIn369, XStx341, XPr, XLeft, XOPr, XMark, XRp, XRpx) :- ocall('_hdtl%3'(XStIn369, 'lo.comp.token#tok'('lo.comp.token#idTok'(XOp), XLc), XNStrm312),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.comp.grammar@Neg12'(XNStrm312, XNStrm313, XNStrm313, X_845, XNegStrm12),
    'lo.comp.operators@prefixOp'(XOp, XOPr, XORight),
    'lo.core@=<'('lo.core$comp$lo.core*integer', XOPr, XPr),
    'lo.comp.grammar@term'(XNStrm312, XStx341, XORight, XArg, XMark, XRp, XRpx),
    ocall('loc%1'(XXV36),XArg,XArg),
    ocall('merge%2'(XXV36, XX12649),XLc,XLc),
    'lo.comp.abstract@unary'(XX12649, XOp, XArg, XX12653),
    XLeft = XX12653.
'lo.comp.grammar@termLeft'(XStIn370, XStx342, X_846, XTerm, 0, XMark, XRp, XRpx) :- 'lo.comp.grammar@term0'(XStIn370, XStx342, XTerm, XMark, XRp, XRpx).
'lo.comp.grammar@term'(XStIn371, XStx344, XPr, XTerm, XMark, XRp, XRpx) :- 'lo.comp.grammar@termLeft'(XStIn371, XStx343, XPr, XLeft, XLftPr, XLLend, XRp, XRp0),
    'lo.comp.grammar@termRight'(XStx343, XStx344, XPr, XLeft, XTerm, XLftPr, XLLend, XMark, XRp0, XRpx).
'lo.comp.grammar@parse'(XStIn372, XStx346, XTerm, XRp, XRpx) :- 'lo.comp.grammar@One8'(XStIn372, XDjOut27, XRp0, XRp, XMark, XTerm),
    'lo.comp.grammar@checkForTerminator'(XDjOut27, XStx346, XMark, XRp0, XRpx).
'lo.comp.grammar@parseFile'(XU, XRp, XRpx, XTerm) :- 'lo.comp.lexer@tokenizeFile'(XU, XX12696),
    'lo.comp.grammar@parse'(XX12696, XStx347, XTerm, XRp, XRp0),
    XRest = XStx347,
    'lo.comp.grammar@or12'(XXV37, X_847, XTk, XRpx, XRp0, XRest),
    !.
'lo.comp.grammar@parseFile'(_, _, _, _) :- raise_exception('error'("parseFile", 14, 3, 175)).
'lo.comp.grammar@parseSrc'(XU, XCodes, XRp, XRpx, XTerm) :- 'lo.uri@getUriPath'(XU, XX12719),
    'lo.comp.lexer@tokenCodes'(XCodes, XX12719, XX12720),
    'lo.comp.grammar@parse'(XX12720, XStx348, XTerm, XRp, XRpx),
    ocall('_eof%1'(XStx348),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    !.
'lo.comp.grammar@parseSrc'(_, _, _, _, _) :- raise_exception('error'("parseSrc", 19, 3, 93)).
'lo.comp.grammar@nextTerm'(XToks, XTerm, XRest, XRp, XRpx) :- 'lo.comp.grammar@parse'(XToks, XStx349, XTerm, XRp, XRpx),
    XRest = XStx349.
'lo.comp.grammar@otherMark'('lo.comp.grammar#otherMark') :- !.
'lo.comp.grammar^tupleize'('_call%2'(XV1966, XV1967), 'lo.comp.grammar^tupleize', _) :- 'lo.comp.grammar@tupleize'(XV1966, XV1967).
'lo.comp.grammar@Hed21'(XHedStrm21, XNStrm271, XNStrm271, XLc, X_794, XHedStrm21) :- ocall('_hdtl%3'(XHedStrm21, 'lo.comp.token#tok'(X_794, XLc), XNStrm271),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar^checkFor'('_call%7'(XV1968, XV1969, XV1970, XV1971, XV1972, XV1973, XV1974), 'lo.comp.grammar^checkFor', _) :- 'lo.comp.grammar@checkFor'(XV1968, XV1969, XV1970, XV1971, XV1972, XV1973, XV1974).
'lo.comp.grammar@endBrce'('lo.comp.grammar#endBrce') :- !.
'lo.comp.grammar@neg24'(X_797, XOp) :- 'lo.comp.operators@isOperator'(XOp, X_797),
    !,
    fail.
'lo.comp.grammar@neg24'(X_797, XOp).
'lo.comp.grammar@cond5'(X_797, XPr, X_796, XOpPr, XOp) :- 'lo.comp.operators@prefixOp'(XOp, XOpPr, X_796),
    !,
    'lo.core@=<'('lo.core$comp$lo.core*integer', XOpPr, XPr).
'lo.comp.grammar@cond5'(X_797, XPr, X_796, XOpPr, XOp) :- 'lo.comp.grammar@neg24'(X_797, XOp).
'lo.comp.grammar^followsInfix'('_call%3'(XV1975, XV1976, XV1977), 'lo.comp.grammar^followsInfix', _) :- 'lo.comp.grammar@followsInfix'(XV1975, XV1976, XV1977).
'lo.comp.grammar@Hed22'(XHedStrm22, XRPr) :- 'lo.comp.grammar@followsInfix'(XHedStrm22, XStx311, XRPr).
'lo.comp.grammar^legalInfixOp'('_call%7'(XV1978, XV1979, XV1980, XV1981, XV1982, XV1983, XV1984), 'lo.comp.grammar^legalInfixOp', _) :- 'lo.comp.grammar@legalInfixOp'(XV1978, XV1979, XV1980, XV1981, XV1982, XV1983, XV1984).
'lo.comp.grammar@Hed23'(XHedStrm23, XNStrm283, XNStrm283, X_823, XHedStrm23) :- ocall('_hdtl%3'(XHedStrm23, 'lo.comp.token#tok'('lo.comp.token#rbrce', X_823), XNStrm283),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar@Hed24'(XHedStrm24, XNStrm284, XNStrm284, XLc, X_825, XHedStrm24) :- ocall('_hdtl%3'(XHedStrm24, 'lo.comp.token#tok'(X_825, XLc), XNStrm284),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.comp.grammar^checkForTerminator'('_call%5'(XV1985, XV1986, XV1987, XV1988, XV1989), 'lo.comp.grammar^checkForTerminator', _) :- 'lo.comp.grammar@checkForTerminator'(XV1985, XV1986, XV1987, XV1988, XV1989).
'lo.comp.grammar^termRight'('_call%10'(XV1990, XV1991, XV1992, XV1993, XV1994, XV1995, XV1996, XV1997, XV1998, XV1999), 'lo.comp.grammar^termRight', _) :- 'lo.comp.grammar@termRight'(XV1990, XV1991, XV1992, XV1993, XV1994, XV1995, XV1996, XV1997, XV1998, XV1999).
'lo.comp.grammar^termArgs'('_call%8'(XV2000, XV2001, XV2002, XV2003, XV2004, XV2005, XV2006, XV2007), 'lo.comp.grammar^termArgs', _) :- 'lo.comp.grammar@termArgs'(XV2000, XV2001, XV2002, XV2003, XV2004, XV2005, XV2006, XV2007).
'lo.comp.grammar^term00'('_call%6'(XV2008, XV2009, XV2010, XV2011, XV2012, XV2013), 'lo.comp.grammar^term00', _) :- 'lo.comp.grammar@term00'(XV2008, XV2009, XV2010, XV2011, XV2012, XV2013).
'lo.comp.grammar^terms'('_call%5'(XV2014, XV2015, XV2016, XV2017, XV2018), 'lo.comp.grammar^terms', _) :- 'lo.comp.grammar@terms'(XV2014, XV2015, XV2016, XV2017, XV2018).
'lo.comp.grammar^interpolateSegment'('_call%6'(XV2019, XV2020, XV2021, XV2022, XV2023, XV2024), 'lo.comp.grammar^interpolateSegment', _) :- 'lo.comp.grammar@interpolateSegment'(XV2019, XV2020, XV2021, XV2022, XV2023, XV2024).
'lo.comp.grammar^parseSegments'('_call%4'(XV2025, XV2026, XV2027, XV2028), 'lo.comp.grammar^parseSegments', _) :- 'lo.comp.grammar@parseSegments'(XV2025, XV2026, XV2027, XV2028).
'lo.comp.grammar^parseString'('_call%5'(XV2029, XV2030, XV2031, XV2032, XV2033), 'lo.comp.grammar^parseString', _) :- 'lo.comp.grammar@parseString'(XV2029, XV2030, XV2031, XV2032, XV2033).
'lo.comp.grammar^term0'('_call%6'(XV2034, XV2035, XV2036, XV2037, XV2038, XV2039), 'lo.comp.grammar^term0', _) :- 'lo.comp.grammar@term0'(XV2034, XV2035, XV2036, XV2037, XV2038, XV2039).
'lo.comp.grammar@Neg12'(XNegStrm12, XNStrm313, XNStrm313, X_845, XNegStrm12) :- ocall('_hdtl%3'(XNegStrm12, 'lo.comp.token#tok'('lo.comp.token#rpar', X_845), XNStrm313),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    !,
    fail.
'lo.comp.grammar@Neg12'(XNegStrm12, XNStrm313, XNStrm313, X_845, XNegStrm12).
'lo.comp.grammar^termLeft'('_call%8'(XV2040, XV2041, XV2042, XV2043, XV2044, XV2045, XV2046, XV2047), 'lo.comp.grammar^termLeft', _) :- 'lo.comp.grammar@termLeft'(XV2040, XV2041, XV2042, XV2043, XV2044, XV2045, XV2046, XV2047).
'lo.comp.grammar^term'('_call%7'(XV2048, XV2049, XV2050, XV2051, XV2052, XV2053, XV2054), 'lo.comp.grammar^term', _) :- 'lo.comp.grammar@term'(XV2048, XV2049, XV2050, XV2051, XV2052, XV2053, XV2054).
'lo.comp.grammar@One8'(XOneStm8, XStx345, XRp0, XRp, XMark, XTerm) :- 'lo.comp.grammar@term'(XOneStm8, XStx345, 2000, XTerm, XMark, XRp, XRp0),
    !.
'lo.comp.grammar^parse'('_call%5'(XV2055, XV2056, XV2057, XV2058, XV2059), 'lo.comp.grammar^parse', _) :- 'lo.comp.grammar@parse'(XV2055, XV2056, XV2057, XV2058, XV2059).
'lo.comp.grammar@or12'(XXV37, X_847, XTk, XRpx, XRp0, XRest) :- XRest = 'lo.core#[]',
    XRp0 = XRpx.
'lo.comp.grammar@or12'(XXV37, X_847, XTk, XRpx, XRp0, XRest) :- XRest = 'lo.core#,..'(XTk, X_847),
    ocall('loc%1'(XXV37),XTk,XTk),
    'lo.comp.errors@reportError'("extra tokens found in file", XXV37, XRp0, XRpx).
'lo.comp.grammar^parseFile'('_call%4'(XV2060, XV2061, XV2062, XV2063), 'lo.comp.grammar^parseFile', _) :- 'lo.comp.grammar@parseFile'(XV2060, XV2061, XV2062, XV2063).
'lo.comp.grammar^parseSrc'('_call%5'(XV2064, XV2065, XV2066, XV2067, XV2068), 'lo.comp.grammar^parseSrc', _) :- 'lo.comp.grammar@parseSrc'(XV2064, XV2065, XV2066, XV2067, XV2068).
'lo.comp.grammar^nextTerm'('_call%5'(XV2069, XV2070, XV2071, XV2072, XV2073), 'lo.comp.grammar^nextTerm', _) :- 'lo.comp.grammar@nextTerm'(XV2069, XV2070, XV2071, XV2072, XV2073).
