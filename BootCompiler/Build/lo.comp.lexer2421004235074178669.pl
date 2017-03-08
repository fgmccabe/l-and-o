'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.lexer'e'*'n15o15'()15'n2o2'import'e'private'n2o2'pkg's'lo.comp.operators'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.token'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.resources'e'*'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I8'charRef'GT1it'lo.comp.lexer*tokenState''readIden'GT1Lit'lo.comp.lexer*tokenState''tokenize'GT1Lt'lo.comp.token*token't'lo.comp.lexer*tokenState''startState'FT2LiSt'lo.comp.lexer*tokenState''tokenizeFile'FT1t'lo.uri*uri'Lt'lo.comp.token*token''tokenCodes'FT2LiSLt'lo.comp.token*token''getNTokens'FT3LiSiLt'lo.comp.token*token''subTokenize'FT2t'lo.comp.location*location'SLt'lo.comp.token*token'\"s\"I1'tokenState'Yt'lo.comp.lexer*tokenState'I1'currentLocation'PT1T4iiiS\"n0o0'()0'n0o0'()0'n1o1'()1'n2o2'()2's'lo.core$stream$lo.comp.lexer*tokenState's\"c'lo.core$stream'T1t'lo.comp.lexer*tokenState'T1i\"").
'lo.comp.lexer@init'() :- !.
'lo.comp.lexer@whiteSpace'(XStIn217, XDjOut19) :- ocall('_hdtl%3'(XStIn217, XX, XNStrm174),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@One3'(XNStrm174, XDjOut19, XX).
'lo.comp.lexer@lineComment'(XStIn218, XNStrm175) :- ocall('_hdtl%3'(XStIn218, 10, XNStrm175),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@lineComment'(XStIn219, XStx238) :- ocall('_hdtl%3'(XStIn219, X_689, XNStrm176),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@lineComment'(XNStrm176, XStx238).
'lo.comp.lexer@blockComment'(XStIn220, XStIn220) :- ocall('_eof%1'(XStIn220),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@blockComment'(XStIn221, XNStrm178) :- ocall('_hdtl%3'(XStIn221, 42, XNStrm177),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    ocall('_hdtl%3'(XNStrm177, 47, XNStrm178),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@blockComment'(XStIn222, XStx239) :- ocall('_hdtl%3'(XStIn222, X_690, XNStrm179),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@blockComment'(XNStrm179, XStx239).
'lo.comp.lexer@skipSpaces'(XStIn223, XStx241) :- 'lo.comp.lexer@whiteSpace'(XStIn223, XStx240),
    'lo.comp.lexer@skipSpaces'(XStx240, XStx241).
'lo.comp.lexer@skipSpaces'(XStIn224, XStx243) :- ocall('_hdtl%3'(XStIn224, 45, XNStrm180),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    ocall('_hdtl%3'(XNStrm180, 45, XNStrm181),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@Disj17'(XNStrm181, XDjOut20, XNStrm183, XNStrm183, XNStrm182, XNStrm182, XDjStrm17),
    'lo.comp.lexer@lineComment'(XDjOut20, XStx242),
    'lo.comp.lexer@skipSpaces'(XStx242, XStx243).
'lo.comp.lexer@skipSpaces'(XStIn225, XStx245) :- ocall('_hdtl%3'(XStIn225, 47, XNStrm184),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    ocall('_hdtl%3'(XNStrm184, 42, XNStrm185),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@blockComment'(XNStrm185, XStx244),
    'lo.comp.lexer@skipSpaces'(XStx244, XStx245).
'lo.comp.lexer@skipSpaces'(XStIn226, XStIn226).
'lo.comp.lexer@digit'(XStIn227, XNStrm186, 0) :- ocall('_hdtl%3'(XStIn227, 48, XNStrm186),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@digit'(XStIn228, XNStrm187, 1) :- ocall('_hdtl%3'(XStIn228, 49, XNStrm187),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@digit'(XStIn229, XNStrm188, 2) :- ocall('_hdtl%3'(XStIn229, 50, XNStrm188),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@digit'(XStIn230, XNStrm189, 3) :- ocall('_hdtl%3'(XStIn230, 51, XNStrm189),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@digit'(XStIn231, XNStrm190, 4) :- ocall('_hdtl%3'(XStIn231, 52, XNStrm190),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@digit'(XStIn232, XNStrm191, 5) :- ocall('_hdtl%3'(XStIn232, 53, XNStrm191),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@digit'(XStIn233, XNStrm192, 6) :- ocall('_hdtl%3'(XStIn233, 54, XNStrm192),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@digit'(XStIn234, XNStrm193, 7) :- ocall('_hdtl%3'(XStIn234, 55, XNStrm193),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@digit'(XStIn235, XNStrm194, 8) :- ocall('_hdtl%3'(XStIn235, 56, XNStrm194),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@digit'(XStIn236, XNStrm195, 9) :- ocall('_hdtl%3'(XStIn236, 57, XNStrm195),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn237, XStx246, XD) :- 'lo.comp.lexer@digit'(XStIn237, XStx246, XD).
'lo.comp.lexer@hexDigit'(XStIn238, XNStrm196, 10) :- ocall('_hdtl%3'(XStIn238, 97, XNStrm196),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn239, XNStrm197, 11) :- ocall('_hdtl%3'(XStIn239, 98, XNStrm197),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn240, XNStrm198, 12) :- ocall('_hdtl%3'(XStIn240, 99, XNStrm198),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn241, XNStrm199, 13) :- ocall('_hdtl%3'(XStIn241, 100, XNStrm199),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn242, XNStrm200, 14) :- ocall('_hdtl%3'(XStIn242, 101, XNStrm200),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn243, XNStrm201, 15) :- ocall('_hdtl%3'(XStIn243, 102, XNStrm201),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn244, XNStrm202, 10) :- ocall('_hdtl%3'(XStIn244, 65, XNStrm202),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn245, XNStrm203, 11) :- ocall('_hdtl%3'(XStIn245, 66, XNStrm203),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn246, XNStrm204, 12) :- ocall('_hdtl%3'(XStIn246, 67, XNStrm204),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn247, XNStrm205, 13) :- ocall('_hdtl%3'(XStIn247, 68, XNStrm205),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn248, XNStrm206, 14) :- ocall('_hdtl%3'(XStIn248, 69, XNStrm206),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexDigit'(XStIn249, XNStrm207, 15) :- ocall('_hdtl%3'(XStIn249, 70, XNStrm207),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexChars'(XStIn250, XStx248, XSoFar, XCh) :- 'lo.comp.lexer@hexDigit'(XStIn250, XStx247, XN),
    ocall('*%3'(XSoFar, 16, XX10326),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('+%3'(XX10326, XN, XX10329),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.lexer@hexChars'(XStx247, XStx248, XX10329, XCh).
'lo.comp.lexer@hexChars'(XStIn251, XNStrm208, XSoFar, XSoFar) :- ocall('_hdtl%3'(XStIn251, 59, XNStrm208),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@backSlashRef'(XStIn252, XNStrm209, 7) :- ocall('_hdtl%3'(XStIn252, 97, XNStrm209),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@backSlashRef'(XStIn253, XNStrm210, 8) :- ocall('_hdtl%3'(XStIn253, 98, XNStrm210),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@backSlashRef'(XStIn254, XNStrm211, 9) :- ocall('_hdtl%3'(XStIn254, 116, XNStrm211),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@backSlashRef'(XStIn255, XNStrm212, 10) :- ocall('_hdtl%3'(XStIn255, 110, XNStrm212),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@backSlashRef'(XStIn256, XNStrm213, 13) :- ocall('_hdtl%3'(XStIn256, 114, XNStrm213),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@backSlashRef'(XStIn257, XStx249, XCh) :- ocall('_hdtl%3'(XStIn257, 117, XNStrm214),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@hexChars'(XNStrm214, XStx249, 0, XCh).
'lo.comp.lexer@backSlashRef'(XStIn258, XNStrm215, XCh) :- ocall('_hdtl%3'(XStIn258, XCh, XNStrm215),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@charRef'(XStIn259, XStx250, XChr) :- ocall('_hdtl%3'(XStIn259, 92, XNStrm216),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@backSlashRef'(XNStrm216, XStx250, XChr).
'lo.comp.lexer@charRef'(XStIn260, XNStrm217, XChr) :- ocall('_hdtl%3'(XStIn260, XChr, XNStrm217),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@hexInt'(XStIn261, XStx252, XSoFar, XCh) :- 'lo.comp.lexer@hexDigit'(XStIn261, XStx251, XN),
    ocall('*%3'(XSoFar, 16, XX10376),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('+%3'(XX10376, XN, XX10379),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.lexer@hexInt'(XStx251, XStx252, XX10379, XCh).
'lo.comp.lexer@hexInt'(XStIn262, XStIn262, XSoFar, XSoFar) :- 'lo.comp.lexer@Neg10'(XStIn262, X_691).
'lo.comp.lexer@readNatural'(XStIn263, XStx255, XSoFar, XInt) :- 'lo.comp.lexer@digit'(XStIn263, XStx254, XD),
    ocall('*%3'(XSoFar, 10, XX10389),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer'),
    ocall('+%3'(XX10389, XD, XX10392),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.lexer@readNatural'(XStx254, XStx255, XX10392, XInt).
'lo.comp.lexer@readNatural'(XStIn264, XStIn264, XSoFar, XSoFar) :- 'lo.comp.lexer@Neg11'(XStIn264, X_692).
'lo.comp.lexer@fraction'(XStIn265, XStx258, XScale, XSoFar, XResult) :- 'lo.comp.lexer@digit'(XStIn265, XStx257, XD),
    ocall('*%3'(XScale, 0.1, XX10403),'lo.core$arith$lo.core*float','lo.core$arith$lo.core*float'),
    '_int2flt'(XD, XX10407),
    ocall('*%3'(XX10407, XScale, XX10409),'lo.core$arith$lo.core*float','lo.core$arith$lo.core*float'),
    ocall('+%3'(XSoFar, XX10409, XX10411),'lo.core$additive$lo.core*float','lo.core$additive$lo.core*float'),
    'lo.comp.lexer@fraction'(XStx257, XStx258, XX10403, XX10411, XResult).
'lo.comp.lexer@fraction'(XStIn266, XStIn266, X_693, XFract, XFract).
'lo.comp.lexer@readDecimal'(XStIn267, XStx259, XIn) :- ocall('_hdtl%3'(XStIn267, 45, XNStrm218),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@readNatural'(XNStrm218, XStx259, 0, XPl),
    ocall('-%3'(0, XPl, XX10424),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    XIn = XX10424.
'lo.comp.lexer@readDecimal'(XStIn268, XStx260, XIn) :- 'lo.comp.lexer@readNatural'(XStIn268, XStx260, 0, XIn).
'lo.comp.lexer@exponent'(XStIn269, XStx261, XSoFar, XFp) :- 'lo.comp.lexer@Disj18'(XStIn269, XDjOut21, XNStrm220, XNStrm220, XNStrm219, XNStrm219, XDjStrm18),
    'lo.comp.lexer@readDecimal'(XDjOut21, XStx261, XExp),
    '_int2flt'(XExp, XX10440),
    '_pwr'(10.0, XX10440, XX10441),
    ocall('*%3'(XSoFar, XX10441, XX10442),'lo.core$arith$lo.core*float','lo.core$arith$lo.core*float'),
    XFp = XX10442.
'lo.comp.lexer@exponent'(XStIn270, XStIn270, XFp, XFp).
'lo.comp.lexer@readMoreNumber'(XStIn271, XStx264, 'lo.comp.token#fltTok'(XFp), XDecimal) :- ocall('_hdtl%3'(XStIn271, 46, XNStrm221),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@Hed11'(XNStrm221, X_694),
    'lo.comp.lexer@fraction'(XNStrm221, XStx263, 0.1, 0.0, XFr),
    '_int2flt'(XDecimal, XX10455),
    ocall('+%3'(XX10455, XFr, XX10457),'lo.core$additive$lo.core*float','lo.core$additive$lo.core*float'),
    'lo.comp.lexer@exponent'(XStx263, XStx264, XX10457, XFp).
'lo.comp.lexer@readMoreNumber'(XStIn272, XStIn272, 'lo.comp.token#intTok'(XIx), XIx).
'lo.comp.lexer@readNumber'(XStIn273, XStx266, XTk) :- 'lo.comp.lexer@readNatural'(XStIn273, XStx265, 0, XFirst),
    'lo.comp.lexer@readMoreNumber'(XStx265, XStx266, XTk, XFirst).
'lo.comp.lexer@regExp'(XStIn274, XStIn274, 'lo.core#[]') :- 'lo.comp.lexer@Hed12'(XStIn274, XNStrm222, XNStrm222, XHedStrm12).
'lo.comp.lexer@regExp'(XStIn275, XStx267, 'lo.core#,..'(92, 'lo.core#,..'(XCh, XMore))) :- ocall('_hdtl%3'(XStIn275, 92, XNStrm223),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    ocall('_hdtl%3'(XNStrm223, XCh, XNStrm224),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@regExp'(XNStrm224, XStx267, XMore).
'lo.comp.lexer@regExp'(XStIn276, XStx268, 'lo.core#,..'(XCh, XMore)) :- ocall('_hdtl%3'(XStIn276, XCh, XNStrm225),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@regExp'(XNStrm225, XStx268, XMore).
'lo.comp.lexer@idStart'(XStIn277, XNStrm226, 95) :- ocall('_hdtl%3'(XStIn277, 95, XNStrm226),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@idStart'(XStIn278, XNStrm227, XCh) :- ocall('_hdtl%3'(XStIn278, XCh, XNStrm227),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    '_isLetterChar'(XCh).
'lo.comp.lexer@alphaNum'(XStIn279, XStx269, XCh) :- 'lo.comp.lexer@idStart'(XStIn279, XStx269, XCh).
'lo.comp.lexer@alphaNum'(XStIn280, XDjOut22, XCh) :- ocall('_hdtl%3'(XStIn280, XCh, XNStrm228),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@One4'(XNStrm228, XDjOut22, XCh).
'lo.comp.lexer@readIden'(XStIn281, XStx271, 'lo.core#,..'(XC, XR)) :- 'lo.comp.lexer@alphaNum'(XStIn281, XStx270, XC),
    'lo.comp.lexer@readIden'(XStx270, XStx271, XR).
'lo.comp.lexer@readIden'(XStIn282, XStIn282, 'lo.core#[]').
'lo.comp.lexer@readQuoted'(XStIn283, XStIn283, 'lo.core#[]') :- 'lo.comp.lexer@Hed13'(XStIn283, XNStrm229, XNStrm229, XHedStrm13).
'lo.comp.lexer@readQuoted'(XStIn284, XStx273, 'lo.core#,..'(XC, XR)) :- 'lo.comp.lexer@charRef'(XStIn284, XStx272, XC),
    'lo.comp.lexer@neg16'(XC),
    'lo.comp.lexer@readQuoted'(XStx272, XStx273, XR).
'lo.comp.lexer@countBrackets'(XStIn285, XStIn285, 'lo.core#[]', 'lo.core#[]') :- 'lo.comp.lexer@Hed14'(XStIn285, XNStrm230, XNStrm230, XC, XHedStrm14),
    'lo.comp.lexer@neg17'(XC).
'lo.comp.lexer@countBrackets'(XStIn286, XStx274, 'lo.core#,..'(XCh, XMore), 'lo.core#,..'(XCh, XStack)) :- ocall('_hdtl%3'(XStIn286, XCh, XNStrm231),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@countBrackets'(XNStrm231, XStx274, XMore, XStack).
'lo.comp.lexer@countBrackets'(XStIn287, XStx275, 'lo.core#,..'(40, XMore), XStack) :- ocall('_hdtl%3'(XStIn287, 40, XNStrm232),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@countBrackets'(XNStrm232, XStx275, XMore, 'lo.core#,..'(41, XStack)).
'lo.comp.lexer@countBrackets'(XStIn288, XStx276, 'lo.core#,..'(91, XMore), XStack) :- ocall('_hdtl%3'(XStIn288, 91, XNStrm233),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@countBrackets'(XNStrm233, XStx276, XMore, 'lo.core#,..'(93, XStack)).
'lo.comp.lexer@countBrackets'(XStIn289, XStx277, 'lo.core#,..'(123, XMore), XStack) :- ocall('_hdtl%3'(XStIn289, 123, XNStrm234),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@countBrackets'(XNStrm234, XStx277, XMore, 'lo.core#,..'(125, XStack)).
'lo.comp.lexer@countBrackets'(XStIn290, XStx278, 'lo.core#,..'(34, XMore), XStack) :- ocall('_hdtl%3'(XStIn290, 34, XNStrm235),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@neg18'(XStack),
    'lo.comp.lexer@countBrackets'(XNStrm235, XStx278, XMore, 'lo.core#,..'(34, XStack)).
'lo.comp.lexer@countBrackets'(XStIn291, XStIn291, 'lo.core#[]', 'lo.core#[]') :- 'lo.comp.lexer@Hed15'(XStIn291, XNStrm236, XNStrm236, XHedStrm15).
'lo.comp.lexer@countBrackets'(XStIn292, XStx279, 'lo.core#,..'(XCh, XMore), XStack) :- ocall('_hdtl%3'(XStIn292, XCh, XNStrm237),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@countBrackets'(XNStrm237, XStx279, XMore, XStack).
'lo.comp.lexer@readFormat'(XStIn293, XNStrm238, 'lo.core#[]') :- ocall('_hdtl%3'(XStIn293, 59, XNStrm238),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@readFormat'(XStIn294, XStx280, 'lo.core#,..'(XCh, XMore)) :- ocall('_hdtl%3'(XStIn294, XCh, XNStrm239),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@neg19'(XCh),
    'lo.comp.lexer@neg20'(XCh),
    'lo.comp.lexer@readFormat'(XNStrm239, XStx280, XMore).
'lo.comp.lexer@readFormat'(XStIn295, XStIn295, 'lo.core#[]') :- ocall('_eof%1'(XStIn295),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@makeLocation'((XStartLine, XStartOff, XStartCol, XPth), (X_695, XEndOff, X_696, X_697), 'lo.comp.location#loc'(XStartLine, XStartOff, XStartCol, XX10635, XPth)) :- !,
    ocall('-%3'(XEndOff, XStartOff, XX10635),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer').
'lo.comp.lexer@makeLocation'(_, _, _) :- raise_exception('error'("makeLocation", 45, 3, 124)).
'lo.comp.lexer@interpolation'(XStIn296, XDjOut23, 'lo.comp.token#interpolate'(XLc, XX10641, XX10643)) :- XStIn296 = X_698,
    ocall('currentLocation%1'(XStart),X_698,X_698),
    'lo.comp.lexer@countBrackets'(XStIn296, XStx281, XText, 'lo.core#[]'),
    'lo.comp.lexer@Disj19'(XStx281, XDjOut23, XFormat, XNStrm240, XNStrm240, XDjStrm19),
    XDjOut23 = X_699,
    ocall('currentLocation%1'(XEnd),X_699,X_699),
    'lo.comp.lexer@makeLocation'(XStart, XEnd, XX10662),
    XLc = XX10662,
    'implode'(XText, XX10641),
    'implode'(XFormat, XX10643).
'lo.comp.lexer@readStr'(XStIn297, XStIn297, 'lo.core#[]') :- 'lo.comp.lexer@Hed16'(XStIn297, XNStrm241, XNStrm241, XHedStrm16).
'lo.comp.lexer@readStr'(XStIn298, XStIn298, 'lo.core#[]') :- 'lo.comp.lexer@Hed17'(XStIn298, XNStrm243, XNStrm243, XNStrm242, XNStrm242, XHedStrm17).
'lo.comp.lexer@readStr'(XStIn299, XStx284, 'lo.core#,..'(XCh, XMore)) :- 'lo.comp.lexer@charRef'(XStIn299, XStx283, XCh),
    'lo.comp.lexer@readStr'(XStx283, XStx284, XMore).
'lo.comp.lexer@readStringSegments'(XStIn300, XStIn300, 'lo.core#[]') :- 'lo.comp.lexer@Hed18'(XStIn300, XNStrm244, XNStrm244, XHedStrm18).
'lo.comp.lexer@readStringSegments'(XStIn301, XStx286, 'lo.core#,..'(XSeg, XSegs)) :- ocall('_hdtl%3'(XStIn301, 92, XNStrm245),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@Hed19'(XNStrm245, XNStrm246, XNStrm246, XHedStrm19),
    'lo.comp.lexer@interpolation'(XNStrm245, XStx285, XSeg),
    'lo.comp.lexer@readStringSegments'(XStx285, XStx286, XSegs).
'lo.comp.lexer@readStringSegments'(XStIn302, XStx288, 'lo.core#,..'('lo.comp.token#segment'(XLc, XSeg), XSegs)) :- XStIn302 = X_700,
    ocall('currentLocation%1'(XStart),X_700,X_700),
    'lo.comp.lexer@One5'(XStIn302, XDjOut24, XChars),
    XDjOut24 = X_701,
    ocall('currentLocation%1'(XEnd),X_701,X_701),
    'implode'(XChars, XX10708),
    XSeg = XX10708,
    'lo.comp.lexer@makeLocation'(XStart, XEnd, XX10712),
    XLc = XX10712,
    'lo.comp.lexer@readStringSegments'(XDjOut24, XStx288, XSegs).
'lo.comp.lexer@followGraph'(XStIn303, XStx289, XState, XOp) :- ocall('_hdtl%3'(XStIn303, XCh, XNStrm247),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.operators@follows'(XState, XCh, XNext),
    'lo.comp.lexer@followGraph'(XNStrm247, XStx289, XNext, XOp).
'lo.comp.lexer@followGraph'(XStIn304, XStIn304, XOp, XOp) :- 'lo.comp.operators@final'(XOp).
'lo.comp.lexer@nxtTok'(XStIn305, XStx290, 'lo.comp.token#intTok'(XCh)) :- ocall('_hdtl%3'(XStIn305, 48, XNStrm248),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    ocall('_hdtl%3'(XNStrm248, 99, XNStrm249),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@charRef'(XNStrm249, XStx290, XCh).
'lo.comp.lexer@nxtTok'(XStIn306, XStx291, 'lo.comp.token#intTok'(XHx)) :- ocall('_hdtl%3'(XStIn306, 48, XNStrm250),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    ocall('_hdtl%3'(XNStrm250, 120, XNStrm251),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@hexInt'(XNStrm251, XStx291, 0, XHx).
'lo.comp.lexer@nxtTok'(XStIn307, XStx293, XNum) :- 'lo.comp.lexer@Hed20'(XStIn307, X_702),
    'lo.comp.lexer@readNumber'(XStIn307, XStx293, XNum).
'lo.comp.lexer@nxtTok'(XStIn308, XNStrm252, 'lo.comp.token#lpar') :- ocall('_hdtl%3'(XStIn308, 40, XNStrm252),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@nxtTok'(XStIn309, XNStrm253, 'lo.comp.token#rpar') :- ocall('_hdtl%3'(XStIn309, 41, XNStrm253),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@nxtTok'(XStIn310, XNStrm254, 'lo.comp.token#lbra') :- ocall('_hdtl%3'(XStIn310, 91, XNStrm254),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@nxtTok'(XStIn311, XNStrm255, 'lo.comp.token#rbra') :- ocall('_hdtl%3'(XStIn311, 93, XNStrm255),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@nxtTok'(XStIn312, XNStrm256, 'lo.comp.token#lbrce') :- ocall('_hdtl%3'(XStIn312, 123, XNStrm256),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@nxtTok'(XStIn313, XNStrm257, 'lo.comp.token#rbrce') :- ocall('_hdtl%3'(XStIn313, 125, XNStrm257),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@nxtTok'(XStIn314, XNStrm259, 'lo.comp.token#lqpar') :- ocall('_hdtl%3'(XStIn314, 60, XNStrm258),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    ocall('_hdtl%3'(XNStrm258, 124, XNStrm259),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@nxtTok'(XStIn315, XNStrm261, 'lo.comp.token#rqpar') :- ocall('_hdtl%3'(XStIn315, 124, XNStrm260),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    ocall('_hdtl%3'(XNStrm260, 62, XNStrm261),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@nxtTok'(XStIn316, XStx294, 'lo.comp.token#period') :- ocall('_hdtl%3'(XStIn316, 46, XNStrm262),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@whiteSpace'(XNStrm262, XStx294).
'lo.comp.lexer@nxtTok'(XStIn317, XNStrm264, 'lo.comp.token#regTok'(XRg)) :- ocall('_hdtl%3'(XStIn317, 96, XNStrm263),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@regExp'(XNStrm263, XStx295, XChars),
    ocall('_hdtl%3'(XStx295, 96, XNStrm264),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'implode'(XChars, XX10802),
    XRg = XX10802.
'lo.comp.lexer@nxtTok'(XStIn318, XStx297, 'lo.comp.token#idTok'(XId)) :- 'lo.comp.lexer@idStart'(XStIn318, XStx296, XC),
    'lo.comp.lexer@readIden'(XStx296, XStx297, XR),
    'implode'('lo.core#,..'(XC, XR), XX10811),
    XId = XX10811.
'lo.comp.lexer@nxtTok'(XStIn319, XNStrm266, 'lo.comp.token#idQTok'(XId)) :- ocall('_hdtl%3'(XStIn319, 39, XNStrm265),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@readQuoted'(XNStrm265, XStx298, XQ),
    ocall('_hdtl%3'(XStx298, 39, XNStrm266),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'implode'(XQ, XX10823),
    XId = XX10823.
'lo.comp.lexer@nxtTok'(XStIn320, XNStrm268, 'lo.comp.token#stringTok'(XText)) :- ocall('_hdtl%3'(XStIn320, 34, XNStrm267),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@readStringSegments'(XNStrm267, XStx299, XText),
    ocall('_hdtl%3'(XStx299, 34, XNStrm268),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@nxtTok'(XStIn321, XDjOut25, 'lo.comp.token#idTok'(XOp)) :- ocall('_hdtl%3'(XStIn321, XCh, XNStrm269),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.operators@follows'("", XCh, XNext),
    'lo.comp.lexer@One6'(XNStrm269, XDjOut25, XOp, XNext).
'lo.comp.lexer@allTokens'(XStIn322, XStIn322, 'lo.core#[]', 0).
'lo.comp.lexer@allTokens'(XStIn323, XStIn323, 'lo.core#[]', X_703) :- ocall('_eof%1'(XStIn323),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@allTokens'(XStIn324, XStx303, 'lo.core#,..'('lo.comp.token#tok'(XTk, XLc), XMore), XCx) :- XStIn324 = X_704,
    ocall('currentLocation%1'(XStart),X_704,X_704),
    'lo.comp.lexer@One7'(XStIn324, XDjOut26, XTk),
    XDjOut26 = X_705,
    ocall('currentLocation%1'(XEnd),X_705,X_705),
    'lo.comp.lexer@makeLocation'(XStart, XEnd, XX10864),
    XLc = XX10864,
    'lo.comp.lexer@skipSpaces'(XDjOut26, XStx302),
    ocall('-%3'(XCx, 1, XX10867),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.lexer@allTokens'(XStx302, XStx303, XMore, XX10867).
'lo.comp.lexer@tokenize'(XStIn325, XStx305, XAllTokens) :- 'lo.comp.lexer@skipSpaces'(XStIn325, XStx304),
    'lo.comp.lexer@allTokens'(XStx304, XStx305, XAllTokens, -1).
'lo.comp.lexer#tkState'('tkState%1'('lo.comp.lexer@tkState'())) :- !.
'lo.comp.lexer#tkState'('currentLocation%1'(XV1767), XLbl261, XThis261) :- !,
    'lo.comp.lexer#tkState@currentLocation'(XV1767, XLbl261, XThis261).
'lo.comp.lexer#tkState'('currentLocation%1'('lo.comp.lexer#tkState^currentLocation'(XLbl262, XThis262)), XLbl262, XThis262).
'lo.comp.lexer#tkState@currentLocation'((XLine, XOff, XCol, XPth), XLbV251, XThV251) :- XLbV251 = 'lo.comp.lexer#tkState'(XChars, XLine, XOff, XCol, XPth).
'lo.comp.lexer@startState'(XSrc, XPth, 'lo.comp.lexer#tkState'(XSrc, 1, 0, 1, XPth)) :- !.
'lo.comp.lexer@startState'(_, _, _) :- raise_exception('error'("startState", 27, 3, 45)).
'lo.comp.lexer@tokenizeFile'(XU, XToks) :- 'lo.resources@getResource'(XU, XX10888),
    'explode'(XX10888, XX10889),
    'lo.uri@getUriPath'(XU, XX10891),
    'lo.comp.lexer@startState'(XX10889, XX10891, XX10892),
    'lo.comp.lexer@tokenize'(XX10892, XStx306, XToks),
    ocall('_eof%1'(XStx306),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    !.
'lo.comp.lexer@tokenizeFile'(_, _) :- raise_exception('error'("tokenizeFile", 13, 3, 95)).
'lo.comp.lexer@tokenCodes'(XChars, XPth, XToks) :- 'lo.comp.lexer@startState'(XChars, XPth, XX10901),
    'lo.comp.lexer@tokenize'(XX10901, XStx307, XToks),
    ocall('_eof%1'(XStx307),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    !.
'lo.comp.lexer@tokenCodes'(_, _, _) :- raise_exception('error'("tokenCodes", 16, 3, 72)).
'lo.comp.lexer@getNTokens'(XChars, XPth, XCount, XToks) :- 'lo.comp.lexer@startState'(XChars, XPth, XX10911),
    'lo.comp.lexer@skipSpaces'(XX10911, XStx308),
    'lo.comp.lexer@allTokens'(XStx308, XStx309, XToks, XCount),
    X_706 = XStx309,
    !.
'lo.comp.lexer@getNTokens'(_, _, _, _) :- raise_exception('error'("getNTokens", 19, 3, 105)).
'lo.core$stream$lo.comp.lexer*tokenState'('lo.core$stream$lo.comp.lexer*tokenState%1'('lo.core$stream$lo.comp.lexer*tokenState')) :- !.
'lo.core$stream$lo.comp.lexer*tokenState'('_eof%1'(XV1782), XLbl263, XThis263) :- !,
    'lo.core$stream$lo.comp.lexer*tokenState@_eof'(XV1782, XLbl263, XThis263).
'lo.core$stream$lo.comp.lexer*tokenState'('_eof%1'('lo.core$stream$lo.comp.lexer*tokenState^_eof'(XLbl264, XThis264)), XLbl264, XThis264).
'lo.core$stream$lo.comp.lexer*tokenState'('_hdtl%3'(XV1787, XV1788, XV1789), XLbl265, XThis265) :- !,
    'lo.core$stream$lo.comp.lexer*tokenState@_hdtl'(XV1787, XV1788, XV1789, XLbl265, XThis265).
'lo.core$stream$lo.comp.lexer*tokenState'('_hdtl%1'('lo.core$stream$lo.comp.lexer*tokenState^_hdtl'(XLbl266, XThis266)), XLbl266, XThis266).
'lo.core$stream$lo.comp.lexer*tokenState@_eof'('lo.comp.lexer#tkState'('lo.core#[]', X_707, X_708, X_709, X_710), XLbV252, XThV252).
'lo.core$stream$lo.comp.lexer*tokenState@_hdtl'('lo.comp.lexer#tkState'('lo.core#,..'(10, XRest), XLine, XOff, XCol, XPth), 10, 'lo.comp.lexer#tkState'(XRest, XX10931, XX10934, 1, XPth), XLbV252, XThV252) :- ocall('+%3'(XLine, 1, XX10931),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('+%3'(XOff, 1, XX10934),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer').
'lo.core$stream$lo.comp.lexer*tokenState@_hdtl'('lo.comp.lexer#tkState'('lo.core#,..'(XCh, XRest), XLine, XOff, XCol, XPth), XCh, 'lo.comp.lexer#tkState'(XRest, XLine, XX10950, XX10953, XPth), XLbV252, XThV252) :- 'lo.core$stream$lo.comp.lexer*tokenState@neg21'(XCh),
    ocall('+%3'(XOff, 1, XX10950),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('+%3'(XCol, 1, XX10953),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer').
'lo.comp.lexer@subTokenize'('lo.comp.location#loc'(XLn, XOff, XCol, X_711, XPth), XText, XToks) :- 'explode'(XText, XX10966),
    'lo.comp.lexer@tokenize'('lo.comp.lexer#tkState'(XX10966, XLn, XOff, XCol, XPth), XStx310, XToks),
    ocall('_eof%1'(XStx310),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    !.
'lo.comp.lexer@subTokenize'(_, _, _) :- raise_exception('error'("subTokenize", 68, 3, 110)).
'lo.comp.lexer@or5'(XX) :- '_isZpChar'(XX).
'lo.comp.lexer@or5'(XX) :- '_isCcChar'(XX).
'lo.comp.lexer@or6'(XX) :- '_isZlChar'(XX).
'lo.comp.lexer@or6'(XX) :- 'lo.comp.lexer@or5'(XX).
'lo.comp.lexer@or7'(XX) :- '_isZsChar'(XX).
'lo.comp.lexer@or7'(XX) :- 'lo.comp.lexer@or6'(XX).
'lo.comp.lexer@One3'(XOneStm3, XOneStm3, XX) :- 'lo.comp.lexer@or7'(XX),
    !.
'lo.comp.lexer^whiteSpace'('_call%2'(XV1667, XV1668), 'lo.comp.lexer^whiteSpace', _) :- 'lo.comp.lexer@whiteSpace'(XV1667, XV1668).
'lo.comp.lexer^lineComment'('_call%2'(XV1669, XV1670), 'lo.comp.lexer^lineComment', _) :- 'lo.comp.lexer@lineComment'(XV1669, XV1670).
'lo.comp.lexer^blockComment'('_call%2'(XV1671, XV1672), 'lo.comp.lexer^blockComment', _) :- 'lo.comp.lexer@blockComment'(XV1671, XV1672).
'lo.comp.lexer@Disj17'(XDjStrm17, XNStrm182, XNStrm183, XNStrm183, XNStrm182, XNStrm182, XDjStrm17) :- ocall('_hdtl%3'(XDjStrm17, 32, XNStrm182),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@Disj17'(XDjStrm17, XNStrm183, XNStrm183, XNStrm183, XNStrm182, XNStrm182, XDjStrm17) :- ocall('_hdtl%3'(XDjStrm17, 9, XNStrm183),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer^skipSpaces'('_call%2'(XV1673, XV1674), 'lo.comp.lexer^skipSpaces', _) :- 'lo.comp.lexer@skipSpaces'(XV1673, XV1674).
'lo.comp.lexer^digit'('_call%3'(XV1675, XV1676, XV1677), 'lo.comp.lexer^digit', _) :- 'lo.comp.lexer@digit'(XV1675, XV1676, XV1677).
'lo.comp.lexer^hexDigit'('_call%3'(XV1678, XV1679, XV1680), 'lo.comp.lexer^hexDigit', _) :- 'lo.comp.lexer@hexDigit'(XV1678, XV1679, XV1680).
'lo.comp.lexer^hexChars'('_call%4'(XV1681, XV1682, XV1683, XV1684), 'lo.comp.lexer^hexChars', _) :- 'lo.comp.lexer@hexChars'(XV1681, XV1682, XV1683, XV1684).
'lo.comp.lexer^backSlashRef'('_call%3'(XV1685, XV1686, XV1687), 'lo.comp.lexer^backSlashRef', _) :- 'lo.comp.lexer@backSlashRef'(XV1685, XV1686, XV1687).
'lo.comp.lexer^charRef'('_call%3'(XV1688, XV1689, XV1690), 'lo.comp.lexer^charRef', _) :- 'lo.comp.lexer@charRef'(XV1688, XV1689, XV1690).
'lo.comp.lexer@Neg10'(XNegStrm10, X_691) :- 'lo.comp.lexer@hexDigit'(XNegStrm10, XStx253, X_691),
    !,
    fail.
'lo.comp.lexer@Neg10'(XNegStrm10, X_691).
'lo.comp.lexer^hexInt'('_call%4'(XV1691, XV1692, XV1693, XV1694), 'lo.comp.lexer^hexInt', _) :- 'lo.comp.lexer@hexInt'(XV1691, XV1692, XV1693, XV1694).
'lo.comp.lexer@Neg11'(XNegStrm11, X_692) :- 'lo.comp.lexer@digit'(XNegStrm11, XStx256, X_692),
    !,
    fail.
'lo.comp.lexer@Neg11'(XNegStrm11, X_692).
'lo.comp.lexer^readNatural'('_call%4'(XV1695, XV1696, XV1697, XV1698), 'lo.comp.lexer^readNatural', _) :- 'lo.comp.lexer@readNatural'(XV1695, XV1696, XV1697, XV1698).
'lo.comp.lexer^fraction'('_call%5'(XV1699, XV1700, XV1701, XV1702, XV1703), 'lo.comp.lexer^fraction', _) :- 'lo.comp.lexer@fraction'(XV1699, XV1700, XV1701, XV1702, XV1703).
'lo.comp.lexer^readDecimal'('_call%3'(XV1704, XV1705, XV1706), 'lo.comp.lexer^readDecimal', _) :- 'lo.comp.lexer@readDecimal'(XV1704, XV1705, XV1706).
'lo.comp.lexer@Disj18'(XDjStrm18, XNStrm219, XNStrm220, XNStrm220, XNStrm219, XNStrm219, XDjStrm18) :- ocall('_hdtl%3'(XDjStrm18, 101, XNStrm219),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@Disj18'(XDjStrm18, XNStrm220, XNStrm220, XNStrm220, XNStrm219, XNStrm219, XDjStrm18) :- ocall('_hdtl%3'(XDjStrm18, 69, XNStrm220),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer^exponent'('_call%4'(XV1707, XV1708, XV1709, XV1710), 'lo.comp.lexer^exponent', _) :- 'lo.comp.lexer@exponent'(XV1707, XV1708, XV1709, XV1710).
'lo.comp.lexer@Hed11'(XHedStrm11, X_694) :- 'lo.comp.lexer@digit'(XHedStrm11, XStx262, X_694).
'lo.comp.lexer^readMoreNumber'('_call%4'(XV1711, XV1712, XV1713, XV1714), 'lo.comp.lexer^readMoreNumber', _) :- 'lo.comp.lexer@readMoreNumber'(XV1711, XV1712, XV1713, XV1714).
'lo.comp.lexer^readNumber'('_call%3'(XV1715, XV1716, XV1717), 'lo.comp.lexer^readNumber', _) :- 'lo.comp.lexer@readNumber'(XV1715, XV1716, XV1717).
'lo.comp.lexer@Hed12'(XHedStrm12, XNStrm222, XNStrm222, XHedStrm12) :- ocall('_hdtl%3'(XHedStrm12, 96, XNStrm222),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer^regExp'('_call%3'(XV1718, XV1719, XV1720), 'lo.comp.lexer^regExp', _) :- 'lo.comp.lexer@regExp'(XV1718, XV1719, XV1720).
'lo.comp.lexer^idStart'('_call%3'(XV1721, XV1722, XV1723), 'lo.comp.lexer^idStart', _) :- 'lo.comp.lexer@idStart'(XV1721, XV1722, XV1723).
'lo.comp.lexer@or8'(XCh) :- '_isPcChar'(XCh).
'lo.comp.lexer@or8'(XCh) :- '_isCfChar'(XCh).
'lo.comp.lexer@or9'(XCh) :- '_isMcChar'(XCh).
'lo.comp.lexer@or9'(XCh) :- 'lo.comp.lexer@or8'(XCh).
'lo.comp.lexer@or10'(XCh) :- '_isMnChar'(XCh).
'lo.comp.lexer@or10'(XCh) :- 'lo.comp.lexer@or9'(XCh).
'lo.comp.lexer@or11'(XCh) :- '_isNdChar'(XCh).
'lo.comp.lexer@or11'(XCh) :- 'lo.comp.lexer@or10'(XCh).
'lo.comp.lexer@One4'(XOneStm4, XOneStm4, XCh) :- 'lo.comp.lexer@or11'(XCh),
    !.
'lo.comp.lexer^alphaNum'('_call%3'(XV1724, XV1725, XV1726), 'lo.comp.lexer^alphaNum', _) :- 'lo.comp.lexer@alphaNum'(XV1724, XV1725, XV1726).
'lo.comp.lexer^readIden'('_call%3'(XV1727, XV1728, XV1729), 'lo.comp.lexer^readIden', _) :- 'lo.comp.lexer@readIden'(XV1727, XV1728, XV1729).
'lo.comp.lexer@Hed13'(XHedStrm13, XNStrm229, XNStrm229, XHedStrm13) :- ocall('_hdtl%3'(XHedStrm13, 39, XNStrm229),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@neg16'(XC) :- XC = 39,
    !,
    fail.
'lo.comp.lexer@neg16'(XC).
'lo.comp.lexer^readQuoted'('_call%3'(XV1730, XV1731, XV1732), 'lo.comp.lexer^readQuoted', _) :- 'lo.comp.lexer@readQuoted'(XV1730, XV1731, XV1732).
'lo.comp.lexer@Hed14'(XHedStrm14, XNStrm230, XNStrm230, XC, XHedStrm14) :- ocall('_hdtl%3'(XHedStrm14, XC, XNStrm230),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@neg17'(XC) :- ocall('in%2'(XC, 'lo.core#,..'(40, 'lo.core#,..'(91, 'lo.core#,..'(123, 'lo.core#[]')))),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !,
    fail.
'lo.comp.lexer@neg17'(XC).
'lo.comp.lexer@neg18'(XStack) :- XStack = 'lo.core#[]',
    !,
    fail.
'lo.comp.lexer@neg18'(XStack).
'lo.comp.lexer@Hed15'(XHedStrm15, XNStrm236, XNStrm236, XHedStrm15) :- ocall('_hdtl%3'(XHedStrm15, 34, XNStrm236),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer^countBrackets'('_call%4'(XV1733, XV1734, XV1735, XV1736), 'lo.comp.lexer^countBrackets', _) :- 'lo.comp.lexer@countBrackets'(XV1733, XV1734, XV1735, XV1736).
'lo.comp.lexer@neg19'(XCh) :- XCh = 92,
    !,
    fail.
'lo.comp.lexer@neg19'(XCh).
'lo.comp.lexer@neg20'(XCh) :- XCh = 34,
    !,
    fail.
'lo.comp.lexer@neg20'(XCh).
'lo.comp.lexer^readFormat'('_call%3'(XV1737, XV1738, XV1739), 'lo.comp.lexer^readFormat', _) :- 'lo.comp.lexer@readFormat'(XV1737, XV1738, XV1739).
'lo.comp.lexer^makeLocation'('_call%3'(XV1740, XV1741, XV1742), 'lo.comp.lexer^makeLocation', _) :- 'lo.comp.lexer@makeLocation'(XV1740, XV1741, XV1742).
'lo.comp.lexer@Disj19'(XDjStrm19, XStx282, XFormat, XNStrm240, XNStrm240, XDjStrm19) :- ocall('_hdtl%3'(XDjStrm19, 58, XNStrm240),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    'lo.comp.lexer@readFormat'(XNStrm240, XStx282, XFormat).
'lo.comp.lexer@Disj19'(XDjStrm19, XDjStrm19, XFormat, XNStrm240, XNStrm240, XDjStrm19) :- XFormat = 'lo.core#[]'.
'lo.comp.lexer^interpolation'('_call%3'(XV1743, XV1744, XV1745), 'lo.comp.lexer^interpolation', _) :- 'lo.comp.lexer@interpolation'(XV1743, XV1744, XV1745).
'lo.comp.lexer@Hed16'(XHedStrm16, XNStrm241, XNStrm241, XHedStrm16) :- ocall('_hdtl%3'(XHedStrm16, 34, XNStrm241),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@Hed17'(XHedStrm17, XNStrm243, XNStrm243, XNStrm242, XNStrm242, XHedStrm17) :- ocall('_hdtl%3'(XHedStrm17, 92, XNStrm242),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState'),
    ocall('_hdtl%3'(XNStrm242, 40, XNStrm243),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer^readStr'('_call%3'(XV1746, XV1747, XV1748), 'lo.comp.lexer^readStr', _) :- 'lo.comp.lexer@readStr'(XV1746, XV1747, XV1748).
'lo.comp.lexer@Hed18'(XHedStrm18, XNStrm244, XNStrm244, XHedStrm18) :- ocall('_hdtl%3'(XHedStrm18, 34, XNStrm244),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@Hed19'(XHedStrm19, XNStrm246, XNStrm246, XHedStrm19) :- ocall('_hdtl%3'(XHedStrm19, 40, XNStrm246),'lo.core$stream$lo.comp.lexer*tokenState','lo.core$stream$lo.comp.lexer*tokenState').
'lo.comp.lexer@One5'(XOneStm5, XStx287, XChars) :- 'lo.comp.lexer@readStr'(XOneStm5, XStx287, XChars),
    !.
'lo.comp.lexer^readStringSegments'('_call%3'(XV1749, XV1750, XV1751), 'lo.comp.lexer^readStringSegments', _) :- 'lo.comp.lexer@readStringSegments'(XV1749, XV1750, XV1751).
'lo.comp.lexer^followGraph'('_call%4'(XV1752, XV1753, XV1754, XV1755), 'lo.comp.lexer^followGraph', _) :- 'lo.comp.lexer@followGraph'(XV1752, XV1753, XV1754, XV1755).
'lo.comp.lexer@Hed20'(XHedStrm20, X_702) :- 'lo.comp.lexer@digit'(XHedStrm20, XStx292, X_702).
'lo.comp.lexer@One6'(XOneStm6, XStx300, XOp, XNext) :- 'lo.comp.lexer@followGraph'(XOneStm6, XStx300, XNext, XOp),
    !.
'lo.comp.lexer^nxtTok'('_call%3'(XV1756, XV1757, XV1758), 'lo.comp.lexer^nxtTok', _) :- 'lo.comp.lexer@nxtTok'(XV1756, XV1757, XV1758).
'lo.comp.lexer@One7'(XOneStm7, XStx301, XTk) :- 'lo.comp.lexer@nxtTok'(XOneStm7, XStx301, XTk),
    !.
'lo.comp.lexer^allTokens'('_call%4'(XV1759, XV1760, XV1761, XV1762), 'lo.comp.lexer^allTokens', _) :- 'lo.comp.lexer@allTokens'(XV1759, XV1760, XV1761, XV1762).
'lo.comp.lexer^tokenize'('_call%3'(XV1763, XV1764, XV1765), 'lo.comp.lexer^tokenize', _) :- 'lo.comp.lexer@tokenize'(XV1763, XV1764, XV1765).
'lo.comp.lexer#tkState^currentLocation'('_call%1'(XV1766), 'lo.comp.lexer#tkState^currentLocation'(XLbV251, XThV251), _) :- 'lo.comp.lexer#tkState@currentLocation'(XV1766, XLbV251, XThV251).
'lo.comp.lexer#tkState^currentLocation'('_call%1'(XV1768), 'lo.comp.lexer#tkState^currentLocation'(XLbV251, XThV251), _) :- 'lo.comp.lexer#tkState@currentLocation'(XV1768, XLbV251, XThV251).
'lo.comp.lexer^startState'('_call%3'(XV1769, XV1770, XV1771), 'lo.comp.lexer^startState', _) :- 'lo.comp.lexer@startState'(XV1769, XV1770, XV1771).
'lo.comp.lexer^tokenizeFile'('_call%2'(XV1772, XV1773), 'lo.comp.lexer^tokenizeFile', _) :- 'lo.comp.lexer@tokenizeFile'(XV1772, XV1773).
'lo.comp.lexer^tokenCodes'('_call%3'(XV1774, XV1775, XV1776), 'lo.comp.lexer^tokenCodes', _) :- 'lo.comp.lexer@tokenCodes'(XV1774, XV1775, XV1776).
'lo.comp.lexer^getNTokens'('_call%4'(XV1777, XV1778, XV1779, XV1780), 'lo.comp.lexer^getNTokens', _) :- 'lo.comp.lexer@getNTokens'(XV1777, XV1778, XV1779, XV1780).
'lo.core$stream$lo.comp.lexer*tokenState^_eof'('_call%1'(XV1781), 'lo.core$stream$lo.comp.lexer*tokenState^_eof'(XLbV252, XThV252), _) :- 'lo.core$stream$lo.comp.lexer*tokenState@_eof'(XV1781, XLbV252, XThV252).
'lo.core$stream$lo.comp.lexer*tokenState^_eof'('_call%1'(XV1783), 'lo.core$stream$lo.comp.lexer*tokenState^_eof'(XLbV252, XThV252), _) :- 'lo.core$stream$lo.comp.lexer*tokenState@_eof'(XV1783, XLbV252, XThV252).
'lo.core$stream$lo.comp.lexer*tokenState@neg21'(XCh) :- XCh = 10,
    !,
    fail.
'lo.core$stream$lo.comp.lexer*tokenState@neg21'(XCh).
'lo.core$stream$lo.comp.lexer*tokenState^_hdtl'('_call%3'(XV1784, XV1785, XV1786), 'lo.core$stream$lo.comp.lexer*tokenState^_hdtl'(XLbV252, XThV252), _) :- 'lo.core$stream$lo.comp.lexer*tokenState@_hdtl'(XV1784, XV1785, XV1786, XLbV252, XThV252).
'lo.core$stream$lo.comp.lexer*tokenState^_hdtl'('_call%3'(XV1790, XV1791, XV1792), 'lo.core$stream$lo.comp.lexer*tokenState^_hdtl'(XLbV252, XThV252), _) :- 'lo.core$stream$lo.comp.lexer*tokenState@_hdtl'(XV1790, XV1791, XV1792, XLbV252, XThV252).
'lo.comp.lexer^subTokenize'('_call%3'(XV1793, XV1794, XV1795), 'lo.comp.lexer^subTokenize', _) :- 'lo.comp.lexer@subTokenize'(XV1793, XV1794, XV1795).
