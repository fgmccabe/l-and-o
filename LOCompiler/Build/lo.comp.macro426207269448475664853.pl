'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.macro's'0.0.1'n5o5'()5'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.ast'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.abstract'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'s\"I1'macroRewrite'PT4Lt'lo.comp.ast*ast'Lt'lo.comp.ast*ast't'lo.comp.errors*report't'lo.comp.errors*report'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.macro@init'():- !.
'lo.comp.macro@listComma'('lo.core#,..'(XT, 'lo.core#[]'), X_35152, XT).
'lo.comp.macro@listComma'('lo.core#,..'(XT, XR), XLc, XXb20229):- 'lo.comp.macro@listComma'(XR, XLc, XRR),
    'lo.comp.abstract@binary'(XLc, ",", XT, XRR, XXb20229).
'lo.comp.macro@wrapConstraints'('lo.core#[]', X_35154, XTp, XTp).
'lo.comp.macro@wrapConstraints'(XCon, XLc, XTp, XXb20230):- 'lo.comp.macro@listComma'(XCon, XLc, XCTp),
    'lo.comp.abstract@binary'(XLc, "|:", XCTp, XTp, XXb20230).
'lo.comp.macro@wrapQuants'('lo.core#[]', X_35155, XRule, XRule).
'lo.comp.macro@wrapQuants'(XQ, XLc, XRl, XXb20232):- 'lo.comp.macro@listComma'(XQ, XLc, XQV),
    'lo.comp.abstract@binary'(XLc, "~~", XQV, XRl, XXb20231),
    'lo.comp.abstract@unary'(XLc, "all", XXb20231, XXb20232).
'lo.comp.macro@generateAnnotations'('lo.core#[]', X_35156, X_35157, XStmts, XStmts).
'lo.comp.macro@generateAnnotations'('lo.core#,..'(XDef, XEls), XQuants, XConstraints, 'lo.core#,..'(XXb20234, XStmts), XS0):- 'lo.comp.abstract@isBinary'(XDef, ":", XLc, XN, XTp),
    'lo.comp.macro@wrapConstraints'(XConstraints, XLc, XTp, XCTp),
    'lo.comp.macro@wrapQuants'(XQuants, XLc, XCTp, XMTp),
    'lo.comp.macro@generateAnnotations'(XEls, XQuants, XConstraints, XStmts, XS0),
    'lo.comp.abstract@binary'(XLc, ":", XN, XMTp, XXb20234).
'lo.comp.macro@generateAnnotations'('lo.core#,..'(X_35161, XEls), XQuants, XConstraints, XStmts, XS0):- 'lo.comp.macro@generateAnnotations'(XEls, XQuants, XConstraints, XStmts, XS0).
'lo.comp.macro#noMark'('noMark%1'('lo.comp.macro@noMark')):- !.
'lo.comp.macro#markPrivate'('markPrivate%1'('lo.comp.macro@markPrivate')):- !.
'lo.comp.macro#markPublic'('markPublic%1'('lo.comp.macro@markPublic')):- !.
'lo.comp.macro@checkSoFar'(X_35162, X_35163, 'lo.core#[]', 'lo.core#[]', XRp, XRp).
'lo.comp.macro@checkSoFar'(XNm, XT, 'lo.core#,..'(XP, XL), XL, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XP, ":", XLc, XNN, XRR),
    'lo.comp.abstract@isIden'(XNN, X_35165, XNm),
    'lo.comp.macro@cond416'(XLc, XXd40083, XXd40082, XXd40081, XXd40080, XXd40079, XXd40078, XXd40077, XXd40076, XXe5085, XXV5455, XXd40075, XXe5084, XXV5454, XXd40074, XXe5083, XXV5453, XNm, XXd40073, XRpx, XRp, XT, XRR).
'lo.comp.macro@checkSoFar'(XNm, XT, 'lo.core#,..'(XP, XL), 'lo.core#,..'(XP, XLL), XRp, XRpx):- 'lo.comp.macro@checkSoFar'(XNm, XT, XL, XLL, XRp, XRpx).
'lo.comp.macro@pickupFields'('lo.core#[]', XFace, XFace, XRp, XRp).
'lo.comp.macro@pickupFields'('lo.core#,..'(XT, XM), XSF, XFace, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XT, ":", XLc, XL, XR),
    'lo.comp.abstract@isIden'(XL, X_35175, XNm),
    'lo.comp.macro@checkSoFar'(XNm, XR, XSF, XSF0, XRp, XRp0),
    'lo.comp.macro@pickupFields'(XM, 'lo.core#,..'(XT, XSF0), XFace, XRp0, XRpx).
'lo.comp.macro@pickupFields'('lo.core#,..'(XT, XM), XSF, XFace, XRp, XRpx):- ocall('disp%1'(XXV5456),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('_call%2'(XT, XXe5086),XXV5456,XXV5456),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("invalid type field: "), 'lo.core#,..'(XXe5086, 'lo.core#[]'))), XXd40089),
    ocall('loc%1'(XXV5457),XT,XT),
    'lo.comp.errors@reportError'(XXd40089, XXV5457, XRp, XRp0),
    'lo.comp.macro@pickupFields'(XM, XSF, XFace, XRp0, XRpx).
'lo.comp.macro@algebraicFace'(XT, XSoFar, XFace, XRp, XRpx):- 'lo.comp.abstract@isBinary'(XT, "|", X_35180, XL, XR),
    'lo.comp.macro@algebraicFace'(XL, XSoFar, XSF, XRp, XRp0),
    'lo.comp.macro@algebraicFace'(XR, XSF, XFace, XRp0, XRpx).
'lo.comp.macro@algebraicFace'(XT, XSoFar, XSoFar, XRp, XRp):- 'lo.comp.abstract@isRoundTerm'(XT, X_35181, X_35182, X_35183).
'lo.comp.macro@algebraicFace'(XT, XFace, XFace, XRp, XRp):- 'lo.comp.abstract@isIden'(XT, X_35184, X_35185).
'lo.comp.macro@algebraicFace'(XT, XSoFar, XFace, XRp, XRpx):- 'lo.comp.abstract@isBraceTerm'(XT, X_35186, X_35187, XArgs),
    'lo.comp.macro@pickupFields'(XArgs, XSoFar, XFace, XRp, XRpx).
'lo.comp.macro@typeRule'(XLc, XQuants, XConstraints, XHd, XBody, XXb20242):- 'lo.comp.abstract@binary'(XLc, "<~", XHd, XBody, XXd40090),
    'lo.comp.macro@wrapConstraints'(XConstraints, XLc, XXd40090, XConRl),
    'lo.comp.macro@wrapQuants'(XQuants, XLc, XConRl, XRule),
    'lo.comp.abstract@unary'(XLc, "type", XRule, XXb20242).
'lo.comp.macro@mark'('lo.comp.macro#markPublic', XLc, XStmt, XXb20243):- 'lo.comp.abstract@unary'(XLc, "public", XStmt, XXb20243).
'lo.comp.macro@mark'('lo.comp.macro#markPrivate', XLc, XStmt, XXb20244):- 'lo.comp.abstract@unary'(XLc, "private", XStmt, XXb20244).
'lo.comp.macro@mark'('lo.comp.macro#noMark', X_35188, XStmt, XStmt).
'lo.comp.macro@genFieldArgs'('lo.core#[]', 'lo.core#[]', 'lo.core#[]').
'lo.comp.macro@genFieldArgs'('lo.core#,..'(XF, XM), 'lo.core#,..'(XV, XR), 'lo.core#,..'(XXb20247, XAR)):- 'lo.comp.abstract@isBinary'(XF, ":", XLc, XL, X_35192),
    '_str_gen'("_", XXc532),
    XV = 'lo.comp.ast#iden'(XLc, XXc532),
    'lo.comp.macro@genFieldArgs'(XM, XR, XAR),
    'lo.comp.abstract@binary'(XLc, "=", XL, XV, XXb20247).
'lo.comp.macro@classType'(XLc, XArgs, XRes, XXb20249):- 'lo.comp.abstract@isRoundTuple'(XA, XLc, XArgs),
    'lo.comp.abstract@binary'(XLc, "<=>", XA, XRes, XXb20249).
'lo.comp.macro@genAnonArgs'('lo.core#[]', 'lo.core#[]').
'lo.comp.macro@genAnonArgs'('lo.core#,..'(XT, XM), 'lo.core#,..'('lo.comp.ast#iden'(XXV5458, "_"), XA)):- ocall('loc%1'(XXV5458),XT,XT),
    'lo.comp.macro@genAnonArgs'(XM, XA).
'lo.comp.macro@bodyRule'(XLc, XHd, XEls, XXb20254):- 'lo.comp.abstract@braceTuple'(XLc, XEls, XXb20253),
    'lo.comp.abstract@binary'(XLc, "<=", XHd, XXb20253, XXb20254).
'lo.comp.macro@convertConstructor'(XNm, XTp, XMark, XQuants, XConstraints, 'lo.core#,..'(XTpRule, 'lo.core#,..'(XBodyRule, XTail)), XTail):- 'lo.comp.abstract@isIden'(XNm, XLc, X_35197),
    'lo.comp.macro@wrapConstraints'(XConstraints, XLc, XTp, XTpCon),
    'lo.comp.macro@wrapQuants'(XQuants, XLc, XTpCon, XQTp),
    'lo.comp.abstract@binary'(XLc, ":", XNm, XQTp, XXd40092),
    'lo.comp.macro@mark'(XMark, XLc, XXd40092, XTpRule),
    'lo.comp.macro@bodyRule'(XLc, XNm, 'lo.core#[]', XBodyRule).
'lo.comp.macro@convertConstructor'(XCon, XTp, XMark, XQuants, XConstraints, 'lo.core#,..'(XTpRule, 'lo.core#,..'(XBodyRule, XTail)), XTail):- 'lo.comp.abstract@isRoundTerm'(XCon, XLc, XOp, XArgTypes),
    'lo.comp.macro@classType'(XLc, XArgTypes, XTp, XClassType),
    'lo.comp.macro@wrapConstraints'(XConstraints, XLc, XClassType, XCTp),
    'lo.comp.macro@wrapQuants'(XQuants, XLc, XCTp, XQTp),
    'lo.comp.abstract@binary'(XLc, ":", XOp, XQTp, XXd40093),
    'lo.comp.macro@mark'(XMark, XLc, XXd40093, XTpRule),
    'lo.comp.macro@genAnonArgs'(XArgTypes, XArgs),
    'lo.comp.abstract@roundTerm'(XLc, XOp, XArgs, XXd40094),
    'lo.comp.macro@bodyRule'(XLc, XXd40094, 'lo.core#[]', XBodyRule).
'lo.comp.macro@convertConstructor'(XCon, XTp, XMark, XQuants, XConstraints, 'lo.core#,..'(XTpRule, 'lo.core#,..'(XBodyRule, XTail)), XTail):- 'lo.comp.abstract@isBraceTerm'(XCon, X_35202, XOp, XFldTps),
    'lo.comp.abstract@braceTuple'(XLc, XFldTps, XXd40095),
    'lo.comp.abstract@binary'(XLc, "<=>", XXd40095, XTp, XXd40096),
    'lo.comp.macro@wrapConstraints'(XConstraints, XLc, XXd40096, XCTp),
    'lo.comp.macro@wrapQuants'(XQuants, XLc, XCTp, XQTp),
    'lo.comp.abstract@binary'(XLc, ":", XOp, XQTp, XXd40097),
    'lo.comp.macro@mark'(XMark, XLc, XXd40097, XTpRule),
    'lo.comp.macro@genFieldArgs'(XFldTps, XArgs, XDefs),
    'lo.comp.abstract@roundTerm'(XLc, XOp, XArgs, XXd40098),
    'lo.comp.macro@bodyRule'(XLc, XXd40098, XDefs, XBodyRule).
'lo.comp.macro@convertConstructors'(XPair, XHead, XMark, XQuants, XConstraints, XElements, XTail):- 'lo.comp.abstract@isBinary'(XPair, "|", X_35203, XL, XR),
    'lo.comp.macro@convertConstructors'(XL, XHead, XMark, XQuants, XConstraints, XElements, XL1),
    'lo.comp.macro@convertConstructors'(XR, XHead, XMark, XQuants, XConstraints, XL1, XTail).
'lo.comp.macro@convertConstructors'(XTerm, XHead, XMark, XQuants, XConstraints, XElements, XTail):- 'lo.comp.macro@convertConstructor'(XTerm, XHead, XMark, XQuants, XConstraints, XElements, XTail).
'lo.comp.macro@convertAlgebraic'(XLc, XMark, XQuants, XConstraints, XHead, XBody, 'lo.core#,..'(XTypeRule, XElements), XTail, XRp, XRpx):- 'lo.comp.macro@algebraicFace'(XBody, 'lo.core#[]', XEls, XRp, XRpx),
    'lo.comp.abstract@isBraceTuple'(XFace, XLc, XEls),
    'lo.comp.macro@typeRule'(XLc, XQuants, XConstraints, XHead, XFace, XFaceRule),
    'lo.comp.macro@mark'(XMark, XLc, XFaceRule, XTypeRule),
    'lo.comp.macro@convertConstructors'(XBody, XHead, XMark, XQuants, XConstraints, XElements, XTail).
'lo.comp.macro@macroRewrite'('lo.core#[]', 'lo.core#[]', XRp, XRp).
'lo.comp.macro@macroRewrite'('lo.core#,..'(XSt, XMore), XStmts, XRp, XRpx):- 'lo.comp.abstract@isUnary'(XSt, "public", X_35206, XInner),
    'lo.comp.abstract@isAlgebraicTypeDef'(XInner, XLc, XQuants, XConstraints, XHead, XBody),
    'lo.comp.macro@convertAlgebraic'(XLc, 'lo.comp.macro#markPublic', XQuants, XConstraints, XHead, XBody, XStmts, XS0, XRp, XRp0),
    'lo.comp.macro@macroRewrite'(XMore, XS0, XRp0, XRpx).
'lo.comp.macro@macroRewrite'('lo.core#,..'(XSt, XMore), XStmts, XRp, XRpx):- 'lo.comp.abstract@isUnary'(XSt, "private", X_35208, XInner),
    'lo.comp.abstract@isAlgebraicTypeDef'(XInner, XLc, XQuants, XConstraints, XHead, XBody),
    'lo.comp.macro@convertAlgebraic'(XLc, 'lo.comp.macro#markPrivate', XQuants, XConstraints, XHead, XBody, XStmts, XS0, XRp, XRp0),
    'lo.comp.macro@macroRewrite'(XMore, XS0, XRp0, XRpx).
'lo.comp.macro@macroRewrite'('lo.core#,..'(XSt, XMore), XStmts, XRp, XRpx):- 'lo.comp.abstract@isAlgebraicTypeDef'(XSt, XLc, XQuants, XConstraints, XHead, XBody),
    'lo.comp.macro@convertAlgebraic'(XLc, 'lo.comp.macro#noMark', XQuants, XConstraints, XHead, XBody, XStmts, XS0, XRp, XRp0),
    'lo.comp.macro@macroRewrite'(XMore, XS0, XRp0, XRpx).
'lo.comp.macro@macroRewrite'('lo.core#,..'(XSt, XMore), 'lo.core#,..'(XSt, XStmts), XRp, XRpx):- 'lo.comp.abstract@isContractSpec'(XSt, X_35212, XQuants, XConstraints, XCon, XEls),
    'lo.comp.macro@generateAnnotations'(XEls, XQuants, 'lo.core#,..'(XCon, XConstraints), XStmts, XS0),
    'lo.comp.macro@macroRewrite'(XMore, XS0, XRp, XRpx).
'lo.comp.macro@macroRewrite'('lo.core#,..'(XSt, XMore), 'lo.core#,..'(XSt, XStmts), XRp, XRpx):- 'lo.comp.macro@macroRewrite'(XMore, XStmts, XRp, XRpx).
'lo.comp.macro@hasType'(XLc, XNm, XTp, XXb20270):- 'lo.comp.abstract@binary'(XLc, ":", 'lo.comp.ast#iden'(XLc, XNm), XTp, XXb20270).
'lo.comp.macro@funType'(XLc, XArgs, XRes, XXb20271):- 'lo.comp.abstract@isRoundTuple'(XA, XLc, XArgs),
    'lo.comp.abstract@binary'(XLc, "=>", XA, XRes, XXb20271).
'lo.comp.macro@genericType'(XLc, XNm, XArgs, XXb20272):- 'lo.comp.abstract@squareTerm'(XLc, XNm, XArgs, XXb20272).
'lo.comp.macro^listComma'('_call%3'(XV31993, XV31994, XV31995), 'lo.comp.macro^listComma', _):- 'lo.comp.macro@listComma'(XV31993, XV31994, XV31995).
'lo.comp.macro^wrapConstraints'('_call%4'(XV31996, XV31997, XV31998, XV31999), 'lo.comp.macro^wrapConstraints', _):- 'lo.comp.macro@wrapConstraints'(XV31996, XV31997, XV31998, XV31999).
'lo.comp.macro^wrapQuants'('_call%4'(XV32000, XV32001, XV32002, XV32003), 'lo.comp.macro^wrapQuants', _):- 'lo.comp.macro@wrapQuants'(XV32000, XV32001, XV32002, XV32003).
'lo.comp.macro^generateAnnotations'('_call%5'(XV32004, XV32005, XV32006, XV32007, XV32008), 'lo.comp.macro^generateAnnotations', _):- 'lo.comp.macro@generateAnnotations'(XV32004, XV32005, XV32006, XV32007, XV32008).
'lo.comp.macro@cond416'(XLc, XXd40083, XXd40082, XXd40081, XXd40080, XXd40079, XXd40078, XXd40077, XXd40076, XXe5085, XXV5455, XXd40075, XXe5084, XXV5454, XXd40074, XXe5083, XXV5453, XNm, XXd40073, XRpx, XRp, XT, XRR):- 'lo.comp.abstract@sameTerm'(XRR, XT),
    !,
    XRp = XRpx.
'lo.comp.macro@cond416'(XLc, XXd40083, XXd40082, XXd40081, XXd40080, XXd40079, XXd40078, XXd40077, XXd40076, XXe5085, XXV5455, XXd40075, XXe5084, XXV5454, XXd40074, XXe5083, XXV5453, XNm, XXd40073, XRpx, XRp, XT, XRR):- ocall('disp%1'(XXV5453),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('disp%1'(XXV5454),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('disp%1'(XXV5455),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('_call%2'(XNm, XXe5083),XXV5453,XXV5453),
    ocall('_call%2'(XT, XXe5084),XXV5454,XXV5454),
    ocall('_call%2'(XRR, XXe5085),XXV5455,XXV5455),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("field "), 'lo.core#,..'(XXe5083, 'lo.core#,..'('lo.core#ss'(" : "), 'lo.core#,..'(XXe5084, 'lo.core#,..'('lo.core#ss'(" must be identical to: "), 'lo.core#,..'(XXe5085, 'lo.core#[]'))))))), XXd40083),
    'lo.comp.errors@reportError'(XXd40083, XLc, XRp, XRpx).
'lo.comp.macro^checkSoFar'('_call%6'(XV32009, XV32010, XV32011, XV32012, XV32013, XV32014), 'lo.comp.macro^checkSoFar', _):- 'lo.comp.macro@checkSoFar'(XV32009, XV32010, XV32011, XV32012, XV32013, XV32014).
'lo.comp.macro^pickupFields'('_call%5'(XV32015, XV32016, XV32017, XV32018, XV32019), 'lo.comp.macro^pickupFields', _):- 'lo.comp.macro@pickupFields'(XV32015, XV32016, XV32017, XV32018, XV32019).
'lo.comp.macro^algebraicFace'('_call%5'(XV32020, XV32021, XV32022, XV32023, XV32024), 'lo.comp.macro^algebraicFace', _):- 'lo.comp.macro@algebraicFace'(XV32020, XV32021, XV32022, XV32023, XV32024).
'lo.comp.macro^typeRule'('_call%6'(XV32025, XV32026, XV32027, XV32028, XV32029, XV32030), 'lo.comp.macro^typeRule', _):- 'lo.comp.macro@typeRule'(XV32025, XV32026, XV32027, XV32028, XV32029, XV32030).
'lo.comp.macro^mark'('_call%4'(XV32031, XV32032, XV32033, XV32034), 'lo.comp.macro^mark', _):- 'lo.comp.macro@mark'(XV32031, XV32032, XV32033, XV32034).
'lo.comp.macro^genFieldArgs'('_call%3'(XV32035, XV32036, XV32037), 'lo.comp.macro^genFieldArgs', _):- 'lo.comp.macro@genFieldArgs'(XV32035, XV32036, XV32037).
'lo.comp.macro^classType'('_call%4'(XV32038, XV32039, XV32040, XV32041), 'lo.comp.macro^classType', _):- 'lo.comp.macro@classType'(XV32038, XV32039, XV32040, XV32041).
'lo.comp.macro^genAnonArgs'('_call%2'(XV32042, XV32043), 'lo.comp.macro^genAnonArgs', _):- 'lo.comp.macro@genAnonArgs'(XV32042, XV32043).
'lo.comp.macro^bodyRule'('_call%4'(XV32044, XV32045, XV32046, XV32047), 'lo.comp.macro^bodyRule', _):- 'lo.comp.macro@bodyRule'(XV32044, XV32045, XV32046, XV32047).
'lo.comp.macro^convertConstructor'('_call%7'(XV32048, XV32049, XV32050, XV32051, XV32052, XV32053, XV32054), 'lo.comp.macro^convertConstructor', _):- 'lo.comp.macro@convertConstructor'(XV32048, XV32049, XV32050, XV32051, XV32052, XV32053, XV32054).
'lo.comp.macro^convertConstructors'('_call%7'(XV32055, XV32056, XV32057, XV32058, XV32059, XV32060, XV32061), 'lo.comp.macro^convertConstructors', _):- 'lo.comp.macro@convertConstructors'(XV32055, XV32056, XV32057, XV32058, XV32059, XV32060, XV32061).
'lo.comp.macro^convertAlgebraic'('_call%10'(XV32062, XV32063, XV32064, XV32065, XV32066, XV32067, XV32068, XV32069, XV32070, XV32071), 'lo.comp.macro^convertAlgebraic', _):- 'lo.comp.macro@convertAlgebraic'(XV32062, XV32063, XV32064, XV32065, XV32066, XV32067, XV32068, XV32069, XV32070, XV32071).
'lo.comp.macro^macroRewrite'('_call%4'(XV32072, XV32073, XV32074, XV32075), 'lo.comp.macro^macroRewrite', _):- 'lo.comp.macro@macroRewrite'(XV32072, XV32073, XV32074, XV32075).
'lo.comp.macro^hasType'('_call%4'(XV32076, XV32077, XV32078, XV32079), 'lo.comp.macro^hasType', _):- 'lo.comp.macro@hasType'(XV32076, XV32077, XV32078, XV32079).
'lo.comp.macro^funType'('_call%4'(XV32080, XV32081, XV32082, XV32083), 'lo.comp.macro^funType', _):- 'lo.comp.macro@funType'(XV32080, XV32081, XV32082, XV32083).
'lo.comp.macro^genericType'('_call%4'(XV32084, XV32085, XV32086, XV32087), 'lo.comp.macro^genericType', _):- 'lo.comp.macro@genericType'(XV32084, XV32085, XV32086, XV32087).
