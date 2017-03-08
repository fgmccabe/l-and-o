'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.errors'e'*'n13o13'()13'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.json'e'*'n2o2'import'e'private'n2o2'pkg's'lo.array'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I13'sourcedMsg'CT2t'lo.comp.errors*reportMsg'St'lo.comp.errors*reportMsg''errorMsg'CT2t'lo.comp.location*location'St'lo.comp.errors*reportMsg''warnMsg'CT2t'lo.comp.location*location'St'lo.comp.errors*reportMsg''othMsg'CT2t'lo.comp.location*location'St'lo.comp.errors*reportMsg''countErrors'FT1t'lo.comp.errors*report'i'countWarnings'FT1t'lo.comp.errors*report'i'errorFree'PT1t'lo.comp.errors*report''reportBase't'lo.comp.errors*report''reportError'PT4St'lo.comp.location*location't'lo.comp.errors*report't'lo.comp.errors*report''reportWarn'PT4St'lo.comp.location*location't'lo.comp.errors*report't'lo.comp.errors*report''reportMsg'PT4St'lo.comp.location*location't'lo.comp.errors*report't'lo.comp.errors*report''populateContext'FT3t'lo.comp.errors*report'SUz1'lo.array*array'1St'lo.comp.errors*report''fullReport'FT1t'lo.comp.errors*report't'lo.core*ss'\"s\"I2'reportMsg'Yt'lo.comp.errors*reportMsg'I0'report'Yt'lo.comp.errors*report'I1'msgs'Lt'lo.comp.errors*reportMsg'\"n0o0'()0'n0o0'()0'n2o2'()2'n2o2'()2's'lo.core$display$lo.comp.errors*report's\"c'lo.core$display'T1t'lo.comp.errors*report'T0\"n2o2'()2's'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json's\"c'lo.coerce$coercion'T2t'lo.comp.errors*reportMsg't'lo.json*json'T0\"").
'lo.comp.errors@init'() :- !.
'lo.comp.errors#sourcedMsg'('sourcedMsg%1'('lo.comp.errors@sourcedMsg'())) :- !.
'lo.comp.errors#errorMsg'('errorMsg%1'('lo.comp.errors@errorMsg'())) :- !.
'lo.comp.errors#warnMsg'('warnMsg%1'('lo.comp.errors@warnMsg'())) :- !.
'lo.comp.errors#othMsg'('othMsg%1'('lo.comp.errors@othMsg'())) :- !.
'lo.comp.errors#report'('report%1'('lo.comp.errors@report'())) :- !.
'lo.comp.errors#report'('msgs%1'(XV1239), XLbl197, XThis197) :- !,
    'lo.comp.errors#report@msgs'(XV1239, XLbl197, XThis197).
'lo.comp.errors#report@msgs'(XLst, XLbV213, XThV213) :- XLbV213 = 'lo.comp.errors#report'(XLst),
    !.
'lo.comp.errors@cntErrors'('lo.core#[]', XC, XC) :- !.
'lo.comp.errors@cntErrors'('lo.core#,..'('lo.comp.errors#sourcedMsg'('lo.comp.errors#errorMsg'(X_578, X_579), X_580), XL), XC, XX6757) :- !,
    ocall('+%3'(XC, 1, XX6755),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.errors@cntErrors'(XL, XX6755, XX6757).
'lo.comp.errors@cntErrors'('lo.core#,..'('lo.comp.errors#errorMsg'(X_581, X_582), XL), XC, XX6768) :- !,
    ocall('+%3'(XC, 1, XX6766),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.errors@cntErrors'(XL, XX6766, XX6768).
'lo.comp.errors@cntErrors'('lo.core#,..'(X_583, XL), XC, XX6775) :- !,
    'lo.comp.errors@cntErrors'(XL, XC, XX6775).
'lo.comp.errors@cntErrors'(_, _, _) :- raise_exception('error'("cntErrors", 47, 3, 20)).
'lo.comp.errors@countErrors'('lo.comp.errors#report'(XL), XX6779) :- !,
    'lo.comp.errors@cntErrors'(XL, 0, XX6779).
'lo.comp.errors@countErrors'(_, _) :- raise_exception('error'("countErrors", 41, 3, 40)).
'lo.comp.errors@cntWarninngs'('lo.core#[]', XC, XC) :- !.
'lo.comp.errors@cntWarninngs'('lo.core#,..'('lo.comp.errors#sourcedMsg'('lo.comp.errors#warnMsg'(X_584, X_585), X_586), XL), XC, XX6795) :- !,
    ocall('+%3'(XC, 1, XX6793),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.errors@cntWarninngs'(XL, XX6793, XX6795).
'lo.comp.errors@cntWarninngs'('lo.core#,..'('lo.comp.errors#warnMsg'(X_587, X_588), XL), XC, XX6806) :- !,
    ocall('+%3'(XC, 1, XX6804),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.errors@cntWarninngs'(XL, XX6804, XX6806).
'lo.comp.errors@cntWarninngs'('lo.core#,..'(X_589, XL), XC, XX6813) :- !,
    'lo.comp.errors@cntWarninngs'(XL, XC, XX6813).
'lo.comp.errors@cntWarninngs'(_, _, _) :- raise_exception('error'("cntWarninngs", 56, 3, 23)).
'lo.comp.errors@countWarnings'('lo.comp.errors#report'(XL), XX6817) :- !,
    'lo.comp.errors@cntWarninngs'(XL, 0, XX6817).
'lo.comp.errors@countWarnings'(_, _) :- raise_exception('error'("countWarnings", 53, 3, 45)).
'lo.comp.errors@showReport'(XM, 'lo.core#ssSeq'('lo.core#,..'(XX6821, 'lo.core#,..'('lo.core#ss'(" errors, "), 'lo.core#,..'(XX6826, 'lo.core#,..'('lo.core#ss'(" warnings
"), 'lo.core#[]')))))) :- !,
    'lo.comp.errors@countErrors'(XM, XX6820),
    ocall('disp%2'(XX6820, XX6821),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    'lo.comp.errors@countWarnings'(XM, XX6825),
    ocall('disp%2'(XX6825, XX6826),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer').
'lo.comp.errors@showReport'(_, _) :- raise_exception('error'("showReport", 24, 3, 131)).
'lo.core$display$lo.comp.errors*report'('lo.core$display$lo.comp.errors*report%1'('lo.core$display$lo.comp.errors*report')) :- !.
'lo.core$display$lo.comp.errors*report'('disp%2'(XV1254, XV1255), XLbl198, XThis198) :- !,
    'lo.core$display$lo.comp.errors*report@disp'(XV1254, XV1255, XLbl198, XThis198).
'lo.core$display$lo.comp.errors*report'('disp%1'('lo.core$display$lo.comp.errors*report^disp'(XLbl199, XThis199)), XLbl199, XThis199).
'lo.core$display$lo.comp.errors*report@disp'(XR, XX6837, XLbV214, XThV214) :- !,
    'lo.comp.errors@showReport'(XR, XX6837).
'lo.core$display$lo.comp.errors*report@disp'(_, _, _, _) :- raise_exception('error'("disp", 20, 5, 24)).
'lo.comp.errors@hasLocation'('lo.comp.errors#errorMsg'(XLc, X_590), XLc) :- 'lo.comp.errors@neg12'(XLc).
'lo.comp.errors@hasLocation'('lo.comp.errors#warnMsg'(XLc, X_591), XLc) :- 'lo.comp.errors@neg13'(XLc).
'lo.comp.errors@hasLocation'('lo.comp.errors#othMsg'(XLc, X_592), XLc) :- 'lo.comp.errors@neg14'(XLc).
'lo.comp.errors@showMsg'('lo.comp.errors#errorMsg'('lo.comp.location#std', XM), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Error "), 'lo.core#,..'('lo.core#ss'(XM), 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#[]'))))) :- !.
'lo.comp.errors@showMsg'('lo.comp.errors#errorMsg'(XLc, XM), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Error at "), 'lo.core#,..'(XX6873, 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#,..'('lo.core#ss'(XM), 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#[]'))))))) :- !,
    ocall('disp%2'(XLc, XX6873),'lo.core$display$lo.comp.location*location','lo.core$display$lo.comp.location*location').
'lo.comp.errors@showMsg'('lo.comp.errors#warnMsg'('lo.comp.location#std', XM), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Warning "), 'lo.core#,..'('lo.core#ss'(XM), 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#[]'))))) :- !.
'lo.comp.errors@showMsg'('lo.comp.errors#warnMsg'(XLc, XM), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Warning at "), 'lo.core#,..'(XX6903, 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#,..'('lo.core#ss'(XM), 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#[]'))))))) :- !,
    ocall('disp%2'(XLc, XX6903),'lo.core$display$lo.comp.location*location','lo.core$display$lo.comp.location*location').
'lo.comp.errors@showMsg'('lo.comp.errors#othMsg'('lo.comp.location#std', XM), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(XM), 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#[]')))) :- !.
'lo.comp.errors@showMsg'('lo.comp.errors#othMsg'(XLc, XM), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Info at "), 'lo.core#,..'(XX6931, 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#,..'('lo.core#ss'(XM), 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#[]'))))))) :- !,
    ocall('disp%2'(XLc, XX6931),'lo.core$display$lo.comp.location*location','lo.core$display$lo.comp.location*location').
'lo.comp.errors@showMsg'('lo.comp.errors#sourcedMsg'(XM, XLine), 'lo.core#ssSeq'('lo.core#,..'(XX6950, 'lo.core#,..'(XX6953, 'lo.core#[]')))) :- 'lo.comp.errors@hasLocation'(XM, XLc),
    !,
    'lo.comp.errors@showMsg'(XM, XX6950),
    'lo.comp.location@locationContext'(XLc, XLine, XX6953).
'lo.comp.errors@showMsg'('lo.comp.errors#sourcedMsg'(XM, X_593), XX6962) :- !,
    'lo.comp.errors@showMsg'(XM, XX6962).
'lo.comp.errors@showMsg'(_, _) :- raise_exception('error'("showMsg", 31, 3, 64)).
'lo.core$display$lo.comp.errors*reportMsg'('lo.core$display$lo.comp.errors*reportMsg%1'('lo.core$display$lo.comp.errors*reportMsg')) :- !.
'lo.core$display$lo.comp.errors*reportMsg'('disp%2'(XV1264, XV1265), XLbl200, XThis200) :- !,
    'lo.core$display$lo.comp.errors*reportMsg@disp'(XV1264, XV1265, XLbl200, XThis200).
'lo.core$display$lo.comp.errors*reportMsg'('disp%1'('lo.core$display$lo.comp.errors*reportMsg^disp'(XLbl201, XThis201)), XLbl201, XThis201).
'lo.core$display$lo.comp.errors*reportMsg@disp'(XM, XX6965, XLbV215, XThV215) :- !,
    'lo.comp.errors@showMsg'(XM, XX6965).
'lo.core$display$lo.comp.errors*reportMsg@disp'(_, _, _, _) :- raise_exception('error'("disp", 27, 5, 21)).
'lo.comp.errors@errorFree'(XR) :- 'lo.comp.errors@countErrors'(XR, XX6968),
    ocall('==%2'(XX6968, 0),'lo.core$equality$lo.core*integer','lo.core$equality$lo.core*integer').
'lo.comp.errors@reportBase'('lo.comp.errors#report'('lo.core#[]')) :- !.
'lo.comp.errors@reportError'(XMsg, XLc, 'lo.comp.errors#report'(XL), 'lo.comp.errors#report'('lo.core#,..'('lo.comp.errors#errorMsg'(XLc, XMsg), XL))).
'lo.comp.errors@reportWarn'(XMsg, XLc, 'lo.comp.errors#report'(XL), 'lo.comp.errors#report'('lo.core#,..'('lo.comp.errors#warnMsg'(XLc, XMsg), XL))).
'lo.comp.errors@reportMsg'(XMsg, XLc, 'lo.comp.errors#report'(XL), 'lo.comp.errors#report'('lo.core#,..'('lo.comp.errors#othMsg'(XLc, XMsg), XL))).
'lo.comp.errors@findSourceLine'('lo.comp.location#loc'(XLn, X_594, X_595, X_596, XPth), XPth, XLines, XLine) :- ocall('present%3'(XLines, XLn, XLine),'lo.collection$map$lo.array*array','lo.collection$map$lo.array*array').
'lo.comp.errors@sourceMsgs'('lo.core#[]', X_597, X_598, 'lo.core#[]') :- !.
'lo.comp.errors@sourceMsgs'('lo.core#,..'('lo.comp.errors#errorMsg'(XLc, XMsg), XL), XPth, XLines, 'lo.core#,..'('lo.comp.errors#sourcedMsg'('lo.comp.errors#errorMsg'(XLc, XMsg), XLine), XX7038)) :- 'lo.comp.errors@findSourceLine'(XLc, XPth, XLines, XLine),
    !,
    'lo.comp.errors@sourceMsgs'(XL, XPth, XLines, XX7038).
'lo.comp.errors@sourceMsgs'('lo.core#,..'('lo.comp.errors#warnMsg'(XLc, XMsg), XL), XPth, XLines, 'lo.core#,..'('lo.comp.errors#sourcedMsg'('lo.comp.errors#warnMsg'(XLc, XMsg), XLine), XX7059)) :- 'lo.comp.errors@findSourceLine'(XLc, XPth, XLines, XLine),
    !,
    'lo.comp.errors@sourceMsgs'(XL, XPth, XLines, XX7059).
'lo.comp.errors@sourceMsgs'('lo.core#,..'(XM, XL), XPth, XLines, 'lo.core#,..'(XM, XX7070)) :- !,
    'lo.comp.errors@sourceMsgs'(XL, XPth, XLines, XX7070).
'lo.comp.errors@sourceMsgs'(_, _, _, _) :- raise_exception('error'("sourceMsgs", 80, 3, 24)).
'lo.comp.errors@populateContext'('lo.comp.errors#report'(XMsgs), XPth, XLines, 'lo.comp.errors#report'(XX7079)) :- !,
    'lo.comp.errors@sourceMsgs'(XMsgs, XPth, XLines, XX7079).
'lo.comp.errors@populateContext'(_, _, _, _) :- raise_exception('error'("populateContext", 77, 3, 77)).
'lo.comp.errors@fullReport'('lo.comp.errors#report'(XMsgs), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ssSeq'(XX7086), 'lo.core#,..'(XX7091, 'lo.core#[]')))) :- !,
    'lo.list@reverse'(XMsgs, XX7084),
    ocall('//%3'(XX7084, 'lo.comp.errors^showMsg', XX7086),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    'lo.comp.errors@showReport'('lo.comp.errors#report'(XMsgs), XX7091).
'lo.comp.errors@fullReport'(_, _) :- raise_exception('error'("fullReport", 91, 3, 91)).
'lo.comp.errors@msg2Json'('lo.comp.errors#sourcedMsg'(XM, X_599), XX7100) :- !,
    'lo.comp.errors@msg2Json'(XM, XX7100).
'lo.comp.errors@msg2Json'('lo.comp.errors#errorMsg'(XLc, XMsg), 'lo.json#jColl'(XX7118)) :- !,
    ocall('_empty%1'(XXV15),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_put%4'(XXV15, "msg", 'lo.json#jTxt'(XMsg), XX7108),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_put%4'(XX7108, "severity", 'lo.json#jTxt'("error"), XX7112),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_coerce%2'(XLc, XX7116),'lo.coerce$coercion$lo.comp.location*location$lo.json*json','lo.coerce$coercion$lo.comp.location*location$lo.json*json'),
    ocall('_put%4'(XX7112, "location", XX7116, XX7118),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.errors@msg2Json'('lo.comp.errors#warnMsg'(XLc, XMsg), 'lo.json#jColl'(XX7139)) :- !,
    ocall('_empty%1'(XXV16),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_put%4'(XXV16, "msg", 'lo.json#jTxt'(XMsg), XX7129),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_put%4'(XX7129, "severity", 'lo.json#jTxt'("warning"), XX7133),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_coerce%2'(XLc, XX7137),'lo.coerce$coercion$lo.comp.location*location$lo.json*json','lo.coerce$coercion$lo.comp.location*location$lo.json*json'),
    ocall('_put%4'(XX7133, "location", XX7137, XX7139),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.errors@msg2Json'('lo.comp.errors#othMsg'(XLc, XMsg), 'lo.json#jColl'(XX7160)) :- !,
    ocall('_empty%1'(XXV17),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_put%4'(XXV17, "msg", 'lo.json#jTxt'(XMsg), XX7150),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_put%4'(XX7150, "severity", 'lo.json#jTxt'("msg"), XX7154),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_coerce%2'(XLc, XX7158),'lo.coerce$coercion$lo.comp.location*location$lo.json*json','lo.coerce$coercion$lo.comp.location*location$lo.json*json'),
    ocall('_put%4'(XX7154, "location", XX7158, XX7160),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.comp.errors@msg2Json'(_, _) :- raise_exception('error'("msg2Json", 103, 3, 40)).
'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json'('lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json%1'('lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json')) :- !.
'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json'('_coerce%2'(XV1299, XV1300), XLbl202, XThis202) :- !,
    'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json@_coerce'(XV1299, XV1300, XLbl202, XThis202).
'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json'('_coerce%1'('lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json^_coerce'(XLbl203, XThis203)), XLbl203, XThis203).
'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json@_coerce'(XM, XX7166, XLbV216, XThV216) :- !,
    'lo.comp.errors@msg2Json'(XM, XX7166).
'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json@_coerce'(_, _, _, _) :- raise_exception('error'("_coerce", 99, 5, 25)).
'lo.comp.errors^cntErrors'('_call%3'(XV1240, XV1241, XV1242), 'lo.comp.errors^cntErrors', _) :- 'lo.comp.errors@cntErrors'(XV1240, XV1241, XV1242).
'lo.comp.errors^countErrors'('_call%2'(XV1243, XV1244), 'lo.comp.errors^countErrors', _) :- 'lo.comp.errors@countErrors'(XV1243, XV1244).
'lo.comp.errors^cntWarninngs'('_call%3'(XV1245, XV1246, XV1247), 'lo.comp.errors^cntWarninngs', _) :- 'lo.comp.errors@cntWarninngs'(XV1245, XV1246, XV1247).
'lo.comp.errors^countWarnings'('_call%2'(XV1248, XV1249), 'lo.comp.errors^countWarnings', _) :- 'lo.comp.errors@countWarnings'(XV1248, XV1249).
'lo.comp.errors^showReport'('_call%2'(XV1250, XV1251), 'lo.comp.errors^showReport', _) :- 'lo.comp.errors@showReport'(XV1250, XV1251).
'lo.core$display$lo.comp.errors*report^disp'('_call%2'(XV1252, XV1253), 'lo.core$display$lo.comp.errors*report^disp'(XLbV214, XThV214), _) :- 'lo.core$display$lo.comp.errors*report@disp'(XV1252, XV1253, XLbV214, XThV214).
'lo.core$display$lo.comp.errors*report^disp'('_call%2'(XV1256, XV1257), 'lo.core$display$lo.comp.errors*report^disp'(XLbV214, XThV214), _) :- 'lo.core$display$lo.comp.errors*report@disp'(XV1256, XV1257, XLbV214, XThV214).
'lo.comp.errors@neg12'(XLc) :- XLc = 'lo.comp.location#std',
    !,
    fail.
'lo.comp.errors@neg12'(XLc).
'lo.comp.errors@neg13'(XLc) :- XLc = 'lo.comp.location#std',
    !,
    fail.
'lo.comp.errors@neg13'(XLc).
'lo.comp.errors@neg14'(XLc) :- XLc = 'lo.comp.location#std',
    !,
    fail.
'lo.comp.errors@neg14'(XLc).
'lo.comp.errors^hasLocation'('_call%2'(XV1258, XV1259), 'lo.comp.errors^hasLocation', _) :- 'lo.comp.errors@hasLocation'(XV1258, XV1259).
'lo.comp.errors^showMsg'('_call%2'(XV1260, XV1261), 'lo.comp.errors^showMsg', _) :- 'lo.comp.errors@showMsg'(XV1260, XV1261).
'lo.core$display$lo.comp.errors*reportMsg^disp'('_call%2'(XV1262, XV1263), 'lo.core$display$lo.comp.errors*reportMsg^disp'(XLbV215, XThV215), _) :- 'lo.core$display$lo.comp.errors*reportMsg@disp'(XV1262, XV1263, XLbV215, XThV215).
'lo.core$display$lo.comp.errors*reportMsg^disp'('_call%2'(XV1266, XV1267), 'lo.core$display$lo.comp.errors*reportMsg^disp'(XLbV215, XThV215), _) :- 'lo.core$display$lo.comp.errors*reportMsg@disp'(XV1266, XV1267, XLbV215, XThV215).
'lo.comp.errors^errorFree'('_call%1'(XV1268), 'lo.comp.errors^errorFree', _) :- 'lo.comp.errors@errorFree'(XV1268).
'lo.comp.errors^reportError'('_call%4'(XV1269, XV1270, XV1271, XV1272), 'lo.comp.errors^reportError', _) :- 'lo.comp.errors@reportError'(XV1269, XV1270, XV1271, XV1272).
'lo.comp.errors^reportWarn'('_call%4'(XV1273, XV1274, XV1275, XV1276), 'lo.comp.errors^reportWarn', _) :- 'lo.comp.errors@reportWarn'(XV1273, XV1274, XV1275, XV1276).
'lo.comp.errors^reportMsg'('_call%4'(XV1277, XV1278, XV1279, XV1280), 'lo.comp.errors^reportMsg', _) :- 'lo.comp.errors@reportMsg'(XV1277, XV1278, XV1279, XV1280).
'lo.comp.errors^findSourceLine'('_call%4'(XV1281, XV1282, XV1283, XV1284), 'lo.comp.errors^findSourceLine', _) :- 'lo.comp.errors@findSourceLine'(XV1281, XV1282, XV1283, XV1284).
'lo.comp.errors^sourceMsgs'('_call%4'(XV1285, XV1286, XV1287, XV1288), 'lo.comp.errors^sourceMsgs', _) :- 'lo.comp.errors@sourceMsgs'(XV1285, XV1286, XV1287, XV1288).
'lo.comp.errors^populateContext'('_call%4'(XV1289, XV1290, XV1291, XV1292), 'lo.comp.errors^populateContext', _) :- 'lo.comp.errors@populateContext'(XV1289, XV1290, XV1291, XV1292).
'lo.comp.errors^fullReport'('_call%2'(XV1293, XV1294), 'lo.comp.errors^fullReport', _) :- 'lo.comp.errors@fullReport'(XV1293, XV1294).
'lo.comp.errors^msg2Json'('_call%2'(XV1295, XV1296), 'lo.comp.errors^msg2Json', _) :- 'lo.comp.errors@msg2Json'(XV1295, XV1296).
'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json^_coerce'('_call%2'(XV1297, XV1298), 'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json^_coerce'(XLbV216, XThV216), _) :- 'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json@_coerce'(XV1297, XV1298, XLbV216, XThV216).
'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json^_coerce'('_call%2'(XV1301, XV1302), 'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json^_coerce'(XLbV216, XThV216), _) :- 'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json@_coerce'(XV1301, XV1302, XLbV216, XThV216).
