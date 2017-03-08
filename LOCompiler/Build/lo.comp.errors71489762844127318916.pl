'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.errors's'0.0.1'n4o4'()4'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.array'e'*'n2o2'import'e'private'n2o2'pkg's'lo.json'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'s\"I13'sourcedMsg'CT2t'lo.comp.errors*reportMsg'St'lo.comp.errors*reportMsg''errorMsg'CT2t'lo.comp.location*location'St'lo.comp.errors*reportMsg''warnMsg'CT2t'lo.comp.location*location'St'lo.comp.errors*reportMsg''othMsg'CT2t'lo.comp.location*location'St'lo.comp.errors*reportMsg''countWarnings'FT1t'lo.comp.errors*report'i'countErrors'FT1t'lo.comp.errors*report'i'errorFree'PT1t'lo.comp.errors*report''reportBase't'lo.comp.errors*report''reportError'PT4St'lo.comp.location*location't'lo.comp.errors*report't'lo.comp.errors*report''reportWarn'PT4St'lo.comp.location*location't'lo.comp.errors*report't'lo.comp.errors*report''reportMsg'PT4St'lo.comp.location*location't'lo.comp.errors*report't'lo.comp.errors*report''populateContext'FT3t'lo.comp.errors*report'SUz1'lo.array*array'1St'lo.comp.errors*report''fullReport'FT1t'lo.comp.errors*report't'lo.core*ss'\"s\"I2'report'Yt'lo.comp.errors*report'I1'msgs'Lt'lo.comp.errors*reportMsg''reportMsg'Yt'lo.comp.errors*reportMsg'I0\"n4o4'()4's'sourcedMsg's'errorMsg's'warnMsg's'othMsg'n0o0'()0'n2o2'()2'n2o2'()2's'lo.core$display$lo.comp.errors*report's\"c'lo.core$display'T1t'lo.comp.errors*report'T0\"n2o2'()2's'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json's\"c'lo.coerce$coercion'T2t'lo.comp.errors*reportMsg't'lo.json*json'T0\"").
'lo.comp.errors@init'():- !.
'lo.comp.errors#sourcedMsg'('sourcedMsg%1'('lo.comp.errors@sourcedMsg'())):- !.
'lo.comp.errors#errorMsg'('errorMsg%1'('lo.comp.errors@errorMsg'())):- !.
'lo.comp.errors#warnMsg'('warnMsg%1'('lo.comp.errors@warnMsg'())):- !.
'lo.comp.errors#othMsg'('othMsg%1'('lo.comp.errors@othMsg'())):- !.
'lo.comp.errors#report'('report%1'('lo.comp.errors@report'())):- !.
'lo.comp.errors#report'('msgs%1'(XV28761), XLbl2064, XThis2064):- !,
    'lo.comp.errors#report@msgs'(XV28761, XLbl2064, XThis2064).
'lo.comp.errors#report@msgs'(XLst, XLbV2236, XThV2236):- XLbV2236 = 'lo.comp.errors#report'(XLst),
    !.
'lo.comp.errors@cntWarninngs'('lo.core#[]', XC, XC):- !.
'lo.comp.errors@cntWarninngs'('lo.core#,..'('lo.comp.errors#sourcedMsg'('lo.comp.errors#warnMsg'(X_31008, X_31009), X_31010), XL), XC, XXd35842):- !,
    ocall('+%1'(XXV4755),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XC, 1, XXe4416),XXV4755,XXV4755),
    'lo.comp.errors@cntWarninngs'(XL, XXe4416, XXd35842).
'lo.comp.errors@cntWarninngs'('lo.core#,..'('lo.comp.errors#warnMsg'(X_31012, X_31013), XL), XC, XXd35843):- !,
    ocall('+%1'(XXV4756),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XC, 1, XXe4417),XXV4756,XXV4756),
    'lo.comp.errors@cntWarninngs'(XL, XXe4417, XXd35843).
'lo.comp.errors@cntWarninngs'('lo.core#,..'(X_31015, XL), XC, XXd35844):- !,
    'lo.comp.errors@cntWarninngs'(XL, XC, XXd35844).
'lo.comp.errors@cntWarninngs'(_, _, _):- raise_exception('error'("lo.comp.errors@cntWarninngs", 56, 3, 23)).
'lo.comp.errors@countWarnings'('lo.comp.errors#report'(XL), XXd35845):- !,
    'lo.comp.errors@cntWarninngs'(XL, 0, XXd35845).
'lo.comp.errors@countWarnings'(_, _):- raise_exception('error'("lo.comp.errors@countWarnings", 53, 3, 45)).
'lo.comp.errors@cntErrors'('lo.core#[]', XC, XC):- !.
'lo.comp.errors@cntErrors'('lo.core#,..'('lo.comp.errors#sourcedMsg'('lo.comp.errors#errorMsg'(X_31017, X_31018), X_31019), XL), XC, XXd35846):- !,
    ocall('+%1'(XXV4757),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XC, 1, XXe4418),XXV4757,XXV4757),
    'lo.comp.errors@cntErrors'(XL, XXe4418, XXd35846).
'lo.comp.errors@cntErrors'('lo.core#,..'('lo.comp.errors#errorMsg'(X_31021, X_31022), XL), XC, XXd35847):- !,
    ocall('+%1'(XXV4758),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_call%3'(XC, 1, XXe4419),XXV4758,XXV4758),
    'lo.comp.errors@cntErrors'(XL, XXe4419, XXd35847).
'lo.comp.errors@cntErrors'('lo.core#,..'(X_31024, XL), XC, XXd35848):- !,
    'lo.comp.errors@cntErrors'(XL, XC, XXd35848).
'lo.comp.errors@cntErrors'(_, _, _):- raise_exception('error'("lo.comp.errors@cntErrors", 47, 3, 20)).
'lo.comp.errors@countErrors'('lo.comp.errors#report'(XL), XXd35849):- !,
    'lo.comp.errors@cntErrors'(XL, 0, XXd35849).
'lo.comp.errors@countErrors'(_, _):- raise_exception('error'("lo.comp.errors@countErrors", 41, 3, 40)).
'lo.comp.errors@showReport'(XM, 'lo.core#ssSeq'('lo.core#,..'(XXe4420, 'lo.core#,..'('lo.core#ss'(" errors, "), 'lo.core#,..'(XXe4421, 'lo.core#,..'('lo.core#ss'(" warnings
"), 'lo.core#[]')))))):- !,
    ocall('disp%1'(XXV4759),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    ocall('disp%1'(XXV4760),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    'lo.comp.errors@countErrors'(XM, XXd35850),
    ocall('_call%2'(XXd35850, XXe4420),XXV4759,XXV4759),
    'lo.comp.errors@countWarnings'(XM, XXd35852),
    ocall('_call%2'(XXd35852, XXe4421),XXV4760,XXV4760).
'lo.comp.errors@showReport'(_, _):- raise_exception('error'("lo.comp.errors@showReport", 24, 3, 131)).
'lo.core$display$lo.comp.errors*report'('lo.core$display$lo.comp.errors*report%1'('lo.core$display$lo.comp.errors*report')):- !.
'lo.core$display$lo.comp.errors*report'('disp%2'(XV28776, XV28777), XLbl2065, XThis2065):- !,
    'lo.core$display$lo.comp.errors*report@disp'(XV28776, XV28777, XLbl2065, XThis2065).
'lo.core$display$lo.comp.errors*report'('disp%1'('lo.core$display$lo.comp.errors*report^disp'(XLbl2066, XThis2066)), XLbl2066, XThis2066).
'lo.core$display$lo.comp.errors*report@disp'(XR, XXd35859, XLbV2237, XThV2237):- !,
    'lo.comp.errors@showReport'(XR, XXd35859).
'lo.core$display$lo.comp.errors*report@disp'(_, _):- raise_exception('error'("lo.core$display$lo.comp.errors*report@disp", 20, 5, 24)).
'lo.comp.errors@hasLocation'('lo.comp.errors#errorMsg'(XLc, X_31029), XLc):- 'lo.comp.errors@neg292'(XLc).
'lo.comp.errors@hasLocation'('lo.comp.errors#warnMsg'(XLc, X_31030), XLc):- 'lo.comp.errors@neg293'(XLc).
'lo.comp.errors@hasLocation'('lo.comp.errors#othMsg'(XLc, X_31031), XLc):- 'lo.comp.errors@neg294'(XLc).
'lo.comp.errors@showMsg'('lo.comp.errors#errorMsg'('lo.comp.location#std', XM), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Error "), 'lo.core#,..'('lo.core#ss'(XM), 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#[]'))))):- !.
'lo.comp.errors@showMsg'('lo.comp.errors#errorMsg'(XLc, XM), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Error at "), 'lo.core#,..'(XXe4422, 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#,..'('lo.core#ss'(XM), 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#[]'))))))):- !,
    ocall('disp%1'(XXV4761),'lo.core$display$lo.comp.location*location','lo.core$display$lo.comp.location*location'),
    ocall('_call%2'(XLc, XXe4422),XXV4761,XXV4761).
'lo.comp.errors@showMsg'('lo.comp.errors#warnMsg'('lo.comp.location#std', XM), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Warning "), 'lo.core#,..'('lo.core#ss'(XM), 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#[]'))))):- !.
'lo.comp.errors@showMsg'('lo.comp.errors#warnMsg'(XLc, XM), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Warning at "), 'lo.core#,..'(XXe4423, 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#,..'('lo.core#ss'(XM), 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#[]'))))))):- !,
    ocall('disp%1'(XXV4762),'lo.core$display$lo.comp.location*location','lo.core$display$lo.comp.location*location'),
    ocall('_call%2'(XLc, XXe4423),XXV4762,XXV4762).
'lo.comp.errors@showMsg'('lo.comp.errors#othMsg'('lo.comp.location#std', XM), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(XM), 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#[]')))):- !.
'lo.comp.errors@showMsg'('lo.comp.errors#othMsg'(XLc, XM), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Info at "), 'lo.core#,..'(XXe4424, 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#,..'('lo.core#ss'(XM), 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#[]'))))))):- !,
    ocall('disp%1'(XXV4763),'lo.core$display$lo.comp.location*location','lo.core$display$lo.comp.location*location'),
    ocall('_call%2'(XLc, XXe4424),XXV4763,XXV4763).
'lo.comp.errors@showMsg'('lo.comp.errors#sourcedMsg'(XM, XLine), 'lo.core#ssSeq'('lo.core#,..'(XXd35909, 'lo.core#,..'(XXd35910, 'lo.core#[]')))):- 'lo.comp.errors@hasLocation'(XM, XLc),
    !,
    'lo.comp.errors@showMsg'(XM, XXd35909),
    'lo.comp.location@locationContext'(XLc, XLine, XXd35910).
'lo.comp.errors@showMsg'('lo.comp.errors#sourcedMsg'(XM, X_31057), XXd35914):- !,
    'lo.comp.errors@showMsg'(XM, XXd35914).
'lo.comp.errors@showMsg'(_, _):- raise_exception('error'("lo.comp.errors@showMsg", 31, 3, 64)).
'lo.core$display$lo.comp.errors*reportMsg'('lo.core$display$lo.comp.errors*reportMsg%1'('lo.core$display$lo.comp.errors*reportMsg')):- !.
'lo.core$display$lo.comp.errors*reportMsg'('disp%2'(XV28784, XV28785), XLbl2067, XThis2067):- !,
    'lo.core$display$lo.comp.errors*reportMsg@disp'(XV28784, XV28785, XLbl2067, XThis2067).
'lo.core$display$lo.comp.errors*reportMsg'('disp%1'('lo.core$display$lo.comp.errors*reportMsg^disp'(XLbl2068, XThis2068)), XLbl2068, XThis2068).
'lo.core$display$lo.comp.errors*reportMsg@disp'(XM, XXd35915, XLbV2238, XThV2238):- !,
    'lo.comp.errors@showMsg'(XM, XXd35915).
'lo.core$display$lo.comp.errors*reportMsg@disp'(_, _):- raise_exception('error'("lo.core$display$lo.comp.errors*reportMsg@disp", 27, 5, 21)).
'lo.comp.errors@errorFree'(XR):- 'lo.comp.errors@countErrors'(XR, XXd35916),
    ocall('==%2'(XXd35916, 0),'lo.core$equality$lo.core*integer','lo.core$equality$lo.core*integer').
'lo.comp.errors@reportBase'('lo.comp.errors#report'('lo.core#[]')):- !.
'lo.comp.errors@reportError'(XMsg, XLc, 'lo.comp.errors#report'(XL), 'lo.comp.errors#report'('lo.core#,..'('lo.comp.errors#errorMsg'(XLc, XMsg), XL))).
'lo.comp.errors@reportWarn'(XMsg, XLc, 'lo.comp.errors#report'(XL), 'lo.comp.errors#report'('lo.core#,..'('lo.comp.errors#warnMsg'(XLc, XMsg), XL))).
'lo.comp.errors@reportMsg'(XMsg, XLc, 'lo.comp.errors#report'(XL), 'lo.comp.errors#report'('lo.core#,..'('lo.comp.errors#othMsg'(XLc, XMsg), XL))).
'lo.comp.errors@findSourceLine'('lo.comp.location#loc'(XLn, X_31061, X_31062, X_31063, XPth), XPth, XLines, XLine):- ocall('present%3'(XLines, XLn, XLine),'lo.collection$map$lo.array*array','lo.collection$map$lo.array*array').
'lo.comp.errors@sourceMsgs'('lo.core#[]', X_31064, X_31065, 'lo.core#[]'):- !.
'lo.comp.errors@sourceMsgs'('lo.core#,..'('lo.comp.errors#errorMsg'(XLc, XMsg), XL), XPth, XLines, 'lo.core#,..'('lo.comp.errors#sourcedMsg'('lo.comp.errors#errorMsg'(XLc, XMsg), XLine), XXd35920)):- 'lo.comp.errors@findSourceLine'(XLc, XPth, XLines, XLine),
    !,
    'lo.comp.errors@sourceMsgs'(XL, XPth, XLines, XXd35920).
'lo.comp.errors@sourceMsgs'('lo.core#,..'('lo.comp.errors#warnMsg'(XLc, XMsg), XL), XPth, XLines, 'lo.core#,..'('lo.comp.errors#sourcedMsg'('lo.comp.errors#warnMsg'(XLc, XMsg), XLine), XXd35924)):- 'lo.comp.errors@findSourceLine'(XLc, XPth, XLines, XLine),
    !,
    'lo.comp.errors@sourceMsgs'(XL, XPth, XLines, XXd35924).
'lo.comp.errors@sourceMsgs'('lo.core#,..'(XM, XL), XPth, XLines, 'lo.core#,..'(XM, XXd35926)):- !,
    'lo.comp.errors@sourceMsgs'(XL, XPth, XLines, XXd35926).
'lo.comp.errors@sourceMsgs'(_, _, _, _):- raise_exception('error'("lo.comp.errors@sourceMsgs", 80, 3, 24)).
'lo.comp.errors@populateContext'('lo.comp.errors#report'(XMsgs), XPth, XLines, 'lo.comp.errors#report'(XXd35928)):- !,
    'lo.comp.errors@sourceMsgs'(XMsgs, XPth, XLines, XXd35928).
'lo.comp.errors@populateContext'(_, _, _, _):- raise_exception('error'("lo.comp.errors@populateContext", 77, 3, 77)).
'lo.comp.errors@fullReport'('lo.comp.errors#report'(XMsgs), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ssSeq'(XXe4425), 'lo.core#,..'(XXd35933, 'lo.core#[]')))):- !,
    ocall('//%1'(XXV4764),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    'lo.list@reverse'(XMsgs, XXd35930),
    ocall('_call%3'(XXd35930, 'lo.comp.errors^showMsg', XXe4425),XXV4764,XXV4764),
    'lo.comp.errors@showReport'('lo.comp.errors#report'(XMsgs), XXd35933).
'lo.comp.errors@fullReport'(_, _):- raise_exception('error'("lo.comp.errors@fullReport", 91, 3, 91)).
'lo.comp.errors@msg2Json'('lo.comp.errors#sourcedMsg'(XM, X_31074), XXd35937):- !,
    'lo.comp.errors@msg2Json'(XM, XXd35937).
'lo.comp.errors@msg2Json'('lo.comp.errors#errorMsg'(XLc, XMsg), 'lo.json#jColl'(XXe4429)):- !,
    ocall('_put%1'(XXV4766),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_put%1'(XXV4767),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_coerce%1'(XXV4768),'lo.coerce$coercion$lo.comp.location*location$lo.json*json','lo.coerce$coercion$lo.comp.location*location$lo.json*json'),
    ocall('_put%1'(XXV4769),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_empty%1'(XXV4765),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XXV4765, "msg", 'lo.json#jTxt'(XMsg), XXe4426),XXV4766,XXV4766),
    ocall('_call%4'(XXe4426, "severity", 'lo.json#jTxt'("error"), XXe4427),XXV4767,XXV4767),
    ocall('_call%2'(XLc, XXe4428),XXV4768,XXV4768),
    ocall('_call%4'(XXe4427, "location", XXe4428, XXe4429),XXV4769,XXV4769).
'lo.comp.errors@msg2Json'('lo.comp.errors#warnMsg'(XLc, XMsg), 'lo.json#jColl'(XXe4433)):- !,
    ocall('_put%1'(XXV4771),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_put%1'(XXV4772),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_coerce%1'(XXV4773),'lo.coerce$coercion$lo.comp.location*location$lo.json*json','lo.coerce$coercion$lo.comp.location*location$lo.json*json'),
    ocall('_put%1'(XXV4774),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_empty%1'(XXV4770),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XXV4770, "msg", 'lo.json#jTxt'(XMsg), XXe4430),XXV4771,XXV4771),
    ocall('_call%4'(XXe4430, "severity", 'lo.json#jTxt'("warning"), XXe4431),XXV4772,XXV4772),
    ocall('_call%2'(XLc, XXe4432),XXV4773,XXV4773),
    ocall('_call%4'(XXe4431, "location", XXe4432, XXe4433),XXV4774,XXV4774).
'lo.comp.errors@msg2Json'('lo.comp.errors#othMsg'(XLc, XMsg), 'lo.json#jColl'(XXe4437)):- !,
    ocall('_put%1'(XXV4776),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_put%1'(XXV4777),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_coerce%1'(XXV4778),'lo.coerce$coercion$lo.comp.location*location$lo.json*json','lo.coerce$coercion$lo.comp.location*location$lo.json*json'),
    ocall('_put%1'(XXV4779),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_empty%1'(XXV4775),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XXV4775, "msg", 'lo.json#jTxt'(XMsg), XXe4434),XXV4776,XXV4776),
    ocall('_call%4'(XXe4434, "severity", 'lo.json#jTxt'("msg"), XXe4435),XXV4777,XXV4777),
    ocall('_call%2'(XLc, XXe4436),XXV4778,XXV4778),
    ocall('_call%4'(XXe4435, "location", XXe4436, XXe4437),XXV4779,XXV4779).
'lo.comp.errors@msg2Json'(_, _):- raise_exception('error'("lo.comp.errors@msg2Json", 103, 3, 40)).
'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json'('lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json%1'('lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json')):- !.
'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json'('_coerce%2'(XV28817, XV28818), XLbl2069, XThis2069):- !,
    'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json@_coerce'(XV28817, XV28818, XLbl2069, XThis2069).
'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json'('_coerce%1'('lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json^_coerce'(XLbl2070, XThis2070)), XLbl2070, XThis2070).
'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json@_coerce'(XM, XXd35959, XLbV2239, XThV2239):- !,
    'lo.comp.errors@msg2Json'(XM, XXd35959).
'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json@_coerce", 99, 5, 25)).
'lo.comp.errors^cntWarninngs'('_call%3'(XV28762, XV28763, XV28764), 'lo.comp.errors^cntWarninngs', _):- 'lo.comp.errors@cntWarninngs'(XV28762, XV28763, XV28764).
'lo.comp.errors^countWarnings'('_call%2'(XV28765, XV28766), 'lo.comp.errors^countWarnings', _):- 'lo.comp.errors@countWarnings'(XV28765, XV28766).
'lo.comp.errors^cntErrors'('_call%3'(XV28767, XV28768, XV28769), 'lo.comp.errors^cntErrors', _):- 'lo.comp.errors@cntErrors'(XV28767, XV28768, XV28769).
'lo.comp.errors^countErrors'('_call%2'(XV28770, XV28771), 'lo.comp.errors^countErrors', _):- 'lo.comp.errors@countErrors'(XV28770, XV28771).
'lo.comp.errors^showReport'('_call%2'(XV28772, XV28773), 'lo.comp.errors^showReport', _):- 'lo.comp.errors@showReport'(XV28772, XV28773).
'lo.core$display$lo.comp.errors*report^disp'('_call%2'(XV28774, XV28775), 'lo.core$display$lo.comp.errors*report^disp'(XLbV2237, XThV2237), _):- 'lo.core$display$lo.comp.errors*report@disp'(XV28774, XV28775, XLbV2237, XThV2237).
'lo.comp.errors@neg292'(XLc):- XLc = 'lo.comp.location#std',
    !,
    fail.
'lo.comp.errors@neg292'(XLc).
'lo.comp.errors@neg293'(XLc):- XLc = 'lo.comp.location#std',
    !,
    fail.
'lo.comp.errors@neg293'(XLc).
'lo.comp.errors@neg294'(XLc):- XLc = 'lo.comp.location#std',
    !,
    fail.
'lo.comp.errors@neg294'(XLc).
'lo.comp.errors^hasLocation'('_call%2'(XV28778, XV28779), 'lo.comp.errors^hasLocation', _):- 'lo.comp.errors@hasLocation'(XV28778, XV28779).
'lo.comp.errors^showMsg'('_call%2'(XV28780, XV28781), 'lo.comp.errors^showMsg', _):- 'lo.comp.errors@showMsg'(XV28780, XV28781).
'lo.core$display$lo.comp.errors*reportMsg^disp'('_call%2'(XV28782, XV28783), 'lo.core$display$lo.comp.errors*reportMsg^disp'(XLbV2238, XThV2238), _):- 'lo.core$display$lo.comp.errors*reportMsg@disp'(XV28782, XV28783, XLbV2238, XThV2238).
'lo.comp.errors^errorFree'('_call%1'(XV28786), 'lo.comp.errors^errorFree', _):- 'lo.comp.errors@errorFree'(XV28786).
'lo.comp.errors^reportError'('_call%4'(XV28787, XV28788, XV28789, XV28790), 'lo.comp.errors^reportError', _):- 'lo.comp.errors@reportError'(XV28787, XV28788, XV28789, XV28790).
'lo.comp.errors^reportWarn'('_call%4'(XV28791, XV28792, XV28793, XV28794), 'lo.comp.errors^reportWarn', _):- 'lo.comp.errors@reportWarn'(XV28791, XV28792, XV28793, XV28794).
'lo.comp.errors^reportMsg'('_call%4'(XV28795, XV28796, XV28797, XV28798), 'lo.comp.errors^reportMsg', _):- 'lo.comp.errors@reportMsg'(XV28795, XV28796, XV28797, XV28798).
'lo.comp.errors^findSourceLine'('_call%4'(XV28799, XV28800, XV28801, XV28802), 'lo.comp.errors^findSourceLine', _):- 'lo.comp.errors@findSourceLine'(XV28799, XV28800, XV28801, XV28802).
'lo.comp.errors^sourceMsgs'('_call%4'(XV28803, XV28804, XV28805, XV28806), 'lo.comp.errors^sourceMsgs', _):- 'lo.comp.errors@sourceMsgs'(XV28803, XV28804, XV28805, XV28806).
'lo.comp.errors^populateContext'('_call%4'(XV28807, XV28808, XV28809, XV28810), 'lo.comp.errors^populateContext', _):- 'lo.comp.errors@populateContext'(XV28807, XV28808, XV28809, XV28810).
'lo.comp.errors^fullReport'('_call%2'(XV28811, XV28812), 'lo.comp.errors^fullReport', _):- 'lo.comp.errors@fullReport'(XV28811, XV28812).
'lo.comp.errors^msg2Json'('_call%2'(XV28813, XV28814), 'lo.comp.errors^msg2Json', _):- 'lo.comp.errors@msg2Json'(XV28813, XV28814).
'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json^_coerce'('_call%2'(XV28815, XV28816), 'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json^_coerce'(XLbV2239, XThV2239), _):- 'lo.coerce$coercion$lo.comp.errors*reportMsg$lo.json*json@_coerce'(XV28815, XV28816, XLbV2239, XThV2239).
