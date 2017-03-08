'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.debug'e'*'n15o15'()15'n2o2'import'e'private'n2o2'pkg's'lo.comp.misc'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.transutils'e'*'n2o2'import'e'public'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I5'debugPreamble'PT7SLt'lo.comp.term*term'Lt'lo.comp.term*term'Lt'lo.comp.term*pred'Lt'lo.comp.term*pred't'lo.comp.transutils*trOptions't'lo.comp.transutils*trOptions''frameDebug'PT6SiLt'lo.comp.term*term'Lt'lo.comp.term*pred'Lt'lo.comp.term*pred't'lo.comp.transutils*trOptions''deframeDebug'PT5SiLt'lo.comp.term*pred'Lt'lo.comp.term*pred't'lo.comp.transutils*trOptions''breakDebug'PT4SLt'lo.comp.term*pred'Lt'lo.comp.term*pred't'lo.comp.transutils*trOptions''lineDebug'PT4t'lo.comp.location*location'Lt'lo.comp.term*pred'Lt'lo.comp.term*pred't'lo.comp.transutils*trOptions'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.debug@init'() :- !.
'lo.comp.debug@debugPreamble'(XNm, XQ, 'lo.core#,..'(XDgVr, 'lo.core#,..'(XFrVr, XQ)), 'lo.core#,..'('lo.comp.term#call'('lo.core#none', 'lo.comp.term#prg'("go.debug@current", 1), 'lo.core#,..'(XDgVr, 'lo.core#[]')), XGx), XGx, XOpts, XX14185) :- 'lo.comp.transutils@isOption'('lo.comp.transutils#debugging', XOpts),
    'lo.comp.term@genVr'("__D", XX14189),
    XDgVr = XX14189,
    'lo.comp.term@genVr'("__F", XX14191),
    XFrVr = XX14191,
    'lo.comp.transutils@pushOpt'('lo.comp.transutils#dbgVars'(XNm, XDgVr, XFrVr), XOpts, XX14185).
'lo.comp.debug@debugPreamble'(X_1115, XQ, XQ, XP, XP, XOpts, XOpts).
'lo.comp.debug@constructFrameList'('lo.core#[]', 'lo.comp.term#enum'("lo.list#[]")) :- !.
'lo.comp.debug@constructFrameList'('lo.core#,..'('lo.comp.term#varbl'(XV), XVars), XX14207) :- 'lo.comp.misc@starts_with'(XV, "_"),
    !,
    'lo.comp.debug@constructFrameList'(XVars, XX14207).
'lo.comp.debug@constructFrameList'('lo.core#,..'('lo.comp.term#varbl'(XV), XVars), 'lo.comp.term#cons'('lo.comp.term#strct'("lo.list#,..", 2), 'lo.core#,..'(XX14220, 'lo.core#,..'(XX14222, 'lo.core#[]')))) :- !,
    'lo.comp.term@mkTpl'('lo.core#,..'('lo.comp.term#strng'(XV), 'lo.core#,..'('lo.comp.term#varbl'(XV), 'lo.core#[]')), XX14220),
    'lo.comp.debug@constructFrameList'(XVars, XX14222).
'lo.comp.debug@constructFrameList'(_, _) :- raise_exception('error'("constructFrameList", 31, 3, 43)).
'lo.comp.debug@frameDebug'(XNm, XQNo, XQ, XG, XGx, XOpts) :- 'lo.comp.transutils@isOption'('lo.comp.transutils#dbgVars'(XNm, XDgVr, XFrVr), XOpts),
    'lo.comp.debug@constructFrameList'(XQ, XX14242),
    'lo.comp.transutils@trCons'("frame", 3, XX14245),
    XG = 'lo.core#,..'('lo.comp.term#unfy'('lo.core#none', XFrVr, XX14242), 'lo.core#,..'('lo.comp.term#ocall'('lo.core#none', 'lo.comp.term#cons'(XX14245, 'lo.core#,..'('lo.comp.term#strng'(XNm), 'lo.core#,..'('lo.comp.term#intgr'(XQNo), 'lo.core#,..'(XFrVr, 'lo.core#[]')))), XDgVr, XDgVr), XGx)).
'lo.comp.debug@frameDebug'(X_1116, X_1117, X_1118, XF, XF, X_1119).
'lo.comp.debug@deframeDebug'(XNm, XQNo, XFB, XFBx, XOpts) :- 'lo.comp.transutils@isOption'('lo.comp.transutils#dbgVars'(XNm, XDgVr, XFrVr), XOpts),
    'lo.comp.transutils@trCons'("deframe", 3, XX14280),
    XFB = 'lo.core#,..'('lo.comp.term#ocall'('lo.core#none', 'lo.comp.term#cons'(XX14280, 'lo.core#,..'('lo.comp.term#strng'(XNm), 'lo.core#,..'('lo.comp.term#intgr'(XQNo), 'lo.core#,..'(XFrVr, 'lo.core#[]')))), XDgVr, XDgVr), XFBx).
'lo.comp.debug@deframeDebug'(X_1120, X_1121, XF, XF, X_1122).
'lo.comp.debug@breakDebug'(XNm, 'lo.core#,..'('lo.comp.term#ocall'('lo.core#none', 'lo.comp.term#cons'(XX14303, 'lo.core#,..'('lo.comp.term#strng'(XNm), 'lo.core#[]')), XDgVr, XDgVr), XBG), XBG, XOpts) :- 'lo.comp.transutils@isOption'('lo.comp.transutils#dbgVars'(XNm, XDgVr, X_1123), XOpts),
    'lo.comp.transutils@trCons'("break", 1, XX14303).
'lo.comp.debug@breakDebug'(X_1124, XG, XG, X_1125).
'lo.comp.debug@lineDebug'(XLc, 'lo.core#,..'('lo.comp.term#ocall'('lo.core#none', 'lo.comp.term#cons'(XX14327, 'lo.core#,..'('lo.comp.term#strng'(XPkgName), 'lo.core#,..'('lo.comp.term#intgr'(XX14331), 'lo.core#,..'('lo.comp.term#intgr'(XX14334), 'lo.core#,..'('lo.comp.term#intgr'(XX14337), 'lo.core#[]'))))), XDgVr, XDgVr), XP), XP, XOpts) :- 'lo.comp.transutils@isOption'('lo.comp.transutils#dbgVars'(X_1126, XDgVr, X_1127), XOpts),
    'lo.comp.transutils@isOption'('lo.comp.transutils#pkgName'(XPkgName), XOpts),
    'lo.comp.transutils@trCons'("line", 4, XX14327),
    'lo.comp.location@lineOf'(XLc, XX14331),
    'lo.comp.location@columnOf'(XLc, XX14334),
    ocall('size%2'(XLc, XX14337),'lo.core$sizeable$lo.comp.location*location','lo.core$sizeable$lo.comp.location*location').
'lo.comp.debug@lineDebug'(X_1128, XP, XP, X_1129).
'lo.comp.debug^debugPreamble'('_call%7'(XV2264, XV2265, XV2266, XV2267, XV2268, XV2269, XV2270), 'lo.comp.debug^debugPreamble', _) :- 'lo.comp.debug@debugPreamble'(XV2264, XV2265, XV2266, XV2267, XV2268, XV2269, XV2270).
'lo.comp.debug^constructFrameList'('_call%2'(XV2271, XV2272), 'lo.comp.debug^constructFrameList', _) :- 'lo.comp.debug@constructFrameList'(XV2271, XV2272).
'lo.comp.debug^frameDebug'('_call%6'(XV2273, XV2274, XV2275, XV2276, XV2277, XV2278), 'lo.comp.debug^frameDebug', _) :- 'lo.comp.debug@frameDebug'(XV2273, XV2274, XV2275, XV2276, XV2277, XV2278).
'lo.comp.debug^deframeDebug'('_call%5'(XV2279, XV2280, XV2281, XV2282, XV2283), 'lo.comp.debug^deframeDebug', _) :- 'lo.comp.debug@deframeDebug'(XV2279, XV2280, XV2281, XV2282, XV2283).
'lo.comp.debug^breakDebug'('_call%4'(XV2284, XV2285, XV2286, XV2287), 'lo.comp.debug^breakDebug', _) :- 'lo.comp.debug@breakDebug'(XV2284, XV2285, XV2286, XV2287).
'lo.comp.debug^lineDebug'('_call%4'(XV2288, XV2289, XV2290, XV2291), 'lo.comp.debug^lineDebug', _) :- 'lo.comp.debug@lineDebug'(XV2288, XV2289, XV2290, XV2291).
