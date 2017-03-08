'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.debug's'0.0.1'n5o5'()5'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.transutils'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.misc'e'*'s\"I5'debugPreamble'PT7SLt'lo.comp.term*term'Lt'lo.comp.term*term'Lt'lo.comp.term*pred'Lt'lo.comp.term*pred't'lo.comp.transutils*trOptions't'lo.comp.transutils*trOptions''frameDebug'PT6SiLt'lo.comp.term*term'Lt'lo.comp.term*pred'Lt'lo.comp.term*pred't'lo.comp.transutils*trOptions''deframeDebug'PT5SiLt'lo.comp.term*pred'Lt'lo.comp.term*pred't'lo.comp.transutils*trOptions''breakDebug'PT4SLt'lo.comp.term*pred'Lt'lo.comp.term*pred't'lo.comp.transutils*trOptions''lineDebug'PT4t'lo.comp.location*location'Lt'lo.comp.term*pred'Lt'lo.comp.term*pred't'lo.comp.transutils*trOptions'\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.debug@init'():- !.
'lo.comp.debug@debugPreamble'(XNm, XQ, 'lo.core#,..'(XDgVr, 'lo.core#,..'(XFrVr, XQ)), 'lo.core#,..'('lo.comp.term#call'('lo.core#none', 'lo.comp.term#prg'("go.debug@current", 1), 'lo.core#,..'(XDgVr, 'lo.core#[]')), XGx), XGx, XOpts, XXb19320):- 'lo.comp.transutils@isOption'('lo.comp.transutils#debugging', XOpts),
    'lo.comp.term@genVr'("__D", XXd39253),
    XDgVr = XXd39253,
    'lo.comp.term@genVr'("__F", XXd39254),
    XFrVr = XXd39254,
    'lo.comp.transutils@pushOpt'('lo.comp.transutils#dbgVars'(XNm, XDgVr, XFrVr), XOpts, XXb19320).
'lo.comp.debug@debugPreamble'(X_34082, XQ, XQ, XP, XP, XOpts, XOpts).
'lo.comp.debug@constructFrameList'('lo.core#[]', 'lo.comp.term#enum'("lo.list#[]")):- !.
'lo.comp.debug@constructFrameList'('lo.core#,..'('lo.comp.term#varbl'(XV), XVars), XXd39256):- 'lo.comp.misc@starts_with'(XV, "_"),
    !,
    'lo.comp.debug@constructFrameList'(XVars, XXd39256).
'lo.comp.debug@constructFrameList'('lo.core#,..'('lo.comp.term#varbl'(XV), XVars), 'lo.comp.term#cons'('lo.comp.term#strct'("lo.list#,..", 2), 'lo.core#,..'(XXd39262, 'lo.core#,..'(XXd39263, 'lo.core#[]')))):- !,
    'lo.comp.term@mkTpl'('lo.core#,..'('lo.comp.term#strng'(XV), 'lo.core#,..'('lo.comp.term#varbl'(XV), 'lo.core#[]')), XXd39262),
    'lo.comp.debug@constructFrameList'(XVars, XXd39263).
'lo.comp.debug@constructFrameList'(_, _):- raise_exception('error'("lo.comp.debug@constructFrameList", 31, 3, 43)).
'lo.comp.debug@frameDebug'(XNm, XQNo, XQ, XG, XGx, XOpts):- 'lo.comp.transutils@isOption'('lo.comp.transutils#dbgVars'(XNm, XDgVr, XFrVr), XOpts),
    'lo.comp.debug@constructFrameList'(XQ, XXd39268),
    'lo.comp.transutils@trCons'("frame", 3, XXd39270),
    XG = 'lo.core#,..'('lo.comp.term#unfy'('lo.core#none', XFrVr, XXd39268), 'lo.core#,..'('lo.comp.term#ocall'('lo.core#none', 'lo.comp.term#cons'(XXd39270, 'lo.core#,..'('lo.comp.term#strng'(XNm), 'lo.core#,..'('lo.comp.term#intgr'(XQNo), 'lo.core#,..'(XFrVr, 'lo.core#[]')))), XDgVr, XDgVr), XGx)).
'lo.comp.debug@frameDebug'(X_34094, X_34095, X_34096, XF, XF, X_34097).
'lo.comp.debug@deframeDebug'(XNm, XQNo, XFB, XFBx, XOpts):- 'lo.comp.transutils@isOption'('lo.comp.transutils#dbgVars'(XNm, XDgVr, XFrVr), XOpts),
    'lo.comp.transutils@trCons'("deframe", 3, XXd39281),
    XFB = 'lo.core#,..'('lo.comp.term#ocall'('lo.core#none', 'lo.comp.term#cons'(XXd39281, 'lo.core#,..'('lo.comp.term#strng'(XNm), 'lo.core#,..'('lo.comp.term#intgr'(XQNo), 'lo.core#,..'(XFrVr, 'lo.core#[]')))), XDgVr, XDgVr), XFBx).
'lo.comp.debug@deframeDebug'(X_34102, X_34103, XF, XF, X_34104).
'lo.comp.debug@breakDebug'(XNm, 'lo.core#,..'('lo.comp.term#ocall'('lo.core#none', 'lo.comp.term#cons'(XXb19325, 'lo.core#,..'('lo.comp.term#strng'(XNm), 'lo.core#[]')), XDgVr, XDgVr), XBG), XBG, XOpts):- 'lo.comp.transutils@isOption'('lo.comp.transutils#dbgVars'(XNm, XDgVr, X_34107), XOpts),
    'lo.comp.transutils@trCons'("break", 1, XXb19325).
'lo.comp.debug@breakDebug'(X_34108, XG, XG, X_34109).
'lo.comp.debug@lineDebug'(XLc, 'lo.core#,..'('lo.comp.term#ocall'('lo.core#none', 'lo.comp.term#cons'(XXb19331, 'lo.core#,..'('lo.comp.term#strng'(XPkgName), 'lo.core#,..'('lo.comp.term#intgr'(XXb19333), 'lo.core#,..'('lo.comp.term#intgr'(XXb19335), 'lo.core#,..'('lo.comp.term#intgr'(XXe4939), 'lo.core#[]'))))), XDgVr, XDgVr), XP), XP, XOpts):- 'lo.comp.transutils@isOption'('lo.comp.transutils#dbgVars'(X_34115, XDgVr, X_34116), XOpts),
    'lo.comp.transutils@isOption'('lo.comp.transutils#pkgName'(XPkgName), XOpts),
    'lo.comp.transutils@trCons'("line", 4, XXb19331),
    'lo.comp.location@lineOf'(XLc, XXb19333),
    'lo.comp.location@columnOf'(XLc, XXb19335),
    ocall('size%1'(XXV5301),'lo.core$sizeable$lo.comp.location*location','lo.core$sizeable$lo.comp.location*location'),
    ocall('_call%2'(XLc, XXe4939),XXV5301,XXV5301).
'lo.comp.debug@lineDebug'(X_34117, XP, XP, X_34118).
'lo.comp.debug^debugPreamble'('_call%7'(XV30699, XV30700, XV30701, XV30702, XV30703, XV30704, XV30705), 'lo.comp.debug^debugPreamble', _):- 'lo.comp.debug@debugPreamble'(XV30699, XV30700, XV30701, XV30702, XV30703, XV30704, XV30705).
'lo.comp.debug^constructFrameList'('_call%2'(XV30706, XV30707), 'lo.comp.debug^constructFrameList', _):- 'lo.comp.debug@constructFrameList'(XV30706, XV30707).
'lo.comp.debug^frameDebug'('_call%6'(XV30708, XV30709, XV30710, XV30711, XV30712, XV30713), 'lo.comp.debug^frameDebug', _):- 'lo.comp.debug@frameDebug'(XV30708, XV30709, XV30710, XV30711, XV30712, XV30713).
'lo.comp.debug^deframeDebug'('_call%5'(XV30714, XV30715, XV30716, XV30717, XV30718), 'lo.comp.debug^deframeDebug', _):- 'lo.comp.debug@deframeDebug'(XV30714, XV30715, XV30716, XV30717, XV30718).
'lo.comp.debug^breakDebug'('_call%4'(XV30719, XV30720, XV30721, XV30722), 'lo.comp.debug^breakDebug', _):- 'lo.comp.debug@breakDebug'(XV30719, XV30720, XV30721, XV30722).
'lo.comp.debug^lineDebug'('_call%4'(XV30723, XV30724, XV30725, XV30726), 'lo.comp.debug^lineDebug', _):- 'lo.comp.debug@lineDebug'(XV30723, XV30724, XV30725, XV30726).
