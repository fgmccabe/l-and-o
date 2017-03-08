'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.code.asm'e'*'n17o17'()17'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.registers'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.instructions'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.code'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'n2o2'import'e'public'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.escapes'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I3'codeSeg'CT4t'lo.comp.term*term'LiLt'lo.comp.term*term'Lt'lo.comp.term*term't'lo.comp.code.asm*codeSeg''codeMdl'CT2t'lo.comp.package*pkgSpec'Lt'lo.comp.code.asm*codeSeg't'lo.comp.code.asm*codeMdl''asm'FT1t'lo.comp.code.code*assem't'lo.comp.code.asm*codeSeg'\"s\"I2'codeSeg'Yt'lo.comp.code.asm*codeSeg'I0'codeMdl'Yt'lo.comp.code.asm*codeMdl'I0\"n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.code.asm@init'() :- !.
'lo.comp.code.asm#codeSeg'('codeSeg%1'('lo.comp.code.asm@codeSeg'())) :- !.
'lo.comp.code.asm#codeMdl'('codeMdl%1'('lo.comp.code.asm@codeMdl'())) :- !.
'lo.comp.code.asm@genLblTbl'('lo.core#[]', X_2539, XD, XD) :- !.
'lo.comp.code.asm@genLblTbl'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XLbl), XI), XPc, XD, XX38440) :- !,
    ocall('_put%4'(XD, XLbl, XPc, XX38437),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.code.asm@genLblTbl'(XI, XPc, XX38437, XX38440).
'lo.comp.code.asm@genLblTbl'('lo.core#,..'(X_2540, XI), XPc, XD, XX38451) :- !,
    ocall('+%3'(XPc, 1, XX38448),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@genLblTbl'(XI, XX38448, XD, XX38451).
'lo.comp.code.asm@genLblTbl'(_, _, _, _) :- raise_exception('error'("genLblTbl", 113, 3, 22)).
'lo.comp.code.asm@ltOff'(XLb, XLbls, XTgt) :- ocall('present%3'(XLbls, XLb, XTgt),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    !.
'lo.comp.code.asm@ltOff'(_, _, _) :- raise_exception('error'("ltOff", 128, 3, 49)).
'lo.comp.code.asm@pcGap'(Xpc, XLb, XLbls, Xmx, XGap) :- ocall('present%3'(XLbls, XLb, XTgt),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('-%3'(XTgt, Xpc, XX38472),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('-%3'(XX38472, 1, XX38474),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    XGap = XX38474,
    'lo.core@=<'('lo.core$comp$lo.core*integer', XGap, Xmx),
    !.
'lo.comp.code.asm@pcGap'(_, _, _, _, _) :- raise_exception('error'("pcGap", 132, 3, 88)).
'lo.comp.code.asm@mnem'('lo.core#[]', X_2541, X_2542, X_2543, 'lo.core#[]') :- !.
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iLbl'(X_2544), XI), XLbls, XLits, XPc, XX38496) :- !,
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XPc, XX38496).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iHalt', XI), XLbls, XLits, Xpc, 'lo.core#,..'(0, XX38509)) :- !,
    ocall('+%3'(Xpc, 1, XX38507),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38507, XX38509).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iDie', XI), XLbls, XLits, Xpc, 'lo.core#,..'(1, XX38523)) :- !,
    ocall('+%3'(Xpc, 1, XX38521),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38521, XX38523).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iSucc', XI), XLbls, XLits, Xpc, 'lo.core#,..'(2, XX38537)) :- !,
    ocall('+%3'(Xpc, 1, XX38535),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38535, XX38537).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iKawl'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX38554, XX38561)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX38548),
    'lo.bits@.|.'(3, XX38548, XX38549),
    'lo.comp.code.asm@ltOff'(XV1, XLits, XX38552),
    'lo.bits@.<<.'(XX38552, 8, XX38553),
    'lo.bits@.|.'(XX38549, XX38553, XX38554),
    ocall('+%3'(Xpc, 1, XX38559),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38559, XX38561).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iLkawl'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX38578, XX38585)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX38572),
    'lo.bits@.|.'(4, XX38572, XX38573),
    'lo.comp.code.asm@ltOff'(XV1, XLits, XX38576),
    'lo.bits@.<<.'(XX38576, 8, XX38577),
    'lo.bits@.|.'(XX38573, XX38577, XX38578),
    ocall('+%3'(Xpc, 1, XX38583),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38583, XX38585).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iDlkawl'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX38602, XX38609)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX38596),
    'lo.bits@.|.'(5, XX38596, XX38597),
    'lo.comp.code.asm@ltOff'(XV1, XLits, XX38600),
    'lo.bits@.<<.'(XX38600, 8, XX38601),
    'lo.bits@.|.'(XX38597, XX38601, XX38602),
    ocall('+%3'(Xpc, 1, XX38607),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38607, XX38609).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iKawlO'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX38624, XX38631)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX38620),
    'lo.bits@.|.'(6, XX38620, XX38621),
    'lo.bits@.<<.'(XV1, 16, XX38623),
    'lo.bits@.|.'(XX38621, XX38623, XX38624),
    ocall('+%3'(Xpc, 1, XX38629),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38629, XX38631).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iLkawlO'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX38646, XX38653)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX38642),
    'lo.bits@.|.'(7, XX38642, XX38643),
    'lo.bits@.<<.'(XV1, 16, XX38645),
    'lo.bits@.|.'(XX38643, XX38645, XX38646),
    ocall('+%3'(Xpc, 1, XX38651),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38651, XX38653).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iDlkawlO'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX38668, XX38675)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX38664),
    'lo.bits@.|.'(8, XX38664, XX38665),
    'lo.bits@.<<.'(XV1, 16, XX38667),
    'lo.bits@.|.'(XX38665, XX38667, XX38668),
    ocall('+%3'(Xpc, 1, XX38673),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38673, XX38675).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iGo_to'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX38689, XX38696)) :- !,
    'lo.comp.code.asm@pcGap'(Xpc, XV0, XLbls, 16777215, XX38687),
    'lo.bits@.<<.'(XX38687, 8, XX38688),
    'lo.bits@.|.'(12, XX38688, XX38689),
    ocall('+%3'(Xpc, 1, XX38694),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38694, XX38696).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iEscape'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX38712, XX38719)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX38707),
    'lo.bits@.|.'(13, XX38707, XX38708),
    'lo.comp.escapes@escCode'(XV1, XX38710),
    'lo.bits@.<<.'(XX38710, 8, XX38711),
    'lo.bits@.|.'(XX38708, XX38711, XX38712),
    ocall('+%3'(Xpc, 1, XX38717),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38717, XX38719).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iAlloc'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX38734, XX38741)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX38730),
    'lo.bits@.|.'(16, XX38730, XX38731),
    'lo.bits@.<<.'(XV1, 8, XX38733),
    'lo.bits@.|.'(XX38731, XX38733, XX38734),
    ocall('+%3'(Xpc, 1, XX38739),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38739, XX38741).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iDealloc', XI), XLbls, XLits, Xpc, 'lo.core#,..'(17, XX38755)) :- !,
    ocall('+%3'(Xpc, 1, XX38753),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38753, XX38755).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iTryme'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX38769, XX38776)) :- !,
    'lo.comp.code.asm@pcGap'(Xpc, XV0, XLbls, 16777215, XX38767),
    'lo.bits@.<<.'(XX38767, 8, XX38768),
    'lo.bits@.|.'(18, XX38768, XX38769),
    ocall('+%3'(Xpc, 1, XX38774),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38774, XX38776).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iRetryme'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX38790, XX38797)) :- !,
    'lo.comp.code.asm@pcGap'(Xpc, XV0, XLbls, 16777215, XX38788),
    'lo.bits@.<<.'(XX38788, 8, XX38789),
    'lo.bits@.|.'(19, XX38789, XX38790),
    ocall('+%3'(Xpc, 1, XX38795),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38795, XX38797).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iTrustme', XI), XLbls, XLits, Xpc, 'lo.core#,..'(20, XX38811)) :- !,
    ocall('+%3'(Xpc, 1, XX38809),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38809, XX38811).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iTrycl'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX38825, XX38832)) :- !,
    'lo.comp.code.asm@pcGap'(Xpc, XV0, XLbls, 16777215, XX38823),
    'lo.bits@.<<.'(XX38823, 8, XX38824),
    'lo.bits@.|.'(21, XX38824, XX38825),
    ocall('+%3'(Xpc, 1, XX38830),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38830, XX38832).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iRetry'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX38846, XX38853)) :- !,
    'lo.comp.code.asm@pcGap'(Xpc, XV0, XLbls, 16777215, XX38844),
    'lo.bits@.<<.'(XX38844, 8, XX38845),
    'lo.bits@.|.'(22, XX38845, XX38846),
    ocall('+%3'(Xpc, 1, XX38851),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38851, XX38853).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iTrust'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX38867, XX38874)) :- !,
    'lo.comp.code.asm@pcGap'(Xpc, XV0, XLbls, 16777215, XX38865),
    'lo.bits@.<<.'(XX38865, 8, XX38866),
    'lo.bits@.|.'(23, XX38866, XX38867),
    ocall('+%3'(Xpc, 1, XX38872),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38872, XX38874).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iFayl', XI), XLbls, XLits, Xpc, 'lo.core#,..'(24, XX38888)) :- !,
    ocall('+%3'(Xpc, 1, XX38886),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38886, XX38888).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iCut', XI), XLbls, XLits, Xpc, 'lo.core#,..'(25, XX38902)) :- !,
    ocall('+%3'(Xpc, 1, XX38900),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38900, XX38902).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iIndexi'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX38917, XX38924)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX38913),
    'lo.bits@.|.'(26, XX38913, XX38914),
    'lo.bits@.<<.'(XV1, 8, XX38916),
    'lo.bits@.|.'(XX38914, XX38916, XX38917),
    ocall('+%3'(Xpc, 1, XX38922),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38922, XX38924).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iIndexs'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX38939, XX38946)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX38935),
    'lo.bits@.|.'(28, XX38935, XX38936),
    'lo.bits@.<<.'(XV1, 8, XX38938),
    'lo.bits@.|.'(XX38936, XX38938, XX38939),
    ocall('+%3'(Xpc, 1, XX38944),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38944, XX38946).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iIndexn'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX38961, XX38968)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX38957),
    'lo.bits@.|.'(29, XX38957, XX38958),
    'lo.bits@.<<.'(XV1, 8, XX38960),
    'lo.bits@.|.'(XX38958, XX38960, XX38961),
    ocall('+%3'(Xpc, 1, XX38966),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38966, XX38968).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iIndexx'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX38983, XX38990)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX38979),
    'lo.bits@.|.'(30, XX38979, XX38980),
    'lo.bits@.<<.'(XV1, 8, XX38982),
    'lo.bits@.|.'(XX38980, XX38982, XX38983),
    ocall('+%3'(Xpc, 1, XX38988),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX38988, XX38990).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iTrpblk', XI), XLbls, XLits, Xpc, 'lo.core#,..'(31, XX39004)) :- !,
    ocall('+%3'(Xpc, 1, XX39002),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39002, XX39004).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iTrpend', XI), XLbls, XLits, Xpc, 'lo.core#,..'(32, XX39018)) :- !,
    ocall('+%3'(Xpc, 1, XX39016),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39016, XX39018).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iExcept'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39029, XX39036)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39028),
    'lo.bits@.|.'(33, XX39028, XX39029),
    ocall('+%3'(Xpc, 1, XX39034),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39034, XX39036).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iGcmap'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39051, XX39058)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39047),
    'lo.bits@.|.'(34, XX39047, XX39048),
    'lo.bits@.<<.'(XV1, 8, XX39050),
    'lo.bits@.|.'(XX39048, XX39050, XX39051),
    ocall('+%3'(Xpc, 1, XX39056),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39056, XX39058).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iGc'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39073, XX39080)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39069),
    'lo.bits@.|.'(35, XX39069, XX39070),
    'lo.bits@.<<.'(XV1, 8, XX39072),
    'lo.bits@.|.'(XX39070, XX39072, XX39073),
    ocall('+%3'(Xpc, 1, XX39078),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39078, XX39080).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iSusp'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39095, XX39102)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39091),
    'lo.bits@.|.'(37, XX39091, XX39092),
    'lo.bits@.<<.'(XV1, 16, XX39094),
    'lo.bits@.|.'(XX39092, XX39094, XX39095),
    ocall('+%3'(Xpc, 1, XX39100),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39100, XX39102).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iResume'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39113, XX39120)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39112),
    'lo.bits@.|.'(38, XX39112, XX39113),
    ocall('+%3'(Xpc, 1, XX39118),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39118, XX39120).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iTrgr'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39131, XX39138)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39130),
    'lo.bits@.|.'(39, XX39130, XX39131),
    ocall('+%3'(Xpc, 1, XX39136),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39136, XX39138).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iUAA'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39153, XX39160)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39149),
    'lo.bits@.|.'(42, XX39149, XX39150),
    'lo.bits@.<<.'(XV1, 16, XX39152),
    'lo.bits@.|.'(XX39150, XX39152, XX39153),
    ocall('+%3'(Xpc, 1, XX39158),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39158, XX39160).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iUAY'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39175, XX39182)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39171),
    'lo.bits@.|.'(43, XX39171, XX39172),
    'lo.bits@.<<.'(XV1, 8, XX39174),
    'lo.bits@.|.'(XX39172, XX39174, XX39175),
    ocall('+%3'(Xpc, 1, XX39180),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39180, XX39182).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iUAS'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39193, XX39200)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39192),
    'lo.bits@.|.'(45, XX39192, XX39193),
    ocall('+%3'(Xpc, 1, XX39198),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39198, XX39200).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iUcAS'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39211, XX39218)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39210),
    'lo.bits@.|.'(46, XX39210, XX39211),
    ocall('+%3'(Xpc, 1, XX39216),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39216, XX39218).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iUAlit'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39235, XX39242)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39229),
    'lo.bits@.|.'(47, XX39229, XX39230),
    'lo.comp.code.asm@ltOff'(XV1, XLits, XX39233),
    'lo.bits@.<<.'(XX39233, 8, XX39234),
    'lo.bits@.|.'(XX39230, XX39234, XX39235),
    ocall('+%3'(Xpc, 1, XX39240),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39240, XX39242).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iUAcns'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39259, XX39266)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39253),
    'lo.bits@.|.'(50, XX39253, XX39254),
    'lo.comp.code.asm@ltOff'(XV1, XLits, XX39257),
    'lo.bits@.<<.'(XX39257, 8, XX39258),
    'lo.bits@.|.'(XX39254, XX39258, XX39259),
    ocall('+%3'(Xpc, 1, XX39264),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39264, XX39266).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iUYY'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39281, XX39288)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39277),
    'lo.bits@.|.'(51, XX39277, XX39278),
    'lo.bits@.<<.'(XV1, 16, XX39280),
    'lo.bits@.|.'(XX39278, XX39280, XX39281),
    ocall('+%3'(Xpc, 1, XX39286),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39286, XX39288).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iUYS'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39299, XX39306)) :- !,
    'lo.bits@.<<.'(XV0, 8, XX39298),
    'lo.bits@.|.'(52, XX39298, XX39299),
    ocall('+%3'(Xpc, 1, XX39304),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39304, XX39306).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iUcYS'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39317, XX39324)) :- !,
    'lo.bits@.<<.'(XV0, 8, XX39316),
    'lo.bits@.|.'(53, XX39316, XX39317),
    ocall('+%3'(Xpc, 1, XX39322),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39322, XX39324).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iUSlit'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39337, XX39344)) :- !,
    'lo.comp.code.asm@ltOff'(XV0, XLits, XX39335),
    'lo.bits@.<<.'(XX39335, 8, XX39336),
    'lo.bits@.|.'(60, XX39336, XX39337),
    ocall('+%3'(Xpc, 1, XX39342),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39342, XX39344).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iUScns'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39357, XX39364)) :- !,
    'lo.comp.code.asm@ltOff'(XV0, XLits, XX39355),
    'lo.bits@.<<.'(XX39355, 8, XX39356),
    'lo.bits@.|.'(63, XX39356, XX39357),
    ocall('+%3'(Xpc, 1, XX39362),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39362, XX39364).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iMAA'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39379, XX39386)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39375),
    'lo.bits@.|.'(70, XX39375, XX39376),
    'lo.bits@.<<.'(XV1, 16, XX39378),
    'lo.bits@.|.'(XX39376, XX39378, XX39379),
    ocall('+%3'(Xpc, 1, XX39384),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39384, XX39386).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iMAY'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39401, XX39408)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39397),
    'lo.bits@.|.'(71, XX39397, XX39398),
    'lo.bits@.<<.'(XV1, 8, XX39400),
    'lo.bits@.|.'(XX39398, XX39400, XX39401),
    ocall('+%3'(Xpc, 1, XX39406),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39406, XX39408).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iMuAY'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39423, XX39430)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39419),
    'lo.bits@.|.'(72, XX39419, XX39420),
    'lo.bits@.<<.'(XV1, 8, XX39422),
    'lo.bits@.|.'(XX39420, XX39422, XX39423),
    ocall('+%3'(Xpc, 1, XX39428),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39428, XX39430).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iMAS'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39441, XX39448)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39440),
    'lo.bits@.|.'(74, XX39440, XX39441),
    ocall('+%3'(Xpc, 1, XX39446),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39446, XX39448).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iMAlit'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39465, XX39472)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39459),
    'lo.bits@.|.'(75, XX39459, XX39460),
    'lo.comp.code.asm@ltOff'(XV1, XLits, XX39463),
    'lo.bits@.<<.'(XX39463, 8, XX39464),
    'lo.bits@.|.'(XX39460, XX39464, XX39465),
    ocall('+%3'(Xpc, 1, XX39470),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39470, XX39472).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iMAcns'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39489, XX39496)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39483),
    'lo.bits@.|.'(76, XX39483, XX39484),
    'lo.comp.code.asm@ltOff'(XV1, XLits, XX39487),
    'lo.bits@.<<.'(XX39487, 8, XX39488),
    'lo.bits@.|.'(XX39484, XX39488, XX39489),
    ocall('+%3'(Xpc, 1, XX39494),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39494, XX39496).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iMYA'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39511, XX39518)) :- !,
    'lo.bits@.<<.'(XV0, 8, XX39507),
    'lo.bits@.|.'(77, XX39507, XX39508),
    'lo.bits@.<<.'(XV1, 24, XX39510),
    'lo.bits@.|.'(XX39508, XX39510, XX39511),
    ocall('+%3'(Xpc, 1, XX39516),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39516, XX39518).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iMYY'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39533, XX39540)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39529),
    'lo.bits@.|.'(78, XX39529, XX39530),
    'lo.bits@.<<.'(XV1, 16, XX39532),
    'lo.bits@.|.'(XX39530, XX39532, XX39533),
    ocall('+%3'(Xpc, 1, XX39538),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39538, XX39540).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iMYS'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39551, XX39558)) :- !,
    'lo.bits@.<<.'(XV0, 8, XX39550),
    'lo.bits@.|.'(80, XX39550, XX39551),
    ocall('+%3'(Xpc, 1, XX39556),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39556, XX39558).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iMSA'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39569, XX39576)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39568),
    'lo.bits@.|.'(83, XX39568, XX39569),
    ocall('+%3'(Xpc, 1, XX39574),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39574, XX39576).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iMSY'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39587, XX39594)) :- !,
    'lo.bits@.<<.'(XV0, 8, XX39586),
    'lo.bits@.|.'(84, XX39586, XX39587),
    ocall('+%3'(Xpc, 1, XX39592),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39592, XX39594).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iMSlit'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39607, XX39614)) :- !,
    'lo.comp.code.asm@ltOff'(XV0, XLits, XX39605),
    'lo.bits@.<<.'(XX39605, 8, XX39606),
    'lo.bits@.|.'(85, XX39606, XX39607),
    ocall('+%3'(Xpc, 1, XX39612),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39612, XX39614).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iMScns'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39627, XX39634)) :- !,
    'lo.comp.code.asm@ltOff'(XV0, XLits, XX39625),
    'lo.bits@.<<.'(XX39625, 8, XX39626),
    'lo.bits@.|.'(88, XX39626, XX39627),
    ocall('+%3'(Xpc, 1, XX39632),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39632, XX39634).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iOAU'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39645, XX39652)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39644),
    'lo.bits@.|.'(90, XX39644, XX39645),
    ocall('+%3'(Xpc, 1, XX39650),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39650, XX39652).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iOYU'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39663, XX39670)) :- !,
    'lo.bits@.<<.'(XV0, 8, XX39662),
    'lo.bits@.|.'(91, XX39662, XX39663),
    ocall('+%3'(Xpc, 1, XX39668),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39668, XX39670).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iOYA'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39685, XX39692)) :- !,
    'lo.bits@.<<.'(XV0, 8, XX39681),
    'lo.bits@.|.'(92, XX39681, XX39682),
    'lo.bits@.<<.'(XV1, 24, XX39684),
    'lo.bits@.|.'(XX39682, XX39684, XX39685),
    ocall('+%3'(Xpc, 1, XX39690),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39690, XX39692).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iOYnil'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39703, XX39710)) :- !,
    'lo.bits@.<<.'(XV0, 8, XX39702),
    'lo.bits@.|.'(93, XX39702, XX39703),
    ocall('+%3'(Xpc, 1, XX39708),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39708, XX39710).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iCAA'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39725, XX39732)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39721),
    'lo.bits@.|.'(110, XX39721, XX39722),
    'lo.bits@.<<.'(XV1, 16, XX39724),
    'lo.bits@.|.'(XX39722, XX39724, XX39725),
    ocall('+%3'(Xpc, 1, XX39730),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39730, XX39732).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iCAY'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39747, XX39754)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39743),
    'lo.bits@.|.'(111, XX39743, XX39744),
    'lo.bits@.<<.'(XV1, 8, XX39746),
    'lo.bits@.|.'(XX39744, XX39746, XX39747),
    ocall('+%3'(Xpc, 1, XX39752),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39752, XX39754).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iCAS'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39765, XX39772)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39764),
    'lo.bits@.|.'(112, XX39764, XX39765),
    ocall('+%3'(Xpc, 1, XX39770),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39770, XX39772).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iCAlit'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39789, XX39796)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39783),
    'lo.bits@.|.'(113, XX39783, XX39784),
    'lo.comp.code.asm@ltOff'(XV1, XLits, XX39787),
    'lo.bits@.<<.'(XX39787, 8, XX39788),
    'lo.bits@.|.'(XX39784, XX39788, XX39789),
    ocall('+%3'(Xpc, 1, XX39794),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39794, XX39796).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iCAcns'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39813, XX39820)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39807),
    'lo.bits@.|.'(116, XX39807, XX39808),
    'lo.comp.code.asm@ltOff'(XV1, XLits, XX39811),
    'lo.bits@.<<.'(XX39811, 8, XX39812),
    'lo.bits@.|.'(XX39808, XX39812, XX39813),
    ocall('+%3'(Xpc, 1, XX39818),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39818, XX39820).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iCYA'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39835, XX39842)) :- !,
    'lo.bits@.<<.'(XV0, 8, XX39831),
    'lo.bits@.|.'(117, XX39831, XX39832),
    'lo.bits@.<<.'(XV1, 24, XX39834),
    'lo.bits@.|.'(XX39832, XX39834, XX39835),
    ocall('+%3'(Xpc, 1, XX39840),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39840, XX39842).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iCYS'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39853, XX39860)) :- !,
    'lo.bits@.<<.'(XV0, 8, XX39852),
    'lo.bits@.|.'(118, XX39852, XX39853),
    ocall('+%3'(Xpc, 1, XX39858),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39858, XX39860).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iCSA'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39871, XX39878)) :- !,
    'lo.bits@.<<.'(XV0, 16, XX39870),
    'lo.bits@.|.'(121, XX39870, XX39871),
    ocall('+%3'(Xpc, 1, XX39876),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39876, XX39878).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iCSY'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39889, XX39896)) :- !,
    'lo.bits@.<<.'(XV0, 8, XX39888),
    'lo.bits@.|.'(122, XX39888, XX39889),
    ocall('+%3'(Xpc, 1, XX39894),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39894, XX39896).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iCSlit'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39909, XX39916)) :- !,
    'lo.comp.code.asm@ltOff'(XV0, XLits, XX39907),
    'lo.bits@.<<.'(XX39907, 8, XX39908),
    'lo.bits@.|.'(123, XX39908, XX39909),
    ocall('+%3'(Xpc, 1, XX39914),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39914, XX39916).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iCScns'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39929, XX39936)) :- !,
    'lo.comp.code.asm@ltOff'(XV0, XLits, XX39927),
    'lo.bits@.<<.'(XX39927, 8, XX39928),
    'lo.bits@.|.'(126, XX39928, XX39929),
    ocall('+%3'(Xpc, 1, XX39934),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39934, XX39936).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iClAA'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39951, XX39958)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39947),
    'lo.bits@.|.'(160, XX39947, XX39948),
    'lo.bits@.<<.'(XV1, 16, XX39950),
    'lo.bits@.|.'(XX39948, XX39950, XX39951),
    ocall('+%3'(Xpc, 1, XX39956),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39956, XX39958).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iClAY'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39973, XX39980)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39969),
    'lo.bits@.|.'(161, XX39969, XX39970),
    'lo.bits@.<<.'(XV1, 8, XX39972),
    'lo.bits@.|.'(XX39970, XX39972, XX39973),
    ocall('+%3'(Xpc, 1, XX39978),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39978, XX39980).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iClAS'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX39991, XX39998)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX39990),
    'lo.bits@.|.'(163, XX39990, XX39991),
    ocall('+%3'(Xpc, 1, XX39996),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX39996, XX39998).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iClSA'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX40009, XX40016)) :- !,
    'lo.bits@.<<.'(XV0, 16, XX40008),
    'lo.bits@.|.'(164, XX40008, XX40009),
    ocall('+%3'(Xpc, 1, XX40014),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX40014, XX40016).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iClSY'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX40027, XX40034)) :- !,
    'lo.bits@.<<.'(XV0, 8, XX40026),
    'lo.bits@.|.'(165, XX40026, XX40027),
    ocall('+%3'(Xpc, 1, XX40032),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX40032, XX40034).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iVrA'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX40045, XX40052)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX40044),
    'lo.bits@.|.'(166, XX40044, XX40045),
    ocall('+%3'(Xpc, 1, XX40050),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX40050, XX40052).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iVrY'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX40063, XX40070)) :- !,
    'lo.bits@.<<.'(XV0, 8, XX40062),
    'lo.bits@.|.'(167, XX40062, XX40063),
    ocall('+%3'(Xpc, 1, XX40068),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX40068, XX40070).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iNvrA'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX40081, XX40088)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX40080),
    'lo.bits@.|.'(168, XX40080, XX40081),
    ocall('+%3'(Xpc, 1, XX40086),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX40086, XX40088).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iNvrY'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX40099, XX40106)) :- !,
    'lo.bits@.<<.'(XV0, 8, XX40098),
    'lo.bits@.|.'(169, XX40098, XX40099),
    ocall('+%3'(Xpc, 1, XX40104),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX40104, XX40106).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iVdA'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX40117, XX40124)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX40116),
    'lo.bits@.|.'(180, XX40116, XX40117),
    ocall('+%3'(Xpc, 1, XX40122),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX40122, XX40124).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iVdAA'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX40139, XX40146)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX40135),
    'lo.bits@.|.'(181, XX40135, XX40136),
    'lo.bits@.<<.'(XV1, 8, XX40138),
    'lo.bits@.|.'(XX40136, XX40138, XX40139),
    ocall('+%3'(Xpc, 1, XX40144),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX40144, XX40146).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iVdY'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX40157, XX40164)) :- !,
    'lo.bits@.<<.'(XV0, 8, XX40156),
    'lo.bits@.|.'(182, XX40156, XX40157),
    ocall('+%3'(Xpc, 1, XX40162),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX40162, XX40164).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iVdYY'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX40179, XX40186)) :- !,
    'lo.bits@.<<.'(XV0, 8, XX40175),
    'lo.bits@.|.'(183, XX40175, XX40176),
    'lo.bits@.<<.'(XV1, 24, XX40178),
    'lo.bits@.|.'(XX40176, XX40178, XX40179),
    ocall('+%3'(Xpc, 1, XX40184),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX40184, XX40186).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iClA'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX40197, XX40204)) :- !,
    'lo.bits@.<<.'(XV0, 24, XX40196),
    'lo.bits@.|.'(184, XX40196, XX40197),
    ocall('+%3'(Xpc, 1, XX40202),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX40202, XX40204).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iClY'(XV0), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX40215, XX40222)) :- !,
    'lo.bits@.<<.'(XV0, 8, XX40214),
    'lo.bits@.|.'(185, XX40214, XX40215),
    ocall('+%3'(Xpc, 1, XX40220),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX40220, XX40222).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iClS', XI), XLbls, XLits, Xpc, 'lo.core#,..'(187, XX40236)) :- !,
    ocall('+%3'(Xpc, 1, XX40234),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX40234, XX40236).
'lo.comp.code.asm@mnem'('lo.core#,..'('lo.comp.code.instructions#iClYY'(XV0, XV1), XI), XLbls, XLits, Xpc, 'lo.core#,..'(XX40251, XX40258)) :- !,
    'lo.bits@.<<.'(XV0, 8, XX40247),
    'lo.bits@.|.'(188, XX40247, XX40248),
    'lo.bits@.<<.'(XV1, 24, XX40250),
    'lo.bits@.|.'(XX40248, XX40250, XX40251),
    ocall('+%3'(Xpc, 1, XX40256),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.asm@mnem'(XI, XLbls, XLits, XX40256, XX40258).
'lo.comp.code.asm@mnem'(_, _, _, _, _) :- raise_exception('error'("mnem", 20, 3, 20)).
'lo.comp.code.asm@genLitTbl'('lo.core#[]', X_2545, XD, XD) :- !.
'lo.comp.code.asm@genLitTbl'('lo.core#,..'('lo.comp.code.code#litrl'(XLbl, X_2546), XI), XPc, XD, XX40281) :- !,
    ocall('+%3'(XPc, 1, XX40273),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('_put%4'(XD, XLbl, XPc, XX40278),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.code.asm@genLitTbl'(XI, XX40273, XX40278, XX40281).
'lo.comp.code.asm@genLitTbl'(_, _, _, _) :- raise_exception('error'("genLitTbl", 118, 3, 22)).
'lo.comp.code.asm@genSrcMap'('lo.core#[]', X_2547, 'lo.core#[]') :- !.
'lo.comp.code.asm@genSrcMap'('lo.core#,..'((XS, XE, 'lo.comp.term#tloc'(XSt, XLn)), XL), XM, 'lo.core#,..'('lo.comp.term#cons'('lo.comp.term#strct'("()4", 4), 'lo.core#,..'('lo.comp.term#intgr'(XSx), 'lo.core#,..'('lo.comp.term#intgr'(XEx), 'lo.core#,..'('lo.comp.term#intgr'(XSt), 'lo.core#,..'('lo.comp.term#intgr'(XLn), 'lo.core#[]'))))), XX40320)) :- ocall('present%3'(XM, XS, XSx),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('present%3'(XM, XE, XEx),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    !,
    'lo.comp.code.asm@genSrcMap'(XL, XM, XX40320).
'lo.comp.code.asm@genSrcMap'(_, _, _) :- raise_exception('error'("genSrcMap", 122, 3, 21)).
'lo.comp.code.asm@asm'('lo.comp.code.code#assem'(XNm, XIns, XLits, XSrcMap), 'lo.comp.code.asm#codeSeg'(XNm, XX40339, XX40345, XX40349)) :- ocall('_empty%1'(XXV79),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.code.asm@genLblTbl'(XIns, 0, XXV79, XX40331),
    XLbls = XX40331,
    !,
    ocall('_empty%1'(XXV80),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.comp.code.asm@genLitTbl'(XLits, 0, XXV80, XX40338),
    'lo.comp.code.asm@mnem'(XIns, XLbls, XX40338, 0, XX40339),
    ocall('//%3'(XLits, 'lo.comp.code.asm@$13', XX40345),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    'lo.comp.code.asm@genSrcMap'(XSrcMap, XLbls, XX40349).
'lo.comp.code.asm@asm'(_, _) :- raise_exception('error'("asm", 16, 3, 166)).
'lo.comp.code.asm^genLblTbl'('_call%4'(XV4813, XV4814, XV4815, XV4816), 'lo.comp.code.asm^genLblTbl', _) :- 'lo.comp.code.asm@genLblTbl'(XV4813, XV4814, XV4815, XV4816).
'lo.comp.code.asm^ltOff'('_call%3'(XV4817, XV4818, XV4819), 'lo.comp.code.asm^ltOff', _) :- 'lo.comp.code.asm@ltOff'(XV4817, XV4818, XV4819).
'lo.comp.code.asm^pcGap'('_call%5'(XV4820, XV4821, XV4822, XV4823, XV4824), 'lo.comp.code.asm^pcGap', _) :- 'lo.comp.code.asm@pcGap'(XV4820, XV4821, XV4822, XV4823, XV4824).
'lo.comp.code.asm^mnem'('_call%5'(XV4825, XV4826, XV4827, XV4828, XV4829), 'lo.comp.code.asm^mnem', _) :- 'lo.comp.code.asm@mnem'(XV4825, XV4826, XV4827, XV4828, XV4829).
'lo.comp.code.asm^genLitTbl'('_call%4'(XV4830, XV4831, XV4832, XV4833), 'lo.comp.code.asm^genLitTbl', _) :- 'lo.comp.code.asm@genLitTbl'(XV4830, XV4831, XV4832, XV4833).
'lo.comp.code.asm^genSrcMap'('_call%3'(XV4834, XV4835, XV4836), 'lo.comp.code.asm^genSrcMap', _) :- 'lo.comp.code.asm@genSrcMap'(XV4834, XV4835, XV4836).
'lo.comp.code.asm@$13'('_call%2'('lo.comp.code.code#litrl'(X_2548, XT), XT), 'lo.comp.code.asm@$13', _) :- !.
'lo.comp.code.asm@$13'(_, _, _) :- raise_exception('error'("lambda", 16, 93, 15)).
'lo.comp.code.asm^asm'('_call%2'(XV4837, XV4838), 'lo.comp.code.asm^asm', _) :- 'lo.comp.code.asm@asm'(XV4837, XV4838).
