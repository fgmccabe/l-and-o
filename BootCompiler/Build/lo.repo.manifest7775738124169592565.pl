'#pkg'("n7o7'()7'n2o2'pkg's'lo.repo.manifest'e'*'n14o14'()14'n2o2'import'e'private'n2o2'pkg's'lo.json'e'*'n2o2'import'e'private'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'n2o2'import'e'private'n2o2'pkg's'lo.resources'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I5'manifest'CT1Uz2'lo.index*map'2St'lo.repo.manifest*manifestEntry't'lo.repo.manifest*manifest''readManifest'FT1t'lo.uri*uri't'lo.repo.manifest*manifest''flushManifest'PT2t'lo.uri*uri't'lo.repo.manifest*manifest''locateInManifest'PT4t'lo.repo.manifest*manifest't'lo.repo*pkg'SS'addToManifest'FT4t'lo.repo.manifest*manifest't'lo.repo*pkg'SSt'lo.repo.manifest*manifest'\"s\"I1'manifest'Yt'lo.repo.manifest*manifest'I0\"n0o0'()0'n0o0'()0'n2o2'()2'n2o2'()2's'lo.core$display$lo.repo.manifest*manifest's\"c'lo.core$display'T1t'lo.repo.manifest*manifest'T0\"n2o2'()2's'lo.core$display$lo.repo.manifest*manifestEntry's\"c'lo.core$display'T1t'lo.repo.manifest*manifestEntry'T0\"").
'lo.repo.manifest@init'() :- !.
'lo.repo.manifest#manifest'('manifest%1'('lo.repo.manifest@manifest'())) :- !.
'lo.repo.manifest#manifestEntry'('manifestEntry%1'('lo.repo.manifest@manifestEntry'())) :- !.
'lo.repo.manifest@resourceEntry'(X_1530, 'lo.json#jTxt'(XS), XS) :- !.
'lo.repo.manifest@resourceEntry'(_, _, _) :- raise_exception('error'("resourceEntry", 32, 3, 29)).
'lo.repo.manifest@jsonVersion'(("*", 'lo.json#jColl'(XDtl)), ('lo.repo#defltVersion', XX22058)) :- !,
    ocall('///%3'(XDtl, 'lo.repo.manifest^resourceEntry', XX22058),'lo.collection$ixmap$lo.index*map','lo.collection$ixmap$lo.index*map').
'lo.repo.manifest@jsonVersion'((XV, 'lo.json#jColl'(XDtl)), ('lo.repo#vers'(XV), XX22067)) :- !,
    ocall('///%3'(XDtl, 'lo.repo.manifest^resourceEntry', XX22067),'lo.collection$ixmap$lo.index*map','lo.collection$ixmap$lo.index*map').
'lo.repo.manifest@jsonVersion'(_, _) :- raise_exception('error'("jsonVersion", 28, 3, 68)).
'lo.repo.manifest@jsonPkgEntry'(XPk, 'lo.json#jColl'(XVs), 'lo.repo.manifest#manifestEntry'(XPk, XX22078)) :- !,
    ocall('pairs%2'(XVs, XX22074),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('//%3'(XX22074, 'lo.repo.manifest^jsonVersion', XX22078),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list').
'lo.repo.manifest@jsonPkgEntry'(_, _, _) :- raise_exception('error'("jsonPkgEntry", 25, 3, 70)).
'lo.repo.manifest@jsonManifest'('lo.json#jColl'(XL), 'lo.repo.manifest#manifest'(XX22085)) :- ocall('///%3'(XL, 'lo.repo.manifest^jsonPkgEntry', XX22085),'lo.collection$ixmap$lo.index*map','lo.collection$ixmap$lo.index*map').
'lo.repo.manifest@readManifest'(Xu, Xm) :- 'lo.resources@getResource'(Xu, XX22090),
    'explode'(XX22090, XX22091),
    'lo.json@parseJson'(XX22091, XStx353, 'lo.core$stream$lo.core*list', Xj),
    ocall('_eof%1'(XStx353),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.repo.manifest@jsonManifest'(Xj, Xm),
    !.
'lo.repo.manifest@readManifest'(_, _) :- raise_exception('error'("readManifest", 13, 3, 128)).
'lo.repo.manifest@versionJson'(XM, ('lo.repo#defltVersion', XR), XX22111) :- !,
    ocall('///%3'(XR, 'lo.repo.manifest@$7', XX22108),'lo.collection$ixmap$lo.index*map','lo.collection$ixmap$lo.index*map'),
    ocall('_put%4'(XM, "*", 'lo.json#jColl'(XX22108), XX22111),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.repo.manifest@versionJson'(XM, ('lo.repo#vers'(XV), XR), XX22128) :- !,
    ocall('///%3'(XR, 'lo.repo.manifest@$8', XX22125),'lo.collection$ixmap$lo.index*map','lo.collection$ixmap$lo.index*map'),
    ocall('_put%4'(XM, XV, 'lo.json#jColl'(XX22125), XX22128),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.repo.manifest@versionJson'(_, _, _) :- raise_exception('error'("versionJson", 41, 3, 70)).
'lo.repo.manifest@entryJson'(XP, 'lo.repo.manifest#manifestEntry'(X_1533, XV), 'lo.json#jColl'(XX22139)) :- !,
    ocall('_empty%1'(XXV39),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('foldLeft%4'('lo.repo.manifest^versionJson', XXV39, XV, XX22139),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list').
'lo.repo.manifest@entryJson'(_, _, _) :- raise_exception('error'("entryJson", 38, 3, 68)).
'lo.repo.manifest@manifestJson'('lo.repo.manifest#manifest'(XE), 'lo.json#jColl'(XX22146)) :- !,
    ocall('///%3'(XE, 'lo.repo.manifest^entryJson', XX22146),'lo.collection$ixmap$lo.index*map','lo.collection$ixmap$lo.index*map').
'lo.repo.manifest@manifestJson'(_, _) :- raise_exception('error'("manifestJson", 35, 3, 49)).
'lo.repo.manifest@flushManifest'(XRoot, XM) :- 'lo.repo.manifest@manifestJson'(XM, XX22153),
    ocall('disp%2'(XX22153, XX22154),'lo.core$display$lo.json*json','lo.core$display$lo.json*json'),
    'lo@formatSS'(XX22154, XX22156),
    'lo.resources@putResource'(XRoot, XX22156).
'lo.repo.manifest@showVersion'((XV, XR), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("    "), 'lo.core#,..'(XX22161, 'lo.core#,..'('lo.core#ss'("="), 'lo.core#,..'(XX22165, 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#[]'))))))) :- !,
    ocall('disp%2'(XV, XX22161),'lo.core$display$lo.repo*version','lo.core$display$lo.repo*version'),
    ocall('disp%2'(XR, XX22165),'lo.core$display$lo.index*map'('lo.core$display$lo.core*string', 'lo.core$display$lo.core*string'),'lo.core$display$lo.index*map'('lo.core$display$lo.core*string', 'lo.core$display$lo.core*string')).
'lo.repo.manifest@showVersion'(_, _) :- raise_exception('error'("showVersion", 64, 3, 74)).
'lo.repo.manifest@showVersions'('lo.core#[]', 'lo.core#[]') :- !.
'lo.repo.manifest@showVersions'('lo.core#,..'(XV, XM), 'lo.core#,..'(XX22183, XX22185)) :- !,
    'lo.repo.manifest@showVersion'(XV, XX22183),
    'lo.repo.manifest@showVersions'(XM, XX22185).
'lo.repo.manifest@showVersions'(_, _) :- raise_exception('error'("showVersions", 60, 3, 22)).
'lo.repo.manifest@showEntry'('lo.repo.manifest#manifestEntry'(XPk, XVersions), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("  "), 'lo.core#,..'('lo.core#ss'(XPk), 'lo.core#,..'('lo.core#ss'(":{
"), 'lo.core#,..'('lo.core#ssSeq'(XX22195), 'lo.core#,..'('lo.core#ss'("  }
"), 'lo.core#[]'))))))) :- !,
    'lo.repo.manifest@showVersions'(XVersions, XX22195).
'lo.repo.manifest@showEntry'(_, _) :- raise_exception('error'("showEntry", 57, 3, 118)).
'lo.repo.manifest@showEntries'(XM, XX22211) :- !,
    ocall('values%2'(XM, XX22207),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('//%3'(XX22207, 'lo.repo.manifest^showEntry', XX22211),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list').
'lo.repo.manifest@showEntries'(_, _) :- raise_exception('error'("showEntries", 50, 3, 38)).
'lo.core$display$lo.repo.manifest*manifest'('lo.core$display$lo.repo.manifest*manifest%1'('lo.core$display$lo.repo.manifest*manifest')) :- !.
'lo.core$display$lo.repo.manifest*manifest'('disp%2'(XV3108, XV3109), XLbl277, XThis277) :- !,
    'lo.core$display$lo.repo.manifest*manifest@disp'(XV3108, XV3109, XLbl277, XThis277).
'lo.core$display$lo.repo.manifest*manifest'('disp%1'('lo.core$display$lo.repo.manifest*manifest^disp'(XLbl278, XThis278)), XLbl278, XThis278).
'lo.core$display$lo.repo.manifest*manifest@disp'('lo.repo.manifest#manifest'(XE), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("manifest"), 'lo.core#,..'('lo.core#ss'("{
"), 'lo.core#,..'('lo.core#ssSeq'(XX22218), 'lo.core#,..'('lo.core#ss'("}
"), 'lo.core#[]'))))), XLbV289, XThV289) :- !,
    'lo.repo.manifest@showEntries'(XE, XX22218).
'lo.core$display$lo.repo.manifest*manifest@disp'(_, _, _, _) :- raise_exception('error'("disp", 46, 5, 86)).
'lo.core$display$lo.repo.manifest*manifestEntry'('lo.core$display$lo.repo.manifest*manifestEntry%1'('lo.core$display$lo.repo.manifest*manifestEntry')) :- !.
'lo.core$display$lo.repo.manifest*manifestEntry'('disp%2'(XV3114, XV3115), XLbl279, XThis279) :- !,
    'lo.core$display$lo.repo.manifest*manifestEntry@disp'(XV3114, XV3115, XLbl279, XThis279).
'lo.core$display$lo.repo.manifest*manifestEntry'('disp%1'('lo.core$display$lo.repo.manifest*manifestEntry^disp'(XLbl280, XThis280)), XLbl280, XThis280).
'lo.core$display$lo.repo.manifest*manifestEntry@disp'(XE, XX22229, XLbV290, XThV290) :- !,
    'lo.repo.manifest@showEntry'(XE, XX22229).
'lo.core$display$lo.repo.manifest*manifestEntry@disp'(_, _, _, _) :- raise_exception('error'("disp", 53, 5, 23)).
'lo.repo.manifest@getVersion'(XVers, XVers, XV, XMap) :- 'lo.repo.manifest@one24'(XV, XMap, XVers).
'lo.repo.manifest@getVersion'('lo.repo#defltVersion', XAct, XV, XMap) :- 'lo.repo.manifest@one25'(XV, XMap, XAct).
'lo.repo.manifest@locateInManifest'('lo.repo.manifest#manifest'(XEntries), 'lo.repo#pkg'(XPkg, XVers), XKind, XURI) :- ocall('present%3'(XEntries, XPkg, 'lo.repo.manifest#manifestEntry'(XPkg, XV)),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.repo.manifest@getVersion'(XVers, X_1534, XV, XMap),
    ocall('present%3'(XMap, XKind, XURI),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.repo.manifest@addToVersion'('lo.core#[]', XV, XK, XU, 'lo.core#,..'((XV, XX22278), 'lo.core#[]')) :- !,
    ocall('_empty%1'(XXV40),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_put%4'(XXV40, XK, XU, XX22278),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.repo.manifest@addToVersion'('lo.core#,..'((XV, XM), XVs), XV, XK, XU, 'lo.core#,..'((XV, XX22294), XVs)) :- !,
    ocall('_put%4'(XM, XK, XU, XX22294),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.repo.manifest@addToVersion'('lo.core#,..'(XVr, XVs), XV, XK, XU, 'lo.core#,..'(XVr, XX22310)) :- !,
    'lo.repo.manifest@addToVersion'(XVs, XV, XK, XU, XX22310).
'lo.repo.manifest@addToVersion'(_, _, _, _, _) :- raise_exception('error'("addToVersion", 89, 3, 38)).
'lo.repo.manifest@addVersion'('lo.repo.manifest#manifestEntry'(XP, XVs), XV, XK, XU, 'lo.repo.manifest#manifestEntry'(XP, XX22323)) :- !,
    'lo.repo.manifest@addToVersion'(XVs, XV, XK, XU, XX22323).
'lo.repo.manifest@addVersion'(_, _, _, _, _) :- raise_exception('error'("addVersion", 86, 3, 80)).
'lo.repo.manifest@addToManifest'('lo.repo.manifest#manifest'(XM), 'lo.repo#pkg'(XP, XV), XKind, XUri, 'lo.repo.manifest#manifest'(XMM)) :- ocall('present%3'(XM, XP, XEntry),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.repo.manifest@addVersion'(XEntry, XV, XKind, XUri, XX22344),
    ocall('_put%4'(XM, XP, XX22344, XX22345),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    XMM = XX22345,
    !.
'lo.repo.manifest@addToManifest'('lo.repo.manifest#manifest'(XM), 'lo.repo#pkg'(XP, XV), XKind, XUri, 'lo.repo.manifest#manifest'(XMM)) :- 'lo.repo.manifest@addVersion'('lo.repo.manifest#manifestEntry'(XP, 'lo.core#[]'), XV, XKind, XUri, XX22366),
    ocall('_put%4'(XM, XP, XX22366, XX22367),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    XMM = XX22367,
    !.
'lo.repo.manifest@addToManifest'(_, _, _, _, _) :- raise_exception('error'("addToManifest", 77, 3, 131)).
'lo.repo.manifest^resourceEntry'('_call%3'(XV3076, XV3077, XV3078), 'lo.repo.manifest^resourceEntry', _) :- 'lo.repo.manifest@resourceEntry'(XV3076, XV3077, XV3078).
'lo.repo.manifest^jsonVersion'('_call%2'(XV3079, XV3080), 'lo.repo.manifest^jsonVersion', _) :- 'lo.repo.manifest@jsonVersion'(XV3079, XV3080).
'lo.repo.manifest^jsonPkgEntry'('_call%3'(XV3081, XV3082, XV3083), 'lo.repo.manifest^jsonPkgEntry', _) :- 'lo.repo.manifest@jsonPkgEntry'(XV3081, XV3082, XV3083).
'lo.repo.manifest^jsonManifest'('_call%2'(XV3084, XV3085), 'lo.repo.manifest^jsonManifest', _) :- 'lo.repo.manifest@jsonManifest'(XV3084, XV3085).
'lo.repo.manifest^readManifest'('_call%2'(XV3086, XV3087), 'lo.repo.manifest^readManifest', _) :- 'lo.repo.manifest@readManifest'(XV3086, XV3087).
'lo.repo.manifest@$7'('_call%3'(X_1531, XS, 'lo.json#jTxt'(XS)), 'lo.repo.manifest@$7', _) :- !.
'lo.repo.manifest@$7'(_, _, _) :- raise_exception('error'("lambda", 41, 56, 14)).
'lo.repo.manifest@$8'('_call%3'(X_1532, XS, 'lo.json#jTxt'(XS)), 'lo.repo.manifest@$8', _) :- !.
'lo.repo.manifest@$8'(_, _, _) :- raise_exception('error'("lambda", 42, 49, 14)).
'lo.repo.manifest^versionJson'('_call%3'(XV3088, XV3089, XV3090), 'lo.repo.manifest^versionJson', _) :- 'lo.repo.manifest@versionJson'(XV3088, XV3089, XV3090).
'lo.repo.manifest^entryJson'('_call%3'(XV3091, XV3092, XV3093), 'lo.repo.manifest^entryJson', _) :- 'lo.repo.manifest@entryJson'(XV3091, XV3092, XV3093).
'lo.repo.manifest^manifestJson'('_call%2'(XV3094, XV3095), 'lo.repo.manifest^manifestJson', _) :- 'lo.repo.manifest@manifestJson'(XV3094, XV3095).
'lo.repo.manifest^flushManifest'('_call%2'(XV3096, XV3097), 'lo.repo.manifest^flushManifest', _) :- 'lo.repo.manifest@flushManifest'(XV3096, XV3097).
'lo.repo.manifest^showVersion'('_call%2'(XV3098, XV3099), 'lo.repo.manifest^showVersion', _) :- 'lo.repo.manifest@showVersion'(XV3098, XV3099).
'lo.repo.manifest^showVersions'('_call%2'(XV3100, XV3101), 'lo.repo.manifest^showVersions', _) :- 'lo.repo.manifest@showVersions'(XV3100, XV3101).
'lo.repo.manifest^showEntry'('_call%2'(XV3102, XV3103), 'lo.repo.manifest^showEntry', _) :- 'lo.repo.manifest@showEntry'(XV3102, XV3103).
'lo.repo.manifest^showEntries'('_call%2'(XV3104, XV3105), 'lo.repo.manifest^showEntries', _) :- 'lo.repo.manifest@showEntries'(XV3104, XV3105).
'lo.core$display$lo.repo.manifest*manifest^disp'('_call%2'(XV3106, XV3107), 'lo.core$display$lo.repo.manifest*manifest^disp'(XLbV289, XThV289), _) :- 'lo.core$display$lo.repo.manifest*manifest@disp'(XV3106, XV3107, XLbV289, XThV289).
'lo.core$display$lo.repo.manifest*manifest^disp'('_call%2'(XV3110, XV3111), 'lo.core$display$lo.repo.manifest*manifest^disp'(XLbV289, XThV289), _) :- 'lo.core$display$lo.repo.manifest*manifest@disp'(XV3110, XV3111, XLbV289, XThV289).
'lo.core$display$lo.repo.manifest*manifestEntry^disp'('_call%2'(XV3112, XV3113), 'lo.core$display$lo.repo.manifest*manifestEntry^disp'(XLbV290, XThV290), _) :- 'lo.core$display$lo.repo.manifest*manifestEntry@disp'(XV3112, XV3113, XLbV290, XThV290).
'lo.core$display$lo.repo.manifest*manifestEntry^disp'('_call%2'(XV3116, XV3117), 'lo.core$display$lo.repo.manifest*manifestEntry^disp'(XLbV290, XThV290), _) :- 'lo.core$display$lo.repo.manifest*manifestEntry@disp'(XV3116, XV3117, XLbV290, XThV290).
'lo.repo.manifest@one24'(XV, XMap, XVers) :- ocall('in%2'((XVers, XMap), XV),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !.
'lo.repo.manifest@one25'(XV, XMap, XAct) :- ocall('in%2'((XAct, XMap), XV),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !.
'lo.repo.manifest^getVersion'('_call%4'(XV3118, XV3119, XV3120, XV3121), 'lo.repo.manifest^getVersion', _) :- 'lo.repo.manifest@getVersion'(XV3118, XV3119, XV3120, XV3121).
'lo.repo.manifest^locateInManifest'('_call%4'(XV3122, XV3123, XV3124, XV3125), 'lo.repo.manifest^locateInManifest', _) :- 'lo.repo.manifest@locateInManifest'(XV3122, XV3123, XV3124, XV3125).
'lo.repo.manifest^addToVersion'('_call%5'(XV3126, XV3127, XV3128, XV3129, XV3130), 'lo.repo.manifest^addToVersion', _) :- 'lo.repo.manifest@addToVersion'(XV3126, XV3127, XV3128, XV3129, XV3130).
'lo.repo.manifest^addVersion'('_call%5'(XV3131, XV3132, XV3133, XV3134, XV3135), 'lo.repo.manifest^addVersion', _) :- 'lo.repo.manifest@addVersion'(XV3131, XV3132, XV3133, XV3134, XV3135).
'lo.repo.manifest^addToManifest'('_call%5'(XV3136, XV3137, XV3138, XV3139, XV3140), 'lo.repo.manifest^addToManifest', _) :- 'lo.repo.manifest@addToManifest'(XV3136, XV3137, XV3138, XV3139, XV3140).
