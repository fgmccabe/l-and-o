'#pkg'("n7o7'()7'n2o2'pkg's'lo.repo.manifest's'1.0.0'n5o5'()5'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.resources'e'*'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'n2o2'import'e'private'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.json'e'*'s\"I5'manifest'CT1Uz2'lo.index*map'2St'lo.repo.manifest*manifestEntry't'lo.repo.manifest*manifest''readManifest'FT1t'lo.uri*uri't'lo.repo.manifest*manifest''flushManifest'PT2t'lo.uri*uri't'lo.repo.manifest*manifest''locateInManifest'PT4t'lo.repo.manifest*manifest't'lo.repo*pkg'SS'addToManifest'FT4t'lo.repo.manifest*manifest't'lo.repo*pkg'SSt'lo.repo.manifest*manifest'\"s\"I1'manifest'Yt'lo.repo.manifest*manifest'I0\"n1o1'()1's'manifest'n0o0'()0'n2o2'()2'n2o2'()2's'lo.core$display$lo.repo.manifest*manifest's\"c'lo.core$display'T1t'lo.repo.manifest*manifest'T0\"n2o2'()2's'lo.core$display$lo.repo.manifest*manifestEntry's\"c'lo.core$display'T1t'lo.repo.manifest*manifestEntry'T0\"").
'lo.repo.manifest@init'():- !.
'lo.repo.manifest#manifest'('manifest%1'('lo.repo.manifest@manifest'())):- !.
'lo.repo.manifest#manifestEntry'('manifestEntry%1'('lo.repo.manifest@manifestEntry'())):- !.
'lo.repo.manifest@resourceEntry'(X_34917, 'lo.json#jTxt'(XS), XS):- !.
'lo.repo.manifest@resourceEntry'(_, _, _):- raise_exception('error'("lo.repo.manifest@resourceEntry", 32, 3, 29)).
'lo.repo.manifest@jsonVersion'('()2'("*", 'lo.json#jColl'(XDtl)), '()2'('lo.repo#defltVersion', XXe5041)):- !,
    ocall('///%1'(XXV5403),'lo.collection$ixmap$lo.index*map','lo.collection$ixmap$lo.index*map'),
    ocall('_call%3'(XDtl, 'lo.repo.manifest^resourceEntry', XXe5041),XXV5403,XXV5403).
'lo.repo.manifest@jsonVersion'('()2'(XV, 'lo.json#jColl'(XDtl)), '()2'('lo.repo#vers'(XV), XXe5042)):- !,
    ocall('///%1'(XXV5404),'lo.collection$ixmap$lo.index*map','lo.collection$ixmap$lo.index*map'),
    ocall('_call%3'(XDtl, 'lo.repo.manifest^resourceEntry', XXe5042),XXV5404,XXV5404).
'lo.repo.manifest@jsonVersion'(_, _):- raise_exception('error'("lo.repo.manifest@jsonVersion", 28, 3, 68)).
'lo.repo.manifest@jsonPkgEntry'(XPk, 'lo.json#jColl'(XVs), 'lo.repo.manifest#manifestEntry'(XPk, XXe5044)):- !,
    ocall('pairs%1'(XXV5405),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('//%1'(XXV5406),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    ocall('_call%2'(XVs, XXe5043),XXV5405,XXV5405),
    ocall('_call%3'(XXe5043, 'lo.repo.manifest^jsonVersion', XXe5044),XXV5406,XXV5406).
'lo.repo.manifest@jsonPkgEntry'(_, _, _):- raise_exception('error'("lo.repo.manifest@jsonPkgEntry", 25, 3, 70)).
'lo.repo.manifest@jsonManifest'('lo.json#jColl'(XL), 'lo.repo.manifest#manifest'(XXe5045)):- ocall('///%1'(XXV5407),'lo.collection$ixmap$lo.index*map','lo.collection$ixmap$lo.index*map'),
    ocall('_call%3'(XL, 'lo.repo.manifest^jsonPkgEntry', XXe5045),XXV5407,XXV5407).
'lo.repo.manifest@readManifest'(Xu, Xm):- 'lo.resources@getResource'(Xu, XXd39670),
    'explode'(XXd39670, XXc529),
    'lo.json@parseJson'(XXc529, XStx1824, 'lo.core$stream$lo.core*list', Xj),
    XStx1824 = X_34919,
    ocall('_eof%1'(X_34919),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    X_34918 = XStx1824,
    ocall('disp%1'(XXV5408),'lo.core$display$lo.json*json','lo.core$display$lo.json*json'),
    ocall('_call%2'(Xj, XXe5046),XXV5408,XXV5408),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("check "), 'lo.core#,..'(XXe5046, 'lo.core#,..'('lo.core#ss'(" for manifest"), 'lo.core#[]')))), XXd39677),
    '_logmsg'(XXd39677),
    'lo.repo.manifest@jsonManifest'(Xj, Xm),
    !.
'lo.repo.manifest@readManifest'(_, _):- raise_exception('error'("lo.repo.manifest@readManifest", 13, 3, 126)).
'lo.repo.manifest@versionJson'(XM, '()2'('lo.repo#defltVersion', XR), XXe5048):- !,
    ocall('///%1'(XXV5409),'lo.collection$ixmap$lo.index*map','lo.collection$ixmap$lo.index*map'),
    ocall('_put%1'(XXV5410),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%3'(XR, 'lo.repo.manifest@fun120', XXe5047),XXV5409,XXV5409),
    ocall('_call%4'(XM, "*", 'lo.json#jColl'(XXe5047), XXe5048),XXV5410,XXV5410).
'lo.repo.manifest@versionJson'(XM, '()2'('lo.repo#vers'(XV), XR), XXe5050):- !,
    ocall('///%1'(XXV5411),'lo.collection$ixmap$lo.index*map','lo.collection$ixmap$lo.index*map'),
    ocall('_put%1'(XXV5412),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%3'(XR, 'lo.repo.manifest@fun121', XXe5049),XXV5411,XXV5411),
    ocall('_call%4'(XM, XV, 'lo.json#jColl'(XXe5049), XXe5050),XXV5412,XXV5412).
'lo.repo.manifest@versionJson'(_, _, _):- raise_exception('error'("lo.repo.manifest@versionJson", 41, 3, 70)).
'lo.repo.manifest@entryJson'(XP, 'lo.repo.manifest#manifestEntry'(X_34925, XV), 'lo.json#jColl'(XXe5051)):- !,
    ocall('foldLeft%1'(XXV5414),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_empty%1'(XXV5413),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'('lo.repo.manifest^versionJson', XXV5413, XV, XXe5051),XXV5414,XXV5414).
'lo.repo.manifest@entryJson'(_, _, _):- raise_exception('error'("lo.repo.manifest@entryJson", 38, 3, 68)).
'lo.repo.manifest@manifestJson'('lo.repo.manifest#manifest'(XE), 'lo.json#jColl'(XXe5052)):- !,
    ocall('///%1'(XXV5415),'lo.collection$ixmap$lo.index*map','lo.collection$ixmap$lo.index*map'),
    ocall('_call%3'(XE, 'lo.repo.manifest^entryJson', XXe5052),XXV5415,XXV5415).
'lo.repo.manifest@manifestJson'(_, _):- raise_exception('error'("lo.repo.manifest@manifestJson", 35, 3, 49)).
'lo.repo.manifest@flushManifest'(XRoot, XM):- ocall('disp%1'(XXV5416),'lo.core$display$lo.json*json','lo.core$display$lo.json*json'),
    'lo.repo.manifest@manifestJson'(XM, XXd39687),
    ocall('_call%2'(XXd39687, XXe5053),XXV5416,XXV5416),
    'lo@formatSS'(XXe5053, XXd39688),
    'lo.resources@putResource'(XRoot, XXd39688).
'lo.repo.manifest@showVersion'('()2'(XV, XR), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("    "), 'lo.core#,..'(XXe5054, 'lo.core#,..'('lo.core#ss'("="), 'lo.core#,..'(XXe5055, 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#[]'))))))):- !,
    ocall('disp%1'(XXV5417),'lo.core$display$lo.repo*version','lo.core$display$lo.repo*version'),
    ocall('disp%1'(XXV5418),'lo.core$display$lo.index*map'('lo.core$display$lo.core*string', 'lo.core$display$lo.core*string'),'lo.core$display$lo.index*map'('lo.core$display$lo.core*string', 'lo.core$display$lo.core*string')),
    ocall('_call%2'(XV, XXe5054),XXV5417,XXV5417),
    ocall('_call%2'(XR, XXe5055),XXV5418,XXV5418).
'lo.repo.manifest@showVersion'(_, _):- raise_exception('error'("lo.repo.manifest@showVersion", 64, 3, 74)).
'lo.repo.manifest@showVersions'('lo.core#[]', 'lo.core#[]'):- !.
'lo.repo.manifest@showVersions'('lo.core#,..'(XV, XM), 'lo.core#,..'(XXd39699, XXd39700)):- !,
    'lo.repo.manifest@showVersion'(XV, XXd39699),
    'lo.repo.manifest@showVersions'(XM, XXd39700).
'lo.repo.manifest@showVersions'(_, _):- raise_exception('error'("lo.repo.manifest@showVersions", 60, 3, 22)).
'lo.repo.manifest@showEntry'('lo.repo.manifest#manifestEntry'(XPk, XVersions), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("  "), 'lo.core#,..'('lo.core#ss'(XPk), 'lo.core#,..'('lo.core#ss'(":{
"), 'lo.core#,..'('lo.core#ssSeq'(XXd39705), 'lo.core#,..'('lo.core#ss'("  }
"), 'lo.core#[]'))))))):- !,
    'lo.repo.manifest@showVersions'(XVersions, XXd39705).
'lo.repo.manifest@showEntry'(_, _):- raise_exception('error'("lo.repo.manifest@showEntry", 57, 3, 118)).
'lo.repo.manifest@showEntries'(XM, XXe5057):- !,
    ocall('values%1'(XXV5419),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('//%1'(XXV5420),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    ocall('_call%2'(XM, XXe5056),XXV5419,XXV5419),
    ocall('_call%3'(XXe5056, 'lo.repo.manifest^showEntry', XXe5057),XXV5420,XXV5420).
'lo.repo.manifest@showEntries'(_, _):- raise_exception('error'("lo.repo.manifest@showEntries", 50, 3, 38)).
'lo.core$display$lo.repo.manifest*manifest'('lo.core$display$lo.repo.manifest*manifest%1'('lo.core$display$lo.repo.manifest*manifest')):- !.
'lo.core$display$lo.repo.manifest*manifest'('disp%2'(XV31543, XV31544), XLbl2211, XThis2211):- !,
    'lo.core$display$lo.repo.manifest*manifest@disp'(XV31543, XV31544, XLbl2211, XThis2211).
'lo.core$display$lo.repo.manifest*manifest'('disp%1'('lo.core$display$lo.repo.manifest*manifest^disp'(XLbl2212, XThis2212)), XLbl2212, XThis2212).
'lo.core$display$lo.repo.manifest*manifest@disp'('lo.repo.manifest#manifest'(XE), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("manifest"), 'lo.core#,..'('lo.core#ss'("{
"), 'lo.core#,..'('lo.core#ssSeq'(XXd39717), 'lo.core#,..'('lo.core#ss'("}
"), 'lo.core#[]'))))), XLbV2538, XThV2538):- !,
    'lo.repo.manifest@showEntries'(XE, XXd39717).
'lo.core$display$lo.repo.manifest*manifest@disp'(_, _):- raise_exception('error'("lo.core$display$lo.repo.manifest*manifest@disp", 46, 5, 86)).
'lo.core$display$lo.repo.manifest*manifestEntry'('lo.core$display$lo.repo.manifest*manifestEntry%1'('lo.core$display$lo.repo.manifest*manifestEntry')):- !.
'lo.core$display$lo.repo.manifest*manifestEntry'('disp%2'(XV31547, XV31548), XLbl2213, XThis2213):- !,
    'lo.core$display$lo.repo.manifest*manifestEntry@disp'(XV31547, XV31548, XLbl2213, XThis2213).
'lo.core$display$lo.repo.manifest*manifestEntry'('disp%1'('lo.core$display$lo.repo.manifest*manifestEntry^disp'(XLbl2214, XThis2214)), XLbl2214, XThis2214).
'lo.core$display$lo.repo.manifest*manifestEntry@disp'(XE, XXd39725, XLbV2539, XThV2539):- !,
    'lo.repo.manifest@showEntry'(XE, XXd39725).
'lo.core$display$lo.repo.manifest*manifestEntry@disp'(_, _):- raise_exception('error'("lo.core$display$lo.repo.manifest*manifestEntry@disp", 53, 5, 23)).
'lo.repo.manifest@getVersion'(XVers, XVers, XV, XMap):- 'lo.repo.manifest@one294'(XV, XMap, XVers).
'lo.repo.manifest@getVersion'('lo.repo#defltVersion', XAct, XV, XMap):- 'lo.repo.manifest@one295'(XV, XMap, XAct).
'lo.repo.manifest@locateInManifest'('lo.repo.manifest#manifest'(XEntries), 'lo.repo#pkg'(XPkg, XVers), XKind, XURI):- ocall('present%3'(XEntries, XPkg, 'lo.repo.manifest#manifestEntry'(XPkg, XV)),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.repo.manifest@getVersion'(XVers, X_34942, XV, XMap),
    ocall('present%3'(XMap, XKind, XURI),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.repo.manifest@addToVersion'('lo.core#[]', XV, XK, XU, 'lo.core#,..'('()2'(XV, XXe5058), 'lo.core#[]')):- !,
    ocall('_put%1'(XXV5422),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_empty%1'(XXV5421),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XXV5421, XK, XU, XXe5058),XXV5422,XXV5422).
'lo.repo.manifest@addToVersion'('lo.core#,..'('()2'(XV, XM), XVs), XV, XK, XU, 'lo.core#,..'('()2'(XV, XXe5059), XVs)):- !,
    ocall('_put%1'(XXV5423),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XM, XK, XU, XXe5059),XXV5423,XXV5423).
'lo.repo.manifest@addToVersion'('lo.core#,..'(XVr, XVs), XV, XK, XU, 'lo.core#,..'(XVr, XXd39734)):- !,
    'lo.repo.manifest@addToVersion'(XVs, XV, XK, XU, XXd39734).
'lo.repo.manifest@addToVersion'(_, _, _, _, _):- raise_exception('error'("lo.repo.manifest@addToVersion", 89, 3, 38)).
'lo.repo.manifest@addVersion'('lo.repo.manifest#manifestEntry'(XP, XVs), XV, XK, XU, 'lo.repo.manifest#manifestEntry'(XP, XXd39736)):- !,
    'lo.repo.manifest@addToVersion'(XVs, XV, XK, XU, XXd39736).
'lo.repo.manifest@addVersion'(_, _, _, _, _):- raise_exception('error'("lo.repo.manifest@addVersion", 86, 3, 80)).
'lo.repo.manifest@addToManifest'('lo.repo.manifest#manifest'(XM), 'lo.repo#pkg'(XP, XV), XKind, XUri, 'lo.repo.manifest#manifest'(XMM)):- ocall('present%3'(XM, XP, XEntry),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_put%1'(XXV5424),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.repo.manifest@addVersion'(XEntry, XV, XKind, XUri, XXd39739),
    ocall('_call%4'(XM, XP, XXd39739, XXe5060),XXV5424,XXV5424),
    XMM = XXe5060,
    !.
'lo.repo.manifest@addToManifest'('lo.repo.manifest#manifest'(XM), 'lo.repo#pkg'(XP, XV), XKind, XUri, 'lo.repo.manifest#manifest'(XMM)):- ocall('_put%1'(XXV5425),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.repo.manifest@addVersion'('lo.repo.manifest#manifestEntry'(XP, 'lo.core#[]'), XV, XKind, XUri, XXd39743),
    ocall('_call%4'(XM, XP, XXd39743, XXe5061),XXV5425,XXV5425),
    XMM = XXe5061,
    !.
'lo.repo.manifest@addToManifest'(_, _, _, _, _):- raise_exception('error'("lo.repo.manifest@addToManifest", 77, 3, 131)).
'lo.repo.manifest^resourceEntry'('_call%3'(XV31511, XV31512, XV31513), 'lo.repo.manifest^resourceEntry', _):- 'lo.repo.manifest@resourceEntry'(XV31511, XV31512, XV31513).
'lo.repo.manifest^jsonVersion'('_call%2'(XV31514, XV31515), 'lo.repo.manifest^jsonVersion', _):- 'lo.repo.manifest@jsonVersion'(XV31514, XV31515).
'lo.repo.manifest^jsonPkgEntry'('_call%3'(XV31516, XV31517, XV31518), 'lo.repo.manifest^jsonPkgEntry', _):- 'lo.repo.manifest@jsonPkgEntry'(XV31516, XV31517, XV31518).
'lo.repo.manifest^jsonManifest'('_call%2'(XV31519, XV31520), 'lo.repo.manifest^jsonManifest', _):- 'lo.repo.manifest@jsonManifest'(XV31519, XV31520).
'lo.repo.manifest^readManifest'('_call%2'(XV31521, XV31522), 'lo.repo.manifest^readManifest', _):- 'lo.repo.manifest@readManifest'(XV31521, XV31522).
'lo.repo.manifest@fun120'('_call%3'(X_34923, XS, 'lo.json#jTxt'(XS)), 'lo.repo.manifest@fun120', _):- !.
'lo.repo.manifest@fun120'(_, _, _):- raise_exception('error'("lo.repo.manifest@fun120", 41, 56, 14)).
'lo.repo.manifest@fun121'('_call%3'(X_34924, XS, 'lo.json#jTxt'(XS)), 'lo.repo.manifest@fun121', _):- !.
'lo.repo.manifest@fun121'(_, _, _):- raise_exception('error'("lo.repo.manifest@fun121", 42, 49, 14)).
'lo.repo.manifest^versionJson'('_call%3'(XV31523, XV31524, XV31525), 'lo.repo.manifest^versionJson', _):- 'lo.repo.manifest@versionJson'(XV31523, XV31524, XV31525).
'lo.repo.manifest^entryJson'('_call%3'(XV31526, XV31527, XV31528), 'lo.repo.manifest^entryJson', _):- 'lo.repo.manifest@entryJson'(XV31526, XV31527, XV31528).
'lo.repo.manifest^manifestJson'('_call%2'(XV31529, XV31530), 'lo.repo.manifest^manifestJson', _):- 'lo.repo.manifest@manifestJson'(XV31529, XV31530).
'lo.repo.manifest^flushManifest'('_call%2'(XV31531, XV31532), 'lo.repo.manifest^flushManifest', _):- 'lo.repo.manifest@flushManifest'(XV31531, XV31532).
'lo.repo.manifest^showVersion'('_call%2'(XV31533, XV31534), 'lo.repo.manifest^showVersion', _):- 'lo.repo.manifest@showVersion'(XV31533, XV31534).
'lo.repo.manifest^showVersions'('_call%2'(XV31535, XV31536), 'lo.repo.manifest^showVersions', _):- 'lo.repo.manifest@showVersions'(XV31535, XV31536).
'lo.repo.manifest^showEntry'('_call%2'(XV31537, XV31538), 'lo.repo.manifest^showEntry', _):- 'lo.repo.manifest@showEntry'(XV31537, XV31538).
'lo.repo.manifest^showEntries'('_call%2'(XV31539, XV31540), 'lo.repo.manifest^showEntries', _):- 'lo.repo.manifest@showEntries'(XV31539, XV31540).
'lo.core$display$lo.repo.manifest*manifest^disp'('_call%2'(XV31541, XV31542), 'lo.core$display$lo.repo.manifest*manifest^disp'(XLbV2538, XThV2538), _):- 'lo.core$display$lo.repo.manifest*manifest@disp'(XV31541, XV31542, XLbV2538, XThV2538).
'lo.core$display$lo.repo.manifest*manifestEntry^disp'('_call%2'(XV31545, XV31546), 'lo.core$display$lo.repo.manifest*manifestEntry^disp'(XLbV2539, XThV2539), _):- 'lo.core$display$lo.repo.manifest*manifestEntry@disp'(XV31545, XV31546, XLbV2539, XThV2539).
'lo.repo.manifest@one294'(XV, XMap, XVers):- ocall('in%2'('()2'(XVers, XMap), XV),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !.
'lo.repo.manifest@one295'(XV, XMap, XAct):- ocall('in%2'('()2'(XAct, XMap), XV),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !.
'lo.repo.manifest^getVersion'('_call%4'(XV31549, XV31550, XV31551, XV31552), 'lo.repo.manifest^getVersion', _):- 'lo.repo.manifest@getVersion'(XV31549, XV31550, XV31551, XV31552).
'lo.repo.manifest^locateInManifest'('_call%4'(XV31553, XV31554, XV31555, XV31556), 'lo.repo.manifest^locateInManifest', _):- 'lo.repo.manifest@locateInManifest'(XV31553, XV31554, XV31555, XV31556).
'lo.repo.manifest^addToVersion'('_call%5'(XV31557, XV31558, XV31559, XV31560, XV31561), 'lo.repo.manifest^addToVersion', _):- 'lo.repo.manifest@addToVersion'(XV31557, XV31558, XV31559, XV31560, XV31561).
'lo.repo.manifest^addVersion'('_call%5'(XV31562, XV31563, XV31564, XV31565, XV31566), 'lo.repo.manifest^addVersion', _):- 'lo.repo.manifest@addVersion'(XV31562, XV31563, XV31564, XV31565, XV31566).
'lo.repo.manifest^addToManifest'('_call%5'(XV31567, XV31568, XV31569, XV31570, XV31571), 'lo.repo.manifest^addToManifest', _):- 'lo.repo.manifest@addToManifest'(XV31567, XV31568, XV31569, XV31570, XV31571).
