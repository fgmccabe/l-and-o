'#pkg'("n7o7'()7'n2o2'pkg's'lo.repo.manifest's'1.0.0'n5o5'()5'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.resources'e'*'n2o2'import'e'private'n2o2'pkg's'lo.uri'e'*'n2o2'import'e'private'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.json'e'*'s\"I5'manifest'CT1Uz2'lo.index*map'2St'lo.repo.manifest*manifestEntry't'lo.repo.manifest*manifest''readManifest'FT1t'lo.uri*uri't'lo.repo.manifest*manifest''flushManifest'PT2t'lo.uri*uri't'lo.repo.manifest*manifest''locateInManifest'PT4t'lo.repo.manifest*manifest't'lo.repo*pkg'SS'addToManifest'FT4t'lo.repo.manifest*manifest't'lo.repo*pkg'SSt'lo.repo.manifest*manifest'\"s\"I1'manifest'Yt'lo.repo.manifest*manifest'I0\"n1o1'()1's'manifest'n0o0'()0'n2o2'()2'n2o2'()2's'lo.core$display$lo.repo.manifest*manifest's\"c'lo.core$display'T1t'lo.repo.manifest*manifest'T0\"n2o2'()2's'lo.core$display$lo.repo.manifest*manifestEntry's\"c'lo.core$display'T1t'lo.repo.manifest*manifestEntry'T0\"").
'lo.repo.manifest@init'():- !.
'lo.repo.manifest#manifest'('manifest%1'('lo.repo.manifest@manifest'())):- !.
'lo.repo.manifest#manifestEntry'('manifestEntry%1'('lo.repo.manifest@manifestEntry'())):- !.
'lo.repo.manifest@resourceEntry'(X_5588, 'lo.json#jTxt'(XS), XS):- !.
'lo.repo.manifest@resourceEntry'(_, _, _):- raise_exception('error'("lo.repo.manifest@resourceEntry", 32, 3, 29)).
'lo.repo.manifest@jsonVersion'('()2'("*", 'lo.json#jColl'(XDtl)), '()2'('lo.repo#defltVersion', XXe1928)):- !,
    ocall('///%1'(XXV1951),'lo.collection$ixmap$lo.index*map','lo.collection$ixmap$lo.index*map'),
    ocall('_call%3'(XDtl, 'lo.repo.manifest^resourceEntry', XXe1928),XXV1951,XXV1951).
'lo.repo.manifest@jsonVersion'('()2'(XV, 'lo.json#jColl'(XDtl)), '()2'('lo.repo#vers'(XV), XXe1929)):- !,
    ocall('///%1'(XXV1952),'lo.collection$ixmap$lo.index*map','lo.collection$ixmap$lo.index*map'),
    ocall('_call%3'(XDtl, 'lo.repo.manifest^resourceEntry', XXe1929),XXV1952,XXV1952).
'lo.repo.manifest@jsonVersion'(_, _):- raise_exception('error'("lo.repo.manifest@jsonVersion", 28, 3, 68)).
'lo.repo.manifest@jsonPkgEntry'(XPk, 'lo.json#jColl'(XVs), 'lo.repo.manifest#manifestEntry'(XPk, XXe1931)):- !,
    ocall('pairs%1'(XXV1953),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('//%1'(XXV1954),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    ocall('_call%2'(XVs, XXe1930),XXV1953,XXV1953),
    ocall('_call%3'(XXe1930, 'lo.repo.manifest^jsonVersion', XXe1931),XXV1954,XXV1954).
'lo.repo.manifest@jsonPkgEntry'(_, _, _):- raise_exception('error'("lo.repo.manifest@jsonPkgEntry", 25, 3, 70)).
'lo.repo.manifest@jsonManifest'('lo.json#jColl'(XL), 'lo.repo.manifest#manifest'(XXe1932)):- ocall('///%1'(XXV1955),'lo.collection$ixmap$lo.index*map','lo.collection$ixmap$lo.index*map'),
    ocall('_call%3'(XL, 'lo.repo.manifest^jsonPkgEntry', XXe1932),XXV1955,XXV1955).
'lo.repo.manifest@readManifest'(Xu, Xm):- 'lo.resources@getResource'(Xu, XXd8994),
    'explode'(XXd8994, XXc833),
    'lo.json@parseJson'(XXc833, XStx1344, 'lo.core$stream$lo.core*list', Xj),
    XStx1344 = X_5590,
    ocall('_eof%1'(X_5590),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    X_5589 = XStx1344,
    ocall('disp%1'(XXV1956),'lo.core$display$lo.json*json','lo.core$display$lo.json*json'),
    ocall('_call%2'(Xj, XXe1933),XXV1956,XXV1956),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("check "), 'lo.core#,..'(XXe1933, 'lo.core#,..'('lo.core#ss'(" for manifest"), 'lo.core#[]')))), XXd9001),
    '_logmsg'(XXd9001),
    'lo.repo.manifest@jsonManifest'(Xj, Xm),
    !.
'lo.repo.manifest@readManifest'(_, _):- raise_exception('error'("lo.repo.manifest@readManifest", 13, 3, 126)).
'lo.repo.manifest@versionJson'(XM, '()2'('lo.repo#defltVersion', XR), XXe1935):- !,
    ocall('///%1'(XXV1957),'lo.collection$ixmap$lo.index*map','lo.collection$ixmap$lo.index*map'),
    ocall('_put%1'(XXV1958),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%3'(XR, 'lo.repo.manifest@fun86', XXe1934),XXV1957,XXV1957),
    ocall('_call%4'(XM, "*", 'lo.json#jColl'(XXe1934), XXe1935),XXV1958,XXV1958).
'lo.repo.manifest@versionJson'(XM, '()2'('lo.repo#vers'(XV), XR), XXe1937):- !,
    ocall('///%1'(XXV1959),'lo.collection$ixmap$lo.index*map','lo.collection$ixmap$lo.index*map'),
    ocall('_put%1'(XXV1960),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%3'(XR, 'lo.repo.manifest@fun87', XXe1936),XXV1959,XXV1959),
    ocall('_call%4'(XM, XV, 'lo.json#jColl'(XXe1936), XXe1937),XXV1960,XXV1960).
'lo.repo.manifest@versionJson'(_, _, _):- raise_exception('error'("lo.repo.manifest@versionJson", 41, 3, 70)).
'lo.repo.manifest@entryJson'(XP, 'lo.repo.manifest#manifestEntry'(X_5596, XV), 'lo.json#jColl'(XXe1938)):- !,
    ocall('foldLeft%1'(XXV1962),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_empty%1'(XXV1961),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'('lo.repo.manifest^versionJson', XXV1961, XV, XXe1938),XXV1962,XXV1962).
'lo.repo.manifest@entryJson'(_, _, _):- raise_exception('error'("lo.repo.manifest@entryJson", 38, 3, 68)).
'lo.repo.manifest@manifestJson'('lo.repo.manifest#manifest'(XE), 'lo.json#jColl'(XXe1939)):- !,
    ocall('///%1'(XXV1963),'lo.collection$ixmap$lo.index*map','lo.collection$ixmap$lo.index*map'),
    ocall('_call%3'(XE, 'lo.repo.manifest^entryJson', XXe1939),XXV1963,XXV1963).
'lo.repo.manifest@manifestJson'(_, _):- raise_exception('error'("lo.repo.manifest@manifestJson", 35, 3, 49)).
'lo.repo.manifest@flushManifest'(XRoot, XM):- ocall('disp%1'(XXV1964),'lo.core$display$lo.json*json','lo.core$display$lo.json*json'),
    'lo.repo.manifest@manifestJson'(XM, XXd9011),
    ocall('_call%2'(XXd9011, XXe1940),XXV1964,XXV1964),
    'lo@formatSS'(XXe1940, XXd9012),
    'lo.resources@putResource'(XRoot, XXd9012).
'lo.repo.manifest@showVersion'('()2'(XV, XR), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("    "), 'lo.core#,..'(XXe1941, 'lo.core#,..'('lo.core#ss'("="), 'lo.core#,..'(XXe1942, 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#[]'))))))):- !,
    ocall('disp%1'(XXV1965),'lo.core$display$lo.repo*version','lo.core$display$lo.repo*version'),
    ocall('disp%1'(XXV1966),'lo.core$display$lo.index*map'('lo.core$display$lo.core*string', 'lo.core$display$lo.core*string'),'lo.core$display$lo.index*map'('lo.core$display$lo.core*string', 'lo.core$display$lo.core*string')),
    ocall('_call%2'(XV, XXe1941),XXV1965,XXV1965),
    ocall('_call%2'(XR, XXe1942),XXV1966,XXV1966).
'lo.repo.manifest@showVersion'(_, _):- raise_exception('error'("lo.repo.manifest@showVersion", 64, 3, 74)).
'lo.repo.manifest@showVersions'('lo.core#[]', 'lo.core#[]'):- !.
'lo.repo.manifest@showVersions'('lo.core#,..'(XV, XM), 'lo.core#,..'(XXd9023, XXd9024)):- !,
    'lo.repo.manifest@showVersion'(XV, XXd9023),
    'lo.repo.manifest@showVersions'(XM, XXd9024).
'lo.repo.manifest@showVersions'(_, _):- raise_exception('error'("lo.repo.manifest@showVersions", 60, 3, 22)).
'lo.repo.manifest@showEntry'('lo.repo.manifest#manifestEntry'(XPk, XVersions), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("  "), 'lo.core#,..'('lo.core#ss'(XPk), 'lo.core#,..'('lo.core#ss'(":{
"), 'lo.core#,..'('lo.core#ssSeq'(XXd9029), 'lo.core#,..'('lo.core#ss'("  }
"), 'lo.core#[]'))))))):- !,
    'lo.repo.manifest@showVersions'(XVersions, XXd9029).
'lo.repo.manifest@showEntry'(_, _):- raise_exception('error'("lo.repo.manifest@showEntry", 57, 3, 118)).
'lo.repo.manifest@showEntries'(XM, XXe1944):- !,
    ocall('values%1'(XXV1967),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('//%1'(XXV1968),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    ocall('_call%2'(XM, XXe1943),XXV1967,XXV1967),
    ocall('_call%3'(XXe1943, 'lo.repo.manifest^showEntry', XXe1944),XXV1968,XXV1968).
'lo.repo.manifest@showEntries'(_, _):- raise_exception('error'("lo.repo.manifest@showEntries", 50, 3, 38)).
'lo.core$display$lo.repo.manifest*manifest'('lo.core$display$lo.repo.manifest*manifest%1'('lo.core$display$lo.repo.manifest*manifest')):- !.
'lo.core$display$lo.repo.manifest*manifest'('disp%2'(XV18725, XV18726), XLbl3856, XThis3856):- !,
    'lo.core$display$lo.repo.manifest*manifest@disp'(XV18725, XV18726, XLbl3856, XThis3856).
'lo.core$display$lo.repo.manifest*manifest'('disp%1'('lo.core$display$lo.repo.manifest*manifest^disp'(XLbl3857, XThis3857)), XLbl3857, XThis3857).
'lo.core$display$lo.repo.manifest*manifest@disp'('lo.repo.manifest#manifest'(XE), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("manifest"), 'lo.core#,..'('lo.core#ss'("{
"), 'lo.core#,..'('lo.core#ssSeq'(XXd9041), 'lo.core#,..'('lo.core#ss'("}
"), 'lo.core#[]'))))), XLbV1692, XThV1692):- !,
    'lo.repo.manifest@showEntries'(XE, XXd9041).
'lo.core$display$lo.repo.manifest*manifest@disp'(_, _):- raise_exception('error'("lo.core$display$lo.repo.manifest*manifest@disp", 46, 5, 86)).
'lo.core$display$lo.repo.manifest*manifestEntry'('lo.core$display$lo.repo.manifest*manifestEntry%1'('lo.core$display$lo.repo.manifest*manifestEntry')):- !.
'lo.core$display$lo.repo.manifest*manifestEntry'('disp%2'(XV18729, XV18730), XLbl3858, XThis3858):- !,
    'lo.core$display$lo.repo.manifest*manifestEntry@disp'(XV18729, XV18730, XLbl3858, XThis3858).
'lo.core$display$lo.repo.manifest*manifestEntry'('disp%1'('lo.core$display$lo.repo.manifest*manifestEntry^disp'(XLbl3859, XThis3859)), XLbl3859, XThis3859).
'lo.core$display$lo.repo.manifest*manifestEntry@disp'(XE, XXd9049, XLbV1693, XThV1693):- !,
    'lo.repo.manifest@showEntry'(XE, XXd9049).
'lo.core$display$lo.repo.manifest*manifestEntry@disp'(_, _):- raise_exception('error'("lo.core$display$lo.repo.manifest*manifestEntry@disp", 53, 5, 23)).
'lo.repo.manifest@getVersion'(XVers, XVers, XV, XMap):- 'lo.repo.manifest@one65'(XV, XMap, XVers).
'lo.repo.manifest@getVersion'('lo.repo#defltVersion', XAct, XV, XMap):- 'lo.repo.manifest@one66'(XV, XMap, XAct).
'lo.repo.manifest@locateInManifest'('lo.repo.manifest#manifest'(XEntries), 'lo.repo#pkg'(XPkg, XVers), XKind, XURI):- ocall('present%3'(XEntries, XPkg, 'lo.repo.manifest#manifestEntry'(XPkg, XV)),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.repo.manifest@getVersion'(XVers, X_5613, XV, XMap),
    ocall('present%3'(XMap, XKind, XURI),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')).
'lo.repo.manifest@addToVersion'('lo.core#[]', XV, XK, XU, 'lo.core#,..'('()2'(XV, XXe1945), 'lo.core#[]')):- !,
    ocall('_put%1'(XXV1970),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_empty%1'(XXV1969),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XXV1969, XK, XU, XXe1945),XXV1970,XXV1970).
'lo.repo.manifest@addToVersion'('lo.core#,..'('()2'(XV, XM), XVs), XV, XK, XU, 'lo.core#,..'('()2'(XV, XXe1946), XVs)):- !,
    ocall('_put%1'(XXV1971),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XM, XK, XU, XXe1946),XXV1971,XXV1971).
'lo.repo.manifest@addToVersion'('lo.core#,..'(XVr, XVs), XV, XK, XU, 'lo.core#,..'(XVr, XXd9058)):- !,
    'lo.repo.manifest@addToVersion'(XVs, XV, XK, XU, XXd9058).
'lo.repo.manifest@addToVersion'(_, _, _, _, _):- raise_exception('error'("lo.repo.manifest@addToVersion", 89, 3, 38)).
'lo.repo.manifest@addVersion'('lo.repo.manifest#manifestEntry'(XP, XVs), XV, XK, XU, 'lo.repo.manifest#manifestEntry'(XP, XXd9060)):- !,
    'lo.repo.manifest@addToVersion'(XVs, XV, XK, XU, XXd9060).
'lo.repo.manifest@addVersion'(_, _, _, _, _):- raise_exception('error'("lo.repo.manifest@addVersion", 86, 3, 80)).
'lo.repo.manifest@addToManifest'('lo.repo.manifest#manifest'(XM), 'lo.repo#pkg'(XP, XV), XKind, XUri, 'lo.repo.manifest#manifest'(XMM)):- ocall('present%3'(XM, XP, XEntry),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_put%1'(XXV1972),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.repo.manifest@addVersion'(XEntry, XV, XKind, XUri, XXd9063),
    ocall('_call%4'(XM, XP, XXd9063, XXe1947),XXV1972,XXV1972),
    XMM = XXe1947,
    !.
'lo.repo.manifest@addToManifest'('lo.repo.manifest#manifest'(XM), 'lo.repo#pkg'(XP, XV), XKind, XUri, 'lo.repo.manifest#manifest'(XMM)):- ocall('_put%1'(XXV1973),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    'lo.repo.manifest@addVersion'('lo.repo.manifest#manifestEntry'(XP, 'lo.core#[]'), XV, XKind, XUri, XXd9067),
    ocall('_call%4'(XM, XP, XXd9067, XXe1948),XXV1973,XXV1973),
    XMM = XXe1948,
    !.
'lo.repo.manifest@addToManifest'(_, _, _, _, _):- raise_exception('error'("lo.repo.manifest@addToManifest", 77, 3, 131)).
'lo.repo.manifest^resourceEntry'('_call%3'(XV18693, XV18694, XV18695), 'lo.repo.manifest^resourceEntry', _):- 'lo.repo.manifest@resourceEntry'(XV18693, XV18694, XV18695).
'lo.repo.manifest^jsonVersion'('_call%2'(XV18696, XV18697), 'lo.repo.manifest^jsonVersion', _):- 'lo.repo.manifest@jsonVersion'(XV18696, XV18697).
'lo.repo.manifest^jsonPkgEntry'('_call%3'(XV18698, XV18699, XV18700), 'lo.repo.manifest^jsonPkgEntry', _):- 'lo.repo.manifest@jsonPkgEntry'(XV18698, XV18699, XV18700).
'lo.repo.manifest^jsonManifest'('_call%2'(XV18701, XV18702), 'lo.repo.manifest^jsonManifest', _):- 'lo.repo.manifest@jsonManifest'(XV18701, XV18702).
'lo.repo.manifest^readManifest'('_call%2'(XV18703, XV18704), 'lo.repo.manifest^readManifest', _):- 'lo.repo.manifest@readManifest'(XV18703, XV18704).
'lo.repo.manifest@fun86'('_call%3'(X_5594, XS, 'lo.json#jTxt'(XS)), 'lo.repo.manifest@fun86', _):- !.
'lo.repo.manifest@fun86'(_, _, _):- raise_exception('error'("lo.repo.manifest@fun86", 41, 56, 14)).
'lo.repo.manifest@fun87'('_call%3'(X_5595, XS, 'lo.json#jTxt'(XS)), 'lo.repo.manifest@fun87', _):- !.
'lo.repo.manifest@fun87'(_, _, _):- raise_exception('error'("lo.repo.manifest@fun87", 42, 49, 14)).
'lo.repo.manifest^versionJson'('_call%3'(XV18705, XV18706, XV18707), 'lo.repo.manifest^versionJson', _):- 'lo.repo.manifest@versionJson'(XV18705, XV18706, XV18707).
'lo.repo.manifest^entryJson'('_call%3'(XV18708, XV18709, XV18710), 'lo.repo.manifest^entryJson', _):- 'lo.repo.manifest@entryJson'(XV18708, XV18709, XV18710).
'lo.repo.manifest^manifestJson'('_call%2'(XV18711, XV18712), 'lo.repo.manifest^manifestJson', _):- 'lo.repo.manifest@manifestJson'(XV18711, XV18712).
'lo.repo.manifest^flushManifest'('_call%2'(XV18713, XV18714), 'lo.repo.manifest^flushManifest', _):- 'lo.repo.manifest@flushManifest'(XV18713, XV18714).
'lo.repo.manifest^showVersion'('_call%2'(XV18715, XV18716), 'lo.repo.manifest^showVersion', _):- 'lo.repo.manifest@showVersion'(XV18715, XV18716).
'lo.repo.manifest^showVersions'('_call%2'(XV18717, XV18718), 'lo.repo.manifest^showVersions', _):- 'lo.repo.manifest@showVersions'(XV18717, XV18718).
'lo.repo.manifest^showEntry'('_call%2'(XV18719, XV18720), 'lo.repo.manifest^showEntry', _):- 'lo.repo.manifest@showEntry'(XV18719, XV18720).
'lo.repo.manifest^showEntries'('_call%2'(XV18721, XV18722), 'lo.repo.manifest^showEntries', _):- 'lo.repo.manifest@showEntries'(XV18721, XV18722).
'lo.core$display$lo.repo.manifest*manifest^disp'('_call%2'(XV18723, XV18724), 'lo.core$display$lo.repo.manifest*manifest^disp'(XLbV1692, XThV1692), _):- 'lo.core$display$lo.repo.manifest*manifest@disp'(XV18723, XV18724, XLbV1692, XThV1692).
'lo.core$display$lo.repo.manifest*manifestEntry^disp'('_call%2'(XV18727, XV18728), 'lo.core$display$lo.repo.manifest*manifestEntry^disp'(XLbV1693, XThV1693), _):- 'lo.core$display$lo.repo.manifest*manifestEntry@disp'(XV18727, XV18728, XLbV1693, XThV1693).
'lo.repo.manifest@one65'(XV, XMap, XVers):- ocall('in%2'('()2'(XVers, XMap), XV),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !.
'lo.repo.manifest@one66'(XV, XMap, XAct):- ocall('in%2'('()2'(XAct, XMap), XV),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    !.
'lo.repo.manifest^getVersion'('_call%4'(XV18731, XV18732, XV18733, XV18734), 'lo.repo.manifest^getVersion', _):- 'lo.repo.manifest@getVersion'(XV18731, XV18732, XV18733, XV18734).
'lo.repo.manifest^locateInManifest'('_call%4'(XV18735, XV18736, XV18737, XV18738), 'lo.repo.manifest^locateInManifest', _):- 'lo.repo.manifest@locateInManifest'(XV18735, XV18736, XV18737, XV18738).
'lo.repo.manifest^addToVersion'('_call%5'(XV18739, XV18740, XV18741, XV18742, XV18743), 'lo.repo.manifest^addToVersion', _):- 'lo.repo.manifest@addToVersion'(XV18739, XV18740, XV18741, XV18742, XV18743).
'lo.repo.manifest^addVersion'('_call%5'(XV18744, XV18745, XV18746, XV18747, XV18748), 'lo.repo.manifest^addVersion', _):- 'lo.repo.manifest@addVersion'(XV18744, XV18745, XV18746, XV18747, XV18748).
'lo.repo.manifest^addToManifest'('_call%5'(XV18749, XV18750, XV18751, XV18752, XV18753), 'lo.repo.manifest^addToManifest', _):- 'lo.repo.manifest@addToManifest'(XV18749, XV18750, XV18751, XV18752, XV18753).
