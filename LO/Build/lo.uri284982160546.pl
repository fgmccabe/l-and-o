'#pkg'("n7o7'()7'n2o2'pkg's'lo.uri's'1.0.0'n1o1'()1'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I16'absUri'CT3St'lo.uri*rsrcName't'lo.uri*query't'lo.uri*uri''relUri'CT2t'lo.uri*rsrcName't'lo.uri*query't'lo.uri*uri''netRsrc'CT2t'lo.uri*authority't'lo.uri*resourcePath't'lo.uri*rsrcName''localRsrc'CT1t'lo.uri*resourcePath't'lo.uri*rsrcName''absPath'CT1LSt'lo.uri*resourcePath''relPath'CT1LSt'lo.uri*resourcePath''server'CT2Uz1'lo.core*option'1t'lo.uri*userInfo't'lo.uri*host't'lo.uri*authority''user'CT1St'lo.uri*userInfo''hostPort'CT2SSt'lo.uri*host''host'CT1St'lo.uri*host''qry'CT1St'lo.uri*query''noQ't'lo.uri*query''uriParse'GT1t'lo.uri*uri'Li'parseUri'FT1St'lo.uri*uri''resolveUri'FT2t'lo.uri*uri't'lo.uri*uri't'lo.uri*uri''getUriPath'FT1t'lo.uri*uri'S\"s\"I7'uri'Yt'lo.uri*uri'I0'query'Yt'lo.uri*query'I0'rsrcName'Yt'lo.uri*rsrcName'I0'resourcePath'Yt'lo.uri*resourcePath'I0'authority'Yt'lo.uri*authority'I0'host'Yt'lo.uri*host'I0'userInfo'Yt'lo.uri*userInfo'I0\"n12o12'()12's'absUri's'relUri's'netRsrc's'localRsrc's'absPath's'relPath's'server's'user's'hostPort's'host's'qry's'noQ'n0o0'()0'n2o2'()2'n2o2'()2's'lo.core$display$lo.uri*uri's\"c'lo.core$display'T1t'lo.uri*uri'T0\"n2o2'()2's'lo.coerce$coercion$lo.uri*uri$lo.core*string's\"c'lo.coerce$coercion'T2t'lo.uri*uri'ST0\"").
'lo.uri@init'():- !.
'lo.uri#absUri'('absUri%1'('lo.uri@absUri'())):- !.
'lo.uri#relUri'('relUri%1'('lo.uri@relUri'())):- !.
'lo.uri#netRsrc'('netRsrc%1'('lo.uri@netRsrc'())):- !.
'lo.uri#localRsrc'('localRsrc%1'('lo.uri@localRsrc'())):- !.
'lo.uri#absPath'('absPath%1'('lo.uri@absPath'())):- !.
'lo.uri#relPath'('relPath%1'('lo.uri@relPath'())):- !.
'lo.uri#server'('server%1'('lo.uri@server'())):- !.
'lo.uri#user'('user%1'('lo.uri@user'())):- !.
'lo.uri#hostPort'('hostPort%1'('lo.uri@hostPort'())):- !.
'lo.uri#host'('host%1'('lo.uri@host'())):- !.
'lo.uri#qry'('qry%1'('lo.uri@qry'())):- !.
'lo.uri#noQ'('noQ%1'('lo.uri@noQ')):- !.
'lo.uri@pChar'(58).
'lo.uri@pChar'(64).
'lo.uri@pChar'(38).
'lo.uri@pChar'(61).
'lo.uri@pChar'(43).
'lo.uri@pChar'(36).
'lo.uri@pChar'(60).
'lo.uri@isHexDigit'(97).
'lo.uri@isHexDigit'(98).
'lo.uri@isHexDigit'(99).
'lo.uri@isHexDigit'(100).
'lo.uri@isHexDigit'(101).
'lo.uri@isHexDigit'(102).
'lo.uri@isHexDigit'(65).
'lo.uri@isHexDigit'(66).
'lo.uri@isHexDigit'(67).
'lo.uri@isHexDigit'(68).
'lo.uri@isHexDigit'(69).
'lo.uri@isHexDigit'(70).
'lo.uri@isDigit'(48).
'lo.uri@isDigit'(49).
'lo.uri@isDigit'(50).
'lo.uri@isDigit'(51).
'lo.uri@isDigit'(52).
'lo.uri@isDigit'(53).
'lo.uri@isDigit'(54).
'lo.uri@isDigit'(55).
'lo.uri@isDigit'(56).
'lo.uri@isDigit'(57).
'lo.uri@digit'(XStIn1233, XNStrm1009, XX):- ocall('_hdtl%3'(XStIn1233, XX, XNStrm1009),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@isDigit'(XX).
'lo.uri@hex'(XStIn1234, XStx1262, XC):- 'lo.uri@digit'(XStIn1234, XStx1262, XC).
'lo.uri@hex'(XStIn1235, XNStrm1010, XC):- ocall('_hdtl%3'(XStIn1235, XC, XNStrm1010),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@isHexDigit'(XC).
'lo.uri@escaped'(XStIn1236, XStx1264, XU, XL):- ocall('_hdtl%3'(XStIn1236, 37, XNStrm1011),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@hex'(XNStrm1011, XStx1263, XU),
    'lo.uri@hex'(XStx1263, XStx1264, XL).
'lo.uri@isMark'(45).
'lo.uri@isMark'(95).
'lo.uri@isMark'(46).
'lo.uri@isMark'(33).
'lo.uri@isMark'(126).
'lo.uri@isMark'(42).
'lo.uri@isMark'(39).
'lo.uri@isMark'(40).
'lo.uri@isMark'(41).
'lo.uri@mark'(XStIn1237, XNStrm1012, XC):- ocall('_hdtl%3'(XStIn1237, XC, XNStrm1012),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@isMark'(XC).
'lo.uri@isUpAlpha'(65).
'lo.uri@isUpAlpha'(66).
'lo.uri@isUpAlpha'(67).
'lo.uri@isUpAlpha'(68).
'lo.uri@isUpAlpha'(69).
'lo.uri@isUpAlpha'(70).
'lo.uri@isUpAlpha'(71).
'lo.uri@isUpAlpha'(72).
'lo.uri@isUpAlpha'(73).
'lo.uri@isUpAlpha'(74).
'lo.uri@isUpAlpha'(75).
'lo.uri@isUpAlpha'(76).
'lo.uri@isUpAlpha'(77).
'lo.uri@isUpAlpha'(78).
'lo.uri@isUpAlpha'(79).
'lo.uri@isUpAlpha'(80).
'lo.uri@isUpAlpha'(81).
'lo.uri@isUpAlpha'(82).
'lo.uri@isUpAlpha'(83).
'lo.uri@isUpAlpha'(84).
'lo.uri@isUpAlpha'(85).
'lo.uri@isUpAlpha'(86).
'lo.uri@isUpAlpha'(87).
'lo.uri@isUpAlpha'(88).
'lo.uri@isUpAlpha'(89).
'lo.uri@isUpAlpha'(90).
'lo.uri@isLowAlpha'(97).
'lo.uri@isLowAlpha'(98).
'lo.uri@isLowAlpha'(99).
'lo.uri@isLowAlpha'(100).
'lo.uri@isLowAlpha'(101).
'lo.uri@isLowAlpha'(102).
'lo.uri@isLowAlpha'(103).
'lo.uri@isLowAlpha'(104).
'lo.uri@isLowAlpha'(105).
'lo.uri@isLowAlpha'(106).
'lo.uri@isLowAlpha'(107).
'lo.uri@isLowAlpha'(108).
'lo.uri@isLowAlpha'(109).
'lo.uri@isLowAlpha'(110).
'lo.uri@isLowAlpha'(111).
'lo.uri@isLowAlpha'(112).
'lo.uri@isLowAlpha'(113).
'lo.uri@isLowAlpha'(114).
'lo.uri@isLowAlpha'(115).
'lo.uri@isLowAlpha'(116).
'lo.uri@isLowAlpha'(117).
'lo.uri@isLowAlpha'(118).
'lo.uri@isLowAlpha'(119).
'lo.uri@isLowAlpha'(120).
'lo.uri@isLowAlpha'(121).
'lo.uri@isLowAlpha'(122).
'lo.uri@alpha'(XStIn1238, XNStrm1013, XC):- ocall('_hdtl%3'(XStIn1238, XC, XNStrm1013),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@or18'(XC).
'lo.uri@alphaNum'(XStIn1239, XStx1265, XC):- 'lo.uri@alpha'(XStIn1239, XStx1265, XC).
'lo.uri@alphaNum'(XStIn1240, XStx1266, XC):- 'lo.uri@digit'(XStIn1240, XStx1266, XC).
'lo.uri@unreserved'(XStIn1241, XStx1267, XC):- 'lo.uri@alphaNum'(XStIn1241, XStx1267, XC).
'lo.uri@unreserved'(XStIn1242, XStx1268, XC):- 'lo.uri@mark'(XStIn1242, XStx1268, XC).
'lo.uri@pChars'(XStIn1243, XStx1270, 'lo.core#,..'(XC, XM), XR):- 'lo.uri@unreserved'(XStIn1243, XStx1269, XC),
    'lo.uri@pChars'(XStx1269, XStx1270, XM, XR).
'lo.uri@pChars'(XStIn1244, XStx1272, 'lo.core#,..'(37, 'lo.core#,..'(XU, 'lo.core#,..'(XL, XM))), XR):- 'lo.uri@escaped'(XStIn1244, XStx1271, XU, XL),
    'lo.uri@pChars'(XStx1271, XStx1272, XM, XR).
'lo.uri@pChars'(XStIn1245, XStx1273, 'lo.core#,..'(XC, XM), XR):- ocall('_hdtl%3'(XStIn1245, XC, XNStrm1014),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@pChar'(XC),
    'lo.uri@pChars'(XNStrm1014, XStx1273, XM, XR).
'lo.uri@pChars'(XStIn1246, XStIn1246, XX, XX).
'lo.uri@parameter'(XStIn1247, XStx1274, 'lo.core#,..'(59, XP), XM):- ocall('_hdtl%3'(XStIn1247, 59, XNStrm1015),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@pChars'(XNStrm1015, XStx1274, XP, XM).
'lo.uri@parameters'(XStIn1248, XStx1276, XP):- 'lo.uri@parameter'(XStIn1248, XStx1275, XP, XS),
    'lo.uri@parameters'(XStx1275, XStx1276, XS).
'lo.uri@parameters'(XStIn1249, XStIn1249, 'lo.core#[]').
'lo.uri@segment'(XStIn1250, XStx1278, XXa89):- 'lo.uri@pChars'(XStIn1250, XStx1277, XC, XR),
    'lo.uri@parameters'(XStx1277, XStx1278, XR),
    'implode'(XC, XXa89).
'lo.uri@pathSegments'(XStIn1251, XDjOut91, 'lo.core#,..'(XSeg, XM)):- 'lo.uri@segment'(XStIn1251, XStx1279, XSeg),
    'lo.uri@Disj83'(XStx1279, XDjOut91, XStx1280, XM, XNStrm1016, XNStrm1016, XDjStrm83).
'lo.uri@relativeRsrc'(XStIn1252, XStx1281, 'lo.uri#relPath'(XSegments)):- 'lo.uri@pathSegments'(XStIn1252, XStx1281, XSegments).
'lo.uri@absoluteRsrc'(XStIn1253, XStx1282, 'lo.uri#absPath'(XSegments)):- ocall('_hdtl%3'(XStIn1253, 47, XNStrm1017),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@pathSegments'(XNStrm1017, XStx1282, XSegments).
'lo.uri@userStar'(XStIn1254, XStx1284, 'lo.core#,..'(XC, XS)):- 'lo.uri@unreserved'(XStIn1254, XStx1283, XC),
    'lo.uri@userStar'(XStx1283, XStx1284, XS).
'lo.uri@userStar'(XStIn1255, XStx1286, 'lo.core#,..'(37, 'lo.core#,..'(XU, 'lo.core#,..'(XL, XS)))):- 'lo.uri@escaped'(XStIn1255, XStx1285, XU, XL),
    'lo.uri@userStar'(XStx1285, XStx1286, XS).
'lo.uri@userStar'(XStIn1256, XStx1287, 'lo.core#,..'(36, XS)):- ocall('_hdtl%3'(XStIn1256, 36, XNStrm1018),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@userStar'(XNStrm1018, XStx1287, XS).
'lo.uri@userStar'(XStIn1257, XStx1288, 'lo.core#,..'(44, XS)):- ocall('_hdtl%3'(XStIn1257, 44, XNStrm1019),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@userStar'(XNStrm1019, XStx1288, XS).
'lo.uri@userStar'(XStIn1258, XStx1289, 'lo.core#,..'(59, XS)):- ocall('_hdtl%3'(XStIn1258, 59, XNStrm1020),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@userStar'(XNStrm1020, XStx1289, XS).
'lo.uri@userStar'(XStIn1259, XStx1290, 'lo.core#,..'(58, XS)):- ocall('_hdtl%3'(XStIn1259, 58, XNStrm1021),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@userStar'(XNStrm1021, XStx1290, XS).
'lo.uri@userStar'(XStIn1260, XStx1291, 'lo.core#,..'(38, XS)):- ocall('_hdtl%3'(XStIn1260, 38, XNStrm1022),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@userStar'(XNStrm1022, XStx1291, XS).
'lo.uri@userStar'(XStIn1261, XStx1292, 'lo.core#,..'(61, XS)):- ocall('_hdtl%3'(XStIn1261, 61, XNStrm1023),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@userStar'(XNStrm1023, XStx1292, XS).
'lo.uri@userStar'(XStIn1262, XStx1293, 'lo.core#,..'(43, XS)):- ocall('_hdtl%3'(XStIn1262, 43, XNStrm1024),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@userStar'(XNStrm1024, XStx1293, XS).
'lo.uri@userStar'(XStIn1263, XStIn1263, 'lo.core#[]'):- 'lo.uri@Hed95'(XStIn1263, XNStrm1025, XNStrm1025, XHedStrm95).
'lo.uri@userInfo'(XStIn1264, XStx1294, XU):- 'lo.uri@userStar'(XStIn1264, XStx1294, XU).
'lo.uri@dot'(XStIn1265, XNStrm1026, 46):- ocall('_hdtl%3'(XStIn1265, 45, XNStrm1026),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.uri@minus'(XStIn1266, XNStrm1027, 45):- ocall('_hdtl%3'(XStIn1266, 45, XNStrm1027),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.uri@alphaDashStar'(XStIn1267, XStx1299, 'lo.core#,..'(XC, XS)):- 'lo.uri@Disj86'(XStIn1267, XDjOut92, XStx1298, XStx1297, XDjOut94, XStx1296, XDjOut93, XStx1295, XC),
    'lo.uri@alphaDashStar'(XDjOut92, XStx1299, XS).
'lo.uri@alphaDashStar'(XStIn1268, XStIn1268, 'lo.core#[]').
'lo.uri@hostName'(XStIn1269, XDjOut95, XXa90):- 'lo.uri@One9'(XStIn1269, XDjOut95, XStx1300, XH),
    'implode'(XH, XXa90).
'lo.uri@digits'(XStIn1270, XStx1302, 'lo.core#,..'(XC, XS)):- 'lo.uri@digit'(XStIn1270, XStx1301, XC),
    'lo.uri@digits'(XStx1301, XStx1302, XS).
'lo.uri@digits'(XStIn1271, XStIn1271, 'lo.core#[]'):- 'lo.uri@Neg27'(XStIn1271, XStx1303, X_5537).
'lo.uri@port'(XStIn1272, XStx1304, XXa91):- 'lo.uri@digits'(XStIn1272, XStx1304, XP),
    'implode'(XP, XXa91).
'lo.uri@hostNamePort'(XStIn1273, XStx1306, 'lo.uri#hostPort'(XH, XP)):- 'lo.uri@hostName'(XStIn1273, XStx1305, XH),
    ocall('_hdtl%3'(XStx1305, 58, XNStrm1028),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@port'(XNStrm1028, XStx1306, XP).
'lo.uri@hostNamePort'(XStIn1274, XStx1307, 'lo.uri#host'(XH)):- 'lo.uri@hostName'(XStIn1274, XStx1307, XH),
    'lo.uri@Neg28'(XStx1307, XNStrm1029, XNStrm1029, XNegStrm28).
'lo.uri@authority'(XStIn1275, XStx1309, 'lo.uri#server'('lo.core#some'('lo.uri#user'(XXa92)), XH)):- 'lo.uri@userInfo'(XStIn1275, XStx1308, XU),
    ocall('_hdtl%3'(XStx1308, 64, XNStrm1030),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@hostNamePort'(XNStrm1030, XStx1309, XH),
    'implode'(XU, XXa92).
'lo.uri@authority'(XStIn1276, XStx1310, 'lo.uri#server'('lo.core#none', XH)):- 'lo.uri@hostNamePort'(XStIn1276, XStx1310, XH).
'lo.uri@optAbsolutePath'(XStIn1277, XStx1311, XP):- 'lo.uri@absoluteRsrc'(XStIn1277, XStx1311, XP).
'lo.uri@optAbsolutePath'(XStIn1278, XStIn1278, 'lo.uri#relPath'('lo.core#[]')):- 'lo.uri@Hed96'(XStIn1278, XNStrm1031, XNStrm1031, XHedStrm96).
'lo.uri@optAbsolutePath'(XStIn1279, XStIn1279, 'lo.uri#relPath'('lo.core#[]')):- XStIn1279 = X_5538,
    ocall('_eof%1'(X_5538),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.uri@netPath'(XStIn1280, XStx1313, 'lo.uri#netRsrc'(XA, XP)):- ocall('_hdtl%3'(XStIn1280, 47, XNStrm1032),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm1032, 47, XNStrm1033),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@authority'(XNStrm1033, XStx1312, XA),
    'lo.uri@optAbsolutePath'(XStx1312, XStx1313, XP).
'lo.uri@isReserved'(59).
'lo.uri@isReserved'(47).
'lo.uri@isReserved'(63).
'lo.uri@isReserved'(58).
'lo.uri@isReserved'(64).
'lo.uri@isReserved'(38).
'lo.uri@isReserved'(61).
'lo.uri@isReserved'(43).
'lo.uri@isReserved'(36).
'lo.uri@isReserved'(44).
'lo.uri@reserved'(XStIn1281, XNStrm1034, XC):- ocall('_hdtl%3'(XStIn1281, XC, XNStrm1034),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@isReserved'(XC).
'lo.uri@uric'(XStIn1282, XDjOut96, 'lo.core#,..'(XC, XM), XM):- 'lo.uri@Disj87'(XStIn1282, XDjOut96, XStx1315, XStx1314, XC).
'lo.uri@uric'(XStIn1283, XStx1316, 'lo.core#,..'(37, 'lo.core#,..'(XU, 'lo.core#,..'(XL, XM))), XM):- 'lo.uri@escaped'(XStIn1283, XStx1316, XU, XL).
'lo.uri@uricStar'(XStIn1284, XStx1318, XS):- 'lo.uri@uric'(XStIn1284, XStx1317, XS, XM),
    'lo.uri@uricStar'(XStx1317, XStx1318, XM).
'lo.uri@uricStar'(XStIn1285, XStIn1285, 'lo.core#[]').
'lo.uri@query'(XStIn1286, XStx1319, 'lo.uri#qry'(XXa93)):- ocall('_hdtl%3'(XStIn1286, 63, XNStrm1035),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@uricStar'(XNStrm1035, XStx1319, XQ),
    'implode'(XQ, XXa93).
'lo.uri@query'(XStIn1287, XStIn1287, 'lo.uri#noQ'):- XStIn1287 = X_5543,
    ocall('_eof%1'(X_5543),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.uri@relativeUri'(XStIn1288, XStx1321, XHier, XQuery):- 'lo.uri@netPath'(XStIn1288, XStx1320, XHier),
    'lo.uri@query'(XStx1320, XStx1321, XQuery).
'lo.uri@relativeUri'(XStIn1289, XStx1323, 'lo.uri#localRsrc'(XRsrc), XQuery):- 'lo.uri@absoluteRsrc'(XStIn1289, XStx1322, XRsrc),
    'lo.uri@query'(XStx1322, XStx1323, XQuery).
'lo.uri@relativeUri'(XStIn1290, XStx1325, 'lo.uri#localRsrc'(XRsrc), XQuery):- 'lo.uri@relativeRsrc'(XStIn1290, XStx1324, XRsrc),
    'lo.uri@query'(XStx1324, XStx1325, XQuery).
'lo.uri@plus'(XStIn1291, XNStrm1036, 43):- ocall('_hdtl%3'(XStIn1291, 43, XNStrm1036),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.uri@alphaStar'(XStIn1292, XStx1331, 'lo.core#,..'(XC, XS)):- 'lo.uri@Disj91'(XStIn1292, XDjOut97, XStx1330, XStx1329, XDjOut100, XStx1328, XDjOut99, XStx1327, XDjOut98, XStx1326, XC),
    'lo.uri@alphaStar'(XDjOut97, XStx1331, XS).
'lo.uri@alphaStar'(XStIn1293, XStIn1293, 'lo.core#[]').
'lo.uri@grabScheme'(XStIn1294, XStx1333, 'lo.core#,..'(XC, XS)):- 'lo.uri@alpha'(XStIn1294, XStx1332, XC),
    'lo.uri@alphaStar'(XStx1332, XStx1333, XS).
'lo.uri@scheme'(XStIn1295, XStx1334, XXa94):- 'lo.uri@grabScheme'(XStIn1295, XStx1334, XScheme),
    'lo.uri@Hed97'(XStx1334, XNStrm1037, XNStrm1037, XHedStrm97),
    'implode'(XScheme, XXa94).
'lo.uri@hierPart'(XStIn1296, XStx1336, XHier, XQuery):- 'lo.uri@netPath'(XStIn1296, XStx1335, XHier),
    'lo.uri@query'(XStx1335, XStx1336, XQuery).
'lo.uri@hierPart'(XStIn1297, XStx1338, 'lo.uri#localRsrc'(XRsrc), XQuery):- 'lo.uri@absoluteRsrc'(XStIn1297, XStx1337, XRsrc),
    'lo.uri@query'(XStx1337, XStx1338, XQuery).
'lo.uri@absoluteUri'(XStIn1298, XStx1340, XScheme, XHier, XQuery):- 'lo.uri@scheme'(XStIn1298, XStx1339, XScheme),
    ocall('_hdtl%3'(XStx1339, 58, XNStrm1038),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@hierPart'(XNStrm1038, XStx1340, XHier, XQuery).
'lo.uri@uriParse'(XStIn1299, XStx1341, 'lo.uri#absUri'(XScheme, XHier, XQuery)):- 'lo.uri@absoluteUri'(XStIn1299, XStx1341, XScheme, XHier, XQuery).
'lo.uri@uriParse'(XStIn1300, XStx1342, 'lo.uri#relUri'(XHier, XQuery)):- 'lo.uri@relativeUri'(XStIn1300, XStx1342, XHier, XQuery).
'lo.uri@parseUri'(XS, XU):- 'explode'(XS, XXc831),
    'lo.uri@uriParse'(XXc831, XStx1343, XU),
    XStx1343 = X_5547,
    ocall('_eof%1'(X_5547),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    X_5546 = XStx1343,
    !.
'lo.uri@parseUri'(_, _):- raise_exception('error'("lo.uri@parseUri", 20, 3, 45)).
'lo.uri@isDelim'(60).
'lo.uri@isDelim'(62).
'lo.uri@isDelim'(35).
'lo.uri@isDelim'(37).
'lo.uri@isDelim'(34).
'lo.uri@delim'(XStIn1301, XNStrm1039, XC):- ocall('_hdtl%3'(XStIn1301, XC, XNStrm1039),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@isDelim'(XC).
'lo.uri@edit'('lo.core#,..'(".", XSegs), XR, XXd8905):- !,
    'lo.uri@edit'(XSegs, XR, XXd8905).
'lo.uri@edit'('lo.core#,..'("..", XSegs), 'lo.core#,..'(X_5551, XR), XXd8906):- !,
    'lo.uri@edit'(XSegs, XR, XXd8906).
'lo.uri@edit'(XSegs, XR, XXd8908):- !,
    'lo.list@reverse'(XR, XXd8907),
    'lo.list@<>'(XXd8907, XSegs, XXd8908).
'lo.uri@edit'(_, _, _):- raise_exception('error'("lo.uri@edit", 304, 3, 36)).
'lo.uri@resolvePath'(X_5552, 'lo.uri#netRsrc'(XA, XP), 'lo.uri#netRsrc'(XA, XP)):- !.
'lo.uri@resolvePath'('lo.uri#netRsrc'(XA, X_5553), 'lo.uri#localRsrc'('lo.uri#absPath'(XP)), 'lo.uri#netRsrc'(XA, 'lo.uri#absPath'(XP))):- !.
'lo.uri@resolvePath'('lo.uri#netRsrc'(XA, 'lo.uri#absPath'(XB)), 'lo.uri#localRsrc'('lo.uri#relPath'(XP)), 'lo.uri#netRsrc'(XA, 'lo.uri#absPath'(XXd8914))):- !,
    'lo.list@reverse'(XB, XXd8912),
    'lo.list@drop'(XXd8912, XXd8913),
    'lo.uri@edit'(XP, XXd8913, XXd8914).
'lo.uri@resolvePath'('lo.uri#localRsrc'(X_5554), 'lo.uri#localRsrc'('lo.uri#absPath'(XP)), 'lo.uri#localRsrc'('lo.uri#absPath'(XP))):- !.
'lo.uri@resolvePath'('lo.uri#localRsrc'('lo.uri#absPath'(XB)), 'lo.uri#localRsrc'('lo.uri#relPath'(XP)), 'lo.uri#localRsrc'('lo.uri#absPath'(XXd8921))):- !,
    'lo.list@reverse'(XB, XXd8919),
    'lo.list@drop'(XXd8919, XXd8920),
    'lo.uri@edit'(XP, XXd8920, XXd8921).
'lo.uri@resolvePath'(_, _, _):- raise_exception('error'("lo.uri@resolvePath", 297, 3, 43)).
'lo.uri@resolveUri'(X_5555, XU, XU):- 'lo.uri#absUri'(X_5556, X_5557, X_5558) = XU,
    !.
'lo.uri@resolveUri'('lo.uri#absUri'(XScheme, XBase, X_5559), 'lo.uri#relUri'(XPath, XQuery), 'lo.uri#absUri'(XScheme, XXd8925, XQuery)):- !,
    'lo.uri@resolvePath'(XBase, XPath, XXd8925).
'lo.uri@resolveUri'(_, _, _):- raise_exception('error'("lo.uri@resolveUri", 293, 3, 39)).
'lo.uri@dispSegs'('lo.core#[]', 'lo.core#[]'):- !.
'lo.uri@dispSegs'('lo.core#,..'(XS, 'lo.core#[]'), 'lo.core#,..'('lo.core#ss'(XS), 'lo.core#[]')):- !.
'lo.uri@dispSegs'('lo.core#,..'(XS, XM), 'lo.core#,..'('lo.core#ss'(XS), 'lo.core#,..'('lo.core#ss'("/"), XXd8931))):- !,
    'lo.uri@dispSegs'(XM, XXd8931).
'lo.uri@dispSegs'(_, _):- raise_exception('error'("lo.uri@dispSegs", 338, 3, 18)).
'lo.uri@dispPath'('lo.uri#absPath'(XSegs), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("/"), XXd8935))):- !,
    'lo.uri@dispSegs'(XSegs, XXd8935).
'lo.uri@dispPath'('lo.uri#relPath'(XSegs), 'lo.core#ssSeq'(XXd8938)):- !,
    'lo.uri@dispSegs'(XSegs, XXd8938).
'lo.uri@dispPath'(_, _):- raise_exception('error'("lo.uri@dispPath", 334, 3, 60)).
'lo.uri@dispUser'('lo.uri#user'(XU), 'lo.core#ss'(XU)):- !.
'lo.uri@dispUser'(_, _):- raise_exception('error'("lo.uri@dispUser", 327, 3, 26)).
'lo.uri@dispHost'('lo.uri#hostPort'(XH, XP), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(XH), 'lo.core#,..'('lo.core#ss'(":"), 'lo.core#,..'('lo.core#ss'(XP), 'lo.core#[]'))))):- !.
'lo.uri@dispHost'('lo.uri#host'(XH), 'lo.core#ss'(XH)):- !.
'lo.uri@dispHost'(_, _):- raise_exception('error'("lo.uri@dispHost", 330, 3, 55)).
'lo.uri@dispAuthority'('lo.uri#server'('lo.core#none', XH), XXd8949):- !,
    'lo.uri@dispHost'(XH, XXd8949).
'lo.uri@dispAuthority'('lo.uri#server'('lo.core#some'(XU), XH), 'lo.core#ssSeq'('lo.core#,..'(XXd8950, 'lo.core#,..'('lo.core#ss'("@"), 'lo.core#,..'(XXd8952, 'lo.core#[]'))))):- !,
    'lo.uri@dispUser'(XU, XXd8950),
    'lo.uri@dispHost'(XH, XXd8952).
'lo.uri@dispAuthority'(_, _):- raise_exception('error'("lo.uri@dispAuthority", 323, 3, 44)).
'lo.uri@dispRsrc'('lo.uri#netRsrc'(XH, XP), 'lo.core#ssSeq'('lo.core#,..'(XXd8957, 'lo.core#,..'(XXd8958, 'lo.core#[]')))):- !,
    'lo.uri@dispAuthority'(XH, XXd8957),
    'lo.uri@dispPath'(XP, XXd8958).
'lo.uri@dispRsrc'('lo.uri#localRsrc'(XP), XXd8962):- !,
    'lo.uri@dispPath'(XP, XXd8962).
'lo.uri@dispRsrc'(_, _):- raise_exception('error'("lo.uri@dispRsrc", 319, 3, 63)).
'lo.core$display$lo.uri*uri'('lo.core$display$lo.uri*uri%1'('lo.core$display$lo.uri*uri')):- !.
'lo.core$display$lo.uri*uri'('dispQuery%2'(XV18674, XV18675), XLbl3850, XThis3850):- !,
    'lo.core$display$lo.uri*uri@dispQuery'(XV18674, XV18675, XLbl3850, XThis3850).
'lo.core$display$lo.uri*uri'('dispQuery%1'('lo.core$display$lo.uri*uri^dispQuery'(XLbl3851, XThis3851)), XLbl3851, XThis3851).
'lo.core$display$lo.uri*uri'('disp%2'(XV18678, XV18679), XLbl3852, XThis3852):- !,
    'lo.core$display$lo.uri*uri@disp'(XV18678, XV18679, XLbl3852, XThis3852).
'lo.core$display$lo.uri*uri'('disp%1'('lo.core$display$lo.uri*uri^disp'(XLbl3853, XThis3853)), XLbl3853, XThis3853).
'lo.core$display$lo.uri*uri@dispQuery'('lo.uri#noQ', 'lo.core#ssSeq'('lo.core#[]'), XLbV1688, XThV1688):- !.
'lo.core$display$lo.uri*uri@dispQuery'('lo.uri#qry'(XQ), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("?"), 'lo.core#,..'('lo.core#ss'(XQ), 'lo.core#[]'))), XLbV1688, XThV1688):- !.
'lo.core$display$lo.uri*uri@dispQuery'(_, _):- raise_exception('error'("lo.core$display$lo.uri*uri@dispQuery", 314, 5, 27)).
'lo.core$display$lo.uri*uri@disp'('lo.uri#absUri'(XScheme, XRsrc, XQuery), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(XScheme), 'lo.core#,..'('lo.core#ss'(":"), 'lo.core#,..'(XXd8971, 'lo.core#,..'(XXd8972, 'lo.core#[]'))))), XLbV1688, XThV1688):- !,
    'lo.uri@dispRsrc'(XRsrc, XXd8971),
    'lo.core$display$lo.uri*uri@dispQuery'(XQuery, XXd8972, XLbV1688, XThV1688).
'lo.core$display$lo.uri*uri@disp'('lo.uri#relUri'(XRsrc, XQuery), 'lo.core#ssSeq'('lo.core#,..'(XXd8978, 'lo.core#,..'(XXd8979, 'lo.core#[]'))), XLbV1688, XThV1688):- !,
    'lo.uri@dispRsrc'(XRsrc, XXd8978),
    'lo.core$display$lo.uri*uri@dispQuery'(XQuery, XXd8979, XLbV1688, XThV1688).
'lo.core$display$lo.uri*uri@disp'(_, _):- raise_exception('error'("lo.core$display$lo.uri*uri@disp", 310, 5, 94)).
'lo.uri@getUriPath'('lo.uri#absUri'(X_5582, XPth, X_5583), XXd8984):- !,
    'lo.uri@dispRsrc'(XPth, XXd8983),
    'lo@formatSS'(XXd8983, XXd8984).
'lo.uri@getUriPath'('lo.uri#relUri'(XPth, X_5584), XXd8986):- !,
    'lo.uri@dispRsrc'(XPth, XXd8985),
    'lo@formatSS'(XXd8985, XXd8986).
'lo.uri@getUriPath'(_, _):- raise_exception('error'("lo.uri@getUriPath", 344, 3, 54)).
'lo.coerce$coercion$lo.uri*uri$lo.core*string'('lo.coerce$coercion$lo.uri*uri$lo.core*string%1'('lo.coerce$coercion$lo.uri*uri$lo.core*string')):- !.
'lo.coerce$coercion$lo.uri*uri$lo.core*string'('_coerce%2'(XV18684, XV18685), XLbl3854, XThis3854):- !,
    'lo.coerce$coercion$lo.uri*uri$lo.core*string@_coerce'(XV18684, XV18685, XLbl3854, XThis3854).
'lo.coerce$coercion$lo.uri*uri$lo.core*string'('_coerce%1'('lo.coerce$coercion$lo.uri*uri$lo.core*string^_coerce'(XLbl3855, XThis3855)), XLbl3855, XThis3855).
'lo.coerce$coercion$lo.uri*uri$lo.core*string@_coerce'(XU, XXd8987, XLbV1689, XThV1689):- !,
    ocall('disp%1'(XXV1950),'lo.core$display$lo.uri*uri','lo.core$display$lo.uri*uri'),
    ocall('_call%2'(XU, XXe1927),XXV1950,XXV1950),
    'lo@formatSS'(XXe1927, XXd8987).
'lo.coerce$coercion$lo.uri*uri$lo.core*string@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.uri*uri$lo.core*string@_coerce", 348, 5, 31)).
'lo.uri^pChar'('_call%1'(XV18516), 'lo.uri^pChar', _):- 'lo.uri@pChar'(XV18516).
'lo.uri^isHexDigit'('_call%1'(XV18517), 'lo.uri^isHexDigit', _):- 'lo.uri@isHexDigit'(XV18517).
'lo.uri^isDigit'('_call%1'(XV18518), 'lo.uri^isDigit', _):- 'lo.uri@isDigit'(XV18518).
'lo.uri^digit'('_call%3'(XV18519, XV18520, XV18521), 'lo.uri^digit', _):- 'lo.uri@digit'(XV18519, XV18520, XV18521).
'lo.uri^hex'('_call%3'(XV18522, XV18523, XV18524), 'lo.uri^hex', _):- 'lo.uri@hex'(XV18522, XV18523, XV18524).
'lo.uri^escaped'('_call%4'(XV18525, XV18526, XV18527, XV18528), 'lo.uri^escaped', _):- 'lo.uri@escaped'(XV18525, XV18526, XV18527, XV18528).
'lo.uri^isMark'('_call%1'(XV18529), 'lo.uri^isMark', _):- 'lo.uri@isMark'(XV18529).
'lo.uri^mark'('_call%3'(XV18530, XV18531, XV18532), 'lo.uri^mark', _):- 'lo.uri@mark'(XV18530, XV18531, XV18532).
'lo.uri^isUpAlpha'('_call%1'(XV18533), 'lo.uri^isUpAlpha', _):- 'lo.uri@isUpAlpha'(XV18533).
'lo.uri^isLowAlpha'('_call%1'(XV18534), 'lo.uri^isLowAlpha', _):- 'lo.uri@isLowAlpha'(XV18534).
'lo.uri@or18'(XC):- 'lo.uri@isLowAlpha'(XC).
'lo.uri@or18'(XC):- 'lo.uri@isUpAlpha'(XC).
'lo.uri^alpha'('_call%3'(XV18535, XV18536, XV18537), 'lo.uri^alpha', _):- 'lo.uri@alpha'(XV18535, XV18536, XV18537).
'lo.uri^alphaNum'('_call%3'(XV18538, XV18539, XV18540), 'lo.uri^alphaNum', _):- 'lo.uri@alphaNum'(XV18538, XV18539, XV18540).
'lo.uri^unreserved'('_call%3'(XV18541, XV18542, XV18543), 'lo.uri^unreserved', _):- 'lo.uri@unreserved'(XV18541, XV18542, XV18543).
'lo.uri^pChars'('_call%4'(XV18544, XV18545, XV18546, XV18547), 'lo.uri^pChars', _):- 'lo.uri@pChars'(XV18544, XV18545, XV18546, XV18547).
'lo.uri^parameter'('_call%4'(XV18548, XV18549, XV18550, XV18551), 'lo.uri^parameter', _):- 'lo.uri@parameter'(XV18548, XV18549, XV18550, XV18551).
'lo.uri^parameters'('_call%3'(XV18552, XV18553, XV18554), 'lo.uri^parameters', _):- 'lo.uri@parameters'(XV18552, XV18553, XV18554).
'lo.uri^segment'('_call%3'(XV18555, XV18556, XV18557), 'lo.uri^segment', _):- 'lo.uri@segment'(XV18555, XV18556, XV18557).
'lo.uri@Disj83'(XDjStrm83, XStx1280, XStx1280, XM, XNStrm1016, XNStrm1016, XDjStrm83):- ocall('_hdtl%3'(XDjStrm83, 47, XNStrm1016),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@pathSegments'(XNStrm1016, XStx1280, XM).
'lo.uri@Disj83'(XDjStrm83, XDjStrm83, XStx1280, XM, XNStrm1016, XNStrm1016, XDjStrm83):- XM = 'lo.core#[]'.
'lo.uri^pathSegments'('_call%3'(XV18558, XV18559, XV18560), 'lo.uri^pathSegments', _):- 'lo.uri@pathSegments'(XV18558, XV18559, XV18560).
'lo.uri^relativeRsrc'('_call%3'(XV18561, XV18562, XV18563), 'lo.uri^relativeRsrc', _):- 'lo.uri@relativeRsrc'(XV18561, XV18562, XV18563).
'lo.uri^absoluteRsrc'('_call%3'(XV18564, XV18565, XV18566), 'lo.uri^absoluteRsrc', _):- 'lo.uri@absoluteRsrc'(XV18564, XV18565, XV18566).
'lo.uri@Hed95'(XHedStrm95, XNStrm1025, XNStrm1025, XHedStrm95):- ocall('_hdtl%3'(XHedStrm95, 64, XNStrm1025),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.uri^userStar'('_call%3'(XV18567, XV18568, XV18569), 'lo.uri^userStar', _):- 'lo.uri@userStar'(XV18567, XV18568, XV18569).
'lo.uri^userInfo'('_call%3'(XV18570, XV18571, XV18572), 'lo.uri^userInfo', _):- 'lo.uri@userInfo'(XV18570, XV18571, XV18572).
'lo.uri^dot'('_call%3'(XV18573, XV18574, XV18575), 'lo.uri^dot', _):- 'lo.uri@dot'(XV18573, XV18574, XV18575).
'lo.uri^minus'('_call%3'(XV18576, XV18577, XV18578), 'lo.uri^minus', _):- 'lo.uri@minus'(XV18576, XV18577, XV18578).
'lo.uri@Disj84'(XDjStrm86, XStx1297, XStx1298, XStx1297, XC):- 'lo.uri@minus'(XDjStrm86, XStx1297, XC).
'lo.uri@Disj84'(XDjStrm86, XStx1298, XStx1298, XStx1297, XC):- 'lo.uri@dot'(XDjStrm86, XStx1298, XC).
'lo.uri@Disj85'(XDjStrm85, XStx1296, XStx1298, XStx1297, XDjOut94, XStx1296, XC):- 'lo.uri@digit'(XDjStrm85, XStx1296, XC).
'lo.uri@Disj85'(XDjStrm85, XDjOut94, XStx1298, XStx1297, XDjOut94, XStx1296, XC):- 'lo.uri@Disj84'(XDjStrm85, XDjOut94, XStx1298, XStx1297, XC).
'lo.uri@Disj86'(XDjStrm84, XStx1295, XStx1298, XStx1297, XDjOut94, XStx1296, XDjOut93, XStx1295, XC):- 'lo.uri@alpha'(XDjStrm84, XStx1295, XC).
'lo.uri@Disj86'(XDjStrm84, XDjOut93, XStx1298, XStx1297, XDjOut94, XStx1296, XDjOut93, XStx1295, XC):- 'lo.uri@Disj85'(XDjStrm84, XDjOut93, XStx1298, XStx1297, XDjOut94, XStx1296, XC).
'lo.uri^alphaDashStar'('_call%3'(XV18579, XV18580, XV18581), 'lo.uri^alphaDashStar', _):- 'lo.uri@alphaDashStar'(XV18579, XV18580, XV18581).
'lo.uri@One9'(XOneStm9, XStx1300, XStx1300, XH):- 'lo.uri@alphaDashStar'(XOneStm9, XStx1300, XH),
    !.
'lo.uri^hostName'('_call%3'(XV18582, XV18583, XV18584), 'lo.uri^hostName', _):- 'lo.uri@hostName'(XV18582, XV18583, XV18584).
'lo.uri@Neg27'(XNegStrm27, XStx1303, X_5537):- 'lo.uri@digit'(XNegStrm27, XStx1303, X_5537),
    !,
    fail.
'lo.uri@Neg27'(XNegStrm27, XStx1303, X_5537).
'lo.uri^digits'('_call%3'(XV18585, XV18586, XV18587), 'lo.uri^digits', _):- 'lo.uri@digits'(XV18585, XV18586, XV18587).
'lo.uri^port'('_call%3'(XV18588, XV18589, XV18590), 'lo.uri^port', _):- 'lo.uri@port'(XV18588, XV18589, XV18590).
'lo.uri@Neg28'(XNegStrm28, XNStrm1029, XNStrm1029, XNegStrm28):- ocall('_hdtl%3'(XNegStrm28, 58, XNStrm1029),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    !,
    fail.
'lo.uri@Neg28'(XNegStrm28, XNStrm1029, XNStrm1029, XNegStrm28).
'lo.uri^hostNamePort'('_call%3'(XV18591, XV18592, XV18593), 'lo.uri^hostNamePort', _):- 'lo.uri@hostNamePort'(XV18591, XV18592, XV18593).
'lo.uri^authority'('_call%3'(XV18594, XV18595, XV18596), 'lo.uri^authority', _):- 'lo.uri@authority'(XV18594, XV18595, XV18596).
'lo.uri@Hed96'(XHedStrm96, XNStrm1031, XNStrm1031, XHedStrm96):- ocall('_hdtl%3'(XHedStrm96, 63, XNStrm1031),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.uri^optAbsolutePath'('_call%3'(XV18597, XV18598, XV18599), 'lo.uri^optAbsolutePath', _):- 'lo.uri@optAbsolutePath'(XV18597, XV18598, XV18599).
'lo.uri^netPath'('_call%3'(XV18600, XV18601, XV18602), 'lo.uri^netPath', _):- 'lo.uri@netPath'(XV18600, XV18601, XV18602).
'lo.uri^isReserved'('_call%1'(XV18603), 'lo.uri^isReserved', _):- 'lo.uri@isReserved'(XV18603).
'lo.uri^reserved'('_call%3'(XV18604, XV18605, XV18606), 'lo.uri^reserved', _):- 'lo.uri@reserved'(XV18604, XV18605, XV18606).
'lo.uri@Disj87'(XDjStrm87, XStx1314, XStx1315, XStx1314, XC):- 'lo.uri@reserved'(XDjStrm87, XStx1314, XC).
'lo.uri@Disj87'(XDjStrm87, XStx1315, XStx1315, XStx1314, XC):- 'lo.uri@unreserved'(XDjStrm87, XStx1315, XC).
'lo.uri^uric'('_call%4'(XV18607, XV18608, XV18609, XV18610), 'lo.uri^uric', _):- 'lo.uri@uric'(XV18607, XV18608, XV18609, XV18610).
'lo.uri^uricStar'('_call%3'(XV18611, XV18612, XV18613), 'lo.uri^uricStar', _):- 'lo.uri@uricStar'(XV18611, XV18612, XV18613).
'lo.uri^query'('_call%3'(XV18614, XV18615, XV18616), 'lo.uri^query', _):- 'lo.uri@query'(XV18614, XV18615, XV18616).
'lo.uri^relativeUri'('_call%4'(XV18617, XV18618, XV18619, XV18620), 'lo.uri^relativeUri', _):- 'lo.uri@relativeUri'(XV18617, XV18618, XV18619, XV18620).
'lo.uri^plus'('_call%3'(XV18621, XV18622, XV18623), 'lo.uri^plus', _):- 'lo.uri@plus'(XV18621, XV18622, XV18623).
'lo.uri@Disj88'(XDjStrm91, XStx1329, XStx1330, XStx1329, XC):- 'lo.uri@minus'(XDjStrm91, XStx1329, XC).
'lo.uri@Disj88'(XDjStrm91, XStx1330, XStx1330, XStx1329, XC):- 'lo.uri@dot'(XDjStrm91, XStx1330, XC).
'lo.uri@Disj89'(XDjStrm90, XStx1328, XStx1330, XStx1329, XDjOut100, XStx1328, XC):- 'lo.uri@plus'(XDjStrm90, XStx1328, XC).
'lo.uri@Disj89'(XDjStrm90, XDjOut100, XStx1330, XStx1329, XDjOut100, XStx1328, XC):- 'lo.uri@Disj88'(XDjStrm90, XDjOut100, XStx1330, XStx1329, XC).
'lo.uri@Disj90'(XDjStrm89, XStx1327, XStx1330, XStx1329, XDjOut100, XStx1328, XDjOut99, XStx1327, XC):- 'lo.uri@digit'(XDjStrm89, XStx1327, XC).
'lo.uri@Disj90'(XDjStrm89, XDjOut99, XStx1330, XStx1329, XDjOut100, XStx1328, XDjOut99, XStx1327, XC):- 'lo.uri@Disj89'(XDjStrm89, XDjOut99, XStx1330, XStx1329, XDjOut100, XStx1328, XC).
'lo.uri@Disj91'(XDjStrm88, XStx1326, XStx1330, XStx1329, XDjOut100, XStx1328, XDjOut99, XStx1327, XDjOut98, XStx1326, XC):- 'lo.uri@alpha'(XDjStrm88, XStx1326, XC).
'lo.uri@Disj91'(XDjStrm88, XDjOut98, XStx1330, XStx1329, XDjOut100, XStx1328, XDjOut99, XStx1327, XDjOut98, XStx1326, XC):- 'lo.uri@Disj90'(XDjStrm88, XDjOut98, XStx1330, XStx1329, XDjOut100, XStx1328, XDjOut99, XStx1327, XC).
'lo.uri^alphaStar'('_call%3'(XV18624, XV18625, XV18626), 'lo.uri^alphaStar', _):- 'lo.uri@alphaStar'(XV18624, XV18625, XV18626).
'lo.uri^grabScheme'('_call%3'(XV18627, XV18628, XV18629), 'lo.uri^grabScheme', _):- 'lo.uri@grabScheme'(XV18627, XV18628, XV18629).
'lo.uri@Hed97'(XHedStrm97, XNStrm1037, XNStrm1037, XHedStrm97):- ocall('_hdtl%3'(XHedStrm97, 58, XNStrm1037),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.uri^scheme'('_call%3'(XV18630, XV18631, XV18632), 'lo.uri^scheme', _):- 'lo.uri@scheme'(XV18630, XV18631, XV18632).
'lo.uri^hierPart'('_call%4'(XV18633, XV18634, XV18635, XV18636), 'lo.uri^hierPart', _):- 'lo.uri@hierPart'(XV18633, XV18634, XV18635, XV18636).
'lo.uri^absoluteUri'('_call%5'(XV18637, XV18638, XV18639, XV18640, XV18641), 'lo.uri^absoluteUri', _):- 'lo.uri@absoluteUri'(XV18637, XV18638, XV18639, XV18640, XV18641).
'lo.uri^uriParse'('_call%3'(XV18642, XV18643, XV18644), 'lo.uri^uriParse', _):- 'lo.uri@uriParse'(XV18642, XV18643, XV18644).
'lo.uri^parseUri'('_call%2'(XV18645, XV18646), 'lo.uri^parseUri', _):- 'lo.uri@parseUri'(XV18645, XV18646).
'lo.uri^isDelim'('_call%1'(XV18647), 'lo.uri^isDelim', _):- 'lo.uri@isDelim'(XV18647).
'lo.uri^delim'('_call%3'(XV18648, XV18649, XV18650), 'lo.uri^delim', _):- 'lo.uri@delim'(XV18648, XV18649, XV18650).
'lo.uri^edit'('_call%3'(XV18651, XV18652, XV18653), 'lo.uri^edit', _):- 'lo.uri@edit'(XV18651, XV18652, XV18653).
'lo.uri^resolvePath'('_call%3'(XV18654, XV18655, XV18656), 'lo.uri^resolvePath', _):- 'lo.uri@resolvePath'(XV18654, XV18655, XV18656).
'lo.uri^resolveUri'('_call%3'(XV18657, XV18658, XV18659), 'lo.uri^resolveUri', _):- 'lo.uri@resolveUri'(XV18657, XV18658, XV18659).
'lo.uri^dispSegs'('_call%2'(XV18660, XV18661), 'lo.uri^dispSegs', _):- 'lo.uri@dispSegs'(XV18660, XV18661).
'lo.uri^dispPath'('_call%2'(XV18662, XV18663), 'lo.uri^dispPath', _):- 'lo.uri@dispPath'(XV18662, XV18663).
'lo.uri^dispUser'('_call%2'(XV18664, XV18665), 'lo.uri^dispUser', _):- 'lo.uri@dispUser'(XV18664, XV18665).
'lo.uri^dispHost'('_call%2'(XV18666, XV18667), 'lo.uri^dispHost', _):- 'lo.uri@dispHost'(XV18666, XV18667).
'lo.uri^dispAuthority'('_call%2'(XV18668, XV18669), 'lo.uri^dispAuthority', _):- 'lo.uri@dispAuthority'(XV18668, XV18669).
'lo.uri^dispRsrc'('_call%2'(XV18670, XV18671), 'lo.uri^dispRsrc', _):- 'lo.uri@dispRsrc'(XV18670, XV18671).
'lo.core$display$lo.uri*uri^dispQuery'('_call%2'(XV18672, XV18673), 'lo.core$display$lo.uri*uri^dispQuery'(XLbV1688, XThV1688), _):- 'lo.core$display$lo.uri*uri@dispQuery'(XV18672, XV18673, XLbV1688, XThV1688).
'lo.core$display$lo.uri*uri^disp'('_call%2'(XV18676, XV18677), 'lo.core$display$lo.uri*uri^disp'(XLbV1688, XThV1688), _):- 'lo.core$display$lo.uri*uri@disp'(XV18676, XV18677, XLbV1688, XThV1688).
'lo.uri^getUriPath'('_call%2'(XV18680, XV18681), 'lo.uri^getUriPath', _):- 'lo.uri@getUriPath'(XV18680, XV18681).
'lo.coerce$coercion$lo.uri*uri$lo.core*string^_coerce'('_call%2'(XV18682, XV18683), 'lo.coerce$coercion$lo.uri*uri$lo.core*string^_coerce'(XLbV1689, XThV1689), _):- 'lo.coerce$coercion$lo.uri*uri$lo.core*string@_coerce'(XV18682, XV18683, XLbV1689, XThV1689).
