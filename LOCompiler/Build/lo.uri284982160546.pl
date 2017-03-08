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
'lo.uri@digit'(XStIn1307, XNStrm1165, XX):- ocall('_hdtl%3'(XStIn1307, XX, XNStrm1165),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@isDigit'(XX).
'lo.uri@hex'(XStIn1308, XStx1217, XC):- 'lo.uri@digit'(XStIn1308, XStx1217, XC).
'lo.uri@hex'(XStIn1309, XNStrm1166, XC):- ocall('_hdtl%3'(XStIn1309, XC, XNStrm1166),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@isHexDigit'(XC).
'lo.uri@escaped'(XStIn1310, XStx1219, XU, XL):- ocall('_hdtl%3'(XStIn1310, 37, XNStrm1167),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@hex'(XNStrm1167, XStx1218, XU),
    'lo.uri@hex'(XStx1218, XStx1219, XL).
'lo.uri@isMark'(45).
'lo.uri@isMark'(95).
'lo.uri@isMark'(46).
'lo.uri@isMark'(33).
'lo.uri@isMark'(126).
'lo.uri@isMark'(42).
'lo.uri@isMark'(39).
'lo.uri@isMark'(40).
'lo.uri@isMark'(41).
'lo.uri@mark'(XStIn1311, XNStrm1168, XC):- ocall('_hdtl%3'(XStIn1311, XC, XNStrm1168),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
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
'lo.uri@alpha'(XStIn1312, XNStrm1169, XC):- ocall('_hdtl%3'(XStIn1312, XC, XNStrm1169),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@or108'(XC).
'lo.uri@alphaNum'(XStIn1313, XStx1220, XC):- 'lo.uri@alpha'(XStIn1313, XStx1220, XC).
'lo.uri@alphaNum'(XStIn1314, XStx1221, XC):- 'lo.uri@digit'(XStIn1314, XStx1221, XC).
'lo.uri@unreserved'(XStIn1315, XStx1222, XC):- 'lo.uri@alphaNum'(XStIn1315, XStx1222, XC).
'lo.uri@unreserved'(XStIn1316, XStx1223, XC):- 'lo.uri@mark'(XStIn1316, XStx1223, XC).
'lo.uri@pChars'(XStIn1317, XStx1225, 'lo.core#,..'(XC, XM), XR):- 'lo.uri@unreserved'(XStIn1317, XStx1224, XC),
    'lo.uri@pChars'(XStx1224, XStx1225, XM, XR).
'lo.uri@pChars'(XStIn1318, XStx1227, 'lo.core#,..'(37, 'lo.core#,..'(XU, 'lo.core#,..'(XL, XM))), XR):- 'lo.uri@escaped'(XStIn1318, XStx1226, XU, XL),
    'lo.uri@pChars'(XStx1226, XStx1227, XM, XR).
'lo.uri@pChars'(XStIn1319, XStx1228, 'lo.core#,..'(XC, XM), XR):- ocall('_hdtl%3'(XStIn1319, XC, XNStrm1170),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@pChar'(XC),
    'lo.uri@pChars'(XNStrm1170, XStx1228, XM, XR).
'lo.uri@pChars'(XStIn1320, XStIn1320, XX, XX).
'lo.uri@parameter'(XStIn1321, XStx1229, 'lo.core#,..'(59, XP), XM):- ocall('_hdtl%3'(XStIn1321, 59, XNStrm1171),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@pChars'(XNStrm1171, XStx1229, XP, XM).
'lo.uri@parameters'(XStIn1322, XStx1231, XP):- 'lo.uri@parameter'(XStIn1322, XStx1230, XP, XS),
    'lo.uri@parameters'(XStx1230, XStx1231, XS).
'lo.uri@parameters'(XStIn1323, XStIn1323, 'lo.core#[]').
'lo.uri@segment'(XStIn1324, XStx1233, XXa70):- 'lo.uri@pChars'(XStIn1324, XStx1232, XC, XR),
    'lo.uri@parameters'(XStx1232, XStx1233, XR),
    'implode'(XC, XXa70).
'lo.uri@pathSegments'(XStIn1325, XDjOut96, 'lo.core#,..'(XSeg, XM)):- 'lo.uri@segment'(XStIn1325, XStx1234, XSeg),
    'lo.uri@Disj71'(XStx1234, XDjOut96, XStx1235, XM, XNStrm1172, XNStrm1172, XDjStrm71).
'lo.uri@relativeRsrc'(XStIn1326, XStx1236, 'lo.uri#relPath'(XSegments)):- 'lo.uri@pathSegments'(XStIn1326, XStx1236, XSegments).
'lo.uri@absoluteRsrc'(XStIn1327, XStx1237, 'lo.uri#absPath'(XSegments)):- ocall('_hdtl%3'(XStIn1327, 47, XNStrm1173),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@pathSegments'(XNStrm1173, XStx1237, XSegments).
'lo.uri@userStar'(XStIn1328, XStx1239, 'lo.core#,..'(XC, XS)):- 'lo.uri@unreserved'(XStIn1328, XStx1238, XC),
    'lo.uri@userStar'(XStx1238, XStx1239, XS).
'lo.uri@userStar'(XStIn1329, XStx1241, 'lo.core#,..'(37, 'lo.core#,..'(XU, 'lo.core#,..'(XL, XS)))):- 'lo.uri@escaped'(XStIn1329, XStx1240, XU, XL),
    'lo.uri@userStar'(XStx1240, XStx1241, XS).
'lo.uri@userStar'(XStIn1330, XStx1242, 'lo.core#,..'(36, XS)):- ocall('_hdtl%3'(XStIn1330, 36, XNStrm1174),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@userStar'(XNStrm1174, XStx1242, XS).
'lo.uri@userStar'(XStIn1331, XStx1243, 'lo.core#,..'(44, XS)):- ocall('_hdtl%3'(XStIn1331, 44, XNStrm1175),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@userStar'(XNStrm1175, XStx1243, XS).
'lo.uri@userStar'(XStIn1332, XStx1244, 'lo.core#,..'(59, XS)):- ocall('_hdtl%3'(XStIn1332, 59, XNStrm1176),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@userStar'(XNStrm1176, XStx1244, XS).
'lo.uri@userStar'(XStIn1333, XStx1245, 'lo.core#,..'(58, XS)):- ocall('_hdtl%3'(XStIn1333, 58, XNStrm1177),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@userStar'(XNStrm1177, XStx1245, XS).
'lo.uri@userStar'(XStIn1334, XStx1246, 'lo.core#,..'(38, XS)):- ocall('_hdtl%3'(XStIn1334, 38, XNStrm1178),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@userStar'(XNStrm1178, XStx1246, XS).
'lo.uri@userStar'(XStIn1335, XStx1247, 'lo.core#,..'(61, XS)):- ocall('_hdtl%3'(XStIn1335, 61, XNStrm1179),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@userStar'(XNStrm1179, XStx1247, XS).
'lo.uri@userStar'(XStIn1336, XStx1248, 'lo.core#,..'(43, XS)):- ocall('_hdtl%3'(XStIn1336, 43, XNStrm1180),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@userStar'(XNStrm1180, XStx1248, XS).
'lo.uri@userStar'(XStIn1337, XStIn1337, 'lo.core#[]'):- 'lo.uri@Hed80'(XStIn1337, XNStrm1181, XNStrm1181, XHedStrm80).
'lo.uri@userInfo'(XStIn1338, XStx1249, XU):- 'lo.uri@userStar'(XStIn1338, XStx1249, XU).
'lo.uri@dot'(XStIn1339, XNStrm1182, 46):- ocall('_hdtl%3'(XStIn1339, 45, XNStrm1182),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.uri@minus'(XStIn1340, XNStrm1183, 45):- ocall('_hdtl%3'(XStIn1340, 45, XNStrm1183),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.uri@alphaDashStar'(XStIn1341, XStx1254, 'lo.core#,..'(XC, XS)):- 'lo.uri@Disj74'(XStIn1341, XDjOut97, XStx1253, XStx1252, XDjOut99, XStx1251, XDjOut98, XStx1250, XC),
    'lo.uri@alphaDashStar'(XDjOut97, XStx1254, XS).
'lo.uri@alphaDashStar'(XStIn1342, XStIn1342, 'lo.core#[]').
'lo.uri@hostName'(XStIn1343, XDjOut100, XXa71):- 'lo.uri@One26'(XStIn1343, XDjOut100, XStx1255, XH),
    'implode'(XH, XXa71).
'lo.uri@digits'(XStIn1344, XStx1257, 'lo.core#,..'(XC, XS)):- 'lo.uri@digit'(XStIn1344, XStx1256, XC),
    'lo.uri@digits'(XStx1256, XStx1257, XS).
'lo.uri@digits'(XStIn1345, XStIn1345, 'lo.core#[]'):- 'lo.uri@Neg43'(XStIn1345, XStx1258, X_22966).
'lo.uri@port'(XStIn1346, XStx1259, XXa72):- 'lo.uri@digits'(XStIn1346, XStx1259, XP),
    'implode'(XP, XXa72).
'lo.uri@hostNamePort'(XStIn1347, XStx1261, 'lo.uri#hostPort'(XH, XP)):- 'lo.uri@hostName'(XStIn1347, XStx1260, XH),
    ocall('_hdtl%3'(XStx1260, 58, XNStrm1184),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@port'(XNStrm1184, XStx1261, XP).
'lo.uri@hostNamePort'(XStIn1348, XStx1262, 'lo.uri#host'(XH)):- 'lo.uri@hostName'(XStIn1348, XStx1262, XH),
    'lo.uri@Neg44'(XStx1262, XNStrm1185, XNStrm1185, XNegStrm44).
'lo.uri@authority'(XStIn1349, XStx1264, 'lo.uri#server'('lo.core#some'('lo.uri#user'(XXa73)), XH)):- 'lo.uri@userInfo'(XStIn1349, XStx1263, XU),
    ocall('_hdtl%3'(XStx1263, 64, XNStrm1186),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@hostNamePort'(XNStrm1186, XStx1264, XH),
    'implode'(XU, XXa73).
'lo.uri@authority'(XStIn1350, XStx1265, 'lo.uri#server'('lo.core#none', XH)):- 'lo.uri@hostNamePort'(XStIn1350, XStx1265, XH).
'lo.uri@optAbsolutePath'(XStIn1351, XStx1266, XP):- 'lo.uri@absoluteRsrc'(XStIn1351, XStx1266, XP).
'lo.uri@optAbsolutePath'(XStIn1352, XStIn1352, 'lo.uri#relPath'('lo.core#[]')):- 'lo.uri@Hed81'(XStIn1352, XNStrm1187, XNStrm1187, XHedStrm81).
'lo.uri@optAbsolutePath'(XStIn1353, XStIn1353, 'lo.uri#relPath'('lo.core#[]')):- XStIn1353 = X_22967,
    ocall('_eof%1'(X_22967),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.uri@netPath'(XStIn1354, XStx1268, 'lo.uri#netRsrc'(XA, XP)):- ocall('_hdtl%3'(XStIn1354, 47, XNStrm1188),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    ocall('_hdtl%3'(XNStrm1188, 47, XNStrm1189),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@authority'(XNStrm1189, XStx1267, XA),
    'lo.uri@optAbsolutePath'(XStx1267, XStx1268, XP).
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
'lo.uri@reserved'(XStIn1355, XNStrm1190, XC):- ocall('_hdtl%3'(XStIn1355, XC, XNStrm1190),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@isReserved'(XC).
'lo.uri@uric'(XStIn1356, XDjOut101, 'lo.core#,..'(XC, XM), XM):- 'lo.uri@Disj75'(XStIn1356, XDjOut101, XStx1270, XStx1269, XC).
'lo.uri@uric'(XStIn1357, XStx1271, 'lo.core#,..'(37, 'lo.core#,..'(XU, 'lo.core#,..'(XL, XM))), XM):- 'lo.uri@escaped'(XStIn1357, XStx1271, XU, XL).
'lo.uri@uricStar'(XStIn1358, XStx1273, XS):- 'lo.uri@uric'(XStIn1358, XStx1272, XS, XM),
    'lo.uri@uricStar'(XStx1272, XStx1273, XM).
'lo.uri@uricStar'(XStIn1359, XStIn1359, 'lo.core#[]').
'lo.uri@query'(XStIn1360, XStx1274, 'lo.uri#qry'(XXa74)):- ocall('_hdtl%3'(XStIn1360, 63, XNStrm1191),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@uricStar'(XNStrm1191, XStx1274, XQ),
    'implode'(XQ, XXa74).
'lo.uri@query'(XStIn1361, XStIn1361, 'lo.uri#noQ'):- XStIn1361 = X_22972,
    ocall('_eof%1'(X_22972),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.uri@relativeUri'(XStIn1362, XStx1276, XHier, XQuery):- 'lo.uri@netPath'(XStIn1362, XStx1275, XHier),
    'lo.uri@query'(XStx1275, XStx1276, XQuery).
'lo.uri@relativeUri'(XStIn1363, XStx1278, 'lo.uri#localRsrc'(XRsrc), XQuery):- 'lo.uri@absoluteRsrc'(XStIn1363, XStx1277, XRsrc),
    'lo.uri@query'(XStx1277, XStx1278, XQuery).
'lo.uri@relativeUri'(XStIn1364, XStx1280, 'lo.uri#localRsrc'(XRsrc), XQuery):- 'lo.uri@relativeRsrc'(XStIn1364, XStx1279, XRsrc),
    'lo.uri@query'(XStx1279, XStx1280, XQuery).
'lo.uri@plus'(XStIn1365, XNStrm1192, 43):- ocall('_hdtl%3'(XStIn1365, 43, XNStrm1192),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.uri@alphaStar'(XStIn1366, XStx1286, 'lo.core#,..'(XC, XS)):- 'lo.uri@Disj79'(XStIn1366, XDjOut102, XStx1285, XStx1284, XDjOut105, XStx1283, XDjOut104, XStx1282, XDjOut103, XStx1281, XC),
    'lo.uri@alphaStar'(XDjOut102, XStx1286, XS).
'lo.uri@alphaStar'(XStIn1367, XStIn1367, 'lo.core#[]').
'lo.uri@grabScheme'(XStIn1368, XStx1288, 'lo.core#,..'(XC, XS)):- 'lo.uri@alpha'(XStIn1368, XStx1287, XC),
    'lo.uri@alphaStar'(XStx1287, XStx1288, XS).
'lo.uri@scheme'(XStIn1369, XStx1289, XXa75):- 'lo.uri@grabScheme'(XStIn1369, XStx1289, XScheme),
    'lo.uri@Hed82'(XStx1289, XNStrm1193, XNStrm1193, XHedStrm82),
    'implode'(XScheme, XXa75).
'lo.uri@hierPart'(XStIn1370, XStx1291, XHier, XQuery):- 'lo.uri@netPath'(XStIn1370, XStx1290, XHier),
    'lo.uri@query'(XStx1290, XStx1291, XQuery).
'lo.uri@hierPart'(XStIn1371, XStx1293, 'lo.uri#localRsrc'(XRsrc), XQuery):- 'lo.uri@absoluteRsrc'(XStIn1371, XStx1292, XRsrc),
    'lo.uri@query'(XStx1292, XStx1293, XQuery).
'lo.uri@absoluteUri'(XStIn1372, XStx1295, XScheme, XHier, XQuery):- 'lo.uri@scheme'(XStIn1372, XStx1294, XScheme),
    ocall('_hdtl%3'(XStx1294, 58, XNStrm1194),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@hierPart'(XNStrm1194, XStx1295, XHier, XQuery).
'lo.uri@uriParse'(XStIn1373, XStx1296, 'lo.uri#absUri'(XScheme, XHier, XQuery)):- 'lo.uri@absoluteUri'(XStIn1373, XStx1296, XScheme, XHier, XQuery).
'lo.uri@uriParse'(XStIn1374, XStx1297, 'lo.uri#relUri'(XHier, XQuery)):- 'lo.uri@relativeUri'(XStIn1374, XStx1297, XHier, XQuery).
'lo.uri@parseUri'(XS, XU):- 'explode'(XS, XXc422),
    'lo.uri@uriParse'(XXc422, XStx1298, XU),
    XStx1298 = X_22976,
    ocall('_eof%1'(X_22976),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    X_22975 = XStx1298,
    !.
'lo.uri@parseUri'(_, _):- raise_exception('error'("lo.uri@parseUri", 20, 3, 45)).
'lo.uri@isDelim'(60).
'lo.uri@isDelim'(62).
'lo.uri@isDelim'(35).
'lo.uri@isDelim'(37).
'lo.uri@isDelim'(34).
'lo.uri@delim'(XStIn1375, XNStrm1195, XC):- ocall('_hdtl%3'(XStIn1375, XC, XNStrm1195),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@isDelim'(XC).
'lo.uri@edit'('lo.core#,..'(".", XSegs), XR, XXd28114):- !,
    'lo.uri@edit'(XSegs, XR, XXd28114).
'lo.uri@edit'('lo.core#,..'("..", XSegs), 'lo.core#,..'(X_22980, XR), XXd28115):- !,
    'lo.uri@edit'(XSegs, XR, XXd28115).
'lo.uri@edit'(XSegs, XR, XXd28117):- !,
    'lo.list@reverse'(XR, XXd28116),
    'lo.list@<>'(XXd28116, XSegs, XXd28117).
'lo.uri@edit'(_, _, _):- raise_exception('error'("lo.uri@edit", 304, 3, 36)).
'lo.uri@resolvePath'(X_22981, 'lo.uri#netRsrc'(XA, XP), 'lo.uri#netRsrc'(XA, XP)):- !.
'lo.uri@resolvePath'('lo.uri#netRsrc'(XA, X_22982), 'lo.uri#localRsrc'('lo.uri#absPath'(XP)), 'lo.uri#netRsrc'(XA, 'lo.uri#absPath'(XP))):- !.
'lo.uri@resolvePath'('lo.uri#netRsrc'(XA, 'lo.uri#absPath'(XB)), 'lo.uri#localRsrc'('lo.uri#relPath'(XP)), 'lo.uri#netRsrc'(XA, 'lo.uri#absPath'(XXd28123))):- !,
    'lo.list@reverse'(XB, XXd28121),
    'lo.list@drop'(XXd28121, XXd28122),
    'lo.uri@edit'(XP, XXd28122, XXd28123).
'lo.uri@resolvePath'('lo.uri#localRsrc'(X_22983), 'lo.uri#localRsrc'('lo.uri#absPath'(XP)), 'lo.uri#localRsrc'('lo.uri#absPath'(XP))):- !.
'lo.uri@resolvePath'('lo.uri#localRsrc'('lo.uri#absPath'(XB)), 'lo.uri#localRsrc'('lo.uri#relPath'(XP)), 'lo.uri#localRsrc'('lo.uri#absPath'(XXd28130))):- !,
    'lo.list@reverse'(XB, XXd28128),
    'lo.list@drop'(XXd28128, XXd28129),
    'lo.uri@edit'(XP, XXd28129, XXd28130).
'lo.uri@resolvePath'(_, _, _):- raise_exception('error'("lo.uri@resolvePath", 297, 3, 43)).
'lo.uri@resolveUri'(X_22984, XU, XU):- 'lo.uri#absUri'(X_22985, X_22986, X_22987) = XU,
    !.
'lo.uri@resolveUri'('lo.uri#absUri'(XScheme, XBase, X_22988), 'lo.uri#relUri'(XPath, XQuery), 'lo.uri#absUri'(XScheme, XXd28134, XQuery)):- !,
    'lo.uri@resolvePath'(XBase, XPath, XXd28134).
'lo.uri@resolveUri'(_, _, _):- raise_exception('error'("lo.uri@resolveUri", 293, 3, 39)).
'lo.uri@dispSegs'('lo.core#[]', 'lo.core#[]'):- !.
'lo.uri@dispSegs'('lo.core#,..'(XS, 'lo.core#[]'), 'lo.core#,..'('lo.core#ss'(XS), 'lo.core#[]')):- !.
'lo.uri@dispSegs'('lo.core#,..'(XS, XM), 'lo.core#,..'('lo.core#ss'(XS), 'lo.core#,..'('lo.core#ss'("/"), XXd28140))):- !,
    'lo.uri@dispSegs'(XM, XXd28140).
'lo.uri@dispSegs'(_, _):- raise_exception('error'("lo.uri@dispSegs", 338, 3, 18)).
'lo.uri@dispPath'('lo.uri#absPath'(XSegs), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("/"), XXd28144))):- !,
    'lo.uri@dispSegs'(XSegs, XXd28144).
'lo.uri@dispPath'('lo.uri#relPath'(XSegs), 'lo.core#ssSeq'(XXd28147)):- !,
    'lo.uri@dispSegs'(XSegs, XXd28147).
'lo.uri@dispPath'(_, _):- raise_exception('error'("lo.uri@dispPath", 334, 3, 60)).
'lo.uri@dispUser'('lo.uri#user'(XU), 'lo.core#ss'(XU)):- !.
'lo.uri@dispUser'(_, _):- raise_exception('error'("lo.uri@dispUser", 327, 3, 26)).
'lo.uri@dispHost'('lo.uri#hostPort'(XH, XP), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(XH), 'lo.core#,..'('lo.core#ss'(":"), 'lo.core#,..'('lo.core#ss'(XP), 'lo.core#[]'))))):- !.
'lo.uri@dispHost'('lo.uri#host'(XH), 'lo.core#ss'(XH)):- !.
'lo.uri@dispHost'(_, _):- raise_exception('error'("lo.uri@dispHost", 330, 3, 55)).
'lo.uri@dispAuthority'('lo.uri#server'('lo.core#none', XH), XXd28158):- !,
    'lo.uri@dispHost'(XH, XXd28158).
'lo.uri@dispAuthority'('lo.uri#server'('lo.core#some'(XU), XH), 'lo.core#ssSeq'('lo.core#,..'(XXd28159, 'lo.core#,..'('lo.core#ss'("@"), 'lo.core#,..'(XXd28161, 'lo.core#[]'))))):- !,
    'lo.uri@dispUser'(XU, XXd28159),
    'lo.uri@dispHost'(XH, XXd28161).
'lo.uri@dispAuthority'(_, _):- raise_exception('error'("lo.uri@dispAuthority", 323, 3, 44)).
'lo.uri@dispRsrc'('lo.uri#netRsrc'(XH, XP), 'lo.core#ssSeq'('lo.core#,..'(XXd28166, 'lo.core#,..'(XXd28167, 'lo.core#[]')))):- !,
    'lo.uri@dispAuthority'(XH, XXd28166),
    'lo.uri@dispPath'(XP, XXd28167).
'lo.uri@dispRsrc'('lo.uri#localRsrc'(XP), XXd28171):- !,
    'lo.uri@dispPath'(XP, XXd28171).
'lo.uri@dispRsrc'(_, _):- raise_exception('error'("lo.uri@dispRsrc", 319, 3, 63)).
'lo.core$display$lo.uri*uri'('lo.core$display$lo.uri*uri%1'('lo.core$display$lo.uri*uri')):- !.
'lo.core$display$lo.uri*uri'('dispQuery%2'(XV21850, XV21851), XLbl1885, XThis1885):- !,
    'lo.core$display$lo.uri*uri@dispQuery'(XV21850, XV21851, XLbl1885, XThis1885).
'lo.core$display$lo.uri*uri'('dispQuery%1'('lo.core$display$lo.uri*uri^dispQuery'(XLbl1886, XThis1886)), XLbl1886, XThis1886).
'lo.core$display$lo.uri*uri'('disp%2'(XV21854, XV21855), XLbl1887, XThis1887):- !,
    'lo.core$display$lo.uri*uri@disp'(XV21854, XV21855, XLbl1887, XThis1887).
'lo.core$display$lo.uri*uri'('disp%1'('lo.core$display$lo.uri*uri^disp'(XLbl1888, XThis1888)), XLbl1888, XThis1888).
'lo.core$display$lo.uri*uri@dispQuery'('lo.uri#noQ', 'lo.core#ssSeq'('lo.core#[]'), XLbV1900, XThV1900):- !.
'lo.core$display$lo.uri*uri@dispQuery'('lo.uri#qry'(XQ), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("?"), 'lo.core#,..'('lo.core#ss'(XQ), 'lo.core#[]'))), XLbV1900, XThV1900):- !.
'lo.core$display$lo.uri*uri@dispQuery'(_, _):- raise_exception('error'("lo.core$display$lo.uri*uri@dispQuery", 314, 5, 27)).
'lo.core$display$lo.uri*uri@disp'('lo.uri#absUri'(XScheme, XRsrc, XQuery), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(XScheme), 'lo.core#,..'('lo.core#ss'(":"), 'lo.core#,..'(XXd28180, 'lo.core#,..'(XXd28181, 'lo.core#[]'))))), XLbV1900, XThV1900):- !,
    'lo.uri@dispRsrc'(XRsrc, XXd28180),
    'lo.core$display$lo.uri*uri@dispQuery'(XQuery, XXd28181, XLbV1900, XThV1900).
'lo.core$display$lo.uri*uri@disp'('lo.uri#relUri'(XRsrc, XQuery), 'lo.core#ssSeq'('lo.core#,..'(XXd28187, 'lo.core#,..'(XXd28188, 'lo.core#[]'))), XLbV1900, XThV1900):- !,
    'lo.uri@dispRsrc'(XRsrc, XXd28187),
    'lo.core$display$lo.uri*uri@dispQuery'(XQuery, XXd28188, XLbV1900, XThV1900).
'lo.core$display$lo.uri*uri@disp'(_, _):- raise_exception('error'("lo.core$display$lo.uri*uri@disp", 310, 5, 94)).
'lo.uri@getUriPath'('lo.uri#absUri'(X_23011, XPth, X_23012), XXd28193):- !,
    'lo.uri@dispRsrc'(XPth, XXd28192),
    'lo@formatSS'(XXd28192, XXd28193).
'lo.uri@getUriPath'('lo.uri#relUri'(XPth, X_23013), XXd28195):- !,
    'lo.uri@dispRsrc'(XPth, XXd28194),
    'lo@formatSS'(XXd28194, XXd28195).
'lo.uri@getUriPath'(_, _):- raise_exception('error'("lo.uri@getUriPath", 344, 3, 54)).
'lo.coerce$coercion$lo.uri*uri$lo.core*string'('lo.coerce$coercion$lo.uri*uri$lo.core*string%1'('lo.coerce$coercion$lo.uri*uri$lo.core*string')):- !.
'lo.coerce$coercion$lo.uri*uri$lo.core*string'('_coerce%2'(XV21860, XV21861), XLbl1889, XThis1889):- !,
    'lo.coerce$coercion$lo.uri*uri$lo.core*string@_coerce'(XV21860, XV21861, XLbl1889, XThis1889).
'lo.coerce$coercion$lo.uri*uri$lo.core*string'('_coerce%1'('lo.coerce$coercion$lo.uri*uri$lo.core*string^_coerce'(XLbl1890, XThis1890)), XLbl1890, XThis1890).
'lo.coerce$coercion$lo.uri*uri$lo.core*string@_coerce'(XU, XXd28196, XLbV1901, XThV1901):- !,
    ocall('disp%1'(XXV3524),'lo.core$display$lo.uri*uri','lo.core$display$lo.uri*uri'),
    ocall('_call%2'(XU, XXe3283),XXV3524,XXV3524),
    'lo@formatSS'(XXe3283, XXd28196).
'lo.coerce$coercion$lo.uri*uri$lo.core*string@_coerce'(_, _):- raise_exception('error'("lo.coerce$coercion$lo.uri*uri$lo.core*string@_coerce", 348, 5, 31)).
'lo.uri^pChar'('_call%1'(XV21692), 'lo.uri^pChar', _):- 'lo.uri@pChar'(XV21692).
'lo.uri^isHexDigit'('_call%1'(XV21693), 'lo.uri^isHexDigit', _):- 'lo.uri@isHexDigit'(XV21693).
'lo.uri^isDigit'('_call%1'(XV21694), 'lo.uri^isDigit', _):- 'lo.uri@isDigit'(XV21694).
'lo.uri^digit'('_call%3'(XV21695, XV21696, XV21697), 'lo.uri^digit', _):- 'lo.uri@digit'(XV21695, XV21696, XV21697).
'lo.uri^hex'('_call%3'(XV21698, XV21699, XV21700), 'lo.uri^hex', _):- 'lo.uri@hex'(XV21698, XV21699, XV21700).
'lo.uri^escaped'('_call%4'(XV21701, XV21702, XV21703, XV21704), 'lo.uri^escaped', _):- 'lo.uri@escaped'(XV21701, XV21702, XV21703, XV21704).
'lo.uri^isMark'('_call%1'(XV21705), 'lo.uri^isMark', _):- 'lo.uri@isMark'(XV21705).
'lo.uri^mark'('_call%3'(XV21706, XV21707, XV21708), 'lo.uri^mark', _):- 'lo.uri@mark'(XV21706, XV21707, XV21708).
'lo.uri^isUpAlpha'('_call%1'(XV21709), 'lo.uri^isUpAlpha', _):- 'lo.uri@isUpAlpha'(XV21709).
'lo.uri^isLowAlpha'('_call%1'(XV21710), 'lo.uri^isLowAlpha', _):- 'lo.uri@isLowAlpha'(XV21710).
'lo.uri@or108'(XC):- 'lo.uri@isLowAlpha'(XC).
'lo.uri@or108'(XC):- 'lo.uri@isUpAlpha'(XC).
'lo.uri^alpha'('_call%3'(XV21711, XV21712, XV21713), 'lo.uri^alpha', _):- 'lo.uri@alpha'(XV21711, XV21712, XV21713).
'lo.uri^alphaNum'('_call%3'(XV21714, XV21715, XV21716), 'lo.uri^alphaNum', _):- 'lo.uri@alphaNum'(XV21714, XV21715, XV21716).
'lo.uri^unreserved'('_call%3'(XV21717, XV21718, XV21719), 'lo.uri^unreserved', _):- 'lo.uri@unreserved'(XV21717, XV21718, XV21719).
'lo.uri^pChars'('_call%4'(XV21720, XV21721, XV21722, XV21723), 'lo.uri^pChars', _):- 'lo.uri@pChars'(XV21720, XV21721, XV21722, XV21723).
'lo.uri^parameter'('_call%4'(XV21724, XV21725, XV21726, XV21727), 'lo.uri^parameter', _):- 'lo.uri@parameter'(XV21724, XV21725, XV21726, XV21727).
'lo.uri^parameters'('_call%3'(XV21728, XV21729, XV21730), 'lo.uri^parameters', _):- 'lo.uri@parameters'(XV21728, XV21729, XV21730).
'lo.uri^segment'('_call%3'(XV21731, XV21732, XV21733), 'lo.uri^segment', _):- 'lo.uri@segment'(XV21731, XV21732, XV21733).
'lo.uri@Disj71'(XDjStrm71, XStx1235, XStx1235, XM, XNStrm1172, XNStrm1172, XDjStrm71):- ocall('_hdtl%3'(XDjStrm71, 47, XNStrm1172),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    'lo.uri@pathSegments'(XNStrm1172, XStx1235, XM).
'lo.uri@Disj71'(XDjStrm71, XDjStrm71, XStx1235, XM, XNStrm1172, XNStrm1172, XDjStrm71):- XM = 'lo.core#[]'.
'lo.uri^pathSegments'('_call%3'(XV21734, XV21735, XV21736), 'lo.uri^pathSegments', _):- 'lo.uri@pathSegments'(XV21734, XV21735, XV21736).
'lo.uri^relativeRsrc'('_call%3'(XV21737, XV21738, XV21739), 'lo.uri^relativeRsrc', _):- 'lo.uri@relativeRsrc'(XV21737, XV21738, XV21739).
'lo.uri^absoluteRsrc'('_call%3'(XV21740, XV21741, XV21742), 'lo.uri^absoluteRsrc', _):- 'lo.uri@absoluteRsrc'(XV21740, XV21741, XV21742).
'lo.uri@Hed80'(XHedStrm80, XNStrm1181, XNStrm1181, XHedStrm80):- ocall('_hdtl%3'(XHedStrm80, 64, XNStrm1181),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.uri^userStar'('_call%3'(XV21743, XV21744, XV21745), 'lo.uri^userStar', _):- 'lo.uri@userStar'(XV21743, XV21744, XV21745).
'lo.uri^userInfo'('_call%3'(XV21746, XV21747, XV21748), 'lo.uri^userInfo', _):- 'lo.uri@userInfo'(XV21746, XV21747, XV21748).
'lo.uri^dot'('_call%3'(XV21749, XV21750, XV21751), 'lo.uri^dot', _):- 'lo.uri@dot'(XV21749, XV21750, XV21751).
'lo.uri^minus'('_call%3'(XV21752, XV21753, XV21754), 'lo.uri^minus', _):- 'lo.uri@minus'(XV21752, XV21753, XV21754).
'lo.uri@Disj72'(XDjStrm74, XStx1252, XStx1253, XStx1252, XC):- 'lo.uri@minus'(XDjStrm74, XStx1252, XC).
'lo.uri@Disj72'(XDjStrm74, XStx1253, XStx1253, XStx1252, XC):- 'lo.uri@dot'(XDjStrm74, XStx1253, XC).
'lo.uri@Disj73'(XDjStrm73, XStx1251, XStx1253, XStx1252, XDjOut99, XStx1251, XC):- 'lo.uri@digit'(XDjStrm73, XStx1251, XC).
'lo.uri@Disj73'(XDjStrm73, XDjOut99, XStx1253, XStx1252, XDjOut99, XStx1251, XC):- 'lo.uri@Disj72'(XDjStrm73, XDjOut99, XStx1253, XStx1252, XC).
'lo.uri@Disj74'(XDjStrm72, XStx1250, XStx1253, XStx1252, XDjOut99, XStx1251, XDjOut98, XStx1250, XC):- 'lo.uri@alpha'(XDjStrm72, XStx1250, XC).
'lo.uri@Disj74'(XDjStrm72, XDjOut98, XStx1253, XStx1252, XDjOut99, XStx1251, XDjOut98, XStx1250, XC):- 'lo.uri@Disj73'(XDjStrm72, XDjOut98, XStx1253, XStx1252, XDjOut99, XStx1251, XC).
'lo.uri^alphaDashStar'('_call%3'(XV21755, XV21756, XV21757), 'lo.uri^alphaDashStar', _):- 'lo.uri@alphaDashStar'(XV21755, XV21756, XV21757).
'lo.uri@One26'(XOneStm26, XStx1255, XStx1255, XH):- 'lo.uri@alphaDashStar'(XOneStm26, XStx1255, XH),
    !.
'lo.uri^hostName'('_call%3'(XV21758, XV21759, XV21760), 'lo.uri^hostName', _):- 'lo.uri@hostName'(XV21758, XV21759, XV21760).
'lo.uri@Neg43'(XNegStrm43, XStx1258, X_22966):- 'lo.uri@digit'(XNegStrm43, XStx1258, X_22966),
    !,
    fail.
'lo.uri@Neg43'(XNegStrm43, XStx1258, X_22966).
'lo.uri^digits'('_call%3'(XV21761, XV21762, XV21763), 'lo.uri^digits', _):- 'lo.uri@digits'(XV21761, XV21762, XV21763).
'lo.uri^port'('_call%3'(XV21764, XV21765, XV21766), 'lo.uri^port', _):- 'lo.uri@port'(XV21764, XV21765, XV21766).
'lo.uri@Neg44'(XNegStrm44, XNStrm1185, XNStrm1185, XNegStrm44):- ocall('_hdtl%3'(XNegStrm44, 58, XNStrm1185),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list'),
    !,
    fail.
'lo.uri@Neg44'(XNegStrm44, XNStrm1185, XNStrm1185, XNegStrm44).
'lo.uri^hostNamePort'('_call%3'(XV21767, XV21768, XV21769), 'lo.uri^hostNamePort', _):- 'lo.uri@hostNamePort'(XV21767, XV21768, XV21769).
'lo.uri^authority'('_call%3'(XV21770, XV21771, XV21772), 'lo.uri^authority', _):- 'lo.uri@authority'(XV21770, XV21771, XV21772).
'lo.uri@Hed81'(XHedStrm81, XNStrm1187, XNStrm1187, XHedStrm81):- ocall('_hdtl%3'(XHedStrm81, 63, XNStrm1187),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.uri^optAbsolutePath'('_call%3'(XV21773, XV21774, XV21775), 'lo.uri^optAbsolutePath', _):- 'lo.uri@optAbsolutePath'(XV21773, XV21774, XV21775).
'lo.uri^netPath'('_call%3'(XV21776, XV21777, XV21778), 'lo.uri^netPath', _):- 'lo.uri@netPath'(XV21776, XV21777, XV21778).
'lo.uri^isReserved'('_call%1'(XV21779), 'lo.uri^isReserved', _):- 'lo.uri@isReserved'(XV21779).
'lo.uri^reserved'('_call%3'(XV21780, XV21781, XV21782), 'lo.uri^reserved', _):- 'lo.uri@reserved'(XV21780, XV21781, XV21782).
'lo.uri@Disj75'(XDjStrm75, XStx1269, XStx1270, XStx1269, XC):- 'lo.uri@reserved'(XDjStrm75, XStx1269, XC).
'lo.uri@Disj75'(XDjStrm75, XStx1270, XStx1270, XStx1269, XC):- 'lo.uri@unreserved'(XDjStrm75, XStx1270, XC).
'lo.uri^uric'('_call%4'(XV21783, XV21784, XV21785, XV21786), 'lo.uri^uric', _):- 'lo.uri@uric'(XV21783, XV21784, XV21785, XV21786).
'lo.uri^uricStar'('_call%3'(XV21787, XV21788, XV21789), 'lo.uri^uricStar', _):- 'lo.uri@uricStar'(XV21787, XV21788, XV21789).
'lo.uri^query'('_call%3'(XV21790, XV21791, XV21792), 'lo.uri^query', _):- 'lo.uri@query'(XV21790, XV21791, XV21792).
'lo.uri^relativeUri'('_call%4'(XV21793, XV21794, XV21795, XV21796), 'lo.uri^relativeUri', _):- 'lo.uri@relativeUri'(XV21793, XV21794, XV21795, XV21796).
'lo.uri^plus'('_call%3'(XV21797, XV21798, XV21799), 'lo.uri^plus', _):- 'lo.uri@plus'(XV21797, XV21798, XV21799).
'lo.uri@Disj76'(XDjStrm79, XStx1284, XStx1285, XStx1284, XC):- 'lo.uri@minus'(XDjStrm79, XStx1284, XC).
'lo.uri@Disj76'(XDjStrm79, XStx1285, XStx1285, XStx1284, XC):- 'lo.uri@dot'(XDjStrm79, XStx1285, XC).
'lo.uri@Disj77'(XDjStrm78, XStx1283, XStx1285, XStx1284, XDjOut105, XStx1283, XC):- 'lo.uri@plus'(XDjStrm78, XStx1283, XC).
'lo.uri@Disj77'(XDjStrm78, XDjOut105, XStx1285, XStx1284, XDjOut105, XStx1283, XC):- 'lo.uri@Disj76'(XDjStrm78, XDjOut105, XStx1285, XStx1284, XC).
'lo.uri@Disj78'(XDjStrm77, XStx1282, XStx1285, XStx1284, XDjOut105, XStx1283, XDjOut104, XStx1282, XC):- 'lo.uri@digit'(XDjStrm77, XStx1282, XC).
'lo.uri@Disj78'(XDjStrm77, XDjOut104, XStx1285, XStx1284, XDjOut105, XStx1283, XDjOut104, XStx1282, XC):- 'lo.uri@Disj77'(XDjStrm77, XDjOut104, XStx1285, XStx1284, XDjOut105, XStx1283, XC).
'lo.uri@Disj79'(XDjStrm76, XStx1281, XStx1285, XStx1284, XDjOut105, XStx1283, XDjOut104, XStx1282, XDjOut103, XStx1281, XC):- 'lo.uri@alpha'(XDjStrm76, XStx1281, XC).
'lo.uri@Disj79'(XDjStrm76, XDjOut103, XStx1285, XStx1284, XDjOut105, XStx1283, XDjOut104, XStx1282, XDjOut103, XStx1281, XC):- 'lo.uri@Disj78'(XDjStrm76, XDjOut103, XStx1285, XStx1284, XDjOut105, XStx1283, XDjOut104, XStx1282, XC).
'lo.uri^alphaStar'('_call%3'(XV21800, XV21801, XV21802), 'lo.uri^alphaStar', _):- 'lo.uri@alphaStar'(XV21800, XV21801, XV21802).
'lo.uri^grabScheme'('_call%3'(XV21803, XV21804, XV21805), 'lo.uri^grabScheme', _):- 'lo.uri@grabScheme'(XV21803, XV21804, XV21805).
'lo.uri@Hed82'(XHedStrm82, XNStrm1193, XNStrm1193, XHedStrm82):- ocall('_hdtl%3'(XHedStrm82, 58, XNStrm1193),'lo.core$stream$lo.core*list','lo.core$stream$lo.core*list').
'lo.uri^scheme'('_call%3'(XV21806, XV21807, XV21808), 'lo.uri^scheme', _):- 'lo.uri@scheme'(XV21806, XV21807, XV21808).
'lo.uri^hierPart'('_call%4'(XV21809, XV21810, XV21811, XV21812), 'lo.uri^hierPart', _):- 'lo.uri@hierPart'(XV21809, XV21810, XV21811, XV21812).
'lo.uri^absoluteUri'('_call%5'(XV21813, XV21814, XV21815, XV21816, XV21817), 'lo.uri^absoluteUri', _):- 'lo.uri@absoluteUri'(XV21813, XV21814, XV21815, XV21816, XV21817).
'lo.uri^uriParse'('_call%3'(XV21818, XV21819, XV21820), 'lo.uri^uriParse', _):- 'lo.uri@uriParse'(XV21818, XV21819, XV21820).
'lo.uri^parseUri'('_call%2'(XV21821, XV21822), 'lo.uri^parseUri', _):- 'lo.uri@parseUri'(XV21821, XV21822).
'lo.uri^isDelim'('_call%1'(XV21823), 'lo.uri^isDelim', _):- 'lo.uri@isDelim'(XV21823).
'lo.uri^delim'('_call%3'(XV21824, XV21825, XV21826), 'lo.uri^delim', _):- 'lo.uri@delim'(XV21824, XV21825, XV21826).
'lo.uri^edit'('_call%3'(XV21827, XV21828, XV21829), 'lo.uri^edit', _):- 'lo.uri@edit'(XV21827, XV21828, XV21829).
'lo.uri^resolvePath'('_call%3'(XV21830, XV21831, XV21832), 'lo.uri^resolvePath', _):- 'lo.uri@resolvePath'(XV21830, XV21831, XV21832).
'lo.uri^resolveUri'('_call%3'(XV21833, XV21834, XV21835), 'lo.uri^resolveUri', _):- 'lo.uri@resolveUri'(XV21833, XV21834, XV21835).
'lo.uri^dispSegs'('_call%2'(XV21836, XV21837), 'lo.uri^dispSegs', _):- 'lo.uri@dispSegs'(XV21836, XV21837).
'lo.uri^dispPath'('_call%2'(XV21838, XV21839), 'lo.uri^dispPath', _):- 'lo.uri@dispPath'(XV21838, XV21839).
'lo.uri^dispUser'('_call%2'(XV21840, XV21841), 'lo.uri^dispUser', _):- 'lo.uri@dispUser'(XV21840, XV21841).
'lo.uri^dispHost'('_call%2'(XV21842, XV21843), 'lo.uri^dispHost', _):- 'lo.uri@dispHost'(XV21842, XV21843).
'lo.uri^dispAuthority'('_call%2'(XV21844, XV21845), 'lo.uri^dispAuthority', _):- 'lo.uri@dispAuthority'(XV21844, XV21845).
'lo.uri^dispRsrc'('_call%2'(XV21846, XV21847), 'lo.uri^dispRsrc', _):- 'lo.uri@dispRsrc'(XV21846, XV21847).
'lo.core$display$lo.uri*uri^dispQuery'('_call%2'(XV21848, XV21849), 'lo.core$display$lo.uri*uri^dispQuery'(XLbV1900, XThV1900), _):- 'lo.core$display$lo.uri*uri@dispQuery'(XV21848, XV21849, XLbV1900, XThV1900).
'lo.core$display$lo.uri*uri^disp'('_call%2'(XV21852, XV21853), 'lo.core$display$lo.uri*uri^disp'(XLbV1900, XThV1900), _):- 'lo.core$display$lo.uri*uri@disp'(XV21852, XV21853, XLbV1900, XThV1900).
'lo.uri^getUriPath'('_call%2'(XV21856, XV21857), 'lo.uri^getUriPath', _):- 'lo.uri@getUriPath'(XV21856, XV21857).
'lo.coerce$coercion$lo.uri*uri$lo.core*string^_coerce'('_call%2'(XV21858, XV21859), 'lo.coerce$coercion$lo.uri*uri$lo.core*string^_coerce'(XLbV1901, XThV1901), _):- 'lo.coerce$coercion$lo.uri*uri$lo.core*string@_coerce'(XV21858, XV21859, XLbV1901, XThV1901).
