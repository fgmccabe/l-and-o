'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.transform'e'*'n23o23'()23'n2o2'import'e'private'n2o2'pkg's'lo.comp.types'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.transutils'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.package'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.misc'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.freevars'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.escapes'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.errors'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.debug'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.canon'e'*'n2o2'import'e'public'n2o2'pkg's'lo.repo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.args'e'*'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I1'transformProg'PT5t'lo.comp.canon*canonPkg't'lo.comp.args*compOption't'lo.comp.term*prProg't'lo.comp.errors*report't'lo.comp.errors*report'\"s'I0'n0o0'()0'n0o0'()0'n1o1'()1'n2o2'()2's'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc's\"c'lo.coerce$coercion'T2t'lo.comp.location*location't'lo.comp.term*tloc'T0\"").
'lo.comp.transform@init'() :- !.
'lo.comp.transform@implementVarPtn'('lo.comp.transutils#localVar'(XVn, X_1130, XClVr, XTVr), XNm, XTLc, XX, XQ, XX14382, 'lo.core#,..'('lo.comp.term#call'(XTLc, 'lo.comp.term#prg'(XVn, 3), 'lo.core#,..'(XX, 'lo.core#,..'(XClVr, 'lo.core#,..'(XTVr, 'lo.core#[]')))), XPre), XPre, XPost, XPost, XRp, XRp) :- 'lo.comp.transutils@genVarbl'(XNm, XX),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#,..'(XClVr, 'lo.core#,..'(XTVr, 'lo.core#[]'))), XQ, XX14382).
'lo.comp.transform@implementVarPtn'('lo.comp.transutils#moduleVar'(X_1131, XVn, X_1132), XNm, XTLc, XX, XQ, 'lo.core#,..'(XX, XQ), 'lo.core#,..'('lo.comp.term#call'(XTLc, 'lo.comp.term#prg'(XVn, 1), 'lo.core#,..'(XX, 'lo.core#[]')), XPre), XPre, XPost, XPost, XRp, XRp) :- 'lo.comp.transutils@genVarbl'(XNm, XX).
'lo.comp.transform@implementVarPtn'('lo.comp.transutils#labelArg'(XN, XClVr, XTVr), X_1133, X_1134, XN, XQ, XX14446, XPre, XPre, XPost, XPost, XRp, XRp) :- 'lo.list@merge'('lo.core#,..'(XN, 'lo.core#,..'(XClVr, 'lo.core#,..'(XTVr, 'lo.core#[]'))), XQ, XX14446).
'lo.comp.transform@implementVarPtn'('lo.comp.transutils#moduleClass'(XEnum, X_1135, 0), X_1136, X_1137, 'lo.comp.term#enum'(XEnum), XQ, XQ, XPre, XPre, XPost, XPost, XRp, XRp).
'lo.comp.transform@implementVarPtn'('lo.comp.transutils#localClass'(XEnum, X_1138, 0, XLbVr, XThVr), X_1139, X_1140, 'lo.comp.term#cons'('lo.comp.term#strct'(XEnum, 2), 'lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XX14490, XPre, XPre, XPost, XPost, XRp, XRp) :- 'lo.list@merge'('lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XX14490).
'lo.comp.transform@implementVarPtn'('lo.comp.transutils#inherit'(XNm, X_1141, XLbVr, XThVr), X_1142, X_1143, 'lo.comp.term#cons'('lo.comp.term#strct'(XNm, 2), 'lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XX14519, XPre, XPre, XPost, XPost, XRp, XRp) :- 'lo.list@merge'('lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XX14519).
'lo.comp.transform@implementVarPtn'('lo.comp.transutils#notInMap', XNm, X_1144, 'lo.comp.term#varbl'(XNm), XQ, XX14537, XPre, XPre, XPost, XPost, XRp, XRp) :- 'lo.list@merge'('lo.core#,..'('lo.comp.term#varbl'(XNm), 'lo.core#[]'), XQ, XX14537).
'lo.comp.transform@trVarPtn'(X_1145, "_", 'lo.comp.term#anon', XQ, XQ, XPre, XPre, XPost, XPost, X_1146, X_1147, XRp, XRp).
'lo.comp.transform@trVarPtn'(XLc, XNm, XA, XQ, XQx, XPre, XPrx, XPost, XPstx, XMap, X_1148, XRp, XRpx) :- 'lo.comp.transutils@lookupVarName'(XMap, XNm, XV),
    ocall('_coerce%2'(XLc, XX14575),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.comp.transform@implementVarPtn'(XV, XNm, 'lo.core#some'(XX14575), XA, XQ, XQx, XPre, XPrx, XPost, XPstx, XRp, XRpx).
'lo.comp.transform@trVarPtn'(XLc, XNm, 'lo.comp.term#anon', XQ, XQ, XPre, XPre, XPost, XPost, X_1149, X_1150, XRp, XRpx) :- ocall('disp%2'(XNm, XX14602),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("'"), 'lo.core#,..'(XX14602, 'lo.core#,..'('lo.core#ss'("' not defined"), 'lo.core#[]')))), XX14610),
    'lo.comp.errors@reportError'(XX14610, XLc, XRp, XRpx).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#localVar'(XVn, X_1151, XLblVr, XThVr), XLc, XNm, XX, XQ, XX14631, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XX14633), 'lo.comp.term#prg'(XVn, 3), 'lo.core#,..'(XX, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')))), XPre), XPre, XTail, XTail, XRp, XRp) :- 'lo.comp.transutils@genVarbl'(XNm, XX),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XX14631),
    ocall('_coerce%2'(XLc, XX14633),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc').
'lo.comp.transform@implementVarExp'('lo.comp.transutils#moduleVar'(X_1152, XV, X_1153), XLc, XNm, XX, XQ, XX14667, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XX14669), 'lo.comp.term#prg'(XV, 1), 'lo.core#,..'(XX, 'lo.core#[]')), XPre), XPre, XTail, XTail, XRp, XRp) :- 'lo.comp.transutils@genVarbl'(XNm, XX),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#[]'), XQ, XX14667),
    ocall('_coerce%2'(XLc, XX14669),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc').
'lo.comp.transform@implementVarExp'('lo.comp.transutils#labelArg'(XN, XLblVr, XThVar), X_1154, X_1155, XN, XQ, XX14703, XPre, XPre, XTail, XTail, XRp, XRp) :- 'lo.list@merge'('lo.core#,..'(XN, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVar, 'lo.core#[]'))), XQ, XX14703).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#inheritField'(XSuper, XLblVr, XThVr), XLc, XNm, XX, XQ, XX14726, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XX14728), XSuper, 'lo.core#,..'('lo.comp.term#cons'(XX14733, 'lo.core#,..'(XX, 'lo.core#[]')), 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')))), XPre), XPre, XTail, XTail, XRp, XRp) :- 'lo.comp.transutils@genVarbl'(XNm, XX),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XX14726),
    ocall('_coerce%2'(XLc, XX14728),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.comp.transutils@trCons'(XNm, 1, XX14733).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#moduleClass'(XEnum, X_1156, 0), X_1157, X_1158, 'lo.comp.term#enum'(XEnum), XQ, XQ, XPre, XPre, XTail, XTail, XRp, XRp).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#moduleImpl'(X_1159, 'lo.comp.term#enum'(XEnum)), X_1160, X_1161, 'lo.comp.term#enum'(XEnum), XQ, XQ, XPre, XPre, XTail, XTail, XRp, XRp).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#localClass'(XEnum, X_1162, X_1163, XLbVr, XThVr), X_1164, X_1165, 'lo.comp.term#cons'('lo.comp.term#strct'(XEnum, 2), 'lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XX14808, XPre, XPre, XTail, XTail, XRp, XRp) :- 'lo.list@merge'('lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XX14808).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#inherit'(XNm, X_1166, XLbVr, XThVr), X_1167, X_1168, 'lo.comp.term#cons'('lo.comp.term#strct'(XNm, 2), 'lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XX14837, XPre, XPre, XTail, XTail, XRp, XRp) :- 'lo.list@merge'('lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XX14837).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#notInMap', X_1169, XNm, 'lo.comp.term#varbl'(XNm), XQ, XX14855, XPre, XPre, XTail, XTail, XRp, XRp) :- 'lo.list@merge'('lo.core#,..'('lo.comp.term#varbl'(XNm), 'lo.core#[]'), XQ, XX14855).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#moduleFun'(X_1170, X_1171, X_1172, XAcc, X_1173), X_1174, X_1175, 'lo.comp.term#enum'(XAcc), XQ, XQ, XPre, XPre, XTail, XTail, XRp, XRp).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#moduleRel'(X_1176, X_1177, X_1178, XAcc, X_1179), X_1180, X_1181, 'lo.comp.term#enum'(XAcc), XQ, XQ, XPre, XPre, XTail, XTail, XRp, XRp).
'lo.comp.transform@implementVarExp'('lo.comp.transutils#localFun'(X_1182, X_1183, XClosure, X_1184, XLblVr, XThVr), X_1185, X_1186, 'lo.comp.term#cons'('lo.comp.term#strct'(XClosure, 2), 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XQ, XPre, XPre, XTail, XTail, XRp, XRp).
'lo.comp.transform@implementVarExp'(X_1187, XLc, XNm, 'lo.comp.term#varbl'(XNm), XQ, XQ, XPre, XPre, XTail, XTail, XRp, XRpx) :- ocall('disp%2'(XNm, XX14938),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot handle "), 'lo.core#,..'(XX14938, 'lo.core#,..'('lo.core#ss'(" in expression"), 'lo.core#[]')))), XX14946),
    'lo.comp.errors@reportError'(XX14946, XLc, XRp, XRpx).
'lo.comp.transform@trVarExp'(X_1188, "_", 'lo.comp.term#anon', XQ, XQ, XPre, XPre, XPost, XPost, X_1189, X_1190, XRp, XRp).
'lo.comp.transform@trVarExp'(XLc, XNm, XExp, XQ, XQx, XPre, XPrx, XPost, XPstx, XMap, X_1191, XRp, XRpx) :- 'lo.comp.transutils@lookupVarName'(XMap, XNm, XV),
    'lo.comp.transform@implementVarExp'(XV, XLc, XNm, XExp, XQ, XQx, XPre, XPrx, XPost, XPstx, XRp, XRpx).
'lo.comp.transform@trVarExp'(XLc, XNm, 'lo.comp.term#anon', XQ, XQ, XPre, XPre, XPost, XPost, X_1192, X_1193, XRp, XRpx) :- ocall('disp%2'(XNm, XX15005),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("'"), 'lo.core#,..'(XX15005, 'lo.core#,..'('lo.core#ss'("' not defined"), 'lo.core#[]')))), XX15013),
    'lo.comp.errors@reportError'(XX15013, XLc, XRp, XRpx).
'lo.comp.transform@implementFunCall'('lo.comp.transutils#localFun'(XFn, X_1194, X_1195, XAr, XLblVr, XThVr), XLc, X_1196, XX, XArgs, XX, XQ, XX15038, XPre, XPx, XTail, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XX15043), 'lo.comp.term#prg'(XFn, XX15050), XX15061), XTailx), XPre, XPx, XTail, XTailx, X_1197, X_1198, XEx, XEx, XRp, XRp) :- 'lo.list@merge'('lo.core#,..'(XX, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XX15038),
    ocall('_coerce%2'(XLc, XX15043),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('size%2'(XArgs, XX15048),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%3'(XX15048, 3, XX15050),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XX, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XX15061).
'lo.comp.transform@implementFunCall'('lo.comp.transutils#moduleFun'(X_1199, XFn, X_1200, X_1201, XAr), XLc, X_1202, XX, XArgs, XX, XQ, XX15091, XPre, XPx, XTail, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XX15096), 'lo.comp.term#prg'(XFn, XX15103), XX15110), XTailx), XPre, XPx, XTail, XTailx, X_1203, X_1204, XEx, XEx, XRp, XRp) :- 'lo.list@merge'('lo.core#,..'(XX, 'lo.core#[]'), XQ, XX15091),
    ocall('_coerce%2'(XLc, XX15096),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('size%2'(XArgs, XX15101),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%3'(XX15101, 1, XX15103),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XX, 'lo.core#[]'), XX15110).
'lo.comp.transform@implementFunCall'('lo.comp.transutils#inheritField'(XSuper, XLblVr, XThVr), XLc, XNm, XX, XArgs, XX, XQ, XX15142, XPre, XPx, XTail, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XX15147), XSuper, 'lo.core#,..'('lo.comp.term#cons'(XX15156, XX15161), 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')))), XTailx), XPre, XPx, XTail, XTailx, X_1205, X_1206, XEx, XEx, XRp, XRp) :- 'lo.list@merge'('lo.core#,..'(XX, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XX15142),
    ocall('_coerce%2'(XLc, XX15147),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.list@length'(XArgs, XX15153),
    ocall('+%3'(XX15153, 1, XX15154),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.transutils@trCons'(XNm, XX15154, XX15156),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XX, 'lo.core#[]'), XX15161).
'lo.comp.transform@implementFunCall'('lo.comp.transutils#moduleClass'(XMdl, X_1207, X_1208), X_1209, X_1210, X_1211, XArgs, 'lo.comp.term#cons'('lo.comp.term#strct'(XMdl, XX15192), XArgs), XQ, XQ, XPre, XPx, XTail, XTailx, XPre, XPx, XTail, XTailx, X_1212, X_1213, XEx, XEx, XRp, XRp) :- ocall('size%2'(XArgs, XX15192),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list').
'lo.comp.transform@implementFunCall'('lo.comp.transutils#localClass'(XMdl, X_1214, X_1215, XLbVr, XThVr), X_1216, X_1217, X_1218, XArgs, 'lo.comp.term#cons'('lo.comp.term#strct'(XMdl, XX15227), XX15236), XQ, XX15245, XPre, XPx, XTail, XTailx, XPre, XPx, XTail, XTailx, X_1219, X_1220, XEx, XEx, XRp, XRp) :- ocall('size%2'(XArgs, XX15225),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%3'(XX15225, 2, XX15227),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XX15236),
    'lo.list@merge'('lo.core#,..'(XLbvr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XX15245).
'lo.comp.transform@implementFunCall'('lo.comp.transutils#inherit'(XMdl, X_1221, XLbVr, XThVr), X_1222, X_1223, X_1224, XArgs, 'lo.comp.term#cons'('lo.comp.term#strct'(XMdl, XX15271), XArgs), XQ, XX15283, XPre, XPx, XTail, XTailx, XPre, XPx, XTail, XTailx, X_1225, X_1226, XEx, XEx, XRp, XRp) :- ocall('size%2'(XArgs, XX15271),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    'lo.list@merge'('lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XX15283).
'lo.comp.transform@implementFunCall'('lo.comp.transutils#moduleImpl'(X_1227, XMdl), X_1228, X_1229, X_1230, 'lo.core#[]', XMdl, XQ, XQ, XPre, XPx, XTail, XTailx, XPre, XPx, XTail, XTailx, X_1231, X_1232, XEx, XEx, XRp, XRp).
'lo.comp.transform@implementFunCall'('lo.comp.transutils#moduleImpl'(X_1233, XMdl), X_1234, X_1235, X_1236, XArgs, 'lo.comp.term#cons'(XMdl, XArgs), XQ, XQ, XPre, XPx, XTail, XTailx, XPre, XPx, XTail, XTailx, X_1237, X_1238, XEx, XEx, XRp, XRp).
'lo.comp.transform@joinStream'(XX, XX, XG, XG).
'lo.comp.transform@joinStream'(XStrm, XStrmx, 'lo.core#,..'('lo.comp.term#unfy'('lo.core#none', XStrm, XStrmx), XGx), XGx).
'lo.comp.transform@mkCanon'('lo.comp.term#varbl'(XNm), 'lo.comp.canon#v'('lo.comp.location#std', XNm)) :- !.
'lo.comp.transform@mkCanon'(_, _) :- raise_exception('error'("mkCanon", 260, 3, 29)).
'lo.comp.transform@mkClosure'(XLam, 'lo.core#[]', 'lo.comp.term#enum'(XLam)) :- !.
'lo.comp.transform@mkClosure'(XLam, XFreeVars, 'lo.comp.term#cons'('lo.comp.term#strct'(XLam, XX15374), XFreeVars)) :- !,
    ocall('size%2'(XFreeVars, XX15374),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list').
'lo.comp.transform@mkClosure'(_, _, _) :- raise_exception('error'("mkClosure", 627, 3, 30)).
'lo.comp.transform@genRaise'(XLc, XLclName, 'lo.core#,..'('lo.comp.term#except'('lo.core#some'(XX15382), 'lo.comp.term#cons'('lo.comp.term#strct'("error", 4), 'lo.core#,..'('lo.comp.term#strng'(XLclName), 'lo.core#,..'('lo.comp.term#intgr'(XX15389), 'lo.core#,..'('lo.comp.term#intgr'(XX15392), 'lo.core#,..'('lo.comp.term#intgr'(XX15395), 'lo.core#[]')))))), XP), XP, XRp, XRp) :- ocall('_coerce%2'(XLc, XX15382),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.comp.location@lineOf'(XLc, XX15389),
    'lo.comp.location@columnOf'(XLc, XX15392),
    'lo.comp.location@widthOf'(XLc, XX15395).
'lo.comp.transform@failSafeEquation'(XLc, XNm, XLclPrg, XArity, 'lo.core#,..'('lo.comp.term#clse'('lo.core#[]', 'lo.comp.term#prg'(XLclPrg, XArity), XX15418, XG), XRest), XRest, XRp, XRpx) :- 'lo.comp.transform@genRaise'(XLc, XLclPrg, XG, 'lo.core#[]', XRp, XRpx),
    'lo.comp.transutils@genAnons'(XArity, XX15418).
'lo.comp.transform@implementPtnCall'('lo.comp.transutils#localFun'(XFn, X_1239, X_1240, XAr, XLblVr, XThVr), X_1241, XTLc, XX, XArgs, XX, XQ, XX15453, XPre, XPx, XTail, 'lo.core#,..'('lo.comp.term#call'(XTLc, 'lo.comp.term#prg'(XFn, XX15462), XX15473), XTailx), XPre, XPx, XTail, XTailx, XRp, XRp) :- 'lo.list@merge'('lo.core#,..'(XX, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XX15453),
    ocall('size%2'(XArgs, XX15460),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%3'(XX15460, 3, XX15462),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XX, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XX15473).
'lo.comp.transform@implementPtnCall'('lo.comp.transutils#moduleFun'(X_1242, XFn, X_1243, X_1244, XAr), X_1245, XTLc, XX, XArgs, XX, XQ, XX15499, XPre, XPx, XTail, 'lo.core#,..'('lo.comp.term#call'(XTLc, 'lo.comp.term#prg'(XFn, XX15508), XX15515), XTailx), XPre, XPx, XTail, XTailx, XRp, XRp) :- 'lo.list@merge'('lo.core#,..'(XX, 'lo.core#[]'), XQ, XX15499),
    ocall('size%2'(XArgs, XX15506),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%3'(XX15506, 1, XX15508),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XX, 'lo.core#[]'), XX15515).
'lo.comp.transform@implementPtnCall'('lo.comp.transutils#inheritField'(XSuper, XLblVr, XThVr), XNm, XTLc, XX, XArgs, XX, XQ, XX15543, XPre, XPx, XTail, 'lo.core#,..'('lo.comp.term#call'(XTLc, XSuper, 'lo.core#,..'('lo.comp.term#cons'(XX15554, XX15559), 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')))), XTailx), XPre, XPx, XTail, XTailx, XRp, XRp) :- 'lo.list@merge'('lo.core#,..'(XX, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XX15543),
    'lo.list@length'(XArgs, XX15551),
    ocall('+%3'(XX15551, 1, XX15552),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.transutils@trCons'(XNm, XX15552, XX15554),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XX, 'lo.core#[]'), XX15559).
'lo.comp.transform@implementPtnCall'('lo.comp.transutils#moduleClass'(XMdl, X_1246, X_1247), X_1248, X_1249, X_1250, XArgs, 'lo.comp.term#cons'('lo.comp.term#strct'(XMdl, XX15586), XArgs), XQ, XQ, XPre, XPx, XTail, XTailx, XPre, XPx, XTail, XTailx, XRp, XRp) :- ocall('size%2'(XArgs, XX15586),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list').
'lo.comp.transform@implementPtnCall'('lo.comp.transutils#localClass'(XAcc, X_1251, X_1252, XLbVr, XThVr), X_1253, X_1254, X_1255, XArgs, 'lo.comp.term#cons'('lo.comp.term#strct'(XAcc, XX15617), XX15626), XQ, XX15635, XPre, XPx, XTail, XTailx, XPre, XPx, XTail, XTailx, XRp, XRp) :- ocall('size%2'(XArgs, XX15615),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%3'(XX15615, 2, XX15617),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XX15626),
    'lo.list@merge'('lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XX15635).
'lo.comp.transform@implementPtnCall'('lo.comp.transutils#inherit'(XNm, X_1256, XLbVr, XThVr), X_1257, X_1258, X_1259, XArgs, 'lo.comp.term#cons'('lo.comp.term#strct'(XNm, XX15657), XArgs), XQ, XX15669, XPre, XPx, XTail, XTailx, XPre, XPx, XTail, XTailx, XRp, XRp) :- ocall('size%2'(XArgs, XX15657),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    'lo.list@merge'('lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XX15669).
'lo.comp.transform@implementPtnCall'('lo.comp.transutils#moduleImpl'(X_1260, XMdl), X_1261, X_1262, X_1263, XArgs, 'lo.comp.term#cons'(XMdl, XArgs), XQ, XQ, XPre, XPx, XTail, XTailx, XPre, XPx, XTail, XTailx, XRp, XRp).
'lo.comp.transform@trPtnCallOp'(XNm, XTLc, XArgs, XX, XQ, XX15711, XPre, XPx, XTail, 'lo.core#,..'('lo.comp.term#ecall'(XTLc, XNm, XX15721), XTailx), XPre, XPx, XTail, XTailx, X_1264, X_1265, XEx, XEx, XRp, XRp) :- 'lo.comp.escapes@isEscape'(XNm),
    'lo.comp.transutils@genVarbl'("Xa", XX),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#[]'), XQ, XX15711),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XX, 'lo.core#[]'), XX15721).
'lo.comp.transform@trPtnCallOp'(XNm, XTLc, XArgs, XPtn, XQ, XQx, XAPre, XAPx, XAPost, XAPstx, XPre, XPx, XTail, XTailx, XMap, X_1266, XEx, XEx, XRp, XRpx) :- 'lo.comp.transutils@lookupFunName'(XMap, XNm, XReslt),
    'lo.comp.transutils@genVarbl'("Xb", XX),
    'lo.comp.transform@implementPtnCall'(XReslt, XNm, XTLc, XX, XArgs, XPtn, XQ, XQx, XAPre, XAPx, XAPost, XAPstx, XPre, XPx, XTail, XTailx, XRp, XRpx).
'lo.comp.transform@trLambda'('lo.comp.canon#equation'(XLc, XNm, 'lo.comp.canon#tpl'(XA), XExp, XCond), XClosure, XQ, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.freevars@freeVarsInRule'('lo.comp.canon#equation'(XLc, XNm, 'lo.comp.canon#tpl'(XA), XExp, XCond), XQ, 'lo.core#[]', XX15804),
    XFreeVars = XX15804,
    'lo.comp.transutils@genNewName'(XMap, XNm, 3, 'lo.comp.term#prg'(XLam, X_1267)),
    'lo.comp.transform@trPtns'(XA, XArgs, 'lo.core#,..'(XRep, 'lo.core#[]'), 'lo.core#[]', XQ1, XGoals, XPreGx, XPostG, 'lo.core#[]', XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XCond, XPreGx, 'lo.core#,..'('lo.comp.term#neck', XPostGx), XQ1, XQ2, XMap, XOpts, XEx0, XEx1, XRp0, XRp1),
    'lo.comp.transform@trExp'(XExp, XRep, XQ2, XQ3, XPostGx, XPVx, XPVx, XPostG, XMap, XOpts, XEx1, XEx2, XRp1, XRp2),
    'lo.comp.transform@mkClosure'(XLam, XFreeVars, XX15857),
    XClosure = XX15857,
    'lo.list@merge'(XFreeVars, XQ3, XX15861),
    ocall('size%2'(XArgs, XX15865),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    'lo.comp.transutils@trCons'("_call", XX15865, XX15867),
    XEx2 = 'lo.core#,..'('lo.comp.term#clse'(XX15861, 'lo.comp.term#prg'(XLam, 3), 'lo.core#,..'('lo.comp.term#cons'(XX15867, XArgs), 'lo.core#,..'(XClosure, 'lo.core#,..'('lo.comp.term#anon', 'lo.core#[]'))), XGoals), XEx3),
    'lo.comp.transform@failSafeEquation'(XLc, 'lo.comp.term#strng'(XNm), XLam, 3, XEx3, XExx, XRp2, XRpx).
'lo.comp.transform@trLambda'('lo.comp.canon#clause'(XLc, XNm, 'lo.comp.canon#tpl'(XA), XCond), XClosure, XQ, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.freevars@freeVarsInRule'('lo.comp.canon#clause'(XLc, XNm, 'lo.comp.canon#tpl'(XA), XCond), XQ, 'lo.core#[]', XX15911),
    XFreeVars = XX15911,
    'lo.comp.transutils@genNewName'(XMap, XNm, 3, 'lo.comp.term#prg'(XLam, X_1268)),
    'lo.comp.transform@trPtns'(XA, XArgs, 'lo.core#[]', 'lo.core#[]', XQ1, XGoals, XPreGx, XPostG, 'lo.core#[]', XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XCond, XPreGx, XPostG, XQ1, XQ2, XMap, XOpts, XEx0, XEx1, XRp0, XRpx),
    'lo.comp.transform@mkClosure'(XLam, XFreeVars, XX15946),
    XClosure = XX15946,
    'lo.list@merge'(XFreeVars, XQ2, XX15950),
    ocall('size%2'(XArgs, XX15954),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    'lo.comp.transutils@trCons'("_call", XX15954, XX15956),
    XEx1 = 'lo.core#,..'('lo.comp.term#clse'(XX15950, 'lo.comp.term#prg'(XLam, 3), 'lo.core#,..'('lo.comp.term#cons'(XX15956, XArgs), 'lo.core#,..'(XClosure, 'lo.core#,..'('lo.comp.term#anon', 'lo.core#[]'))), XGoals), XExx).
'lo.comp.transform@trLambda'('lo.comp.canon#grRule'(XLc, XNm, 'lo.comp.canon#tpl'(XA), 'lo.comp.canon#grTerms'(XPB), XBody), XClosure, XQ, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.freevars@freeVarsInRule'('lo.comp.canon#grRule'(XLc, XNm, 'lo.comp.canon#tpl'(XA), 'lo.comp.canon#grTerms'(XPB), XBody), XQ, 'lo.core#[]', XX15996),
    XFreeVars = XX15996,
    'lo.comp.transutils@genNewName'(XMap, XNm, 3, 'lo.comp.term#prg'(XLam, X_1269)),
    'lo.comp.transform@trPtns'(XA, XArgs, 'lo.core#[]', 'lo.core#[]', XQ1, XGoals, XPreGx, XPostG, XG7, XMap, XClOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transutils@genVarbl'("StIn", XStIn),
    'lo.comp.transform@dcgBody'(XBody, XMap, XOpts, XPreGx, XPostG, XStIn, XStOut, 'lo.core#,..'(XStIn, XQ1), XQ2, XEx0, XEx1, XRp0, XRp1),
    'lo.comp.transform@pushTerminals'(XPB, XMap, XOpts, XG7, 'lo.core#[]', XStOut, XStX, XQ2, XQ4, XEx1, XEx2, XRp1, XRpx),
    'lo.comp.transform@mkClosure'(XLam, XFreeVars, XX16049),
    XClosure = XX16049,
    ocall('size%2'(XArgs, XX16052),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%3'(XX16052, 2, XX16054),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.transutils@trCons'("_call", XX16054, XX16056),
    XCallStrct = 'lo.comp.term#cons'(XX16056, 'lo.core#,..'(XStIn, 'lo.core#,..'(XStX, XArgs))),
    'lo.list@merge'(XFreeVars, XQ4, XX16066),
    XEx2 = 'lo.core#,..'('lo.comp.term#clse'(XX16066, 'lo.comp.term#prg'(XLam, 3), 'lo.core#,..'(XCallStrct, 'lo.core#,..'(XClosure, 'lo.core#,..'('lo.comp.term#anon', 'lo.core#[]'))), XGoals), XExx).
'lo.comp.transform@trGoalDot'(X_1270, 'lo.comp.canon#v'(XLc, XNm), XC, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XX16086), XSuper, 'lo.core#,..'(XC, 'lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')))), XGx), XGx, XQ, XX16108, XMap, X_1271, XEx, XEx, XRp, XRp) :- 'lo.comp.transutils@lookupVarName'(XMap, XNm, 'lo.comp.transutils#inherit'(X_1272, XSuper, XLbVr, XThVr)),
    ocall('_coerce%2'(XLc, XX16086),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.list@merge'('lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XX16108).
'lo.comp.transform@trGoalDot'(XLc, XRec, XC, XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- ocall('_coerce%2'(XLc, XX16143),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.comp.transform@trExp'(XRec, XNR, XQ, XQx, XG, XG0, XG0, 'lo.core#,..'('lo.comp.term#ocall'('lo.core#some'(XX16143), XC, XNR, XNR), XGx), XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@implementGoalCall'('lo.comp.transutils#localRel'(XFn, X_1273, X_1274, XAr, XLblVr, XThVr), XLc, X_1275, XArgs, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XX16169), 'lo.comp.term#prg'(XFn, XX16176), XX16185), XTail), XTail, XQ, XX16197, X_1276, X_1277, XEx, XEx, XRp, XRp) :- ocall('_coerce%2'(XLc, XX16169),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('size%2'(XArgs, XX16174),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%3'(XX16174, 2, XX16176),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XX16185),
    'lo.list@merge'('lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XX16197).
'lo.comp.transform@implementGoalCall'('lo.comp.transutils#moduleRel'(X_1278, XFn, X_1279, X_1280, XAr), XLc, X_1281, XArgs, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XX16214), 'lo.comp.term#prg'(XFn, XX16219), XArgs), XTail), XTail, XQ, XQ, X_1282, X_1283, XEx, XEx, XRp, XRp) :- ocall('_coerce%2'(XLc, XX16214),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    ocall('size%2'(XArgs, XX16219),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list').
'lo.comp.transform@implementGoalCall'('lo.comp.transutils#inheritField'(XSuper, XLblVr, XThVr), XLc, XPred, XArgs, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XX16243), XSuper, 'lo.core#,..'('lo.comp.term#cons'(XX16250, XArgs), 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')))), XTail), XTail, XQ, XX16270, X_1284, X_1285, XEx, XEx, XRp, XRp) :- ocall('_coerce%2'(XLc, XX16243),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.list@length'(XArgs, XX16249),
    'lo.comp.transutils@trCons'(XPred, XX16249, XX16250),
    'lo.list@merge'('lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XQ, XX16270).
'lo.comp.transform@implementGoalCall'('lo.comp.transutils#notInMap', XLc, XPred, XArgs, XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.list@length'(XArgs, XX16296),
    'lo.comp.transutils@trCons'("_call", XX16296, XX16297),
    'lo.comp.transform@trGoalDot'(XLc, 'lo.comp.canon#v'(XLc, XPred), 'lo.comp.term#cons'(XX16297, XArgs), XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@implementGoalCall'(X_1286, XLc, XPred, X_1287, XG, XG, XQ, XQ, X_1288, X_1289, XEx, XEx, XRp, XRpx) :- ocall('disp%2'(XPred, XX16326),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("cannot handle source for "), 'lo.core#,..'(XX16326, 'lo.core#[]'))), XX16332),
    'lo.comp.errors@reportError'(XX16332, XLc, XRp, XRpx).
'lo.comp.transform@trGoalCall'('lo.comp.canon#v'(XLc, XNm), XArgs, 'lo.core#,..'('lo.comp.term#ecall'('lo.core#some'(XX16341), XNm, XArgs), XTail), XTail, XQ, XQ, X_1290, X_1291, XEx, XEx, XRp, XRp) :- 'lo.comp.escapes@isEscape'(XNm),
    ocall('_coerce%2'(XLc, XX16341),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc').
'lo.comp.transform@trGoalCall'('lo.comp.canon#v'(XLc, XNm), XArgs, XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transutils@lookupRelName'(XMap, XNm, XRSpec),
    'lo.comp.transform@implementGoalCall'(XRSpec, XLc, XNm, XArgs, XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@trGoalCall'('lo.comp.canon#dot'(XLc, XRec, XPred), XArgs, XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.list@length'(XArgs, XX16409),
    'lo.comp.transutils@trCons'(XPred, XX16409, XX16410),
    'lo.comp.transform@trGoalDot'(XLc, XRec, 'lo.comp.term#cons'(XX16410, XArgs), XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@dcgAhead'(XTst, XMap, XOpts, 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XHdProg, 'lo.core#,..'(XStrm, XTQ)), XG), XG, XStrm, XQ, XX16439, XEx, XExx, XRp, XRpx) :- 'lo.comp.transutils@genVarbl'("HedStrm", XHedStrm),
    'lo.comp.transform@dcgBody'(XTst, XMap, XOpts, XTG, 'lo.core#[]', XHedStrm, X_1292, 'lo.core#[]', XTQ, XEx, 'lo.core#,..'(XC1, XExx), XRp, XRpx),
    'lo.list@length'(XTQ, XX16462),
    ocall('+%3'(XX16462, 1, XX16463),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.transutils@genNewName'(XMap, "Hed", XX16463, XHdProg),
    XC1 = 'lo.comp.term#clse'('lo.core#,..'(XHedStrm, XTQ), XHdProg, 'lo.core#,..'(XHedStrm, XTQ), XTG),
    'lo.list@merge'(XTQ, XQ, XX16439).
'lo.comp.transform@dcgNeg'(XTst, XMap, XOpts, 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XNegProg, 'lo.core#,..'(XStrm, XTQ)), XG), XG, XStrm, XQ, XX16492, XEx, XExx, XRp, XRpx) :- 'lo.comp.transutils@genVarbl'("NegStrm", XNegStrm),
    'lo.comp.transform@dcgBody'(XTst, XMap, XOpts, XTG, 'lo.core#,..'('lo.comp.term#neck', 'lo.core#,..'('lo.comp.term#fail', 'lo.core#[]')), XNegStrm, X_1293, 'lo.core#[]', XTQ, XEx, 'lo.core#,..'(XC1, 'lo.core#,..'(XC2, XExx)), XRp, XRpx),
    'lo.list@length'(XTQ, XX16521),
    ocall('+%3'(XX16521, 1, XX16522),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.transutils@genNewName'(XMap, "Neg", XX16522, XNegProg),
    XC1 = 'lo.comp.term#clse'('lo.core#,..'(XNegStrm, XTQ), XNegProg, 'lo.core#,..'(XNegStrm, XTQ), XTG),
    XC2 = 'lo.comp.term#clse'('lo.core#,..'(XNegStrm, XTQ), XNegProg, 'lo.core#,..'(XNegStrm, XTQ), 'lo.core#[]'),
    'lo.list@merge'(XTQ, XQ, XX16492).
'lo.comp.transform@dcgOne'(XLhs, XMap, XOpts, 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XOneProg, 'lo.core#,..'(XStrm, 'lo.core#,..'(XStrmx, XDQ))), XG), XG, XStrm, XStrmx, XQ, XX16564, XEx, XExx, XRp, XRpx) :- 'lo.comp.transutils@genVarbl'("OneStm", XOneStm),
    'lo.comp.transform@cond7'(XStrmx),
    'lo.comp.transform@dcgBody'(XLhs, XMap, XOpts, XLG, 'lo.core#,..'('lo.comp.term#neck', 'lo.core#[]'), XOneStm, XOneStmx, 'lo.core#[]', XDQ, XEx, 'lo.core#,..'(XC1, XExx), XRp, XRpx),
    'lo.list@length'(XDQ, XX16591),
    ocall('+%3'(XX16591, 2, XX16592),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.transutils@genNewName'(XMap, "One", XX16592, XOneProg),
    XC1 = 'lo.comp.term#clse'('lo.core#,..'(XOneStm, XDQ), XOneProg, 'lo.core#,..'(XOneStm, 'lo.core#,..'(XOneStmx, XDQ)), XLG),
    'lo.list@merge'(XDQ, XQ, XX16564).
'lo.comp.transform@dcgConditional'(XTst, XLhs, XRhs, XMap, XOpts, 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XCondProg, 'lo.core#,..'(XStrm, 'lo.core#,..'(XStrmx, XDQ))), XG), XG, XStrm, XStrmx, XQ, XX16628, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@cond8'(XStrmx),
    'lo.comp.transutils@genVarbl'("CondStrm", XCndStrm),
    'lo.comp.transform@dcgBody'(XTst, XMap, XOpts, XTG, 'lo.core#,..'('lo.comp.term#neck', XLG), XCndStrm, XCndStrm0, 'lo.core#[]', XTQ, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@dcgBody'(XLhs, XMap, XOpts, XLG, 'lo.core#[]', XCndStrm0, XCndStrmx, XTQ, XLQ, XEx0, XEx1, XRp0, XRp1),
    'lo.comp.transform@dcgBody'(XRhs, XMap, XOpts, XRG, 'lo.core#[]', XCndStrm, XCndStrmy, XLQ, XDQ, XEx1, 'lo.core#,..'(XC1, 'lo.core#,..'(XC2, XExx)), XRp1, XRpx),
    'lo.list@length'(XDQ, XX16683),
    ocall('+%3'(XX16683, 2, XX16684),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.transutils@genNewName'(XMap, "Cond", XX16684, XCondProg),
    XC1 = 'lo.comp.term#clse'('lo.core#,..'(XCndStrm, XDQ), XCondProg, 'lo.core#,..'(XCndStrm, 'lo.core#,..'(XCndStrmx, XDQ)), XTG),
    XC2 = 'lo.comp.term#clse'('lo.core#,..'(XCndStrm, XDQ), XCondProg, 'lo.core#,..'(XCndStrm, 'lo.core#,..'(XCndStrmy, XDQ)), XRG),
    'lo.list@merge'(XDQ, XQ, XX16628).
'lo.comp.transform@dcgDisj'(XLhs, XRhs, XMap, XOpts, 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XDisProg, 'lo.core#,..'(XStrm, 'lo.core#,..'(XStrmx, XDQ))), XG), XG, XStrm, XStrmx, XQ, XX16731, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@cond9'(XQ, XQ0, XStrmx),
    'lo.comp.transutils@genVarbl'("DjStrm", XDjStrm),
    'lo.comp.transform@dcgBody'(XLhs, XMap, XOpts, XLG, 'lo.core#[]', XDjStrm, XDjStrmx, 'lo.core#[]', XLQ, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@dcgBody'(XRhs, XMap, XOpts, XRG, 'lo.core#[]', XDjStrm, XDjStrmy, XLQ, XDQ, XEx0, XEx1, XRp0, XRpx),
    'lo.list@length'(XDQ, XX16773),
    ocall('+%3'(XX16773, 2, XX16774),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.transutils@genNewName'(XMap, "Disj", XX16774, XDisProg),
    XC1 = 'lo.comp.term#clse'('lo.core#,..'(XDjStrm, XDQ), XDisProg, 'lo.core#,..'(XDjStrm, 'lo.core#,..'(XDjStrmx, XDQ)), XLG),
    XC2 = 'lo.comp.term#clse'('lo.core#,..'(XDjStrm, XDQ), XDisProg, 'lo.core#,..'(XDjStrm, 'lo.core#,..'(XDjStrmy, XDQ)), XRG),
    XEx1 = 'lo.core#,..'(XC1, 'lo.core#,..'(XC2, XExx)),
    'lo.list@merge'(XDQ, XQ0, XX16731).
'lo.comp.transform@pushTerminals'('lo.core#[]', X_1294, X_1295, XG, XGx, XStrm, XStrmx, XQ, XQ, XEx, XEx, XRp, XRp) :- 'lo.comp.transform@joinStream'(XStrm, XStrmx, XG, XGx).
'lo.comp.transform@pushTerminals'('lo.core#,..'((XLc, XSV, XT), XMore), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transutils@genVarbl'("NStrm", XNStrm),
    'lo.comp.transform@mkCanon'(XStrm, XX16845),
    'lo.comp.transform@mkCanon'(XNStrm, XX16848),
    'lo.comp.transform@trGoal'('lo.comp.canon#callCond'(XLc, XSV, 'lo.comp.canon#tpl'('lo.core#,..'(XX16845, 'lo.core#,..'(XT, 'lo.core#,..'(XX16848, 'lo.core#[]'))))), XG, XG0, XQ, XQ0, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@pushTerminals'(XMore, XMap, XOpts, XG0, XGx, XNStrm, XStrmx, 'lo.core#,..'(XNStrm, XQ0), XQx, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@dcgBody'('lo.comp.canon#grTerms'(XTerms), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@pushTerminals'(XTerms, XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@dcgBody'('lo.comp.canon#grConj'(XLhs, XRhs), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@dcgBody'(XLhs, XMap, XOpts, XG, XG0, XStrm, XStrm0, XQ, XQ0, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@dcgBody'(XRhs, XMap, XOpts, XG0, XGx, XStrm0, XStrmx, XQ0, XQx, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@dcgBody'('lo.comp.canon#grDisj'(XLhs, XRhs), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@dcgDisj'(XLhs, XRhs, XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@dcgBody'('lo.comp.canon#grCond'(XTst, XLhs, XRhs), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@dcgConditional'(XTst, XLhs, XRhs, XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@dcgBody'('lo.comp.canon#grOne'(XTst), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@dcgOne'(XTst, XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@dcgBody'('lo.comp.canon#grNeg'(XTst), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@joinStream'(XStrm, XStrmx, XG, XG0),
    'lo.comp.transform@dcgNeg'(XTst, XMap, XOpts, XG0, XGx, XStrm, XQ, XQx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@dcgBody'('lo.comp.canon#grAhed'(XTst), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@joinStream'(XStrm, XStrmx, XG, XG0),
    'lo.comp.transform@dcgAhead'(XTst, XMap, XOpts, XG0, XGx, XStrm, XQ, XQx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@dcgBody'('lo.comp.canon#grTest'(XGoal), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@joinStream'(XStrm, XStrmx, XG, XG0),
    'lo.comp.transform@trGoal'(XGoal, XG0, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@dcgBody'('lo.comp.canon#grDip'(XV, XCond), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@joinStream'(XStrm, XStrmx, XG, XG0),
    'lo.comp.transform@trExp'(XV, XStrmVr, XQ, XQ0, XG0, XG1, XG1, 'lo.core#,..'('lo.comp.term#unfy'('lo.core#none', XStrm, XStrmVr), XG2), XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XCond, XG2, XGx, XQ0, XQx, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@dcgBody'('lo.comp.canon#grCall'(XLc, XNT, 'lo.comp.canon#tpl'(XArgs)), XMap, XOpts, XG, XGx, XStrm, XStrmx, XQ, XQx, XEx, XExx, XRp, XRpx) :- 'lo.comp.debug@lineDebug'(XLc, XG, XG0, XOpts),
    'lo.comp.transform@trExps'(XArgs, XAG, 'lo.core#[]', XQ, XQ0, XG0, XPr, XPr, XG3, XMap, XOpts, XEx, XEx0, XRp, XRpx),
    'lo.comp.transform@cond10'(XQ0, XQ1, XStrmx),
    'lo.comp.transform@trGoalCall'(XNT, 'lo.core#,..'(XStrm, 'lo.core#,..'(XStrmx, XAG)), XG3, XGx, XQ1, XQx, XMap, XOpts, XEx0, XExx, XRp, XRpx).
'lo.comp.transform@trGoal'('lo.comp.canon#trueCond', XGoals, XGoals, XQ, XQ, X_1296, X_1297, XEx, XEx, XRp, XRp).
'lo.comp.transform@trGoal'('lo.comp.canon#falseCond', 'lo.core#,..'('lo.comp.term#fail', XRest), XRest, XQ, XQ, X_1298, X_1299, XEx, XEx, XRp, XRp).
'lo.comp.transform@trGoal'('lo.comp.canon#conjCond'(XL, XR), XGoals, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@trGoal'(XL, XGoals, XG0, XQ, XQ0, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XR, XG0, XGx, XQ0, XQx, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@trGoal'('lo.comp.canon#disjCond'(XL, XR), 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XDisjPr, XLQ), XGx), XGx, XQ, XX17305, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@trGoal'(XL, XLG, 'lo.core#[]', 'lo.core#[]', XQ0, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XR, XRG, 'lo.core#[]', XQ0, XLQ, XMap, XOpts, XEx0, XEx1, XRp0, XRpx),
    'lo.list@length'(XLQ, XX17336),
    'lo.comp.transutils@genNewName'(XMap, "or", XX17336, XDisjPr),
    XCl1 = 'lo.comp.term#clse'(XLQ, XDisjPr, XLQ, XLG),
    XCl2 = 'lo.comp.term#clse'(XLQ, XDisjPr, XLQ, XRG),
    XEx1 = 'lo.core#,..'(XCl1, 'lo.core#,..'(XCl2, XExx)),
    'lo.list@merge'(XLQ, XQ, XX17305).
'lo.comp.transform@trGoal'('lo.comp.canon#condCond'(XT, XL, XR), 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XCondPr, XLQ), XGx), XGx, XQ, XX17370, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@trGoal'(XT, XTG, 'lo.core#,..'('lo.comp.term#neck', XLG), 'lo.core#[]', XQ0, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XL, XLG, 'lo.core#[]', XQ0, XQ1, XMap, XOpts, XEx0, XEx1, XRp0, XRp1),
    'lo.comp.transform@trGoal'(XR, XRG, 'lo.core#[]', XQ1, XLQ, XMap, XOpts, XEx1, XEx2, XRp1, XRpx),
    'lo.list@length'(XLQ, XX17414),
    'lo.comp.transutils@genNewName'(XMap, "cond", XX17414, XCondPr),
    XCl1 = 'lo.comp.term#clse'(XLQ, XCondPr, XLQ, XTG),
    XCl2 = 'lo.comp.term#clse'(XLQ, XCondPr, XLQ, XRG),
    XEx2 = 'lo.core#,..'(XCl1, 'lo.core#,..'(XCl2, XExx)),
    'lo.list@merge'(XLQ, XQ, XX17370).
'lo.comp.transform@trGoal'('lo.comp.canon#oneCond'(XT), 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XOnePr, XLQ), XGx), XGx, XQ, XX17446, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@trGoal'(XT, XTG, 'lo.core#,..'('lo.comp.term#neck', 'lo.core#[]'), 'lo.core#[]', XLQ, XMap, XOpts, XEx, XEx0, XRp, XRpx),
    'lo.list@length'(XLQ, XX17468),
    'lo.comp.transutils@genNewName'(XMap, "one", XX17468, XOnePr),
    XCl1 = 'lo.comp.term#clse'(XLQ, XOnePr, XLQ, XTG),
    XEx0 = 'lo.core#,..'(XCl1, XExx),
    'lo.list@merge'(XLQ, XQ, XX17446).
'lo.comp.transform@trGoal'('lo.comp.canon#negCond'(XT), 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XNegPr, XLQ), XGx), XGx, XQ, XX17492, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@trGoal'(XT, XTG, 'lo.core#,..'('lo.comp.term#neck', 'lo.core#,..'('lo.comp.term#fail', 'lo.core#[]')), 'lo.core#[]', XLQ, XMap, XOpts, XEx, XEx0, XRp, XRpx),
    'lo.list@length'(XLQ, XX17516),
    'lo.comp.transutils@genNewName'(XMap, "neg", XX17516, XNegPr),
    XCl1 = 'lo.comp.term#clse'(XLQ, XNegPr, XLQ, XTG),
    XCl2 = 'lo.comp.term#clse'(XLQ, XNegPr, XLQ, 'lo.core#[]'),
    XEx0 = 'lo.core#,..'(XCl1, 'lo.core#,..'(XCl2, XExx)),
    'lo.list@merge'(XLQ, XQ, XX17492).
'lo.comp.transform@trGoal'('lo.comp.canon#forallCond'(XL, XR), 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XAPr, XLQ), XGx), XGx, XQ, XX17549, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@trGoal'(XL, XLG, 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XBPr, XLQ), 'lo.core#,..'('lo.comp.term#neck', 'lo.core#,..'('lo.comp.term#fail', 'lo.core#[]'))), 'lo.core#[]', XQ0, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XR, XRG, 'lo.core#,..'('lo.comp.term#neck', 'lo.core#,..'('lo.comp.term#fail', 'lo.core#[]')), XQ0, XLQ, XMap, XOpts, XEx0, XEx1, XRp0, XRpx),
    'lo.list@length'(XLQ, XX17593),
    XQAr = XX17593,
    'lo.comp.transutils@genNewName'(XMap, "forallA", XQAr, XAPr),
    'lo.comp.transutils@genNewName'(XMap, "forallB", XQAr, XBPr),
    XACl1 = 'lo.comp.term#clse'(XLQ, XAPr, XLQ, XLG),
    XACl2 = 'lo.comp.term#clse'(XLQ, XAPr, XLQ, 'lo.core#[]'),
    XBCl1 = 'lo.comp.term#clse'(XLQ, XBPr, XLQ, XRG),
    XBCl2 = 'lo.comp.term#clse'(XLQ, XBPr, XLQ, 'lo.core#[]'),
    XEx1 = 'lo.core#,..'(XACl1, 'lo.core#,..'(XACl2, 'lo.core#,..'(XBCl1, 'lo.core#,..'(XBCl2, XExx)))),
    'lo.list@merge'(XLQ, XQ, XX17549).
'lo.comp.transform@trGoal'('lo.comp.canon#unifyCond'(XLc, XL, XR), XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- ocall('_coerce%2'(XLc, XX17651),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.comp.debug@lineDebug'(XLc, XG3, 'lo.core#,..'('lo.comp.term#unfy'('lo.core#some'(XX17651), XLx, XRx), XGx), XOpts),
    'lo.comp.transform@trExp'(XL, XLx, XQ, XQ0, XG, XG0, XG0, XG1, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trExp'(XR, XRx, XQ0, XQx, XG1, XG2, XG2, XG3, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@trGoal'('lo.comp.canon#phraseCond'(XLc, XNT, XStrm, XRem), XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@trExp'(XStrm, XStIn, XQ, XQ0, XG, XG0, XG0, XG1, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.debug@lineDebug'(XLc, XG1, XG2, XOpts),
    'lo.comp.transform@dcgBody'(XNT, XMap, XOpts, XG2, XG3, XStIn, XStOut, XQ0, XQ2, XEx0, XEx1, XRp0, XRp1),
    'lo.comp.transform@trExp'(XRem, XOut, XQ2, XQx, XG3, XG4, XG4, XG5, XMap, XOpts, XEx1, XExx, XRp1, XRpx),
    'lo.comp.transform@joinStream'(XOut, XStOut, XG5, XGx).
'lo.comp.transform@trGoal'('lo.comp.canon#callCond'(XLc, XPred, 'lo.comp.canon#tpl'(XArgs)), XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.debug@lineDebug'(XLc, XG, XG0, XOpts),
    'lo.comp.transform@trExps'(XArgs, XAG, 'lo.core#[]', XQ, XQ0, XG0, XPr, XPr, XG3, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoalCall'(XPred, XAG, XG3, XGx, XQ0, XQx, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@trGoal'('lo.comp.canon#isTrue'(XE), XG, XGx, XQ, XQx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@trExp'(XE, XExp, XQ, XQx, XG, XG0, XG0, 'lo.core#,..'('lo.comp.term#unfy'('lo.core#none', XExp, 'lo.comp.term#enum'("lo.core#true")), XGx), XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@trExpCallOp'(XLc, 'lo.comp.canon#v'(X_1300, XNm), XArgs, XX, XQ, XX17840, XPre, XPx, XTail, 'lo.core#,..'('lo.comp.term#ecall'('lo.core#some'(XX17845), XNm, XX17853), XTailx), XPre, XPx, XTail, XTailx, X_1301, X_1302, XEx, XEx, XRp, XRp) :- 'lo.comp.escapes@isEscape'(XNm),
    'lo.comp.transutils@genVarbl'("Xc", XX),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#[]'), XQ, XX17840),
    ocall('_coerce%2'(XLc, XX17845),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XX, 'lo.core#[]'), XX17853).
'lo.comp.transform@trExpCallOp'(XLc, 'lo.comp.canon#v'(X_1303, XNm), XArgs, XExp, XQ, XX17880, XAPre, XAPx, XAPost, XAPstx, XPre, XPx, XTail, XTailx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transutils@lookupFunName'(XMap, XNm, XReslt),
    'lo.comp.transutils@genVarbl'("Xd", XX),
    'lo.comp.transform@implementFunCall'(XReslt, XLc, XNm, XX, XArgs, XExp, XQ, XQx, XAPre, XAPx, XAPost, XAPstx, XPre, XPx, XTail, XTailx, XMap, XOpts, XEx, XExx, XRp, XRpx),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#[]'), XQx, XX17880).
'lo.comp.transform@trExpCallOp'(XLc, XT, XArgs, XX, XQ, XX17930, XPre, XAPx, XTail, 'lo.core#,..'('lo.comp.term#ocall'('lo.core#some'(XX17935), XC, XCl, XCl), XTailx), XPre, XPx, XTail, XTailx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@trExp'(XT, XCl, XQ, XQx, XAPx, XRx, XRx, XPx, XMap, XOpts, XEx, XExx, XRp, XRpx),
    'lo.comp.transutils@genVarbl'("Xe", XX),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XX, 'lo.core#[]'), XX17974),
    XXArgs = XX17974,
    'lo.list@length'(XXArgs, XX17977),
    'lo.comp.transutils@trCons'("_call", XX17977, XX17978),
    XC = 'lo.comp.term#cons'(XX17978, XXArgs),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#[]'), XQx, XX17930),
    ocall('_coerce%2'(XLc, XX17935),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc').
'lo.comp.transform@trExps'('lo.core#[]', XArgs, XArgs, XQ, XQ, XPre, XPre, XPost, XPost, X_1304, X_1305, XEx, XEx, XRp, XRp).
'lo.comp.transform@trExps'('lo.core#,..'(XP, XMore), 'lo.core#,..'(XA, XArgs), XExtra, XQ, XQx, XPre, XPrx, XPost, XPsx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@one20'(XRp0, XRp, XEx0, XEx, XOpts, XMap, XPst0, XPost, XPre0, XPre, XQ0, XQ, XA, XP),
    'lo.comp.transform@trExps'(XMore, XArgs, XExtra, XQ0, XQx, XPre0, XPrx, XPst0, XPsx, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@trExp'('lo.comp.canon#v'(X_1306, "this"), XThVr, XQ, XX18052, XPre, XPre, XPost, XPost, XMap, X_1307, XEx, XEx, XRp, XRp) :- 'lo.comp.transutils@thisVar'(XMap, XThVr),
    'lo.list@merge'('lo.core#,..'(XThVr, 'lo.core#[]'), XQ, XX18052).
'lo.comp.transform@trExp'('lo.comp.canon#v'(XLc, XNm), XVr, XQ, XQx, XPre, XPx, XPost, XPstx, XMap, XOpts, XEx, XEx, XRp, XRpx) :- 'lo.comp.transform@trVarExp'(XLc, XNm, XVr, XQ, XQx, XPre, XPx, XPost, XPstx, XMap, XOpts, XRp, XRpx).
'lo.comp.transform@trExp'('lo.comp.canon#int'(XIx), 'lo.comp.term#intgr'(XIx), XQ, XQ, XPre, XPre, XPost, XPost, X_1308, X_1309, XEx, XEx, XRp, XRp).
'lo.comp.transform@trExp'('lo.comp.canon#flt'(XIx), 'lo.comp.term#flot'(XIx), XQ, XQ, XPre, XPre, XPost, XPost, X_1310, X_1311, XEx, XEx, XRp, XRp).
'lo.comp.transform@trExp'('lo.comp.canon#str'(XIx), 'lo.comp.term#strng'(XIx), XQ, XQ, XPre, XPre, XPost, XPost, X_1312, X_1313, XEx, XEx, XRp, XRp).
'lo.comp.transform@trExp'('lo.comp.canon#tpl'(XA), XX18145, XQ, XQx, XPre, XPx, XPost, XPstx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@trExps'(XA, XTA, 'lo.core#[]', XQ, XQx, XPre, XPx, XPost, XPstx, XMap, XOpts, XEx, XExx, XRp, XRpx),
    'lo.comp.term@mkTpl'(XTA, XX18145).
'lo.comp.transform@trExp'('lo.comp.canon#apply'(XLc, XOp, 'lo.comp.canon#tpl'(XA)), XExp, XQ, XQx, XPre, XPx, XPost, XPstx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@trExps'(XA, XArgs, 'lo.core#[]', XQ, XQ0, XAPre, XAPx, XAPost, XAPostx, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trExpCallOp'(XLc, XOp, XArgs, XExp, XQ0, XQx, XAPre, XAPx, XAPost, XAPostx, XPre, XPx, XPost, XPstx, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@trExp'('lo.comp.canon#dot'(XLc, XRec, XFld), XExp, XQ, XX18236, XPre, XPx, XTail, XTailx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transutils@genVarbl'("XV", XX),
    'lo.comp.transutils@trCons'(XFld, 1, XX18251),
    'lo.comp.transform@trDotExp'(XLc, XRec, 'lo.comp.term#cons'(XX18251, 'lo.core#,..'(XX, 'lo.core#[]')), XX, XExp, XQ, XQx, XPre, XPx, XTail, XTailx, XMap, XOpts, XEx, XExx, XRp, XRpx),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#[]'), XQx, XX18236).
'lo.comp.transform@trExp'('lo.comp.canon#whre'(XP, XC), XPtn, XQ, XQx, XPre, XPx, XPost, XPstx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@trExp'(XP, XPtn, XQ, XQ0, XPre, XP0, XPost, XPstx, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XC, XP0, XPx, XQ0, XQx, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@trExp'('lo.comp.canon#condExp'(XT, XL, XR), XRslt, XQ, XX18321, XPre, XPrx, XPost, XPost, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transutils@genVarbl'("CndV", XRslt),
    'lo.comp.debug@lineDebug'(XLc, XPre, 'lo.core#,..'('lo.comp.term#call'('lo.core#none', XCondPr, 'lo.core#,..'(XRslt, XLQ)), XPrx), XOpts),
    'lo.comp.transform@trGoal'(XT, XTG, 'lo.core#,..'('lo.comp.term#neck', XLG), 'lo.core#[]', XQ0, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trExp'(XL, XLRslt, XQ0, XQ1, XLG, XLx, XLx, 'lo.core#[]', XMap, XOpts, XEx0, XEx1, XRp0, XRp1),
    'lo.comp.transform@trExp'(XR, XRRslt, XQ1, XLQ, XRG, XRx, XRx, 'lo.core#[]', XMap, XOpts, XEx1, XEx2, XRp1, XRpx),
    'lo.list@length'(XLQ, XX18387),
    'lo.comp.transutils@genNewName'(XMap, "condExp", XX18387, XCondPr),
    XCl1 = 'lo.comp.term#clse'(XLQ, XCondPr, 'lo.core#,..'(XLRslt, XLQ), XTG),
    XCl2 = 'lo.comp.term#clse'(XLQ, XCondPr, 'lo.core#,..'(XRRslt, XLQ), XRG),
    XEx2 = 'lo.core#,..'(XCl1, 'lo.core#,..'(XCl2, XExx)),
    'lo.list@merge'('lo.core#,..'(XRslt, XLQ), XQ, XX18321).
'lo.comp.transform@trExp'('lo.comp.canon#lambda'(XRl), XRslt, XQ, XQ, XPr, XPr, XPost, XPost, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transutils@allLabelArgs'(XMap, 'lo.core#[]', XX18430),
    'lo.list@merge'(XX18430, XQ, XX18432),
    'lo.comp.transform@trLambda'(XRl, XRslt, XX18432, XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@trExp'(XXX, 'lo.comp.term#anon', XQ, XQ, XPre, XPre, XPost, XPost, X_1314, X_1315, XEx, XEx, XRp, XRpx) :- ocall('disp%2'(XXX, XX18455),'lo.core$display$lo.comp.canon*canonTerm','lo.core$display$lo.comp.canon*canonTerm'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("internal: cannot transform "), 'lo.core#,..'(XX18455, 'lo.core#,..'('lo.core#ss'(" as expression"), 'lo.core#[]')))), XX18463),
    'lo.comp.errors@reportError'(XX18463, 'lo.comp.location#std', XRp, XRpx).
'lo.comp.transform@implementDotExp'('lo.comp.transutils#inherit'(X_1316, XSuper, XClVr, XThVr), X_1317, XTLc, XC, XX, XX, XQ, XX18486, XPre, XPre, 'lo.core#,..'('lo.comp.term#call'(XTLc, XSuper, 'lo.core#,..'(XC, 'lo.core#,..'(XClVr, 'lo.core#,..'(XThVr, 'lo.core#[]')))), XTail), XTail, X_1318, X_1319, XEx, XEx, XRp, XRp) :- 'lo.list@merge'('lo.core#,..'(XX, 'lo.core#,..'(XClVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XQ, XX18486).
'lo.comp.transform@implementDotExp'(X_1320, XR, XTLc, XC, XX, XX, XQ, XX18519, XPre, XPx, XTail, XTailx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@trExp'(XR, XRc, XQ, XQ0, XPre, XPx, XTail, 'lo.core#,..'('lo.comp.term#ocall'(XTLc, XC, XRc, XRc), XTailx), XMap, XOpts, XEx, XExx, XRp, XRpx),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#[]'), XQ0, XX18519).
'lo.comp.transform@trDotExp'(XLc, 'lo.comp.canon#v'(X_1321, XNm), XC, XX, XExp, XQ, XQx, XPre, XPx, XTail, XTailx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transutils@lookupVarName'(XMap, XNm, XReslt),
    ocall('_coerce%2'(XLc, XX18577),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.comp.transform@implementDotExp'(XReslt, 'lo.comp.canon#v'(XLc, XNm), 'lo.core#some'(XX18577), XC, XX, XExp, XQ, XQx, XPre, XPx, XTail, XTailx, XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@trDotExp'(XLc, XR, XC, XX, XX, XQ, XX18605, XPre, XPx, XTail, XTailx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- ocall('_coerce%2'(XLc, XX18624),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.comp.transform@trExp'(XR, XRc, XQ, XQ0, XPre, XPx, XTail, 'lo.core#,..'('lo.comp.term#ocall'('lo.core#some'(XX18624), XC, XRc, XRc), XTailx), XMap, XOpts, XEx, XExx, XRp, XRpx),
    'lo.list@merge'('lo.core#,..'(XX, 'lo.core#[]'), XQ0, XX18605).
'lo.comp.transform@trPtn'('lo.comp.canon#v'(X_1322, "this"), XThVr, XQ, XX18647, XPre, XPre, XPost, XPost, XMap, X_1323, XEx, XEx, XRp, XRp) :- 'lo.comp.transutils@thisVar'(XMap, XThVr),
    'lo.list@merge'('lo.core#,..'(XThVr, 'lo.core#[]'), XQ, XX18647).
'lo.comp.transform@trPtn'('lo.comp.canon#v'(XLc, "this"), 'lo.comp.term#anon', XQ, XQ, XPre, XPre, XPost, XPost, X_1324, X_1325, XEx, XEx, XRp, XRpx) :- 'lo.comp.errors@reportError'("'this' not defined here", XLc, XRp, XRpx).
'lo.comp.transform@trPtn'('lo.comp.canon#v'(XLc, XNm), XA, XQ, XQx, XPre, XPrx, XPost, XPstx, XMap, XOpts, XEx, XEx, XRp, XRpx) :- 'lo.comp.transform@trVarPtn'(XLc, XNm, XA, XQ, XQx, XPre, XPrx, XPost, XPstx, XMap, XOpts, XRp, XRpx).
'lo.comp.transform@trPtn'('lo.comp.canon#int'(XIx), 'lo.comp.term#intgr'(XIx), XQ, XQ, XPre, XPre, XPost, XPost, X_1326, X_1327, XEx, XEx, XRp, XRp).
'lo.comp.transform@trPtn'('lo.comp.canon#flt'(XIx), 'lo.comp.term#flot'(XIx), XQ, XQ, XPre, XPre, XPost, XPost, X_1328, X_1329, XEx, XEx, XRp, XRp).
'lo.comp.transform@trPtn'('lo.comp.canon#str'(XIx), 'lo.comp.term#strng'(XIx), XQ, XQ, XPre, XPre, XPost, XPost, X_1330, X_1331, XEx, XEx, XRp, XRp).
'lo.comp.transform@trPtn'('lo.comp.canon#dot'(XLc, XRc, XFld), XExp, XQ, XQx, XPre, XPx, XPost, XPost, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transutils@genVarbl'("XV", XX),
    'lo.comp.transutils@trCons'(XFld, 1, XX18776),
    'lo.comp.transform@trDotExp'(XLc, XRc, 'lo.comp.term#cons'(XX18776, 'lo.core#,..'(XX, 'lo.core#[]')), XX, XExp, XQ, XQx, XPre, XPi, XPi, XPx, XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@trPtn'('lo.comp.canon#tpl'(XPtns), XX18798, XQ, XQx, XPre, XPx, XPost, XPostx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@trPtns'(XPtns, XP, 'lo.core#[]', XQ, XQx, XPre, XPx, XPost, XPostx, XMap, XOpts, XEx, XExx, XRp, XRpx),
    'lo.comp.term@mkTpl'(XP, XX18798).
'lo.comp.transform@trPtn'('lo.comp.canon#apply'(XLc, 'lo.comp.canon#v'(X_1332, XNm), 'lo.comp.canon#tpl'(XA)), XPtn, XQ, XQx, XPre, XPx, XPost, XPstx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@trPtns'(XA, XArgs, 'lo.core#[]', XQ, XQ0, XAPre, XAP0, XAPost, XAPs0, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    ocall('_coerce%2'(XLc, XX18863),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.comp.transform@trPtnCallOp'(XNm, 'lo.core#some'(XX18863), XArgs, XPtn, XQ0, XQx, XAPre, XAP0, XAPost, XAPs0, XPre, XPx, XPost, XPstx, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@trPtn'('lo.comp.canon#whre'(XP, XC), XPtn, XQ, XQx, XPre, XPx, XPost, XPstx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@trPtn'(XP, XPtn, XQ, XQ0, XPre, XP0, XPost, XPstx, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XC, XP0, XPx, XQ0, XQx, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@trPtn'(XXX, XExp, XQ, XQx, XPre, XPre, XPost, XPostx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@trExp'(XXX, XExp, XQ, XQx, XPost, XPi, XPi, XPostx, XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@trPtns'('lo.core#[]', XArgs, XArgs, XQ, XQ, XPre, XPre, XPost, XPost, X_1333, X_1334, XEx, XEx, XRp, XRp).
'lo.comp.transform@trPtns'('lo.core#,..'(XP, XMore), 'lo.core#,..'(XA, XArgs), XAx, XQ, XQx, XPre, XPrx, XPost, XPsx, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@one21'(XRp0, XRp, XEx0, XEx, XOpts, XMap, XPst0, XPost, XPre0, XPre, XQ0, XQ, XA, XP),
    'lo.comp.transform@trPtns'(XMore, XArgs, XAx, XQ0, XQx, XPre0, XPrx, XPst0, XPsx, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@transformEqn'('lo.comp.canon#equation'(XLc, XNm, 'lo.comp.canon#tpl'(XA), XValue, XCond), XMap, XOpts, XLclFun, XArity, XExtra, XQNo, 'lo.core#,..'('lo.comp.term#clse'(XQ, 'lo.comp.term#prg'(XLclFun, XX19035), XArgs, XBody), XRx), XRx, XEx, XExx, XRp, XRpx) :- 'lo.comp.debug@debugPreamble'(XNm, XExtra, XQ0, XLbLx, XFBg, XOpts, XClOpts),
    'lo.comp.transform@trPtns'(XA, XArgs, 'lo.core#,..'(XRep, XExtra), XQ0, XQ1, XPreG, XPreGx, XPostG, XPreV, XMap, XClOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XCond, XPreGx, 'lo.core#,..'('lo.comp.term#neck', XCGx), XQ1, XQ2, XMap, XClOpts, XEx0, XEx1, XRp0, XRp1),
    'lo.comp.transform@trExp'(XValue, XRep, XQ2, XQ3, XPreV, XPVx, XPVx, XPx, XMap, XClOpts, XEx1, XExx, XRp1, XRpx),
    'lo.comp.transutils@labelAccess'(XQ3, XQ, XMap, XBody, XLbLx),
    'lo.comp.debug@frameDebug'(XNm, XQNo, XQ, XFBg, XLG, XClOpts),
    'lo.comp.debug@lineDebug'(XLc, XLG, XPreG, XClOpts),
    'lo.comp.debug@deframeDebug'(XNm, XQNo, XPx, 'lo.core#[]', XClOpts),
    'lo.comp.debug@breakDebug'(XNm, XCGx, XPostG, XClOpts),
    ocall('size%2'(XA, XX19125),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%3'(XX19125, 1, XX19127),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    XArity = XX19127,
    ocall('size%2'(XExtra, XX19033),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%3'(XArity, XX19033, XX19035),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer').
'lo.comp.transform@transformEquations'(X_1335, X_1336, X_1337, X_1338, X_1339, 'lo.core#[]', X_1340, XRules, XRules, XEx, XEx, XRp, XRp).
'lo.comp.transform@transformEquations'(XMap, XOpts, XLclFun, XArity, XExtra, 'lo.core#,..'(XEqn, XDefs), XNo, XRules, XRx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@transformEqn'(XEqn, XMap, XOpts, XLclFun, XArity, XExtra, XNo, XRules, XR0, XEx, XEx0, XRp, XRp0),
    ocall('+%3'(XNo, 1, XX19177),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.transform@transformEquations'(XMap, XOpts, XLclFun, X_1341, XExtra, XDefs, XX19177, XR0, XRx, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@closureEntry'(XMap, XTLc, XProg, XClosure, XArity, 'lo.core#,..'('lo.comp.term#clse'(XQ, 'lo.comp.term#prg'(XClosure, 3), 'lo.core#,..'(XCallStrct, 'lo.core#,..'(XClosureCons, 'lo.core#,..'('lo.comp.term#anon', 'lo.core#[]'))), 'lo.core#,..'('lo.comp.term#call'(XTLc, 'lo.comp.term#prg'(XProg, XX19203), XQ), 'lo.core#[]')), XL), XL) :- 'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transutils@genVars'(XArity, XX19217),
    XArgs = XX19217,
    'lo.list@<>'(XArgs, XExtra, XX19221),
    XQ = XX19221,
    'lo.comp.transutils@trCons'("_call", XArity, XX19224),
    XCallStrct = 'lo.comp.term#cons'(XX19224, XArgs),
    'lo.comp.transform@cond11'(XX19235, XClosure, XClosureCons, XExtra),
    'lo.list@length'(XQ, XX19203).
'lo.comp.transform@transformFunction'('lo.comp.canon#funDef'(XLc, XNm, XTp, 'lo.core#[]', XEqns), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transutils@lookupFunName'(XMap, XNm, XReslt),
    'lo.comp.transutils@programAccess'(XReslt, XLclFun, X_1342, XClosure, X_1343),
    'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transform@transformEquations'(XMap, XOpts, XLclFun, XAr, XExtra, XEqns, 1, XRules, XR0, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@failSafeEquation'(XLc, 'lo.comp.term#strng'(XNm), XLclFun, XAr, XR0, XRx, XRp0, XRpx),
    ocall('_coerce%2'(XLc, XX19287),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.comp.transform@closureEntry'(XMap, 'lo.core#some'(XX19287), XLclFun, XClosure, XAr, XEx0, XExx).
'lo.comp.transform@transformGrammarRule'('lo.comp.canon#grRule'(XLc, XNm, 'lo.comp.canon#tpl'(XA), 'lo.comp.canon#grTerms'(XPB), XBody), XMap, XOpts, XLclFun, XArity, XExtra, XQNo, 'lo.core#,..'('lo.comp.term#clse'(XQ, 'lo.comp.term#prg'(XLclFun, XArity), 'lo.core#,..'(XStIn, 'lo.core#,..'(XStX, XArgs)), XGoals), XRx), XRx, XEx, XExx, XRp, XRpx) :- 'lo.comp.debug@debugPreamble'(XNm, XExtra, XQ0, XG0, XG1, XOpts, XClOpts),
    'lo.comp.transform@trPtns'(XA, XArgs, XExtra, XQ0, XQ1, XG4, XG5, XG6, XG7, XMap, XClOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transutils@genVarbl'("StIn", XStIn),
    'lo.comp.transform@dcgBody'(XBody, XMap, XClOpts, XG5, XG6, XStIn, XStOut, 'lo.core#,..'(XStIn, XQ1), XQ2, XEx0, XEx1, XRp0, XRp1),
    'lo.comp.transform@pushTerminals'(XPB, XMap, XClOpts, XG7, XG8, XStOut, XStX, XQ2, XQ4, XEx1, XExx, XRp1, XRpx),
    'lo.comp.transutils@labelAccess'(XQ4, XQ, XMap, XGoals, XG0),
    'lo.comp.debug@frameDebug'(XNm, XQNo, XQ, XG1, XG2, XClOpts),
    'lo.comp.debug@lineDebug'(XLc, XG2, XG3, XClOpts),
    'lo.comp.debug@deframeDebug'(XNm, XQNo, XG8, 'lo.core#[]', XClOpts),
    'lo.comp.debug@breakDebug'(XNm, XG3, XG4, XClOpts),
    ocall('size%2'(XArgs, XX19404),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%3'(XX19404, 2, XX19406),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    XArity = XX19406.
'lo.comp.transform@transformGrammarRules'(X_1344, X_1345, X_1346, X_1347, X_1348, 'lo.core#[]', XNo, XRules, XRules, XEx, XEx, XRp, XRp).
'lo.comp.transform@transformGrammarRules'(XMap, XOpts, XLclFun, XArity, XExtra, 'lo.core#,..'(XRl, XDefs), XNo, XRules, XRx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@transformGrammarRule'(XRl, XMap, XOpts, XLclFun, XArity, XExtra, XNo, XRules, XR0, XEx, XEx0, XRp, XRp0),
    ocall('+%3'(XNo, 1, XX19456),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.transform@transformGrammarRules'(XMap, XOpts, XLclFun, X_1349, XExtra, XDefs, XX19456, XR0, XRx, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@transformGrammar'('lo.comp.canon#grammDef'(XLc, XNm, X_1350, 'lo.core#[]', XRls), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transutils@lookupRelName'(XMap, XNm, XReslt),
    'lo.comp.transutils@programAccess'(XReslt, XLclFun, X_1351, XClosure, X_1352),
    'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transform@transformGrammarRules'(XMap, XOpts, XLclFun, XArity, XExtra, XRls, 1, XRules, XRx, XEx, XEx0, XRp, XRpx),
    ocall('_coerce%2'(XLc, XX19502),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.comp.transform@closureEntry'(XMap, 'lo.core#some'(XX19502), XLclFun, XClosure, XArity, XEx0, XExx).
'lo.comp.transform@transformClause'('lo.comp.canon#clause'(XLc, XNm, 'lo.comp.canon#tpl'(XA), XBody), XMap, XOpts, XPrdNme, XArity, XExtra, XQNo, 'lo.core#,..'('lo.comp.term#clse'(XQ, 'lo.comp.term#prg'(XPrdNme, XArity), XArgs, XGoals), XRx), XRx, XEx, XExx, XRp, XRpx) :- 'lo.comp.debug@debugPreamble'(XNm, XExtra, XQ0, XG0, XG1, XOpts, XClOpts),
    'lo.comp.transform@trPtns'(XA, XArgs, XExtra, XQ0, XQ1, XG4, XG5, XG7, XG8, XMap, XClOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trGoal'(XBody, XG5, XG7, XQ1, XQ3, XMap, XClOpts, XEx0, XExx, XRp0, XRpx),
    'lo.comp.transutils@labelAccess'(XQ3, XQ, XMap, XGoals, XG0),
    'lo.comp.debug@frameDebug'(XNm, XQNo, XQ, XG1, XG2, XClOpts),
    'lo.comp.debug@lineDebug'(XLc, XG2, XG3, XClOpts),
    'lo.comp.debug@deframeDebug'(XNm, XQNo, XG8, 'lo.core#[]', XClOpts),
    'lo.comp.debug@breakDebug'(XNm, XG3, XG4, XClOpts),
    ocall('size%2'(XArgs, XX19595),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    XArity = XX19595.
'lo.comp.transform@transformClauses'(X_1353, X_1354, X_1355, X_1356, X_1357, 'lo.core#[]', XNo, XRules, XRules, XEx, XEx, XRpx, XRpx).
'lo.comp.transform@transformClauses'(XMap, XOpts, XPrdNme, XArity, XExtra, 'lo.core#,..'(XCl, XDefs), XNo, XRules, XRx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@transformClause'(XCl, XMap, XOpts, XPrdNme, XArity, XExtra, XNo, XRules, XR0, XEx, XEx0, XRp, XRp0),
    ocall('+%3'(XNo, 1, XX19645),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.transform@transformClauses'(XMap, XOpts, XPrdNme, X_1358, XExtra, XDefs, XX19645, XR0, XRx, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@transformPredicate'('lo.comp.canon#relDef'(XLc, XNm, X_1359, 'lo.core#[]', XClses), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transutils@lookupRelName'(XMap, XNm, XReslt),
    'lo.comp.transutils@programAccess'(XReslt, XPrdNme, X_1360, XClosure, X_1361),
    'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transform@transformClauses'(XMap, XOpts, XPrdNme, XArity, XExtra, XClses, 1, XRules, XRx, XEx, XEx0, XRp, XRpx),
    ocall('_coerce%2'(XLc, XX19691),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.comp.transform@closureEntry'(XMap, 'lo.core#some'(XX19691), XPrdNme, XClosure, XArity, XEx0, XExx).
'lo.comp.transform@transformDefn'(XMap, XOpts, XLc, XNm, XCond, 'lo.comp.canon#lambda'('lo.comp.canon#equation'(X_1362, X_1363, 'lo.comp.canon#tpl'(XCVars), XValue, X_1364)), 'lo.core#,..'('lo.comp.term#clse'(XQ, XLclPrg, XArgs, XBody), XRx), XRx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transform@trPtns'(XCVars, XArgs, 'lo.core#,..'(XRep, XExtra), XExtra, XQ0, XPreG, XG0, XG7, XG8, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.list@length'(XExtra, XX19746),
    ocall('+%3'(XX19746, 1, XX19747),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    XLclPrg = 'lo.comp.term#prg'(XLclName, XX19747),
    'lo.comp.transutils@lookupVarName'(XMap, XNm, XReslt),
    'lo.comp.transutils@programAccess'(XReslt, XLclName, X_1365, X_1366, X_1367),
    'lo.comp.debug@debugPreamble'(XNm, XExtra, XQ0, XG0, XG1, XOpts, XClOpts),
    'lo.comp.transform@trGoal'(XCond, XG4, 'lo.core#,..'('lo.comp.term#neck', XG5), XQ0, XQ2, XMap, XClOpts, XEx0, XEx1, XRp0, XRp1),
    'lo.comp.transform@trExp'(XValue, XRep, XQ2, XQ3, XG5, XG6, XG6, XG7, XMap, XClOpts, XEx1, XExx, XRp1, XRpx),
    'lo.comp.transutils@labelAccess'(XQ3, XQ, XMap, XBody, XPreG),
    'lo.comp.debug@frameDebug'(XNm, 0, XQ, XG1, XG2, XClOpts),
    'lo.comp.debug@lineDebug'(XLc, XG2, XG3, XClOpts),
    'lo.comp.debug@deframeDebug'(XNm, 0, XG8, 'lo.core#[]', XClOpts),
    'lo.comp.debug@breakDebug'(XNm, XG3, XG4, XClOpts).
'lo.comp.transform@transformDefn'(XMap, XOpts, XLc, XNm, XCond, XValue, 'lo.core#,..'('lo.comp.term#clse'(XQ, XLclPrg, 'lo.core#,..'(XRep, XExtra), XBody), XRx), XRx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.list@length'(XExtra, XX19839),
    ocall('+%3'(XX19839, 1, XX19840),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    XLclPrg = 'lo.comp.term#prg'(XLclName, XX19840),
    'lo.comp.transutils@lookupVarName'(XMap, XNm, XReslt),
    'lo.comp.transutils@programAccess'(XReslt, XLclName, X_1368, X_1369, X_1370),
    'lo.comp.debug@debugPreamble'(XNm, XExtra, XQ0, XG0, XG1, XOpts, XClOpts),
    'lo.comp.transform@trGoal'(XCond, XG4, 'lo.core#,..'('lo.comp.term#neck', XG5), XQ0, XQ2, XMap, XClOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@trExp'(XValue, XRep, XQ2, XQ3, XG5, XG6, XG6, XG7, XMap, XClOpts, XEx0, XExx, XRp0, XRpx),
    'lo.comp.transutils@labelAccess'(XQ3, XQ, XMap, XBody, XG0),
    'lo.comp.debug@frameDebug'(XNm, 0, XQ, XG1, XG2, XClOpts),
    'lo.comp.debug@lineDebug'(XLc, XG2, XG3, XClOpts),
    'lo.comp.debug@deframeDebug'(XNm, 0, XG7, 'lo.core#[]', XClOpts),
    'lo.comp.debug@breakDebug'(XNm, XG3, XG4, XClOpts).
'lo.comp.transform@extraArity'(XArity, XVars, XX19912) :- 'lo.list@length'(XVars, XX19910),
    ocall('+%3'(XX19910, XArity, XX19912),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer').
'lo.comp.transform@makeLabelTerm'('lo.comp.transutils#localClass'(XLclName, XStrct, X_1371, XLblVr, XThVr), XLclName, 'lo.comp.term#cons'('lo.comp.term#strct'(XStrct, 2), 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XLclName).
'lo.comp.transform@makeLabelTerm'('lo.comp.transutils#moduleClass'(XAccess, XStrct, 0), XAccess, 'lo.comp.term#enum'(XStrct), XAccess).
'lo.comp.transform@makeLabelTerm'('lo.comp.transutils#moduleClass'(XAccess, XStrct, XAr), XAccess, 'lo.comp.term#cons'('lo.comp.term#strct'(XStrct, 0), 'lo.core#[]'), XAccess).
'lo.comp.transform@makeLabelTerm'('lo.comp.transutils#moduleImpl'(XAccess, XStrct), XAccess, XStrct, XAccess).
'lo.comp.transform@labelDefn'(XMap, XNm, XLclName, 'lo.core#,..'('lo.comp.term#clse'(XExtra, 'lo.comp.term#prg'(XAccess, XX19961), 'lo.core#,..'('lo.comp.term#cons'(XX19965, 'lo.core#,..'(XLblTerm, 'lo.core#[]')), XExtra), 'lo.core#,..'('lo.comp.term#neck', 'lo.core#[]')), XRx), XRx) :- 'lo.comp.transutils@lookupVarName'(XMap, XNm, XSpec),
    'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transform@extraArity'(1, XExtra, XArA),
    'lo.comp.transform@makeLabelTerm'(XSpec, XAccess, XLblTerm, XLclName),
    ocall('size%2'(XExtra, XX19959),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%3'(XX19959, 1, XX19961),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.transutils@trCons'(XNm, 1, XX19965).
'lo.comp.transform@pickAllFieldsFromFace'(XTp, XFields) :- 'lo.comp.types@moveQuants'(XTp, X_1372, XQTp),
    'lo.comp.types@moveConstraints'(XQTp, X_1373, 'lo.comp.types#faceType'(XFields)).
'lo.comp.transform@collectMtd'('lo.comp.canon#funDef'(X_1374, XNm, XTp, X_1375, X_1376), XOuterNm, XLbl, XThV, XList, 'lo.core#,..'((XNm, 'lo.comp.transutils#localFun'(XX20012, XX20015, XX20018, XX20020, XLbl, XThV)), XList)) :- 'lo.comp.transutils@localName'(XOuterNm, "@", XNm, XX20012),
    'lo.comp.transutils@localName'(XOuterNm, "%", XNm, XX20015),
    'lo.comp.transutils@localName'(XOuterNm, "^", XNm, XX20018),
    'lo.comp.transutils@effectiveArity'(XTp, 0, XX20020).
'lo.comp.transform@collectMtd'('lo.comp.canon#relDef'(X_1377, XNm, XTp, X_1378, X_1379), XOuterNm, XLbl, XThV, XList, 'lo.core#,..'((XNm, 'lo.comp.transutils#localRel'(XX20039, XX20042, XX20045, XX20047, XLbl, XThV)), XList)) :- 'lo.comp.transutils@localName'(XOuterNm, "@", XNm, XX20039),
    'lo.comp.transutils@localName'(XOuterNm, "%", XNm, XX20042),
    'lo.comp.transutils@localName'(XOuterNm, "^", XNm, XX20045),
    'lo.comp.transutils@effectiveArity'(XTp, 0, XX20047).
'lo.comp.transform@collectMtd'('lo.comp.canon#grammDef'(X_1380, XNm, XTp, X_1381, X_1382), XOuterNm, XLbl, XThV, XList, 'lo.core#,..'((XNm, 'lo.comp.transutils#localRel'(XX20066, XX20069, XX20072, XX20074, XLbl, XThV)), XList)) :- 'lo.comp.transutils@localName'(XOuterNm, "@", XNm, XX20066),
    'lo.comp.transutils@localName'(XOuterNm, "%", XNm, XX20069),
    'lo.comp.transutils@localName'(XOuterNm, "^", XNm, XX20072),
    'lo.comp.transutils@effectiveArity'(XTp, 0, XX20074).
'lo.comp.transform@collectMtd'('lo.comp.canon#varDef'(X_1383, XNm, X_1384, X_1385, X_1386, X_1387), XOuterNm, XLbl, XThV, XList, 'lo.core#,..'((XNm, 'lo.comp.transutils#localVar'(XX20094, XX20097, XLbl, XThV)), XList)) :- 'lo.comp.transutils@localName'(XOuterNm, "@", XNm, XX20094),
    'lo.comp.transutils@localName'(XOuterNm, "%", XNm, XX20097).
'lo.comp.transform@collectMtd'('lo.comp.canon#classDef'(X_1388, XNm, XTp, X_1389, X_1390, X_1391), XOuterNm, XLbl, XThV, XList, 'lo.core#,..'((XNm, 'lo.comp.transutils#localClass'(XX20117, XX20120, 0, XLbl, XThV)), XList)) :- 'lo.comp.transform@neg26'(XTp),
    'lo.comp.transutils@localName'(XOuterNm, "@", XNm, XX20117),
    'lo.comp.transutils@localName'(XOuterNm, "%", XNm, XX20120).
'lo.comp.transform@collectMtd'('lo.comp.canon#classDef'(X_1392, XNm, XTp, X_1393, X_1394, X_1395), XOuterNm, XLbl, XThV, XList, 'lo.core#,..'((XNm, 'lo.comp.transutils#localClass'(XX20141, XX20144, XX20146, XLbl, XThV)), XList)) :- 'lo.comp.transutils@localName'(XOuterNm, "@", XNm, XX20141),
    'lo.comp.transutils@localName'(XOuterNm, "%", XNm, XX20144),
    'lo.comp.transutils@effectiveArity'(XTp, 0, XX20146).
'lo.comp.transform@collectMtds'('lo.core#[]', X_1396, X_1397, X_1398, XList, XList, X_1399).
'lo.comp.transform@collectMtds'('lo.core#,..'(XEntry, XDefs), XOuterNm, XLbVr, XThVr, XList, XLx, XFields) :- 'lo.comp.transform@collectMtd'(XEntry, XOuterNm, XLbVr, XThVr, XList, XL0),
    'lo.comp.transform@collectMtds'(XDefs, XOuterNm, XLbVr, XThVr, XL0, XLx, XFields).
'lo.comp.transform@collectLabelVars'('lo.core#[]', X_1400, X_1401, XList, XList).
'lo.comp.transform@collectLabelVars'('lo.core#,..'('lo.comp.term#varbl'(XNm), XArgs), XLbVr, XThVr, XList, XLx) :- 'lo.comp.transform@collectLabelVars'(XArgs, XLbVr, XThVr, 'lo.core#,..'((XNm, 'lo.comp.transutils#labelArg'('lo.comp.term#varbl'(XNm), XLbVr, XThVr)), XList), XLx).
'lo.comp.transform@makeLblTerm'('lo.comp.term#enum'(XNm), 'lo.core#[]', 'lo.comp.term#enum'(XNm)).
'lo.comp.transform@makeLblTerm'('lo.comp.term#enum'(XNm), XExtra, 'lo.comp.term#cons'('lo.comp.term#strct'(XNm, XX20216), XExtra)) :- ocall('size%2'(XExtra, XX20216),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list').
'lo.comp.transform@makeLblTerm'('lo.comp.term#cons'('lo.comp.term#strct'(XNm, X_1402), XArgs), XExtra, 'lo.comp.term#cons'('lo.comp.term#strct'(XNm, XX20234), XX20239)) :- ocall('size%2'(XArgs, XX20229),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('size%2'(XExtra, XX20232),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%3'(XX20229, XX20232, XX20234),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.list@<>'(XArgs, XExtra, XX20239).
'lo.comp.transform@makeClassMtdMap'('lo.core#[]', X_1403, X_1404, X_1405, X_1406, XList, XList, X_1407, X_1408, X_1409, XEx, XEx, XRp, XRp).
'lo.comp.transform@makeClassMtdMap'('lo.core#,..'('lo.comp.canon#clRule'(XLc, X_1410, 'lo.comp.canon#v'(X_1411, X_1412), 'lo.comp.canon#theta'(XStmts, X_1413), X_1414, X_1415), XRules), XLclName, XLbVr, XThVr, XLblGl, XList, XLx, XFields, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@collectMtds'(XStmts, XLclName, XLbVr, XThVr, XList, XL0, XFields),
    'lo.comp.transform@collectLabelVars'('lo.core#[]', XLbVr, XThVr, XL0, XL1),
    'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transform@makeLblTerm'('lo.comp.term#enum'(XLclName), XExtra, XLblTerm),
    'lo.comp.transform@cond12'(XLblTerm, XLbVr, XX20305, XLc, XLblGl, XExtra),
    'lo.comp.transform@makeClassMtdMap'(XRules, XLclName, XLbVr, XThVr, X_1416, XL1, XLx, XFields, XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@makeClassMtdMap'('lo.core#,..'('lo.comp.canon#clRule'(XLc, X_1417, XHd, 'lo.comp.canon#theta'(XStmts, X_1418), X_1419, X_1420), XRules), XLclName, XLbVr, XThVr, XLblGl, XList, XLx, XFields, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@collectMtds'(XStmts, XLclName, XLbVr, XThVr, XList, XL0, XFields),
    ocall('_coerce%2'(XLc, XX20366),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.comp.transform@trPtn'(XHd, XLbl, 'lo.core#[]', XVs, XLblGl, XPx, XPx, 'lo.core#,..'('lo.comp.term#unfy'('lo.core#some'(XX20366), XLbVr, XLblTerm), 'lo.core#[]'), XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@collectLabelVars'(XVs, XLbVr, XThVr, XL0, XL1),
    'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transform@makeLblTerm'(XLbl, XExtra, XLblTerm),
    'lo.comp.transform@makeClassMtdMap'(XRules, XLclName, XLbVr, XThVr, X_1421, XL1, XLx, XFields, XMap, XOpts, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@makeClassMtdMap'('lo.core#,..'('lo.comp.canon#clRule'(X_1422, X_1423, X_1424, X_1425, X_1426, X_1427), XRules), XLclName, XLbVr, XThVr, XLblGl, XList, XL0, XFields, XMap, XOpts, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@makeClassMtdMap'(XRules, XLclName, XLbVr, XThVr, XLblGl, XList, XL0, XFields, XMap, XOpts, XEx, XExx, XRp, XRpx).
'lo.comp.transform@inheritClause'(XName, XTp, XTLc, XPrefix, XSuper, 'lo.core#,..'('lo.comp.term#clse'(XX20451, 'lo.comp.term#prg'(XPrefix, 3), 'lo.core#,..'(XCon, 'lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), 'lo.core#,..'('lo.comp.term#neck', 'lo.core#,..'('lo.comp.term#call'(XTLc, XSuper, 'lo.core#,..'(XCon, 'lo.core#,..'(XLbVr, 'lo.core#,..'(XThVr, 'lo.core#[]')))), 'lo.core#[]'))), XEn), XEn) :- 'lo.comp.transutils@effectiveArity'(XTp, 0, XX20481),
    XArity = XX20481,
    'lo.comp.transutils@genVars'(XArity, XX20484),
    XArgs = XX20484,
    'lo.comp.transutils@genVarbl'("This", XThVr),
    'lo.comp.transutils@genVarbl'("Lbl", XLbVr),
    'lo.comp.transutils@trCons'(XName, XArity, XX20490),
    XCon = 'lo.comp.term#cons'(XX20490, XArgs),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XX20451).
'lo.comp.transform@makeInheritFields'('lo.core#[]', X_1428, X_1429, X_1430, X_1431, X_1432, X_1433, XEntry, XEntry, XList, XList).
'lo.comp.transform@makeInheritFields'('lo.core#,..'((XNm, XTp), XInhFields), XLclName, XTLc, XSuper, XFields, XLbVr, XThVr, XEntry, XEn, XList, XLx) :- ocall('in%2'((XNm, X_1434), XList),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list'),
    'lo.comp.transform@neg27'(XTp),
    'lo.comp.transform@makeInheritFields'(XInhFields, XLclName, XTLc, XSuper, XFields, XLbVr, XThVr, XEntry, XEn, XList, XLx).
'lo.comp.transform@makeInheritFields'('lo.core#,..'((XNm, XTp), XInhFields), XLclName, XTLc, XSuper, XFields, XLbVr, XThVr, XEntry, XEn, XList, XLx) :- 'lo.comp.transform@inheritClause'(XNm, XTp, XTLc, XLclName, XSuper, XEntry, XEn0),
    'lo.comp.transform@makeInheritFields'(XInhFields, XLclName, XTLc, XSuper, XFields, XLbVr, XThVr, XEn0, XEn, 'lo.core#,..'((XNm, 'lo.comp.transutils#inheritField'(XSuper, XLbVr, XThVr)), XList), XLx).
'lo.comp.transform@makeInheritanceMap'('lo.core#[]', X_1435, X_1436, X_1437, X_1438, X_1439, XList, XList, X_1440, XEn, XEn, XEx, XEx, XRp, XRp).
'lo.comp.transform@makeInheritanceMap'('lo.core#,..'('lo.comp.canon#clRule'(X_1441, X_1442, X_1443, 'lo.comp.canon#theta'(X_1444, X_1445), X_1446, X_1447), XDefs), XLclName, XLbVr, XThVr, XMap, XOpts, XList, XLx, XFields, XEntry, XEn, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@makeInheritanceMap'(XDefs, XLclName, XLbVr, XThVr, XMap, XOpts, XList, XLx, XFields, XEntry, XEn, XEx, XExx, XRp, XRpx).
'lo.comp.transform@makeInheritanceMap'('lo.core#,..'('lo.comp.canon#clRule'(XLc, X_1448, XP, XR, X_1449, XFaceTp), XDefs), XLclName, XLbVr, XThVr, XMap, XOpts, XList, XLx, XFields, XEntry, XEn, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@pickAllFieldsFromFace'(XFaceTp, XInhFields),
    'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transutils@genVarbl'("CV", XCV),
    'lo.comp.transform@trPtn'(XP, XPtn, XExtra, XQ0, XBody, XPre0, XPre0, XPrx, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    ocall('_coerce%2'(XLc, XX20677),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.comp.transform@trExp'(XR, XRepl, XQ0, XQ1, XPrx, XPx, XPx, 'lo.core#,..'('lo.comp.term#ocall'('lo.core#some'(XX20677), XCV, XRepl, XThVr), 'lo.core#[]'), XMap, XOpts, XEx0, XEx1, XRP0, XRp1),
    'lo.comp.transutils@genNewName'(XMap, "^", 3, XSuper),
    'lo.list@merge'(XQ1, 'lo.core#,..'(XCV, 'lo.core#,..'(XThVr, 'lo.core#[]')), XX20701),
    XEx1 = 'lo.core#,..'('lo.comp.term#clse'(XX20701, XSuper, 'lo.core#,..'(XCV, 'lo.core#,..'(XPtn, 'lo.core#,..'(XThVr, 'lo.core#[]'))), XBody), XEx2),
    ocall('_coerce%2'(XLc, XX20717),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.comp.transform@makeInheritFields'(XInhFields, XLclName, 'lo.core#some'(XX20717), XSuper, XFields, XLbVr, XThVr, XEntry, XEn0, XList, XL1),
    'lo.comp.transform@makeInheritanceMap'(XDefs, XLclName, XLbVr, XThVr, XMap, XOpts, XL1, XLx, XFields, XEn0, XEn, XEx2, XExx, XRp1, XRpx).
'lo.comp.transform@genClassMap'(XMap, XOpts, XLc, XLclName, XDefs, XFace, 'lo.core#,..'('lo.comp.transutils#lyr'(XLclName, XList, XLblGl, XLbVr, XThVr), XMap), XEntry, XEn, XEx, XExx, XRp, XRpx) :- 'lo.comp.transutils@genVarbl'("LbV", XLbVr),
    'lo.comp.transutils@genVarbl'("ThV", XThVr),
    'lo.comp.transform@pickAllFieldsFromFace'(XFace, XFields),
    'lo.comp.transform@makeClassMtdMap'(XDefs, XLclName, XLbVr, XThVr, XLblGl, 'lo.core#[]', XL0, XFields, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    'lo.comp.transform@makeInheritanceMap'(XDefs, XLclName, XLbVr, XThVr, XMap, XOpts, XL0, XList, XFields, XEntry, XEn, XEx0, XExx, XRp0, XRpx).
'lo.comp.transform@findClassBody'(XDefs, XStmts) :- ocall('in%2'('lo.comp.canon#clRule'(X_1450, X_1451, X_1452, 'lo.comp.canon#theta'(XStmts, X_1453), X_1454, X_1455), XDefs),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.comp.transform@entryClause'(XMap, XName, XTLc, 'lo.core#,..'('lo.comp.term#clse'(XQ, 'lo.comp.term#prg'(XX20814, 3), 'lo.core#,..'('lo.comp.term#cons'(XX20818, XArgs), 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), 'lo.core#,..'('lo.comp.term#neck', 'lo.core#,..'('lo.comp.term#call'(XTLc, 'lo.comp.term#prg'(XProg, XX20831), XQ), 'lo.core#[]'))), XRx), XRx) :- 'lo.comp.transutils@lookupVarName'(XMap, XName, XReslt),
    'lo.comp.transutils@programAccess'(XReslt, XProg, X_1456, X_1457, XArity),
    'lo.comp.transutils@genVars'(XArity, XX20853),
    XArgs = XX20853,
    'lo.comp.transutils@genVarbl'("This", XThVr),
    'lo.comp.transutils@genVarbl'("Lbl", XLblVr),
    'lo.list@<>'(XArgs, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), XX20863),
    XQ = XX20863,
    'lo.comp.transutils@layerName'(XMap, XX20814),
    'lo.comp.transutils@trCons'(XName, XArity, XX20818),
    ocall('+%3'(XArity, 2, XX20831),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer').
'lo.comp.transform@closureAccess'(XMap, XName, 'lo.core#,..'('lo.comp.term#clse'('lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]')), 'lo.comp.term#prg'(XX20872, 3), 'lo.core#,..'(XLamCons, 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), 'lo.core#[]'), XRx), XRx) :- 'lo.comp.transutils@lookupVarName'(XMap, XName, XReslt),
    'lo.comp.transutils@programAccess'(XReslt, X_1458, X_1459, XClosure, X_1460),
    'lo.comp.transutils@genVarbl'("This", XThVr),
    'lo.comp.transutils@genVarbl'("Lbl", XLblVr),
    'lo.comp.transutils@trCons'(XName, 1, XX20898),
    XLamCons = 'lo.comp.term#cons'(XX20898, 'lo.core#,..'('lo.comp.term#cons'('lo.comp.term#strct'(XClosure, 2), 'lo.core#,..'(XLblVr, 'lo.core#,..'(XThVr, 'lo.core#[]'))), 'lo.core#[]')),
    'lo.comp.transutils@layerName'(XMap, XX20872).
'lo.comp.transform@transformClassDef'('lo.comp.canon#funDef'(XLc, XNm, XTp, XCx, XEqns), XMap, XOpts, XRules, XRx, XEntry, XEnx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@transformFunction'('lo.comp.canon#funDef'(XLc, XNm, XTp, XCx, XEqns), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx),
    ocall('_coerce%2'(XLc, XX20943),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.comp.transform@entryClause'(XMap, XNm, 'lo.core#some'(XX20943), XEntry, XEn0),
    'lo.comp.transform@closureAccess'(XMap, XNm, XEn0, XEnx).
'lo.comp.transform@transformClassDef'('lo.comp.canon#relDef'(XLc, XNm, XTp, XCx, XClses), XMap, XOpts, XRules, XRx, XEntry, XEnx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@transformPredicate'('lo.comp.canon#relDef'(XLc, XNm, XTp, XCx, XClses), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx),
    ocall('_coerce%2'(XLc, XX20985),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.comp.transform@entryClause'(XMap, XNm, 'lo.core#some'(XX20985), XEntry, XEn0),
    'lo.comp.transform@closureAccess'(XMap, XNm, XEn0, XEnx).
'lo.comp.transform@transformClassDef'('lo.comp.canon#grammDef'(XLc, XNm, XTp, XCx, XClses), XMap, XOpts, XRules, XRx, XEntry, XEnx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@transformGrammar'('lo.comp.canon#grammDef'(XLc, XNm, XTp, XCx, XClses), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx),
    ocall('_coerce%2'(XLc, XX21027),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.comp.transform@entryClause'(XMap, XNm, 'lo.core#some'(XX21027), XEntry, XEn0),
    'lo.comp.transform@closureAccess'(XMap, XNm, XEn0, XEnx).
'lo.comp.transform@transformClassDef'('lo.comp.canon#varDef'(XLc, XNm, X_1461, X_1462, XValue, XCond), XMap, XOpts, XRules, XRx, XEntry, XEnx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@transformDefn'(XMap, XOpts, XLc, XNm, XCond, XValue, XRules, XRx, XEx, XExx, XRp, XRpx),
    ocall('_coerce%2'(XLc, XX21068),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.comp.transform@entryClause'(XMap, XNm, 'lo.core#some'(XX21068), XEntry, XEnx).
'lo.comp.transform@transformClassDef'('lo.comp.canon#classDef'(XLc, XNm, XTp, XCx, XDefs, XFace), XMap, XOpts, XRules, XRx, XEntry, XEnx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@transformClass'(XLc, XNm, XDefs, XFace, XMap, XOpts, XRules, XRx, XEntry, XE0, XEx, XExx, XRp, XRpx),
    ocall('_coerce%2'(XLc, XX21107),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    'lo.comp.transform@entryClause'(XMap, XNm, 'lo.core#some'(XX21107), XE0, XEnx).
'lo.comp.transform@transformClassDefs'('lo.core#[]', X_1463, X_1464, XRules, XRules, XEntry, XEntry, XExtra, XExtra, XRp, XRp).
'lo.comp.transform@transformClassDefs'('lo.core#,..'(XDef, XDefs), XMap, XOpts, XRules, XRx, XEntry, XEnx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@transformClassDef'(XDef, XMap, XOpts, XRules, XR0, XEntry, XEn0, XEx, XEx1, XRp, XRp0),
    'lo.comp.transform@transformClassDefs'(XDefs, XMap, XOpts, XR0, XRx, XEn0, XEnx, XEx1, XExx, XRp0, XRpx).
'lo.comp.transform@transformClassBody'(XDefs, XMap, XOpts, XRules, XRx, XEntry, XEnx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@one22'(XStmts, XDefs),
    'lo.comp.transform@transformClassDefs'(XStmts, XMap, XOpts, XRules, XRx, XEntry, XEnx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@transformClassBody'(X_1465, X_1466, X_1467, XRules, XRules, XEntry, XEntry, XEx, XEx, XRp, XRp).
'lo.comp.transform@transformClass'(XLc, XNm, XDefs, XFace, XMap, XOpts, XRules, XRx, XEntry, XEntry, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@labelDefn'(XMap, XNm, XLclName, XRules, XR0),
    'lo.comp.transform@one23'(XRp0, XRp, XEx1, XEx, XEn0, XR0, XCMap, XFace, XDefs, XLclName, XLc, XOpts, XMap),
    'lo.comp.transform@transformClassBody'(XDefs, XCMap, XOpts, XEn1, XRx, XEn0, XEn1, XEx1, XExx, XRp0, XRpx).
'lo.comp.transform@transformImplementation'(XLc, XImplName, 'lo.comp.canon#tpl'('lo.core#[]'), XBody, XFace, XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@transformClass'(XLc, XImplName, 'lo.core#,..'('lo.comp.canon#clRule'(XLc, XImplName, 'lo.comp.canon#v'(XLc, XImplName), XBody, 'lo.comp.canon#trueCond', XFace), 'lo.core#[]'), XFace, XMap, XOpts, XRules, XR0, XR0, XRx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@transformImplementation'(XLc, XImplName, XHd, XBody, XFace, XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@transformClass'(XLc, XImplName, 'lo.core#,..'('lo.comp.canon#clRule'(XLc, XImplName, 'lo.comp.canon#apply'(XLc, 'lo.comp.canon#v'(XLc, XImplName), XHd), XBody, 'lo.comp.canon#trueCond', XFace), 'lo.core#[]'), XFace, XMap, XOpts, XRules, XR0, XR0, XRx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@transformMdlDef'('lo.comp.canon#funDef'(XLc, XNm, XTp, XCx, XEqns), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@transformFunction'('lo.comp.canon#funDef'(XLc, XNm, XTp, XCx, XEqns), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@transformMdlDef'('lo.comp.canon#grammDef'(XLc, XNm, XTp, XCx, XRls), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@transformGrammar'('lo.comp.canon#grammDef'(XLc, XNm, XTp, XCx, XRls), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@transformMdlDef'('lo.comp.canon#relDef'(XLc, XNm, XTp, XCx, XClses), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@transformPredicate'('lo.comp.canon#relDef'(XLc, XNm, XTp, XCx, XClses), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@transformMdlDef'('lo.comp.canon#varDef'(XLc, XNm, X_1468, X_1469, XValue, XCond), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@transformDefn'(XMap, XOpts, XLc, XNm, XCond, XValue, XRules, XRx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@transformMdlDef'('lo.comp.canon#classDef'(XLc, XNm, XTp, X_1470, XDefs, XFace), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@transformClass'(XLc, XNm, XDefs, XFace, XMap, XOpts, XRules, XRx, X_1471, X_1472, XEx, XExx, XRp, XRpx).
'lo.comp.transform@transformMdlDef'('lo.comp.canon#typeDef'(X_1473, X_1474, X_1475, X_1476), X_1477, X_1478, XRules, XRules, XEx, XEx, XRp, XRp).
'lo.comp.transform@transformMdlDef'('lo.comp.canon#cnDefn'(X_1479, X_1480, X_1481), X_1482, X_1483, XRules, XRules, XEx, XEx, XRp, XRp).
'lo.comp.transform@transformMdlDef'('lo.comp.canon#implDef'(XLc, X_1484, XImplName, X_1485, X_1486, XHd, XBody, XFace), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@transformImplementation'(XLc, XImplName, XHd, XBody, XFace, XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx).
'lo.comp.transform@transformModuleDefs'('lo.core#[]', X_1487, X_1488, XRules, XRules, XEx, XEx, XRp, XRp).
'lo.comp.transform@transformModuleDefs'('lo.core#,..'(XDef, XDefs), XMap, XOpts, XRules, XRx, XEx, XExx, XRp, XRpx) :- 'lo.comp.transform@transformMdlDef'(XDef, XMap, XOpts, XRules, XR0, XEx, XEx1, XRp, XRp0),
    'lo.comp.transform@transformModuleDefs'(XDefs, XMap, XOpts, XR0, XRx, XEx1, XExx, XRp0, XRpx).
'lo.comp.transform@isAssertion'('lo.comp.canon#integrity'(X_1489, X_1490)).
'lo.comp.transform@collectGoal'('lo.comp.canon#integrity'(X_1491, XG), 'lo.comp.canon#trueCond', XG) :- !.
'lo.comp.transform@collectGoal'('lo.comp.canon#integrity'(X_1492, XG), XO, 'lo.comp.canon#conjCond'(XG, XO)) :- !.
'lo.comp.transform@collectGoal'(_, _, _) :- raise_exception('error'("collectGoal", 290, 3, 41)).
'lo.comp.transform@transformAssertions'(XAsserts, XMap, XOpts, XLc, 'lo.comp.term#prg'(XAssertNm, XArity), XRules, XRx, XRp, XRpx) :- 'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transutils@layerName'(XMap, XX21577),
    'lo.comp.transutils@localName'(XX21577, "@", "assert", XX21578),
    XAssertNm = XX21578,
    ocall('foldRight%4'('lo.comp.transform^collectGoal', 'lo.comp.canon#trueCond', XAsserts, XX21585),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    'lo.comp.transform@transformClause'('lo.comp.canon#clause'(XLc, "assert", 'lo.comp.canon#tpl'('lo.core#[]'), XX21585), XMap, XOpts, XAssertNm, XArity, XExtra, 1, XRules, XR0, XR0, XRx, XRp, XRpx).
'lo.comp.transform@isShow'('lo.comp.canon#expShow'(X_1493, X_1494)).
'lo.comp.transform@collectShow'('lo.comp.canon#expShow'(XLc, XS), 'lo.comp.canon#trueCond', 'lo.comp.canon#callCond'(XLc, 'lo.comp.canon#v'(XLc, "_logmsg"), 'lo.comp.canon#tpl'('lo.core#,..'(XS, 'lo.core#[]')))) :- !.
'lo.comp.transform@collectShow'('lo.comp.canon#expShow'(XLc, XS), XO, 'lo.comp.canon#conjCond'('lo.comp.canon#callCond'(XLc, 'lo.comp.canon#v'(XLc, "_logmsg"), 'lo.comp.canon#tpl'('lo.core#,..'(XS, 'lo.core#[]'))), XO)) :- !.
'lo.comp.transform@collectShow'(_, _, _) :- raise_exception('error'("collectShow", 300, 3, 76)).
'lo.comp.transform@transformShows'(XShows, XMap, XOpts, XLc, 'lo.comp.term#prg'(XShowNm, XArity), XRules, XRx, XRp, XRpx) :- 'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transutils@layerName'(XMap, XX21643),
    'lo.comp.transutils@localName'(XX21643, "@", "show", XX21644),
    XShowNm = XX21644,
    ocall('foldRight%4'('lo.comp.transform^collectShow', 'lo.comp.canon#trueCond', XShows, XX21651),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    'lo.comp.transform@transformClause'('lo.comp.canon#clause'(XLc, "show", 'lo.comp.canon#tpl'('lo.core#[]'), XX21651), XMap, XOpts, XShowNm, XArity, XExtra, 1, XRules, XR0, XR0, XRx, XRp, XRpx).
'lo.comp.transform@transformOthers'('lo.core#[]', X_1495, X_1496, 'lo.core#,..'('lo.comp.term#neck', 'lo.core#[]'), XRx, XRx, XRp, XRp).
'lo.comp.transform@transformOthers'('lo.core#,..'('lo.comp.canon#integrity'(XLc, XG), XOthers), XMap, XOpts, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XX21683), XAssertName, 'lo.core#[]'), XInits), XRules, XRx, XRp, XRpx) :- 'lo.comp.misc@collect'(XOthers, 'lo.comp.transform^isAssertion', XAsserts, XRest),
    'lo.comp.transform@transformAssertions'('lo.core#,..'('lo.comp.canon#integrity'(XLc, XG), XAsserts), XMap, XOpts, XLc, XAssertName, XRules, XR0, XRp, XRp0),
    'lo.comp.transform@transformOthers'(XRest, XMap, XOpts, XInits, XR0, XRx, XRp0, XRpx),
    ocall('_coerce%2'(XLc, XX21683),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc').
'lo.comp.transform@transformOthers'('lo.core#,..'('lo.comp.canon#expShow'(XLc, XE), XOthers), XMap, XOpts, 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XX21728), XShowName, 'lo.core#[]'), XInits), XRules, XRx, XRp, XRpx) :- 'lo.comp.misc@collect'(XOthers, 'lo.comp.transform^isShow', XShows, XRest),
    'lo.comp.transform@transformShows'('lo.core#,..'('lo.comp.canon#expShow'(XLc, XE), XShows), XMap, XOpts, XLc, XShowName, XRules, XR0, XRp, XRp0),
    'lo.comp.transform@transformOthers'(XRest, XMap, XOpts, XInits, XR0, XRx, XRp0, XRpx),
    ocall('_coerce%2'(XLc, XX21728),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc').
'lo.comp.transform@thetaInit'(XMap, XInits, 'lo.core#,..'('lo.comp.term#clse'('lo.core#[]', 'lo.comp.term#prg'(XX21770, 0), 'lo.core#[]', XInits), XR), XR) :- 'lo.comp.transutils@layerName'(XMap, XX21769),
    'lo.comp.transutils@localName'(XX21769, "@", "init", XX21770).
'lo.comp.transform@transformProg'('lo.comp.canon#canonPkg'(XPkSpec, XImports, XDefs, XOthers), XOpt, 'lo.comp.term#prProg'(XPkSpec, XRules), XRp, XRpx) :- XPkSpec = 'lo.comp.package#pkgSpec'('lo.repo#pkg'(XPkg, XVers), X_1497, X_1498, X_1499, X_1500, X_1501, X_1502),
    'lo.comp.transutils@makePkgMap'(XPkg, XDefs, XImports, XX21803),
    XX21803 = (XEnums, XMap),
    'lo.comp.transutils@trOpts'(XOpt, XPkg, XX21809),
    XPOpts = XX21809,
    'lo.comp.transform@transformModuleDefs'(XDefs, XMap, XPOpts, XR1, XRx, XRx, 'lo.core#[]', XRp, XRp0),
    'lo.comp.transform@transformOthers'(XOthers, XMap, XPOpts, XInits, XRules, XR0, XRp0, XRpx),
    'lo.comp.transform@thetaInit'(XMap, XInits, XR0, XR1).
'lo.comp.transform@trLocation'(XLc, 'lo.comp.canon#tpl'('lo.core#,..'('lo.comp.canon#int'(XX21833), 'lo.core#,..'('lo.comp.canon#int'(XX21836), 'lo.core#,..'('lo.comp.canon#int'(XX21839), 'lo.core#[]'))))) :- !,
    'lo.comp.location@lineOf'(XLc, XX21833),
    'lo.comp.location@columnOf'(XLc, XX21836),
    'lo.comp.location@widthOf'(XLc, XX21839).
'lo.comp.transform@trLocation'(_, _) :- raise_exception('error'("trLocation", 304, 3, 75)).
'lo.comp.transform@enumAccess'(XMap, XName, 'lo.core#,..'('lo.comp.term#clse'(XExtra, 'lo.comp.term#prg'(XAccessName, XX21852), 'lo.core#,..'(XLamCons, XExtra), 'lo.core#[]'), XRx), XRx) :- 'lo.comp.transutils@lookupVarName'(XMap, XName, XReslt),
    'lo.comp.transutils@programAccess'(XReslt, XLclName, XAccessName, X_1503, 0),
    'lo.comp.transutils@extraVars'(XMap, XExtra),
    'lo.comp.transform@cond13'(XX21880, XLclName, XLamCons, XExtra),
    'lo.list@length'(XExtra, XX21851),
    ocall('+%3'(XX21851, 1, XX21852),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer').
'lo.comp.transform@enumAccess'(X_1504, X_1505, XRls, XRls).
'lo.comp.transform@implementPkgRefPtn'('lo.comp.transutils#moduleVar'(X_1506, XVn, X_1507), XLc, X_1508, X_1509, XXi, XXi, XQ, 'lo.core#,..'(XXi, XQ), 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XX21903), 'lo.comp.term#prg'(XVn, 1), 'lo.core#,..'(XXi, 'lo.core#[]')), XTail), XTail, XRp, XRp) :- ocall('_coerce%2'(XLc, XX21903),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc').
'lo.comp.transform@implementPkgRefPtn'('lo.comp.transutils#moduleClass'(XEnum, X_1510, 0), X_1511, X_1512, X_1513, X_1514, 'lo.comp.term#enum'(XEnum), XQ, XQ, XTail, XTail, XRp, XRp).
'lo.comp.transform@implementPkgRefPtn'(X_1515, XLc, XPkg, XRf, X_1516, 'lo.comp.term#anon', XQ, XQ, XPost, XPost, XRp, XRpx) :- ocall('disp%2'(XPkg, XX21946),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('disp%2'(XRf, XX21950),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("illegal access to "), 'lo.core#,..'(XX21946, 'lo.core#,..'('lo.core#ss'("#"), 'lo.core#,..'(XX21950, 'lo.core#[]'))))), XX21958),
    'lo.comp.errors@reportError'(XX21958, XLc, XRp, XRpx).
'lo.comp.transform@implementPkgRefExp'('lo.comp.transutils#moduleVar'(X_1517, XVn, X_1518), XLc, X_1519, X_1520, XXi, XXi, XQ, 'lo.core#,..'(XXi, XQ), 'lo.core#,..'('lo.comp.term#call'('lo.core#some'(XX21976), 'lo.comp.term#prg'(XVn, 1), 'lo.core#,..'(XXi, 'lo.core#[]')), XPre), XPre, XRp, XRp) :- ocall('_coerce%2'(XLc, XX21976),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc').
'lo.comp.transform@implementPkgRefExp'('lo.comp.transutils#moduleClass'(XEnum, X_1521, 0), X_1522, X_1523, X_1524, X_1525, 'lo.comp.term#enum'(XEnum), XQ, XQ, XPre, XPre, XRp, XRp).
'lo.comp.transform@implementPkgRefExp'(X_1526, XLc, XPkg, XRef, XXi, XXi, XQ, XQ, XPre, XPre, XRp, XRpx) :- ocall('disp%2'(XPkg, XX22019),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('disp%2'(XRef, XX22023),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("illegal access to "), 'lo.core#,..'(XX22019, 'lo.core#,..'('lo.core#ss'("#"), 'lo.core#,..'(XX22023, 'lo.core#[]'))))), XX22031),
    'lo.comp.errors@reportError'(XX22031, XLc, XRp, XRpx).
'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'('lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc%1'('lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc')) :- !.
'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'('_coerce%2'(XV3072, XV3073), XLbl275, XThis275) :- !,
    'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc@_coerce'(XV3072, XV3073, XLbl275, XThis275).
'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'('_coerce%1'('lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc^_coerce'(XLbl276, XThis276)), XLbl276, XThis276).
'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc@_coerce'('lo.comp.location#loc'(XLine, XOff, XCol, XLen, XPth), 'lo.comp.term#tloc'(XOff, XLen), XLbV286, XThV286) :- !.
'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc@_coerce'(_, _, _, _) :- raise_exception('error'("_coerce", 812, 5, 51)).
'lo.comp.transform^implementVarPtn'('_call%12'(XV2292, XV2293, XV2294, XV2295, XV2296, XV2297, XV2298, XV2299, XV2300, XV2301, XV2302, XV2303), 'lo.comp.transform^implementVarPtn', _) :- 'lo.comp.transform@implementVarPtn'(XV2292, XV2293, XV2294, XV2295, XV2296, XV2297, XV2298, XV2299, XV2300, XV2301, XV2302, XV2303).
'lo.comp.transform^trVarPtn'('_call%13'(XV2304, XV2305, XV2306, XV2307, XV2308, XV2309, XV2310, XV2311, XV2312, XV2313, XV2314, XV2315, XV2316), 'lo.comp.transform^trVarPtn', _) :- 'lo.comp.transform@trVarPtn'(XV2304, XV2305, XV2306, XV2307, XV2308, XV2309, XV2310, XV2311, XV2312, XV2313, XV2314, XV2315, XV2316).
'lo.comp.transform^implementVarExp'('_call%12'(XV2317, XV2318, XV2319, XV2320, XV2321, XV2322, XV2323, XV2324, XV2325, XV2326, XV2327, XV2328), 'lo.comp.transform^implementVarExp', _) :- 'lo.comp.transform@implementVarExp'(XV2317, XV2318, XV2319, XV2320, XV2321, XV2322, XV2323, XV2324, XV2325, XV2326, XV2327, XV2328).
'lo.comp.transform^trVarExp'('_call%13'(XV2329, XV2330, XV2331, XV2332, XV2333, XV2334, XV2335, XV2336, XV2337, XV2338, XV2339, XV2340, XV2341), 'lo.comp.transform^trVarExp', _) :- 'lo.comp.transform@trVarExp'(XV2329, XV2330, XV2331, XV2332, XV2333, XV2334, XV2335, XV2336, XV2337, XV2338, XV2339, XV2340, XV2341).
'lo.comp.transform^implementFunCall'('_call%22'(XV2342, XV2343, XV2344, XV2345, XV2346, XV2347, XV2348, XV2349, XV2350, XV2351, XV2352, XV2353, XV2354, XV2355, XV2356, XV2357, XV2358, XV2359, XV2360, XV2361, XV2362, XV2363), 'lo.comp.transform^implementFunCall', _) :- 'lo.comp.transform@implementFunCall'(XV2342, XV2343, XV2344, XV2345, XV2346, XV2347, XV2348, XV2349, XV2350, XV2351, XV2352, XV2353, XV2354, XV2355, XV2356, XV2357, XV2358, XV2359, XV2360, XV2361, XV2362, XV2363).
'lo.comp.transform^joinStream'('_call%4'(XV2364, XV2365, XV2366, XV2367), 'lo.comp.transform^joinStream', _) :- 'lo.comp.transform@joinStream'(XV2364, XV2365, XV2366, XV2367).
'lo.comp.transform^mkCanon'('_call%2'(XV2368, XV2369), 'lo.comp.transform^mkCanon', _) :- 'lo.comp.transform@mkCanon'(XV2368, XV2369).
'lo.comp.transform^mkClosure'('_call%3'(XV2370, XV2371, XV2372), 'lo.comp.transform^mkClosure', _) :- 'lo.comp.transform@mkClosure'(XV2370, XV2371, XV2372).
'lo.comp.transform^genRaise'('_call%6'(XV2373, XV2374, XV2375, XV2376, XV2377, XV2378), 'lo.comp.transform^genRaise', _) :- 'lo.comp.transform@genRaise'(XV2373, XV2374, XV2375, XV2376, XV2377, XV2378).
'lo.comp.transform^failSafeEquation'('_call%8'(XV2379, XV2380, XV2381, XV2382, XV2383, XV2384, XV2385, XV2386), 'lo.comp.transform^failSafeEquation', _) :- 'lo.comp.transform@failSafeEquation'(XV2379, XV2380, XV2381, XV2382, XV2383, XV2384, XV2385, XV2386).
'lo.comp.transform^implementPtnCall'('_call%18'(XV2387, XV2388, XV2389, XV2390, XV2391, XV2392, XV2393, XV2394, XV2395, XV2396, XV2397, XV2398, XV2399, XV2400, XV2401, XV2402, XV2403, XV2404), 'lo.comp.transform^implementPtnCall', _) :- 'lo.comp.transform@implementPtnCall'(XV2387, XV2388, XV2389, XV2390, XV2391, XV2392, XV2393, XV2394, XV2395, XV2396, XV2397, XV2398, XV2399, XV2400, XV2401, XV2402, XV2403, XV2404).
'lo.comp.transform^trPtnCallOp'('_call%20'(XV2405, XV2406, XV2407, XV2408, XV2409, XV2410, XV2411, XV2412, XV2413, XV2414, XV2415, XV2416, XV2417, XV2418, XV2419, XV2420, XV2421, XV2422, XV2423, XV2424), 'lo.comp.transform^trPtnCallOp', _) :- 'lo.comp.transform@trPtnCallOp'(XV2405, XV2406, XV2407, XV2408, XV2409, XV2410, XV2411, XV2412, XV2413, XV2414, XV2415, XV2416, XV2417, XV2418, XV2419, XV2420, XV2421, XV2422, XV2423, XV2424).
'lo.comp.transform^trLambda'('_call%9'(XV2425, XV2426, XV2427, XV2428, XV2429, XV2430, XV2431, XV2432, XV2433), 'lo.comp.transform^trLambda', _) :- 'lo.comp.transform@trLambda'(XV2425, XV2426, XV2427, XV2428, XV2429, XV2430, XV2431, XV2432, XV2433).
'lo.comp.transform^trGoalDot'('_call%13'(XV2434, XV2435, XV2436, XV2437, XV2438, XV2439, XV2440, XV2441, XV2442, XV2443, XV2444, XV2445, XV2446), 'lo.comp.transform^trGoalDot', _) :- 'lo.comp.transform@trGoalDot'(XV2434, XV2435, XV2436, XV2437, XV2438, XV2439, XV2440, XV2441, XV2442, XV2443, XV2444, XV2445, XV2446).
'lo.comp.transform^implementGoalCall'('_call%14'(XV2447, XV2448, XV2449, XV2450, XV2451, XV2452, XV2453, XV2454, XV2455, XV2456, XV2457, XV2458, XV2459, XV2460), 'lo.comp.transform^implementGoalCall', _) :- 'lo.comp.transform@implementGoalCall'(XV2447, XV2448, XV2449, XV2450, XV2451, XV2452, XV2453, XV2454, XV2455, XV2456, XV2457, XV2458, XV2459, XV2460).
'lo.comp.transform^trGoalCall'('_call%12'(XV2461, XV2462, XV2463, XV2464, XV2465, XV2466, XV2467, XV2468, XV2469, XV2470, XV2471, XV2472), 'lo.comp.transform^trGoalCall', _) :- 'lo.comp.transform@trGoalCall'(XV2461, XV2462, XV2463, XV2464, XV2465, XV2466, XV2467, XV2468, XV2469, XV2470, XV2471, XV2472).
'lo.comp.transform^dcgAhead'('_call%12'(XV2473, XV2474, XV2475, XV2476, XV2477, XV2478, XV2479, XV2480, XV2481, XV2482, XV2483, XV2484), 'lo.comp.transform^dcgAhead', _) :- 'lo.comp.transform@dcgAhead'(XV2473, XV2474, XV2475, XV2476, XV2477, XV2478, XV2479, XV2480, XV2481, XV2482, XV2483, XV2484).
'lo.comp.transform^dcgNeg'('_call%12'(XV2485, XV2486, XV2487, XV2488, XV2489, XV2490, XV2491, XV2492, XV2493, XV2494, XV2495, XV2496), 'lo.comp.transform^dcgNeg', _) :- 'lo.comp.transform@dcgNeg'(XV2485, XV2486, XV2487, XV2488, XV2489, XV2490, XV2491, XV2492, XV2493, XV2494, XV2495, XV2496).
'lo.comp.transform@cond7'(XStrmx) :- 'var'(XStrmx),
    !,
    'lo.comp.transutils@genVarbl'("DjOut", XStrmx).
'lo.comp.transform@cond7'(XStrmx).
'lo.comp.transform^dcgOne'('_call%13'(XV2497, XV2498, XV2499, XV2500, XV2501, XV2502, XV2503, XV2504, XV2505, XV2506, XV2507, XV2508, XV2509), 'lo.comp.transform^dcgOne', _) :- 'lo.comp.transform@dcgOne'(XV2497, XV2498, XV2499, XV2500, XV2501, XV2502, XV2503, XV2504, XV2505, XV2506, XV2507, XV2508, XV2509).
'lo.comp.transform@cond8'(XStrmx) :- 'var'(XStrmx),
    !,
    'lo.comp.transutils@genVarbl'("CndOut", XStrmx).
'lo.comp.transform@cond8'(XStrmx).
'lo.comp.transform^dcgConditional'('_call%15'(XV2510, XV2511, XV2512, XV2513, XV2514, XV2515, XV2516, XV2517, XV2518, XV2519, XV2520, XV2521, XV2522, XV2523, XV2524), 'lo.comp.transform^dcgConditional', _) :- 'lo.comp.transform@dcgConditional'(XV2510, XV2511, XV2512, XV2513, XV2514, XV2515, XV2516, XV2517, XV2518, XV2519, XV2520, XV2521, XV2522, XV2523, XV2524).
'lo.comp.transform@cond9'(XQ, XQ0, XStrmx) :- 'var'(XStrmx),
    !,
    'lo.comp.transutils@genVarbl'("DjOut", XStrmx),
    XQ0 = 'lo.core#,..'(XStrmx, XQ).
'lo.comp.transform@cond9'(XQ, XQ0, XStrmx) :- XQ0 = XQ.
'lo.comp.transform^dcgDisj'('_call%14'(XV2525, XV2526, XV2527, XV2528, XV2529, XV2530, XV2531, XV2532, XV2533, XV2534, XV2535, XV2536, XV2537, XV2538), 'lo.comp.transform^dcgDisj', _) :- 'lo.comp.transform@dcgDisj'(XV2525, XV2526, XV2527, XV2528, XV2529, XV2530, XV2531, XV2532, XV2533, XV2534, XV2535, XV2536, XV2537, XV2538).
'lo.comp.transform^pushTerminals'('_call%13'(XV2539, XV2540, XV2541, XV2542, XV2543, XV2544, XV2545, XV2546, XV2547, XV2548, XV2549, XV2550, XV2551), 'lo.comp.transform^pushTerminals', _) :- 'lo.comp.transform@pushTerminals'(XV2539, XV2540, XV2541, XV2542, XV2543, XV2544, XV2545, XV2546, XV2547, XV2548, XV2549, XV2550, XV2551).
'lo.comp.transform@cond10'(XQ0, XQ1, XStrmx) :- 'var'(XStrmx),
    !,
    'lo.comp.transutils@genVarbl'("Stx", XStrmx),
    XQ1 = 'lo.core#,..'(XStrmx, XQ0).
'lo.comp.transform@cond10'(XQ0, XQ1, XStrmx) :- XQ0 = XQ1.
'lo.comp.transform^dcgBody'('_call%13'(XV2552, XV2553, XV2554, XV2555, XV2556, XV2557, XV2558, XV2559, XV2560, XV2561, XV2562, XV2563, XV2564), 'lo.comp.transform^dcgBody', _) :- 'lo.comp.transform@dcgBody'(XV2552, XV2553, XV2554, XV2555, XV2556, XV2557, XV2558, XV2559, XV2560, XV2561, XV2562, XV2563, XV2564).
'lo.comp.transform^trGoal'('_call%11'(XV2565, XV2566, XV2567, XV2568, XV2569, XV2570, XV2571, XV2572, XV2573, XV2574, XV2575), 'lo.comp.transform^trGoal', _) :- 'lo.comp.transform@trGoal'(XV2565, XV2566, XV2567, XV2568, XV2569, XV2570, XV2571, XV2572, XV2573, XV2574, XV2575).
'lo.comp.transform^trExpCallOp'('_call%20'(XV2576, XV2577, XV2578, XV2579, XV2580, XV2581, XV2582, XV2583, XV2584, XV2585, XV2586, XV2587, XV2588, XV2589, XV2590, XV2591, XV2592, XV2593, XV2594, XV2595), 'lo.comp.transform^trExpCallOp', _) :- 'lo.comp.transform@trExpCallOp'(XV2576, XV2577, XV2578, XV2579, XV2580, XV2581, XV2582, XV2583, XV2584, XV2585, XV2586, XV2587, XV2588, XV2589, XV2590, XV2591, XV2592, XV2593, XV2594, XV2595).
'lo.comp.transform@one20'(XRp0, XRp, XEx0, XEx, XOpts, XMap, XPst0, XPost, XPre0, XPre, XQ0, XQ, XA, XP) :- 'lo.comp.transform@trExp'(XP, XA, XQ, XQ0, XPre, XPre0, XPost, XPst0, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    !.
'lo.comp.transform^trExps'('_call%15'(XV2596, XV2597, XV2598, XV2599, XV2600, XV2601, XV2602, XV2603, XV2604, XV2605, XV2606, XV2607, XV2608, XV2609, XV2610), 'lo.comp.transform^trExps', _) :- 'lo.comp.transform@trExps'(XV2596, XV2597, XV2598, XV2599, XV2600, XV2601, XV2602, XV2603, XV2604, XV2605, XV2606, XV2607, XV2608, XV2609, XV2610).
'lo.comp.transform^trExp'('_call%14'(XV2611, XV2612, XV2613, XV2614, XV2615, XV2616, XV2617, XV2618, XV2619, XV2620, XV2621, XV2622, XV2623, XV2624), 'lo.comp.transform^trExp', _) :- 'lo.comp.transform@trExp'(XV2611, XV2612, XV2613, XV2614, XV2615, XV2616, XV2617, XV2618, XV2619, XV2620, XV2621, XV2622, XV2623, XV2624).
'lo.comp.transform^implementDotExp'('_call%18'(XV2625, XV2626, XV2627, XV2628, XV2629, XV2630, XV2631, XV2632, XV2633, XV2634, XV2635, XV2636, XV2637, XV2638, XV2639, XV2640, XV2641, XV2642), 'lo.comp.transform^implementDotExp', _) :- 'lo.comp.transform@implementDotExp'(XV2625, XV2626, XV2627, XV2628, XV2629, XV2630, XV2631, XV2632, XV2633, XV2634, XV2635, XV2636, XV2637, XV2638, XV2639, XV2640, XV2641, XV2642).
'lo.comp.transform^trDotExp'('_call%17'(XV2643, XV2644, XV2645, XV2646, XV2647, XV2648, XV2649, XV2650, XV2651, XV2652, XV2653, XV2654, XV2655, XV2656, XV2657, XV2658, XV2659), 'lo.comp.transform^trDotExp', _) :- 'lo.comp.transform@trDotExp'(XV2643, XV2644, XV2645, XV2646, XV2647, XV2648, XV2649, XV2650, XV2651, XV2652, XV2653, XV2654, XV2655, XV2656, XV2657, XV2658, XV2659).
'lo.comp.transform^trPtn'('_call%14'(XV2660, XV2661, XV2662, XV2663, XV2664, XV2665, XV2666, XV2667, XV2668, XV2669, XV2670, XV2671, XV2672, XV2673), 'lo.comp.transform^trPtn', _) :- 'lo.comp.transform@trPtn'(XV2660, XV2661, XV2662, XV2663, XV2664, XV2665, XV2666, XV2667, XV2668, XV2669, XV2670, XV2671, XV2672, XV2673).
'lo.comp.transform@one21'(XRp0, XRp, XEx0, XEx, XOpts, XMap, XPst0, XPost, XPre0, XPre, XQ0, XQ, XA, XP) :- 'lo.comp.transform@trPtn'(XP, XA, XQ, XQ0, XPre, XPre0, XPost, XPst0, XMap, XOpts, XEx, XEx0, XRp, XRp0),
    !.
'lo.comp.transform^trPtns'('_call%15'(XV2674, XV2675, XV2676, XV2677, XV2678, XV2679, XV2680, XV2681, XV2682, XV2683, XV2684, XV2685, XV2686, XV2687, XV2688), 'lo.comp.transform^trPtns', _) :- 'lo.comp.transform@trPtns'(XV2674, XV2675, XV2676, XV2677, XV2678, XV2679, XV2680, XV2681, XV2682, XV2683, XV2684, XV2685, XV2686, XV2687, XV2688).
'lo.comp.transform^transformEqn'('_call%13'(XV2689, XV2690, XV2691, XV2692, XV2693, XV2694, XV2695, XV2696, XV2697, XV2698, XV2699, XV2700, XV2701), 'lo.comp.transform^transformEqn', _) :- 'lo.comp.transform@transformEqn'(XV2689, XV2690, XV2691, XV2692, XV2693, XV2694, XV2695, XV2696, XV2697, XV2698, XV2699, XV2700, XV2701).
'lo.comp.transform^transformEquations'('_call%13'(XV2702, XV2703, XV2704, XV2705, XV2706, XV2707, XV2708, XV2709, XV2710, XV2711, XV2712, XV2713, XV2714), 'lo.comp.transform^transformEquations', _) :- 'lo.comp.transform@transformEquations'(XV2702, XV2703, XV2704, XV2705, XV2706, XV2707, XV2708, XV2709, XV2710, XV2711, XV2712, XV2713, XV2714).
'lo.comp.transform@cond11'(XX19235, XClosure, XClosureCons, XExtra) :- XExtra = 'lo.core#[]',
    !,
    XClosureCons = 'lo.comp.term#enum'(XClosure).
'lo.comp.transform@cond11'(XX19235, XClosure, XClosureCons, XExtra) :- ocall('size%2'(XExtra, XX19235),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    XClosureCons = 'lo.comp.term#cons'('lo.comp.term#strct'(XClosure, XX19235), XExtra).
'lo.comp.transform^closureEntry'('_call%7'(XV2715, XV2716, XV2717, XV2718, XV2719, XV2720, XV2721), 'lo.comp.transform^closureEntry', _) :- 'lo.comp.transform@closureEntry'(XV2715, XV2716, XV2717, XV2718, XV2719, XV2720, XV2721).
'lo.comp.transform^transformFunction'('_call%9'(XV2722, XV2723, XV2724, XV2725, XV2726, XV2727, XV2728, XV2729, XV2730), 'lo.comp.transform^transformFunction', _) :- 'lo.comp.transform@transformFunction'(XV2722, XV2723, XV2724, XV2725, XV2726, XV2727, XV2728, XV2729, XV2730).
'lo.comp.transform^transformGrammarRule'('_call%13'(XV2731, XV2732, XV2733, XV2734, XV2735, XV2736, XV2737, XV2738, XV2739, XV2740, XV2741, XV2742, XV2743), 'lo.comp.transform^transformGrammarRule', _) :- 'lo.comp.transform@transformGrammarRule'(XV2731, XV2732, XV2733, XV2734, XV2735, XV2736, XV2737, XV2738, XV2739, XV2740, XV2741, XV2742, XV2743).
'lo.comp.transform^transformGrammarRules'('_call%13'(XV2744, XV2745, XV2746, XV2747, XV2748, XV2749, XV2750, XV2751, XV2752, XV2753, XV2754, XV2755, XV2756), 'lo.comp.transform^transformGrammarRules', _) :- 'lo.comp.transform@transformGrammarRules'(XV2744, XV2745, XV2746, XV2747, XV2748, XV2749, XV2750, XV2751, XV2752, XV2753, XV2754, XV2755, XV2756).
'lo.comp.transform^transformGrammar'('_call%9'(XV2757, XV2758, XV2759, XV2760, XV2761, XV2762, XV2763, XV2764, XV2765), 'lo.comp.transform^transformGrammar', _) :- 'lo.comp.transform@transformGrammar'(XV2757, XV2758, XV2759, XV2760, XV2761, XV2762, XV2763, XV2764, XV2765).
'lo.comp.transform^transformClause'('_call%13'(XV2766, XV2767, XV2768, XV2769, XV2770, XV2771, XV2772, XV2773, XV2774, XV2775, XV2776, XV2777, XV2778), 'lo.comp.transform^transformClause', _) :- 'lo.comp.transform@transformClause'(XV2766, XV2767, XV2768, XV2769, XV2770, XV2771, XV2772, XV2773, XV2774, XV2775, XV2776, XV2777, XV2778).
'lo.comp.transform^transformClauses'('_call%13'(XV2779, XV2780, XV2781, XV2782, XV2783, XV2784, XV2785, XV2786, XV2787, XV2788, XV2789, XV2790, XV2791), 'lo.comp.transform^transformClauses', _) :- 'lo.comp.transform@transformClauses'(XV2779, XV2780, XV2781, XV2782, XV2783, XV2784, XV2785, XV2786, XV2787, XV2788, XV2789, XV2790, XV2791).
'lo.comp.transform^transformPredicate'('_call%9'(XV2792, XV2793, XV2794, XV2795, XV2796, XV2797, XV2798, XV2799, XV2800), 'lo.comp.transform^transformPredicate', _) :- 'lo.comp.transform@transformPredicate'(XV2792, XV2793, XV2794, XV2795, XV2796, XV2797, XV2798, XV2799, XV2800).
'lo.comp.transform^transformDefn'('_call%12'(XV2801, XV2802, XV2803, XV2804, XV2805, XV2806, XV2807, XV2808, XV2809, XV2810, XV2811, XV2812), 'lo.comp.transform^transformDefn', _) :- 'lo.comp.transform@transformDefn'(XV2801, XV2802, XV2803, XV2804, XV2805, XV2806, XV2807, XV2808, XV2809, XV2810, XV2811, XV2812).
'lo.comp.transform^extraArity'('_call%3'(XV2813, XV2814, XV2815), 'lo.comp.transform^extraArity', _) :- 'lo.comp.transform@extraArity'(XV2813, XV2814, XV2815).
'lo.comp.transform^makeLabelTerm'('_call%4'(XV2816, XV2817, XV2818, XV2819), 'lo.comp.transform^makeLabelTerm', _) :- 'lo.comp.transform@makeLabelTerm'(XV2816, XV2817, XV2818, XV2819).
'lo.comp.transform^labelDefn'('_call%5'(XV2820, XV2821, XV2822, XV2823, XV2824), 'lo.comp.transform^labelDefn', _) :- 'lo.comp.transform@labelDefn'(XV2820, XV2821, XV2822, XV2823, XV2824).
'lo.comp.transform^pickAllFieldsFromFace'('_call%2'(XV2825, XV2826), 'lo.comp.transform^pickAllFieldsFromFace', _) :- 'lo.comp.transform@pickAllFieldsFromFace'(XV2825, XV2826).
'lo.comp.transform@neg26'(XTp) :- 'lo.comp.types@isClassType'(XTp),
    !,
    fail.
'lo.comp.transform@neg26'(XTp).
'lo.comp.transform^collectMtd'('_call%6'(XV2827, XV2828, XV2829, XV2830, XV2831, XV2832), 'lo.comp.transform^collectMtd', _) :- 'lo.comp.transform@collectMtd'(XV2827, XV2828, XV2829, XV2830, XV2831, XV2832).
'lo.comp.transform^collectMtds'('_call%7'(XV2833, XV2834, XV2835, XV2836, XV2837, XV2838, XV2839), 'lo.comp.transform^collectMtds', _) :- 'lo.comp.transform@collectMtds'(XV2833, XV2834, XV2835, XV2836, XV2837, XV2838, XV2839).
'lo.comp.transform^collectLabelVars'('_call%5'(XV2840, XV2841, XV2842, XV2843, XV2844), 'lo.comp.transform^collectLabelVars', _) :- 'lo.comp.transform@collectLabelVars'(XV2840, XV2841, XV2842, XV2843, XV2844).
'lo.comp.transform^makeLblTerm'('_call%3'(XV2845, XV2846, XV2847), 'lo.comp.transform^makeLblTerm', _) :- 'lo.comp.transform@makeLblTerm'(XV2845, XV2846, XV2847).
'lo.comp.transform@cond12'(XLblTerm, XLbVr, XX20305, XLc, XLblGl, XExtra) :- XExtra = 'lo.core#[]',
    !,
    XLblGl = 'lo.core#[]'.
'lo.comp.transform@cond12'(XLblTerm, XLbVr, XX20305, XLc, XLblGl, XExtra) :- ocall('_coerce%2'(XLc, XX20305),'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc','lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc'),
    XLblGl = 'lo.core#,..'('lo.comp.term#unfy'('lo.core#some'(XX20305), XLbVr, XLblTerm), 'lo.core#[]').
'lo.comp.transform^makeClassMtdMap'('_call%14'(XV2848, XV2849, XV2850, XV2851, XV2852, XV2853, XV2854, XV2855, XV2856, XV2857, XV2858, XV2859, XV2860, XV2861), 'lo.comp.transform^makeClassMtdMap', _) :- 'lo.comp.transform@makeClassMtdMap'(XV2848, XV2849, XV2850, XV2851, XV2852, XV2853, XV2854, XV2855, XV2856, XV2857, XV2858, XV2859, XV2860, XV2861).
'lo.comp.transform^inheritClause'('_call%7'(XV2862, XV2863, XV2864, XV2865, XV2866, XV2867, XV2868), 'lo.comp.transform^inheritClause', _) :- 'lo.comp.transform@inheritClause'(XV2862, XV2863, XV2864, XV2865, XV2866, XV2867, XV2868).
'lo.comp.transform@neg27'(XTp) :- 'lo.comp.types@isPredType'(XTp),
    !,
    fail.
'lo.comp.transform@neg27'(XTp).
'lo.comp.transform^makeInheritFields'('_call%11'(XV2869, XV2870, XV2871, XV2872, XV2873, XV2874, XV2875, XV2876, XV2877, XV2878, XV2879), 'lo.comp.transform^makeInheritFields', _) :- 'lo.comp.transform@makeInheritFields'(XV2869, XV2870, XV2871, XV2872, XV2873, XV2874, XV2875, XV2876, XV2877, XV2878, XV2879).
'lo.comp.transform^makeInheritanceMap'('_call%15'(XV2880, XV2881, XV2882, XV2883, XV2884, XV2885, XV2886, XV2887, XV2888, XV2889, XV2890, XV2891, XV2892, XV2893, XV2894), 'lo.comp.transform^makeInheritanceMap', _) :- 'lo.comp.transform@makeInheritanceMap'(XV2880, XV2881, XV2882, XV2883, XV2884, XV2885, XV2886, XV2887, XV2888, XV2889, XV2890, XV2891, XV2892, XV2893, XV2894).
'lo.comp.transform^genClassMap'('_call%13'(XV2895, XV2896, XV2897, XV2898, XV2899, XV2900, XV2901, XV2902, XV2903, XV2904, XV2905, XV2906, XV2907), 'lo.comp.transform^genClassMap', _) :- 'lo.comp.transform@genClassMap'(XV2895, XV2896, XV2897, XV2898, XV2899, XV2900, XV2901, XV2902, XV2903, XV2904, XV2905, XV2906, XV2907).
'lo.comp.transform^findClassBody'('_call%2'(XV2908, XV2909), 'lo.comp.transform^findClassBody', _) :- 'lo.comp.transform@findClassBody'(XV2908, XV2909).
'lo.comp.transform^entryClause'('_call%5'(XV2910, XV2911, XV2912, XV2913, XV2914), 'lo.comp.transform^entryClause', _) :- 'lo.comp.transform@entryClause'(XV2910, XV2911, XV2912, XV2913, XV2914).
'lo.comp.transform^closureAccess'('_call%4'(XV2915, XV2916, XV2917, XV2918), 'lo.comp.transform^closureAccess', _) :- 'lo.comp.transform@closureAccess'(XV2915, XV2916, XV2917, XV2918).
'lo.comp.transform^transformClassDef'('_call%11'(XV2919, XV2920, XV2921, XV2922, XV2923, XV2924, XV2925, XV2926, XV2927, XV2928, XV2929), 'lo.comp.transform^transformClassDef', _) :- 'lo.comp.transform@transformClassDef'(XV2919, XV2920, XV2921, XV2922, XV2923, XV2924, XV2925, XV2926, XV2927, XV2928, XV2929).
'lo.comp.transform^transformClassDefs'('_call%11'(XV2930, XV2931, XV2932, XV2933, XV2934, XV2935, XV2936, XV2937, XV2938, XV2939, XV2940), 'lo.comp.transform^transformClassDefs', _) :- 'lo.comp.transform@transformClassDefs'(XV2930, XV2931, XV2932, XV2933, XV2934, XV2935, XV2936, XV2937, XV2938, XV2939, XV2940).
'lo.comp.transform@one22'(XStmts, XDefs) :- 'lo.comp.transform@findClassBody'(XDefs, XStmts),
    !.
'lo.comp.transform^transformClassBody'('_call%11'(XV2941, XV2942, XV2943, XV2944, XV2945, XV2946, XV2947, XV2948, XV2949, XV2950, XV2951), 'lo.comp.transform^transformClassBody', _) :- 'lo.comp.transform@transformClassBody'(XV2941, XV2942, XV2943, XV2944, XV2945, XV2946, XV2947, XV2948, XV2949, XV2950, XV2951).
'lo.comp.transform@one23'(XRp0, XRp, XEx1, XEx, XEn0, XR0, XCMap, XFace, XDefs, XLclName, XLc, XOpts, XMap) :- 'lo.comp.transform@genClassMap'(XMap, XOpts, XLc, XLclName, XDefs, XFace, XCMap, XR0, XEn0, XEx, XEx1, XRp, XRp0),
    !.
'lo.comp.transform^transformClass'('_call%14'(XV2952, XV2953, XV2954, XV2955, XV2956, XV2957, XV2958, XV2959, XV2960, XV2961, XV2962, XV2963, XV2964, XV2965), 'lo.comp.transform^transformClass', _) :- 'lo.comp.transform@transformClass'(XV2952, XV2953, XV2954, XV2955, XV2956, XV2957, XV2958, XV2959, XV2960, XV2961, XV2962, XV2963, XV2964, XV2965).
'lo.comp.transform^transformImplementation'('_call%13'(XV2966, XV2967, XV2968, XV2969, XV2970, XV2971, XV2972, XV2973, XV2974, XV2975, XV2976, XV2977, XV2978), 'lo.comp.transform^transformImplementation', _) :- 'lo.comp.transform@transformImplementation'(XV2966, XV2967, XV2968, XV2969, XV2970, XV2971, XV2972, XV2973, XV2974, XV2975, XV2976, XV2977, XV2978).
'lo.comp.transform^transformMdlDef'('_call%9'(XV2979, XV2980, XV2981, XV2982, XV2983, XV2984, XV2985, XV2986, XV2987), 'lo.comp.transform^transformMdlDef', _) :- 'lo.comp.transform@transformMdlDef'(XV2979, XV2980, XV2981, XV2982, XV2983, XV2984, XV2985, XV2986, XV2987).
'lo.comp.transform^transformModuleDefs'('_call%9'(XV2988, XV2989, XV2990, XV2991, XV2992, XV2993, XV2994, XV2995, XV2996), 'lo.comp.transform^transformModuleDefs', _) :- 'lo.comp.transform@transformModuleDefs'(XV2988, XV2989, XV2990, XV2991, XV2992, XV2993, XV2994, XV2995, XV2996).
'lo.comp.transform^isAssertion'('_call%1'(XV2997), 'lo.comp.transform^isAssertion', _) :- 'lo.comp.transform@isAssertion'(XV2997).
'lo.comp.transform^collectGoal'('_call%3'(XV2998, XV2999, XV3000), 'lo.comp.transform^collectGoal', _) :- 'lo.comp.transform@collectGoal'(XV2998, XV2999, XV3000).
'lo.comp.transform^transformAssertions'('_call%9'(XV3001, XV3002, XV3003, XV3004, XV3005, XV3006, XV3007, XV3008, XV3009), 'lo.comp.transform^transformAssertions', _) :- 'lo.comp.transform@transformAssertions'(XV3001, XV3002, XV3003, XV3004, XV3005, XV3006, XV3007, XV3008, XV3009).
'lo.comp.transform^isShow'('_call%1'(XV3010), 'lo.comp.transform^isShow', _) :- 'lo.comp.transform@isShow'(XV3010).
'lo.comp.transform^collectShow'('_call%3'(XV3011, XV3012, XV3013), 'lo.comp.transform^collectShow', _) :- 'lo.comp.transform@collectShow'(XV3011, XV3012, XV3013).
'lo.comp.transform^transformShows'('_call%9'(XV3014, XV3015, XV3016, XV3017, XV3018, XV3019, XV3020, XV3021, XV3022), 'lo.comp.transform^transformShows', _) :- 'lo.comp.transform@transformShows'(XV3014, XV3015, XV3016, XV3017, XV3018, XV3019, XV3020, XV3021, XV3022).
'lo.comp.transform^transformOthers'('_call%8'(XV3023, XV3024, XV3025, XV3026, XV3027, XV3028, XV3029, XV3030), 'lo.comp.transform^transformOthers', _) :- 'lo.comp.transform@transformOthers'(XV3023, XV3024, XV3025, XV3026, XV3027, XV3028, XV3029, XV3030).
'lo.comp.transform^thetaInit'('_call%4'(XV3031, XV3032, XV3033, XV3034), 'lo.comp.transform^thetaInit', _) :- 'lo.comp.transform@thetaInit'(XV3031, XV3032, XV3033, XV3034).
'lo.comp.transform^transformProg'('_call%5'(XV3035, XV3036, XV3037, XV3038, XV3039), 'lo.comp.transform^transformProg', _) :- 'lo.comp.transform@transformProg'(XV3035, XV3036, XV3037, XV3038, XV3039).
'lo.comp.transform^trLocation'('_call%2'(XV3040, XV3041), 'lo.comp.transform^trLocation', _) :- 'lo.comp.transform@trLocation'(XV3040, XV3041).
'lo.comp.transform@cond13'(XX21880, XLclName, XLamCons, XExtra) :- XExtra = 'lo.core#[]',
    !,
    XLamCons = 'lo.comp.term#enum'(XLclName).
'lo.comp.transform@cond13'(XX21880, XLclName, XLamCons, XExtra) :- ocall('size%2'(XExtra, XX21880),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    XLamCons = 'lo.comp.term#cons'('lo.comp.term#strct'(XLclName, XX21880), XExtra).
'lo.comp.transform^enumAccess'('_call%4'(XV3042, XV3043, XV3044, XV3045), 'lo.comp.transform^enumAccess', _) :- 'lo.comp.transform@enumAccess'(XV3042, XV3043, XV3044, XV3045).
'lo.comp.transform^implementPkgRefPtn'('_call%12'(XV3046, XV3047, XV3048, XV3049, XV3050, XV3051, XV3052, XV3053, XV3054, XV3055, XV3056, XV3057), 'lo.comp.transform^implementPkgRefPtn', _) :- 'lo.comp.transform@implementPkgRefPtn'(XV3046, XV3047, XV3048, XV3049, XV3050, XV3051, XV3052, XV3053, XV3054, XV3055, XV3056, XV3057).
'lo.comp.transform^implementPkgRefExp'('_call%12'(XV3058, XV3059, XV3060, XV3061, XV3062, XV3063, XV3064, XV3065, XV3066, XV3067, XV3068, XV3069), 'lo.comp.transform^implementPkgRefExp', _) :- 'lo.comp.transform@implementPkgRefExp'(XV3058, XV3059, XV3060, XV3061, XV3062, XV3063, XV3064, XV3065, XV3066, XV3067, XV3068, XV3069).
'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc^_coerce'('_call%2'(XV3070, XV3071), 'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc^_coerce'(XLbV286, XThV286), _) :- 'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc@_coerce'(XV3070, XV3071, XLbV286, XThV286).
'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc^_coerce'('_call%2'(XV3074, XV3075), 'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc^_coerce'(XLbV286, XThV286), _) :- 'lo.coerce$coercion$lo.comp.location*location$lo.comp.term*tloc@_coerce'(XV3074, XV3075, XLbV286, XThV286).
