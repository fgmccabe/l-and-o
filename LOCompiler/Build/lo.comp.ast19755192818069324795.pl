'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.ast's'0.0.1'n4o4'()4'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.location'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.keywords'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.operators'e'*'s\"I6'appl'CT3t'lo.comp.location*location't'lo.comp.ast*ast't'lo.comp.ast*ast't'lo.comp.ast*ast''tupl'CT3t'lo.comp.location*location'SLt'lo.comp.ast*ast't'lo.comp.ast*ast''strg'CT2t'lo.comp.location*location'St'lo.comp.ast*ast''flot'CT2t'lo.comp.location*location'ft'lo.comp.ast*ast''intg'CT2t'lo.comp.location*location'it'lo.comp.ast*ast''iden'CT2t'lo.comp.location*location'St'lo.comp.ast*ast'\"s\"I1'ast'Yt'lo.comp.ast*ast'I1'loc't'lo.comp.location*location'\"n6o6'()6's'appl's'tupl's'strg's'flot's'intg's'iden'n0o0'()0'n1o1'()1'n2o2'()2's'lo.core$display$lo.comp.ast*ast's\"c'lo.core$display'T1t'lo.comp.ast*ast'T0\"").
'lo.comp.ast@init'():- !.
'lo.comp.ast#appl'('appl%1'('lo.comp.ast@appl'())):- !.
'lo.comp.ast#appl'('loc%1'(XV30231), XLbl2201, XThis2201):- !,
    'lo.comp.ast#appl@loc'(XV30231, XLbl2201, XThis2201).
'lo.comp.ast#appl@loc'(XLoc, XLbV2502, XThV2502):- XLbV2502 = 'lo.comp.ast#appl'(XLoc, XOp, XArg),
    !.
'lo.comp.ast@showClose'(XPr, XPrOp, 'lo.core#ss'(")")):- 'lo.core@>'('lo.core$comp$lo.core*integer', XPrOp, XPr),
    !.
'lo.comp.ast@showClose'(X_33504, X_33505, 'lo.core#ss'("")):- !.
'lo.comp.ast@showClose'(_, _, _):- raise_exception('error'("lo.comp.ast@showClose", 87, 3, 41)).
'lo.comp.ast@showOpen'(XPr, XPrOp, 'lo.core#ss'("(")):- 'lo.core@>'('lo.core$comp$lo.core*integer', XPrOp, XPr),
    !.
'lo.comp.ast@showOpen'(X_33506, X_33507, 'lo.core#ss'("")):- !.
'lo.comp.ast@showOpen'(_, _, _):- raise_exception('error'("lo.comp.ast@showOpen", 82, 3, 39)).
'lo.comp.ast#tupl'('tupl%1'('lo.comp.ast@tupl'())):- !.
'lo.comp.ast#tupl'('loc%1'(XV30238), XLbl2202, XThis2202):- !,
    'lo.comp.ast#tupl@loc'(XV30238, XLbl2202, XThis2202).
'lo.comp.ast#tupl@loc'(XLoc, XLbV2503, XThV2503):- XLbV2503 = 'lo.comp.ast#tupl'(XLoc, XNm, XEls),
    !.
'lo.comp.ast@tupleBrackets'("()", "(", ")", 2000, ", ").
'lo.comp.ast@tupleBrackets'("[]", "[", "]", 1000, ", ").
'lo.comp.ast@tupleBrackets'("{}", "{", "}", 2000, ".
").
'lo.comp.ast#strg'('strg%1'('lo.comp.ast@strg'())):- !.
'lo.comp.ast#strg'('loc%1'(XV30244), XLbl2203, XThis2203):- !,
    'lo.comp.ast#strg@loc'(XV30244, XLbl2203, XThis2203).
'lo.comp.ast#strg@loc'(XLoc, XLbV2504, XThV2504):- XLbV2504 = 'lo.comp.ast#strg'(XLoc, XSt),
    !.
'lo.comp.ast#flot'('flot%1'('lo.comp.ast@flot'())):- !.
'lo.comp.ast#flot'('loc%1'(XV30245), XLbl2204, XThis2204):- !,
    'lo.comp.ast#flot@loc'(XV30245, XLbl2204, XThis2204).
'lo.comp.ast#flot@loc'(XLoc, XLbV2505, XThV2505):- XLbV2505 = 'lo.comp.ast#flot'(XLoc, XDx),
    !.
'lo.comp.ast#intg'('intg%1'('lo.comp.ast@intg'())):- !.
'lo.comp.ast#intg'('loc%1'(XV30246), XLbl2205, XThis2205):- !,
    'lo.comp.ast#intg@loc'(XV30246, XLbl2205, XThis2205).
'lo.comp.ast#intg@loc'(XLoc, XLbV2506, XThV2506):- XLbV2506 = 'lo.comp.ast#intg'(XLoc, XIx),
    !.
'lo.comp.ast#iden'('iden%1'('lo.comp.ast@iden'())):- !.
'lo.comp.ast#iden'('loc%1'(XV30247), XLbl2206, XThis2206):- !,
    'lo.comp.ast#iden@loc'(XV30247, XLbl2206, XThis2206).
'lo.comp.ast#iden@loc'(XLoc, XLbV2507, XThV2507):- XLbV2507 = 'lo.comp.ast#iden'(XLoc, XNm),
    !.
'lo.comp.ast@showEls'('lo.core#[]', X_33508, X_33509, X_33510, 'lo.core#[]'):- !.
'lo.comp.ast@showEls'('lo.core#,..'(XE, XLs), XPr, XSep, XSp, 'lo.core#,..'('lo.core#ss'(XSp), 'lo.core#,..'(XXd38969, XXd38970))):- !,
    'lo.comp.ast@showAst'(XE, XPr, XXd38969),
    'lo.comp.ast@showEls'(XLs, XPr, XSep, XSep, XXd38970).
'lo.comp.ast@showEls'(_, _, _, _, _):- raise_exception('error'("lo.comp.ast@showEls", 77, 3, 23)).
'lo.comp.ast@showAst'('lo.comp.ast#iden'(X_33514, XNm), X_33515, 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("("), 'lo.core#,..'('lo.core#ss'(XNm), 'lo.core#,..'('lo.core#ss'(")"), 'lo.core#[]'))))):- 'lo.comp.operators@isOperator'(XNm, X_33516),
    !.
'lo.comp.ast@showAst'('lo.comp.ast#iden'(X_33520, XNm), X_33521, 'lo.core#ss'(XNm)):- !.
'lo.comp.ast@showAst'('lo.comp.ast#intg'(X_33522, XIx), X_33523, XXe4897):- !,
    ocall('disp%1'(XXV5245),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    ocall('_call%2'(XIx, XXe4897),XXV5245,XXV5245).
'lo.comp.ast@showAst'('lo.comp.ast#flot'(X_33524, XDx), X_33525, XXe4898):- !,
    ocall('disp%1'(XXV5246),'lo.core$display$lo.core*float','lo.core$display$lo.core*float'),
    ocall('_call%2'(XDx, XXe4898),XXV5246,XXV5246).
'lo.comp.ast@showAst'('lo.comp.ast#strg'(X_33526, XSx), X_33527, XXe4899):- !,
    ocall('disp%1'(XXV5247),'lo.core$display$lo.core*string','lo.core$display$lo.core*string'),
    ocall('_call%2'(XSx, XXe4899),XXV5247,XXV5247).
'lo.comp.ast@showAst'('lo.comp.ast#tupl'(X_33528, XNm, XEls), X_33529, 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'(XLft), 'lo.core#,..'('lo.core#ssSeq'(XXd38982), 'lo.core#,..'('lo.core#ss'(XRgt), 'lo.core#[]'))))):- 'lo.comp.ast@tupleBrackets'(XNm, XLft, XRgt, XPr, XSep),
    !,
    'lo.comp.ast@showEls'(XEls, XPr, XSep, "", XXd38982).
'lo.comp.ast@showAst'('lo.comp.ast#appl'(X_33533, 'lo.comp.ast#iden'(X_33534, XOp), 'lo.comp.ast#tupl'(X_33535, "()", 'lo.core#,..'(XA, 'lo.core#[]'))), XPr, 'lo.core#ssSeq'('lo.core#,..'(XXd38989, 'lo.core#,..'('lo.core#ss'(XOp), 'lo.core#,..'('lo.core#ss'(" "), 'lo.core#,..'(XXd38992, 'lo.core#,..'(XXd38993, 'lo.core#[]'))))))):- 'lo.comp.operators@prefixOp'(XOp, XPrOp, XPrR),
    !,
    'lo.comp.ast@showOpen'(XPr, XPrOp, XXd38989),
    'lo.comp.ast@showAst'(XA, XPrR, XXd38992),
    'lo.comp.ast@showClose'(XPr, XPrOp, XXd38993).
'lo.comp.ast@showAst'('lo.comp.ast#appl'(X_33542, 'lo.comp.ast#iden'(X_33543, XOp), 'lo.comp.ast#tupl'(X_33544, "()", 'lo.core#,..'(XA, 'lo.core#[]'))), XPr, 'lo.core#ssSeq'('lo.core#,..'(XXd39000, 'lo.core#,..'(XXd39001, 'lo.core#,..'('lo.core#ss'(" "), 'lo.core#,..'('lo.core#ss'(XOp), 'lo.core#,..'(XXd39004, 'lo.core#[]'))))))):- 'lo.comp.operators@postfixOp'(XOp, XPrL, XPrOp),
    !,
    'lo.comp.ast@showOpen'(XPr, XPrOp, XXd39000),
    'lo.comp.ast@showAst'(XA, XPrL, XXd39001),
    'lo.comp.ast@showClose'(XPr, XPrOp, XXd39004).
'lo.comp.ast@showAst'('lo.comp.ast#appl'(X_33551, 'lo.comp.ast#iden'(X_33552, XOp), 'lo.comp.ast#tupl'(X_33553, "()", 'lo.core#,..'(XL, 'lo.core#,..'(XR, 'lo.core#[]')))), XPr, 'lo.core#ssSeq'('lo.core#,..'(XXd39011, 'lo.core#,..'(XXd39012, 'lo.core#,..'('lo.core#ss'(" "), 'lo.core#,..'('lo.core#ss'(XOp), 'lo.core#,..'('lo.core#ss'(" "), 'lo.core#,..'(XXd39016, 'lo.core#,..'(XXd39017, 'lo.core#[]'))))))))):- 'lo.comp.operators@infixOp'(XOp, XPrL, XPrOp, XPrR),
    !,
    'lo.comp.ast@showOpen'(XPr, XPrOp, XXd39011),
    'lo.comp.ast@showAst'(XL, XPrL, XXd39012),
    'lo.comp.ast@showAst'(XR, XPrR, XXd39016),
    'lo.comp.ast@showClose'(XPr, XPrOp, XXd39017).
'lo.comp.ast@showAst'('lo.comp.ast#appl'(X_33563, XOp, XArgs), X_33564, 'lo.core#ssSeq'('lo.core#,..'(XXd39026, 'lo.core#,..'(XXd39027, 'lo.core#[]')))):- !,
    'lo.comp.ast@showAst'(XOp, 0, XXd39026),
    'lo.comp.ast@showAst'(XArgs, 0, XXd39027).
'lo.comp.ast@showAst'(_, _, _):- raise_exception('error'("lo.comp.ast@showAst", 51, 3, 80)).
'lo.core$display$lo.comp.ast*ast'('lo.core$display$lo.comp.ast*ast%1'('lo.core$display$lo.comp.ast*ast')):- !.
'lo.core$display$lo.comp.ast*ast'('disp%2'(XV30258, XV30259), XLbl2207, XThis2207):- !,
    'lo.core$display$lo.comp.ast*ast@disp'(XV30258, XV30259, XLbl2207, XThis2207).
'lo.core$display$lo.comp.ast*ast'('disp%1'('lo.core$display$lo.comp.ast*ast^disp'(XLbl2208, XThis2208)), XLbl2208, XThis2208).
'lo.core$display$lo.comp.ast*ast@disp'(XT, XXd39031, XLbV2508, XThV2508):- !,
    'lo.comp.ast@showAst'(XT, 2000, XXd39031).
'lo.core$display$lo.comp.ast*ast@disp'(_, _):- raise_exception('error'("lo.core$display$lo.comp.ast*ast@disp", 10, 5, 26)).
'lo.comp.ast@dumpAst'(XA):- ocall('disp%1'(XXV5248),'lo.core$display$lo.comp.ast*ast','lo.core$display$lo.comp.ast*ast'),
    ocall('_call%2'(XA, XXe4900),XXV5248,XXV5248),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XXe4900, 'lo.core#[]')), XXd39034),
    '_logmsg'(XXd39034).
'lo.comp.ast^showClose'('_call%3'(XV30232, XV30233, XV30234), 'lo.comp.ast^showClose', _):- 'lo.comp.ast@showClose'(XV30232, XV30233, XV30234).
'lo.comp.ast^showOpen'('_call%3'(XV30235, XV30236, XV30237), 'lo.comp.ast^showOpen', _):- 'lo.comp.ast@showOpen'(XV30235, XV30236, XV30237).
'lo.comp.ast^tupleBrackets'('_call%5'(XV30239, XV30240, XV30241, XV30242, XV30243), 'lo.comp.ast^tupleBrackets', _):- 'lo.comp.ast@tupleBrackets'(XV30239, XV30240, XV30241, XV30242, XV30243).
'lo.comp.ast^showEls'('_call%5'(XV30248, XV30249, XV30250, XV30251, XV30252), 'lo.comp.ast^showEls', _):- 'lo.comp.ast@showEls'(XV30248, XV30249, XV30250, XV30251, XV30252).
'lo.comp.ast^showAst'('_call%3'(XV30253, XV30254, XV30255), 'lo.comp.ast^showAst', _):- 'lo.comp.ast@showAst'(XV30253, XV30254, XV30255).
'lo.core$display$lo.comp.ast*ast^disp'('_call%2'(XV30256, XV30257), 'lo.core$display$lo.comp.ast*ast^disp'(XLbV2508, XThV2508), _):- 'lo.core$display$lo.comp.ast*ast@disp'(XV30256, XV30257, XLbV2508, XThV2508).
'lo.comp.ast^dumpAst'('_call%1'(XV30260), 'lo.comp.ast^dumpAst', _):- 'lo.comp.ast@dumpAst'(XV30260).
