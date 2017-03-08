'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.code.code's'0.0.1'n4o4'()4'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.instructions'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.code.registers'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'s\"I2'assem'CT4t'lo.comp.term*term'Lt'lo.comp.code.instructions*instruction'Lt'lo.comp.code.code*litrl'LT3SSt'lo.comp.term*tloc't'lo.comp.code.code*assem''litrl'CT2St'lo.comp.term*term't'lo.comp.code.code*litrl'\"s\"I2'litrl'Yt'lo.comp.code.code*litrl'I0'assem'Yt'lo.comp.code.code*assem'I0\"n2o2'()2's'assem's'litrl'n0o0'()0'n1o1'()1'n2o2'()2's'lo.core$display$lo.comp.code.code*assem's\"c'lo.core$display'T1t'lo.comp.code.code*assem'T0\"").
'lo.comp.code.code@init'():- !.
'lo.comp.code.code#assem'('assem%1'('lo.comp.code.code@assem'())):- !.
'lo.comp.code.code#litrl'('litrl%1'('lo.comp.code.code@litrl'())):- !.
'lo.comp.code.code@anyLits'('lo.core#[]', 'lo.core#[]'):- !.
'lo.comp.code.code@anyLits'(XL, 'lo.core#,..'(XXe4695, 'lo.core#,..'('lo.core#ss'(" literals
"), 'lo.core#[]'))):- !,
    ocall('size%1'(XXV5031),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('disp%1'(XXV5032),'lo.core$display$lo.core*integer','lo.core$display$lo.core*integer'),
    ocall('_call%2'(XL, XXe4694),XXV5031,XXV5031),
    ocall('_call%2'(XXe4694, XXe4695),XXV5032,XXV5032).
'lo.comp.code.code@anyLits'(_, _):- raise_exception('error'("lo.comp.code.code@anyLits", 24, 3, 17)).
'lo.comp.code.code@dispIns'('lo.core#[]', X_32102, 'lo.core#[]'):- !.
'lo.comp.code.code@dispIns'('lo.core#,..'('lo.comp.code.instructions#iLbl'(XLb), XL), XLits, 'lo.core#,..'('lo.core#ss'(XLb), 'lo.core#,..'('lo.core#ss'(":
"), XXd37171))):- !,
    'lo.comp.code.code@dispIns'(XL, XLits, XXd37171).
'lo.comp.code.code@dispIns'('lo.core#,..'(XI, XL), XLits, 'lo.core#,..'('lo.core#ss'("  "), 'lo.core#,..'(XXd37175, 'lo.core#,..'('lo.core#ss'("
"), XXd37177)))):- !,
    'lo.comp.code.instructions@showIns'(XI, XLits, XXd37175),
    'lo.comp.code.code@dispIns'(XL, XLits, XXd37177).
'lo.comp.code.code@dispIns'(_, _, _):- raise_exception('error'("lo.comp.code.code@dispIns", 19, 3, 19)).
'lo.comp.code.code@dispCodeSeg'('lo.comp.code.code#assem'(XNm, XIns, XLits, XSrcMap), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("program: "), 'lo.core#,..'(XXe4696, 'lo.core#,..'('lo.core#ss'("
"), 'lo.core#,..'('lo.core#ssSeq'(XXd37185), 'lo.core#,..'('lo.core#ssSeq'(XXd37187), 'lo.core#[]'))))))):- !,
    ocall('disp%1'(XXV5033),'lo.core$display$lo.comp.term*term','lo.core$display$lo.comp.term*term'),
    ocall('foldLeft%1'(XXV5036),'lo.collection$folding$lo.core*list','lo.collection$folding$lo.core*list'),
    ocall('_call%2'(XNm, XXe4696),XXV5033,XXV5033),
    ocall('_empty%1'(XXV5035),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'('lo.comp.code.code@fun117', XXV5035, XLits, XXe4698),XXV5036,XXV5036),
    'lo.comp.code.code@dispIns'(XIns, XXe4698, XXd37185),
    'lo.comp.code.code@anyLits'(XLits, XXd37187).
'lo.comp.code.code@dispCodeSeg'(_, _):- raise_exception('error'("lo.comp.code.code@dispCodeSeg", 16, 3, 167)).
'lo.core$display$lo.comp.code.code*assem'('lo.core$display$lo.comp.code.code*assem%1'('lo.core$display$lo.comp.code.code*assem')):- !.
'lo.core$display$lo.comp.code.code*assem'('disp%2'(XV29358, XV29359), XLbl2117, XThis2117):- !,
    'lo.core$display$lo.comp.code.code*assem@disp'(XV29358, XV29359, XLbl2117, XThis2117).
'lo.core$display$lo.comp.code.code*assem'('disp%1'('lo.core$display$lo.comp.code.code*assem^disp'(XLbl2118, XThis2118)), XLbl2118, XThis2118).
'lo.core$display$lo.comp.code.code*assem@disp'(XSeg, XXd37195, XLbV2400, XThV2400):- !,
    'lo.comp.code.code@dispCodeSeg'(XSeg, XXd37195).
'lo.core$display$lo.comp.code.code*assem@disp'(_, _):- raise_exception('error'("lo.core$display$lo.comp.code.code*assem@disp", 12, 5, 29)).
'lo.comp.code.code@dispLits'('lo.core#[]', 'lo.core#[]'):- !.
'lo.comp.code.code@dispLits'('lo.core#,..'('lo.comp.code.code#litrl'(XLbl, XTerm), XL), 'lo.core#,..'('lo.core#ss'(XLbl), 'lo.core#,..'('lo.core#ss'(":"), 'lo.core#,..'(XXe4699, 'lo.core#,..'('lo.core#ss'("
"), XXd37199))))):- !,
    ocall('disp%1'(XXV5037),'lo.core$display$lo.comp.term*term','lo.core$display$lo.comp.term*term'),
    ocall('_call%2'(XTerm, XXe4699),XXV5037,XXV5037),
    'lo.comp.code.code@dispLits'(XL, XXd37199).
'lo.comp.code.code@dispLits'(_, _):- raise_exception('error'("lo.comp.code.code@dispLits", 28, 3, 18)).
'lo.comp.code.code^anyLits'('_call%2'(XV29349, XV29350), 'lo.comp.code.code^anyLits', _):- 'lo.comp.code.code@anyLits'(XV29349, XV29350).
'lo.comp.code.code^dispIns'('_call%3'(XV29351, XV29352, XV29353), 'lo.comp.code.code^dispIns', _):- 'lo.comp.code.code@dispIns'(XV29351, XV29352, XV29353).
'lo.comp.code.code@fun117'('_call%3'(XM, 'lo.comp.code.code#litrl'(XN, XT), XXe4697), 'lo.comp.code.code@fun117', _):- !,
    ocall('_put%1'(XXV5034),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string'),'lo.collection$map$lo.index*map'('lo.core$equality$lo.core*string')),
    ocall('_call%4'(XM, XN, XT, XXe4697),XXV5034,XXV5034).
'lo.comp.code.code@fun117'(_, _, _):- raise_exception('error'("lo.comp.code.code@fun117", 16, 113, 23)).
'lo.comp.code.code^dispCodeSeg'('_call%2'(XV29354, XV29355), 'lo.comp.code.code^dispCodeSeg', _):- 'lo.comp.code.code@dispCodeSeg'(XV29354, XV29355).
'lo.core$display$lo.comp.code.code*assem^disp'('_call%2'(XV29356, XV29357), 'lo.core$display$lo.comp.code.code*assem^disp'(XLbV2400, XThV2400), _):- 'lo.core$display$lo.comp.code.code*assem@disp'(XV29356, XV29357, XLbV2400, XThV2400).
'lo.comp.code.code^dispLits'('_call%2'(XV29360, XV29361), 'lo.comp.code.code^dispLits', _):- 'lo.comp.code.code@dispLits'(XV29360, XV29361).
