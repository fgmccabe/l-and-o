'#pkg'("n7o7'()7'n2o2'pkg's'lo.unit's'1.0.0'n1o1'()1'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I1'checkUnit'PT1t'lo.unit*harness'\"s\"I1'harness'Yt'lo.unit*harness'I2'doTest'PT0'desc'S\"n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.unit@init'():- !.
'lo.unit#harness'('harness%1'('lo.unit@harness'())):- !.
'lo.unit#harness'('doTest%0'(), XLbl4124, XThis4124):- !,
    'lo.unit#harness@doTest'(XLbl4124, XThis4124).
'lo.unit#harness'('doTest%1'('lo.unit#harness^doTest'(XLbl4125, XThis4125)), XLbl4125, XThis4125).
'lo.unit#harness@doTest'(XLbV1791, XThV1791):- XLbV1791 = 'lo.unit#harness'(XNm).
'lo.core$display$lo.unit*harness'('lo.core$display$lo.unit*harness%1'('lo.core$display$lo.unit*harness')):- !.
'lo.core$display$lo.unit*harness'('disp%2'(XV19856, XV19857), XLbl4126, XThis4126):- !,
    'lo.core$display$lo.unit*harness@disp'(XV19856, XV19857, XLbl4126, XThis4126).
'lo.core$display$lo.unit*harness'('disp%1'('lo.core$display$lo.unit*harness^disp'(XLbl4127, XThis4127)), XLbl4127, XThis4127).
'lo.core$display$lo.unit*harness@disp'(XH, 'lo.core#ss'(XXV2068), XLbV1792, XThV1792):- !,
    ocall('desc%1'(XXV2068),XH,XH).
'lo.core$display$lo.unit*harness@disp'(_, _):- raise_exception('error'("lo.core$display$lo.unit*harness@disp", 16, 5, 21)).
'lo.unit@check'(XT):- 'lo.unit@cond100'(XXd9583, XXd9582, XXd9581, XXd9580, XXd9579, XXe2048, XXV2070, XXd9578, XXd9577, XXd9576, XXd9575, XXd9574, XXe2047, XXV2069, XT).
'lo.unit@checkUnit'(XT):- ocall('disp%1'(XXV2071),'lo.core$display$lo.unit*harness','lo.core$display$lo.unit*harness'),
    ocall('_call%2'(XT, XXe2049),XXV2071,XXV2071),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("starting "), 'lo.core#,..'(XXe2049, 'lo.core#[]'))), XXd9588),
    '_logmsg'(XXd9588),
    'lo.unit@check'(XT).
'lo.unit#harness^doTest'('_call%2'(XV19852, XV19853), 'lo.unit#harness^doTest'(XLbV1791, XThV1791), _):- 'lo.unit#harness@doTest'(XV19852, XV19853, XLbV1791, XThV1791).
'lo.core$display$lo.unit*harness^disp'('_call%2'(XV19854, XV19855), 'lo.core$display$lo.unit*harness^disp'(XLbV1792, XThV1792), _):- 'lo.core$display$lo.unit*harness@disp'(XV19854, XV19855, XLbV1792, XThV1792).
'lo.unit@cond100'(XXd9583, XXd9582, XXd9581, XXd9580, XXd9579, XXe2048, XXV2070, XXd9578, XXd9577, XXd9576, XXd9575, XXd9574, XXe2047, XXV2069, XT):- ocall('doTest%0'(),XT,XT),
    !,
    ocall('disp%1'(XXV2069),'lo.core$display$lo.unit*harness','lo.core$display$lo.unit*harness'),
    ocall('_call%2'(XT, XXe2047),XXV2069,XXV2069),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XXe2047, 'lo.core#,..'('lo.core#ss'(" ok"), 'lo.core#[]'))), XXd9578),
    '_logmsg'(XXd9578).
'lo.unit@cond100'(XXd9583, XXd9582, XXd9581, XXd9580, XXd9579, XXe2048, XXV2070, XXd9578, XXd9577, XXd9576, XXd9575, XXd9574, XXe2047, XXV2069, XT):- ocall('disp%1'(XXV2070),'lo.core$display$lo.unit*harness','lo.core$display$lo.unit*harness'),
    ocall('_call%2'(XT, XXe2048),XXV2070,XXV2070),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'(XXe2048, 'lo.core#,..'('lo.core#ss'(" failed"), 'lo.core#[]'))), XXd9583),
    '_logmsg'(XXd9583).
'lo.unit^check'('_call%1'(XV19858), 'lo.unit^check', _):- 'lo.unit@check'(XV19858).
'lo.unit^checkUnit'('_call%1'(XV19859), 'lo.unit^checkUnit', _):- 'lo.unit@checkUnit'(XV19859).
