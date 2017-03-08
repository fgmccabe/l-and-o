'#pkg'("n7o7'()7'n2o2'pkg's'lo's'1.0.0'n9o9'()9'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'s\"I2'flatten'FT1t'lo.core*ss'LS'formatSS'FT1t'lo.core*ss'S\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo@init'():- !.
'lo@flttnList'('lo.core#[]', XRest, XRest):- !.
'lo@flttnList'('lo.core#,..'(XE, XR), XRest, XXd8805):- !,
    'lo@flttnList'(XR, XRest, XXd8804),
    'lo@flttn'(XE, XXd8804, XXd8805).
'lo@flttnList'(_, _, _):- raise_exception('error'("lo@flttnList", 21, 3, 26)).
'lo@flttn'('lo.core#ss'(XS), XRest, 'lo.core#,..'(XS, XRest)):- !.
'lo@flttn'('lo.core#ssSeq'(XL), XRest, XXd8807):- !,
    'lo@flttnList'(XL, XRest, XXd8807).
'lo@flttn'('lo.core#sc'(XC), XRest, 'lo.core#,..'(XXc824, XRest)):- !,
    'implode'('lo.core#,..'(XC, 'lo.core#[]'), XXc824).
'lo@flttn'(_, _, _):- raise_exception('error'("lo@flttn", 16, 3, 31)).
'lo@flatten'(XD, XXd8810):- !,
    'lo@flttn'(XD, 'lo.core#[]', XXd8810).
'lo@flatten'(_, _):- raise_exception('error'("lo@flatten", 13, 3, 25)).
'lo@formatSS'(XS, XXc825):- !,
    'lo@flatten'(XS, XXd8811),
    '_str_multicat'(XXd8811, XXc825).
'lo@formatSS'(_, _):- raise_exception('error'("lo@formatSS", 25, 3, 40)).
'lo^flttnList'('_call%3'(XV18274, XV18275, XV18276), 'lo^flttnList', _):- 'lo@flttnList'(XV18274, XV18275, XV18276).
'lo^flttn'('_call%3'(XV18277, XV18278, XV18279), 'lo^flttn', _):- 'lo@flttn'(XV18277, XV18278, XV18279).
'lo^flatten'('_call%2'(XV18280, XV18281), 'lo^flatten', _):- 'lo@flatten'(XV18280, XV18281).
'lo^formatSS'('_call%2'(XV18282, XV18283), 'lo^formatSS', _):- 'lo@formatSS'(XV18282, XV18283).
