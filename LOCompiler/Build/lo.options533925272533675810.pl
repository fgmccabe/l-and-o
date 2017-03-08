'#pkg'("n7o7'()7'n2o2'pkg's'lo.options's'1.0.0'n2o2'()2'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.either'e'*'s\"I1'processOptions':k'o'FT3LSLUz1'lo.options*optionsProcessor'1k'o'k'o'Uz2'lo.either*either'2T2k'o'LSS\"s\"I1'optionsProcessor':k'o':k'o'YUz1'lo.options*optionsProcessor'1k'o'I5'shortForm'S'alternatives'LS'usage'S'validator'Uz1'lo.core*option'1PT1S'setOption'FT2Sk'o'k'o'\"n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.options@init'():- !.
'lo.options@collectUsage'(XSpecs, XXd28983):- !,
    ocall('//%1'(XXV3696),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    ocall('//%1'(XXV3697),'lo.collection$mapping$lo.core*list','lo.collection$mapping$lo.core*list'),
    ocall('_call%3'(XSpecs, 'lo.options@fun71', XXe3420),XXV3696,XXV3696),
    'lo.list@interleave'(XXe3420, "
", XXd28979),
    ocall('_call%3'(XXd28979, 'lo.options@fun72', XXe3421),XXV3697,XXV3697),
    'lo@formatSS'('lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("Usage: 
"), XXe3421)), XXd28983).
'lo.options@collectUsage'(_, _):- raise_exception('error'("lo.options@collectUsage", 36, 3, 112)).
'lo.options@checkOption'(XArgs, XO, 'lo.core#none', Xsetter, XSpecs, XSoFar, XXd28985):- !,
    ocall('_call%3'(XO, XSoFar, XXe3422),Xsetter,Xsetter),
    'lo.options@processAll'(XArgs, XSpecs, XXe3422, XXd28985).
'lo.options@checkOption'('lo.core#,..'(XA, XArgs), XO, 'lo.core#some'(XV), XSetter, XSpecs, XSoFar, XXd28987):- ocall('_call%1'(XA),XV,XV),
    !,
    ocall('_call%3'(XA, XSoFar, XXe3423),XSetter,XSetter),
    'lo.options@processAll'(XArgs, XSpecs, XXe3423, XXd28987).
'lo.options@checkOption'(X_23899, X_23900, X_23901, X_23902, XSpecs, X_23903, 'lo.either#alternate'(XXd28988)):- !,
    'lo.options@collectUsage'(XSpecs, XXd28988).
'lo.options@checkOption'(_, _, _, _, _, _, _):- raise_exception('error'("lo.options@checkOption", 30, 3, 85)).
'lo.options@processOption'(XA, XL, XSpecs, XSoFar, XXd28990):- 'lo.options@one205'(XXV3699, XXV3698, XA, XSpecs, XO),
    !,
    ocall('shortForm%1'(XXV3700),XO,XO),
    ocall('validator%1'(XXV3701),XO,XO),
    ocall('setOption%1'(XXV3702),XO,XO),
    'lo.options@checkOption'(XL, XXV3700, XXV3701, XXV3702, XSpecs, XSoFar, XXd28990).
'lo.options@processOption'(XA, XL, X_23904, XSoFar, 'lo.either#either'('()2'(XSoFar, 'lo.core#,..'(XA, XL)))):- !.
'lo.options@processOption'(_, _, _, _, _):- raise_exception('error'("lo.options@processOption", 24, 3, 162)).
'lo.options@processAll'('lo.core#[]', X_23906, XSoFar, 'lo.either#either'('()2'(XSoFar, 'lo.core#[]'))):- !.
'lo.options@processAll'('lo.core#,..'("--", XA), X_23908, XSoFar, 'lo.either#either'('()2'(XSoFar, XA))):- !.
'lo.options@processAll'('lo.core#,..'(XA, XL), XSpecs, XSoFar, XXd28995):- !,
    'lo.options@processOption'(XA, XL, XSpecs, XSoFar, XXd28995).
'lo.options@processAll'(_, _, _, _):- raise_exception('error'("lo.options@processAll", 19, 3, 44)).
'lo.options@processOptions'(XRaw, XSpecs, XOpts, XXd28996):- !,
    'lo.options@processAll'(XRaw, XSpecs, XOpts, XXd28996).
'lo.options@processOptions'(_, _, _, _):- raise_exception('error'("lo.options@processOptions", 16, 3, 60)).
'lo.options@fun71'('_call%2'(XO, XXV3695), 'lo.options@fun71', _):- !,
    ocall('usage%1'(XXV3695),XO,XO).
'lo.options@fun71'(_, _, _):- raise_exception('error'("lo.options@fun71", 36, 79, 12)).
'lo.options@fun72'('_call%2'(XX, 'lo.core#ss'(XX)), 'lo.options@fun72', _):- !.
'lo.options@fun72'(_, _, _):- raise_exception('error'("lo.options@fun72", 36, 101, 10)).
'lo.options^collectUsage'('_call%2'(XV23084, XV23085), 'lo.options^collectUsage', _):- 'lo.options@collectUsage'(XV23084, XV23085).
'lo.options^checkOption'('_call%7'(XV23086, XV23087, XV23088, XV23089, XV23090, XV23091, XV23092), 'lo.options^checkOption', _):- 'lo.options@checkOption'(XV23086, XV23087, XV23088, XV23089, XV23090, XV23091, XV23092).
'lo.options@or119'(XXV3699, XXV3698, XO, XA):- ocall('shortForm%1'(XXV3698),XO,XO),
    ocall('==%2'(XA, XXV3698),'lo.core$equality$lo.core*string','lo.core$equality$lo.core*string').
'lo.options@or119'(XXV3699, XXV3698, XO, XA):- ocall('alternatives%1'(XXV3699),XO,XO),
    ocall('in%2'(XA, XXV3699),'lo.collection$membership$lo.core*list','lo.collection$membership$lo.core*list').
'lo.options@one205'(XXV3699, XXV3698, XA, XSpecs, XO):- 'lo.list@listEl'(XO, XSpecs),
    'lo.options@or119'(XXV3699, XXV3698, XO, XA),
    !.
'lo.options^processOption'('_call%5'(XV23093, XV23094, XV23095, XV23096, XV23097), 'lo.options^processOption', _):- 'lo.options@processOption'(XV23093, XV23094, XV23095, XV23096, XV23097).
'lo.options^processAll'('_call%4'(XV23098, XV23099, XV23100, XV23101), 'lo.options^processAll', _):- 'lo.options@processAll'(XV23098, XV23099, XV23100, XV23101).
'lo.options^processOptions'('_call%4'(XV23102, XV23103, XV23104, XV23105), 'lo.options^processOptions', _):- 'lo.options@processOptions'(XV23102, XV23103, XV23104, XV23105).
