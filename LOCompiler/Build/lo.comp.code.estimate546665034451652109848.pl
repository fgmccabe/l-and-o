'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.code.estimate's'0.0.1'n2o2'()2'n2o2'import'e'private'n2o2'pkg's'lo'e'*'n2o2'import'e'private'n2o2'pkg's'lo.comp.term'e'*'s\"I3'estimateUnifySpace'FT2t'lo.comp.term*term'li'estimateArgSpace'FT2Lt'lo.comp.term*term'li'estimateGoalSpace'FT1t'lo.comp.term*pred'i\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.code.estimate@init'():- !.
'lo.comp.code.estimate@estimateEls'('lo.core#[]', X_32080, 0):- !.
'lo.comp.code.estimate@estimateEls'('lo.core#,..'(Xt, Xl), Xtop, XXe4687):- !,
    ocall('+%1'(XXV5024),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.estimate@estimateUnifySpace'(Xt, Xtop, XXd37154),
    'lo.comp.code.estimate@estimateEls'(Xl, Xtop, XXd37155),
    ocall('_call%3'(XXd37154, XXd37155, XXe4687),XXV5024,XXV5024).
'lo.comp.code.estimate@estimateEls'(_, _, _):- raise_exception('error'("lo.comp.code.estimate@estimateEls", 17, 3, 22)).
'lo.comp.code.estimate@estimateUnifySpace'('lo.comp.term#anon', X_32082, 0):- !.
'lo.comp.code.estimate@estimateUnifySpace'('lo.comp.term#varbl'(X_32083), 'lo.core#true', 2):- !.
'lo.comp.code.estimate@estimateUnifySpace'('lo.comp.term#varbl'(X_32084), 'lo.core#false', 0):- !.
'lo.comp.code.estimate@estimateUnifySpace'(XT, X_32085, 0):- 'lo.comp.term@isGroundTerm'(XT),
    !.
'lo.comp.code.estimate@estimateUnifySpace'('lo.comp.term#cons'(XOp, XEls), X_32086, XXe4690):- !,
    ocall('size%1'(XXV5025),'lo.core$sizeable$lo.core*list','lo.core$sizeable$lo.core*list'),
    ocall('+%1'(XXV5026),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('+%1'(XXV5027),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.estimate@estimateEls'(XEls, 'lo.core#false', XXd37156),
    ocall('_call%2'(XEls, XXe4688),XXV5025,XXV5025),
    ocall('_call%3'(XXd37156, XXe4688, XXe4689),XXV5026,XXV5026),
    ocall('_call%3'(XXe4689, 2, XXe4690),XXV5027,XXV5027).
'lo.comp.code.estimate@estimateUnifySpace'(_, _, _):- raise_exception('error'("lo.comp.code.estimate@estimateUnifySpace", 10, 3, 31)).
'lo.comp.code.estimate@estimateArgSpace'(XArgs, Xtop, XXd37157):- !,
    'lo.comp.code.estimate@estimateEls'(XArgs, Xtop, XXd37157).
'lo.comp.code.estimate@estimateArgSpace'(_, _, _):- raise_exception('error'("lo.comp.code.estimate@estimateArgSpace", 20, 3, 51)).
'lo.comp.code.estimate@estimateGoalSpace'('lo.comp.term#call'(X_32087, X_32088, XArgs), XXd37158):- !,
    'lo.comp.code.estimate@estimateArgSpace'(XArgs, 'lo.core#true', XXd37158).
'lo.comp.code.estimate@estimateGoalSpace'('lo.comp.term#ecall'(X_32089, X_32090, XArgs), XXd37159):- !,
    'lo.comp.code.estimate@estimateArgSpace'(XArgs, 'lo.core#true', XXd37159).
'lo.comp.code.estimate@estimateGoalSpace'('lo.comp.term#unfy'(X_32091, XL, XR), XXe4691):- !,
    ocall('+%1'(XXV5028),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.estimate@estimateUnifySpace'(XL, 'lo.core#true', XXd37160),
    'lo.comp.code.estimate@estimateUnifySpace'(XR, 'lo.core#true', XXd37161),
    ocall('_call%3'(XXd37160, XXd37161, XXe4691),XXV5028,XXV5028).
'lo.comp.code.estimate@estimateGoalSpace'('lo.comp.term#ocall'(X_32092, XC, XT, XL), XXe4693):- !,
    ocall('+%1'(XXV5029),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    ocall('+%1'(XXV5030),'lo.core$additive$lo.core*integer','lo.core$additive$lo.core*integer'),
    'lo.comp.code.estimate@estimateUnifySpace'(XC, 'lo.core#true', XXd37162),
    'lo.comp.code.estimate@estimateUnifySpace'(XT, 'lo.core#true', XXd37163),
    ocall('_call%3'(XXd37162, XXd37163, XXe4692),XXV5029,XXV5029),
    'lo.comp.code.estimate@estimateUnifySpace'(XL, 'lo.core#true', XXd37164),
    ocall('_call%3'(XXe4692, XXd37164, XXe4693),XXV5030,XXV5030).
'lo.comp.code.estimate@estimateGoalSpace'('lo.comp.term#neck', 0):- !.
'lo.comp.code.estimate@estimateGoalSpace'('lo.comp.term#fail', 0):- !.
'lo.comp.code.estimate@estimateGoalSpace'('lo.comp.term#except'(X_32093, XT), XXd37165):- !,
    'lo.comp.code.estimate@estimateUnifySpace'(XT, 'lo.core#true', XXd37165).
'lo.comp.code.estimate@estimateGoalSpace'(_, _):- raise_exception('error'("lo.comp.code.estimate@estimateGoalSpace", 22, 3, 64)).
'lo.comp.code.estimate^estimateEls'('_call%3'(XV29337, XV29338, XV29339), 'lo.comp.code.estimate^estimateEls', _):- 'lo.comp.code.estimate@estimateEls'(XV29337, XV29338, XV29339).
'lo.comp.code.estimate^estimateUnifySpace'('_call%3'(XV29340, XV29341, XV29342), 'lo.comp.code.estimate^estimateUnifySpace', _):- 'lo.comp.code.estimate@estimateUnifySpace'(XV29340, XV29341, XV29342).
'lo.comp.code.estimate^estimateArgSpace'('_call%3'(XV29343, XV29344, XV29345), 'lo.comp.code.estimate^estimateArgSpace', _):- 'lo.comp.code.estimate@estimateArgSpace'(XV29343, XV29344, XV29345).
'lo.comp.code.estimate^estimateGoalSpace'('_call%2'(XV29346, XV29347), 'lo.comp.code.estimate^estimateGoalSpace', _):- 'lo.comp.code.estimate@estimateGoalSpace'(XV29346, XV29347).
