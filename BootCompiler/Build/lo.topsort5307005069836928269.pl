'#pkg'("n7o7'()7'n2o2'pkg's'lo.topsort'e'*'n10o10'()10'n2o2'import'e'public'n2o2'pkg's'lo.core'e'*'n2o2'import'e'public'n2o2'pkg's'lo.list'e'*'n2o2'import'e'public'n2o2'pkg's'lo.index'e'*'n2o2'import'e'public'n2o2'pkg's'lo.io'e'*'n2o2'import'e'public'n2o2'pkg's'lo.coerce'e'*'n2o2'import'e'public'n2o2'pkg's'lo.bits'e'*'n2o2'import'e'public'n2o2'pkg's'lo.collection'e'*'n2o2'import'e'public'n2o2'pkg's'lo.sets'e'*'n2o2'import'e'public'n2o2'pkg's'lo.trie'e'*'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I1'topsort':k'e':k't'|FT1Lk't'LLk't'c'lo.topsort$depends'T1k't'T1k'e'\"s'I0'n0o0'()0'n1o1'()1'n4o4'()4's'depends's'lo.topsort$depends's\":k'e':k't'c'lo.topsort$depends'T1k't'T1k'e'\"s\":k'e':k't'I2'references'FT1k't'Lk'e''defines'PT2k't'k'e'\"n0o0'()0'").
'lo.topsort@init'() :- !.
'lo.topsort@pushDef'(Xlo_topsort_depends_t1, XD, XStk, 'lo.core#,..'((XD, XPt), XStk), XPt) :- 'lo.list@length'(XStk, XX22589),
    XPt = XX22589.
'lo.topsort@inStack'(Xlo_topsort_depends_t2, XRf, 'lo.core#,..'((XD, XPt), X_1537), XPt) :- ocall('defines%2'(XD, XRf),Xlo_topsort_depends_t2,Xlo_topsort_depends_t2).
'lo.topsort@inStack'(Xlo_topsort_depends_t2, XRf, 'lo.core#,..'(X_1538, XStk), XPt) :- 'lo.topsort@inStack'(Xlo_topsort_depends_t2, XRf, XStk, XPt).
'lo.topsort@minPoint'(XX, XY, XX) :- 'lo.core@=<'('lo.core$comp$lo.core*integer', XX, XY).
'lo.topsort@minPoint'(XX, XY, XY) :- 'lo.core@>'('lo.core$comp$lo.core*integer', XX, XY).
'lo.topsort@pickDef'(Xlo_topsort_depends_t3, XR, 'lo.core#,..'(XD, XDfs), XDfs, XD) :- ocall('defines%2'(XD, XR),Xlo_topsort_depends_t3,Xlo_topsort_depends_t3).
'lo.topsort@pickDef'(Xlo_topsort_depends_t3, XR, 'lo.core#,..'(XD, XDfs), 'lo.core#,..'(XD, XODfs), XDf) :- 'lo.topsort@pickDef'(Xlo_topsort_depends_t3, XR, XDfs, XODfs, XDf).
'lo.topsort@mkGroup'('lo.core#[]', XGrps, XGrps) :- !.
'lo.topsort@mkGroup'(XG, XGrps, 'lo.core#,..'(XG, XGrps)) :- !.
'lo.topsort@mkGroup'(_, _, _) :- raise_exception('error'("mkGroup", 69, 3, 24)).
'lo.topsort@popGroup'(Xlo_topsort_depends_t4, 'lo.core#,..'((XD, XDPt), XStk), XOStk, 'lo.core#,..'(XD, XDfs), XLow) :- ocall('>=%2'(XDPt, XLow),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer'),
    'lo.topsort@popGroup'(Xlo_topsort_depends_t4, XStk, XOStk, XDfs, XLow).
'lo.topsort@popGroup'(Xlo_topsort_depends_t4, XStk, XStk, 'lo.core#[]', X_1539).
'lo.topsort@popGroups'(Xlo_topsort_depends_t5, XStk, XStk, XGrps, XGrps, XLow, XPt) :- ocall('<%2'(XPt, XLow),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer').
'lo.topsort@popGroups'(Xlo_topsort_depends_t5, XStk, XOStk, XGrps, XX22693, XLow, XPt) :- 'lo.topsort@popGroup'(Xlo_topsort_depends_t5, XStk, XOStk, XGroup, XPt),
    'lo.topsort@mkGroup'(XGroup, XGrps, XX22693).
'lo.topsort@analyse'(Xlo_topsort_depends_t6, XRf, XDefs, XDefs, XStk, XStk, XG, XG, XLow, XPt) :- 'lo.topsort@inStack'(Xlo_topsort_depends_t6, XRf, XStk, XX),
    'lo.topsort@minPoint'(XX, XLow, XPt).
'lo.topsort@analyse'(Xlo_topsort_depends_t6, XRf, XDefs, XODefs, XStack, XOStack, XG, XOG, XLow, XPt) :- 'lo.topsort@pickDef'(Xlo_topsort_depends_t6, XRf, XDefs, XRDefs, XDef),
    'lo.topsort@analyseDef'(Xlo_topsort_depends_t6, XDef, XRDefs, XODefs, XStack, XOStack, XG, XOG, XDfPt),
    'lo.topsort@minPoint'(XLow, XDfPt, XPt).
'lo.topsort@analyse'(Xlo_topsort_depends_t6, X_1540, XDefs, XDefs, XStack, XStack, XGroups, XGroups, XLow, XLow).
'lo.topsort@analyseRefs'(Xlo_topsort_depends_t7, 'lo.core#[]', XDefs, XDefs, XStk, XStk, XGrp, XGrp, XPt, XPt).
'lo.topsort@analyseRefs'(Xlo_topsort_depends_t7, 'lo.core#,..'(XR, XRefs), XDefs, XODefs, XStk, XOStk, XG, XG1, XLow, XPt) :- 'lo.topsort@analyse'(Xlo_topsort_depends_t7, XR, XDefs, XIDefs, XStk, XS1, XG, XG0, XLow, XLow1),
    'lo.topsort@analyseRefs'(Xlo_topsort_depends_t7, XRefs, XIDefs, XODefs, XS1, XOStk, XG0, XG1, XLow1, XPt).
'lo.topsort@analyseDef'(Xlo_topsort_depends_t8, XDf, XDefs, XODefs, XStack, XOStack, XG, XOG, XPt) :- 'lo.topsort@pushDef'(Xlo_topsort_depends_t8, XDf, XStack, XS0, XSPt),
    ocall('references%2'(XDf, XX22813),Xlo_topsort_depends_t8,Xlo_topsort_depends_t8),
    'lo.topsort@analyseRefs'(Xlo_topsort_depends_t8, XX22813, XDefs, XODefs, XS0, XS1, XG, XG1, XSPt, XPt),
    'lo.topsort@popGroups'(Xlo_topsort_depends_t8, XS1, XOStack, XG1, XOG, XSPt, XPt).
'lo.topsort@analyseDefs'(Xlo_topsort_depends_t9, 'lo.core#[]', XGroups, XGroups) :- !.
'lo.topsort@analyseDefs'(Xlo_topsort_depends_t9, 'lo.core#,..'(XDef, XDefs), XGrps, XX22851) :- 'lo.topsort@analyseDef'(Xlo_topsort_depends_t9, XDef, XDefs, XIDefs, 'lo.core#[]', X_1541, XGrps, XG0, X_1542),
    !,
    'lo.topsort@analyseDefs'(Xlo_topsort_depends_t9, XIDefs, XG0, XX22851).
'lo.topsort@analyseDefs'(_, _, _) :- raise_exception('error'("analyseDefs", 14, 3, 32)).
'lo.topsort@topsort'(Xlo_topsort_depends_t10, XDefs, XX22858) :- !,
    'lo.topsort@analyseDefs'(Xlo_topsort_depends_t10, XDefs, 'lo.core#[]', XX22857),
    'lo.list@reverse'(XX22857, XX22858).
'lo.topsort@topsort'(_, _) :- raise_exception('error'("topsort", 11, 3, 46)).
'lo.topsort^pushDef'('_call%4'(XV3201, XV3202, XV3203, XV3204), 'lo.topsort^pushDef', _) :- 'lo.topsort@pushDef'(XV3201, XV3202, XV3203, XV3204).
'lo.topsort^inStack'('_call%3'(XV3205, XV3206, XV3207), 'lo.topsort^inStack', _) :- 'lo.topsort@inStack'(XV3205, XV3206, XV3207).
'lo.topsort^minPoint'('_call%3'(XV3208, XV3209, XV3210), 'lo.topsort^minPoint', _) :- 'lo.topsort@minPoint'(XV3208, XV3209, XV3210).
'lo.topsort^pickDef'('_call%4'(XV3211, XV3212, XV3213, XV3214), 'lo.topsort^pickDef', _) :- 'lo.topsort@pickDef'(XV3211, XV3212, XV3213, XV3214).
'lo.topsort^mkGroup'('_call%3'(XV3215, XV3216, XV3217), 'lo.topsort^mkGroup', _) :- 'lo.topsort@mkGroup'(XV3215, XV3216, XV3217).
'lo.topsort^popGroup'('_call%4'(XV3218, XV3219, XV3220, XV3221), 'lo.topsort^popGroup', _) :- 'lo.topsort@popGroup'(XV3218, XV3219, XV3220, XV3221).
'lo.topsort^popGroups'('_call%6'(XV3222, XV3223, XV3224, XV3225, XV3226, XV3227), 'lo.topsort^popGroups', _) :- 'lo.topsort@popGroups'(XV3222, XV3223, XV3224, XV3225, XV3226, XV3227).
'lo.topsort^analyse'('_call%9'(XV3228, XV3229, XV3230, XV3231, XV3232, XV3233, XV3234, XV3235, XV3236), 'lo.topsort^analyse', _) :- 'lo.topsort@analyse'(XV3228, XV3229, XV3230, XV3231, XV3232, XV3233, XV3234, XV3235, XV3236).
'lo.topsort^analyseRefs'('_call%9'(XV3237, XV3238, XV3239, XV3240, XV3241, XV3242, XV3243, XV3244, XV3245), 'lo.topsort^analyseRefs', _) :- 'lo.topsort@analyseRefs'(XV3237, XV3238, XV3239, XV3240, XV3241, XV3242, XV3243, XV3244, XV3245).
'lo.topsort^analyseDef'('_call%8'(XV3246, XV3247, XV3248, XV3249, XV3250, XV3251, XV3252, XV3253), 'lo.topsort^analyseDef', _) :- 'lo.topsort@analyseDef'(XV3246, XV3247, XV3248, XV3249, XV3250, XV3251, XV3252, XV3253).
'lo.topsort^analyseDefs'('_call%3'(XV3254, XV3255, XV3256), 'lo.topsort^analyseDefs', _) :- 'lo.topsort@analyseDefs'(XV3254, XV3255, XV3256).
'lo.topsort^topsort'('_call%2'(XV3257, XV3258), 'lo.topsort^topsort', _) :- 'lo.topsort@topsort'(XV3257, XV3258).
