'#pkg'("n7o7'()7'n2o2'pkg's'lo.topsort's'1.0.0'n1o1'()1'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I1'topsort':k'e':k't'|FT1Lk't'LLk't'c'lo.topsort$depends'T1k't'T1k'e'\"s'I0'n0o0'()0'n1o1'()1'n4o4'()4's'depends's'lo.topsort$depends's\":k'e':k't'c'lo.topsort$depends'T1k't'T1k'e'\"s\":k'e':k't'I2'defines'PT2k't'k'e''references'FT1k't'Lk'e'\"n0o0'()0'").
'lo.topsort@init'():- !.
'lo.topsort@pushDef'(Xdepends31, XD, XStk, 'lo.core#,..'('()2'(XD, XPt), XStk), XPt):- 'lo.list@length'(XStk, XXd27596),
    XPt = XXd27596.
'lo.topsort@pickDef'(Xdepends32, XR, 'lo.core#,..'(XD, XDfs), XDfs, XD):- ocall('defines%2'(XD, XR),Xdepends32,Xdepends32).
'lo.topsort@pickDef'(Xdepends32, XR, 'lo.core#,..'(XD, XDfs), 'lo.core#,..'(XD, XODfs), XDf):- 'lo.topsort@pickDef'(Xdepends32, XR, XDfs, XODfs, XDf).
'lo.topsort@inStack'(Xdepends33, XRf, 'lo.core#,..'('()2'(XD, XPt), X_22459), XPt):- ocall('defines%2'(XD, XRf),Xdepends33,Xdepends33).
'lo.topsort@inStack'(Xdepends33, XRf, 'lo.core#,..'(X_22461, XStk), XPt):- 'lo.topsort@inStack'(Xdepends33, XRf, XStk, XPt).
'lo.topsort@minPoint'(XX, XY, XX):- 'lo.core@=<'('lo.core$comp$lo.core*integer', XX, XY).
'lo.topsort@minPoint'(XX, XY, XY):- 'lo.core@>'('lo.core$comp$lo.core*integer', XX, XY).
'lo.topsort@mkGroup'('lo.core#[]', XGrps, XGrps):- !.
'lo.topsort@mkGroup'(XG, XGrps, 'lo.core#,..'(XG, XGrps)):- !.
'lo.topsort@mkGroup'(_, _, _):- raise_exception('error'("lo.topsort@mkGroup", 69, 3, 24)).
'lo.topsort@popGroup'(Xdepends34, 'lo.core#,..'('()2'(XD, XDPt), XStk), XOStk, 'lo.core#,..'(XD, XDfs), XLow):- ocall('>=%2'(XDPt, XLow),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer'),
    'lo.topsort@popGroup'(Xdepends34, XStk, XOStk, XDfs, XLow).
'lo.topsort@popGroup'(Xdepends34, XStk, XStk, 'lo.core#[]', X_22465).
'lo.topsort@popGroups'(Xdepends35, XStk, XStk, XGrps, XGrps, XLow, XPt):- ocall('<%2'(XPt, XLow),'lo.core$comp$lo.core*integer','lo.core$comp$lo.core*integer').
'lo.topsort@popGroups'(Xdepends35, XStk, XOStk, XGrps, XXb12249, XLow, XPt):- 'lo.topsort@popGroup'(Xdepends35, XStk, XOStk, XGroup, XPt),
    'lo.topsort@mkGroup'(XGroup, XGrps, XXb12249).
'lo.topsort@analyse'(Xdepends36, XRf, XDefs, XDefs, XStk, XStk, XG, XG, XLow, XPt):- 'lo.topsort@inStack'(Xdepends36, XRf, XStk, XX),
    'lo.topsort@minPoint'(XX, XLow, XPt).
'lo.topsort@analyse'(Xdepends36, XRf, XDefs, XODefs, XStack, XOStack, XG, XOG, XLow, XPt):- 'lo.topsort@pickDef'(Xdepends36, XRf, XDefs, XRDefs, XDef),
    'lo.topsort@analyseDef'(Xdepends36, XDef, XRDefs, XODefs, XStack, XOStack, XG, XOG, XDfPt),
    'lo.topsort@minPoint'(XLow, XDfPt, XPt).
'lo.topsort@analyse'(Xdepends36, X_22466, XDefs, XDefs, XStack, XStack, XGroups, XGroups, XLow, XLow).
'lo.topsort@analyseRefs'(Xdepends37, 'lo.core#[]', XDefs, XDefs, XStk, XStk, XGrp, XGrp, XPt, XPt).
'lo.topsort@analyseRefs'(Xdepends37, 'lo.core#,..'(XR, XRefs), XDefs, XODefs, XStk, XOStk, XG, XG1, XLow, XPt):- 'lo.topsort@analyse'(Xdepends37, XR, XDefs, XIDefs, XStk, XS1, XG, XG0, XLow, XLow1),
    'lo.topsort@analyseRefs'(Xdepends37, XRefs, XIDefs, XODefs, XS1, XOStk, XG0, XG1, XLow1, XPt).
'lo.topsort@analyseDef'(Xdepends38, XDf, XDefs, XODefs, XStack, XOStack, XG, XOG, XPt):- 'lo.topsort@pushDef'(Xdepends38, XDf, XStack, XS0, XSPt),
    ocall('references%1'(XXV3496),Xdepends38,Xdepends38),
    ocall('_call%2'(XDf, XXe3261),XXV3496,XXV3496),
    'lo.topsort@analyseRefs'(Xdepends38, XXe3261, XDefs, XODefs, XS0, XS1, XG, XG1, XSPt, XPt),
    'lo.topsort@popGroups'(Xdepends38, XS1, XOStack, XG1, XOG, XSPt, XPt).
'lo.topsort@analyseDefs'(Xdepends39, 'lo.core#[]', XGroups, XGroups):- !.
'lo.topsort@analyseDefs'(Xdepends39, 'lo.core#,..'(XDef, XDefs), XGrps, XXd27598):- 'lo.topsort@analyseDef'(Xdepends39, XDef, XDefs, XIDefs, 'lo.core#[]', X_22469, XGrps, XG0, X_22470),
    !,
    'lo.topsort@analyseDefs'(Xdepends39, XIDefs, XG0, XXd27598).
'lo.topsort@analyseDefs'(_, _, _, _):- raise_exception('error'("lo.topsort@analyseDefs", 14, 3, 32)).
'lo.topsort@topsort'(Xdepends40, XDefs, XXd27600):- !,
    'lo.topsort@analyseDefs'(Xdepends40, XDefs, 'lo.core#[]', XXd27599),
    'lo.list@reverse'(XXd27599, XXd27600).
'lo.topsort@topsort'(_, _, _):- raise_exception('error'("lo.topsort@topsort", 11, 3, 46)).
'lo.topsort^pushDef'('_call%5'(XV21182, XV21183, XV21184, XV21185, XV21186), 'lo.topsort^pushDef', _):- 'lo.topsort@pushDef'(XV21182, XV21183, XV21184, XV21185, XV21186).
'lo.topsort^pickDef'('_call%5'(XV21187, XV21188, XV21189, XV21190, XV21191), 'lo.topsort^pickDef', _):- 'lo.topsort@pickDef'(XV21187, XV21188, XV21189, XV21190, XV21191).
'lo.topsort^inStack'('_call%4'(XV21192, XV21193, XV21194, XV21195), 'lo.topsort^inStack', _):- 'lo.topsort@inStack'(XV21192, XV21193, XV21194, XV21195).
'lo.topsort^minPoint'('_call%3'(XV21196, XV21197, XV21198), 'lo.topsort^minPoint', _):- 'lo.topsort@minPoint'(XV21196, XV21197, XV21198).
'lo.topsort^mkGroup'('_call%3'(XV21199, XV21200, XV21201), 'lo.topsort^mkGroup', _):- 'lo.topsort@mkGroup'(XV21199, XV21200, XV21201).
'lo.topsort^popGroup'('_call%5'(XV21202, XV21203, XV21204, XV21205, XV21206), 'lo.topsort^popGroup', _):- 'lo.topsort@popGroup'(XV21202, XV21203, XV21204, XV21205, XV21206).
'lo.topsort^popGroups'('_call%7'(XV21207, XV21208, XV21209, XV21210, XV21211, XV21212, XV21213), 'lo.topsort^popGroups', _):- 'lo.topsort@popGroups'(XV21207, XV21208, XV21209, XV21210, XV21211, XV21212, XV21213).
'lo.topsort^analyse'('_call%10'(XV21214, XV21215, XV21216, XV21217, XV21218, XV21219, XV21220, XV21221, XV21222, XV21223), 'lo.topsort^analyse', _):- 'lo.topsort@analyse'(XV21214, XV21215, XV21216, XV21217, XV21218, XV21219, XV21220, XV21221, XV21222, XV21223).
'lo.topsort^analyseRefs'('_call%10'(XV21224, XV21225, XV21226, XV21227, XV21228, XV21229, XV21230, XV21231, XV21232, XV21233), 'lo.topsort^analyseRefs', _):- 'lo.topsort@analyseRefs'(XV21224, XV21225, XV21226, XV21227, XV21228, XV21229, XV21230, XV21231, XV21232, XV21233).
'lo.topsort^analyseDef'('_call%9'(XV21234, XV21235, XV21236, XV21237, XV21238, XV21239, XV21240, XV21241, XV21242), 'lo.topsort^analyseDef', _):- 'lo.topsort@analyseDef'(XV21234, XV21235, XV21236, XV21237, XV21238, XV21239, XV21240, XV21241, XV21242).
'lo.topsort^analyseDefs'('_call%4'(XV21243, XV21244, XV21245, XV21246), 'lo.topsort^analyseDefs', _):- 'lo.topsort@analyseDefs'(XV21243, XV21244, XV21245, XV21246).
'lo.topsort^topsort'('_call%3'(XV21247, XV21248, XV21249), 'lo.topsort^topsort', _):- 'lo.topsort@topsort'(XV21247, XV21248, XV21249).
