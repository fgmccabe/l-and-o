:- use_module(ocall).
'lo.thing#export'("I6'ssStr'C1t'lo.thing*string't'lo.thing*ss''thing't'lo.thing*thing''ssSp't'lo.thing*ss''ssNl't'lo.thing*ss''ssSeq'C1Lt'lo.thing*ss't'lo.thing*ss''ssIndent'C1t'lo.thing*ss't'lo.thing*ss'").
'lo.thing#types'("I3'ss'T2Yt'lo.thing*ss't'lo.thing*thing'Yt'lo.thing*ss'I1'show'F0t'lo.thing*ss''thing'T1Yt'lo.thing*thing'I1'show'F0t'lo.thing*ss''string'T2Yt'lo.thing*string't'lo.thing*thing'Yt'lo.thing*string'I1'show'F0t'lo.thing*ss'").
'lo.thing#ssStr'('show%1'(XV19), XLbl19, XThis19) :- !,
    'lo.thing#ssStr^16'('show%1'(XV19), XLbl19, XThis19).
'lo.thing#thing'('show%1'(XV20), XLbl20, XThis20) :- !,
    'lo.thing#thing@show'(XV20, XLbl20, XThis20).
'lo.thing#thing@show'('lo.thing#ssStr'("thing"), XLbV20, XThV20) :- XLbV20 = 'lo.thing#thing',
    !.
'lo.thing#thing@show'(_, _, _) :- abort.
'lo.thing#ssSp'('show%1'(XV21), XLbl21, XThis21) :- !,
    'lo.thing#ssSp^17'('show%1'(XV21), XLbl21, XThis21).
'lo.thing#ssNl'('show%1'(XV22), XLbl22, XThis22) :- !,
    'lo.thing#ssNl^18'('show%1'(XV22), XLbl22, XThis22).
'lo.thing#ssSeq'('show%1'(XV23), XLbl23, XThis23) :- !,
    'lo.thing#ssSeq^19'('show%1'(XV23), XLbl23, XThis23).
'lo.thing#ssIndent'('show%1'(XV24), XLbl24, XThis24) :- !,
    'lo.thing#ssIndent^20'('show%1'(XV24), XLbl24, XThis24).
'lo.thing#ssStr^16'(XCV16, 'lo.thing#ssStr'(X_), XThV19) :- ocall(XCV16,'lo.thing#thing',XThV19).
'lo.thing#ssSp^17'(XCV17, 'lo.thing#ssSp', XThV21) :- ocall(XCV17,'lo.thing#thing',XThV21).
'lo.thing#ssNl^18'(XCV18, 'lo.thing#ssNl', XThV22) :- ocall(XCV18,'lo.thing#thing',XThV22).
'lo.thing#ssSeq^19'(XCV19, 'lo.thing#ssSeq'(X_), XThV23) :- ocall(XCV19,'lo.thing#thing',XThV23).
'lo.thing#ssIndent^20'(XCV20, 'lo.thing#ssIndent'(X_), XThV24) :- ocall(XCV20,'lo.thing#thing',XThV24).
