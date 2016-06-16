:- use_module(ocall).
'lo.thing#export'("I6'ssStr'C1St'lo.thing*ss''thing't'lo.thing*thing''ssSp't'lo.thing*ss''ssNl't'lo.thing*ss''ssSeq'C1Lt'lo.thing*ss't'lo.thing*ss''ssIndent'C1t'lo.thing*ss't'lo.thing*ss'").
'lo.thing#types'("I3'ss'T2Yt'lo.thing*ss't'lo.thing*thing'Yt'lo.thing*ss'I1'show'F0t'lo.thing*ss''thing'T1Yt'lo.thing*thing'I1'show'F0t'lo.thing*ss''string'T2YSt'lo.thing*thing'YSI1'show'F0t'lo.thing*ss'").
'lo.thing#ssStr'('show%1'(XV20), XLbl20, XThis20) :- !,
    'lo.thing#ssStr^17'('show%1'(XV20), XLbl20, XThis20).
'lo.thing#thing'('show%1'(XV21), XLbl21, XThis21) :- !,
    'lo.thing#thing@show'(XV21, XLbl21, XThis21).
'lo.thing#thing@show'('lo.thing#ssStr'("thing"), XLbV22, XThV22) :- XLbV22 = 'lo.thing#thing',
    !.
'lo.thing#thing@show'(_, _, _) :- abort.
'lo.thing#ssSp'('show%1'(XV22), XLbl22, XThis22) :- !,
    'lo.thing#ssSp^18'('show%1'(XV22), XLbl22, XThis22).
'lo.thing#ssNl'('show%1'(XV23), XLbl23, XThis23) :- !,
    'lo.thing#ssNl^19'('show%1'(XV23), XLbl23, XThis23).
'lo.thing#ssSeq'('show%1'(XV24), XLbl24, XThis24) :- !,
    'lo.thing#ssSeq^20'('show%1'(XV24), XLbl24, XThis24).
'lo.thing#ssIndent'('show%1'(XV25), XLbl25, XThis25) :- !,
    'lo.thing#ssIndent^21'('show%1'(XV25), XLbl25, XThis25).
'lo.thing#ssStr^17'(XCV17, 'lo.thing#ssStr'(X_), XThV21) :- ocall(XCV17,'lo.thing#thing',XThV21).
'lo.thing#ssSp^18'(XCV18, 'lo.thing#ssSp', XThV23) :- ocall(XCV18,'lo.thing#thing',XThV23).
'lo.thing#ssNl^19'(XCV19, 'lo.thing#ssNl', XThV24) :- ocall(XCV19,'lo.thing#thing',XThV24).
'lo.thing#ssSeq^20'(XCV20, 'lo.thing#ssSeq'(X_), XThV25) :- ocall(XCV20,'lo.thing#thing',XThV25).
'lo.thing#ssIndent^21'(XCV21, 'lo.thing#ssIndent'(X_), XThV26) :- ocall(XCV21,'lo.thing#thing',XThV26).
