:- use_module(ocall).
'lo.thing#export'("I6'ssStr'C1St'lo.thing*ss''thing't'lo.thing*thing''ssSp't'lo.thing*ss''ssNl't'lo.thing*ss''ssSeq'C1Lt'lo.thing*ss't'lo.thing*ss''ssIndent'C1t'lo.thing*ss't'lo.thing*ss'").
'lo.thing#types'("I3'ss'T2Yt'lo.thing*ss't'lo.thing*thing'Yt'lo.thing*ss'I1'show'F0t'lo.thing*ss''thing'T1Yt'lo.thing*thing'I1'show'F0t'lo.thing*ss''string'T2YSt'lo.thing*thing'YSI1'show'F0t'lo.thing*ss'").
'lo.thing#ssStr'('show%1'(XV1), XLbl1, XThis1) :- !,
    'lo.thing#ssStr^1'('show%1'(XV1), XLbl1, XThis1).
'lo.thing#thing'('show%1'(XV2), XLbl2, XThis2) :- !,
    'lo.thing#thing@show'(XV2, XLbl2, XThis2).
'lo.thing#thing@show'('lo.thing#ssStr'("thing"), XLbV2, XThV2) :- !.
'lo.thing#thing@show'(_, _, _) :- abort.
'lo.thing#ssSp'('show%1'(XV3), XLbl3, XThis3) :- !,
    'lo.thing#ssSp^2'('show%1'(XV3), XLbl3, XThis3).
'lo.thing#ssNl'('show%1'(XV4), XLbl4, XThis4) :- !,
    'lo.thing#ssNl^3'('show%1'(XV4), XLbl4, XThis4).
'lo.thing#ssSeq'('show%1'(XV5), XLbl5, XThis5) :- !,
    'lo.thing#ssSeq^4'('show%1'(XV5), XLbl5, XThis5).
'lo.thing#ssIndent'('show%1'(XV6), XLbl6, XThis6) :- !,
    'lo.thing#ssIndent^5'('show%1'(XV6), XLbl6, XThis6).
'lo.thing#ssStr^1'(XCV1, 'lo.thing#ssStr'(X_), XThV1) :- ocall(XCV1,'lo.thing#thing',XThV1).
'lo.thing#ssSp^2'(XCV2, 'lo.thing#ssSp', XThV3) :- ocall(XCV2,'lo.thing#thing',XThV3).
'lo.thing#ssNl^3'(XCV3, 'lo.thing#ssNl', XThV4) :- ocall(XCV3,'lo.thing#thing',XThV4).
'lo.thing#ssSeq^4'(XCV4, 'lo.thing#ssSeq'(X_), XThV5) :- ocall(XCV4,'lo.thing#thing',XThV5).
'lo.thing#ssIndent^5'(XCV5, 'lo.thing#ssIndent'(X_), XThV6) :- ocall(XCV5,'lo.thing#thing',XThV6).
