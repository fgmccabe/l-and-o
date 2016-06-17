:- use_module(ocall).
'lo.stream#export'("I1'seq':k'E'C2k'E'U'lo.stream*stream'1k'E'U'lo.stream*stream'1k'E'").
'lo.stream#types'("I1'stream'T1:k'E'YU'lo.stream*stream'1k'E'I3'cons'F1k'E'h'hdtl'P2k'E'h'eof'P0").
'lo.stream#seq'('eof%0'(), XLbl1, XThis1) :- !,
    'lo.stream#seq@eof'(XLbl1, XThis1).
'lo.stream#seq'('hdtl%2'(XV1, XV2), XLbl2, XThis2) :- !,
    'lo.stream#seq@hdtl'(XV1, XV2, XLbl2, XThis2).
'lo.stream#seq'('cons%2'(XV3, XV4), XLbl3, XThis3) :- !,
    'lo.stream#seq@cons'(XV3, XV4, XLbl3, XThis3).
'lo.stream#seq@eof'(XLbV1, XThV1) :- XLbV1 = 'lo.stream#seq'(XH, XT),
    fail.
'lo.stream#seq@hdtl'(XH, XT, XLbV1, XThV1) :- XLbV1 = 'lo.stream#seq'(XH, XT).
'lo.stream#seq@cons'(XE, 'lo.stream#seq'(XE, XThV1), XLbV1, XThV1) :- XLbV1 = 'lo.stream#seq'(XH, XT),
    !.
'lo.stream#seq@cons'(_, _, _, _) :- abort.
