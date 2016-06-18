:- use_module(ocall).
:-['/Users/fgm/Projects/LandO/LO/Build/stream.pl'].
'lo.list#import'("file:/Users/fgm/Projects/LandO/LO/Build/stream.pl").
'lo.list#export'("I3',..':k't'C2k't'U'lo.list*list'1k't'U'lo.list*list'1k't''[]'U'lo.list*list'1_'<>':k't'F2U'lo.list*list'1k't'U'lo.list*list'1k't'U'lo.list*list'1k't'").
'lo.list#types'("I1'list'T2:k't'YU'lo.list*list'1k't'U'lo.stream*stream'1k't':k't'YU'lo.list*list'1k't'I3'eof'P0'hdtl'P2k't'h'cons'F1k't'h").
'lo.list@,..'(',..%1'('lo.list#,..'())) :- !.
'lo.list#,..'('eof%0'(), XLbl19, XThis19) :- !,
    'lo.list#,..@eof'(XLbl19, XThis19).
'lo.list#,..'('hdtl%2'(XV25, XV26), XLbl20, XThis20) :- !,
    'lo.list#,..@hdtl'(XV25, XV26, XLbl20, XThis20).
'lo.list#,..'('cons%2'(XV27, XV28), XLbl21, XThis21) :- !,
    'lo.list#,..@cons'(XV27, XV28, XLbl21, XThis21).
'lo.list#,..@eof'(XLbV7, XThV7) :- XLbV7 = 'lo.list#,..'(XH, XT),
    fail.
'lo.list#,..@hdtl'(XH, XT, XLbV7, XThV7) :- XLbV7 = 'lo.list#,..'(XH, XT).
'lo.list#,..@cons'(XX, 'lo.list#,..'(XX, XThV7), XLbV7, XThV7) :- XLbV7 = 'lo.list#,..'(XH, XT),
    !.
'lo.list#,..@cons'(_, _, _, _) :- abort.
'lo.list#[]'('eof%0'(), XLbl22, XThis22) :- !,
    'lo.list#[]@eof'(XLbl22, XThis22).
'lo.list#[]'('cons%2'(XV29, XV30), XLbl23, XThis23) :- !,
    'lo.list#[]@cons'(XV29, XV30, XLbl23, XThis23).
'lo.list#[]'('hdtl%2'(XV31, XV32), XLbl24, XThis24) :- !,
    'lo.list#[]@hdtl'(XV31, XV32, XLbl24, XThis24).
'lo.list#[]@eof'(XLbV8, XThV8).
'lo.list#[]@cons'(XX, 'lo.list#,..'(XX, 'lo.list#[]'), XLbV8, XThV8) :- !.
'lo.list#[]@cons'(_, _, _, _) :- abort.
'lo.list#[]@hdtl'(X_, X_, XLbV8, XThV8) :- fail.
'lo.list@<>'('lo.list#[]', XX, XX) :- !.
'lo.list@<>'('lo.list#,..'(XE, XX), XY, 'lo.list#,..'(XE, XX95)) :- !,
    'lo.list@<>'(XX, XY, XX95).
'lo.list@<>'(_, _, _) :- abort.
