:- module(tree,['tree@assert'/0]).

'tree@assert'() :- 'tree@u'(XX111),
    ocall('leaves%1'(XX110),XX111,XX111),
    'tree@t'(XX113),
    ocall('leaves%1'(XX112),XX113,XX113),
    XX110 = XX112.
'tree#em'('leaves%1'(XV16), XLbl16, XThis16) :- !,
    'tree#em@leaves'(XV16, XLbl16, XThis16).
'tree#em@leaves'([], XLbV15, XThV15) :- XLbV15 = 'tree#em',
    !.
'tree#em@leaves'(_, _, _) :- abort.
'tree@<>'([], XX, XX) :- !.
'tree@<>'([XE | XX], XY, [XE | XX65]) :- !,
    'tree@<>'(XX, XY, XX65).
'tree@<>'(_, _, _) :- abort.
'tree#nd'('label%1'(XV17), XLbl17, XThis17) :- !,
    'tree#nd@label'(XV17, XLbl17, XThis17).
'tree#nd'('leaves%1'(XV18), XLbl18, XThis18) :- !,
    'tree#nd@leaves'(XV18, XLbl18, XThis18).
'tree#nd@label'(XB, XLbV16, XThV16) :- XLbV16 = 'tree#nd'(XL, XB, XR),
    !.
'tree#nd@label'(_, _, _) :- abort.
'tree#nd@leaves'(XX80, XLbV16, XThV16) :- XLbV16 = 'tree#nd'(XL, XB, XR),
    !,
    ocall('leaves%1'(XX72),XL,XL),
    ocall('leaves%1'(XX77),XR,XR),
    'tree@<>'([XB], XX77, XX79),
    'tree@<>'(XX72, XX79, XX80).
'tree#nd@leaves'(_, _, _) :- abort.
'tree@walk'('tree#nd'(XL, XB, XR), XX93) :- !,
    'tree@walk'(XL, XX86),
    'tree@walk'(XR, XX91),
    'tree@<>'([XB], XX91, XX92),
    'tree@<>'(XX86, XX92, XX93).
'tree@walk'('tree#em', []) :- !.
'tree@walk'(_, _) :- abort.
'tree@t'('tree#nd'('tree#nd'('tree#em', "a", 'tree#em'), "b", 'tree#nd'('tree#em', "c", 'tree#em'))) :- !.
'tree@u'('tree#nd'('tree#em', "a", 'tree#nd'('tree#em', "b", 'tree#nd'('tree#em', "c", 'tree#em')))) :- !.

ocall(Call,Lbl,ThVr) :- functor(Lbl,P,_), call(P,Call,Lbl,ThVr).
