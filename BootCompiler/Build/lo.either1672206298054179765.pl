'#pkg'("n7o7'()7'n2o2'pkg's'lo.either'e'*'n1o1'()1'n2o2'import'e'private'n2o2'pkg's'lo.core'e'*'s\"I2'either':k'a':k'b'CT1k'a'Uz2'lo.either*either'2k'a'k'b''alternate':k'a':k'b'CT1k'b'Uz2'lo.either*either'2k'a'k'b'\"s\"I1'either':k'a':k'b'YUz2'lo.either*either'2k'a'k'b'I0\"n0o0'()0'n0o0'()0'n2o2'()2'n2o2'()2's'lo.core$display$lo.either*either's\":k'x':k'y'||c'lo.core$display'T1Uz2'lo.either*either'2k'x'k'y'T0c'lo.core$display'T1k'y'T0c'lo.core$display'T1k'x'T0\"n2o2'()2's'lo.core$equality$lo.either*either's\":k'x':k'y'||c'lo.core$equality'T1Uz2'lo.either*either'2k'x'k'y'T0c'lo.core$equality'T1k'y'T0c'lo.core$equality'T1k'x'T0\"").
'lo.either@init'() :- !.
'lo.either#either'('either%1'('lo.either@either'())) :- !.
'lo.either#alternate'('alternate%1'('lo.either@alternate'())) :- !.
'lo.either@dispEither'(Xlo_core_display_x3, Xlo_core_display_y3, 'lo.either#either'(Xa), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("either("), 'lo.core#,..'(XX6533, 'lo.core#,..'('lo.core#ss'(")"), 'lo.core#[]'))))) :- !,
    ocall('disp%2'(Xa, XX6533),Xlo_core_display_x3,Xlo_core_display_x3).
'lo.either@dispEither'(Xlo_core_display_x3, Xlo_core_display_y3, 'lo.either#alternate'(Xb), 'lo.core#ssSeq'('lo.core#,..'('lo.core#ss'("or("), 'lo.core#,..'(XX6547, 'lo.core#,..'('lo.core#ss'(")"), 'lo.core#[]'))))) :- !,
    ocall('disp%2'(Xb, XX6547),Xlo_core_display_y3,Xlo_core_display_y3).
'lo.either@dispEither'(_, _) :- raise_exception('error'("dispEither", 13, 3, 63)).
'lo.core$display$lo.either*either'('lo.core$display$lo.either*either%1'('lo.core$display$lo.either*either')) :- !.
'lo.core$display$lo.either*either'('disp%2'(XV1197, XV1198), XLbl191, XThis191) :- !,
    'lo.core$display$lo.either*either@disp'(XV1197, XV1198, XLbl191, XThis191).
'lo.core$display$lo.either*either'('disp%1'('lo.core$display$lo.either*either^disp'(XLbl192, XThis192)), XLbl192, XThis192).
'lo.core$display$lo.either*either@disp'(XT, XX6562, XLbV207, XThV207) :- XLbV207 = 'lo.core$display$lo.either*either'(Xlo_core_display_x4, Xlo_core_display_y4),
    !,
    'lo.either@dispEither'(Xlo_core_display_x4, Xlo_core_display_y4, XT, XX6562).
'lo.core$display$lo.either*either@disp'(_, _, _, _) :- raise_exception('error'("disp", 9, 5, 24)).
'lo.either@eitherEquals'(Xlo_core_equality_x4, Xlo_core_equality_y4, 'lo.either#either'(XA), 'lo.either#either'(XB)) :- ocall('==%2'(XA, XB),Xlo_core_equality_x4,Xlo_core_equality_x4).
'lo.either@eitherEquals'(Xlo_core_equality_x4, Xlo_core_equality_y4, 'lo.either#alternate'(XA), 'lo.either#alternate'(XB)) :- ocall('==%2'(XA, XB),Xlo_core_equality_y4,Xlo_core_equality_y4).
'lo.either@eitherHash'(Xlo_core_equality_x5, Xlo_core_equality_y5, 'lo.either#either'(XA), XX6588) :- !,
    ocall('hash%2'(XA, XX6586),Xlo_core_equality_x5,Xlo_core_equality_x5),
    ocall('*%3'(XX6586, 37, XX6588),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer').
'lo.either@eitherHash'(Xlo_core_equality_x5, Xlo_core_equality_y5, 'lo.either#alternate'(XB), XX6597) :- !,
    ocall('hash%2'(XB, XX6595),Xlo_core_equality_y5,Xlo_core_equality_y5),
    ocall('*%3'(XX6595, 41, XX6597),'lo.core$arith$lo.core*integer','lo.core$arith$lo.core*integer').
'lo.either@eitherHash'(_, _) :- raise_exception('error'("eitherHash", 26, 3, 35)).
'lo.core$equality$lo.either*either'('lo.core$equality$lo.either*either%1'('lo.core$equality$lo.either*either')) :- !.
'lo.core$equality$lo.either*either'('==%2'(XV1207, XV1208), XLbl193, XThis193) :- !,
    'lo.core$equality$lo.either*either@=='(XV1207, XV1208, XLbl193, XThis193).
'lo.core$equality$lo.either*either'('==%1'('lo.core$equality$lo.either*either^=='(XLbl194, XThis194)), XLbl194, XThis194).
'lo.core$equality$lo.either*either'('hash%2'(XV1213, XV1214), XLbl195, XThis195) :- !,
    'lo.core$equality$lo.either*either@hash'(XV1213, XV1214, XLbl195, XThis195).
'lo.core$equality$lo.either*either'('hash%1'('lo.core$equality$lo.either*either^hash'(XLbl196, XThis196)), XLbl196, XThis196).
'lo.core$equality$lo.either*either@=='(XX, XY, XLbV208, XThV208) :- XLbV208 = 'lo.core$equality$lo.either*either'(Xlo_core_equality_x6, Xlo_core_equality_y6),
    'lo.either@eitherEquals'(Xlo_core_equality_x6, Xlo_core_equality_y6, XX, XY).
'lo.core$equality$lo.either*either@hash'(XX, XX6612, XLbV208, XThV208) :- XLbV208 = 'lo.core$equality$lo.either*either'(Xlo_core_equality_x6, Xlo_core_equality_y6),
    !,
    'lo.either@eitherHash'(Xlo_core_equality_x6, Xlo_core_equality_y6, XX, XX6612).
'lo.core$equality$lo.either*either@hash'(_, _, _, _) :- raise_exception('error'("hash", 18, 5, 24)).
'lo.either^dispEither'('_call%2'(XV1193, XV1194), 'lo.either^dispEither', _) :- 'lo.either@dispEither'(XV1193, XV1194).
'lo.core$display$lo.either*either^disp'('_call%2'(XV1195, XV1196), 'lo.core$display$lo.either*either^disp'(XLbV207, XThV207), _) :- 'lo.core$display$lo.either*either@disp'(XV1195, XV1196, XLbV207, XThV207).
'lo.core$display$lo.either*either^disp'('_call%2'(XV1199, XV1200), 'lo.core$display$lo.either*either^disp'(XLbV207, XThV207), _) :- 'lo.core$display$lo.either*either@disp'(XV1199, XV1200, XLbV207, XThV207).
'lo.either^eitherEquals'('_call%2'(XV1201, XV1202), 'lo.either^eitherEquals', _) :- 'lo.either@eitherEquals'(XV1201, XV1202).
'lo.either^eitherHash'('_call%2'(XV1203, XV1204), 'lo.either^eitherHash', _) :- 'lo.either@eitherHash'(XV1203, XV1204).
'lo.core$equality$lo.either*either^=='('_call%2'(XV1205, XV1206), 'lo.core$equality$lo.either*either^=='(XLbV208, XThV208), _) :- 'lo.core$equality$lo.either*either@=='(XV1205, XV1206, XLbV208, XThV208).
'lo.core$equality$lo.either*either^=='('_call%2'(XV1209, XV1210), 'lo.core$equality$lo.either*either^=='(XLbV208, XThV208), _) :- 'lo.core$equality$lo.either*either@=='(XV1209, XV1210, XLbV208, XThV208).
'lo.core$equality$lo.either*either^hash'('_call%2'(XV1211, XV1212), 'lo.core$equality$lo.either*either^hash'(XLbV208, XThV208), _) :- 'lo.core$equality$lo.either*either@hash'(XV1211, XV1212, XLbV208, XThV208).
'lo.core$equality$lo.either*either^hash'('_call%2'(XV1215, XV1216), 'lo.core$equality$lo.either*either^hash'(XLbV208, XThV208), _) :- 'lo.core$equality$lo.either*either@hash'(XV1215, XV1216, XLbV208, XThV208).
