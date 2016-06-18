:- use_module(ocall).
:-['/Users/fgm/Projects/LandO/LO/Build/stream.pl'].
:-['/Users/fgm/Projects/LandO/LO/Build/arith.pl'].
'grtest#export'("I5'isLetter'P1i'isDigit'P1i'alphaNum'G0U'lo.stream*stream'1i'alphaNums'G0U'lo.stream*stream'1i'ident'G0U'lo.stream*stream'1i").
'grtest#types'("I0").
'grtest@isLetter'(97).
'grtest@isLetter'(98).
'grtest@isLetter'(99).
'grtest@isLetter'(100).
'grtest@isDigit'(48).
'grtest@isDigit'(49).
'grtest@isDigit'(50).
'grtest@isDigit'(51).
'grtest@isDigit'(52).
'grtest@isDigit'(53).
'grtest@isDigit'(54).
'grtest@isDigit'(55).
'grtest@isDigit'(56).
'grtest@isDigit'(57).
'grtest@alphaNum'(XStIn50, XC) :- ocall('hdtl%2'(XC, XNStrm30),XStIn50,XStIn50),
    XNStrm30 = XC,
    'grtest@isLetter'(XC).
'grtest@alphaNum'(XStIn51, XC) :- ocall('hdtl%2'(XC, XNStrm31),XStIn51,XStIn51),
    XNStrm31 = XC,
    'grtest@isDigit'(XC).
'grtest@alphaNums'(XStIn52, XStx22) :- 'grtest@alphaNum'(XStIn52, XStx21),
    'grtest@alphaNums'(XStx21, XStx22).
'grtest@alphaNums'(XStIn53, XStIn53).
'grtest@ident'(XStIn54, XC) :- ocall('hdtl%2'(XC, XNStrm32),XStIn54,XStIn54),
    'grtest@isLetter'(XC),
    'grtest@alphaNums'(XNStrm32, XC).
