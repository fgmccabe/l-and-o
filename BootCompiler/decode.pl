:- module(decode,[decodeTerm//1, decodeValue/2, decodeType//1, decodeSignature/2]).

:- use_module(misc).
:- use_module(types).
:- use_module(base64).

% Decode a sequence of characters into various entites

/*
 Decided by a leading 'flag' byte which encodes a type indicator:
 'a': Unbound variable
 'x': Integer
 'd': Floating point
 'e': Identifier
 's': String
 'o': Constructor specifier
 'p': Program specifier
 'u': Tuple
 'n': Term
 '#': Code
*/

decodeValue(Txt,Val) :-
  string_chars(Txt,Chrs),
  phrase(decodeTerm(Val),Chrs).

decodeTerm(anon) --> ['a'].
decodeTerm(intgr(Ix)) --> ['x'], decInt(Ix).
decodeTerm(float(Dx)) --> ['d'], decFloat(Dx).
decodeTerm(enum(Nm)) --> ['e'], decodeName(Nm).
decodeTerm(strg(Txt)) --> ['s'], decodeText(Chrs), { string_chars(Txt,Chrs)}.
decodeTerm(strct(Nm,Ar)) --> ['o'], decInt(Ar), decodeName(Nm).
decodeTerm(prg(Nm,Ar)) --> ['p'], decInt(Ar), decodeName(Nm).
decodeTerm(tpl(Els)) --> ['u'], decInt(Len), decTerms(Len,Els).
decodeTerm(cons(Con,Els)) --> ['n'], decInt(Len), decodeTerm(Con), decTerms(Len,Els).
decodeTerm(code(Tp,Bytes,Lits)) --> ['#'],
    decodeType(Tp), 
    decodeTerm(Lits),
    decodeText(B64),
    { decode64(B64,Bytes,[])}.

decInt(Ix) --> ['-'], digits(0,N), Ix is -N.
decInt(Ix) --> digits(0,Ix).

decodeText(Chrs) --> [C], collectQuoted(C,Chrs).

collectQuoted(C,[]) --> [C].
collectQuoted(C,[Ch|M]) --> ['\\', Ch], collectQuoted(C,M).
collectQuoted(C,[Ch|M]) --> [Ch], collectQuoted(C,M).

decodeSignature(S,Tp) :-
  string_chars(S,Chrs),
  phrase(decodeType(Tp),Chrs).

decodeType(anonType) --> ['_'].
decodeType(voidType) --> ['v'].
decodeType(topType) --> ['A'].
decodeType(thisType) --> ['h'].
decodeType(type("lo.arith*integer")) --> ['i'].
decodeType(type("lo.arith*float")) --> ['f'].
decodeType(type("lo.thing*string")) --> ['S'].
decodeType(type("lo.logical*logical")) --> ['l'].
decodeType(kVar(Nm)) --> ['k'], decodeName(Nm).
decodeType(type(Nm)) --> ['t'], decodeName(Nm).
decodeType(typeExp("lo.list*list",[ElTp])) --> ['L'], decodeType(ElTp).
decodeType(typeExp(Nm,ArgTypes)) --> ['U'], decodeName(Nm), decodeTypes(ArgTypes).
decodeType(univType(TV,Tp)) --> [':'], decodeType(TV), decodeType(Tp).
decodeType(constrained(Lower,Tp,Upper)) --> ['c'], decodeType(Lower), decodeType(Tp), decodeType(Upper).
decodeType(faceType(Fields)) --> ['I'], decodeFields(Fields).
decodeType(funType(A,T)) --> ['F'], decodeArgTypes(A), decodeType(T).
decodeType(grammarType(A,T)) --> ['G'], decodeArgTypes(A), decodeType(T).
decodeType(predType(A)) --> ['P'], decodeArgTypes(A).
decodeType(classType(A,T)) --> ['C'], decodeTypes(A), decodeType(T).
decodeType(tupleType(Tps)) --> ['T'], decodeTypes(Tps).
decodeType(typeRule(L,R)) --> ['Y'], decodeType(L), decodeType(R).

decodeArgType(in(Tp)) --> ['+'], decodeType(Tp).
decodeArgType(out(Tp)) --> ['-'], decodeType(Tp).
decodeArgType(inout(Tp)) --> ['?'], decodeType(Tp).

decodeArgTypes(0,[]) --> [].
decodeArgTypes(Ln,[A|More]) --> { Ln > 0 }, decodeArgType(A), {L1 is Ln-1}, decodeArgTypes(L1,More).
decodeArgTypes(Types) --> typeLen(Len), decodeArgTypes(Len,Types).

typeLen(Len) --> digits(0,Len).

decodeTypes(0,[]) --> [].
decodeTypes(Ln,[A|More]) --> { Ln > 0 }, decodeType(A), {L1 is Ln-1}, decodeTypes(L1,More).
decodeTypes(Types) --> typeLen(Len), decodeTypes(Len,Types).

decodeFields(0,[]) --> [].
decodeFields(Ln,[(Nm,Tp)|More]) --> { Ln > 0 }, decodeName(Nm), decodeType(Tp), {L1 is Ln-1}, decodeFields(L1,More).
decodeFields(Fields) --> typeLen(Len), decodeFields(Len,Fields).

decodeName(Str) --> [C], collectUntil(C,Text), { string_chars(Str,Text)}.

collectUntil(C,[]) --> [C].
collectUntil(C,[B|More]) --> [B], collectUntil(C,More).

digits(SoFar,Ix) --> digit(D), { Nx is SoFar*10+D}, digits(Nx,Ix).
digits(Ix,Ix) --> \+ digit(_).

digit(0) --> ['0'].
digit(1) --> ['1'].
digit(2) --> ['2'].
digit(3) --> ['3'].
digit(4) --> ['4'].
digit(5) --> ['5'].
digit(6) --> ['6'].
digit(7) --> ['7'].
digit(8) --> ['8'].
digit(9) --> ['9'].

