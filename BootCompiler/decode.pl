:- module(decode,[decodeTerm/2,decodeTerm/3, decodeType//1,decodeSignature/2]).

:- use_module(misc).
:- use_module(types).

% Decode a sequence of bytes into various entites

/*
 Decided by a leading 'flag' byte, the upper nibble of which encodes a type indicator:
 0x00: Unbound variable
 0x10: Integer
 0x20: Floating point
 0x30: Identifier
 0x40: String
 0x50: Constructor specifier
 0x60: Program specifier
 0x70: Tuple
 0x80: Term
 0x90: Code
 0xa0: Label Definition
 0xb0: Label Reference
 0xc0: Reserved 
 0xd0: Reserved 
 0xe0: Reserved 
 0xf0: Reserved 

 Where appropriate, a numeric argument is determined by the lower nibble, and subsequent byte:

 0x0: The numeric argument is zero
 0x1-0xe: The nibble is the length. The actual numeric argument is in the following xx bytes, most significant first
 0xf: The length of the numeric argument is encoded as an integer, followed by the appropriate number of bytes encoding the argument
*/

% decode a flag's byte into a numeric argument. Might be double encoded.
decLen(Flg,Len,Sin,Sout) :-
 Ln is Flg /\ 15,
 decLn(Ln,Len,Sin,Sout).
decLn(0,0,Sin,Sin) :- !.          % special value of zero. 
decLn(15,Len,[X|Sin],Sout) :-  !, % special length 15 means more than 14 bytes
 decLen(X,Ln,Sin,Si0),
 decInt(Ln,Len,Si0,Sout).
decLn(Len,Len,Sin,Sin).

decInt(Flg,Reslt,Sin,Sout) :- % decode an integer, given a flag byte
 decLen(Flg,Len,Sin,Si0),
 signByte(Leading,Si0,Si1),
 decInt(Leading,Reslt,Len,Si1,Sout).

signByte(L,[B|Sin],Sin) :-
  (B>>7)/\1 =:= 1,!,
  L is -1 \/B.
signByte(B,[B|Sin],Sin).

decInt(X,X,0,Sin,Sin) :-!.
decInt(SoFar,Reslt,Len,[By|Sin],Sout) :-
  Nx is (SoFar<<8) \/ By,
  L1 is Len-1,
  decInt(Nx,Reslt,L1,Sin,Sout).

decodeText(Flg,Text,Sin,Sout) :-
  decLen(Flg,Len,Sin,Si0),
  grabBytes(Len,Bytes,Si0,Sout),
  string_codes(Text,Bytes).

grabBytes(0,[],Sin,Sin).
grabBytes(Ln,[B|More],[B|Sin],Sout) :-
  Ln > 0,
  Ln1 is Ln-1,
  grabBytes(Ln1,More,Sin,Sout).

decodeTerm(Src,Term) :-
  string_codes(Src,Codes),
  decodeTerm(Term,Codes,[]).

decodeTerm(Term,Sin,Sout) :-
  decodeTerm(Term,[],Sin,Sout).

decodeTerm(Term,Refs,[B|Sin],Sout) :-
  Flg is (B >> 4) /\ 15,
  decodeTerm(Flg,B,Term,Refs,Sin,Sout).

decodeTerm(0,_,_,anon,Sin,Sin).
decodeTerm(1,Flg,intgr(Ix),_,Sin,Sout) :-
  decInt(Flg,Ix,Sin,Sout).
decodeTerm(2,Flg,float(Dx),_,Sin,Sout) :-
  decFloat(Flg,Dx,Sin,Sout).
decodeTerm(3,Flg,enum(Nm),_,Sin,Sout) :-
  decodeText(Flg,Nm,Sin,Sout).
decodeTerm(4,Flg,string(Str),_,Sin,Sout) :-
  decodeText(Flg,Str,Sin,Sout).
decodeTerm(5,Flg,strct(Nm,Arity),Refs,Sin,Sout) :-
  decInt(Flg,Arity,Sin,Si0),
  decodeTerm(enum(Nm),Refs,Si0,Sout).
decodeTerm(6,Flg,prg(Nm,Arity),Refs,Sin,Sout) :-
  decInt(Flg,Arity,Sin,S0),
  decodeTerm(enum(Nm),Refs,S0,Sout).
decodeTerm(7,Flg,tpl(Args),Refs,Sin,Sout) :-
  decInt(Flg,Ar,Sin,S0),
  decodeTerms(Ar,Args,Refs,S0,Sout).
decodeTerm(8,Flg,cons(C,Args),Refs,Sin,Sout) :-
  decInt(Flg,Ar,Sin,S0),
  decodeTerm(C,Refs,S0,S1),
  decodeTerms(Ar,Args,Refs,S1,Sout).
decodeTerm(9,Flg,code(Tp,Bytes,Lits),Refs,Sin,Sout) :-
  decodeText(Flg,Signature,Sin,S0),
  decodeSignature(Signature,Tp),
  decodeTerm(tpl(Lits),Refs,S0,S1),
  decodeTerm(intgr(CodeSize),Refs,S1,S2),
  grabBytes(CodeSize,Bytes,S2,Sout).
decodeTerm(10,Flg,Term,Refs,Sin,Sout) :-
  decodeInt(Flg,Key,Sin,S0),
  decodeTerm(S0,Value,Refs,S0,S1),
  decodeTerm(Term,[(Key,Value)|Refs],S1,Sout).
decodeTerm(11,Flg,Term,Refs,Sin,Sout) :-
  decodeInt(Flg,Key,Sin,Sout),
  is_member((Key,Term),Refs),!.
decodeTerm(_,_,_,_,_) :- abort.

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
decodeType(type("lo.std*logical")) --> ['l'].
decodeType(kVar(Nm)) --> ['k'], decodeName(Nm).
decodeType(type(Nm)) --> ['t'], decodeName(Nm).
decodeType(typeExp("lo.std*list",[ElTp])) --> ['L'], decodeType(ElTp).
decodeType(typeExp(Nm,ArgTypes)) --> ['U'], decodeName(Nm), decodeTypes(ArgTypes).
decodeType(univType(TV,Tp)) --> [':'], decodeType(TV), decodeType(Tp).
decodeType(constrained(Lower,Tp,Upper)) --> ['c'], decodeType(Lower), decodeType(Tp), decodeType(Upper).
decodeType(faceType(Fields)) --> ['I'], decodeFields(Fields).
decodeType(funType(A,T)) --> ['F'], decodeArgTypes(A), decodeType(T).
decodeType(grammarType(A,T)) --> ['G'], decodeArgTypes(A), decodeType(T).
decodeType(predType(A)) --> ['P'], decodeArgTypes(A).
decodeType(classType(A,T)) --> ['C'], decodeArgTypes(A), decodeType(T).
decodeType(tupleType(Tps)) --> ['T'], decodeTypes(Tps).
decodeType(typeRule(L,R)) --> ['Y'], decodeType(L), decodeType(R).

decodeArgType(in(Tp)) --> ['+'], decodeType(Tp).
decodeArgType(out(Tp)) --> ['-'], decodeType(Tp).
decodeArgType(Tp) --> decodeType(Tp).

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

