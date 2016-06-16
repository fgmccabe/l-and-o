:- module(encode,[encodeTerm/3,encodeType/3]).

:- use_module(types).
:- use_module(misc).

encodeTerm(anon,[0|O],O).
encodeTerm(intgr(Ix),O,Ox) :- encodeInteger(0x10,Ix,O,Ox).
encodeTerm(float(Dx),O,Ox) :- encodeFloat(Dx,O,Ox).
encodeTerm(enum(Nm),O,Ox) :- encodeText(0x30,Nm,O,Ox).
encodeTerm(strg(St),O,Ox) :- encodeText(0x40,St,O,Ox).
encodeTerm(strct(Nm,Arity),O,Ox) :- 
  encodeInteger(0x50,Arity,O,O1),
  encodeText(0x30,Nm,O1,Ox).
encodeTerm(prg(Nm,Arity),O,Ox) :- encodeInteger(0x60,Arity,O,O1), encodeText(0x30,Nm,O1,Ox).
encodeTerm(tpl(Els),O,Ox) :- length(Els,Ln), encodeInteger(0x70,Ln,O,O1), encodeTerms(Els,O1,Ox).
encodeTerm(cons(Con,Els),O,Ox) :- 
  length(Els,Ln), 
  encodeInteger(0x80,Ln,O,O1), 
  encodeTerm(Con,O1,O2),
  encodeTerms(Els,O2,Ox).
encodeTerm(code(Tp,Bytes,Lits),O,Ox) :-
  length(Bytes,Ln),
  encodeType(Tp,TpText,[]),
  encodeText(0x90,TpText,O,O1),
  encodeTerm(tpl(Lits),O1,O2),
  encodeInteger(0x10,Ln,O2,O3),
  concat(Bytes,Ox,O3).

encodeInteger(Flg,Ix,O,Ox) :- 
  encodeInt(Ix,Bytes),
  length(Bytes,Ln),
  encodeLength(Flg,Ln,O,O1),
  concat(Bytes,Ox,O1).

encodeLength(Flg,0,[Flg|O],O).
encodeLength(Flg,Ln,[F1|O],O) :- Ln>0, Ln<0xf, F1 is Flg \/ (Ln /\ 0xf).
encodeLength(Flg,Ln,[F1|O],Ox) :- Ln >= 0xf, encodeInteger(0x10,Ln,O,Ox), F1 is Flg\/0xf.
  
encodeText(Flg,Str,O,Ox) :- 
  string_codes(Str,Codes),
  length(Codes,Ln),
  encodeLength(Flg,Ln,O,O1),
  concat(Codes,Ox,O1).

encodeType(anonType,['_'|O],O).
encodeType(voidType,['v'|O],O).
encodeType(topType,['A'|O],O).
encodeType(thisType,['h'|O],O).
encodeType(typeExp("lo.std*list",[T]),['L'|O],Ox) :- encodeType(T,O,Ox).
encodeType(type("lo.arith*integer"),['i'|O],O).
encodeType(type("lo.arith*float"),['f'|O],O).
encodeType(type("lo.thing*string"),['S'|O],O).
encodeType(kVar(Nm),['k'|O],Ox) :- encodeTpName(Nm,O,Ox).
encodeType(type(Nm),['t'|O],Ox) :- encodeTpName(Nm,O,Ox).
encodeType(typeExp(Nm,Args),['U'|O],Ox) :- encodeTpName(Nm,O,O1), encodeTypes(Args,O1,Ox).
encodeType(funType(Args,Tp),['F'|O],Ox) :- encodeArgTypes(Args,O,O1), encodeType(Tp,O1,Ox).
encodeType(predType(Args),['P'|O],Ox) :- encodeArgTypes(Args,O,Ox).
encodeType(classType(Args,Tp),['C'|O],Ox) :- encodeArgTypes(Args,O,O1), encodeType(Tp,O1,Ox).
encodeType(tupleType(Args),['T'|O],Ox) :- encodeTypes(Args,O,Ox).
encodeType(faceType(Fields),['I'|O],Ox) :- encodeFieldTypes(Fields,O,Ox).
encodeType(univType(B,Tp),[':'|O],Ox) :- encodeType(B,O,O1),encodeType(Tp,O1,Ox).
encodeType(constrained(Lw,Tp,Up),['c'|O],Ox) :- encodeType(Lw,O,O1), encodeType(Tp,O1,O2),encodeType(Up,O2,Ox).
encodeType(typeRule(L,R),['Y'|O],Ox) :- encodeType(L,O,O1), encodeType(R,O1,Ox).
encodeTpName(Nm,O,Ox) :- string_chars(Nm,Chrs), findDelim(Chrs,Delim), encodeChars(Chrs,Delim,O,Ox).

findDelim(Chrs,Delim) :-
  is_member(Delim,['''','"', '|', '/', '%']),
  \+ is_member(Delim,Chrs),!.

encodeChars(Chars,Delim,[Delim|O],Ox) :-
 concat(Chars,[Delim|Ox],O).

encodeInt(K,[D|O],O) :- K>=0 , K=<10, digit(K,D).
encodeInt(N,[D|O],Ox) :- N1 is N div 10, K is N mod 10, digit(K,D), encodeInt(N1,O,Ox).

digit(0,'0').
digit(1,'1').
digit(2,'2').
digit(3,'3').
digit(4,'4').
digit(5,'5').
digit(6,'6').
digit(7,'7').
digit(8,'8').
digit(9,'9').

encodeTypes(Tps,O,Ox) :- length(Tps,L), encodeInt(L,O,O1),encodeTps(Tps,O1,Ox).

encodeTps([],O,O).
encodeTps([Tp|More],O,Ox) :- encodeType(Tp,O,O1), encodeTps(More,O1,Ox).

encodeArgTypes(Args,O,Ox) :- length(Args,L), encodeInt(L,O,O1),encodeArgTps(Args,O1,Ox).

encodeArgTps([],O,O).
encodeArgTps([Tp|More],O,Ox) :- encodeArgType(Tp,O,O1), encodeArgTps(More,O1,Ox).

encodeArgType(in(Tp),['+'|O],Ox) :- encodeType(Tp,O,Ox).
encodeArgType(out(Tp),['-'|O],Ox) :- encodeType(Tp,O,Ox).
encodeArgType(inout(Tp),O,Ox) :- encodeType(Tp,O,Ox).
encodeArgType(Tp,O,Ox) :- encodeType(Tp,O,Ox).

encodeFieldTypes(Fields,O,Ox) :- length(Fields,L), encodeInt(L,O,O1),encodeFieldTps(Fields,O1,Ox).

encodeFieldTps([],O,O).
encodeFieldTps([(Nm,Tp)|More],O,Ox) :- encodeTpName(Nm,O,O1),encodeType(Tp,O1,O2), encodeFieldTps(More,O2,Ox).

