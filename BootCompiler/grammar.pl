:- module(bootgrammar,[parse/3]).

:- use_module(operators).
:- use_module(abstract).
:- use_module(location).
:- use_module(errors).
:- use_module(lexer).

parse(Tks,T,RTks) :-
    term(Tks,2000,T,Tks1,Lst),!,
    checkTerminator(Lst,Tks1,RTks).

term(Tks,Pr,T,Toks,Lst) :-
    termLeft(Tks,Pr,Left,LftPr,Tks1,LLst),!,
    termRight(Tks1,Pr,LftPr,Left,T,Toks,LLst,Lst).

termLeft([idTok(Id,Lc),rpar(Lcy)|Toks],_,name(Lc,Id),0,[rpar(Lcy)|Toks],id).
termLeft([idTok(Id,Lcx)|Tks],Pr,Left,LftPr,Toks,Lst) :-
    prefixOp(Id,LftPr,OpRightPr),
    \+ lookAhead(rpar(_),Tks),
    LftPr =< Pr, !,
    term(Tks,OpRightPr,Arg,Toks,Lst),
    locOfAst(Arg,Lcy),
    mergeLoc(Lcx,Lcy,Lc),
    unary(Lc,Id,Arg,Left).
termLeft(Tks,_,T,0,Toks,Lst) :- term0(Tks,T,Toks,Lst).

termRight([idTok(Id,_)|Tks],Pr,LPr,Left,T,Toks,_,Lst) :- 
  infixOp(Id,InfOpL,InfOpPr,InfOpR), InfOpPr =< Pr, LPr =< InfOpL,
  postfixOp(Id,PostOpL,PostOpPr), PostOpPr =< Pr, LPr =< PostOpL,
  legalNextRight(Tks,InfOpR), !,
  term(Tks,InfOpR, Right, Tks1,LLst),
  locOfAst(Left,Lcx),
  locOfAst(Right,Lcy),
  mergeLoc(Lcx,Lcy,Lc),
  binary(Lc,Id,Left,Right,NewLeft),
  termRight(Tks1,Pr,InfOpPr,NewLeft,T,Toks,LLst,Lst).
termRight([idTok(Id,Lcy)|Tks],Pr,LPr,Left,T,Toks,_,Lst) :- 
  postfixOp(Id,PostOpL,PostOpPr), PostOpPr =< Pr, LPr =< PostOpL,
  locOfAst(Left,Lcx),
  mergeLoc(Lcx,Lcy,Lc),
  unary(Lc,Id,Left,NewLeft),
  termRight(Tks,Pr,PostOpPr,NewLeft,T,Toks,id,Lst).
termRight([idTok(Id,_)|Tks],Pr,LPr,Left,T,Toks,_,Lst) :- 
  infixOp(Id,InfOpL,InfOpPr,InfOpR), InfOpPr =< Pr, LPr =< InfOpL,
  term(Tks,InfOpR, Right, Tks1,LLst),
  locOfAst(Left,Lcx),
  locOfAst(Right,Lcy),
  mergeLoc(Lcx,Lcy,Lc),
  binary(Lc,Id,Left,Right,NewLeft),
  termRight(Tks1,Pr,InfOpPr,NewLeft,T,Toks,LLst,Lst).
termRight(Toks,_,_,Left,Left,Toks,Lst,Lst).

legalNextRight([idTok(I,_)|_],Pr) :- ( prefixOp(I,PPr,_), PPr=<Pr ; \+ isOperator(I,_)) , !.
legalNextRight([lpar(_)|_],_).
legalNextRight([lbra(_)|_],_).
legalNextRight([lbrce(_)|_],_).
legalNextRight([lqpar(_)|_],_).
legalNextRight([stringTok(_,_)|_],_).
legalNextRight([integerTok(_,_)|_],_).
legalNextRight([floatTok(_,_)|_],_).

term00([idTok(I,Lc)|Toks],T,Toks,id) :- 
      (isOperator(I,_), \+lookAhead(rpar(_),Toks), !, reportError("unexpected operator: '%s'",[I],Lc),T=void(Lc);
      T = name(Lc,I)).
term00([lpar(Lc0),rpar(Lc2)|Toks],tuple(Lc,"()",[]),Toks,rpar) :- 
  mergeLoc(Lc0,Lc2,Lc).
term00([lpar(Lcx)|Tks],T,Toks,rpar) :- 
  term(Tks,2000,Seq,Tks2,_), 
  checkToken(Tks2,Toks,rpar(Lcy),"missing close parenthesis, got %w"), 
  mergeLoc(Lcx,Lcy,Lc),
  tupleize(Seq,Lc,"()",T).
term00([lbra(Lc0),rbra(Lc2)|Toks],tuple(Lc,"[]",[]),Toks,rbra) :-
  mergeLoc(Lc0,Lc2,Lc).
term00([lbra(Lcx)|Tks],T,Toks,rbra) :-
  term(Tks,2000,Seq,Tks2,_), 
  checkToken(Tks2,Toks,rbra(Lcy),"mising close bracket, got %w"), 
  mergeLoc(Lcx,Lcy,Lc), 
  tupleize(Seq,Lc,"[]",T).
term00([lbrce(Lc0),rbrce(Lc2)|Toks],tuple(Lc,"{}",[]),Toks,rbrce) :- 
  mergeLoc(Lc0,Lc2,Lc).
term00([lbrce(Lcx)|Tks],tuple(Lc,"{}",Seq),Toks,rbrce) :- 
  terms(Tks,Tks2,Seq), 
  checkToken(Tks2,Toks,rbrce(Lcy),"missing close brace, got %w"),
  mergeLoc(Lcx,Lcy,Lc).
term00([lqpar(Lcx)|Tks],unary(Lc,"<||>",T),Toks,rpar) :- 
  term(Tks,2000,T,Tks2,_), 
  checkToken(Tks2,Toks,rpar(Lcy),"missing close parenthesis, got %w"),
  mergeLoc(Lcx,Lcy,Lc).

term0([stringTok(St,Lc)|Toks],Str,Toks,id) :- 
  handleInterpolation(St,Lc,Str).
term0([integerTok(In,Lc)|Toks],integer(Lc,In),Toks,id).
term0([floatTok(Fl,Lc)|Toks],float(Lc,Fl),Toks,id).
term0(Tks,T,Toks,Lst) :- term00(Tks,Op,RTks,LLst), termArgs(RTks,Op,T,Toks,LLst,Lst).

termArgs([],T,T,[],Lst,Lst).
termArgs([lpar(Lcx),rpar(Lcy)|Tks],Op,T,Toks,_,Lst) :- 
    mergeLoc(Lcx,Lcy,Lc),
    apply(Lc,Op,tuple(Lc,"()",[]),NOP),
    termArgs(Tks,NOP,T,Toks,rpar,Lst).
termArgs([lpar(Lcx)|Tks],Op,T,Toks,_,Lst) :- 
    term(Tks,2000,Seq,Tks2,LLst),
    checkToken(Tks2,Tks3,rpar(Lcy),"missing close parenthesis"),
    mergeLoc(Lcx,Lcy,Lc),
    tupleize(Seq,Lc,"()",Args),
    apply(Lc,Op,Args,NOP),
    termArgs(Tks3,NOP,T,Toks,LLst,Lst).
termArgs([lbra(Lcx),rbra(Lcy)|Tks],Op,T,Toks,_,Lst) :- 
    mergeLoc(Lcx,Lcy,Lc),
    apply(Lc,Op,tuple(Lc,"[]",[]),NOP),
    termArgs(Tks,NOP,T,Toks,rbra,Lst).
termArgs([lbra(Lcx)|Tks],Op,T,Toks,_,Lst) :- 
    term(Tks,2000,Seq,Tks2,_),
    checkToken(Tks2,Tks3,rbra(Lcy),"missing close bracket, got %w"),
    mergeLoc(Lcx,Lcy,Lc),
    tupleize(Seq,Lc,"[]",Args),
    apply(Lc,Op,Args,NOP),
    termArgs(Tks3,NOP,T,Toks,rbra,Lst).
termArgs([lbrce(Lcx),rbrce(Lcy)|Tks],Op,T,Tks,_,rbrce) :- 
    mergeLoc(Lcx,Lcy,Lc),
    apply(Lc,Op,tuple(Lc,"{}",[]),T).
termArgs([lbrce(Lcx)|Tks],Op,T,Toks,_,rbrce) :- 
    terms(Tks,Tks2,Seq),
    checkToken(Tks2,Toks,rbrce(Lcy),"missing close brace, got %w"),
    mergeLoc(Lcx,Lcy,Lc),
    apply(Lc,Op,tuple(Lc,"{}",Seq),T).
termArgs([idTok(".",Lcd),idTok(Fld,LcF)|Tks],Op,T,Toks,_,Lst) :-
    binary(Lcd,".",Op,name(LcF,Fld),NOP),
    termArgs(Tks,NOP,T,Toks,id,Lst).
termArgs(Toks,T,T,Toks,Lst,Lst).

tupleize(app(_,name(_,","),tuple(_,"()",[L,R])), Lc, Op, tuple(Lc,Op,[L|Rest])) :-
    getTupleArgs(R,Rest).
tupleize(T,Lc,Op,tuple(Lc,Op,[T])).

getTupleArgs(app(_,name(_,","),tuple(_,"()",[L,R])), [L|Rest]) :-
    getTupleArgs(R,Rest).
getTupleArgs(T,[T]).

terms([],[],[]).
terms([rbrce(Lc)|Toks],[rbrce(Lc)|Toks],[]) :- !.
terms(Tks,Toks,[T|R]) :- 
    parse(Tks,T,Tks2),
    terms(Tks2,Toks,R).

lookAhead(Tk,[Tk|_]).

checkToken([Tk|Toks],Toks,Tk,_) :- !.
checkToken([Tk|Toks],Toks,_,Msg) :- locOfToken(Tk,Lc), reportError(Msg,[Tk],Lc).

checkTerminator(_,[],[]).
checkTerminator(_,Toks,Toks) :- Toks = [rbrce(_)|_].
checkTerminator(_,[term(_)|Toks],Toks) .
checkTerminator(rbrce,Toks,Toks).
checkTerminator(_,Tks,RTks) :-
    checkToken(Tks,RTks,term(_),"missing terminator, got '%s'").

handleInterpolation([segment(Str,Lc)],_,string(Lc,Str)).
handleInterpolation([],Lc,string(Lc,"")).
handleInterpolation(Segments,Lc,Term) :- 
  stringSegments(Segments,Inters),
  unary(Lc,"ssSeq",tuple(Lc,"[]",Inters),Fltn),
  unary(Lc,"ssFormat",Fltn,Term).

stringSegments([],[]).
stringSegments([Seg|More],[H|T]) :- stringSegment(Seg,H), stringSegments(More,T).

stringSegment(segment(Str,Lc),string(Lc,Str)).
stringSegment(interpolate(Text,[],Lc),Disp) :-
  subTokenize(Lc,Text,Toks),
  term(Toks,2000,Term,TksX,_),
  unary(Lc,"display",Term,Disp),
  ( TksX = [] ; lookAhead(ATk,TksX),locOf(ATk,ALc),reportError("extra tokens in string interpolation",[],ALc)).
stringSegment(interpolate(Text,Fmt,Lc),format(Lc,Term,Disp)) :-
  subTokenize(Lc,Text,Toks),
  term(Toks,2000,Term,TksX,_),
  binary(Lc,"format",Term,string(Lc,Fmt),Disp),
  ( TksX = [] ; lookAhead(ATk,TksX),locOf(ATk,ALc),reportError("extra tokens in string interpolation",[],ALc)).
