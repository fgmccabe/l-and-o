:- module(bootgrammar,[parse/3]).

:- use_module(operators).
:- use_module(abstract).
:- use_module(location).
:- use_module(errors).
:- use_module(lexer).

parse(Tks,T,RTks) :-
    term(Tks,2000,T,Tks1),!,
    checkTerminator(Tks1,RTks).

term(Tks,Pr,T,Toks) :-
    termLeft(Tks,Pr,Left,LftPr,Tks1),!,
    termRight(Tks1,Pr,LftPr,Left,T,Toks).

termLeft([idTok(Id,Lc),rpar(Lcy)|Toks],_,name(Lc,Id),0,[rpar(Lcy)|Toks]).
termLeft([idTok(Id,Lcx)|Tks],Pr,Left,LftPr,Toks) :-
    prefixOp(Id,LftPr,OpRightPr),
    \+ lookAhead(rpar(_),Tks),
    LftPr =< Pr, !,
    term(Tks,OpRightPr,Arg,Toks),
    locOfAst(Arg,Lcy),
    mergeLoc(Lcx,Lcy,Lc),
    unary(Lc,Id,Arg,Left).
termLeft(Tks,_,T,0,Toks) :- term0(Tks,T,Toks).

termRight([idTok(Id,_)|Tks],Pr,LPr,Left,T,Toks) :- 
  infixOp(Id,InfOpL,InfOpPr,InfOpR), InfOpPr =< Pr, LPr =< InfOpL,
  postfixOp(Id,PostOpL,PostOpPr), PostOpPr =< Pr, LPr =< PostOpL,
  legalNextRight(Tks,InfOpR), !,
  term(Tks,InfOpR, Right, Tks1),
  locOfAst(Left,Lcx),
  locOfAst(Right,Lcy),
  mergeLoc(Lcx,Lcy,Lc),
  binary(Lc,Id,Left,Right,NewLeft),
  termRight(Tks1,Pr,InfOpPr,NewLeft,T,Toks).
termRight([idTok(Id,Lcy)|Tks],Pr,LPr,Left,T,Toks) :- 
  postfixOp(Id,PostOpL,PostOpPr), PostOpPr =< Pr, LPr =< PostOpL,
  locOfAst(Left,Lcx),
  mergeLoc(Lcx,Lcy,Lc),
  unary(Lc,Id,Left,NewLeft),
  termRight(Tks,Pr,PostOpPr,NewLeft,T,Toks).
termRight([idTok(Id,_)|Tks],Pr,LPr,Left,T,Toks) :- 
  infixOp(Id,InfOpL,InfOpPr,InfOpR), InfOpPr =< Pr, LPr =< InfOpL,
  term(Tks,InfOpR, Right, Tks1),
  locOfAst(Left,Lcx),
  locOfAst(Right,Lcy),
  mergeLoc(Lcx,Lcy,Lc),
  binary(Lc,Id,Left,Right,NewLeft),
  termRight(Tks1,Pr,InfOpPr,NewLeft,T,Toks).
termRight(Toks,_,_,Left,Left,Toks).

legalNextRight([idTok(I,_)|_],Pr) :- ( prefixOp(I,PPr,_), PPr=<Pr ; \+ isOperator(I,_)) , !.
legalNextRight([lpar(_)|_],_).
legalNextRight([lbra(_)|_],_).
legalNextRight([lbrce(_)|_],_).
legalNextRight([lqpar(_)|_],_).
legalNextRight([stringTok(_,_)|_],_).
legalNextRight([integerTok(_,_)|_],_).
legalNextRight([floatTok(_,_)|_],_).

term00([idTok(I,Lc)|Toks],T,Toks) :- 
      (isOperator(I,_), \+lookAhead(rpar(_),Toks), !, reportError("unexpected operator: %s",[I],Lc),T=void(Lc);
      T = name(Lc,I)).
term00([lpar(Lc0),rpar(Lc2)|Toks],tuple(Lc,"()",[]),Toks) :- mergeLoc(Lc0,Lc2,Lc).
term00([lpar(Lcx)|Tks],T,Toks) :- term(Tks,2000,Seq,Tks2), checkToken(Tks2,Toks,rpar(Lcy),"missing close parenthesis, got %w"), mergeLoc(Lcx,Lcy,Lc), tupleize(Seq,Lc,"()",T).
term00([lbra(Lc0),rbra(Lc2)|Toks],tuple(Lc,"[]",[]),Toks) :- mergeLoc(Lc0,Lc2,Lc).
term00([lbra(Lcx)|Tks],T,Toks) :- term(Tks,2000,Seq,Tks2), checkToken(Tks2,Toks,rbra(Lcy),"mising close bracket, got %w"), mergeLoc(Lcx,Lcy,Lc), tupleize(Seq,Lc,"[]",T).
term00([lbrce(Lc0),rbrce(Lc2)|Toks],tuple(Lc,"{}",[]),Toks) :- mergeLoc(Lc0,Lc2,Lc).
term00([lbrce(Lcx)|Tks],tuple(Lc,"{}",Seq),Toks) :- terms(Tks,Tks2,Seq), checkToken(Tks2,Toks,rbrce(Lcy),"missing close brace, got %w"), mergeLoc(Lcx,Lcy,Lc).
term00([lqpar(Lcx)|Tks],unary(Lc,"<||>",T),Toks) :- term(Tks,2000,T,Tks2), checkToken(Tks2,Toks,rpar(Lcy),"missing close parenthesis, got %w"),mergeLoc(Lcx,Lcy,Lc).

term0([stringTok(St,Lc)|Toks],Str,Toks) :- handleInterpolation(St,Lc,Str).
term0([integerTok(In,Lc)|Toks],integer(Lc,In),Toks).
term0([floatTok(Fl,Lc)|Toks],float(Lc,Fl),Toks).
term0(Tks,T,Toks) :- term00(Tks,Op,RTks), termArgs(RTks,Op,T,Toks).

termArgs([],T,T,[]).
termArgs([lpar(Lcx),rpar(Lcy)|Tks],Op,T,Toks) :- 
    mergeLoc(Lcx,Lcy,Lc),
    apply(Lc,Op,tuple(Lc,"()",[]),NOP),
    termArgs(Tks,NOP,T,Toks).
termArgs([lpar(Lcx)|Tks],Op,T,Toks) :- term(Tks,2000,Seq,Tks2),
    checkToken(Tks2,Tks3,rpar(Lcy),"missing close parenthesis"),
    mergeLoc(Lcx,Lcy,Lc),
    tupleize(Seq,Lc,"()",Args),
    apply(Lc,Op,Args,NOP),
    termArgs(Tks3,NOP,T,Toks).
termArgs([lbra(Lcx),rbra(Lcy)|Tks],Op,T,Toks) :- 
    mergeLoc(Lcx,Lcy,Lc),
    apply(Lc,Op,tuple(Lc,"[]",[]),NOP),
    termArgs(Tks,NOP,T,Toks).
termArgs([lbra(Lcx)|Tks],Op,T,Toks) :- 
    term(Tks,2000,Seq,Tks2),
    checkToken(Tks2,Tks3,rbra(Lcy),"missing close bracket, got %w"),
    mergeLoc(Lcx,Lcy,Lc),
    tupleize(Seq,Lc,"[]",Args),
    apply(Lc,Op,Args,NOP),
    termArgs(Tks3,NOP,T,Toks).
termArgs([lbrce(Lcx),rbrce(Lcy)|Tks],Op,T,Tks) :- 
    mergeLoc(Lcx,Lcy,Lc),
    apply(Lc,Op,tuple(Lc,"{}",[]),T).
termArgs([lbrce(Lcx)|Tks],Op,T,Toks) :- terms(Tks,Tks2,Seq),
    checkToken(Tks2,Toks,rbrce(Lcy),"missing close brace, got %w"),
    mergeLoc(Lcx,Lcy,Lc),
    apply(Lc,Op,tuple(Lc,"{}",Seq),T).
termArgs([idTok(".",Lcd),idTok(Fld,LcF)|Tks],Op,T,Toks) :-
    binary(Lcd,".",Op,name(LcF,Fld),NOP),
    termArgs(Tks,NOP,T,Toks).
termArgs(Toks,T,T,Toks).

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

printAhead([Tk|_]) :- writeln(Tk).

checkToken([Tk|Toks],Toks,Tk,_) :- !.
checkToken([Tk|Toks],Toks,_,Msg) :- locOfToken(Tk,Lc), reportError(Msg,[Tk],Lc).

checkTerminator([],[]).
checkTerminator(Toks,Toks) :- Toks = [rbrce(_)|_].
checkTerminator(Tks,RTks) :-
    checkToken(Tks,RTks,term(_),"missing terminator, got %w").

handleInterpolation([segment(Str,Lc)],_,string(Lc,Str)).
handleInterpolation([],Lc,string(Lc,"")).
handleInterpolation(Segments,Lc,interString(Lc,Inters)) :- stringSegments(Segments,Inters).

stringSegments([],[]).
stringSegments([Seg|More],[H|T]) :- stringSegment(Seg,H), stringSegments(More,T).

stringSegment(segment(Str,Lc),string(Lc,Str)).
stringSegment(interpolate(Text,[],Lc),display(Lc,Term)) :-
  subTokenize(Lc,Text,Toks),
  term(Toks,2000,Term,TksX),
  ( TksX = [] ; lookAhead(ATk,TksX),locOf(ATk,ALc),reportError("extra tokens in string interpolation",[],ALc)).
stringSegment(interpolate(Text,Fmt,Lc),format(Lc,Term,Fmt)) :-
  subTokenize(Lc,Text,Toks),
  term(Toks,2000,Term,TksX),
  ( TksX = [] ; lookAhead(ATk,TksX),locOf(ATk,ALc),reportError("extra tokens in string interpolation",[],ALc)).
