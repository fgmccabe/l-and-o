:- module(uri,[parseURI/2,resolveURI/3,showUri/3,uriPath/2,makePath/2]).

:- use_module(misc).

parseURI(S,U) :-
  string_chars(S,Chrs),
  phrase(parseURI(U),Chrs).

parseURI(absUri(Scheme,Path,Query)) --> absoluteURI(Scheme,Path,Query).
parseURI(relUri(Path,Query)) --> relativeURI(Path,Query).

isAbsoluteURI(absUri(_,_,_)).

absoluteURI(Scheme,Path,Query) -->
    scheme(Sc), [':'], {string_chars(Scheme,Sc)}, 
    hierPart(Path,Query).

relativeURI(Path,Query) -->
  netPath(Path), optQuery(Query).
relativeURI(Path,Query) -->
  absolutePath(Path), optQuery(Query).
relativeURI(Path,Query) --> relativePath(Path), optQuery(Query).

scheme([C|S]) --> alpha(C), alphaStar(S).

hierPart(Path,Query) --> ( netPath(Path) ; absolutePath(Path)) , optQuery(Query).

netPath(net(A,P)) --> ['/','/'], authority(A), optAbsolutePath(P).

absolutePath(abs(P)) --> ['/'], pathSegments(P).

relativePath(rel(S)) --> pathSegments(S).

authority(A) --> server(A).

optAbsolutePath(Segs) --> absolutePath(Segs).
optAbsolutePath(empty) --> [].

stringify(S,L) :- map(L,string_chars,S).

pathSegments([Seg|Ments]) --> pathSegment(Seg), (['/'], pathSegments(Ments) ; {Ments = []}).

pathSegment(Seg) --> pchars(S,Rest), parameters(Rest), { string_chars(Seg,S) }.

parameters(P) --> parameter(P,S), parameters(S).
parameters([]) --> [].

parameter([';'|P],M) --> [';'], pchars(P,M).

pchars([C|M],R) --> unreserved(C), pchars(M,R).
pchars(['%',U,L|M],R) --> escaped(U,L), pchars(M,R).
pchars([':'|M],R) --> [':'], pchars(M,R).
pchars(['@'|M],R) --> ['@'], pchars(M,R).
pchars(['&'|M],R) --> ['&'], pchars(M,R).
pchars(['='|M],R) --> ['='], pchars(M,R).
pchars(['+'|M],R) --> ['+'], pchars(M,R).
pchars(['$'|M],R) --> ['$'], pchars(M,R).
pchars([','|M],R) --> [','], pchars(M,R).
pchars(X,X) --> [].

optServer(S) --> server(S).
optServer(noHost) --> [].

server(userHost(U,H)) --> optUserInfo(U), hostPort(H).

optUserInfo(U) --> userInfo(U), ['@'].
optUserInfo(noOne) --> [].

userInfo(user(User)) --> userStar(U), { string_chars(User,U) }.

userStar([C|S]) --> unreserved(C), userStar(S).
userStar(['%',U,L|S]) --> escaped(U,L), userStar(S).
userStar(['$'|S]) --> ['$'], userStar(S).
userStar([','|S]) --> [','], userStar(S).
userStar([';'|S]) --> [';'], userStar(S).
userStar([':'|S]) --> [':'], userStar(S).
userStar(['&'|S]) --> ['&'], userStar(S).
userStar(['='|S]) --> ['='], userStar(S).
userStar(['+'|S]) --> ['+'], userStar(S).
userStar([]) --> [].

hostPort(host(Host,Port)) --> host(H), ([':'], port(P) ; { P=[] }), {string_chars(Host,H), string_chars(Port,P) }.

host(H) --> alphaDashStar(H).

alphaDashStar([C|S]) --> (alpha(C) ; digit(C) ; minus(C) ; dot(C)) , alphaDashStar(S).
alphaDashStar([]) --> [].

port([D|M]) --> digit(D), port(M).
port([]) --> [].

optQuery(query(Q)) --> ['?'], query(S), { string_chars(Q,S)}.
optQuery(noQuery) --> [].

query(Q) --> uric(Q,M), query(M).
query([]) --> [].

alphaStar([C|S]) --> (alpha(C) ; digit(C) ; plus(C) ; minus(C) ; dot(C)), alphaStar(S).
alphaStar([]) --> [].

minus('-') --> ['-'].
plus('+') --> ['+'].
dot('.') --> ['.'].


% low level designations

alpha(C) --> (lowAlpha(C) ; upAlpha(C)).

lowAlpha(C) --> [C], { isLowAlpha(C) }.

upAlpha(C) --> [C], { isUpAlpha(C) }.

digit(C) --> [C], { digit(C) }.

alphanum(C) --> alpha(C) ; digit(C).

uric([C|M],M) --> reserved(C) ; unreserved(C).
uric(['%',U,L|M],M) --> escaped(U,L).

reserved(C) --> [C], { reserved(C) }.

unreserved(C) --> [C], { alphanum(C); mark(C) }.

mark(C) --> [C], { mark(C) }.

escaped(U,L) --> ['%'], hex(U), hex(L).

hex(C) --> digit(C).
hex(C) --> [C], { hexDigit(C) }.

delim(C) --> [C], { delim(C) }.

reserved(';').
reserved('/').
reserved('?').
reserved(':').
reserved('@').
reserved('&').
reserved('=').
reserved('+').
reserved('$').
reserved(',').

mark('-').
mark('_').
mark('.').
mark('!').
mark('~').
mark('*').
mark('''').
mark('(').
mark(')').

alphanum(C) :- isLowAlpha(C).
alphanum(C) :- isUpAlpha(C).
alphanum(C) :- digit(C).

isLowAlpha('a').
isLowAlpha('b').
isLowAlpha('c').
isLowAlpha('d').
isLowAlpha('e').
isLowAlpha('f').
isLowAlpha('g').
isLowAlpha('h').
isLowAlpha('i').
isLowAlpha('j').
isLowAlpha('k').
isLowAlpha('l').
isLowAlpha('m').
isLowAlpha('n').
isLowAlpha('o').
isLowAlpha('p').
isLowAlpha('q').
isLowAlpha('r').
isLowAlpha('s').
isLowAlpha('t').
isLowAlpha('u').
isLowAlpha('v').
isLowAlpha('w').
isLowAlpha('x').
isLowAlpha('y').
isLowAlpha('z').

isUpAlpha('A').
isUpAlpha('B').
isUpAlpha('C').
isUpAlpha('D').
isUpAlpha('E').
isUpAlpha('F').
isUpAlpha('G').
isUpAlpha('H').
isUpAlpha('I').
isUpAlpha('J').
isUpAlpha('K').
isUpAlpha('L').
isUpAlpha('M').
isUpAlpha('N').
isUpAlpha('O').
isUpAlpha('P').
isUpAlpha('Q').
isUpAlpha('R').
isUpAlpha('S').
isUpAlpha('T').
isUpAlpha('U').
isUpAlpha('V').
isUpAlpha('W').
isUpAlpha('X').
isUpAlpha('Y').
isUpAlpha('Z').

digit('0').
digit('1').
digit('2').
digit('3').
digit('4').
digit('5').
digit('6').
digit('7').
digit('8').
digit('9').

hexDigit('a').
hexDigit('b').
hexDigit('c').
hexDigit('d').
hexDigit('e').
hexDigit('f').
hexDigit('A').
hexDigit('B').
hexDigit('C').
hexDigit('D').
hexDigit('E').
hexDigit('F').

delim('<').
delim('>').
delim('#').
delim('%').
delim('"').

resolveURI(_,Resolve,Resolve) :-
  isAbsoluteURI(Resolve),!.
resolveURI(absUri(Scheme,Base,_),relUri(Path,Query),absUri(Scheme,NP,Query)) :-
  resolvePath(Base,Path,NP).

resolvePath(_,net(A,P),net(A,P)).
resolvePath(net(A,_),abs(P),net(A,P)).
resolvePath(net(A,P),rel(Segs),net(A,NP)) :-
  reverse(P,[_|R]),
  edit(Segs,R,NP).
resolvePath(abs(_),abs(P),abs(P)).
resolvePath(abs(B),rel(P),abs(NP)) :-
  reverse(B,[_|R]),
  edit(P,R,NP).

edit([""|Segs],R,Edit) :- edit(Segs,R,Edit).
edit(["."|Segs],R,Edit) :- edit(Segs,R,Edit).
edit([".."|Segs],[_|R],Edit) :- edit(Segs,R,Edit).
edit(Segs,R,Edit) :- reverse(R,RR), concat(RR,Segs,Edit).

showUri(absUri(Scheme,Path,Query),O,Ox) :-
  pScheme(Scheme,O,O1),
  pHierPath(Path,O1,O2),
  pQuery(Query,O2,Ox).

pScheme(S,O,Ox) :-
  string_chars(S,C),
  concat(C,[':'|Ox],O).

pHierPath(net(A,P),['/','/'|O],Ox) :-
  pAuthority(A,O,O1),
  pPath(P,O1,Ox).
pHierPath(abs(P),['/'|O],Ox) :-
  pSegs(P,O,Ox).

pAuthority(userHost(U,H),O,Ox) :-
  pUser(U,O,O1),
  pHost(H,O1,Ox).

pUser(noOne,O,O).
pUser(user(U),O,Ox) :-
  string_chars(U,Ch),
  concat(Ch,['@'|Ox],O).

pHost(host(Host,Port),O,Ox) :-
  string_chars(Host,Chrs),
  concat(Chrs,O,O1),
  pPort(Port,O1,Ox).

pPort("",O,O).
pPort(Port,[':'|O],Ox) :-
  string_chars(Port,P),
  concat(P,Ox,O).

pPath(rel(Segs),O,Ox) :-
  pSegs(Segs,O,Ox).
pPath(abs(Segs),['/'|O],Ox) :-
  pSegs(Segs,O,Ox).

pSegs([],O,O).
pSegs([Seg],O,Ox) :- !,
  string_chars(Seg,Chrs),
  concat(Chrs,Ox,O).
pSegs([Seg|More],O,Ox) :-
  string_chars(Seg,Chrs),
  concat(Chrs,['/'|O1],O),
  pSegs(More,O1,Ox).

pQuery(noQuery,O,O).
pQuery(query(Q),['?'|O],Ox) :-
  string_chars(Q,Chrs),
  concat(Chrs,Ox,O).

makePath(P,Text) :-
  pHierPath(P,O,[]),
  string_chars(Text,O).

uriPath(absUri(_,Pth,_),Path) :-
  makePath(Pth,Path).
uriPath(relUri(Pth,_),Path) :-
  makePath(Pth,Path).
