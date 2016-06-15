:- module(catalog,[locateCatalog/2,catalog//1,resolveCatalog/3]).

:- use_module(resource).
:- use_module(misc).
:- use_module(uri).

locateCatalog(Uri,Cat) :-
  resolveURI(Uri,relUri(rel(["catalog"]),noQuery),CatURI),
  locateResource(CatURI,Chars),
  parseBagOChars(Chars,CatURI,Cat).

resolveCatalog(Nm,cat(Cat),Uri) :-
  is_member(contents(Map),Cat),
  is_member(entry(Nm,Uri),Map),!.
resolveCatalog(Nm,cat(Cat),Uri) :-
  is_member(base(Base),Cat),
  resolveURI(Base,Nm,Uri).

parseBagOChars(Chrs,Uri,Cat) :-
  phrase(tokens(Toks),Chrs),
  phrase(catalog(C),Toks),
  defaultBase(C,Uri,Cat).

defaultBase(cat(Stmts),Fl,cat(NStmts)) :-
  replace(Stmts,base(_),base(Fl),NStmts).

catalog(cat(Stmts)) -->
  [catalog, lbrce], catStmts(Stmts), [rbrce].

catStmts([]) --> [].
catStmts([St|More]) --> catStmt(St), catStmts(More).

catStmt(entries(Contents)) --> [content, is, lbrce], contents(Contents), [rbrce].
catStmt(base(Base)) --> [base, is], string(Base), [period].
catStmt(version(Version)) --> [version, is], string(Version), [period].

contents([]) --> [].
contents([Entry|More]) --> entry(Entry), contents(More).

entry(entry(Key,Url)) --> string(Key), [thin_arrow], string(Url), [period].

string(S) --> [string(S)].

% Tokenization

tokens(Toks) --> skipSpaces, moreToks(Toks).

moreToks([]) --> at_end.
moreToks([Tok|More]) --> token(Tok), tokens(More).

skipSpaces --> space, skipSpaces.
skipSpaces --> \+ space.

space --> ([' '] ; ['\t']; ['\n']),!.
space --> ['-', '-'], ([' '] ; ['\t']), eol.

eol --> ['\n'].
eol --> [C], { C \= '\n'}, eol.
eol --> at_end.

token(catalog) --> [c,a,t,a,l,o,g].
token(content) --> [c,o,n,t,e,n,t].
token(version) --> [v,e,r,s,i,o,n].
token(base) --> [b,a,s,e].
token(is) --> [i,s].
token(lbrce) --> ['{'].
token(rbrce) --> ['}'].
token(thin_arrow) --> ['-','>'].
token(period) --> ['.'], (at_end ; [' '] ; ['\n']).
token(string(Text)) --> ['"'], stringText(Seq), ['"'], { string_chars(Text,Seq) }.

stringText([]) --> [].
stringText([C|More]) --> ['\\'], quote(C), stringText(More).
stringText([C|More]) --> [C], { C \= '"' }, stringText(More).

quote('\n') --> 'n'.
quote('"') --> '"'.
quote('''') --> ''''.
quote('\t') --> t.
quote(C) --> [C].

at_end --> \+ [_].