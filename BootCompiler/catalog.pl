:- module(catalog,[locateCatalog/2,catalog//1,resolveCatalog/4,catalogBase/2]).

:- use_module(resource).
:- use_module(misc).
:- use_module(uri).
:- use_module(parseUtils).

locateCatalog(Uri,Cat) :-
  resolveURI(Uri,relUri(rel(["catalog"]),noQuery),CatURI),
  locateResource(CatURI,Chars),
  parseCatalog(Chars,CatURI,Cat),!.

resolveCatalog(cat(Cat),Nm,Uri,V) :-
  is_member(entries(Map),Cat),
  is_member(base(Base),Cat),
  is_member(entry(Nm,U),Map),!,
  resolveURI(Base,U,Uri),
  resolveVersion(Nm,Cat,V).
resolveCatalog(cat(Cat),Nm,Uri,V) :-
  is_member(default(Deflt),Cat),!,
  is_member(base(Base),Cat),
  resolveURI(Base,Deflt,DefltUri),
  locateCatalog(DefltUri,DefltCat),
  resolveCatalog(DefltCat,Nm,Uri,V).

resolveVersion(pkg(Pkg,defltVersion),Cat,pkg(Pkg,V)) :-
  catalogVersion(Cat,V).
resolveVersion(Pkg,_,Pkg).

catalogVersion(Cat,v(V)) :-
  is_member(version(V),Cat),!.
catalogVersion(_,defltVersion).

parseCatalog(Chrs,Uri,Cat) :-
  phrase(tokens(Toks),Chrs),
  phrase(catalog(C),Toks),
  defaultBase(C,Uri,Cat),!.

defaultBase(cat(Stmts),Fl,cat(NStmts)) :-
  replace(Stmts,base(_),base(Fl),NStmts).

catalogBase(cat(Stmts),Base) :-
  is_member(base(Base),Stmts).

catalog(cat(Stmts)) -->
  [iden("catalog"), lbrce], catStmts(Stmts), [rbrce].

catStmts([St|More]) --> catStmt(St), catStmts(More).
catStmts([]) --> [].

catStmt(entries(Contents)) --> [iden("content"), lbrce], contents(Contents), [rbrce].
catStmt(base(BaseUri)) --> [iden("base"), colon], string(Base), { parseURI(Base,BaseUri) }, [term].
catStmt(version(V)) --> [iden("version"), colon, string(V), term].
catStmt(default(CatUri)) --> [iden("default"), colon], string(U), { parseURI(U,CatUri) }, [term].

contents([Entry|More]) --> entry(Entry), contents(More).
contents([]) --> [].

entry(entry(Key,Uri)) --> package(Key), [thin_arrow], string(U), [term], {parseURI(U,Uri)}.

package(pkg(Pkg,V)) --> [iden(Id)], suffixes(Suf), version(V), { stringify([Id|Suf], ".", Pkg)}.

suffixes([Id|More]) --> [period], [iden(Id)], suffixes(More).
suffixes([]) --> [].

version(v(V)) --> [hash, iden(F)], vSegs(Vs), { stringify([F|Vs],".", V)}.
version(defltVersion) --> [].

vSegs([V|More]) --> [period], iden(iden(V)), vSegs(More).
vSegs([]) --> [].

stringify(L,S,O) :-
  interleave(L,S,I),
  concatStrings(I,O).

string(S) --> [string(S)].

% Tokenization

tokens(Toks) --> spaces, moreToks(Toks).

moreToks([]) --> at_end.
moreToks([Tok|More]) --> token(Tok), tokens(More).

token(lbrce) --> ['{'].
token(rbrce) --> ['}'].
token(thin_arrow) --> ['-','>'].
token(T) --> ['.'], ((at_end ; [' '] ; ['\n']) -> {T=term} ; {T=period}).
token(colon) --> [':'].
token(hash) --> ['#'].
token(string(Text)) --> ['"'], stringText(Seq), ['"'], { string_chars(Text,Seq) }.
token(iden(Id)) --> iden(Text), {string_chars(Id,Text)}.

stringText([]) --> [].
stringText([C|More]) --> ['\\'], quote(C), stringText(More).
stringText([C|More]) --> [C], { C \= '"' }, stringText(More).

quote('\n') --> 'n'.
quote('"') --> '"'.
quote('''') --> ''''.
quote('\t') --> t.
quote(C) --> [C].
