:- module(catalog,[parse_catalog/2,catalog//1]).
:- use_module(grab).

parse_catalog(Fl,Cat) :- grab_text(Fl,Chrs),
  phrase(tokens(Toks),Chrs),
  phrase(catalog(Cat),Toks).

catalog(cat(Stmts)) -->
  [catalog, lbrce], catStmts(Stmts), [rbrce].

catStmts([]) --> [].
catStmts([St|More]) --> catStmt(St), catStmts(More).

catStmt(entries(Contents)) --> [content, is, lbrce], contents(Contents), [rbrce].
catStmt(base(Base)) --> [base, is], string(Base).
catStmt(version(Version)) --> [version, is], string(Version).

contents([]) --> [].
contents([Entry|More]) --> entry(Entry), contents(More).

entry(entry(Key,Url)) --> string(Key), [thin_arrow], string(Url).

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