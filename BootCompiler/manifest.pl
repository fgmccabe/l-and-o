:- module(manifest,[readManifest/2,parseManifest/2,manifest//1,showManifest/3]).

:- use_module(parseUtils).
:- use_module(uri).
:- use_module(misc).

readManifest(Path,Manifest) :-
  readFile(Path,Chars),
  parseManifest(Chars,Manifest).

parseManifest(Chrs,M) :-
  phrase(manifest(M),Chrs).

manifest(man(E)) --> manifst, lbrce, entries(E), rbrce,spaces.

% An entry looks like:
% packageName : { (version:file)* }

entries([Entry|More]) --> entry(Entry), entries(More).
entries([]) --> [].

entry(entry(Pkg,Versions)) --> package(Pkg), colon, lbrce, versions(Pkg,Versions), rbrce.

manifst --> spaces, ['m','a','n','i','f','e','s','t'].

colon --> spaces, [':'].

lbrce --> spaces, ['{'].

rbrce --> spaces, ['}'].

equals --> spaces, ['='].

package(Pkg) --> spaces, iden(Id), suffixes(Suf), { flatten([Id|Suf], P), string_chars(Pkg,P)}.

suffixes([['.'|Id]|More]) --> ['.'], iden(Id), suffixes(More).
suffixes([]) --> [].

fileName(fl(Fn)) --> spaces, fileSeg(F), segments(Rest), { flatten([F|Rest],Fl), string_chars(Fn,Fl)}.

fileSeg([Ch|Mo]) --> (alpha(Ch) ; digit(Ch) ; period(Ch)), fileSeg(Mo).
fileSeg([]) --> [].

segments([['/'|Seg]|More]) --> ['/'], fileSeg(Seg), segments(More).
segments([]) --> \+['/'].

period('.') --> ['.'].

versions(Pkg,[V|More]) --> version(Pkg,V), versions(Pkg,More).
versions(_,[]) --> [].

version(Pkg,(pkg(Pkg,V),U,F)) --> versionName(V), equals, fileName(F), ['['], uri(U), [']'], !.

versionName(defltVersion) --> spaces, ['*'].
versionName(v(V)) --> spaces, fileSeg(S), { string_chars(V,S) }. %% do more later

showManifest(man(E),O,Ox) :- 
  appStr("manifest",O,O1),
  appStr("{\n",O1,O2),
  showEntries(E,O2,O3),
  appStr("}\n",O3,Ox).

showEntries([],O,O).
showEntries([E|M],O,Ox) :-
  showEntry(E,O,O1),
  showEntries(M,O1,Ox).

showEntry(entry(Pkg,Versions),O,Ox) :-
  appStr("  ",O,O0),
  appStr(Pkg,O0,O1),
  appStr(":{\n",O1,O2),
  showVersions(Versions,O2,O3),
  appStr("  }\n",O3,Ox).

showPkg(pkg(Pkg),O,Ox) :-
  appStr(Pkg,O,Ox).

showVersions([],O,O).
showVersions([V|M],O,Ox) :-
  showVersion(V,O,O1),
  showVersions(M,O1,Ox).

showVersion((V,U,F),O,Ox) :-
  appStr("    ",O,O1),
  showV(V,O1,O2),
  appStr("=",O2,O3),
  showFileName(F,O3,O4),
  appStr("[",O4,O5),
  showUri(U,O5,O6),
  appStr("]\n",O6,Ox).

showV(v(V),O,Ox) :-
  appStr(V,O,Ox).
showV(defltVersion,O,Ox) :-
  appStr("*",O,Ox).

showFileName(fl(Nm),O,Ox) :-
  appStr(Nm,O,Ox).
