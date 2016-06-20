:- module(repository,[openRepository/2,
          locatePackage/5,locatePackage/4,
          openPackageAsStream/4,
          addPackage/4,addPackage/5,
          packagePresent/3]).

% Implement a file-based repository.

:- use_module(uri).
:- use_module(misc).
:- use_module(resource).
:- use_module(parseUtils).

openRepository(Root,repo(Root,Manifest)) :-
  resolveFile(Root,"manifest",MF),
  access_file(MF,read),
  readFile(MF,Chrs),
  parseManifest(Chrs,Manifest),!.
openRepository(Root,repo(Root,man([]))) :-
  resolveFile(Root,"manifest",MF),
  access_file(MF,write),!.

locatePackage(repo(Root,Man),Pkg,V,Vers,Text) :-
  locateVersion(Man,Pkg,V,Vers,Fn),
  resolveFile(Root,Fn,FileNm),
  readFile(FileNm,Text).

locatePackage(repo(Root,Man),Pkg,Vers,Text) :-
  locateVersion(Man,Pkg,defltVersion,Vers,Fn),
  resolveFile(Root,Fn,FileNm),
  readFile(FileNm,Text).

resolveFile(Root,Fl,MF) :-
  parseURI(Fl,Rel),
  resolveURI(Root,Rel,F),
  getUriPath(F,MF).

openPackageAsStream(repo(Root,Man),Pkg,Vers,Stream) :-
  locateVersion(Man,Pkg,defltVersion,Vers,fl(Fn)),
  resolveFile(Root,Fn,Fl),
  open(Fl,read,Stream).

locateVersion(man(Entries),Pkg,Vers,Act,Fn) :-
  is_member(entry(Pkg,V),Entries),
  getVersion(Vers,Act,V,Fn).

getVersion(Vers,Vers,V,Fn) :- is_member((Vers,Fn),V),!.
getVersion(defltVersion,Act,V,Fn) :- is_member((Act,Fn),V),!.

addPackage(Repo,Pkg,Text,Rx) :-
  addPackage(Repo,Pkg,defltVersion,Text,Rx).
addPackage(repo(Root,Man),Pkg,Vers,Text,repo(Root,NM)) :-
  packageHash(Pkg,Vers,Hash),
  string_concat(Pkg,Hash,Fn),
  resolveFile(Root,Fn,FileNm),
  writeFile(FileNm,Text),!,
  addToManifest(Man,pkg(Pkg),Vers,fl(Fn),NM),
  flushManifest(Root,NM).

packageHash(Pkg,defltVersion,Hash) :-
  stringHash(0,Pkg,H),
  hashSixtyFour(H,Hash).
packageHash(Pkg,v(V),Hash) :-
  stringHash(0,Pkg,H1),
  stringHash(H1,V,H2),
  hashSixtyFour(H2,Hash).

packagePresent(repo(Root,man(Entries)),Pkg,Vers) :-
  is_member(entry(Pkg,V),Entries),
  is_member((Vers,FN),V),
  resolveFile(Root,FN,FileNm),
  access_file(FileNm,read).

flushManifest(Root,M) :-
  showManifest(M,Chrs,[]),
  resolveFile(Root,"manifest",Fn),
  string_chars(Text,Chrs),
  writeFile(Fn,Text).

%% Each end-point directory has a manifest file in it.
% The role of the manifest is to map URIs to files

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

entry(entry(Package,Versions)) --> package(Package), colon, lbrce, versions(Versions), rbrce.

manifst --> spaces, ['m','a','n','i','f','e','s','t'].

colon --> spaces, [':'].

lbrce --> spaces, ['{'].

rbrce --> spaces, ['}'].

equals --> spaces, ['='].

package(pkg(Pkg)) --> spaces, iden(Id), suffixes(Suf), { flatten([Id|Suf], P), string_chars(Pkg,P)}.

suffixes([['.'|Id]|More]) --> ['.'], iden(Id), suffixes(More).
suffixes([]) --> [].

fileName(fl(Fn)) --> spaces, fileSeg(F), segments(Rest), { flatten([F|Rest],Fl), string_chars(Fn,Fl)}.

fileSeg([Ch|Mo]) --> (alpha(Ch) ; digit(Ch) ; period(Ch)), fileSeg(Mo).
fileSeg([]) --> [].

segments([['/'|Seg]|More]) --> ['/'], fileSeg(Seg), segments(More).
segments([]) --> \+['/'].

period('.') --> ['.'].

versions([V|More]) --> version(V), versions(More).
versions([]) --> [].

version((V,F)) --> versionName(V), equals, fileName(F),!.

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
  showPkg(Pkg,O0,O1),
  appStr(":{\n",O1,O2),
  showVersions(Versions,O2,O3),
  appStr("  }\n",O3,Ox).

showPkg(pkg(Pkg),O,Ox) :-
  appStr(Pkg,O,Ox).

showVersions([],O,O).
showVersions([V|M],O,Ox) :-
  showVersion(V,O,O1),
  showVersions(M,O1,Ox).

showVersion((V,F),O,Ox) :-
  appStr("    ",O,O1),
  showV(V,O1,O2),
  appStr("=",O2,O3),
  showFileName(F,O3,O4),
  appStr("\n",O4,Ox).

showV(v(V),O,Ox) :-
  appStr(V,O,Ox).
showV(defltVersion,O,Ox) :-
  appStr("*",O,Ox).

showFileName(fl(Nm),O,Ox) :-
  appStr(Nm,O,Ox).

addToManifest(M,Pkg,FileName,NM) :-
  addToManifest(M,Pkg,defltVersion,FileName,NM).
addToManifest(man(M),Pkg,Version,FileName,man(NM)) :-
  addEntry(M,Pkg,Version,FileName,NM).

addEntry([],Pkg,Version,FileName,[(entry(Pkg,[(Version,FileName)]))]).
addEntry([entry(Pkg,Vers)|E],Pkg,Version,FileName,[entry(Pkg,NV)|E]) :- !,
  addVersion(Vers,Version,FileName,NV).
addEntry([E|M],Pkg,Version,FileName,[E|R]) :-
  addEntry(M,Pkg,Version,FileName,R).

addVersion([],Vers,FileNm,[(Vers,FileNm)]).
addVersion([(Vers,_)|V],Vers,FileNm,[(Vers,FileNm)|V]) :- !. % replace version
addVersion([V|M],Vers,FileNm,[V|R]) :- addVersion(M,Vers,FileNm,R).
