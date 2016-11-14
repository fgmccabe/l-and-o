:- module(repository,[openRepository/2,
          locatePackage/5,
          openPackageAsStream/5,
          addPackage/5,
          packagePresent/6]).

% Implement a file-based repository.

:- use_module(uri).
:- use_module(misc).
:- use_module(resource).
:- use_module(parseUtils).
:- use_module(manifest).

openRepository(Root,repo(Root,Manifest)) :-
  resolveFile(Root,"manifest",MF),
  access_file(MF,read),
  readFile(MF,Chrs),
  parseManifest(Chrs,Manifest),!.
openRepository(Root,repo(Root,man([]))) :-
  resolveFile(Root,"manifest",MF),
  access_file(MF,write),!.

locatePackage(repo(Root,Man),Pkg,Act,U,Text) :-
  locateVersion(Man,Pkg,Act,U,Fn),
  resolveFile(Root,Fn,FileNm),
  readFile(FileNm,Text).

resolveFile(Root,Fl,MF) :-
  parseURI(Fl,Rel),
  resolveURI(Root,Rel,F),
  getUriPath(F,MF).

openPackageAsStream(repo(Root,Man),Pkg,Act,U,Stream) :-
  locateVersion(Man,Pkg,Act,U,fl(Fn)),
  resolveFile(Root,Fn,Fl),
  open(Fl,read,Stream).

locateVersion(man(Entries),pkg(Pkg,Vers),Act,U,Fn) :-
  is_member(entry(Pkg,Versions),Entries),
  getVersion(Vers,Versions,Act,U,Fn).

getVersion(Vers,V,Vers,U,Fn) :- is_member((Vers,U,Fn),V),!.
getVersion(defltVersion,V,Act,U,Fn) :- is_member((Act,U,Fn),V),!.

addPackage(repo(Root,Man),U,pkg(Pkg,Vers),Text,repo(Root,NM)) :-
  packageHash(Pkg,Vers,Hash),
  string_concat(Pkg,Hash,Fn),
  resolveFile(Root,Fn,FileNm),
  writeFile(FileNm,Text),!,
  addToManifest(Man,U,Pkg,Vers,fl(Fn),NM),
  flushManifest(Root,NM).

packageHash(Pkg,defltVersion,Hash) :-
  stringHash(0,Pkg,H),
  hashSixtyFour(H,Hash).
packageHash(Pkg,v(V),Hash) :-
  stringHash(0,Pkg,H1),
  stringHash(H1,V,H2),
  hashSixtyFour(H2,Hash).

packagePresent(repo(Root,Man),Pkg,Act,U,SrcWhen,When) :-
  locateVersion(Man,Pkg,Act,U,fl(Fn)),
  resolveFile(Root,Fn,FileNm),
  access_file(FileNm,read),
  time_file(FileNm,When),
  getUriPath(U,SrcFn),
  time_file(SrcFn,SrcWhen).

flushManifest(Root,M) :-
  showManifest(M,Chrs,[]),
  resolveFile(Root,"manifest",Fn),
  string_chars(Text,Chrs),
  writeFile(Fn,Text).

%% Each end-point directory has a manifest file in it.
% The role of the manifest is to map URIs to files

addToManifest(man(M),U,Pkg,Version,FileName,man(NM)) :-
  addEntry(M,U,Pkg,Version,FileName,NM).

addEntry([],U,Pkg,Version,FileName,[(entry(Pkg,[(pkg(Pkg,Version),U,FileName)]))]).
addEntry([entry(Pkg,Vers)|E],U,Pkg,Version,FileName,[entry(pkg(Pkg,Vers),NV)|E]) :- !,
  addVersion(Vers,U,Version,FileName,NV).
addEntry([E|M],U,Pkg,Version,FileName,[E|R]) :-
  addEntry(M,U,Pkg,Version,FileName,R).

addVersion([],U,Vers,FileNm,[(Vers,U,FileNm)]).
addVersion([(Vers,_,_)|V],U,Vers,FileNm,[(Vers,U,FileNm)|V]) :- !. % replace version
addVersion([V|M],U,Vers,FileNm,[V|R]) :- addVersion(M,U,Vers,FileNm,R).
