:- module(resource,[locateResource/2,locateResource/3]).

:- use_module(uri).

locateResource(absUri("file",Path,noQuery),Chars) :- 
  makePath(Path,Fl),
  grab_text(Fl,Chars).

locateResource(Base,Rel,Chars) :-
  resolveURI(Base,Rel,Uri),
  locateResource(Uri,Chars).

grab_text(Fl,T) :- open(Fl,read,Str), read_until_eof(Str,T), !.

read_until_eof(Str,[]) :- at_end_of_stream(Str), close(Str).
read_until_eof(Str,[Ch|M]) :- get_char(Str,Ch), read_until_eof(Str,M).
