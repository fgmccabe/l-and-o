:- module(grab, [grab_text/2]).

grab_text(Fl,T) :- open(Fl,read,Str), read_until_eof(Str,T), !.

read_until_eof(Str,[]) :- at_end_of_stream(Str), close(Str).
read_until_eof(Str,[Ch|M]) :- get_char(Str,Ch), read_until_eof(Str,M).