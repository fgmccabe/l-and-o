/*
  List view of I/O
  Allows file input to a grammar parser
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */

go.stream{
  import go.io.

  inputSequence:[inChannel,integer]@=list[char].
  inputSequence(F,filePosition)..{

    eof():-
	action{adjustOffset()},
	F.eof().

    head()::F.eof() => raise error("end of stream",'eFAIL').
    head()=>valof{
	      adjustOffset();
	      valis F.inCh();
	    }.

    cons(H) => [H,..this].

    tack(_) => raise error("tack not supported",'eINVAL').

    eq(this).

    hdtl(H,T) :-
	action{
	  adjustOffset()
	},
	\+F.eof(),
	H = F.inCh(),
	T = tail().

    tail()=>inputSequence(F,filePosition+1).

    private adjustOffset:[]*.
    adjustOffset()->
	(F.pos()!=filePosition ?
	   F.seek(filePosition)).
  }.

  listFile:[string,ioEncoding]=>list[char].
  listFile(Fl,Enc) => inputSequence(openInFile(Fl,Enc),0).

  listSocket:[string,integer,ioEncoding]=>(list[char],outChannel).
  listSocket(Host,Port,Enc) => valof{
				 tcpConnect(Host,Port,inChnl,outChnl,Enc);
				 valis (inputSequence(inChnl,0),outChnl)
			       }.
}