/*
  Analyse a profile listing 
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */

go.profiler{
  import go.io.
  import go.xml.
  import go.sort.
  import go.hash.
  import go.stdparse.

  Table:hash[(symbol,symbol,integer),entry] = hash([],1024).

  main(_) ->
      pF = openInFile("goProfile.out",utf8Encoding);
      (P0,F0,L0,T0) = extractInfo(xmlParse%%pF.inLine("\n")); -- first line starts the aggregation
      processProfile(pF,1,P0,F0,L0,T0);
      display(sort({P..(_,P) in Table.ext()},pcompare),'').

  processProfile:[inChannel,integer,symbol,symbol,integer,number]*.
  processProfile(pF,Count,_,_,_,_)::pF.eof() -> 
      stdout.outLine("done reading "<>Count.show()<>" lines").
  processProfile(pF,Count,P0,F0,L0,T0) ->
      stdout.outStr("("<>Count.show()<>")\r");
      (P1,F1,L1,T1) = extractInfo(xmlParse%%pF.inLine("\n"));
      tabulate(P0,F0,L0,T1-T0);
      processProfile(pF,Count+1,P1,F1,L1,T1).

  extractInfo:[xmlDOM]=>(symbol,symbol,integer,float).
  extractInfo(xmlElement('event',Atts,_))::
    xmlAtt('tId',Process) in Atts, xmlAtt('File',File) in Atts,
	  xmlAtt('Line',Line) in Atts,
	  xmlAtt('timeStamp',tmText) in Atts => 
      (implode(Process),implode(File),integerOf%%Line,floatOf%%tmText).

  tabulate:[symbol,symbol,integer,number]*.
  tabulate(P,F,L,T) :: Table.present((P,F,L),prof(P,F,L,Count,oT)) ->
      Table.insert((P,F,L),prof(P,F,L,Count+1,oT+T)).
  tabulate(P,F,L,T) ->
      Table.insert((P,F,L),prof(P,F,L,1,T)).

  pcompare:[]@=comparable[entry].
  pcompare..{
    less(prof(_,_,_,_,C1),prof(_,_,_,_,C2)) :- C1<C2.
    equal(x,x).
  }.

  entry <~ thing.

  prof:[symbol,symbol,integer,integer,number]@=entry.
  prof(Proc,File,Line,Count,Time)..{
    show() => Count.show()<>" times, for "<>Time.show()<>" secs".
  }.

  display:[list[entry],symbol]*.
  display([],_) -> {}.
  display([prof(P,F,L,Count,T),..Bins],P) ->
      stdout.outLine("At "<>explode(F)<>":"<>L.show()<>", "<>Count.show()<>" times, for "<>
                     T.show()<>"secs");
      display(Bins,P).
  display([prof(P,F,L,Count,T),..Bins],_) ->
      stdout.outLine("In process "<>explode(P));
      stdout.outLine("At "<>explode(F)<>":"<>L.show()<>
		     ", "<>Count.show()<>" times, for "<>
                     T.show()<>"secs");
    display(Bins,P).
}.
      

  


