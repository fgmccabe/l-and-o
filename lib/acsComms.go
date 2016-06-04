/*
 * Interface to the ACS communications system
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */
 
acsComms{
  import go.io.
  
-- A locator is an address structure
  locator ::= locator(symbol,string,QoS[]).
  
  acsHandle ::= acsHdl((symbol,any)[]).
  
-- A directory entry has a name, quality of service and other attributes
  dir_entry ::= dirE(symbol,QoS[],acsHandle).
  
  acsComms(acsHost,acsPort,dsHost,dsPort,acsHdl(Desc)) => 
      ((genHandle(acsHdl([('address',lcAddr),..Desc])),spawn{ outGoing()}) ..{
	 lcAddr = valof{
	   enCode(acsOut,??(('connectMe',[])));
	  ??(('addressIs',Reply:string)) = deCode(acsIn);
	   spawn{inComing()};
	   valis ??(Reply)
	 }.
    
	 outGoing() -> 
	     ( (For,Opts,Msg) << From ->
		   enCode(acsOut,??((genAddress(For),genAddress(From),[],sencode(Msg))))
	     | X << From -> outLine(stdout,"Unknown message "<>X^2<>" from "<>From^0)
	     ); outGoing().
  
	 inComing()::\+atEof(acsIn) ->
	    ??((_,_,_,Msg)) = deCode(acsIn);
	     distributeMsg(sdecode(Msg));
	     inComing().
	 
	 distributeMsg(??((For,From,Opts,Msg))) ->
	     __postMsg(genHandle(From),Msg,genHandle(For),genReply(Opts,From),genLease(Opts)).
      
	 genReply([],F)=>F.
	 genReply([('replyTo',??(H:handle)),.._],_) => H.
	 genReply([_,..L],F) => genReply(L,F).
	 
	 genLease([])=>0.
	 genLease([('leaseHold',??(N:number)),.._]) => N.
	 genLeas([_,..L]) => genLease(L).
	 
       })..{
	(acsIn,acsOut) = tcpConnect(acsHost,acsPort).
	(genAddress,genHandle) = dsTable(dsHost,dsPort).
	
	dsTable(host,port) =>
	    (genAddress,genHandle) .. {
	      table = $cell[handle]([]).
	      (dsIn,dsOut) = tcpConnect(host,port).
       
	      matchHdl(hdl(L1),hdl(L2)) :-
		  mtch(L1)..{
		    mtch([]):--true.
		    mtch([(K,V),..Lr]) :--
			(K,V) in L2,
			mtch(Lr)
		  }.
	      
	      genHandle(h) => H :- H in table.get(), matchHdl(h,H).
	      genHandle(h) => valof{
				enCode(dsOut,??(('queryOne',h)));
				Rpl = deCode(dsIn);
				table.set([Rpl,.. table.get()]);
				valis Rpl
			      }.
	      
	      genAddress(h) => A:string :- acsHdl(H)=genHandle(h),('address',??(A)) in H.
	    }.
       }.
}.

