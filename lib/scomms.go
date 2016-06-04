/*
   Interface to the simple agent communications system
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */
 
go.scomms{
  import go.io.
  import go.stdparse.
  import go.hash.
  import go.encoded.
  import go.mbox.

  scsMailbox(N)<=mailbox(N).
  scsMailbox(N):mailbox[]..{
    ${
      known.insert(N,this)
    }
  }.

  defaultHost() => getenv('GO_COMMS_HOST',"localhost").
  defaultPort() => numeric%%getenv('GO_COMMS_PORT',"4545").

  __default(C,Opts,_) => Val :- (C,Val) in Opts.
  __default(_,_,Def) => Def.

  pickHandle(Options) => __default(`n,Options,'<main>').

  scsOut := nullout.
  scsIn := nullin.

  known = $hash([],32).

  ${
    tcpConnect(defaultHost(),defaultPort(),In,Out,rawEncoding);
    scsOut := Out;
    scsIn := In;
    spawn{inComing(In)}
  }.

  inComing(In)::In.eof() -> {}.
  inComing(In) ->
      grabCodedInput(In,Data);
      processIncoming(decodeTerm%%Data);
      inComing(In).

  processIncoming(anyVal(tuple([_,_,strg(Msg)]),_)) ->
      ( ??((For:handle[],F:handle[],_:list[(symbol,any)],M:any)) = sdecode(Msg) ?
          distributeMessage(For,F,M)
      | {__logmsg("Unrecognizable message "<>sdecode(Msg)^0)}
      ).
  processIncoming(anyVal(symb("'Ok"),_)) -> {}.
  
  distributeMessage(For,From,Msg) ->
--      stdout.outLine("Message "<>Msg^0<>" for "<>For.show()<>" from "<>From.show());
      ( known.present(For,H) ?
          H.enQueue(Msg,From)
      | stderr.outLine("discarding message "<>Msg^0<>" for unknown target: "<>For.show())
      ).

  hdl(Thr,_) <= hd(Thr).  -- We inherit from internal handles for internal delivery
  hdl(Thr,Root):handle[] .. {
    register() -> 
        scsOut.encode(??(('register',Root)));
--        ( ??('Ok') = scsIn.decode() ? {}
--        | raise error("register",'eFAIL'));
        known.insert(hdl(Thr,Root),this).
    
    deregister() ->
        known.delete(hdl(Thr,Root)).
    
    show()=>explode(Thr)<>":"<>explode(Root).

    postMsg(Msg)::self<=hdl(T,R) ->  -- Someone has posted a message to this handle, 
        scsOut.outBytes(encodeTerm(anyVal(tuple([hndl(Thr,Root),hndl(T,R),
                                                 strg(sencode(??((hdl(Thr,Root),
                                                                  hdl(T,R),[]:list[(symbol,any)],Msg))))]),
                                          tupleType([handleType,handleType,polyType("list",[charType])])))).
    postMsg(_) ->
        raise error("can only send to this handle from other registered scs handles",'fail')
  }.
}.
