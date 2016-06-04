/*
  Bootstrap program, this program is loaded automatically 
  when the Go! engine is started. Its main function is to set up the 
  environment and then load and execute the program mentioned in the command line
  
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/
go.boot{
  __main:[string,list[(char,symbol)],list[string]]*.
  __main(Path,Options,Cmd) -> 
      ([Prog,..Args] = Cmd ?
	 pkg = implode(chopGoc(Prog));
	 entry = __default(`m,Options,'main');
	 __ensure_loaded(Path,pkg,__default(`M,Options,''),depends)
     | raise error("missing package name",'eNOTFND')
      )
      onerror(
       Err -> {__logmsg("Fatal error on load: "<>Err.show())}; exit(1)
      );
      execInits(reverse(depends));
      (__defined(pkg,entry,1)?
         ((__call(pkg,entry,1,Args);exit(0)) onerror ( -- __call may *not* be the last call!!!!
        E -> {__logmsg("Fatal error("<>E.show()<>")")};exit(1)
          ))
     | {__logmsg("Entry: "<>explode(entry)<>" not defined in "<>Prog)};exit(1)
      ).
  
  private execInits:[list[symbol]]*.
  execInits([]) -> {}.
  execInits([P,..L]) -> 
      __call(P,'$init',0,[]);
      execInits(L).

  private __default:[char,list[(char,symbol)],symbol]=>symbol.
  __default(C,Opts,_)::(C,Val) in Opts => Val.
  __default(_,_,Def) => Def.

  private chopGoc:[string]=>string.
  chopGoc([])=>[].
  chopGoc(".goc")=>[].
  chopGoc([C,..L])=>[C,..chopGoc(L)].
}
