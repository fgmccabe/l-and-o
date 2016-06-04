/*
 * A utility package that supports the concept of equivalence classe
 * Each equivalence class has a canonical member, which may have
 * an arbitrary set of properties associated with it
   Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */
go.equiv{

  Equiv[T] <~ { 
	makeSameAs:[symbol,symbol]*.
	makeDifferentFrom:[symbol,symbol]*.
      }.

  equiv:[list[(symbol,T)],list[(symbol,T)]]@>Equiv[T].
  equiv(Sm,Df)..{
    sameTbl:hash[symbol,T] = hash(Sm,64).
    diffTbl:hash[symbol,T] = hash(Df,64).

  }.
}