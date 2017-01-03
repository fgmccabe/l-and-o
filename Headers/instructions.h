/*
  Specification of the L&O Machine Instruction set
  Copyright (c) 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

/*
 * The pattern and no of operands of the instruction is given by the 
 * instruction template field.
*/


/* program control instructions */
instruction(halt,0,nOp,nOp,"stop execution")
instruction(die,1,nOp,nOp,"stop current process")
instruction(succ,2,nOp,nOp,"Succeed a clause")
instruction(kawl,3,uAr,ltl,"call to program")
instruction(lkawl,4,uAr,ltl,"last call to program")
instruction(dlkawl,5,uAr,ltl,"deallocating last call")
instruction(kawlO,6,uAr,iAm,"call object method")
instruction(lkawlO,7,uAr,iAm,"last method call")
instruction(dlkawlO,8,uAr,iAm,"deallocating last variable call")
instruction(go_to,12,pcl,nOp,"jump")
instruction(escape,13,uAr,Es,"service function")
instruction(alloc,16,uAr,vSz,"allocate locals")
instruction(dealloc,17,nOp,nOp,"deallocate locals")
instruction(tryme,18,pcl,nOp,"try inline clause")
instruction(retryme,19,pcl,nOp,"retry inline clause")
instruction(trustme,20,nOp,nOp,"last inline clause")
instruction(trycl,21,pcl,nOp,"try clause")
instruction(retry,22,pcl,nOp,"retry clause")
instruction(trust,23,pcl,nOp,"last clause")
instruction(fayl,24,nOp,nOp,"fail current execution")
instruction(cut,25,nOp,nOp,"cut choice point")
instruction(indexi,26,iAh,Ltl,"integer index jump")
instruction(indexl,27,iAh,nOp,"list index")
instruction(indexs,28,iAh,Ltl,"symbol index jump")
instruction(indexn,29,iAh,Ltl,"numerical index jump")
instruction(indexx,30,iAh,Ltl,"constructor index jump")
instruction(trpblk,31,nOp,nOp,"start error block")
instruction(trpend,32,nOp,nOp,"end error block")
instruction(except,33,iAh,nOp,"raise run-time exception")
instruction(gcmap,34,uAr,lSz,"Set active arguments & local depth")
instruction(gc,35,uAr,lSz,"Invoke GC if not enough space")
instruction(susp,37,iAh,iAm,"suspend/execute call")
instruction(resume,38,oAr,nOp,"continue from suspended call")
instruction(trgr,39,uAr,nOp,"trigger suspended calls")

/* Unification instructions */
instruction(uAA,42,iAh,iAm,"Unify argument registers")
instruction(uAY,43,iAh,iLc,"Unify")
instruction(uAS,45,iAh,iSt,"Unify")
instruction(ucAS,46,iAh,iSt,"Unify with occurs check")
instruction(uAlit,47,iAh,ltl,"Unify with literal")
instruction(uAcns,50,iAh,ltl,"Unify with constructor")
instruction(uYY,51,iLh,iLm,"Unify Y[h],Y[m]")
instruction(uYS,52,iLc,iSt,"Unify")
instruction(ucYS,53,iLc,iSt,"Unify with occurs check")
instruction(uSlit,60,iSt,ltl,"Unify literal")
instruction(uScns,63,iSt,ltl,"Unify constructor")

instruction(uAcns0,55,ltl,nOp,"Unify A[0] with constructor")
instruction(uAcns1,56,ltl,nOp,"Unify A[1] with constructor")
instruction(uAcns2,57,ltl,nOp,"Unify A[2] with constructor")
instruction(uAcns3,58,ltl,nOp,"Unify A[3] with constructor")
instruction(uAcns4,59,ltl,nOp,"Unify A[4] with constructor")


// build instructions
instruction(mAA,70,oAh,iAm,"Move")
instruction(mAY,71,oAh,iLc,"Move")
instruction(muAY,72,oAh,iLc,"Move unsafe")
instruction(mAS,74,oAh,iSt,"Move")
instruction(mAlit,75,oAh,ltl,"Move literal")
instruction(mAcns,76,oAh,ltl,"Build constructor ")
instruction(mYA,77,oLc,iAh,"Move")
instruction(mYY,78,oLh,iLm,"Move")
instruction(mYS,80,oLc,iSt,"Move")
instruction(mSA,83,oSt,iAh,"Move")
instruction(mSY,84,oSt,iLc,"Move")
instruction(mSlit,85,oSt,ltl,"Move literal")
instruction(mScns,88,oSt,ltl,"Build constructor")

// binding (assign) instructions
instruction(oAU,90,iAh,nOp,"Unbind A[h]")
instruction(oYU,91,iLc,nOp,"Unbind local")
instruction(oYA,92,iLc,iAh,"Overwrite local")
instruction(oYnil,93,iLc,nOp,"Overwrite with empty list")

/* Matching, i.e., non-binding instructions */
instruction(cAA,110,iAh,iAm,"Match")
instruction(cAY,111,iAh,iLc,"Match")
instruction(cAS,112,iAh,iSt,"Match")
instruction(cAlit,113,iAh,ltl,"Match literal")
instruction(cAcns,116,iAh,ltl,"Match constructor")
instruction(cYA,117,iLc,iAh,"Match")
instruction(cYS,118,iLc,iSt,"Match")
instruction(cSA,121,iSt,iAm,"Match")
instruction(cSY,122,iSt,iLc,"Match")
instruction(cSlit,123,iSt,ltl,"Match literal")
instruction(cScns,126,iSt,ltl,"Match constructor")

instruction(cAcns0,128,ltl,nOp,"Match constructor in A[0]")
instruction(cAcns1,129,ltl,nOp,"Match constructor in A[1]")
instruction(cAcns2,130,ltl,nOp,"Match constructor in A[2]")
instruction(cAcns3,131,ltl,nOp,"Match constructor in A[3]")
instruction(cAcns4,132,ltl,nOp,"Match constructor in A[4]")

/* First time initialization/clear registers */
instruction(clAA,160,oAh,oAm,"First/clear")
instruction(clAY,161,oAh,oLc,"First/clear")
instruction(clAS,163,oAh,oSt,"First/clear")

instruction(clSA,164,oSt,oAm,"First/clear")
instruction(clSY,165,oSt,oLc,"First/clear")

instruction(vrA,166,iAh,nOp,"Test for variable")
instruction(vrY,167,iLc,nOp,"Test for variable")

instruction(nvrA,168,iAh,nOp,"Test for non-variable")
instruction(nvrY,169,iLc,nOp,"Test for non-variable")

instruction(vdA,180,oAh,nOp,"Void A[h]")
instruction(vdAA,181,oAh,Ltl,"Void A[h],Count")
instruction(vdY,182,oLc,nOp,"Void Y[X]")
instruction(vdYY,183,oLc,uLt,"Void Y[X],Count}")
instruction(clA,184,oAh,nOp,"Clear A[h]")
instruction(clY,185,oLc,nOp,"Clear Y[X]")
instruction(clS,187,oSt,nOp,"Clear S++")

lastInstruction
instruction(clYY,188,oLc,uLt,"Clear Y[X],Count")

