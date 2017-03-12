#ifndef _LO_DEBUG_H_
#define _LO_DEBUG_H_

/* Declarations of the debugger hooks */ 

extern logical SymbolDebug;

#ifdef EXECTRACE
extern logical debugging;	/* Level of debugging */
#endif

extern long cmdCounter;
extern logical tracing;	        /* do we show each instruction */

void showReg(ptrPo a,char *name,integer reg);

retCode
debug_stop(processPo p, ptrI prog, insPo pc, ptrI cprog, insPo cpc, ptrPo a, ptrPo y, ptrPo S, long Svalid, rwmode mode,
           callPo C, choicePo B, choicePo SB, choicePo T);

retCode breakPoint(processPo p);

#endif
