#ifndef _LO_DEBUG_H_
#define _LO_DEBUG_H_

/* Declarations of the debugger hooks */ 

extern logical SymbolDebug;

#ifdef EXECTRACE
extern logical debugging;	/* Level of debugging */
#endif

typedef enum {
  nextIns, nextSucc, nextFail, nextBreak
} DebugWaitFor;

extern long cmdCounter;
extern DebugWaitFor waitingFor; /* waiting for next instruction */
extern logical tracing;	        /* do we show each instruction */

extern byte debuggerPrefix[MAX_SYMB_LEN];

void showReg(ptrPo a,char *name,integer reg);

retCode debug_stop(processPo p,ptrI prog,insPo pc,ptrI cprog,insPo cpc,ptrPo a,ptrPo y,
		   ptrPo S,long Svalid,rwmode mode,
                   choicePo B,choicePo SB,choicePo T,
		   ptrPo hBase,ptrPo H,
		   trailPo trail,
		   ptrI prefix);
		   

#endif
