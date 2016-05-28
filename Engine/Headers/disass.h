/*
 * disass.h
 *
 *  Created on: May 19, 2016
 *      Author: fgm
 */

#ifndef ENGINE_HEADERS_DISASS_H_
#define ENGINE_HEADERS_DISASS_H_

#include "go.h"

void showInstructions(codePo code,insPo pc,unsigned long count);
insPo dissass(byte *pref, codePo code, insPo pc, ptrPo a, ptrPo y, ptrPo S, rwmode mode, choicePo B,
    ptrPo hBase, ptrPo hLimit);

#endif /* ENGINE_HEADERS_DISASS_H_ */
