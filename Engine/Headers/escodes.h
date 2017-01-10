#ifndef _ESCODES_H_
#define _ESCODES_H_

#undef escape
#define escape(name,secr,pr,spec,cmnt) \
 Esc##name,

typedef enum {
#include "escapes.h"
  Esc_None
} EscapeCode;

#define escapeOpCode(name) (Esc_##name)

#undef escape
#define escape(name,secr,pr,spec,cmnt) \
extern retCode g_##name(processPo,ptrPo);

#include "escapes.h"
#undef escape

#endif
