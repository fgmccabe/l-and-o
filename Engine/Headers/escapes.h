/* 
  This is where you define a new escape function so that the compiler and
  the run-time system can see it
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.

 */

/* Declare standard symbols and constructors */

#define processState "U'#processState'\0"
#define errorType "U'#exception'\0"
#define threadType "U'#thread'\0"
#define thingType "U'#thing'\0"
#define fileType "U'go.io#fileType'\0"
#define filePerm "U'go.io#filePerm'\0"
#define debugType "U'go.debug#debugger'\0"
#define tplType "U'#,'\2"

  /* Define the standard escapes */
  escape(exit,0,True,False,"p\1+i","terminate go engine")
  escape(_command_line,1,False,False,"F\0LS","command line arguments")
  escape(_command_opts,3,False,False,"F\0L"tplType"cs","command line options")

  escape(_unify,4,False,False,":\0AP\2?$\0?$\0","unification")
  escape(_identical,5,False,False,":\0AP\2+$\0+$\0","test for identicality")
  escape(_match,6,False,False,":\0AP\2+$\0?$\0","matching")

  escape(var,7,False,False,":\1AP\1+$\1","test for variable")
  escape(nonvar,8,False,False,":\1AP\1+$\1","test for non-variable")

  escape(_errorcode,10,False,False,"F\1+sS","decode error symbol")

  escape(_call,11,True,False,"p\4+s+s+i+LS","dynamic call")

  escape(_defined,12,True,False,"P\3+s+s+i","test for defined name")

  escape(_plus,13,False,False,":\1NF\2+$\1+$\1$\1","add two numbers")
  escape(_minus,14,False,False,":\1NF\2+$\1+$\1$\1","subtract two numbers")
  escape(_times,15,False,False,":\1NF\2+$\1+$\1$\1","multiply two numbers")
  escape(_div,16,False,False,":\1NF\2+$\1+$\1f","divide two numbers")
  escape(iplus,17,False,False,"F\3+i+i+ii","modulo addition")
  escape(iminus,18,False,False,"F\3+i+i+ii","modulo subtraction")
  escape(itimes,19,False,False,"F\3+i+i+ii","modulo multiplication")
  escape(idiv,20,False,False,"F\3+i+i+ii","modulo division")
  escape(imod,21,False,False,"F\2+i+ii","modulo remainder")
  escape(quot,22,False,False,":\1NF\2+$\1+$\1i","integer quotient")
  escape(rem,23,False,False,":\1NF\2+$\1+$\1f","remainder")
  escape(abs,24,False,False,":\1NF\1+$\1$\1","absolute value")
  escape(_power,25,False,False,":\1NF\2+$\1+$\1$\1","raise X to the power Y")
  escape(sqrt,26,False,False,":\1NF\1+$\1f","square root")
  escape(exp,27,False,False,":\1NF\1+$\1$\1","exponential")
  escape(log,28,False,False,":\1NF\1+$\1f","logarithm")
  escape(log10,29,False,False,":\1NF\1+$\1f","10-based logarithm")
  escape(pi,30,False,False,"F\0f","return PI")
  escape(sin,31,False,False,":\1NF\1+$\1f","sine")
  escape(cos,32,False,False,":\1NF\1+$\1f","cosine")
  escape(tan,33,False,False,":\1NF\1+$\1f","tangent")
  escape(asin,34,False,False,":\1NF\1+$\1f","arc sine")
  escape(acos,35,False,False,":\1NF\1+$\1f","arc cosine")
  escape(atan,36,False,False,":\1NF\1+$\1f","arc tangent")

  escape(srand,37,False,False,":1Np\1+$\1","set random seed")
  escape(rand,38,False,False,":\1NF\1+$\1f","random # generator")
  escape(irand,39,False,False,"F\1+ii","generate random integer")

  escape(ldexp,40,False,False,"F\2+N+Nf","raise x to 2**y")
  escape(frexp,41,False,False,"P\3+f-f-i","split x into mant and exp")
  escape(modf,42,False,False,"P\3+f-f-f","split x into int and frac")

  escape(band,43,False,False,"F\2+i+ii","bitwise and two numbers")
  escape(bor,44,False,False,"F\2+i+ii","bitwise or two numbers")
  escape(bxor,45,False,False,"F\2+i+ii","bitwise xor two numbers")
  escape(bleft,46,False,False,"F\2+i+ii","bitwise left shift")
  escape(bright,47,False,False,"F\2+i+ii","bitwise right shift")
  escape(bnot,48,False,False,"F\1+ii","bitwise negate number")

  escape(trunc,49,False,False,":\1NF\1+$\1$\1","truncate to nearest integer")
  escape(floor,50,False,False,":\1NF\1+$\1$\1","truncate to lower integer")
  escape(ceil,51,False,False,"F\1+Ni","truncate to next integer")
  escape(itrunc,52,False,False,"F\1+Ni","truncate to 64 bit integer")
  escape(integral,53,False,False,"P\1+N","test if number is integral")
  escape(n2float,54,False,False,":\1NF\1+$\1f","float a number")
  
  escape(_less,55,False,False,":\0AP\2+$\0+$\0","compare terms")
  escape(_leq,56,False,False,":\0AP\2+$\0+$\0","compare terms")
  escape(_gt,57,False,False,":\0AP\2+$\0+$\0","compare terms")
  escape(_geq,58,False,False,":\0AP\2+$\0+$\0","compare terms")
  
  escape(ground,59,False,False,":\0AP\1+$\0","test for grounded")

  escape(_suspend,64,False,False,":\1A:\2AP\2+$\1+$\2","suspend if variable not bound")

  escape(_assert,70,False,False,":\1Ap\2+s+$\1","assert a term")
  escape(_retract,71,False,False,"p\1+s","remove assertion")

  escape(_term,75,False,False,":\1AF\1+$\1s","define an assertion")
  escape(_is,76,False,False,":\1AP\2+s+$\1","invoke an assertion")
  escape(_remove,77,False,False,"p\1+s","retract a definition")

  // Create a new object -- clone a term to make an object
  escape(_newObject,80,False,False,":\1AF\1+$\1$\1","create a new object")

  // Property management
 // escape(_setProp,81,False,False,":\1AP\3+"thingType"+s+$\1","set a property on a symbol")
 // escape(_getProp,82,False,False,":\1AP\3+"thingType"+s-$\1","get a symbol property")
 // escape(_delProp,83,False,False,":\1AP\2+"thingType"+s","delete a property from a symbol")

  // Term construction
  escape(_univ,84,False,False,":\1A:\2AF\2+s+L$\1$\2","weird function to construct terms")  

  // Lock management
  escape(_acquireLock,91,False,False,":\0AP\2+$\0+N","acquire lock")
  escape(_waitLock,92,False,False,":\0AP\2+$\0+N","release and wait on a lock")
  escape(_releaseLock,93,False,False,":\0AP\1+$\0","release a lock")

  // Sha function
  escape(_sha1,102,False,False,"F\1+LiLi","compute hash of a byte string")

  escape(_openURL,110,True,False,"F\4+S+S+S+iO","open a URL")
  escape(_openInFile,111,True,False,"F\2+S+iO","open input file")
  escape(_openOutFile,112,True,False,"F\2+S+iO","open output file")
  escape(_openAppendFile,113,True,False,"F\2+S+iO","open output file")
  escape(_openAppendIOFile,114,True,False,"F\2+S+iO","open output file")
  escape(_checkRoot,115,True,False,"P\2+S+S","check url against root URL")
  escape(_mergeURL,116,True,False,"F\2+S+SS","merge URLs")
  escape(_createURL,117,True,False,"F\4+S+S+S+iO","create a URL")
  escape(_popen,118,True,False,"p\7+S+LS+L"tplType"sS+O+O+O+i","open a pipe")
  
  escape(_close,119,True,False,"p\1+O","close file")
  escape(_eof,120,True,False,"P\1+O","end of file test")
  escape(_ready,121,True,False,"P\1+O","file ready test")
  escape(_inchars,122,True,False,"F\2+O+iS","read block string")
  escape(_inbytes,123,True,False,"F\2+O+iLi","read block of bytes")
  escape(_inchar,124,True,False,"F\1+Oc","read single character")
  escape(_inbyte,125,True,False,"F\1+Oi","read single byte")
  escape(_inline,126,True,False,"F\2+O+SS","read a line")
  escape(_intext,127,True,False,"F\2+O+SS","read until matching character")
  escape(_outch,128,True,False,"p\2+O+i","write a single character")
  escape(_outbyte,129,True,False,"p\2+O+i","write a single byte")
  escape(_outtext,130,True,False,"p\2+O+S","write a string as a block")
  escape(_outsym,131,True,False,"p\2+O+s","write a symbol")
  escape(_stdfile,132,True,False,"F\1+iO","standard file descriptor")
  escape(_fposition,135,True,False,"F\1+Oi","report current file position")
  escape(_fseek,136,True,False,"p\2+O+i","seek to new file position")
  escape(_flush,137,True,False,"p\1+O","flush the I/O buffer")
  escape(_flushall,138,True,False,"p\0","flush all files")
  escape(_setfileencoding,140,True,False,"p\2+O+i", "set file encoding on file")
    
  escape(_classload,141,True,False,"p\4+S+s+s-Ls","load class file")

  escape(_logmsg,142,False,False,"P\1+S","log a message in logfile")
  
  /* Socket handling functions */
  escape(_connect,143,True,False,"p\5+S+i+i-O-O","connect to remote host")
  escape(_listen,144,True,False,"p\2+i-O","listen on a port")
  escape(_accept,145,True,False,"p\7+O-O-O-S-S-i+i","accept connection")
  //  escape(_udpPort,146,True,False,"p\2NO","estabish a UDP port")
  //  escape(_udpGet,147,True,False,"p\4OSSN","read a UDP datagram")
  //  escape(_udpSend,148,True,False,"p\4OSSN","send a UDP datagram")
  escape(hosttoip,150,False,False,"F\1+SLS","IP address of host")
  escape(iptohost,151,False,False,"F\1+SS","host name from IP")
  
  escape(_cwd,152,False,False,"F\0S","return current working directory")
  escape(_cd,153,False,False,"p\1+S","change current working directory")
  escape(_rm,154,True,False,"p\1+S","remove file")
  escape(_mv,155,True,False,"p\2+S+S","rename file")
  escape(_mkdir,156,True,False,"p\2+S+i","create directory")
  escape(_rmdir,157,True,False,"p\1+S","delete directory")
  escape(_chmod,158,True,False,"p\2+S+i","change mode of a file or directory")
  escape(_ls,159,True,False,"F\1+SL"tplType"S"fileType"","report on contents of a directory")
  escape(_file_present,160,True,False,"P\1+S","test for file")
  escape(_file_mode,161,True,False,"F\1+Si","report modes of a file")
  escape(_file_type,162,True,False,"F\1+S"fileType,"report on the type of a file")
  escape(_file_size,163,True,False,"F\1+Si","report on the size of a file")
  escape(_file_date,164,True,False,"p\4+S-f-f-f","report on file access time and modification times")

/* Timing and delaying */
  escape(delay,170,False,False,"p\1+N","delay for period of time")  
  escape(sleep,171,False,False,"p\1+N","sleep until a definite time")  
  escape(now,172,False,False,"F\0f","current time")  
  escape(today,173,False,False,"F\0i","time at midnight")  
  escape(ticks,174,False,False,"F\0f","used CPU time")
  escape(_time2date,175,False,False,"P\012+N-i-i-i-i-i-i-f-f-S", "convert a time to a date")
  escape(_time2utc,176,False,False,"P\012+N-i-i-i-i-i-i-f-f-S", "convert a time to UTC date")
  escape(_date2time,177,False,False,"F\007+i+i+i+i+i+N+Nf", "convert a date to a time")
  escape(_utc2time,178,False,False,"F\010+N+N+N+N+N+N+N+Nf", "convert a UTC date to a time")

 /* Character class escapes */

  escape(_isCcChar,180,False,False,"P\1+i","is Other, control char")
  escape(_isCfChar,181,False,False,"P\1+i","is Other, format char")
  escape(_isCnChar,182,False,False,"P\1+i","is Other, unassigned char")
  escape(_isCoChar,183,False,False,"P\1+i","is Other, private char")
  escape(_isCsChar,184,False,False,"P\1+i","is Other, surrogate char")
  escape(_isLlChar,185,False,False,"P\1+i","is Letter, lowercase char")
  escape(_isLmChar,186,False,False,"P\1+i","is Letter, modifier char")
  escape(_isLoChar,187,False,False,"P\1+i","is Letter, other char")
  escape(_isLtChar,188,False,False,"P\1+i","is Letter, title char")
  escape(_isLuChar,189,False,False,"P\1+i","is Letter, uppercase char")
  escape(_isMcChar,190,False,False,"P\1+i","is Mark, spacing char")
  escape(_isMeChar,191,False,False,"P\1+i","is Mark, enclosing char")
  escape(_isMnChar,192,False,False,"P\1+i","is Mark, nonspacing char")
  escape(_isNdChar,193,False,False,"P\1+i","is Number, decimal digit")
  escape(_isNlChar,194,False,False,"P\1+i","is Number, letter char")
  escape(_isNoChar,195,False,False,"P\1+i","is Number, other char")
  escape(_isPcChar,196,False,False,"P\1+i","is Punctuation, connector")
  escape(_isPdChar,197,False,False,"P\1+i","is Punctuation, dash char")
  escape(_isPeChar,198,False,False,"P\1+i","is Punctuation, close char")
  escape(_isPfChar,199,False,False,"P\1+i","is Punctuation, final quote")
  escape(_isPiChar,200,False,False,"P\1+i","is Punctuation, initial quote")
  escape(_isPoChar,201,False,False,"P\1+i","is Punctuation, other char")
  escape(_isPsChar,202,False,False,"P\1+i","is Punctuation, open char")
  escape(_isScChar,203,False,False,"P\1+i","is Symbol, currency char")
  escape(_isSkChar,204,False,False,"P\1+i","is Symbol, modifier char")
  escape(_isSmChar,205,False,False,"P\1+i","is Symbol, math char")
  escape(_isSoChar,206,False,False,"P\1+i","is Symbol, other char")
  escape(_isZlChar,207,False,False,"P\1+i","is Separator, line char")
  escape(_isZpChar,208,False,False,"P\1+i","is Separator, para char")
  escape(_isZsChar,209,False,False,"P\1+i","is Separator, space char")

  escape(_isLetterChar,210,False,False,"P\1+i","is letter char")
  escape(_digitCode,211,False,False,"F\1+ii","convert char to num")

/* String and symbol handling escapes */
  escape(int2str,214,False,False,"F\4+i+N+N+iS","format an integer as a string")
  escape(num2str,215,False,False,"F\5+N+N+N+l+lS","format a number as a string")
  escape(_stringOf,216,False,False,":\0AF\3+$\0+i+iS","convert value to a string")
  escape(_trim,217,False,False,"F\2+S+iS","trim a string to a width")
  escape(explode,218,False,False,"F\1+SLi","convert string to list of code points")
  escape(implode,219,False,False,"F\1+LiS","convert list of code points to string")

  escape(getenv,230,False,False,"F\2+S+SS","get an environment variable")
  escape(setenv,231,True,False,"P\2+S+S","set an environment variable")
  escape(envir,232,False,False,"F\0L"tplType"sS","return entire environment")
  escape(getlogin,233,False,False,"F\0S","return user's login")

/* Process manipulation */
  escape(_fork,240,False,False,"p\1+"threadType,"fork new process")
  escape(_thread,241,False,False,"F\0"threadType"","report thread of current process")
  escape(kill,242,True,False,"p\1+"threadType ,"kill off a process")
  escape(thread_state,243,False,False,"F\1+"threadType processState"","state of process")
  escape(waitfor,244,False,False,"p\1+"threadType,"wait for other thread to terminate")
  escape(_assoc,246,False,False,":\0AP\2+$\0+p\0","associate a goal with a var")
  escape(_shell,247,True,False,"p\4+S+LS+L"tplType"sS-i","Run a shell cmd")

  escape(_ins_debug,254,False,False,"p\0","set instruction-level")
  escape(_stackTrace,255,False,False,"p\0","Print a stack trace")


#undef processState
#undef errorType
#undef threadType
#undef thingType
#undef fileType
#undef filePerm
#undef debugType
#undef tplType

#if 0
#ifdef GOXLIB
#include "xlibescapes.h"
#endif

#ifdef GOSSLIB
#include "sslEscapes.h"
#endif

#endif
