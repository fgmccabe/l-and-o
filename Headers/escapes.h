/*
  This is where you define a new escape function so that the compiler and
  the run-time system can see it
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.

 */

/* Declare standard types used in escapes */

#define processState "t'lo.thread#processState'"
#define threadType "t'lo.thread#thread'"
#define lockType "t'lo.thread#lock'"
#define thingType "t'lo.core#thing'"
#define fileType "t'lo.io#fileHandle'"

/* Define the standard escapes */
escape(_exit,True,False,"P1i","terminate L&O engine")
escape(_command_line,False,False,"F0LS","command line arguments")
escape(_command_opts,False,False,"F0LT2SS","command line options")

escape(_unify,False,False,":k't'P2k't'k't'","unification")
escape(_identical,False,False,":k't'P2k't'k't'","test for identicality")

escape(var,False,False,":k't'P1k't'","test for variable")
escape(ground,False,False,":k't'P1k't'","test for grounded-ness")

escape(_call,True,False,"P3SSLS","dynamic call")
escape(_defined,True,False,"P2SS","test for defined name")

escape(_int_plus,False,False,"F2iii","add two integers")
escape(_int_minus,False,False,"F2iii","subtract two integers")
escape(_int_times,False,False,"F2iii","multiply two integers")
escape(_int_div,False,False,"F2iii","divide two integers")
escape(_int_mod,False,False,"F2iii","modulo remainder")

escape(_flt_plus,False,False,"F2fff","add two floats")
escape(_flt_minus,False,False,"F2fff","subtract two floats")
escape(_flt_times,False,False,"F2fff","multiply two floats")
escape(_flt_div,False,False,"F2fff","divide two floats")
escape(_flt_mod,False,False,"F2fff","modulo remainder")

escape(_int_abs,False,False,"F1ii","integer absolute value")
escape(_flt_abs,False,False,"F1ff","float absolute value")

escape(_int_lt,False,False,"P2ii","integer less than")
escape(_int_ge,False,False,"P2ii","integer greater or equal")

escape(_flt_lt,False,False,"P2ff","float less than")
escape(_flt_ge,False,False,"P2ff","float greater or equal")

escape(_int2flt,False,False,"F1if","convert integer to float")
escape(_flt2int,False,False,"F1fi","convert float to integer")

escape(_flt_hash,False,False,"F1fi","compute hash of float")

escape(_pwr,False,False,"F2fff","raise X to the power Y")

escape(sqrt,False,False,"F1ff","square root")
escape(exp,False,False,"F1ff","exponential")
escape(log,False,False,"F1ff","logarithm")
escape(log10,False,False,"F1ff","10-based logarithm")
escape(pi,False,False,"F0f","return PI")
escape(sin,False,False,"F1ff","sine")
escape(cos,False,False,"F1ff","cosine")
escape(tan,False,False,"F1ff","tangent")
escape(asin,False,False,"F1ff","arc sine")
escape(acos,False,False,"F1ff","arc cosine")
escape(atan,False,False,"F1ff","arc tangent")

escape(trunc,False,False,"F1ff","truncate to nearest integer")
escape(floor,False,False,"F1ff","truncate to lower integer")
escape(ceil,False,False,"F1ff","truncate to next integer")
escape(integral,False,False,"P1f","test if number is integral")

escape(srand,False,False,"P1f","set random seed")
escape(rand,False,False,"F0f","random # generator")
escape(irand,False,False,"F1ii","generate random integer")

escape(_ldexp,False,False,"F2fff","raise x to 2**y")
escape(_frexp,False,False,"P3ffi","split x into mant and exp")
escape(_modf,False,False,"P3fff","split x into int and frac")

escape(_band,False,False,"F2iii","bitwise and two integers")
escape(_bor,False,False,"F2iii","bitwise or two integers")
escape(_bxor,False,False,"F2iii","bitwise xor two integers")
escape(_blsl,False,False,"F2iii","logical left shift")
escape(_blsr,False,False,"F2iii","logical right shift")
escape(_basr,False,False,"F2iii","arithmetic right shift")
escape(_bnot,False,False,"F1ii","bitwise negate number")
escape(_nthb,False,False,"P2ii","is nth bit set?")

/*
  escape(_suspend,False,False,":k'u'A:k'v'AP2k'u'k'v'","suspend if variable not bound")

  escape(_assert,False,False,":k'u'Ap2sk'u'","assert a term")
  escape(_retract,False,False,"p1s","remove assertion")

  escape(_term,False,False,":k'u'AF1k'u's","define an assertion")
  escape(_is,False,False,":k'u'AP2sk'u'","invoke an assertion")
  escape(_remove,False,False,"p1s","retract a definition")

  // Create a new object -- clone a term to make an object
  escape(_newObject,False,False,":k'u'AF1k'u'k'u'","create a new object")

  // Property management
 // escape(_setProp,False,False,":k'u'AP3"thingType"sk'u'","set a property on a symbol")
 // escape(_getProp,False,False,":k'u'AP3"thingType"sk'u'","get a symbol property")
 // escape(_delProp,False,False,":k'u'AP2"thingType"s","delete a property from a symbol")

  // Term construction
  escape(_univ,False,False,":k'u'A:k'v'AF2sLk'u'k'v'","weird function to construct terms")

  // Sha function
  escape(_sha1,False,False,"F1LiLi","compute hash of a byte string")

*/
  escape(_get_file,True,False,"F1SS","Get the contents of a file as a string")
  escape(_put_file,True,False,"P2SS","write a file from a string")
  escape(_cwd,True,False,"F0S","return url of current working directory")
  escape(_cd,False,False,"P1S","change current working directory")
  escape(_rm,True,False,"P1S","remove file")
  escape(_mv,True,False,"P2SS","rename file")
  escape(_mkdir,True,False,"P2Si","create directory")
  escape(_rmdir,True,False,"P1S","delete directory")
  escape(_chmod,True,False,"P2Si","change mode of a file or directory")
  escape(_ls,True,False,"F1SLS","return a list of files in a directory")

  escape(_file_mode,True,False,"F1Si","report modes of a file")
  escape(_file_present,True,False,"P1S","check presence of a file")
  escape(_file_type,True,False,"F1Si","report on the type of a file")
  escape(_file_size,True,False,"F1Si","report on the size of a file")
  escape(_file_modified,True,False,"F1Sf","report on when a file was last modified")
  escape(_file_date,True,False,"P4Siii","report on file access time and modification times")

  escape(_openInFile,True,False,"F2Si"fileType,"open input file")
  escape(_openOutFile,True,False,"F2Si"fileType,"open output file")
  escape(_openAppendFile,True,False,"F2Si"fileType,"open output file")
  escape(_openAppendIOFile,True,False,"F2Si"fileType,"open output file")
 /*
  escape(_openURL,True,False,"F4SSSiO","open a URL")
  escape(_checkRoot,True,False,"P2SS","check url against root URL")
  escape(_mergeURL,True,False,"F2SSS","merge URLs")
  escape(_createURL,True,False,"F4SSSiO","create a URL")
  escape(_popen,True,False,"p7SLSLT2sSOOOi","open a pipe")
  */
  escape(_close,True,False,"P1"fileType,"close file")
  escape(_end_of_file,True,False,"P1"fileType,"end of file test")
  escape(_ready,True,False,"P1"fileType,"file ready test")
  escape(_inchars,True,False,"F2"fileType"iS","read block string")
  escape(_inbytes,True,False,"F2"fileType"iLi","read block of bytes")
  escape(_inchar,True,False,"F1"fileType"i","read single character")
  escape(_inbyte,True,False,"F1"fileType"i","read single byte")
  escape(_inline,True,False,"F2"fileType"SS","read a line")
  escape(_intext,True,False,"F2"fileType"SS","read until matching character")
  escape(_outch,True,False,"P2"fileType"i","write a single character")
  escape(_outbyte,True,False,"P2"fileType"i","write a single byte")
  escape(_outtext,True,False,"P2"fileType"S","write a string as a block")
  escape(_stdfile,True,False,"F1i"fileType,"standard file descriptor")
  escape(_fposition,True,False,"F1"fileType"i","report current file position")
  escape(_fseek,True,False,"P2"fileType"i","seek to new file position")
  escape(_flush,True,False,"P1"fileType,"flush the I/O buffer")
  escape(_flushall,True,False,"P0","flush all files")
  escape(_setfileencoding,True,False,"P2"fileType"i", "set file encoding on file")

  escape(_ensure_loaded,True,False,"F2SSLT2SS","load class file")

  escape(_logmsg,False,False,"P1S","log a message in logfile or console")

  /*
  // Socket handling functions
  escape(_connect,True,False,"p5SiiOO","connect to remote host")
  escape(_listen,True,False,"p2iO","listen on a port")
  escape(_accept,True,False,"p7OOOSSii","accept connection")
  escape(_udpPort,True,False,"p2NO","estabish a UDP port")
  escape(_udpGet,True,False,"p4OSSN","read a UDP datagram")
  escape(_udpSend,True,False,"p4OSSN","send a UDP datagram")
  escape(hosttoip,False,False,"F1SLS","IP address of host")
  escape(iptohost,False,False,"F1SS","host name from IP")
*/


// Timing and delaying
  escape(delay,False,False,"P1f","delay for period of time")
  escape(sleep,False,False,"P1f","sleep until a definite time")
  escape(now,False,False,"F0f","current time")
  escape(today,False,False,"F0f","time at midnight")
  escape(ticks,False,False,"F0f","used CPU time")
  escape(_time2date,False,False,"P10fiiiiiifii", "convert a time to a date")
  escape(_time2utc,False,False, "P10fiiiiiifii", "convert a time to UTC date")
  escape(_date2time,False,False,"F7iiiiifif", "convert a date to a time")
  escape(_utc2time,False,False,"F7iiiiifif", "convert a UTC date to a time")

 // Character class escapes

  escape(_isCcChar,False,False,"P1i","is Other, control char")
  escape(_isCfChar,False,False,"P1i","is Other, format char")
  escape(_isCnChar,False,False,"P1i","is Other, unassigned char")
  escape(_isCoChar,False,False,"P1i","is Other, private char")
  escape(_isCsChar,False,False,"P1i","is Other, surrogate char")
  escape(_isLlChar,False,False,"P1i","is Letter, lowercase char")
  escape(_isLmChar,False,False,"P1i","is Letter, modifier char")
  escape(_isLoChar,False,False,"P1i","is Letter, other char")
  escape(_isLtChar,False,False,"P1i","is Letter, title char")
  escape(_isLuChar,False,False,"P1i","is Letter, uppercase char")
  escape(_isMcChar,False,False,"P1i","is Mark, spacing char")
  escape(_isMeChar,False,False,"P1i","is Mark, enclosing char")
  escape(_isMnChar,False,False,"P1i","is Mark, nonspacing char")
  escape(_isNdChar,False,False,"P1i","is Number, decimal digit")
  escape(_isNlChar,False,False,"P1i","is Number, letter char")
  escape(_isNoChar,False,False,"P1i","is Number, other char")
  escape(_isPcChar,False,False,"P1i","is Punctuation, connector")
  escape(_isPdChar,False,False,"P1i","is Punctuation, dash char")
  escape(_isPeChar,False,False,"P1i","is Punctuation, close char")
  escape(_isPfChar,False,False,"P1i","is Punctuation, final quote")
  escape(_isPiChar,False,False,"P1i","is Punctuation, initial quote")
  escape(_isPoChar,False,False,"P1i","is Punctuation, other char")
  escape(_isPsChar,False,False,"P1i","is Punctuation, open char")
  escape(_isScChar,False,False,"P1i","is Symbol, currency char")
  escape(_isSkChar,False,False,"P1i","is Symbol, modifier char")
  escape(_isSmChar,False,False,"P1i","is Symbol, math char")
  escape(_isSoChar,False,False,"P1i","is Symbol, other char")
  escape(_isZlChar,False,False,"P1i","is Separator, line char")
  escape(_isZpChar,False,False,"P1i","is Separator, para char")
  escape(_isZsChar,False,False,"P1i","is Separator, space char")

  escape(_isLetterChar,False,False,"P1i","is letter char")
  escape(_digitCode,False,False,"F1ii","convert char to num")

// String handling escapes
  escape(_int2str,False,False,"F4iiiiS","format an integer as a string")
  escape(_flt2str,False,False,"F5fiillS","format a floating as a string")
  escape(_str2flt,False,False,"F1Sf","parse a string as a float")

  escape(_str_lt,False,False,"P2SS","String 1 is less than string 2")
  escape(_str_ge,False,False,"P2SS","String 1 is greater than or equals to string 2")

  escape(_str_hash,False,False,"F1Si","Compute hash of string")
  escape(_str_len,False,False,"F1Si","return length of string")

  escape(_str_gen,False,False,"F1SS","Generate a unique string")

  escape(_stringOf,False,False,":k't'F3k't'iiS","Display a general term")
  escape(_trim,False,False,"F2SiS","trim a string to a width")

  escape(explode,False,False,"F1SLi","convert string to list of code points")
  escape(implode,False,False,"F1LiS","convert list of code points to string")

  escape(_str_find,False,False,"P4SSii","find a substring in string")
  escape(_sub_str,False,False,"F3SiiS","extract a substring")
  escape(_str_split,False,False,"P4SiSS","split a string at a point")
  escape(_str_concat,False,False,"F2SSS","Concatenate two strings")
  escape(_str_start,False,False,"P2SS","True if second string starts with first")

  escape(getenv,False,False,"F2SSS","get an environment variable")
  escape(setenv,True,False,"P2SS","set an environment variable")
  escape(envir,False,False,"F0LT2SS","return entire environment")
  escape(getlogin,False,False,"F0S","return user's login")
/*
/ Process manipulation
  escape(_fork,False,False,"p1"threadType,"fork new process")
  escape(_thread,False,False,"F0"threadType"","report thread of current process")
  escape(kill,True,False,"p1"threadType ,"kill off a process")
  escape(thread_state,False,False,"F1"threadType processState"","state of process")
  escape(waitfor,False,False,"p1"threadType,"wait for other thread to terminate")
  escape(_assoc,False,False,":k't'AP2k't'p0","associate a goal with a var")
  escape(_shell,True,False,"p4SLSLT2SSi","Run a shell cmd")

  // Lock management
  escape(_acquireLock,False,False,":k't'AP2k't'N","acquire lock")
  escape(_waitLock,False,False,":k't'AP2k't'N","release and wait on a lock")
  escape(_releaseLock,False,False,":k't'AP1k't'","release a lock")
*/
  escape(_ins_debug,False,False,"P0","set instruction-level")
  escape(_stackTrace,False,False,"P0","Print a stack trace")


#undef processState
#undef threadType
#undef thingType
#undef fileType
