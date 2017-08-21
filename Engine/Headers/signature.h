#ifndef _LO_TYPE_SIG_H_
#define _LO_TYPE_SIG_H_

/* Type signatures for Logic and Objects types */
typedef enum {
  anon_sig = '_', // Anonymous type
  this_sig = 'h', // this type is a special type
  integer_sig = 'i', /* An integer */
  float_sig = 'f', /* A float */
  string_sig = 'S', /* String */
  logical_sig = 'l', /* Logical value */
  kvar_sig = 'k', /* quantified type variable */
  kfun_sig = 'K', /* quantified type function */
  void_sig = 'v', /* Bottom type - no value has this type */
  type_sig = 't',                         /* A named type */
  tpfun_sig = 'z', /* A type function */

  list_sig = 'L', /* List pair -- NULL = nil */
  tuple_sig = 'T', /* Tuple - followed by length byte */
  univ_sig = ':', /* universally quantified formula */
  constrained_sig = '|',                /* A constrained type variable */

  /* Contract signatures */

  typeexp_sig = 'U',                       /* polymorphic user type */

  func_sig = 'F', /* Function object signature */
  pred_sig = 'P', /* Predicate signature */
  grammar_sig = 'G', /* Grammar rule signature */

  face_sig = 'I', /* class interface type specification */

  class_sig = 'C', /* class type constructor */
} loTypeSig;

typedef enum{
  univ_con = ':',
  contract_sig = 'c',
  implements_sig = 'a',
  constrained_constraint = '|'
} constraintSig;

typedef enum {
  input_mode = '+',
  output_mode = '-',
  bi_mode = '?'
} typeModeSig;

/* First L&O version */
// First four bytes of any code sequence must be this magic number
#define SIGNATURE 0x01030507L  /* code signature */
#define SIGNBSWAP 0x03010705L  /* signature when we must swap bytes not words */
#define SIGNWSWAP 0x05070103L  /* signature to sap words only */
#define SIGNBWSWP 0x07050301L  /* when we have to swap words and bytes */

#endif
