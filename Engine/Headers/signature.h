#ifndef _GO_TYPE_SIG_H_
#define _GO_TYPE_SIG_H_

/* Type signatures for Logic and Objects types */
typedef enum {
  anon_sig = '_', // Anonymous type
  integer_sig = 'i', /* An integer */
    float_sig = 'f', /* A float */
    string_sig = 'S', /* String */
    logical_sig = 'l', /* Logical value */
    kvar_sig = 'k', /* quantified type variable */
    kfun_sig = 'K', /* quantified type function */
    void_sig = 'v', /* Bottom type - no value has this type */
    type_sig = 't',                         /* A type */
    tpfun_sig = 'z', /* A type function */
    this_sig = 'h', // this type is a special type

  /* Compound type signatures */
    list_sig = 'L', /* List pair -- NULL = nil */
    tuple_sig = 'T', /* Tuple - followed by length byte */
    forall_sig = ':', /* universally quantified formula */
    constrained_sig = 'c',                /* A constrained type variable */

 /* Contract signatures */
    implements_sig = 'I',

/* signatures for user-defined types */
    poly_sig = 'U',                       /* polymorphic user type */

/* Code signatures */
    funct_sig = 'F', /* Function object signature */
    pred_sig = 'P', /* Predicate signature */
    grammar_sig = 'G', /* Grammar rule signature */

    face_sig = 'I', /* class interface type specification */

    class_sig = 'C', /* class type constructor */
    enu_sig = 'E',                        /* enumerated symbol */
} goTypeSig;

/* First Go! version */
#define SIGNATURE 0x01030507L  /* code signature */
#define SIGNBSWAP 0x03010705L  /* signature when we must swap bytes not words */
#define SIGNWSWAP 0x05070103L  /* signature to sap words only */
#define SIGNBWSWP 0x07050301L  /* when we have to swap words and bytes */

#endif
