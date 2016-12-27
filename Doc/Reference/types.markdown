# Types

L&O is a statically checked strongly typed language. 
Strong typing means that every variable and every expression has a single type associated with it, and that the uses of these expressions are consistent with expectations. Static type checking means simply that types are checked at compile-time rather than at run-time.  Type errors arise when type checking detects an inconsistency. For example, if a function `foo` is defined over lists, then passing a numeric valued expression to `foo` is inconsistent because no list is equal to any number. 
The type system in L&O is very rich and contains some fairly advanced typing concepts; most notably the concepts of `contract`s and `implementation`. These are related to similar concepts found in OO languages, except that they are not tied to the class system of the language.

## L&O's type language

L&O's type language is founded on four key concepts. The \firstterm{type expression}{A type expression is a term that _denotes_ a type. A simple type expression would be something like the symbol `char` — which denotes the type of character expressions. A more complex example would be `list[char]` which denotes lists of characters — i.e., strings.} is a term that denotes a type. However, although logically type terms are terms, they not normal logical terms in that they are not manipulable by L&O programs: their meaning and existence is strictly compile-time. To help distinguish type terms from regular terms we use square brackets for type constructor terms — as opposed to round parentheses for normal constructor terms.

Type terms are related to each other by the _sub-type_ relation: which represents a partial ordering on type terms. The sub-type relation is indicated by the programmer explicitly declaring which type terms are sub-types of other type terms — as part of type definition statements.

The third key concept is the \firstterm{type interface}{An interface is associated with a type that defines the legal operations on values of that type. More specifically, the interface defines the expressions possible on the right hand side of a `dot' expression.}. A type interface defines the set of queries and operations that may be performed relative to a labeled theory — specifically, it defines the kinds of`dot' references that are supported by a given type of value. All named types, including system types, may support an interface. If the type term defines what kind of value an expression has, the type interface defines to a large extent what you can do with the value.

Finally, \firstterm{type inference}{Type inference is the process by which a type expression can be _automatically_ assigned to an identifier or expression _without_ requiring that the type of the identifier be explicitly declared.} is used to reduce the burden of annotating a program with type expressions. Programs and top-level names are _declared_, but type inference is used to verify that programs _conform_ to the declared type, and is also used to automatically determine the types of rule argument variables and pattern variables; a great saving in a rule-oriented language.

Every expression in a L&O program is associated with a type term which is called the expression's \firstterm{type assignment}{A type assignment of an expression is a mapping from the expression to a type term.} or _type denotation_. At the start of the type inference process all unknown identifiers are assigned an unbound type variable as their type.  Type inference uses declared program types, and the types of the program's literal values, to infer a type value or  a set of consistent sub-type constraints for each of these type variables. 

In addition to the type assignment, there are a number of \firstterm{type constraints}{a type constraint is a predicate that must be satisfied if the program is to be _type safe_.}. Type constraints encode the rules for type safety in programs. There are two kinds of type constraints, type _inequality_ constraints that reflect the sub-type relationship and _program_ constraints that reflect the program being type checked.
Typically, program constraints are constraints on the arguments of functions and other programs; for example, that the type of a function argument is the declared argument type, or a sub-type of the declared type. A program is _type safe_ if there is a single consistent type assignment for all the identifiers in the program and the set of all the inferred type predicates are consistent.

###Sub-type constraint

A sub-type constraint is a statement of the form:
\begin{alltt}
T \impl Tp
\end{alltt}
where `T` is a named type and `Tp` is a named type or a type interface. This statement declares that `T` is a sub-type of `Tp`. For example, the statements:
\begin{alltt}
student \impl person.
marriedStudent \impl student.
\end{alltt}
declare that `student` is a sub-type of `person`, and that `marriedStudent` is a sub-type of `student` and hence of `person`. So the constraint:
\begin{alltt}
T \impl person
\end{alltt}
is satisfied if `T` is `person`, `student` or `marriedStudent` — or some other sub-type of the `person` type.

In the case that `Tp` is an interface, as in, for example,
\begin{alltt}
person \impl \{ age:[]=\>integer\}
\end{alltt}
then this means that the `person` type implements an interface that includes an `integer`-valued function `age`.
\begin{aside}
Note how we use square brackets here for the list of arguments. This is to further reinforce the distinction between type expressions and normal terms.
\end{aside}

We can combine, for a given type, the two forms of type statement:
\begin{alltt}
student \impl \{ studies:[]=\>string\}.
student \impl person.
\end{alltt}
This means that `student` is a sub-type of `person` and that it also implements an interface with a `studies` function, the different interfaces are combined. Given only these two type statements, `student`'s full interface would be
\begin{alltt}
\{ studies:[]=\>string. age:[]=\>integer\}.
\end{alltt}

####A lattice of types
The set of types forms a \firstterm{type lattice}{a partial ordering with an identified `top` element that is bigger than all others, and a bottom element `void` that is smaller than all other elements.}. A lattice is simply a set with a partial ordering associated with it; together with a top element (`top` in L&O's case) which is larger than all other values and a bottom element (`void`) which is smaller than all others. A type lattice, as in figure\~\vref{type:lattice}, is a kind of lattice where the elements are type terms; and the partial ordering is, in fact, the _sub-type_ relation. 

\begin{figure}
\centerline{\includegraphics[width=\textwidth]{lattice}}
\caption{\label{type:lattice}Part of L&O's standard type lattice}
\end{figure}

In the sub-type partial ordering, higher in the order means more general and lower in the ordering means more specific. Thus `top` type is the most general type (and therefore the least is known about values of type `top`) and `void` is so specific that there are _no_ legal `void` values.

The significance of `top` and `void` is largely technical, however, a function that accepts `top` arguments will accept anything and a `void` value is acceptable for all functions. On the whole, if you see either `top` or `void` in a type expression in an error message, you are likely to be in trouble!

You will notice that L&O's type lattice is wide and shallow — that for the most part there are few significant \firstterm{chain}{A chain is a sequence of types where for each pair of types in the link _T\subi_ and _T\sub{i+1_} it  known that _T\subi_ is a subtype of _T\sub{i+1_}. All types are in a chain of at least three elements: `void`, the type itself and `top`.}s in the lattice. This is in the nature of type lattice systems. However, when defining types, particularly in terms of type inheritance, then we do get a richer network. For example, figure\~\vref{type:lattice:number} shows the lattice associated with the `number` type.

\begin{figure}
\centerline{\includegraphics{numberlattice}}
\caption{\label{type:lattice:number}L&O's `number` type lattice}
\end{figure}

This graph highlights the fact that a lattice is not necessarily a simple \firstterm{basket}{A basket type lattice is one where every chain is exactly three elements long; effectively meaning that there is no significant subtype relationship between non-trivial type elements.} or _chain_, but can have have branching elements in it. It cannot, however, have cycles — a lattice with a cycle is not permitted.

###Polymorphism
Types  may be \firstterm{polymorphic}{A type that is not completely ground — it may refer to many kinds of values.} and \firstterm{recursive}{A recursive type is one which may have components of the same type as the overall type.}. This polymorphism is reflected in the names of types — they can have type arguments. For example, the `char` type is the type of characters; and `list[char]` represents the type list  of `char`acters — i.e., `string`s.

The list type (`list[_type_]`) is polymorphic: it requires a type argument which is the type of the elements of the list. It is also recursive because the type of a component element of a list term — namely the tail of the list — is also of the same type as the whole list.  A recursive type is one whose values contain components that are of the same type as the whole. Although the `list[\ldots]` type is built-in to L&O, it is straightforward to define new recursive types. 

Recursive types have a particular hallmark: their type terms are _opaque_ — it is not possible to infer solely from the type expression what values of that type look like. For example, although a list is necessarily defined as a term which includes a list as a component (the tail of a list is also a list of the same type) that recursion is not itself reflected in the `list` type term. In contrast with this, type expressions for functions are _transparent_ — it is obvious from the type term what the structure of the values are — and conversely, transparent types may not be recursive. For tuples, that means, for example, that it is not possible to define a tuple type in L&O that has the same tuple type as itself as one of the elements of the tuple:
\begin{example}
\begin{boxed}
\begin{alltt}
( number, ( number, ( number, \ldots)))
\end{alltt}
\end{boxed}
\caption{\label{recursive:tuple}an impossible recursive tuple type}
\end{example}

## Type inference

L&O program expressions are only meaningful if a consistent type assignment or consistent sub-type constraints  can be inferred for them using the declared program and class types. The process of inferring a consistent sent of type assignments and sub-type constraints to a L&O program is called \firstterm{type inference}{A process for determining whether a program is type safe or not. A type-unsafe program _might_ be able to compute a value, but is likely to not be able to compute a valid value.}.

The fundamental predicate relating types to each other, and hence the key concept for understanding L&O's type inference system is the subtype relation. In presenting the sub-type relation we use the notation:
\begin{equation}
\subtype{T\sub{1}}{T\sub2}
\end{equation}
to denote that the type `_T\sub1`_ is a subtype of `_T\sub2`_. Implicit in this is always the possibility that the types `_T\sub1`_ and `_T\sub2`_ are _equal_.

As noted above, L&O's types form a lattice with more general types being higher in the lattice and more specific types lower in the lattice. 

To define the meaningful type associations we use a form of type inference rule notation. The general form of a type rule is:

\begin{prooftree}
\AxiomC{\mbox{_Condition_}}
\UnaryInfC{\typeprd{E}{X}{T}}
\end{prooftree}
where the intended reading is:
\begin{quote}
The type associated with the expression $X$, in the context of the environment $E$, is $T$ providing that _Condition_ holds. Often _Condition_ is a conjunction of other type constraints mentioning some of the same variables as in consequent.
\end{quote}
The environment $E$ is a sequence of pairs, of the form `(\mbox{_Name`_,\mbox{_Type_})}, which is used as a means of recording the type associated with the identifiers that may be in scope when assigning a type.

If we are inferring that one type is less than another, then we are likely to use an inference rule of the form:
\begin{prooftree}
\AxiomC{\mbox{_Condition_}}
\UnaryInfC{\subtype{T\sub{1}}{T\sub2}}
\end{prooftree}
since, in many cases, the subtype inference depends on preconditions being satisfied.

In addition to type associations and subtype determinations, we also need to make certain other kinds of type inference. For example, the \safegoal{E}{Goal} predicate is satisfied if its goal argument is type consistent in environment _E_. Other predicates we will need include \safeact{E}{Act} which is satisfied for an action that is type safe, \grammprd{E}{G}{S} which is satisfied in context $E$ if $G$ is a grammar over streams of type $S$, .

###Type variables
The process of type inference is complicated by the fact that it is often not known _what_ a particular type actually is. For example, during the inference process associated with the expression:
\begin{alltt}
O.f()
\end{alltt}
the type of `O` may not be known immediately. In which case the type inference process will be computing whether
\begin{alltt}
O\sub{t} \impl \{ f:[]=\>R\sub{t} \}
\end{alltt}
If the type of `O` is not known, then it must record this inequality against the type variable `O\sub{t`}; in case another step in the type inference process requires it or other constraints.

In general, in a type lattice, all type variables may be characterized by two types: an upper bound and a lower bound. The upper bound denotes that type which it known that the type variable is _lower_ than, and conversely the lower bound determines a _minimum_ type for the variable.

The type inference system goes one step further than this: it assumes that if the only thing known about two type variables is that one is greater than the other, it assumes that they are equal. A consequence of this is that the lower and upper bounds of a type variable are always known to be non-variable.

From a strict mathematical point of view, this is a strong inference that is not sanctioned by lattice theory: it is sound — in the sense that two such types may indeed be equal — but not complete, since it is quite possible for there to be a third type _between_ the upper and lower types.

However, in practice this is not nearly as big an issue as it might seem. The reason is that the most common situation where variable-variable subtyping arises is in recursive programs. Again, in such situations, the most common scenario is:
\begin{alltt}
V\sub1 \impl V\sub2 \impl \ldots V\sub1
\end{alltt}
I.e., all of the type variables are, in fact equal!

Finally, the effect of this restriction is safe; in the sense that potentially type correct programs will be rejected — but it will not permit a type incorrect program to be accepted. Such false rejections can normally be resolved by additional type annotations on the text of the program that eliminate the implicit uncertainty.

The benefits of this form of type inference are two-fold: the type inference procedure itself is highly efficient and always terminates; and, in the case of erroneous programs, the error messages displayed by the type inference system are bounded.\footnote{Both of these are serious concerns for non-trivial programs.}

## Standard Types
\label{types:types}
\index{types}
L&O has a range of built-in `atomic' types:`integer`,`float`,`char`and`symbol\`; and a range of built-in _type constructors_:
\begin{itemize}
\item
`list[_type`]_ (list of _type_), 
\item
`[_type\sub1`,\ldots,\emph{type\subn_]\{\}} (predicate type), 
\item
`[_type\sub1`,\ldots,\emph{type\subn_]=\>}_type_ (function type), 
\item
`[_type\sub1`,\ldots,\emph{type\subn_]--\>}_type_ (grammar type),
\item
`[_type\sub1`,\ldots,\emph{type\subn_]\*} (action type), and
\item
`\{ _field\sub1`:\emph{type\sub1_. \ldots _field\subn_:_type\subn_. \}} (type interface)
\end{itemize}

###Type variables
\label{types:standard:variable}

\index{type variable}
L&O does not use any special lexical markers to distinguish type variables from other variables, or even other type names — the scope of the identifier serves to distinguish the cases. An identifier `foo` occurring in a type expression will refer to a type name if a type definition for `foo` is 'in scope'; otherwise it refers to a type variable.

The normal scope rules do not apply for certain of L&O's built-in types; for example, the identifier `number` (say) is predefined in the language and always refers to the `number` type.

\paragraph{Universally quantified types}
\index{unversally qunatified types}
\index{type variable!universal}
A _universally_ quantified type is written using the notation:
\begin{alltt}
[s\sub1,\ldots,s\subn]-_Type_
\end{alltt}
this type binds the type variables s\subi{} occurring in the _Type_ expression. L&O supports nested quantification of type terms: if _Type_ contains a type expression that is itself quantified and binds any or all of the `s\subi` type variables then the inner occurrences refer to the inner-most quantification.

It is not normally required to manually identify the type variables in a type expression in this way; however, when type expressions are printed (in error messages typically) they will be displayed in fully quantified form.


###Standard value types
\label{types:standard}
\index{types!standard types}

Most of L&O's standard types are defined in a standard library: `go.stdlib`. This library, which also includes a number of utility definitions, is automatically `import`ed in every package.\footnote{It is possible, if not advised, to suppress this behavior with a special compiler option.}

####`thing` type
\label{types:standard:thing}

\index{thing@`thing` type}
\index{type!thing@`thing` type}

The `thing` type is the top of the value part of the type lattice — all values are a sub-type of `thing`. It is not the top of the type lattice itself — that is `top`. The program types — such as function type — are not a sub-type of `thing` — although they are sub-types of `top`.

The `thing` type has a simple type interface; it is declared to be:
\begin{alltt}
thing \impl \{ 
  show:[]=\>string.
  meta:[]=\>meta.
  \}.
\end{alltt}
The `show` function is used to compute a printable `string` display of a term and `meta` is used to compute a meta-term representation of a term. Note that different types of terms may well use different methods for displaying themselves; it is only the _type_ of `show` that is defined at this juncture.
  

####`number` type\\
\label{types:standard:number}

\index{number@`number` type}
\index{type! number@`number` type}
The `number` type symbol is used to denote the union type of integer and floating point values and expressions. The `number` type has the definition:
\begin{alltt}
number \impl thing.
\end{alltt}

####`integer` type
\label{types:standard:integer}

\index{integer@`integer` type}
\index{type!integer@`integer` type}
The `integer` type symbol is used to denote the type of integer values and expressions. The `integer` type has the definition:
\begin{alltt}
integer \impl number.
\end{alltt}

####`float` type
\label{types:standard:float}

\index{float@`float` type}
\index{type! float@`float` type}
The `float` type symbol is used to denote the type of floating point values and expressions. The `float` type has the definition:
\begin{alltt}
float \impl number.
\end{alltt}

\begin{aside}
  Note that there is no subtype relationship between `integer`s and `float`s. They are, however, both subtypes of the standard type `number`.  This can occasional quirks where it is necessary to specifically convert `integer` values to `float`s using the built-in function `n2float`.
\end{aside}

####`char` type
\label{types:standard:char}

\index{char@`char` type}
\index{type!char@`char` type}
The `char` type symbol is used to denote character values and expressions. The `char` type has the definition:
\begin{alltt}
char \impl thing.
\end{alltt}

####`symbol` type
\label{types:standard:symbol}
\index{symbol@`symbol` type}
\index{type!symbol@`symbol` type}

The `symbol` type symbol is use to denote symbol values and expressions. Note that this type refers to the \`general' class of symbols — literals of which are written as identifiers surrounded by single quote characters. The other main class of symbols — those introduced within user-defined type definitions are not covered by this type symbol; and nor are they written with single quotes.

The `symbol` type has the definition:
\begin{alltt}
symbol \impl thing.
\end{alltt}


####`logical` type
\label{types:standard:logical}
\index{logical@`logical` type}
\index{type!logical@`logical` type}

The `logical` type is used to denote truth values. Although a standard type, `logical` can be defined as a normal user-defined type, using the type definition:
\begin{alltt}
logical ::= true | false.
\end{alltt}

####`opaque` type
\label{types:standard:opaque}
\index{opaque@`opaque` type}
\index{type!opaque@`opaque` type}
The `opaque` type symbol is used to denote certain \`internal' values that are managed by the L&O system. There is no written notation that corresponds to opaque literal values, and they will never be displayed in normal circumstances.

####`meta` type
\label{types:standard:meta}
\index{meta@`meta` type}
\index{type!meta@`meta` type}

The `meta` type is used to represent a meta-level representation of a term. It is defined using the definition:
\begin{alltt}
meta \impl thing.
\end{alltt}

####Tuple type
\label{types:standard:tuple}
\index{tuple@`,` type}
\index{type!tuple@`,` type}

The `,` type is used to denote pairs of values. Although a standard type, `,` can be defined as though it were a normal user-defined type, using the definition:
\begin{alltt}
(u,v) ::= (u,v).
\end{alltt}
This rather bizarre type definition declares that the tuple type — denoted by the type term `(u,v)` has a single constructor, also `(u,v)`. This notation is a slight twist on the normal type notation — we are using the `,` operator as an infix type operator which is not actually permitted in programs.

####`list` type
\label{types:standard:list}

The list type is used to denote lists of values. The list type is written using the notation:
\begin{alltt}
list[_type_]
\end{alltt}
For example, the type expression:
\begin{alltt}
list[char]
\end{alltt}
denotes the type `list of`char`'. This is the type assignment given to expressions which are inferred to be lists of`char`acter — including string literals. L&O supports the`string`synonym for the`list[char]\` type term.

The `list` type has a definition equivalent to:
\begin{alltt}
list[t] \impl thing.
list[t] \impl \{
	head:[]=>t.                -- head of the list
	tail:[]=>list[t].          -- tail of the list
	eof:[]\{\}.                  -- is the list empty
	cons:[t]=>list[t].         -- new list on the front
	tack:[t]=>list[t].         -- new list on the back
	hdtl:[t,list[t]]\{\}.        -- pick off the head and tail
	eq:[list[t]]\{\}.            -- is equal to this list
\}
\end{alltt}

###Program types

####Function type
\label{types:standard:function}

The function type is used to denote function values. The function type is written:
\begin{alltt}
[_T\sub{1_},\ldots,_T\sub{n_}] \funarrow{} _T\sub{R_}
\end{alltt}
where `[_T\sub{1`_,\ldots,_T\sub{n_}]} is a list of the type expressions corresponding to the arguments of the function and _T\sub{R_} corresponds to the result of the function. For example, the type expression:
\begin{alltt}
[list[char],list[char]]=\>list[char]
\end{alltt}
denotes a function — from two string arguments to a string result.

The default mode for function arguments is _input_. However, it is occasionally useful to mark a function argument as output — as an out-of-band way of returning values from a function.

####Predicate type
\label{types:standard:predicate}

The predicate type is used to denote relational or predicate values. The predicate type is written:
\begin{alltt}
[_T\sub{1_},\ldots,_T\sub{n_}]\{\}
\end{alltt}
where `[_T\sub{1`_,\ldots,_T\sub{n_}]} is the list of type expressions of the arguments of the predicate. For example, the type expression:
\begin{alltt}
[list[char],list[char],list[char]]\{\}
\end{alltt}
denotes the type of a ternary predicate where all the arguments are strings.

The default mode for predicate arguments is _bidirectional_. However, it is often useful to mark a predicate argument as input — to be explicit about the expect usage of the predicate.


####Action type
\label{types:standard:action}

The action type is used to denote procedure values. The action type is written using a postfix `*` operator:
\begin{alltt}
[_T\sub{1_},\ldots,_T\sub{n_}]\*
\end{alltt}
where `[_T\sub{1`_,\ldots,_T\sub{n_}]} is the list of type expressions of the arguments of the procedure. For example, the type expression:
\begin{alltt}
[list[char]]\*
\end{alltt}
denotes the type of a unary procedure whose single argument is a string.

The default mode for action procedure arguments is _input_. However, it is occasionally useful to mark an argument as output — as that is the only way of arranging for output from an action procedure call.

####Grammar type
\label{types:standard:grammar}

The grammar type is used to denote grammar values. The grammar type is written as a `—>` mapping from the types of the arguments of the grammar to the type of the stream of values the grammar is defined over:
\begin{alltt}
[_T\sub{1_},\ldots,_T\sub{n_}] --\> _T\sub{S_}
\end{alltt}
where `[_T\sub{1`_,\ldots,_T\sub{n_}]} is the list of type expressions of the arguments of the grammar and _T\sub{S_} is the type of the stream that the grammar is defined over — typically a list of some kind. For example, the type expression:
\begin{alltt}
[number]--\>list[char]
\end{alltt}
denotes the type of a grammar whose single argument is a number and which may be used to parse strings.

The default mode for grammar arguments is _bidirectional_. However, it is occasionally useful to mark a grammar argument as input — to be explicit about the expect usage of the grammar, and also to mark an argument as output — to be explicit about the outputs associated with parsing a stream.

####Class type
\label{types:standard:class}

\index{type!class}
\index{class type}
\index{type!@= type operator}
There are two kinds of class type declarations: those that introduce a stateless class and those that introduce a stateful class.

The state-free class type is used to denote constructor classes, analogous to constructor functions. The stateful class type denotes classes that may carry state.


A state-free class type declaration takes the form:
\begin{alltt}
_class_:[_T\sub1_,\ldots,_T\subn_] @= _Type_.
\end{alltt}
The only permitted mode for the label arguments of a state-free class label is _bidrectional_.

A stateful class type declaration takes the form:
\begin{alltt}
_class_:[_T\sub1_,\ldots,_T\subn_] @\> _Type_.
\end{alltt}
The default mode for label arguments of a stateful class label is _input_.

\begin{aside}
The reason that state-free label arguments must be bi-directional is that class labels are equivalent to constructor functions: i.e., they have an inverse. That means that it is always possible to recover the arguments of a 'call' to a constructor function.

On the other hand, one of the differences between state-free class labels and statefull class labels is that the latter _do not_ have inverses — they are semantically closer to regular functions.
\end{aside}
Note that a class type statement does not define the class itself, it simply defines its type.

## Algebraic type definitions
\label{type:algebraic}

In addition to a type being defined using the sub-type statements, a type may also be introduced using an _algebraic type definition_ statement. An algebraic type definition is one where the type is introduced at the same time as a set of enumerated symbols and constructors for the type:
\begin{alltt}
_UserT_[_T_\sub1,\dots,_T_\subn] ::= \dots\ | _S_ | \dots.
\end{alltt}
where _T\subi_ are identifiers indicating type arguments and the right hand side is a series of enumerated symbols and constructor function templates.

For example, the `tree` type defined below may be used to denote tree values:

\begin{alltt}
tree[a] ::= empty | node(tree[a],a,tree[a]).
\end{alltt}
This statement defines a new type type constructor `tree` together with the enumerated symbols and constructor terms that make up values of the `tree` type.

###Type parameters of a type definition

Where the template of a _UserType_ takes the form:
\begin{alltt}
_UserType_[_T_\sub1,\dots,_T_\subn] \impl \ldots
\end{alltt}
the various arguments _T\subi_ are the _type parameter_s of the type definition. They must all be identifiers and they are interpreted as type variables. Such a _UserType_ is implicitly universally quantified with respect to the type parameters (hence the polymorphism of the type).

L&O imposes a restriction on type variables occurring in a type definition: all type variables appearing in the body of the type definition — i.e., appearing in templates for the type constructors in class definitions — must also appear in the type template expression itself.

###Enumerated symbol
\label{type:user-symbol}
An _enumerated_ symbol is equivalent to a zero-arity label term. For example, the introduction of `empty` in the definition of `tree` above is equivalent to the class definition:
\begin{alltt}
empty:tree[a] .. \{\}.
\end{alltt}

The type analysis of a definition of an enumerated symbol may be captured via an \`introduced' type inference rule:
\begin{equation\*}
\forall \vec{V}.\left\{\frac{}
{\typeprd{E}{`S`}{`_UserT`[\emph{T_\sub1,\dots,_T_\subn]}}}\right\}
\end{equation\*}
where $\vec{V}$ are all the type variables ocurring in the type definition and `S` is the newly introduced enumerated symbol.

Note that, in general, the type of a enumerated symbol can have type variables even when clearly the symbol itself has no variables. We can see this more clearly with the empty list case. L&O's list notation is based on the `,..` constructor function and the `[]` enumerated symbol; which might be captured in the algebraic type definition:
\begin{alltt}
list[T] ::= [T,..list[T]] | []
\end{alltt}
The type of an occurrence of the empty list is, then, an expression of the form:
\begin{alltt}
list[T\sub{i}]
\end{alltt}
where `T\sub{i`} may or may not be known. The logic of this is that an empty list is always of a list of a specific type — even if we cannot determine what that type is in given circumstances.

###Constructor functions
\label{type:constructor}
\index{contructor function definition}
\index{functions!constructor}

Constructor functions (a.k.a. class labels) are analogous to functors in \prolog: they fulfill very similar roles. However, their semantics are quite distinct to normal \prolog terms since they always are associated with logical theories — even when defined within an algebraice type definition.

\begin{aside}
  Constructor functions are so-named because they are functions: with the
  particular property that every expression involving the constructor function
  has an exact inverse. This property allows constructor functions to be used as
  patterns as well as in other expressions.
\end{aside}

The type assignment for constructor functions can be viewed as an additional type inference rule; where a type definition was of the form:
\begin{alltt}
_Template_ ::= \ldots | _F_(_T\sub1_,\ldots,_T\subn_) | \ldots
\end{alltt}
we introduce a new inference rule of the form:
\begin{equation}
\LeftLabel{$\forall \vec{V}$}
\AxiomC{\typeprd{{_E_}}{A\sub1}{_T\sub{1_}}}
\AxiomC{\ldots}
\AxiomC{\typeprd{{_E_}}{A\subn}{_T\sub{n_}}}
\TrinaryInfC{\typeprd{{_E_}}{`_F`(\emph{A\sub1_,\ldots,_A\subn_)}}
{`_Template`_}}
\DisplayProof
\end{equation}
where $\vec{V}$ are the type variables ocurring in the type definition.

###Type extension
One of the key features of L&O's type system is its extensibility. In particular, because types are distinct from classes, there is always the potential to introduce new constructors for a type — including types that have been introduced using an algebraic type definition.

For example, given the `tree[]` type, we may decide that we need a new kind of node for a tree — perhaps a binary tree node that does not include a label. We can do so simply by defining a class for it:
\begin{alltt}
bin:[tree[a],tree[a]]@=tree[a].
bin(L,R) .. \{ \ldots \}
\end{alltt}
The effect of this is as though the original type definition were:
\begin{alltt}
tree[a] ::= empty 
	    | node(tree[a],a,tree[a])
	    | bin(tree[a],tree[a]).
\end{alltt}
We can even introduce this class in a different package than the one in which `tree[]` itself is defined:
\begin{alltt}
foo\{
  import tree.
  bin:[tree[a],tree[a]]@=tree[a].
  bin(L,R) .. \{ \ldots \}
\}
\end{alltt}
  
