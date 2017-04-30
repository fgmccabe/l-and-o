# Introduction
It can be claimed that _all_ successful software projects are _team_ efforts. In any case, software at scale _is_ a team effort.

Developing software in the context of a team, requires techniques and features that are significantly different to those in the typical laboratory workbench. Applications that routinely interface with other systems need to adopt a much more very defensive approach to inter-working. 

L&O is a logic programming language, designed from scratch to be effective in such an environment.

## Software engineering

Software engineering is the practice of reliably developing reliable software that meets the needs of the relevant stakeholders.

This applies as much to 'Artificially Intelligent' systems as it does to more conventional ones.

The design of L&O is guided by a strong desire to gain the benefits of modern software engineering best practice as well as that of knowledge engineering.

This is reflected in the design of L&O in a number of ways:

* Strong[^ Strong in both senses: all expressions have a type and the type language is powerful.] static typing supported by type inference
* Powerful library mechanisms
* Multi-paradigm
* Extensibility & Integration

L&O is a multi-paradigm language – there are specialized notations for functions, and objects, as well as predicates and grammar rules. By using specific notation for specific purposes we can allow programmers to signal their intentions more clearly than if all kinds of program have to be expressed in a single formalism.

## Strong Static types

L&O is a strongly, statically, typed programming language. The purpose of using a strong type system is to enhance programmers’ confidence in the correctness of the program – it cannot replace a formal proof of correctness.

The purpose of static typing is to ensure that type errors as possible occur *before* running the program. The semantics of L&O is such that it is impossible for a type error to arise at run-time.

Having a strongly typed language can be quite constrictive compared to the untyped freedom one gets in languages such as Prolog or JavaScript. However, for applications requiring a strong sense of reliability, having a strongly typed language provides a better base than an untyped language.

An important quality of type systems is 'how strong they are?'. The type language strongly (sic) affects what the programmer can express. L&O's type language includes algebraic types, type quantification and type constraints. On the other hand, L&O's type language is _not_ based on sub-typing: which may be an issue if you are used to languages like Java which rely heavily on concerts such as inheritance.

One of the key policy questions in types and programming languages relates to explicit type annotations (where a variable or expression is given a type by the programmer) and type inference (where the compiler infers the types of variables and expressions).

It is perfectly possible for a language to use 100% type inference, or 0% type inference. The trade-offs are one of verbosity on the one hand or visibility on the other. In L&O we require named functions and other top-level variables to have explicit type annotations and 'rule variables' will have their type inferred automatically. The intention is to strike a pleasant balance between clarity of programs and succinctness of programs. 

Part of the reason for this choice is that the type system of L&O is sufficiently rich and subtle that being explicit about some types is desireable.

## Object orderedness

L&O’s *object orderedness* is key to L&O’s approach to software engineering. However, an object is L&O is quite different to the mutable objects one sees in conventional OO languages: it is closer in spirit to a _module_ or a _record_ containing functions.

In addition, an object may contain _existentially quantified types_.

> We use the term *object ordered programming* to avoid some of the specifics of common object oriented languages – the key feature of object ordered programming is the encapsulation of code and data that permits the *hiding* of the implementation of a concept from the parts of the application that wish to merely *use* the concept. Features such as inheritance are important but secondary compared to the core concepts of encapsulation and hiding.

We observe above that L&O's type system is not based on sub-types. This is another difference between L&O's objects and that found in conventional OO languages.

### Meta-order and object order

Unlike Prolog, L&O does not permit data to be directly interpreted as code. The standard Prolog approach of using the same language for meta-level names of programs and programs themselves – which in turn allows program text to be manipulated like other data – has a number of technical problems; especially when considering distributed and secure applications. Many of the applications of meta-level reasoning in Prolog can be more directly – and safely – be affected using L&O’s object oriented features.

## Threads and distribution

L&O is a multi-tasking programming language. It is possible to spawn off computations as separate threads or tasks. These tasks can communicate with each other using shared global objects and by message passing.

In a multi-threaded environment there are two overlapping concerns – sharing of resources and coordination of activities. In the cases of shared resources the primary requirement is that the different users of a resource see consistent views of the resource. In the case of coordination the primary requirement is a means of controlling the flow of execution in the different threads of activity.

L&O threads may share access to objects, and to their state in the case of stateful objects – within a single invocation of the system. L&O supports synchronized access to such shared objects to permit contention issues to be addressed.

Coordination is achieved through *message communication*. The communications model is very simple: a communication channel is established by a pair of entities: a `mailbox` and one or more associated `dropbox`s. When one process wishes to send a message to another process, it does so by using an appropriate `dropbox`. A process wishing to read a message does so by picking up messages from its `mailbox`.

## This reference manual

Overall, the intention behind the design of L&O is to make programs more transparent: what you see in a L&O program is what you mean – there can be no hidden semantics. This property is what makes L&O a reasonable language to use for high integrity applications – such as agents that will be performing tasks that may involve real resources, or in safety critical areas.

This manual is intended to represent both a reference to a particular L&O implementation and as a specification of the language itself. In the future, these functions may be separated into distinct documents.
