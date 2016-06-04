/*
 * The BDI module for our agents
 */
bdi{
  import go.dynamic.
  import go.io.

  bdi[T] <~ ( 
       isAbelief:[T]{}.
       isAdesire:[T]{}.
       isAgoal:[T]{}.

       newBelief:[T]*.
       newDesire:[T]*.
       newGoal:[T]*.
      ).

  bdi:[list[T],list[T],list[T]]$=bdi[T].
  bdi(Bf,Ds,Gl) ..{
    beliefs:dynamic[T] = dynamic(Bf).
    desires:dynamic[T] = dynamic(Ds).
    goals:dynamic[T] = dynamic(Gl).

    isAbelief(X) :- beliefs.mem(X).
    isAdesire(X) :- desires.mem(X).
    isAgoal(X) :- goals.mem(X).
    
    newBelief(G)::isAbelief(G) -> {}.
    newBelief(G) -> beliefs.add(G).

    newDesire(G)::isAdesire(G) -> {}.
    newDesire(G) -> desires.add(G).

    newGoal(G)::isAgoal(G) -> {}.
    newGoal(G) -> goals.add(G).
  }.

}  
  