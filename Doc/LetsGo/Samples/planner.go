/*
 * A simple STRIPS planner adapted from Shoham
 */
 
planner..{
  include "sys:go/stdlib.gof".
  include "sys:go/setlib.gof".
  include "sys:go/io.gof".
  include "meta.gh".
  include "action.gh".
  include "meta.gof".

  planner(W<~actions[_,_],conj(GoalList)) => seq(reverse(Plan)) :-
      strips(([],[]),GoalList,[],[],_,Plan)..{
        strips(State,Goals,okPlan,_,State,okPlan) :-
            G in Goals *> satisfied(G,State).
        strips(State,Goals,PlanSoFar,Forbidden,NewState,NewPlan) :-
            Goal in Goals,
            \+ satisfied(Goal,State),
            W.strips_rule(Act,Prec,Adds,Dels),
            Goal in Adds,
            \+ Act in Forbidden,
            strips(State,Prec,PlanSoFar,[Act,..Forbidden],TmpSt1,TmpPl1),
            strips(newState(TmpSt1,Adds,Dels),
                   Goals,[prim(conj(Prec),Act),..TmpPl1],
                   Forbidden,NewState,NewPlan).

        satisfied(G,(Plus,_)) :- G in Plus.
        satisfied(G,(_,Minus)) :- \+G in Minus, evaluate(W,G).
      }.

  newState((Facts,nFact),Plus,Minus) =>
      ((Facts\Minus)\/Plus,(nFact\Plus)\/Minus).
}
