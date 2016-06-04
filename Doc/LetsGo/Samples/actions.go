/*
 * An action evaluator
 */
 
(run) .. {
  include "sys:go/io.gof".
  include "sys:go/stdlib.gof".
  include "meta.gh".
  include "action.gh".
  include "meta.gof".
  include "planner.gof".
  
  run(W<~actions[aT,gT],Action:actType[aT,gT]) -> exec(Action) .. {
        exec(prim(Prec,Act)::evaluate(W,Prec)) -> W.do(Act).
        exec(prim(G,_)) -> stdout.outLine("replanning "<>G^0);
            exec(planner(W,G)).
        exec(seq(L)) -> E in L*>exec(E).
        exec(choice(Tst,A1,_)::evaluate(W,Tst)) -> exec(A1).
        exec(choice(_,_,A2)) -> exec(A2).
        exec(subplan(Name)) -> exec(W.which(Name)).
    
        exec_seq([])->{}.
        exec_seq([E,..L]) -> exec(E);exec_seq(L).
      }.
}
