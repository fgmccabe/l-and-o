-- This implements the STRIPS planner

strips{
  import go.setlib.

  planner[A,G] <~ {
	plan:[list[G],list[G]]=>list[A]
      }.

  stripsWorld[A,G] <~ {
	strips_rule:[A,list[G],list[G],list[G]]{}.
	holds:[G]{}.
	do:[A]*.
  }.

  planner:[stripsWorld[A,G]]@>planner[A,G].
  planner(W:stripsWorld[A,G])..{
    plan(Init,Goal)::strips(Init,Goal,[],Plan) => Plan.

    -- This is a progressive planner ... from the state to the goal
    strips:[list[G],list[G],list[A],list[A]]{}.
    strips(State,Goal,_,[]):-
	subset(Goal,State).
    strips(State,Goal,Forbidden,[Ac,..Plan]) :-
        W.strips_rule(Ac,Prec,Adds,Dels),
	subset(Prec,State),
	\+ Ac in Forbidden,
	strips((State\/Adds)\Dels,Goal,[Ac,..Forbidden],Plan).
  }.
}








