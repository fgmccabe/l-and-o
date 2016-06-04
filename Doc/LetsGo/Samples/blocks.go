/*
 * A sample test to bring together planning, evaluation and execution
 * with a smidgin of replanning
 */

blocks{
  import go.io.
  import strips.
  import robbie.

  blockProblem:[list[blockPred],list[blockPred]]{}.
  blockProblem([clear('a'),clear('b'),clear('c'),
		ontable('a'),ontable('b'),ontable('c'),
		handempty],
	       [on('b','c'),on('a','b')]).
  blockProblem([clear('c'),
		ontable('a'),
		on('b','a'),on('c','b'),
		handempty],
	       [on('a','b'),on('b','c')]).
  blockProblem([clear('b'),clear('c'),
		ontable('a'),ontable('b'),on('c','a'),
		handempty],
	       [on('a','b'),on('b','c')]).
  blockProblem([clear('b'),clear('c'),
		ontable('a'),ontable('b'),on('c','a'),
		handempty],
	       [on('a','c')]).

  main(_) ->
      blockProblem(State,Goal) *>
      stdout.outLine("Solving "<>Goal.show()<>" in state "<>State.show());
      Robbie = robbie(State);
      Planner = planner(Robbie);
      ( A in Planner.plan(State,Goal) *>
	Robbie.do(A)).
}
