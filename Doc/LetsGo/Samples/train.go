/*
 * A generic train
 */
train{
  import engine.

  train <~ { journey_time:[number]=>number. coaches:[integer]{}}.

  train:[engine]@=train.
  train(E)..{
    speed:[]=>number.
    speed()::this.coaches(Length) => E.power()/(n2float(Length)+1.0).

    coaches(0).

    journey_time(D) => D/speed().
  }
}.
    