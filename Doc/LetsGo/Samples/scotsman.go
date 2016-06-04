/*
 * Test the object and class notation of Go!
 */
 
scotsman{
  import go.io.
  import go.stdparse.
  import train.
  import engine.

  steamLoco:[]@>engine.
  steamLoco() <= engine(1000.0,'coal').

  electricLoco:[]@>engine.
  electricLoco() <= engine(2000.0,'electricity').

  scotsman:[]@=train.
  scotsman <= train(steamLoco()).
  scotsman..{
    coaches(4).
  }.
  
  main([Dist,.._]) ->
      D = floatOf%%Dist;
      O = scotsman;
      stdout.outLine("The Flying Scotsman takes "<>
                     O.journey_time(D).show()<>" to do "<>D.show()<>" miles").
}.
