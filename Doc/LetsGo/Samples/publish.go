-- Publish a description to the directory
publish{
  import directory.
  import go.datelib.
  import go.io.

  rawSkills:list[symbol] = ['wood','science','economics','smith','teacher'].

  publish:[integer]*.
  publish(0) -> {}.
  publish(Count) ->
      stdout.outLine("Publishing "<>Count.show());
      dir.register([attr('name',name(implode("entry "<>Count.show()))),
                    attr('when',when(time2date(now()))),
                    attr('skills',skills({X .. (X::rand(2.0)>1.0) in rawSkills}))]);
      delay(rand(4));
      publish(Count-1).
}
