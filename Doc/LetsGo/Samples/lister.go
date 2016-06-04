lister{
  import directory.
  import go.io.
  import go.datelib.

  lister:[symbol]*.
  lister(K) ->
      stdout.outLine("List of "<>explode(K));
      (D in dir.find([],['name',K]) *> stdout.outLine(showEntry(D)));
      delay(rand(1));
      lister(K).

  listAll:[]*.
  listAll() ->
      stdout.outLine("All entries list");
      (D in dir.find([],[]) *> stdout.outLine(showEntry(D)));
      delay(rand(1));
      listAll().
    
  showEntry:[list[attribute]]=>string.
  showEntry([]) => [].
  showEntry([E,..L]) => showAtt(E)<>"; "<>showEntry(L).

  showAtt:[attribute]=>string.
  showAtt(attr('when',when(D))) => D.show().
  showAtt(attr('name',name(W))) => explode(W).
  showAtt(attr('skills',skills(L))) => L^.
  showAtt(attr(K,V)) => explode(K)<>":"<>V^.

}

