phemail{
  import go.io.
  import directory.
  import protocol.
  import go.mbox.

  phemail:[symbol,integer]@>dancer.
  phemail(Name,Limit)..{
    ourBox:mailbox[mailProto] = mailbox().
    ourDrop:dropbox[mailProto] = ourBox.dropbox().
    partners:list[symbol] := [].
    
    init:[]*.
    init() ->
        dir.register([attr('name',name(Name)),
                      attr('gender',sex(female)),
                      attr('loc',locM(ourDrop))]).
  
    accepting:[integer]*.
    accepting(Count)::Count>0 ->
        ourBox.msg(shallWe(Who,Reply));
        Reply.post(whyNot(Name,ourDrop));
        partners := [Who,..partners];
        accepting(Count-1).
    accepting(0) -> rejecting().

    rejecting:[]*.
    rejecting() ->
        ourBox.msg(shallWe(_,Reply));
        Reply.post(rainCheck(Name,ourDrop));
        rejecting().
    
    start() -> init(); accepting(Limit).
    
    report() =>
        explode(Name)<>" partners are "<>partners.show().
  }
}


