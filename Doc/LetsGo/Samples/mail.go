mail{
  import go.io.
  import directory.
  import protocol.
  import go.mbox.

  mail:[symbol]@>dancer.
  mail(Name)..{
    ourBox:mailbox[phemailProto] = mailbox().
    ourDrop:dropbox[phemailProto] = ourBox.dropbox().
    partners:list[symbol] := [].

    propose:[dropbox[mailProto],symbol]{}.
    propose(P,H) :- action{
		      P.post(shallWe(Name,ourDrop));
		      ourBox.msg(Reply);
		      (whyNot(H,_).=Reply ? valis true | valis false)
		    }.
    
    Phemails:[]=>list[dropbox[mailProto]].
    Phemails() => { locOf(A) .. A in dir.find([attr('gender',sex(female))],['loc']) }.

    locOf:[list[attribute]]=>dropbox[mailProto].
    locOf(A) :: attr('loc',locM(P)) in A => P.

    start() -> 
        partners := { H..(P::propose(P,H)) in Phemails() }.

    report() =>
        explode(Name)<>" partners are "<>partners.show().
  }.
}

