protocol{
  import directory.
  import go.mbox.

  gender ::= male|female.

  mailProto::=shallWe(symbol,dropbox[phemailProto]).

  phemailProto::=whyNot(symbol,dropbox[mailProto]) | 
                 rainCheck(symbol,dropbox[mailProto]).

  locM:[dropbox[mailProto]]@=dirVal.
  locM(D)..{
    show()=>D.show().
  }.

  locF:[dropbox[phemailProto]]@=dirVal.
  locF(D)..{
    show()=>D.show().
  }.

  sex:[gender]@=dirVal.
  sex(G)..{
    show() => G.show().
  }.

  dancer <~ {
        start:[]*.
        report:[]=>string
      }.
}