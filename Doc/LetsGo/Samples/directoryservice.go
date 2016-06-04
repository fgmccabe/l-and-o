directory_server..{
  include "sys:go/dynamic.gof".
  
  include ds.gh.
  
  description=$dynamic([]).

  matches(D,E) :-
    A in E *> A in D.
    
  directory_server() ->                   
    ( register(Descr) << _ -> description.add(Descr)
    | search(SDescr, AttrNms) << Client ->
       (description.mem(Descr),matches(Descr,SDescr) *> 
          inform({attr(Nm,A) || Nm in AttrNms, 
                                attr(Nm,A) in Descr}) >> Client)
    );
    directory_server().
}