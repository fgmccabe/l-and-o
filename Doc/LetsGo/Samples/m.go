Likes..{
  include "dynclauses.gof".
  include "meta.gh".
  
  callterm::= 	parent(symbol,symbol) | 
  		likes(symbol,symbol).
  				
  Likes= $dynamic_clauses[callterm]([cl(likes(P,'a'),TRUE)]).
}