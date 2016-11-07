= Notes on L&O =

== Objections vs Functions ==

foo {
  bar()=>E

  map(L,bar)

  equivalent to

  IX.map(L,{apply:(x)=>this.bar(x)})
  where IX is the implementation object for the map contract
  and 

  f(x)

  equalivant to

  {apply:(u)=>X.f(u)}.apply(x)

  where f is defined in X

  and

  {... m:a=>b ...}.m(x)

  equivalent to

  b[a<-x, this<-X]
}