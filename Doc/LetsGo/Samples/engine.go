/*
 * A sample engine class
 */
engine{
  engine <~ { power:[]=>float. set:[float]* }.

  engine:[float,symbol]@>engine.
  engine(Power,Fuel)..{
    Pwr:float := Power.
 
    power()=>Pwr.

    set(P)::(P>=0.0, P=<Power) -> Pwr := P.
    set(_) -> {}.

    fuel:[]=>symbol.
    fuel() => Fuel.
  }
}