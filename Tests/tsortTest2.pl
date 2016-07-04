:- use_module(topsort).

defs([
(var("parse"), [var("term"), var("checkForTerminator")],3,"parse"),
(var("term"), [ var("termLeft"), var("termRight")],4,"term"),
(var("termLeft"), [var("term"), var("term0")],5,"termLeft"),
(var("termRight"), [var("legalInfixOp"), var("term"), var("termRight"), var("legalPostfixOp"), var("termRight")],6,"termRight"),
(var("legalPostfixOp"), [],9, "legalPostfixOp"),
(var("term0"), [ var("parseString"), var("tupleize"),  var("term"), var("checkFor"), var("terms"), var("checkFor"), var("term"), var("checkFor"),var("term00"), var("termArgs")],10,"term0"),
(var("term00"), [var("tupleize"),  var("term"), var("checkFor")],11,"term00"),
(var("termArgs"), [var("termArgs"), var("term"), var("checkFor"), var("termArgs"),var("tupleize"), var("terms"), var("checkFor")],12,"termArgs"),
(var("terms"), [var("parse"), var("terms")],13,"terms"),
(var("tupleize"), [var("tupleize")],14,"tupleize"),
(var("checkForTerminator"), [var("checkFor")],16,"checkForTerminator"),
(var("parseString"), [var("parseSegments")],17,"parseString"),
(var("parseSegments"), [var("parseSegments"), var("interpolateSegment"), var("parseSegments")],18,"parseSegments"),
(var("interpolateSegment"), [var("term"), var("formatDisp"), var("term"), var("formatDisp")],19,"interpolateSegment"),
(var("formatDisp"), [],20,"formatDisp")
]).

tsb(Groups) :- defs(Defs), topsort(Defs,Groups), writef("result groups: %w\n",[Groups]).
