:- use_module(ocall).
:-['/Users/fgm/Projects/LandO/LO/Build/string.pl'].
'comp.operators#import'("file:/Users/fgm/Projects/LandO/LO/Build/string.pl").
:-['/Users/fgm/Projects/LandO/LO/Build/arith.pl'].
'comp.operators#import'("file:/Users/fgm/Projects/LandO/LO/Build/arith.pl").
'comp.operators#export'("I6'infixOp'P4t'lo.string*string'iii'prefixOp'P3t'lo.string*string'ii'postfixOp'P3t'lo.string*string'ii'isOperator'P2t'lo.string*string'i'follows'P3t'lo.string*string'it'lo.string*string''final'P1t'lo.string*string'").
'comp.operators#types'("I0").
'comp.operators@infixOp'(". ", 1899, 1900, 1900).
'comp.operators@infixOp'("::=", 1459, 1460, 1459).
'comp.operators@infixOp'("|", 1249, 1250, 1250).
'comp.operators@infixOp'(":", 1249, 1250, 1249).
'comp.operators@infixOp'("~~", 1239, 1240, 1239).
'comp.operators@infixOp'("?", 1199, 1200, 1199).
'comp.operators@infixOp'("<=>", 1199, 1200, 1199).
'comp.operators@infixOp'("=>", 1199, 1200, 1199).
'comp.operators@infixOp'(":-", 1199, 1200, 1199).
'comp.operators@infixOp'(":--", 1199, 1200, 1199).
'comp.operators@infixOp'("-->", 1199, 1200, 1199).
'comp.operators@infixOp'("*>", 1151, 1152, 1151).
'comp.operators@infixOp'("::", 1129, 1125, 1129).
'comp.operators@infixOp'("||", 1059, 1060, 1059).
'comp.operators@infixOp'(",", 999, 1000, 1000).
'comp.operators@infixOp'(",..", 999, 1000, 1000).
'comp.operators@infixOp'("<=", 949, 950, 949).
'comp.operators@infixOp'("<~", 949, 949, 948).
'comp.operators@infixOp'("=", 899, 900, 899).
'comp.operators@infixOp'("==", 899, 900, 899).
'comp.operators@infixOp'("\\=", 899, 900, 899).
'comp.operators@infixOp'("!=", 899, 900, 899).
'comp.operators@infixOp'("<", 899, 900, 899).
'comp.operators@infixOp'("=<", 899, 900, 899).
'comp.operators@infixOp'(">", 899, 900, 899).
'comp.operators@infixOp'(">=", 899, 900, 899).
'comp.operators@infixOp'(".=", 899, 900, 899).
'comp.operators@infixOp'("=.", 899, 900, 899).
'comp.operators@infixOp'("..", 895, 896, 895).
'comp.operators@infixOp'("in", 894, 895, 894).
'comp.operators@infixOp'("<>", 799, 800, 800).
'comp.operators@infixOp'("#", 759, 760, 759).
'comp.operators@infixOp'("+", 720, 720, 719).
'comp.operators@infixOp'("-", 720, 720, 719).
'comp.operators@infixOp'("*", 700, 700, 699).
'comp.operators@infixOp'("/", 700, 700, 699).
'comp.operators@infixOp'("%", 700, 700, 699).
'comp.operators@infixOp'("rem", 700, 700, 699).
'comp.operators@infixOp'("**", 600, 600, 599).
'comp.operators@infixOp'("%%", 499, 500, 499).
'comp.operators@infixOp'("^", 499, 500, 499).
'comp.operators@infixOp'("~", 934, 935, 934).
'comp.operators@infixOp'(".", 450, 450, 449).
'comp.operators@prefixOp'("private", 1700, 1699).
'comp.operators@prefixOp'("public", 1700, 1699).
'comp.operators@prefixOp'("assert", 1260, 1259).
'comp.operators@prefixOp'("import", 900, 899).
'comp.operators@prefixOp'("all", 1245, 1244).
'comp.operators@prefixOp'("\\+", 905, 904).
'comp.operators@prefixOp'("@", 905, 904).
'comp.operators@prefixOp'("-", 300, 299).
'comp.operators@postfixOp'(". ", 1899, 1900).
'comp.operators@postfixOp'(";", 1149, 1150).
'comp.operators@postfixOp'("+", 759, 760).
'comp.operators@postfixOp'("-", 759, 760).
'comp.operators@postfixOp'("-+", 759, 760).
'comp.operators@postfixOp'("+-", 759, 760).
'comp.operators@postfixOp'("!", 904, 905).
'comp.operators@isOperator'(XOp, XPr) :- 'comp.operators@prefixOp'(XOp, XPr, X_).
'comp.operators@isOperator'(XOp, XPr) :- 'comp.operators@infixOp'(XOp, X_, XPr, X_).
'comp.operators@isOperator'(XOp, XPr) :- 'comp.operators@postfixOp'(XOp, X_, XPr).
'comp.operators@follows'("", 37, "%").
'comp.operators@follows'("%", 37, "%%").
'comp.operators@follows'("", 42, "*").
'comp.operators@follows'("*", 42, "**").
'comp.operators@follows'("*", 62, "*>").
'comp.operators@follows'("", 43, "+").
'comp.operators@follows'("+", 45, "+-").
'comp.operators@follows'("", 44, ",").
'comp.operators@follows'(",", 46, ",.").
'comp.operators@follows'(",.", 46, ",..").
'comp.operators@follows'("", 45, "-").
'comp.operators@follows'("-", 43, "-+").
'comp.operators@follows'("-", 45, "--").
'comp.operators@follows'("--", 62, "-->").
'comp.operators@follows'("", 46, ".").
'comp.operators@follows'(".", 46, "..").
'comp.operators@follows'(".", 32, ". ").
'comp.operators@follows'(".", 61, ".=").
'comp.operators@follows'("", 47, "/").
'comp.operators@follows'("", 124, "|").
'comp.operators@follows'("|", 124, "||").
'comp.operators@follows'("", 126, "~").
'comp.operators@follows'("~", 126, "~~").
'comp.operators@follows'("", 92, "\\").
'comp.operators@follows'("\\", 43, "\\+").
'comp.operators@follows'("\\", 61, "\\=").
'comp.operators@follows'("", 94, "^").
'comp.operators@follows'("", 58, ":").
'comp.operators@follows'(":", 58, "::").
'comp.operators@follows'("::", 61, "::=").
'comp.operators@follows'(":", 45, ":-").
'comp.operators@follows'(":-", 45, ":--").
'comp.operators@follows'("", 59, ";").
'comp.operators@follows'("", 60, "<").
'comp.operators@follows'("<", 126, "<~").
'comp.operators@follows'("<", 61, "<=").
'comp.operators@follows'("<=", 62, "<=>").
'comp.operators@follows'("<", 62, "<>").
'comp.operators@follows'("", 61, "=").
'comp.operators@follows'("=", 46, "=.").
'comp.operators@follows'("=", 60, "=<").
'comp.operators@follows'("=", 61, "==").
'comp.operators@follows'("=", 62, "=>").
'comp.operators@follows'("", 62, ">").
'comp.operators@follows'(">", 61, ">=").
'comp.operators@follows'("", 63, "?").
'comp.operators@follows'("", 64, "@").
'comp.operators@follows'("", 33, "!").
'comp.operators@follows'("!", 61, "!=").
'comp.operators@follows'("", 35, "#").
'comp.operators@final'("%").
'comp.operators@final'("%%").
'comp.operators@final'("*").
'comp.operators@final'("**").
'comp.operators@final'("*>").
'comp.operators@final'("+").
'comp.operators@final'("+-").
'comp.operators@final'(",").
'comp.operators@final'(",..").
'comp.operators@final'("-").
'comp.operators@final'("-+").
'comp.operators@final'("-->").
'comp.operators@final'(".").
'comp.operators@final'("..").
'comp.operators@final'(". ").
'comp.operators@final'(".=").
'comp.operators@final'("/").
'comp.operators@final'("|").
'comp.operators@final'("||").
'comp.operators@final'("~").
'comp.operators@final'("~~").
'comp.operators@final'("\\+").
'comp.operators@final'("\\=").
'comp.operators@final'("^").
'comp.operators@final'(":").
'comp.operators@final'("::").
'comp.operators@final'("::=").
'comp.operators@final'(":-").
'comp.operators@final'(":--").
'comp.operators@final'(";").
'comp.operators@final'("<").
'comp.operators@final'("<~").
'comp.operators@final'("<=").
'comp.operators@final'("<=>").
'comp.operators@final'("<>").
'comp.operators@final'("=").
'comp.operators@final'("=.").
'comp.operators@final'("=<").
'comp.operators@final'("==").
'comp.operators@final'("=>").
'comp.operators@final'(">").
'comp.operators@final'(">=").
'comp.operators@final'("?").
'comp.operators@final'("@").
'comp.operators@final'("!").
'comp.operators@final'("!=").
'comp.operators@final'("#").
