:-module(keywords, [keyword/1,isKeyword/1,isRuleKeyword/1]).

  isKeyword(X):- keyword(X), !.

  keyword("|").
  keyword("||").
  keyword(";").
  keyword(":").
  keyword("::").
  keyword(",").
  keyword("..").
  keyword("?").
  keyword("!").
  keyword("^").
  keyword("~").
  keyword("~~").
  keyword("=").
  keyword(".=").
  keyword("=.").
  keyword("=>").
  keyword("<=>").
  keyword("->").
  keyword(":-").
  keyword(":--").
  keyword("-->").
  keyword("::=").
  keyword("<=").
  keyword("<~").
  keyword("$").
  keyword("*>").
  keyword("\\+").
  keyword("\\=").
  keyword("!=").
  keyword(".").
  keyword("%%").
  keyword("@").
  keyword("@@").
  keyword("true").
  keyword("false").
  keyword("this").
  keyword("import").
  keyword("public").
  keyword("private").
  keyword("type").
  keyword("contract").
  keyword("implementation").
  keyword("void").
  keyword("top").
  keyword("all").
  keyword("#").

  isRuleKeyword(X):- ruleKeyword(X), !.

  ruleKeyword("::").
  ruleKeyword("..").
  ruleKeyword("~~").
  ruleKeyword("=>").
  ruleKeyword("<=>").
  ruleKeyword("->").
  ruleKeyword(":-").
  ruleKeyword(":--").
  ruleKeyword("-->").
  ruleKeyword("::=").
  ruleKeyword("<=").
  ruleKeyword("<~").
