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
