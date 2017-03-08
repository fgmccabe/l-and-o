'#pkg'("n7o7'()7'n2o2'pkg's'lo.comp.keywords's'0.0.1'n1o1'()1'n2o2'import'e'private'n2o2'pkg's'lo'e'*'s\"I2'isKeyword'PT1S'isRuleKeyword'PT1S\"s'I0'n0o0'()0'n0o0'()0'n0o0'()0'").
'lo.comp.keywords@init'():- !.
'lo.comp.keywords@keyword'("|").
'lo.comp.keywords@keyword'("||").
'lo.comp.keywords@keyword'(";").
'lo.comp.keywords@keyword'(":").
'lo.comp.keywords@keyword'("::").
'lo.comp.keywords@keyword'(",").
'lo.comp.keywords@keyword'("?").
'lo.comp.keywords@keyword'("!").
'lo.comp.keywords@keyword'("^").
'lo.comp.keywords@keyword'("~").
'lo.comp.keywords@keyword'("~~").
'lo.comp.keywords@keyword'("=").
'lo.comp.keywords@keyword'("=>").
'lo.comp.keywords@keyword'("<=>").
'lo.comp.keywords@keyword'("->").
'lo.comp.keywords@keyword'(":-").
'lo.comp.keywords@keyword'("-->").
'lo.comp.keywords@keyword'("->>").
'lo.comp.keywords@keyword'("::=").
'lo.comp.keywords@keyword'("<=").
'lo.comp.keywords@keyword'("<~").
'lo.comp.keywords@keyword'("*>").
'lo.comp.keywords@keyword'("\\+").
'lo.comp.keywords@keyword'("\\=").
'lo.comp.keywords@keyword'("!=").
'lo.comp.keywords@keyword'(".").
'lo.comp.keywords@keyword'("%%").
'lo.comp.keywords@keyword'("@").
'lo.comp.keywords@keyword'("@@").
'lo.comp.keywords@keyword'("this").
'lo.comp.keywords@keyword'("import").
'lo.comp.keywords@keyword'("private").
'lo.comp.keywords@keyword'("public").
'lo.comp.keywords@keyword'("contract").
'lo.comp.keywords@keyword'("all").
'lo.comp.keywords@keyword'("exists").
'lo.comp.keywords@keyword'("type").
'lo.comp.keywords@keyword'("implementation").
'lo.comp.keywords@keyword'("void").
'lo.comp.keywords@keyword'("assert").
'lo.comp.keywords@keyword'("show").
'lo.comp.keywords@keyword'("#").
'lo.comp.keywords@isKeyword'(XX):- 'lo.comp.keywords@one182'(XX).
'lo.comp.keywords@ruleKeyword'("@@").
'lo.comp.keywords@ruleKeyword'("~~").
'lo.comp.keywords@ruleKeyword'("=>").
'lo.comp.keywords@ruleKeyword'("<=>").
'lo.comp.keywords@ruleKeyword'("->").
'lo.comp.keywords@ruleKeyword'(":-").
'lo.comp.keywords@ruleKeyword'("-->").
'lo.comp.keywords@ruleKeyword'("::=").
'lo.comp.keywords@ruleKeyword'("<=").
'lo.comp.keywords@ruleKeyword'("<~").
'lo.comp.keywords@isRuleKeyword'(XX):- 'lo.comp.keywords@one183'(XX).
'lo.comp.keywords^keyword'('_call%1'(XV20816), 'lo.comp.keywords^keyword', _):- 'lo.comp.keywords@keyword'(XV20816).
'lo.comp.keywords@one182'(XX):- 'lo.comp.keywords@keyword'(XX),
    !.
'lo.comp.keywords^isKeyword'('_call%1'(XV20817), 'lo.comp.keywords^isKeyword', _):- 'lo.comp.keywords@isKeyword'(XV20817).
'lo.comp.keywords^ruleKeyword'('_call%1'(XV20818), 'lo.comp.keywords^ruleKeyword', _):- 'lo.comp.keywords@ruleKeyword'(XV20818).
'lo.comp.keywords@one183'(XX):- 'lo.comp.keywords@ruleKeyword'(XX),
    !.
'lo.comp.keywords^isRuleKeyword'('_call%1'(XV20819), 'lo.comp.keywords^isRuleKeyword', _):- 'lo.comp.keywords@isRuleKeyword'(XV20819).
