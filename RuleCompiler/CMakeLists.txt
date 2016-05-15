cmake_minimum_required(VERSION 3.5)
project(ruleC)

find_package(BISON)
find_package(FLEX)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")

BISON_TARGET(RuleParser ruleGrammar.y ${CMAKE_CURRENT_BINARY_DIR}/ruleGrammar.c)
FLEX_TARGET(RuleTokeniser token.l ${CMAKE_CURRENT_BINARY_DIR}/token.c)
ADD_FLEX_BISON_DEPENDENCY(RuleTokeniser RuleParser)

set(SOURCE_FILES
        ${BISON_RuleParser_OUTPUTS}
        ${FLEX_RuleTokeniser_OUTPUTS}
        main.c)
add_executable(ruleC ${SOURCE_FILES})
