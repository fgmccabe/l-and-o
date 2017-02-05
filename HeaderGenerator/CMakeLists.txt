cmake_minimum_required(VERSION 3.5)
project(gen)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("Headers" "../Headers" "../Engine/Headers" "../OOlib/Headers")

set(SOURCE_FILES opcodes.c ../Headers/instructions.h)

add_executable(opcodes ${SOURCE_FILES})

add_executable(ops genoperators.c trie.c ../Headers/operators.h)
target_link_libraries(ops ooio)

set(ESC_SOURCE_FILES genescapes.c ../Headers/escapes.h)
add_executable(genesc ${ESC_SOURCE_FILES})

target_link_libraries(genesc ooio)

set(INS_SOURCE_FILES instr.c ../Headers/instructions.h)
add_executable(genins ${INS_SOURCE_FILES})

target_link_libraries(genins ooio)

set(ASM_SOURCE_FILES mnem.c)
add_executable(mnem ${ASM_SOURCE_FILES})
target_link_libraries(mnem ooio)
