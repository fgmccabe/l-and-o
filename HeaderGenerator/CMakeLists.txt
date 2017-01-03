cmake_minimum_required(VERSION 3.5)
project(gen)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("Headers" "../Headers" "../Engine/Headers" "../OOlib/Headers")

set(SOURCE_FILES opcodes.c)

add_executable(opcodes ${SOURCE_FILES})

add_executable(ops genoperators.c trie.c)
target_link_libraries(ops ooio)

set(ESC_SOURCE_FILES genescapes.c)
add_executable(genesc ${ESC_SOURCE_FILES})

target_link_libraries(genesc ooio)

set(INS_SOURCE_FILES instr.c)
add_executable(genins ${INS_SOURCE_FILES})

target_link_libraries(genins ooio)
