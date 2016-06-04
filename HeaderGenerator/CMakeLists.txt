cmake_minimum_required(VERSION 3.5)
project(gen)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c99")

include_directories("Headers" "../Headers" "../Engine/Headers" "../OOlib/Headers")

set(SOURCE_FILES opcodes.c)

add_executable(opcodes ${SOURCE_FILES})

add_executable(genops genoperators.c trie.c)
target_link_libraries(genops ooio)
