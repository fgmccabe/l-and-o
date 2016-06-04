cmake_minimum_required(VERSION 3.5)
project(loEngine)

find_package(BISON)
find_package(FLEX)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")

include_directories("Headers" "../Headers" "../OOlib/Headers")

set(SOURCE_FILES
        args.c arith.c boot.c call.c char.c class.c clock.c code.c date.c debug.c dict.c dir.c disass.c dynamic.c
        hash.c
        encoded.c envir.c error.c escapes.c eval.c exec.c fileio.c float.c gc.c global.c heap.c lists.c lockvar.c
        misc.c process.c program.c put.c rendezvous.c sha.c signal.c stats.c string.c suspend.c netfile.c
        verify.c writef.c
        main.c)

add_executable(lo ${SOURCE_FILES})
target_link_libraries(lo ooio)

