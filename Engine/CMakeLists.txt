cmake_minimum_required(VERSION 3.5)
project(loEngine)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")

include_directories("Headers" "../Headers" "../OOlib/Headers")

set(SOURCE_FILES
        args.c fltArith.c intArith.c boot.c call.c char.c class.c clock.c code.c date.c debug.c dict.c dir.c disass.c dynamic.c
        hash.c
        encoded.c envir.c error.c escapes.c eval.c exec.c fileio.c float.c gc.c global.c heap.c lists.c lockvar.c
        misc.c process.c program.c put.c rendezvous.c sha.c signal.c stats.c string.c suspend.c netfile.c
        verify.c writef.c
        main.c

        Headers/word.h
        Headers/char.h Headers/clock.h
        Headers/code.h Headers/cons.h Headers/constants.h Headers/dbgflags.h Headers/debug.h Headers/dict.h Headers/disass.h
        Headers/encoded.h Headers/signature.h Headers/term.h
        Headers/errors.h Headers/esc.h ../Headers/escodes.h Headers/eval.h Headers/lock.h Headers/process.h Headers/ops.h Headers/opcodes.h
        Headers/global.h Headers/heap.h Headers/list.h Headers/local.h Headers/perms.h Headers/vars.h
        Headers/fileio.h Headers/lo.h
        Headers/floats.h Headers/str.h Headers/symbols.h Headers/dll.h
        Headers/unix.h Headers/stats.h Headers/signals.h

        ../Headers/instructions.h ../Headers/config.h ../Headers/escapes.h ../Headers/version.h

        Headers/manifest.h Headers/manifestP.h load.c manifest.c thread.c Headers/thread.h)

add_executable(lo ${SOURCE_FILES})
target_link_libraries(lo ooio)

