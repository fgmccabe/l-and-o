cmake_minimum_required(VERSION 3.5)
project(asm)

find_package(BISON)
find_package(FLEX)

BISON_TARGET(AsmParser grammar.y ${CMAKE_CURRENT_BINARY_DIR}/grammar.c)
FLEX_TARGET(AsmTokeniser token.l ${CMAKE_CURRENT_BINARY_DIR}/token.c)
ADD_FLEX_BISON_DEPENDENCY(AsmTokeniser AsmParser)


set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")

include_directories("../Headers" "../Engine/Headers" "../OOlib/Headers" "Headers")

set(SOURCE_FILES
        class.c
        file.c
        files.c
        formin.c
        formout.c
        hashTable.c
        hosts.c
        io.c
        ixTree.c
        lists.c
        locked.c
        managed.c
        object.c
        pair.c
        parseuri.c
        pipe.c
        pool.c
        sockets.c
        stringBuffer.c
        stringReader.c
        strio.c
        timer.c
        unichar.c
        unicode.c
        unitable.c
        uri.c
        utils.c

        ${BISON_CatalogParser_OUTPUTS}
        ${FLEX_CatalogTokeniser_OUTPUTS}
        )

add_executable(asm ${SOURCE_FILES})
target_link_libraries(asm ooio)

