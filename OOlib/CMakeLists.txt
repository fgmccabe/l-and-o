cmake_minimum_required(VERSION 3.5)
project(OOIO)

find_package(BISON)
find_package(FLEX)

BISON_TARGET(CatalogParser catgrammar.y ${CMAKE_CURRENT_BINARY_DIR}/catgrammar.c)
FLEX_TARGET(CatalogTokeniser token.l ${CMAKE_CURRENT_BINARY_DIR}/token.c)
ADD_FLEX_BISON_DEPENDENCY(CatalogTokeniser CatalogParser)


set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c99")

include_directories("../Headers" "Headers")

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

add_library(ooio STATIC ${SOURCE_FILES})

install(TARGETS ooio DESTINATION lib)
install(FILES integer.h logical.h retcode.h unicode.h DESTINATION include)
