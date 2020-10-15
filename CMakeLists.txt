cmake_minimum_required(VERSION 3.7)
project(Prdm9)

set(CMAKE_CXX_STANDARD 14)

# Compilation options
option(DEBUG_MODE "Debug mode (with asserts and such) " OFF) #OFF by default
set(ALWAYSON_CXX_FLAGS "--std=c++14 -Wall -Wextra -Wno-unused-parameter -Wfatal-errors ")
if(DEBUG_MODE)
    set(CMAKE_CXX_FLAGS "-O2 -g ${ALWAYSON_CXX_FLAGS}") # debug mode
    message("-- INFO: Compiling in debug mode.\n-- INFO: flags are: " ${CMAKE_CXX_FLAGS})
else()
    set(CMAKE_CXX_FLAGS "-O3 -DNDEBUG ${ALWAYSON_CXX_FLAGS}") # release mode
    message("-- INFO: Compiling in release mode.\n-- INFO: flags are: " ${CMAKE_CXX_FLAGS})
endif(DEBUG_MODE)

include_directories("src")

add_executable(Prdm9 src/prdm9.cpp)
add_executable(Simple src/simple_prdm9.cpp)
