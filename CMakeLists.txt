project(cpptool)
cmake_minimum_required(VERSION 2.8)

set(LLVM_SRC_PATH "/home/alex/llvm/llvm" CACHE STRING "LLVM sources path")
set(LLVM_BUILD_PATH "/home/alex/llvm/build" CACHE STRING "LLVM build path")
set(LLVM_BIN_PATH "${LLVM_BUILD_PATH}/bin" CACHE STRING "LLVM binaries path")

execute_process(COMMAND ${LLVM_BIN_PATH}/llvm-config --cxxflags
  OUTPUT_VARIABLE LLVM_CXXFLAGS
  OUTPUT_STRIP_TRAILING_WHITESPACE)
string(REGEX REPLACE "\n" " " LLVM_CXXFLAGS ${LLVM_CXXFLAGS}) # Also strips newlines
set(LLVM_CXXFLAGS "${LLVM_CXXFLAGS} -fno-rtti")

execute_process(COMMAND ${LLVM_BIN_PATH}/llvm-config --ldflags --libs --system-libs
  OUTPUT_VARIABLE LLVM_LDFLAGS
  OUTPUT_STRIP_TRAILING_WHITESPACE)
string(REGEX REPLACE "\n" " " LLVM_LDFLAGS ${LLVM_LDFLAGS}) # Also strips newlines

set(CLANG_INCLUDES
  ${LLVM_SRC_PATH}/include
  ${LLVM_SRC_PATH}/tools/clang/include
  ${LLVM_BUILD_PATH}/include
  ${LLVM_BUILD_PATH}/tools/clang/include)
 
set(CLANG_LIBS
  -Wl,--start-group
  clangAST
  clangAnalysis
  clangBasic
  clangDriver
  clangEdit
  clangFrontend
  clangFrontendTool
  clangLex
  clangParse
  clangSema
  clangEdit
  clangASTMatchers
  clangRewrite
  clangRewriteFrontend
  clangStaticAnalyzerFrontend
  clangStaticAnalyzerCheckers
  clangStaticAnalyzerCore
  clangSerialization
  clangToolingCore
  clangTooling
  -Wl,--end-group)
 
include_directories(${CLANG_INCLUDES})

add_executable(${PROJECT_NAME} main.cpp)

set_target_properties(${PROJECT_NAME} PROPERTIES
  COMPILE_FLAGS ${LLVM_CXXFLAGS})

# These need to be placed (both libraries and flags) at the end (do not use
# LINK_FLAGS since they're inserted before the .o obj files to link and thus
# since ld is positional no symbols are available when actual objs ask them)
target_link_libraries(${PROJECT_NAME} ${LLVM_LDFLAGS})
target_link_libraries(${PROJECT_NAME} ${CLANG_LIBS})
