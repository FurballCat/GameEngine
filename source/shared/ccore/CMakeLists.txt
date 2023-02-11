cmake_minimum_required(VERSION 3.10)
include(${CMAKE_SOURCE_DIR}/config/CMakeLists.txt)

# manually add fcontext assember code
if(WIN32)
file(GLOB FCONTEXT_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/internal/fcontext/*.asm)
elseif(APPLE)
file(GLOB FCONTEXT_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/internal/fcontext/*.s)
endif()

# add fcontext to the library so it's compiled
target_sources(${ProjectName} PRIVATE ${FCONTEXT_FILES})

# set assembler language: MASM for Windows and ASM for Mac
if(WIN32)
set_source_files_properties(${FCONTEXT_FILES} PROPERTIES LANGUAGE ASM_MASM)
elseif(APPLE)
set_source_files_properties(${FCONTEXT_FILES} PROPERTIES LANGUAGE ASM)
endif()

# create filter in project for fcontext
source_group("src/internal/fcontext" FILES ${FCONTEXT_FILES})
