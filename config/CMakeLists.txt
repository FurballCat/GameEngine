
get_filename_component(ProjectName ${CMAKE_CURRENT_SOURCE_DIR} NAME)
message(STATUS "Project: " ${ProjectName})

project(${ProjectName})

# use C++11 standard
set (CMAKE_CXX_STANDARD 11)
set (CMAKE_C_STANDARD 17)

# suppress generating ZERO_CHECK project
set (CMAKE_SUPPRESS_REGENERATION true)

# add source files
file(GLOB SOURCE_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/*.c
	${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)
	
# add header files
file(GLOB HEADER_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/*.h
	${CMAKE_CURRENT_SOURCE_DIR}/*.hpp)
	
# add source files
file(GLOB INTERNAL_SOURCE_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/internal/*.c
	${CMAKE_CURRENT_SOURCE_DIR}/internal/*.cpp)
	
# add header files
file(GLOB INTERNAL_HEADER_FILES
	${CMAKE_CURRENT_SOURCE_DIR}/internal/*.h
	${CMAKE_CURRENT_SOURCE_DIR}/internal/*.hpp)

if(DEFINED FUR_PROJECT_TYPE)
	if(${FUR_PROJECT_TYPE} STREQUAL "executable")
		add_executable(${ProjectName} ${HEADER_FILES} ${SOURCE_FILES} ${INTERNAL_HEADER_FILES} ${INTERNAL_SOURCE_FILES} )
	else()
		add_library(${ProjectName} ${HEADER_FILES} ${SOURCE_FILES} ${INTERNAL_HEADER_FILES} ${INTERNAL_SOURCE_FILES} )
	endif()
else()
	add_library(${ProjectName} ${HEADER_FILES} ${SOURCE_FILES} ${INTERNAL_HEADER_FILES} ${INTERNAL_SOURCE_FILES} )
endif()

# exclude from ALL_BUILD
set_property(TARGET ${ProjectName} PROPERTY EXCLUDE_FROM_ALL ON)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/internal)
include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/..)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../../shared)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../../common)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../../game)

source_group("src" FILES ${HEADER_FILES} ${SOURCE_FILES})
source_group("src/internal" FILES ${INTERNAL_HEADER_FILES} ${INTERNAL_SOURCE_FILES})

if(WIN32)
	add_compile_definitions(_CRT_SECURE_NO_WARNINGS)
	add_compile_definitions(PLATFORM_WINDOWS)
	add_compile_definitions(_GLFW_WIN32)
elseif(APPLE)
	add_compile_definitions(PLATFORM_OSX)
	add_compile_definitions(_GLFW_COCOA)
endif()

# set project parent folder/filter
get_filename_component(SubProjectPath ${CMAKE_CURRENT_SOURCE_DIR} DIRECTORY)
	cmake_path(GET SubProjectPath FILENAME SubFolderName)
set_target_properties(${ProjectName} PROPERTIES FOLDER ${SubFolderName})

# set output directories for binaries
set_target_properties(${ProjectName} PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY "${FurBinPath}/lib"
	RUNTIME_OUTPUT_DIRECTORY "${FurBinPath}")
	