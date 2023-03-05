cmake_minimum_required(VERSION 3.10)
project(GameEngine)

# use C++11 standard
set (CMAKE_CXX_STANDARD 11)

# suppress generating ZERO_CHECK project
set (CMAKE_SUPPRESS_REGENERATION true)

# enable filters/folders on project level
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# set paths for bin and thirdparty
set(FurThirdPartyPath ${CMAKE_SOURCE_DIR}/thirdparty)
set(FurBinPath ${CMAKE_SOURCE_DIR}/bin)

# set output paths for executables and libraries (not sure it works...)
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin )
set( CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin/lib )

# enable ASM
enable_language(ASM)

# set static linking rather than dynamic linking for MSVC
if(MSVC)
  set(CompilerFlags
    CMAKE_CXX_FLAGS
    CMAKE_CXX_FLAGS_DEBUG
    CMAKE_CXX_FLAGS_RELEASE
	CMAKE_CXX_FLAGS_RELWITHDEBINFO
    CMAKE_C_FLAGS
    CMAKE_C_FLAGS_DEBUG
    CMAKE_C_FLAGS_RELEASE
	CMAKE_C_FLAGS_RELWITHDEBINFO
    )
  foreach(CompilerFlag ${CompilerFlags})
    string(REPLACE "/MD" "/MT" ${CompilerFlag} "${${CompilerFlag}}")
  endforeach()
endif()

file(GLOB_RECURSE SubProjects ${CMAKE_SOURCE_DIR}/source/*CMakeLists.txt)
foreach(SubProj ${SubProjects})
	get_filename_component(SubProjectPath ${SubProj} DIRECTORY)
	cmake_path(GET SubProjectPath FILENAME SubProjectName)
	#message(STATUS "Project=" ${SubProjectPath} " Output=" ${CMAKE_SOURCE_DIR}/intermediate/projects/${SubProjectName})
	add_subdirectory(${SubProjectPath} ${CMAKE_SOURCE_DIR}/intermediate/projects/${SubProjectName})
endforeach()
