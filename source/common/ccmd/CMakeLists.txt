cmake_minimum_required(VERSION 3.10)

set(FUR_PROJECT_TYPE "executable")
include(${CMAKE_SOURCE_DIR}/config/CMakeLists.txt)

target_link_libraries(${ProjectName}
	ccore
	cmath
	cphysics
	crend
	cimport)
