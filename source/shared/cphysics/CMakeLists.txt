cmake_minimum_required(VERSION 3.10)
include(${CMAKE_SOURCE_DIR}/config/CMakeLists.txt)

target_link_libraries(${ProjectName}
	ccore
	cmath)

# third party: "physx", "physx_common", "physx_foundation", "physx_extensions", "physx_character_kinematic"
