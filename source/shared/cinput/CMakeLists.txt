cmake_minimum_required(VERSION 3.10)
include(${CMAKE_SOURCE_DIR}/config/CMakeLists.txt)

target_link_libraries(${ProjectName}
	ccore)
	
# using GLFW
include_directories(${FurThirdPartyPath}/glfw/include)
target_link_libraries(${ProjectName}
	$<$<CONFIG:Debug>:${FurThirdPartyPath}/glfw/build/src/Debug/glfw3.lib>
	$<$<NOT:$<CONFIG:Debug>>:${FurThirdPartyPath}/glfw/build/src/Release/glfw3.lib>)
