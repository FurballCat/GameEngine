cmake_minimum_required(VERSION 3.10)
include(${CMAKE_SOURCE_DIR}/config/CMakeLists.txt)

target_link_libraries(${ProjectName}
	ccore
	cmath
	cimport)

# using GLFW
include_directories(${FurThirdPartyPath}/glfw/include)
target_link_libraries(${ProjectName} ${FurThirdPartyPath}/glfw/build/src/Release/glfw3.lib)

# using Vulkan includes from third party folder	
include_directories(${FurThirdPartyPath}/Vulkan/include)

# third party: "vulkan", "moltenvk"
# paths to the libs (note: if you can't find them, then make sure to install Vulkan SDK and set env variable)
message(STATUS "Vulkan SDK Path: $ENV{VULKAN_SDK}")
set(VULKAN_SDK_LIB_PATH "$ENV{VULKAN_SDK}/Lib")

# link Vulkan SDK libs
target_link_directories(${ProjectName} PUBLIC ${VULKAN_SDK_LIB_PATH})
target_link_libraries(${ProjectName} vulkan-1)
