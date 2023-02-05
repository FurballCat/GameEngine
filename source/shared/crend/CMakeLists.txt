cmake_minimum_required(VERSION 3.10)
include(${CMAKE_SOURCE_DIR}/config/CMakeLists.txt)

target_link_libraries(${ProjectName}
	ccore
	cmath
	cimport
	glfw)
	
include_directories(${FurThirdPartyPath}/Vulkan/include)

# third party: "vulkan", "moltenvk"
#find_package(VulkanHeaders REQUIRED CONFIG)
#target_link_libraries(${ProjectName} PRIVATE Vulkan::Headers)
#message(STATUS "Vulkan Headers Registry: ${VULKAN_HEADERS_REGISTRY_DIRECTORY}")
#message(STATUS "Vulkan Headers Version: ${VulkanHeaders_VERSION}")
