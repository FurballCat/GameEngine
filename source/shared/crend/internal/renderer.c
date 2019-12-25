/* Copyright (c) 2016-2019 Furball Cat */

#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "renderer.h"
#include "vulkansdk/macOS/include/vulkan/vulkan.h"
#include "glfw/glfw3.h"

#include "renderBuffer.h"
#include "renderUtils.h"

// stb library
#ifdef __clang__
#define STBIDEF static inline
#endif

#define STB_IMAGE_STATIC
#define STB_IMAGE_IMPLEMENTATION

#include "stb_image.h"
//-----

#include <math.h>
#include "3dmath.h"

#define FUR_ASSERT(x) assert(x)

#define FUR_RENDER_MEMORY_DEBUG 0

#define MAX(a,b) \
	({ __typeof__ (a) _a = (a); \
	__typeof__ (b) _b = (b); \
	_a > _b ? _a : _b; })

#define MIN(a,b) \
	({ __typeof__ (a) _a = (a); \
	__typeof__ (b) _b = (b); \
	_a < _b ? _a : _b; })

void* furAlloc(size_t size, size_t alignment,
				 enum fr_memory_scope_t scope, const char* info)
{
	return malloc(size);
}

void furDealloc(void* pMemory, const char* info)
{
	free(pMemory);
}

void* furAllocateWithFallback(struct fr_allocation_callbacks_t* pAllocCallbacks, size_t size, size_t alignment,
						 enum fr_memory_scope_t scope, const char* info)
{
#if FUR_RENDER_MEMORY_DEBUG == 0
	if(pAllocCallbacks)
		return pAllocCallbacks->pfnAllocate(pAllocCallbacks->pUserData, size, alignment, scope);
#endif
	
	return furAlloc(size, alignment, scope, info);
}

void furDeallocWithFallback(struct fr_allocation_callbacks_t* pAllocCallbacks, void* pMemory, const char* info)
{
#if FUR_RENDER_MEMORY_DEBUG == 0
	if(pAllocCallbacks)
		return pAllocCallbacks->pfnFree(pAllocCallbacks->pUserData, pMemory);
#endif
	
	furDealloc(pMemory, info);
}

#define S1(x) #x
#define S2(x) S1(x)

#if FUR_RENDER_MEMORY_DEBUG == 0
	#define FUR_ALLOC(_size, _alignment, _scope, _pAllocCallbacks)	\
		furAllocateWithFallback(_pAllocCallbacks, _size, _alignment, _scope, "")
	#define FUR_FREE(_pMemory, _pAllocCallbacks)	\
		furDeallocWithFallback(_pAllocCallbacks, _pMemory, "")
#else
	#define FUR_ALLOC(_size, _alignment, _scope, _pAllocCallbacks)	\
		furAllocateWithFallback(_pAllocCallbacks, _size, _alignment, _scope, __FILE__ ":" S2(__LINE__))
	#define FUR_FREE(_pMemory, _pAllocCallbacks)	\
		furDeallocWithFallback(_pAllocCallbacks, _pMemory, __FILE__ ":" S2(__LINE__))
#endif

/*************************************************************/

const char* g_lastError = "";

const char* fr_get_last_error(void)
{
	return g_lastError;
}

void fur_set_last_error(const char* error)
{
	g_lastError = error;
}

#define FUR_CASE_ENUM_TO_CSTR(value, details) case value: return #value details; break;

const char* frInterpretVulkanResult(VkResult result)
{
	switch(result)
	{
			FUR_CASE_ENUM_TO_CSTR(VK_SUCCESS, "");
			FUR_CASE_ENUM_TO_CSTR(VK_NOT_READY, "");
			FUR_CASE_ENUM_TO_CSTR(VK_TIMEOUT, "");
			FUR_CASE_ENUM_TO_CSTR(VK_EVENT_SET, "");
			FUR_CASE_ENUM_TO_CSTR(VK_EVENT_RESET, "");
			FUR_CASE_ENUM_TO_CSTR(VK_INCOMPLETE, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_OUT_OF_HOST_MEMORY, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_OUT_OF_DEVICE_MEMORY, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_INITIALIZATION_FAILED, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_DEVICE_LOST, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_MEMORY_MAP_FAILED, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_LAYER_NOT_PRESENT, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_EXTENSION_NOT_PRESENT, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_FEATURE_NOT_PRESENT, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_INCOMPATIBLE_DRIVER, " - probably missing Vulkan SDK in app package. See assets/vulkan_files_to_copy.");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_TOO_MANY_OBJECTS, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_FORMAT_NOT_SUPPORTED, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_FRAGMENTED_POOL, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_OUT_OF_POOL_MEMORY, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_INVALID_EXTERNAL_HANDLE, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_SURFACE_LOST_KHR, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_NATIVE_WINDOW_IN_USE_KHR, "");
			FUR_CASE_ENUM_TO_CSTR(VK_SUBOPTIMAL_KHR, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_OUT_OF_DATE_KHR, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_INCOMPATIBLE_DISPLAY_KHR, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_VALIDATION_FAILED_EXT, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_INVALID_SHADER_NV, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_FRAGMENTATION_EXT, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_NOT_PERMITTED_EXT, "");
			FUR_CASE_ENUM_TO_CSTR(VK_ERROR_INVALID_DEVICE_ADDRESS_EXT, "");
		default:
			break;
	}
	
	return "";
}

/*************************************************************/

struct fr_app_t
{
	const char* title;
	uint32_t viewportWidth;
	uint32_t viewportHeight;
	
	GLFWwindow* pWindow;
};

enum fr_result_t fr_create_app(const struct fr_app_desc_t* pDesc,
									struct fr_app_t** ppApp,
									struct fr_allocation_callbacks_t* pAllocCallbacks)
{
	struct fr_app_t* pApp = FUR_ALLOC(sizeof(struct fr_app_t), 8, FR_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
	
	pApp->title = pDesc->appTitle;
	pApp->viewportWidth = pDesc->viewportWidth;
	pApp->viewportHeight = pDesc->viewportHeight;
	
	glfwInit();
	
	glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
	pApp->pWindow = glfwCreateWindow(pApp->viewportWidth, pApp->viewportHeight, pApp->title, NULL, NULL);
	
	if(pApp->pWindow == NULL)
	{
		fur_set_last_error("Can't create window.");
		return FR_RESULT_ERROR;
	}
	
	*ppApp = pApp;
	
	return FR_RESULT_OK;
}

enum fr_result_t fr_release_app(struct fr_app_t* pApp,
									 struct fr_allocation_callbacks_t* pAllocCallbacks)
{
	glfwDestroyWindow(pApp->pWindow);
	glfwTerminate();
	
	FUR_FREE(pApp, pAllocCallbacks);
	
	return FR_RESULT_OK;
}

uint32_t fr_update_app(struct fr_app_t* pApp)
{
	if(!glfwWindowShouldClose(pApp->pWindow))
	{
		glfwPollEvents();
		return 1;
	}
	
	return 0;
}

/*************************************************************/

struct fr_binary_buffer_t
{
	void* pData;
	size_t size;
};

enum fr_result_t fr_load_binary_file_into_binary_buffer(const char* path, struct fr_binary_buffer_t* pBuffer, struct fr_allocation_callbacks_t* pAllocCallbacks)
{
	FILE* pFile = fopen(path, "rb");
	if(pFile && pBuffer)
	{
		fseek(pFile, 0, SEEK_END);
		size_t size = ftell(pFile);
		fseek(pFile, 0, SEEK_SET);
		
		pBuffer->pData = FUR_ALLOC(size, 8, FR_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
		pBuffer->size = size;
		
		fread(pBuffer->pData, size, 1, pFile);
		fclose(pFile);
		
		return FR_RESULT_OK;
	}
	
	fur_set_last_error(path);
	return FR_RESULT_ERROR;
}

void fr_release_binary_buffer(struct fr_binary_buffer_t* pBuffer, struct fr_allocation_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(pBuffer->pData, pAllocCallbacks);
}

/*************************************************************/

enum fr_result_t fr_create_shader_module(VkDevice device, const char* path, VkShaderModule* pShader, struct fr_allocation_callbacks_t* pAllocCallbacks)
{
	struct fr_binary_buffer_t buffer;
	memset(&buffer, 0, sizeof(struct fr_binary_buffer_t));
	
	enum fr_result_t res = fr_load_binary_file_into_binary_buffer(path, &buffer, pAllocCallbacks);
	if(res == FR_RESULT_OK)
	{
		VkShaderModuleCreateInfo createInfo = {};
		createInfo.sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
		createInfo.codeSize = buffer.size;
		createInfo.pCode = buffer.pData;
		
		VkResult res = vkCreateShaderModule(device, &createInfo, NULL, pShader);
		fr_release_binary_buffer(&buffer, pAllocCallbacks);
		
		if (res != VK_SUCCESS)
		{
			fur_set_last_error(path);
			return FR_RESULT_ERROR_SHADER_MODULE_CREATION;
		}
		
		return FR_RESULT_OK;
	}
	else
	{
		return res;
	}
}

/*************************************************************/

typedef struct fr_vec2_t
{
	float x, y;
} fr_vec2_t;

typedef struct fr_vec3_t
{
	float x, y, z;
} fr_vec3_t;

typedef struct fr_vertex_t
{
	fr_vec2_t position;
	fr_vec3_t color;
} fr_vertex_t;

const uint32_t g_numTestVertices = 4;
const fr_vertex_t g_testGeometry[g_numTestVertices] = {
	{{-0.5f, -0.5f}, {1.0f, 0.0f, 0.0f}},
	{{0.5f, -0.5f}, {0.0f, 1.0f, 0.0f}},
	{{0.5f, 0.5f}, {0.0f, 0.0f, 1.0f}},
	{{-0.5f, 0.5f}, {1.0f, 1.0f, 1.0f}}};

const uint32_t g_numTestIndices = 6;
const uint16_t g_testIndices[g_numTestIndices] = {0, 1, 2, 2, 3, 0};

typedef struct fr_uniform_buffer_t
{
	fm_mat4_t model;
	fm_mat4_t view;
	fm_mat4_t proj;
} fr_uniform_buffer_t;

const char* g_texturePath = "../../../../../assets/test_texture.png";
const uint32_t g_textureWidth = 1024;
const uint32_t g_textureHeight = 1024;

/*************************************************************/

#define NUM_SWAP_CHAIN_IMAGES 3

struct fr_renderer_t
{
	VkInstance vkInstance;
	VkPhysicalDevice physicalDevice;
	VkDevice device;
	
	uint32_t idxQueueGraphics;
	uint32_t idxQueuePresent;
	
	VkQueue graphicsQueue;
	VkQueue presentQueue;
	
	struct fr_app_t* pApp;
	VkSurfaceKHR surface;
	VkSwapchainKHR swapChain;
	VkFormat swapChainSurfaceFormat;
	VkExtent2D swapChainExtent;
	
	VkImage aSwapChainImages[NUM_SWAP_CHAIN_IMAGES];
	VkImageView aSwapChainImagesViews[NUM_SWAP_CHAIN_IMAGES];
	
	VkShaderModule vertexShaderModule;
	VkShaderModule fragmentShaderModule;
	
	VkRenderPass renderPass;
	VkDescriptorSetLayout descriptorSetLayout;	// for uniform buffer
	VkPipelineLayout pipelineLayout;
	VkPipeline graphicsPipeline;
	
	VkFramebuffer aSwapChainFrameBuffers[NUM_SWAP_CHAIN_IMAGES];
	
	VkCommandPool commandPool;
	VkCommandBuffer aCommandBuffers[NUM_SWAP_CHAIN_IMAGES];
	
	VkCommandPool stagingCommandPool;
	VkCommandBuffer stagingCommandBuffer;
	
	VkSemaphore imageAvailableSemaphore;
	VkSemaphore renderFinishedSemaphore;
	
	// test geometry
	VkVertexInputBindingDescription bindingDescription[2];
	VkVertexInputAttributeDescription vertexAttributes[2];
	
	VkBuffer vertexBuffer;
	VkDeviceMemory vertexBufferMemory;
	
	VkBuffer colorVertexBuffer[NUM_SWAP_CHAIN_IMAGES];
	VkDeviceMemory colorVertexBufferMemory[NUM_SWAP_CHAIN_IMAGES];
	
	VkBuffer indexBuffer;
	VkDeviceMemory indexBufferMemory;
	
	VkImage textureImage;
	VkDeviceMemory textureImageMemory;
	
	VkBuffer stagingBuffer;
	VkDeviceMemory stagingBufferMemory;
	
	VkBuffer aUniformBuffer[NUM_SWAP_CHAIN_IMAGES];
	VkDeviceMemory aUniformBufferMemory[NUM_SWAP_CHAIN_IMAGES];
	
	VkDescriptorPool descriptorPool;
	VkDescriptorSet aDescriptorSets[NUM_SWAP_CHAIN_IMAGES];
	
	// color buffer
	fr_vec3_t* vertexColors[NUM_SWAP_CHAIN_IMAGES];
	
	float rotationAngle;
};

void fr_pixels_free_func(void* pData, size_t size, void* pUserData)
{
	stbi_uc* pixels = (stbi_uc*)pData;
	stbi_image_free(pixels);
}

void fr_generic_buffer_free_func(void* pData, size_t size, void* pUserData)
{
	struct fr_allocation_callbacks_t* pAllocCallbacks = (struct fr_allocation_callbacks_t*)pUserData;
	
	FUR_FREE(pData, pAllocCallbacks);
}

enum fr_result_t fr_create_renderer(const struct fr_renderer_desc_t* pDesc,
					   struct fr_renderer_t** ppRenderer,
					   struct fr_allocation_callbacks_t*	pAllocCallbacks)
{
	struct fr_renderer_t* pRenderer = FUR_ALLOC(sizeof(struct fr_renderer_t), 8, FR_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
	if(!pRenderer)
	{
		fur_set_last_error("Can't allocate renderer.");
		return FR_RESULT_ERROR;
	}
	
	memset(pRenderer, 0, sizeof(struct fr_renderer_t));
	
	enum fr_result_t res = FR_RESULT_OK;
	
	// create vulkan instance
	if(res == FR_RESULT_OK)
	{
		pRenderer->pApp = pDesc->pApp;
		
		VkApplicationInfo appInfo = {};
		appInfo.sType = VK_STRUCTURE_TYPE_APPLICATION_INFO;
		appInfo.pApplicationName = pDesc->pApp->title;
		appInfo.applicationVersion = VK_MAKE_VERSION(1, 0, 0);
		appInfo.pEngineName = "Furball Cat Game Engine";
		appInfo.engineVersion = VK_MAKE_VERSION(1, 0, 0);
		appInfo.apiVersion = VK_API_VERSION_1_0;
		
		VkInstanceCreateInfo createInfo = {};
		createInfo.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
		createInfo.pApplicationInfo = &appInfo;
		
		uint32_t glfwExtensionCount = 0;
		const char** glfwExtensions;
		
		glfwExtensions = glfwGetRequiredInstanceExtensions(&glfwExtensionCount);
		
		createInfo.enabledExtensionCount = glfwExtensionCount;
		createInfo.ppEnabledExtensionNames = glfwExtensions;
		
		createInfo.enabledLayerCount = 0;
		
		VkResult result = vkCreateInstance(&createInfo, NULL, &pRenderer->vkInstance);
		
		if(result != VK_SUCCESS)
		{
			fur_set_last_error(frInterpretVulkanResult(result));
			res = FR_RESULT_ERROR;
		}
	}
	
	// enumerate extensions
	if(res == FR_RESULT_OK)
	{
		uint32_t extensionCount = 0;
		vkEnumerateInstanceExtensionProperties(NULL, &extensionCount, NULL);
		
		//struct VkExtensionProperties* aProperties = FUR_ALLOC(extensionCount * sizeof(struct VkExtensionProperties), 8, RENDER_MEMORY_DEFAULT, pAllocCallbacks);
		
		//FUR_FREE(aProperties, pAllocCallbacks);
	}
	
	// create physical device
	if(res == FR_RESULT_OK)
	{
		VkPhysicalDevice physicalDevice = VK_NULL_HANDLE;
		
		uint32_t numDevices = 0;
		vkEnumeratePhysicalDevices(pRenderer->vkInstance, &numDevices, NULL);
		
		VkPhysicalDevice* devices = FUR_ALLOC(numDevices * sizeof(VkPhysicalDevice), 8, FR_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
		
		vkEnumeratePhysicalDevices(pRenderer->vkInstance, &numDevices, devices);
		
		for(uint32_t i=0; i<numDevices; ++i)
		{
			VkPhysicalDevice device = devices[i];
			
			VkPhysicalDeviceProperties deviceProperties;
			VkPhysicalDeviceFeatures deviceFeatures;
			vkGetPhysicalDeviceProperties(device, &deviceProperties);
			vkGetPhysicalDeviceFeatures(device, &deviceFeatures);
			
			// note: Apple Metal doesn't support geometry shaders yet, so on Mac it won't have that feature
			// this is because Vulkan on Mac (MoltenVK) is using Metal under the hood.
			if(deviceProperties.deviceType == VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU)
			{
				physicalDevice = device;
				break;
			}
		}
		
		FUR_FREE(devices, pAllocCallbacks);
		
		if(!physicalDevice)
		{
			fur_set_last_error("Cannot find suitable GPU device.");
			res = FR_RESULT_ERROR;
		}
		
		pRenderer->physicalDevice = physicalDevice;
	}
	
	// create window surface
	if(res == FR_RESULT_OK)
	{
		VkResult result = glfwCreateWindowSurface(pRenderer->vkInstance, pRenderer->pApp->pWindow, NULL, &pRenderer->surface);
		
		if(result != VK_SUCCESS)
		{
			fur_set_last_error(frInterpretVulkanResult(result));
			res = FR_RESULT_ERROR;
		}
	}
	
	VkPhysicalDevice physicalDevice = pRenderer->physicalDevice;
	uint32_t idxQueueGraphics = -1;
	uint32_t idxQueuePresent = -1;
	
	// enumerate extensions
	if(res == FR_RESULT_OK)
	{
		uint32_t numExtensions = 0;
		vkEnumerateDeviceExtensionProperties(physicalDevice, NULL, &numExtensions, NULL);
		
		VkExtensionProperties* extensions = FUR_ALLOC(numExtensions * sizeof(struct VkExtensionProperties), 8, FR_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
		
		vkEnumerateDeviceExtensionProperties(physicalDevice, NULL, &numExtensions, extensions);
		
		bool extensionsFound = false;
		for(uint32_t i=0; i<numExtensions; ++i)
		{
			if(strcmp(extensions[i].extensionName, VK_KHR_SWAPCHAIN_EXTENSION_NAME) == 0)
			{
				extensionsFound = true;
				break;
			}
		}
		
		FUR_FREE(extensions, pAllocCallbacks);
		
		if(!extensionsFound)
		{
			fur_set_last_error("Cannot find required extensions");
			res = FR_RESULT_ERROR;
		}
	}
	
	// enumerate queue families
	if(res == FR_RESULT_OK)
	{
		uint32_t numQueueFamilies = 0;
		vkGetPhysicalDeviceQueueFamilyProperties(physicalDevice, &numQueueFamilies, NULL);
		
		VkQueueFamilyProperties* queueFamilies = FUR_ALLOC(numQueueFamilies * sizeof(struct VkQueueFamilyProperties), 8, FR_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
		
		vkGetPhysicalDeviceQueueFamilyProperties(physicalDevice, &numQueueFamilies, queueFamilies);
		
		for(uint32_t i=0; i<numQueueFamilies; ++i)
		{
			if(queueFamilies[i].queueCount > 0 && queueFamilies[i].queueFlags & VK_QUEUE_GRAPHICS_BIT)
			{
				idxQueueGraphics = i;
			}
		}
		
		FUR_FREE(queueFamilies, pAllocCallbacks);
		
		if(idxQueueGraphics == -1)
		{
			fur_set_last_error("Cannot find graphics device with suitable queue family.");
			res = FR_RESULT_ERROR;
		}
		
		pRenderer->idxQueueGraphics = idxQueueGraphics;
		
		const float queuePriority = 1.0f;	// range: 0.0 to 1.0
		
		// get surface queue
		for(uint32_t i=0; i<numQueueFamilies; ++i)
		{
			VkBool32 presentSupport = false;
			vkGetPhysicalDeviceSurfaceSupportKHR(physicalDevice, i, pRenderer->surface, &presentSupport);
			
			if(presentSupport)
			{
				idxQueuePresent = (int32_t)i;
				break;
			}
		}
		
		if(idxQueuePresent == -1)
		{
			fur_set_last_error("Can't find present queue for surface");
			res = FR_RESULT_ERROR;
		}
		
		pRenderer->idxQueuePresent = idxQueuePresent;
		
		VkDeviceQueueCreateInfo queueCreateInfo[2] = {};
		queueCreateInfo[0].sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
		queueCreateInfo[0].queueFamilyIndex = idxQueueGraphics;
		queueCreateInfo[0].queueCount = 1;
		queueCreateInfo[0].pQueuePriorities = &queuePriority;
		
		queueCreateInfo[1].sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
		queueCreateInfo[1].queueFamilyIndex = idxQueuePresent;
		queueCreateInfo[1].queueCount = 1;
		queueCreateInfo[1].pQueuePriorities = &queuePriority;
		
		VkPhysicalDeviceFeatures deviceFeatures = {};
		
		VkDeviceCreateInfo createInfo = {};
		createInfo.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
		createInfo.pQueueCreateInfos = queueCreateInfo;
		createInfo.queueCreateInfoCount = 2;
		createInfo.pEnabledFeatures = &deviceFeatures;
		createInfo.enabledExtensionCount = 0;
		
		const char* extensionNames[] =
		{
			VK_KHR_SWAPCHAIN_EXTENSION_NAME
		};
		
		createInfo.enabledExtensionCount = 1;
		createInfo.ppEnabledExtensionNames = extensionNames;
		
		const bool enableValidationLayers = false;
		
		if (enableValidationLayers)
		{
			//createInfo.enabledLayerCount = numValidationLayers;
			//createInfo.ppEnabledLayerNames = aValidationLayers;
		}
		else
		{
			createInfo.enabledLayerCount = 0;
		}
		
		if (vkCreateDevice(physicalDevice, &createInfo, NULL, &pRenderer->device) != VK_SUCCESS)
		{
			fur_set_last_error("Cannot create logical device");
			res = FR_RESULT_ERROR;
		}
		
		vkGetDeviceQueue(pRenderer->device, idxQueueGraphics, 0, &pRenderer->graphicsQueue);
		vkGetDeviceQueue(pRenderer->device, idxQueuePresent, 0, &pRenderer->presentQueue);
	}
	
	// create swap chain
	if(res == FR_RESULT_OK)
	{
		VkSurfaceCapabilitiesKHR surfaceCapabilities;
		vkGetPhysicalDeviceSurfaceCapabilitiesKHR(pRenderer->physicalDevice, pRenderer->surface, &surfaceCapabilities);
		
		const uint32_t imageCount = surfaceCapabilities.maxImageCount > 1 ? surfaceCapabilities.minImageCount + 1 : surfaceCapabilities.minImageCount;
		VkExtent2D extent;
		extent.width = MAX(surfaceCapabilities.minImageExtent.width,
						   MIN(surfaceCapabilities.maxImageExtent.width, pRenderer->pApp->viewportWidth));
		extent.height = MAX(surfaceCapabilities.minImageExtent.height,
							MIN(surfaceCapabilities.maxImageExtent.height, pRenderer->pApp->viewportHeight));
		
		const VkFormat surfaceFormat = VK_FORMAT_B8G8R8A8_UNORM;
		
		VkSwapchainCreateInfoKHR createInfo = {};
		createInfo.sType = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
		createInfo.surface = pRenderer->surface;
		createInfo.minImageCount = imageCount;
		createInfo.imageFormat = surfaceFormat;
		createInfo.imageColorSpace = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR;
		createInfo.imageExtent = extent;
		createInfo.imageArrayLayers = 1;
		createInfo.imageUsage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
		
		uint32_t queueIndices[2] = {idxQueueGraphics, idxQueuePresent};
		
		if (idxQueueGraphics != idxQueuePresent)
		{
			createInfo.imageSharingMode = VK_SHARING_MODE_CONCURRENT;
			createInfo.queueFamilyIndexCount = 2;
			createInfo.pQueueFamilyIndices = queueIndices;
		}
		else
		{
			createInfo.imageSharingMode = VK_SHARING_MODE_EXCLUSIVE;
			createInfo.queueFamilyIndexCount = 0; // Optional
			createInfo.pQueueFamilyIndices = NULL; // Optional
		}
		
		createInfo.preTransform = surfaceCapabilities.currentTransform;
		createInfo.compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
		
		createInfo.presentMode = VK_PRESENT_MODE_IMMEDIATE_KHR;		// todo: check also VK_PRESENT_MODE_MAILBOX_KHR
		createInfo.clipped = VK_TRUE;
		
		createInfo.oldSwapchain = VK_NULL_HANDLE;
		
		if(vkCreateSwapchainKHR(pRenderer->device, &createInfo, NULL, &pRenderer->swapChain) != VK_SUCCESS)
		{
			fur_set_last_error("Cannot create swap chain");
			res = FR_RESULT_ERROR;
		}
		
		uint32_t numSwapChainImages = 0;
		vkGetSwapchainImagesKHR(pRenderer->device, pRenderer->swapChain, &numSwapChainImages, NULL);
		
		FUR_ASSERT(numSwapChainImages == NUM_SWAP_CHAIN_IMAGES);
		vkGetSwapchainImagesKHR(pRenderer->device, pRenderer->swapChain, &numSwapChainImages, pRenderer->aSwapChainImages);
		
		pRenderer->swapChainExtent = extent;
		pRenderer->swapChainSurfaceFormat = surfaceFormat;
	}
	
	// create swap chain image views
	if(res == FR_RESULT_OK)
	{
		for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			VkImageViewCreateInfo createInfo = {};
			createInfo.sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
			createInfo.image = pRenderer->aSwapChainImages[i];
			
			createInfo.viewType = VK_IMAGE_VIEW_TYPE_2D;
			createInfo.format = VK_FORMAT_B8G8R8A8_UNORM;
			
			createInfo.components.r = VK_COMPONENT_SWIZZLE_IDENTITY;
			createInfo.components.g = VK_COMPONENT_SWIZZLE_IDENTITY;
			createInfo.components.b = VK_COMPONENT_SWIZZLE_IDENTITY;
			createInfo.components.a = VK_COMPONENT_SWIZZLE_IDENTITY;
			
			createInfo.subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
			createInfo.subresourceRange.baseMipLevel = 0;
			createInfo.subresourceRange.levelCount = 1;
			createInfo.subresourceRange.baseArrayLayer = 0;
			createInfo.subresourceRange.layerCount = 1;
			
			if (vkCreateImageView(pRenderer->device, &createInfo, NULL, &pRenderer->aSwapChainImagesViews[i]) != VK_SUCCESS)
			{
				fur_set_last_error("Cannot create swap chain image views");
				res = FR_RESULT_ERROR;
			}
		}
	}
	
	// create shader libraries
	
	// todo: remove that, paths should be passed or something
	// note: paths for mac when using fopen requires additional "../../../" because of bundle
	const char* basicVertexShaderPath = "../../../../../shaders/vert.spv";
	const char* basicFragmentShaderPath = "../../../../../shaders/frag.spv";
	
	if(res == FR_RESULT_OK)
	{
		res = fr_create_shader_module(pRenderer->device, basicVertexShaderPath, &pRenderer->vertexShaderModule, pAllocCallbacks);
	}
	
	if(res == FR_RESULT_OK)
	{
		res = fr_create_shader_module(pRenderer->device, basicFragmentShaderPath, &pRenderer->fragmentShaderModule, pAllocCallbacks);
	}
	
	// test geometry
	if(res == FR_RESULT_OK)
	{
		pRenderer->bindingDescription[0].binding = 0;
		pRenderer->bindingDescription[0].stride = sizeof(fr_vertex_t);
		pRenderer->bindingDescription[0].inputRate = VK_VERTEX_INPUT_RATE_VERTEX;
		
		// todo: this one is not used
		pRenderer->bindingDescription[1].binding = 1;
		pRenderer->bindingDescription[1].stride = sizeof(fr_vertex_t);
		pRenderer->bindingDescription[1].inputRate = VK_VERTEX_INPUT_RATE_VERTEX;
		
		pRenderer->vertexAttributes[0].binding = 0;
		pRenderer->vertexAttributes[0].location = 0;
		pRenderer->vertexAttributes[0].format = VK_FORMAT_R32G32_SFLOAT;
		pRenderer->vertexAttributes[0].offset = offsetof(fr_vertex_t, position);
		
		pRenderer->vertexAttributes[1].binding = 0;
		pRenderer->vertexAttributes[1].location = 1;
		pRenderer->vertexAttributes[1].format = VK_FORMAT_R32G32B32_SFLOAT;
		pRenderer->vertexAttributes[1].offset = offsetof(fr_vertex_t, color);
	}
	
	// create descriptor set layout
	if(res == FR_RESULT_OK)
	{
		VkDescriptorSetLayoutBinding uboLayoutBinding = {};
		uboLayoutBinding.binding = 0;
		uboLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
		uboLayoutBinding.descriptorCount = 1;
		uboLayoutBinding.stageFlags = VK_SHADER_STAGE_VERTEX_BIT;
		uboLayoutBinding.pImmutableSamplers = NULL;
		
		VkDescriptorSetLayoutCreateInfo layoutInfo = {};
		layoutInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
		layoutInfo.bindingCount = 1;
		layoutInfo.pBindings = &uboLayoutBinding;
		
		if (vkCreateDescriptorSetLayout(pRenderer->device, &layoutInfo, NULL, &pRenderer->descriptorSetLayout) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create uniform buffer descriptor layout");
			res = FR_RESULT_ERROR_GPU;
		}
	}
	
	// create uniform buffer
	if(res == FR_RESULT_OK)
	{
		const uint32_t uniformBufferSize = sizeof(fr_uniform_buffer_t);
		for(uint32_t i=0; i<3; ++i)
		{
			fr_create_buffer(pRenderer->device, pRenderer->physicalDevice, uniformBufferSize,
					   VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
					   &pRenderer->aUniformBuffer[i], &pRenderer->aUniformBufferMemory[i], pAllocCallbacks);
		}
	}
	
	// create render stages
	if(res == FR_RESULT_OK)
	{
		// create shader stages
		VkPipelineShaderStageCreateInfo vertShaderStageInfo = {};
		vertShaderStageInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
		vertShaderStageInfo.stage = VK_SHADER_STAGE_VERTEX_BIT;
		vertShaderStageInfo.module = pRenderer->vertexShaderModule;
		vertShaderStageInfo.pName = "main";
		
		VkPipelineShaderStageCreateInfo fragShaderStageInfo = {};
		fragShaderStageInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
		fragShaderStageInfo.stage = VK_SHADER_STAGE_FRAGMENT_BIT;
		fragShaderStageInfo.module = pRenderer->fragmentShaderModule;
		fragShaderStageInfo.pName = "main";
		
		VkPipelineShaderStageCreateInfo shaderStages[] = {vertShaderStageInfo, fragShaderStageInfo};
		
		VkPipelineVertexInputStateCreateInfo vertexInputInfo = {};
		vertexInputInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
		vertexInputInfo.vertexBindingDescriptionCount = 1;
		vertexInputInfo.vertexAttributeDescriptionCount = 2;
		vertexInputInfo.pVertexBindingDescriptions = pRenderer->bindingDescription;
		vertexInputInfo.pVertexAttributeDescriptions = pRenderer->vertexAttributes;
		
		// create input assembly state
		VkPipelineInputAssemblyStateCreateInfo inputAssembly = {};
		inputAssembly.sType = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
		inputAssembly.topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
		inputAssembly.primitiveRestartEnable = VK_FALSE;
		
		// create viewport
		VkViewport viewport = {};
		viewport.x = 0.0f;
		viewport.y = 0.0f;
		viewport.width = (float)pRenderer->swapChainExtent.width;
		viewport.height = (float)pRenderer->swapChainExtent.height;
		viewport.minDepth = 0.0f;
		viewport.maxDepth = 1.0f;
		
		VkRect2D scissor = {};
		scissor.offset.x = 0;
		scissor.offset.y = 0;
		scissor.extent = pRenderer->swapChainExtent;
		
		VkPipelineViewportStateCreateInfo viewportState = {};
		viewportState.sType = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
		viewportState.viewportCount = 1;
		viewportState.pViewports = &viewport;
		viewportState.scissorCount = 1;
		viewportState.pScissors = &scissor;
		
		// create rasterizer state
		VkPipelineRasterizationStateCreateInfo rasterizer = {};
		rasterizer.sType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
		rasterizer.depthClampEnable = VK_FALSE;
		rasterizer.rasterizerDiscardEnable = VK_FALSE;
		rasterizer.polygonMode = VK_POLYGON_MODE_FILL;
		rasterizer.lineWidth = 1.0f;
		rasterizer.cullMode = VK_CULL_MODE_BACK_BIT;
		rasterizer.frontFace = VK_FRONT_FACE_COUNTER_CLOCKWISE;
		rasterizer.depthBiasEnable = VK_FALSE;
		rasterizer.depthBiasConstantFactor = 0.0f; // Optional
		rasterizer.depthBiasClamp = 0.0f; // Optional
		rasterizer.depthBiasSlopeFactor = 0.0f; // Optional
		
		// create multi sampling state
		VkPipelineMultisampleStateCreateInfo multisampling = {};
		multisampling.sType = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
		multisampling.sampleShadingEnable = VK_FALSE;
		multisampling.rasterizationSamples = VK_SAMPLE_COUNT_1_BIT;
		multisampling.minSampleShading = 1.0f; // Optional
		multisampling.pSampleMask = NULL; // Optional
		multisampling.alphaToCoverageEnable = VK_FALSE; // Optional
		multisampling.alphaToOneEnable = VK_FALSE; // Optional
		
		// depth and stencil state
		VkPipelineColorBlendAttachmentState colorBlendAttachment = {};
		colorBlendAttachment.colorWriteMask = VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT | VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT;
		colorBlendAttachment.blendEnable = VK_FALSE;
		colorBlendAttachment.srcColorBlendFactor = VK_BLEND_FACTOR_ONE; // Optional
		colorBlendAttachment.dstColorBlendFactor = VK_BLEND_FACTOR_ZERO; // Optional
		colorBlendAttachment.colorBlendOp = VK_BLEND_OP_ADD; // Optional
		colorBlendAttachment.srcAlphaBlendFactor = VK_BLEND_FACTOR_ONE; // Optional
		colorBlendAttachment.dstAlphaBlendFactor = VK_BLEND_FACTOR_ZERO; // Optional
		colorBlendAttachment.alphaBlendOp = VK_BLEND_OP_ADD; // Optional
		
		/* // for blending
		 colorBlendAttachment.blendEnable = VK_TRUE;
		 colorBlendAttachment.srcColorBlendFactor = VK_BLEND_FACTOR_SRC_ALPHA;
		 colorBlendAttachment.dstColorBlendFactor = VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA;
		 colorBlendAttachment.colorBlendOp = VK_BLEND_OP_ADD;
		 colorBlendAttachment.srcAlphaBlendFactor = VK_BLEND_FACTOR_ONE;
		 colorBlendAttachment.dstAlphaBlendFactor = VK_BLEND_FACTOR_ZERO;
		 colorBlendAttachment.alphaBlendOp = VK_BLEND_OP_ADD;
		 */
		
		/* // dynamic state for dynamic changes to pipeline state
		 VkDynamicState dynamicStates[] = {
		 VK_DYNAMIC_STATE_VIEWPORT,
		 VK_DYNAMIC_STATE_LINE_WIDTH
		 };
		 
		 VkPipelineDynamicStateCreateInfo dynamicState = {};
		 dynamicState.sType = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
		 dynamicState.dynamicStateCount = 2;
		 dynamicState.pDynamicStates = dynamicStates;
		 */
		
		VkPipelineColorBlendStateCreateInfo colorBlending = {};
		colorBlending.sType = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
		colorBlending.logicOpEnable = VK_FALSE;
		colorBlending.logicOp = VK_LOGIC_OP_COPY; // Optional
		colorBlending.attachmentCount = 1;
		colorBlending.pAttachments = &colorBlendAttachment;
		colorBlending.blendConstants[0] = 0.0f; // Optional
		colorBlending.blendConstants[1] = 0.0f; // Optional
		colorBlending.blendConstants[2] = 0.0f; // Optional
		colorBlending.blendConstants[3] = 0.0f; // Optional
		
		VkPipelineLayoutCreateInfo pipelineLayoutInfo = {};
		pipelineLayoutInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
		pipelineLayoutInfo.setLayoutCount = 1; // Optional, 1 because of uniform buffer
		pipelineLayoutInfo.pSetLayouts = &pRenderer->descriptorSetLayout; // Optional, setting it befause of uniform buffer
		pipelineLayoutInfo.pushConstantRangeCount = 0; // Optional
		pipelineLayoutInfo.pPushConstantRanges = NULL; // Optional
		
		if (vkCreatePipelineLayout(pRenderer->device, &pipelineLayoutInfo, NULL, &pRenderer->pipelineLayout) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create pipeline layout");
			res = FR_RESULT_ERROR_GPU;
		}
		
		// create render pass
		VkAttachmentDescription colorAttachment = {};
		colorAttachment.format = pRenderer->swapChainSurfaceFormat;
		colorAttachment.samples = VK_SAMPLE_COUNT_1_BIT;
		colorAttachment.loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR;
		colorAttachment.storeOp = VK_ATTACHMENT_STORE_OP_STORE;
		colorAttachment.stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
		colorAttachment.stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
		colorAttachment.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
		colorAttachment.finalLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
		
		VkAttachmentReference colorAttachmentRef = {};
		colorAttachmentRef.attachment = 0;
		colorAttachmentRef.layout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
		
		// subpass
		VkSubpassDescription subpass = {};
		subpass.pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS;
		
		subpass.colorAttachmentCount = 1;
		subpass.pColorAttachments = &colorAttachmentRef;
		
		// subpass dependency
		VkSubpassDependency dependency = {};
		dependency.srcSubpass = VK_SUBPASS_EXTERNAL;
		dependency.dstSubpass = 0;
		
		dependency.srcStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
		dependency.srcAccessMask = 0;
		
		dependency.dstStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
		dependency.dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_READ_BIT | VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
		
		// create render pass
		VkRenderPassCreateInfo renderPassInfo = {};
		renderPassInfo.sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
		renderPassInfo.attachmentCount = 1;
		renderPassInfo.pAttachments = &colorAttachment;
		renderPassInfo.subpassCount = 1;
		renderPassInfo.pSubpasses = &subpass;
		
		renderPassInfo.dependencyCount = 1;
		renderPassInfo.pDependencies = &dependency;
		
		if (vkCreateRenderPass(pRenderer->device, &renderPassInfo, NULL, &pRenderer->renderPass) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create render pass");
			res = FR_RESULT_ERROR_GPU;
		}
		
		// create graphics pipeline
		VkGraphicsPipelineCreateInfo pipelineInfo = {};
		pipelineInfo.sType = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;
		pipelineInfo.stageCount = 2;
		pipelineInfo.pStages = shaderStages;
		
		pipelineInfo.pVertexInputState = &vertexInputInfo;
		pipelineInfo.pInputAssemblyState = &inputAssembly;
		pipelineInfo.pViewportState = &viewportState;
		pipelineInfo.pRasterizationState = &rasterizer;
		pipelineInfo.pMultisampleState = &multisampling;
		pipelineInfo.pDepthStencilState = NULL; // Optional
		pipelineInfo.pColorBlendState = &colorBlending;
		pipelineInfo.pDynamicState = NULL; // Optional
		
		pipelineInfo.layout = pRenderer->pipelineLayout;
		
		pipelineInfo.renderPass = pRenderer->renderPass;
		pipelineInfo.subpass = 0;
		
		pipelineInfo.basePipelineHandle = VK_NULL_HANDLE; // Optional
		pipelineInfo.basePipelineIndex = 0; // Optional
		
		if (vkCreateGraphicsPipelines(pRenderer->device, VK_NULL_HANDLE, 1, &pipelineInfo, NULL, &pRenderer->graphicsPipeline) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create graphics pipeline");
			res = FR_RESULT_ERROR_GPU;
		}
	}
	
	const uint32_t numTestVertices = g_numTestVertices;
	
	const VkDeviceSize testVertexBufferSize = sizeof(fr_vertex_t) * numTestVertices;
	const VkDeviceSize testColorVertexBufferSize = sizeof(fr_vec3_t) * numTestVertices;
	
	// create test geometry vertex buffer
	if(res == FR_RESULT_OK)
	{
		fr_create_buffer(pRenderer->device, pRenderer->physicalDevice, testVertexBufferSize,
					   FR_VERTEX_BUFFER_USAGE_FLAGS, FR_VERTEX_BUFFER_MEMORY_FLAGS,
					   &pRenderer->vertexBuffer, &pRenderer->vertexBufferMemory, pAllocCallbacks);
		
		for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			fr_create_buffer(pRenderer->device, pRenderer->physicalDevice, testColorVertexBufferSize,
						   VK_BUFFER_USAGE_VERTEX_BUFFER_BIT,
						   VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
						   &pRenderer->colorVertexBuffer[i], &pRenderer->colorVertexBufferMemory[i],
						   pAllocCallbacks);
		}
	}
	
	// allocate vertex colors on CPU side
	if(res == FR_RESULT_OK)
	{
		for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			pRenderer->vertexColors[i] = FUR_ALLOC(testColorVertexBufferSize, 8, FR_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
			
			memset(pRenderer->vertexColors[i], 0, testColorVertexBufferSize);
		}
	}
	
	const VkDeviceSize testIndexBufferSize = sizeof(uint16_t) * g_numTestIndices;
	
	// create test geometry index buffer
	if(res == FR_RESULT_OK)
	{
		fr_create_buffer(pRenderer->device, pRenderer->physicalDevice, testIndexBufferSize,
					   VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_INDEX_BUFFER_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
					   &pRenderer->indexBuffer, &pRenderer->indexBufferMemory, pAllocCallbacks);
	}
	
	// create frame buffers
	if(res == FR_RESULT_OK)
	{
		for (size_t i = 0; i < NUM_SWAP_CHAIN_IMAGES; i++)
		{
			VkImageView attachments[] =
			{
				pRenderer->aSwapChainImagesViews[i]
			};
			
			VkFramebufferCreateInfo framebufferInfo = {};
			framebufferInfo.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
			framebufferInfo.renderPass = pRenderer->renderPass;
			framebufferInfo.attachmentCount = 1;
			framebufferInfo.pAttachments = attachments;
			framebufferInfo.width = pRenderer->swapChainExtent.width;
			framebufferInfo.height = pRenderer->swapChainExtent.height;
			framebufferInfo.layers = 1;
			
			if (vkCreateFramebuffer(pRenderer->device, &framebufferInfo, NULL, &pRenderer->aSwapChainFrameBuffers[i]) != VK_SUCCESS)
			{
				fur_set_last_error("Can't create frame buffers");
				res = FR_RESULT_ERROR_GPU;
			}
		}
	}
	
	// create descriptor pool (for uniform buffer)
	if(res == FR_RESULT_OK)
	{
		VkDescriptorPoolSize poolSize = {};
		poolSize.type = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
		poolSize.descriptorCount = NUM_SWAP_CHAIN_IMAGES;
		
		VkDescriptorPoolCreateInfo poolInfo = {};
		poolInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
		poolInfo.poolSizeCount = 1;
		poolInfo.pPoolSizes = &poolSize;
		poolInfo.maxSets = NUM_SWAP_CHAIN_IMAGES;
		
		if (vkCreateDescriptorPool(pRenderer->device, &poolInfo, NULL, &pRenderer->descriptorPool) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create descriptor pool for uniform buffers");
			res = FR_RESULT_ERROR_GPU;
		}
	}
	
	// create descriptor sets
	if(res == FR_RESULT_OK)
	{
		VkDescriptorSetLayout layouts[NUM_SWAP_CHAIN_IMAGES] = {pRenderer->descriptorSetLayout,
			pRenderer->descriptorSetLayout, pRenderer->descriptorSetLayout};
		
		VkDescriptorSetAllocateInfo allocInfo = {};
		allocInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
		allocInfo.descriptorPool = pRenderer->descriptorPool;
		allocInfo.descriptorSetCount = NUM_SWAP_CHAIN_IMAGES;
		allocInfo.pSetLayouts = layouts;
		
		if (vkAllocateDescriptorSets(pRenderer->device, &allocInfo, pRenderer->aDescriptorSets) != VK_SUCCESS)
		{
			fur_set_last_error("Can't allocate descriptor sets for uniform buffers");
			res = FR_RESULT_ERROR_GPU;
		}
		
		for (size_t i = 0; i < NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			VkDescriptorBufferInfo bufferInfo = {};
			bufferInfo.buffer = pRenderer->aUniformBuffer[i];
			bufferInfo.offset = 0;
			bufferInfo.range = sizeof(fr_uniform_buffer_t);
			
			VkWriteDescriptorSet descriptorWrite = {};
			descriptorWrite.sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
			descriptorWrite.dstSet = pRenderer->aDescriptorSets[i];
			descriptorWrite.dstBinding = 0;
			descriptorWrite.dstArrayElement = 0;
			descriptorWrite.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
			descriptorWrite.descriptorCount = 1;
			descriptorWrite.pBufferInfo = &bufferInfo;
			descriptorWrite.pImageInfo = NULL; // Optional
			descriptorWrite.pTexelBufferView = NULL; // Optional
			
			vkUpdateDescriptorSets(pRenderer->device, 1, &descriptorWrite, 0, NULL);
		}
	}
	
	// create command pool
	if(res == FR_RESULT_OK)
	{
		VkCommandPoolCreateInfo poolInfo = {};
		poolInfo.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
		poolInfo.queueFamilyIndex = pRenderer->idxQueueGraphics;
		poolInfo.flags = 0; // Optional
		
		if (vkCreateCommandPool(pRenderer->device, &poolInfo, NULL, &pRenderer->commandPool) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create graphics command pool");
			res = FR_RESULT_ERROR_GPU;
		}
	}
	
	fr_staging_buffer_builder_t stagingBuilder;
	fr_staging_init(&stagingBuilder);
	
	VkDeviceSize imageSize = 0;
	VkDeviceSize imageOffsetInBuffer = 0;
	
	// load texture
	{
		int texWidth, texHeight, texChannels;
		stbi_uc* pixels = stbi_load(g_texturePath, &texWidth, &texHeight, &texChannels, STBI_rgb_alpha);
		imageSize = texWidth * texHeight * 4;
		
		if(!pixels)
		{
			fur_set_last_error("Can't load texture");
			res = FR_RESULT_ERROR_GPU;
		}
		else
		{
			imageOffsetInBuffer = stagingBuilder.totalSize;
			fr_staging_add(&stagingBuilder, pixels, (uint32_t)imageSize, NULL, fr_pixels_free_func);
		}
	}
	
	// create texture image
	{
		fr_create_image(pRenderer->device, pRenderer->physicalDevice, imageSize,
						VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
						g_textureWidth, g_textureHeight,
						&pRenderer->textureImage, &pRenderer->textureImageMemory, pAllocCallbacks);
	}
	
	// record commands
	if(res == FR_RESULT_OK)
	{
		VkCommandBufferAllocateInfo allocInfo = {};
		allocInfo.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
		allocInfo.commandPool = pRenderer->commandPool;
		allocInfo.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
		allocInfo.commandBufferCount = NUM_SWAP_CHAIN_IMAGES;
		
		if (vkAllocateCommandBuffers(pRenderer->device, &allocInfo, pRenderer->aCommandBuffers) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create command buffers");
			res = FR_RESULT_ERROR_GPU;
		}
		
		// record commands into command buffers
		for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			VkCommandBufferBeginInfo beginInfo = {};
			beginInfo.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
			beginInfo.flags = VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT;
			beginInfo.pInheritanceInfo = NULL; // Optional
			
			if (vkBeginCommandBuffer(pRenderer->aCommandBuffers[i], &beginInfo) != VK_SUCCESS)
			{
				fur_set_last_error("Can't begin command buffer");
				res = FR_RESULT_ERROR_GPU;
			}
			
			VkRenderPassBeginInfo renderPassInfo = {};
			renderPassInfo.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
			renderPassInfo.renderPass = pRenderer->renderPass;
			renderPassInfo.framebuffer = pRenderer->aSwapChainFrameBuffers[i];
			
			renderPassInfo.renderArea.offset.x = 0;
			renderPassInfo.renderArea.offset.y = 0;
			renderPassInfo.renderArea.extent = pRenderer->swapChainExtent;
			
			VkClearValue clearColor = {0.0f, 0.0f, 0.0f, 1.0f};
			renderPassInfo.clearValueCount = 1;
			renderPassInfo.pClearValues = &clearColor;
			
			vkCmdBeginRenderPass(pRenderer->aCommandBuffers[i], &renderPassInfo, VK_SUBPASS_CONTENTS_INLINE);
			
			vkCmdBindPipeline(pRenderer->aCommandBuffers[i], VK_PIPELINE_BIND_POINT_GRAPHICS, pRenderer->graphicsPipeline);
			
			// bind vertex and index buffers
			VkBuffer vertexBuffers[] = {pRenderer->vertexBuffer};
			VkDeviceSize offsets[] = {0};
			vkCmdBindVertexBuffers(pRenderer->aCommandBuffers[i], 0, 1, vertexBuffers, offsets);
			vkCmdBindIndexBuffer(pRenderer->aCommandBuffers[i], pRenderer->indexBuffer, 0, VK_INDEX_TYPE_UINT16);
			
			// bind uniform buffer
			vkCmdBindDescriptorSets(pRenderer->aCommandBuffers[i], VK_PIPELINE_BIND_POINT_GRAPHICS,
									pRenderer->pipelineLayout, 0, 1, &pRenderer->aDescriptorSets[i], 0, NULL);
			
			//vkCmdDraw(pRenderer->aCommandBuffers[i], g_numTestVertices, 1, 0, 0);
			vkCmdDrawIndexed(pRenderer->aCommandBuffers[i], g_numTestIndices, 1, 0, 0, 0);
			vkCmdEndRenderPass(pRenderer->aCommandBuffers[i]);
			
			if (vkEndCommandBuffer(pRenderer->aCommandBuffers[i]) != VK_SUCCESS)
			{
				fur_set_last_error("Can't record command buffer");
				res = FR_RESULT_ERROR_GPU;
			}
		}
	}
	
	// do staging pass - copy vertex data from staging buffer to vertex buffer
	if(res == FR_RESULT_OK)
	{
		// add vertices & indices data to staging buffer
		{
			fr_staging_add(&stagingBuilder, (void*)g_testGeometry, sizeof(fr_vertex_t) * g_numTestVertices, NULL, NULL);
			fr_staging_add(&stagingBuilder, (void*)g_testIndices, sizeof(uint16_t) * g_numTestIndices, NULL, NULL);
		}
		
		// create staging buffer & release memory of source data
		{
			fr_staging_build(&stagingBuilder, pRenderer->device, pRenderer->physicalDevice, &pRenderer->stagingBuffer, &pRenderer->stagingBufferMemory, pAllocCallbacks);
			fr_staging_release_builder(&stagingBuilder);
		}
		
		// create staging command pool
		{
			VkCommandPoolCreateInfo poolInfo = {};
			poolInfo.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
			poolInfo.queueFamilyIndex = pRenderer->idxQueueGraphics;
			poolInfo.flags = VK_COMMAND_POOL_CREATE_TRANSIENT_BIT; // Optional
			
			if (vkCreateCommandPool(pRenderer->device, &poolInfo, NULL, &pRenderer->stagingCommandPool) != VK_SUCCESS)
			{
				FUR_ASSERT(false);
			}
		}
		
		// record and execute staging command buffer
		{
			VkCommandBuffer commandBuffer = fr_begin_simple_commands(pRenderer->device, pRenderer->stagingCommandPool, pAllocCallbacks);
			
			// copy vertex buffer region
			{
				uint32_t srcStagingIndices[2] = {1, 2};
				VkBuffer dstBuffers[2] = {pRenderer->vertexBuffer, pRenderer->indexBuffer};
				
				fr_staging_record_copy_commands(&stagingBuilder, commandBuffer, pRenderer->stagingBuffer, srcStagingIndices, dstBuffers, 2);
			}
			
			fr_end_simple_commands(pRenderer->device, pRenderer->graphicsQueue, commandBuffer, pRenderer->stagingCommandPool, pAllocCallbacks);
		}
		
		// image layout transitions
		{
			fr_transition_image_layout(pRenderer->device, pRenderer->graphicsQueue, pRenderer->stagingCommandPool,
									   VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, pRenderer->textureImage, pAllocCallbacks);
			fr_copy_buffer_to_image(pRenderer->device, pRenderer->graphicsQueue, pRenderer->stagingCommandPool, pRenderer->stagingBuffer, imageOffsetInBuffer, pRenderer->textureImage, g_textureWidth, g_textureHeight, pAllocCallbacks);
			
		}
		
		// release staging buffer
		{
			vkDestroyBuffer(pRenderer->device, pRenderer->stagingBuffer, NULL);
			vkFreeMemory(pRenderer->device, pRenderer->stagingBufferMemory, NULL);
		}
	}
	
	// create render semaphores
	if(res == FR_RESULT_OK)
	{
		VkSemaphoreCreateInfo semaphoreInfo;
		memset(&semaphoreInfo, 0, sizeof(VkSemaphoreCreateInfo));
		semaphoreInfo.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
		
		if (vkCreateSemaphore(pRenderer->device, &semaphoreInfo, NULL, &pRenderer->imageAvailableSemaphore) != VK_SUCCESS ||
			vkCreateSemaphore(pRenderer->device, &semaphoreInfo, NULL, &pRenderer->renderFinishedSemaphore) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create render semaphores");
			res = FR_RESULT_ERROR_GPU;
		}
	}
	
	if(res == FR_RESULT_OK)
	{
		*ppRenderer = pRenderer;
	}
	else
	{
		FUR_FREE(pRenderer, pAllocCallbacks);
	}
	
	pRenderer->rotationAngle = 0.0f;
	
	return res;
}

enum fr_result_t fr_release_renderer(struct fr_renderer_t* pRenderer,
					   struct fr_allocation_callbacks_t*	pAllocCallbacks)
{
	// this should be in clean-up swap chain
	{
		// destroy uniform buffer
		for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			vkDestroyBuffer(pRenderer->device, pRenderer->aUniformBuffer[i], NULL);
			vkFreeMemory(pRenderer->device, pRenderer->aUniformBufferMemory[i], NULL);
		}
		
		vkDestroyDescriptorPool(pRenderer->device, pRenderer->descriptorPool, NULL);
	}
	
	// dealloc vertex colors
	for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
	{
		if(pRenderer->vertexColors[i])
		{
			FUR_FREE(pRenderer->vertexColors[i], pAllocCallbacks);
		}
		
		// destroy color vertex buffer
		vkDestroyBuffer(pRenderer->device, pRenderer->colorVertexBuffer[i], NULL);
		vkFreeMemory(pRenderer->device, pRenderer->colorVertexBufferMemory[i], NULL);
	}
	
	// destroy image
	vkDestroyImage(pRenderer->device, pRenderer->textureImage, NULL);
	vkFreeMemory(pRenderer->device, pRenderer->textureImageMemory, NULL);
	
	// destroy vertex buffer
	vkDestroyBuffer(pRenderer->device, pRenderer->vertexBuffer, NULL);
	vkFreeMemory(pRenderer->device, pRenderer->vertexBufferMemory, NULL);
	
	// destroy index buffer
	vkDestroyBuffer(pRenderer->device, pRenderer->indexBuffer, NULL);
	vkFreeMemory(pRenderer->device, pRenderer->indexBufferMemory, NULL);
	
	// destroy descriptor set layout
	vkDestroyDescriptorSetLayout(pRenderer->device, pRenderer->descriptorSetLayout, NULL);
	
	// destroy semaphores
	vkDestroySemaphore(pRenderer->device, pRenderer->imageAvailableSemaphore, NULL);
	vkDestroySemaphore(pRenderer->device, pRenderer->renderFinishedSemaphore, NULL);
	
	// destroy graphics command pool
	vkDestroyCommandPool(pRenderer->device, pRenderer->commandPool, NULL);
	
	// destroy staging command pool
	vkDestroyCommandPool(pRenderer->device, pRenderer->stagingCommandPool, NULL);
	
	// destroy swap chain frame buffers
	for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
	{
		vkDestroyFramebuffer(pRenderer->device, pRenderer->aSwapChainFrameBuffers[i], NULL);
	}
	
	// destroy render pass and pipeline layout and graphics pipeline
	vkDestroyPipeline(pRenderer->device, pRenderer->graphicsPipeline, NULL);
	vkDestroyPipelineLayout(pRenderer->device, pRenderer->pipelineLayout, NULL);
	vkDestroyRenderPass(pRenderer->device, pRenderer->renderPass, NULL);
	
	// destroy shaders
	vkDestroyShaderModule(pRenderer->device, pRenderer->vertexShaderModule, NULL);
	vkDestroyShaderModule(pRenderer->device, pRenderer->fragmentShaderModule, NULL);
	
	// destroy window surface and swap chain
	for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
	{
		vkDestroyImageView(pRenderer->device, pRenderer->aSwapChainImagesViews[i], NULL);
	}
	
	// destroy swap chain and surface
	vkDestroySwapchainKHR(pRenderer->device, pRenderer->swapChain, NULL);
	vkDestroySurfaceKHR(pRenderer->vkInstance, pRenderer->surface, NULL);
	
	// destroy device and instance
	vkDestroyDevice(pRenderer->device, NULL);
	vkDestroyInstance(pRenderer->vkInstance, NULL);
	
	FUR_FREE(pRenderer, pAllocCallbacks);
	
	return FR_RESULT_OK;
}

void fr_wait_for_device(struct fr_renderer_t* pRenderer)
{
	vkDeviceWaitIdle(pRenderer->device);
}

const float g_rotationSpeed = FM_DEG_TO_RAD(5);
const fm_vec4 g_eye = {0, -4, 2, 0};
const fm_vec4 g_at = {0, 0, 0, 0};
const fm_vec4 g_up = {0, 0, 1, 0};

double g_timeDelta = 0.0f;
double g_time = 0.0f;

void fr_update_renderer(struct fr_renderer_t* pRenderer, const struct fr_update_context_t* ctx)
{
	pRenderer->rotationAngle += g_rotationSpeed * ctx->dt;
	
	g_timeDelta = ctx->dt;
	g_time += g_timeDelta;
};

uint32_t g_prevImageIndex = 0;

void fr_draw_frame(struct fr_renderer_t* pRenderer)
{
	uint32_t imageIndex;
	vkAcquireNextImageKHR(pRenderer->device, pRenderer->swapChain, (uint64_t)-1,
						  pRenderer->imageAvailableSemaphore, VK_NULL_HANDLE, &imageIndex);
	
	if(imageIndex != g_prevImageIndex)
	{
		const uint32_t prevImageIndex = g_prevImageIndex;
		g_prevImageIndex = imageIndex;
	}
	
	FUR_ASSERT(imageIndex < NUM_SWAP_CHAIN_IMAGES);
	
	VkSubmitInfo submitInfo = {};
	submitInfo.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
	
	VkSemaphore waitSemaphores[] = {pRenderer->imageAvailableSemaphore};
	VkPipelineStageFlags waitStages[] = {VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT};
	submitInfo.waitSemaphoreCount = 1;
	submitInfo.pWaitSemaphores = waitSemaphores;
	submitInfo.pWaitDstStageMask = waitStages;
	
	submitInfo.commandBufferCount = 1;
	submitInfo.pCommandBuffers = &pRenderer->aCommandBuffers[imageIndex];
	
	VkSemaphore signalSemaphores[] = {pRenderer->renderFinishedSemaphore};
	submitInfo.signalSemaphoreCount = 1;
	submitInfo.pSignalSemaphores = signalSemaphores;

	// update uniform buffer
	{
		const float aspectRatio = pRenderer->swapChainExtent.width / (float)pRenderer->swapChainExtent.height;
		
		fm_mat4_t tempView;
		
		fr_uniform_buffer_t ubo = {};
		fm_mat4_rot_z(pRenderer->rotationAngle, &ubo.model);
		fm_mat4_lookat(&g_eye, &g_at, &g_up, &tempView);
		fm_mat4_projection_fov(45.0f, aspectRatio, 0.1f, 1000.0f, &ubo.proj);
		
		fm_mat4_t rot_x_vulkan_correction;
		fm_mat4_rot_x(FM_DEG_TO_RAD(-90), &rot_x_vulkan_correction);
		fm_mat4_mul(&rot_x_vulkan_correction, &tempView, &ubo.view);
		
		ubo.proj.y.y *= -1.0f;
		
		//fm_mat4_identity(&ubo.model);
		//fm_mat4_identity(&ubo.view);
		//fm_mat4_identity(&ubo.proj);
		
		fm_mat4_transpose(&ubo.model);
		fm_mat4_transpose(&ubo.view);
		fm_mat4_transpose(&ubo.proj);
		
		fr_copy_data_to_buffer(pRenderer->device, pRenderer->aUniformBufferMemory[imageIndex], &ubo, 0, sizeof(ubo));
		fr_copy_data_to_buffer(pRenderer->device, pRenderer->colorVertexBufferMemory[imageIndex], pRenderer->vertexColors[imageIndex], 0, sizeof(fr_vec3_t) * g_numTestVertices);
	}
	
	// draw
	if (vkQueueSubmit(pRenderer->graphicsQueue, 1, &submitInfo, VK_NULL_HANDLE) != VK_SUCCESS)
	{
		FUR_ASSERT(false);
	}
	
	// present
	VkPresentInfoKHR presentInfo = {};
	presentInfo.sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
	
	presentInfo.waitSemaphoreCount = 1;
	presentInfo.pWaitSemaphores = signalSemaphores;
	
	VkSwapchainKHR swapChains[] = {pRenderer->swapChain};
	presentInfo.swapchainCount = 1;
	presentInfo.pSwapchains = swapChains;
	presentInfo.pImageIndices = &imageIndex;
	
	presentInfo.pResults = NULL; // Optional
	
	vkQueuePresentKHR(pRenderer->presentQueue, &presentInfo);
}

// serialization

typedef void (*fr_serialize_read_func_t)(void* pSerializerData, size_t size, void* pData);
typedef void (*fr_serialize_write_func_t)(void* pSerializerData, size_t size, const void* pData);

typedef struct fr_serializer_t
{
	void* serializerData;
	
	fr_serialize_read_func_t pfnRead;
	fr_serialize_write_func_t pfnWrite;
	
	bool isReader;
	int32_t version;
} fr_serializer_t;

// fundamental types serialization
#define fr_serialize(_serializer, _type) _Generic((_type), \
	int32_t*: fr_serialize_int32, \
	uint32_t*: fr_serialize_uint32 \
	)(_serializer, _type)

void fr_serialize_int32(fr_serializer_t* ser, int32_t* data)
{
	if(ser->isReader)
	{
		ser->pfnRead(ser->serializerData, sizeof(int32_t), data);
	}
	else
	{
		ser->pfnWrite(ser->serializerData, sizeof(int32_t), data);
	}
}

void fr_serialize_uint32(fr_serializer_t* ser, uint32_t* data)
{
	if(ser->isReader)
	{
		ser->pfnRead(ser->serializerData, sizeof(uint32_t), data);
	}
	else
	{
		ser->pfnWrite(ser->serializerData, sizeof(uint32_t), data);
	}
}

// serialization interface
#define FR_ADD_FIELD(_version, _field) \
	if(ser->version >= _version)	\
	{	\
		fr_serialize(ser, &data->_field);	\
	}

#define FR_REM_FIELD(_versionAdded, _versionRemoved, _type, _field, _defaultValue) \
	_type _field = _defaultValue;	\
	if(ser->version >= _versionAdded && ser->version < _versionRemoved)	\
	{	\
		fr_serialize(ser, &_field);	\
	}

// data size reader
typedef struct fr_data_size_reader_serializer_t
{
	uint32_t size;
} fr_data_size_reader_serializer_t;

void fr_data_size_reader_serializer_write_func(void* pSerData, size_t size, const void* data)
{
	fr_data_size_reader_serializer_t* pSer = (fr_data_size_reader_serializer_t*)(pSerData);
	pSer->size += size;
}

void fr_init_data_size_reader_serializer(fr_serializer_t* ser, fr_data_size_reader_serializer_t* serData)
{
	ser->isReader = false;
	ser->pfnRead = NULL;
	ser->pfnWrite = &fr_data_size_reader_serializer_write_func;
	ser->version = 0;
	ser->serializerData = serData;
}

// example serialization

typedef struct fr_example_struct_t
{
	int32_t fieldA;
} fr_example_struct_t;

enum fr_example_struct_version_t
{
	FR_VER_INITIAL = 1,
	FR_VER_LASTEST_PLUS_ONE
};

void frSerialize_example(fr_serializer_t* ser, fr_example_struct_t* data)
{
	FR_ADD_FIELD(FR_VER_INITIAL, fieldA);
}
