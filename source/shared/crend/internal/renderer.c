/* Copyright (c) 2016-2019 Furball Cat */

#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "ccore/public.h"
#include "cinput/public.h"

#include "renderer.h"
#include "vulkansdk/macOS/include/vulkan/vulkan.h"
#include "glfw/glfw3.h"

#include "renderBuffer.h"
#include "image.h"
#include "renderUtils.h"
#include "psoUtils.h"

#include "cimport/public.h"

// stb library
#ifdef __clang__
#define STBIDEF static inline
#endif

#define STB_IMAGE_STATIC
#define STB_IMAGE_IMPLEMENTATION

#include "stb_image.h"
//-----

#include <math.h>
#include "cmath/public.h"
#include "canim/public.h"

#define MAX(a,b) \
	({ __typeof__ (a) _a = (a); \
	__typeof__ (b) _b = (b); \
	_a > _b ? _a : _b; })

#define MIN(a,b) \
	({ __typeof__ (a) _a = (a); \
	__typeof__ (b) _b = (b); \
	_a < _b ? _a : _b; })

#define S1(x) #x
#define S2(x) S1(x)

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
									struct fc_alloc_callbacks_t* pAllocCallbacks)
{
	struct fr_app_t* pApp = FUR_ALLOC(sizeof(struct fr_app_t), 8, FC_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
	
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
	
	// init debug fragments - since now you can use debug lines
	fc_dbg_init(pAllocCallbacks);
	
	return FR_RESULT_OK;
}

enum fr_result_t fr_release_app(struct fr_app_t* pApp,
									 struct fc_alloc_callbacks_t* pAllocCallbacks)
{
	glfwDestroyWindow(pApp->pWindow);
	glfwTerminate();
	
	FUR_FREE(pApp, pAllocCallbacks);
	
	// release debug fragments - since now you cannot use debug lines
	fc_dbg_release(pAllocCallbacks);
	
	// validate memory - after this line there should be no fur_alloc/fur_free functions called
	// all memory deallocations should be already done at this point
	FUR_ASSERT(fc_validate_memory());
	
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

enum fr_result_t fr_load_binary_file_into_binary_buffer(const char* path, struct fr_binary_buffer_t* pBuffer, struct fc_alloc_callbacks_t* pAllocCallbacks)
{
	FILE* pFile = fopen(path, "rb");
	if(pFile && pBuffer)
	{
		fseek(pFile, 0, SEEK_END);
		size_t size = ftell(pFile);
		fseek(pFile, 0, SEEK_SET);
		
		pBuffer->pData = FUR_ALLOC(size, 8, FC_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
		pBuffer->size = size;
		
		fread(pBuffer->pData, size, 1, pFile);
		fclose(pFile);
		
		return FR_RESULT_OK;
	}
	
	fur_set_last_error(path);
	return FR_RESULT_ERROR;
}

void fr_release_binary_buffer(struct fr_binary_buffer_t* pBuffer, struct fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(pBuffer->pData, pAllocCallbacks);
}

/*************************************************************/

enum fr_result_t fr_create_shader_module(VkDevice device, const char* path, VkShaderModule* pShader, struct fc_alloc_callbacks_t* pAllocCallbacks)
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
	fr_vec3_t position;
	fr_vec3_t color;
	fr_vec2_t texCoord;
} fr_vertex_t;

typedef struct fr_uniform_buffer_t
{
	fm_mat4_t model;
	fm_mat4_t view;
	fm_mat4_t proj;
} fr_uniform_buffer_t;

typedef struct fr_skinning_buffer_t
{
	fm_mat4_t bones[512];
} fr_skinning_buffer_t;

typedef enum fr_mesh_chunk_buffer_offset_t
{
	FR_MESH_CHUNK_BUFFER_OFFSET_INDICES = 0,
	FR_MESH_CHUNK_BUFFER_OFFSET_VERTICES,
	FR_MESH_CHUNK_BUFFER_OFFSET_SKIN,
	FR_MESH_CHUNK_BUFFER_OFFSET_COUNT,
} fr_mesh_chunk_buffer_offset_t;

typedef struct fr_mesh_chunk_t
{
	fr_buffer_t data;
	VkDeviceSize offsets[FR_MESH_CHUNK_BUFFER_OFFSET_COUNT];
	uint32_t numIndices;
	int32_t textureIndex;
	
} fr_mesh_chunk_t;

typedef struct fr_mesh_t
{
	fr_mesh_chunk_t* chunks;
	uint32_t numChunks;
} fr_mesh_t;

const char* g_texturePathZeldaDiff = "../../../../../assets/characters/zelda/mesh/textures/zelda_diff.png";
const char* g_texturePathHairDiff = "../../../../../assets/characters/zelda/mesh/textures/hair_diff.png";
const char* g_texturePathEyesDiff = "../../../../../assets/characters/zelda/mesh/textures/eyes_diff2.png";

#define NUM_TEXTURES_IN_ARRAY 3

/*************************************************************/

#define NUM_SWAP_CHAIN_IMAGES 3

typedef struct fr_skinning_mapping_t
{
	uint32_t* indicesMapping;
	uint32_t count;
} fr_skinning_mapping_t;

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
	
	fr_image_t depthImage;
	
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
	
	// debug draw
	VkPipeline debugLinesPSO;
	VkPipeline debugTrianglesPSO;
	
	VkShaderModule debugVertexShaderModule;
	VkShaderModule debugFragmentShaderModule;
	
	VkShaderModule textVertexShaderModule;
	VkShaderModule textFragmentShaderModule;
	
	VkVertexInputBindingDescription debugLinesVertexBindingDescription;
	VkVertexInputAttributeDescription debugLinesVertexAttributes[2];
	
	VkVertexInputBindingDescription debugTextVertexBindingDescription;
	VkVertexInputAttributeDescription debugTextVertexAttributes[3];
	
	fr_buffer_t debugLinesVertexBuffer[NUM_SWAP_CHAIN_IMAGES];
	fr_buffer_t debugTrianglesVertexBuffer[NUM_SWAP_CHAIN_IMAGES];
	
	// test geometry
	VkVertexInputBindingDescription bindingDescription[2];
	VkVertexInputAttributeDescription vertexAttributes[5];
	
	fr_mesh_t mesh;
	
	fr_image_t textureZeldaDiff;
	fr_image_t textureHairDiff;
	fr_image_t textureEyesDiff;
	
	VkSampler textureSampler;
	
	fr_buffer_t stagingBuffer;
	fr_buffer_t aUniformBuffer[NUM_SWAP_CHAIN_IMAGES];
	fr_buffer_t aSkinningBuffer[NUM_SWAP_CHAIN_IMAGES];
	
	VkDescriptorPool descriptorPool;
	VkDescriptorSet aDescriptorSets[NUM_SWAP_CHAIN_IMAGES];
	
	fr_resource_mesh_t* pMesh;
	
	fa_rig_t* pRig;
	fa_anim_clip_t* pAnimClip;
	fa_anim_clip_t* pAnimClip2;
	
	fr_skinning_mapping_t skinningMapping;
	
	void* scratchpadBuffer;
	uint32_t scratchpadBufferSize;
	
	float rotationAngle;
	float cameraZoom;
	
	fi_input_manager_t* pInputManager;
};

void fr_pixels_free_func(void* pData, size_t size, void* pUserData)
{
	stbi_uc* pixels = (stbi_uc*)pData;
	stbi_image_free(pixels);
}

void fr_generic_buffer_free_func(void* pData, size_t size, void* pUserData)
{
	struct fc_alloc_callbacks_t* pAllocCallbacks = (struct fc_alloc_callbacks_t*)pUserData;
	
	FUR_FREE(pData, pAllocCallbacks);
}

enum fr_result_t fr_create_renderer(const struct fr_renderer_desc_t* pDesc,
					   struct fr_renderer_t** ppRenderer,
					   struct fc_alloc_callbacks_t*	pAllocCallbacks)
{
	struct fr_renderer_t* pRenderer = FUR_ALLOC(sizeof(struct fr_renderer_t), 8, FC_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
	if(!pRenderer)
	{
		fur_set_last_error("Can't allocate renderer.");
		return FR_RESULT_ERROR;
	}
	
	memset(pRenderer, 0, sizeof(struct fr_renderer_t));
	
	pRenderer->pInputManager = fi_input_manager_create(pAllocCallbacks);	// todo: move out of renderer
	
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
		
		VkPhysicalDevice* devices = FUR_ALLOC(numDevices * sizeof(VkPhysicalDevice), 8, FC_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
		
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
		
		VkExtensionProperties* extensions = FUR_ALLOC(numExtensions * sizeof(struct VkExtensionProperties), 8, FC_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
		
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
		
		VkQueueFamilyProperties* queueFamilies = FUR_ALLOC(numQueueFamilies * sizeof(struct VkQueueFamilyProperties), 8, FC_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
		
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
	const char* basicVertexShaderPath = "../../../../../shaders/compiled/basic_vs.spv";
	const char* basicFragmentShaderPath = "../../../../../shaders/compiled/basic_fs.spv";
	const char* debugVertexShaderPath = "../../../../../shaders/compiled/debug_vs.spv";
	const char* debugFragmentShaderPath = "../../../../../shaders/compiled/debug_fs.spv";
	const char* textVertexShaderPath = "../../../../../shaders/compiled/text_vs.spv";
	const char* textFragmentShaderPath = "../../../../../shaders/compiled/text_fs.spv";
	
	if(res == FR_RESULT_OK)
	{
		res = fr_create_shader_module(pRenderer->device, basicVertexShaderPath, &pRenderer->vertexShaderModule, pAllocCallbacks);
	}
	
	if(res == FR_RESULT_OK)
	{
		res = fr_create_shader_module(pRenderer->device, basicFragmentShaderPath, &pRenderer->fragmentShaderModule, pAllocCallbacks);
	}
	
	if(res == FR_RESULT_OK)
	{
		res = fr_create_shader_module(pRenderer->device, debugVertexShaderPath, &pRenderer->debugVertexShaderModule, pAllocCallbacks);
	}
	
	if(res == FR_RESULT_OK)
	{
		res = fr_create_shader_module(pRenderer->device, debugFragmentShaderPath, &pRenderer->debugFragmentShaderModule, pAllocCallbacks);
	}
	
	if(res == FR_RESULT_OK)
	{
		res = fr_create_shader_module(pRenderer->device, textVertexShaderPath, &pRenderer->textVertexShaderModule, pAllocCallbacks);
	}
	
	if(res == FR_RESULT_OK)
	{
		res = fr_create_shader_module(pRenderer->device, textFragmentShaderPath, &pRenderer->textFragmentShaderModule, pAllocCallbacks);
	}
	
	// debug draw bindings
	if(res == FR_RESULT_OK)
	{
		pRenderer->debugLinesVertexBindingDescription.binding = 0;
		pRenderer->debugLinesVertexBindingDescription.stride = 7 * sizeof(float);	// todo: take it from debug draw somehow instead of hardcoding
		pRenderer->debugLinesVertexBindingDescription.inputRate = VK_VERTEX_INPUT_RATE_VERTEX;
		
		pRenderer->debugLinesVertexAttributes[0].binding = 0;
		pRenderer->debugLinesVertexAttributes[0].location = 0;
		pRenderer->debugLinesVertexAttributes[0].format = VK_FORMAT_R32G32B32_SFLOAT;
		pRenderer->debugLinesVertexAttributes[0].offset = 0;
		
		pRenderer->debugLinesVertexAttributes[1].binding = 0;
		pRenderer->debugLinesVertexAttributes[1].location = 1;
		pRenderer->debugLinesVertexAttributes[1].format = VK_FORMAT_R32G32B32A32_SFLOAT;
		pRenderer->debugLinesVertexAttributes[1].offset = 3 * sizeof(float);
	}
	
	// debug text draw bindings
	if(res == FR_RESULT_OK)
	{
		pRenderer->debugTextVertexBindingDescription.binding = 0;
		pRenderer->debugTextVertexBindingDescription.stride = 9 * sizeof(float);
		pRenderer->debugTextVertexBindingDescription.inputRate = VK_VERTEX_INPUT_RATE_VERTEX;
		
		pRenderer->debugTextVertexAttributes[0].binding = 0;
		pRenderer->debugTextVertexAttributes[0].location = 0;
		pRenderer->debugTextVertexAttributes[0].format = VK_FORMAT_R32G32B32_SFLOAT;
		pRenderer->debugTextVertexAttributes[0].offset = 0;
		
		pRenderer->debugTextVertexAttributes[1].binding = 0;
		pRenderer->debugTextVertexAttributes[1].location = 1;
		pRenderer->debugTextVertexAttributes[1].format = VK_FORMAT_R32G32_SFLOAT;
		pRenderer->debugTextVertexAttributes[1].offset = 3 * sizeof(float);
		
		pRenderer->debugTextVertexAttributes[2].binding = 0;
		pRenderer->debugTextVertexAttributes[2].location = 2;
		pRenderer->debugTextVertexAttributes[2].format = VK_FORMAT_R32G32B32A32_SFLOAT;
		pRenderer->debugTextVertexAttributes[2].offset = 5 * sizeof(float);
	}
	
	// test geometry
	if(res == FR_RESULT_OK)
	{
		pRenderer->bindingDescription[0].binding = 0;
		pRenderer->bindingDescription[0].stride = sizeof(fr_vertex_t);
		pRenderer->bindingDescription[0].inputRate = VK_VERTEX_INPUT_RATE_VERTEX;
		
		pRenderer->vertexAttributes[0].binding = 0;
		pRenderer->vertexAttributes[0].location = 0;
		pRenderer->vertexAttributes[0].format = VK_FORMAT_R32G32B32_SFLOAT;
		pRenderer->vertexAttributes[0].offset = offsetof(fr_vertex_t, position);
		
		pRenderer->vertexAttributes[1].binding = 0;
		pRenderer->vertexAttributes[1].location = 1;
		pRenderer->vertexAttributes[1].format = VK_FORMAT_R32G32B32_SFLOAT;
		pRenderer->vertexAttributes[1].offset = offsetof(fr_vertex_t, color);
		
		pRenderer->vertexAttributes[2].binding = 0;
		pRenderer->vertexAttributes[2].location = 2;
		pRenderer->vertexAttributes[2].format = VK_FORMAT_R32G32_SFLOAT;
		pRenderer->vertexAttributes[2].offset = offsetof(fr_vertex_t, texCoord);
		
		pRenderer->bindingDescription[1].binding = 1;
		pRenderer->bindingDescription[1].stride = sizeof(fr_resource_mesh_chunk_skin_t);
		pRenderer->bindingDescription[1].inputRate = VK_VERTEX_INPUT_RATE_VERTEX;
		
		pRenderer->vertexAttributes[3].binding = 1;
		pRenderer->vertexAttributes[3].location = 3;
		pRenderer->vertexAttributes[3].format = VK_FORMAT_R16G16B16A16_SINT;
		pRenderer->vertexAttributes[3].offset = offsetof(fr_resource_mesh_chunk_skin_t, indices);
		
		pRenderer->vertexAttributes[4].binding = 1;
		pRenderer->vertexAttributes[4].location = 4;
		pRenderer->vertexAttributes[4].format = VK_FORMAT_R32G32B32A32_SFLOAT;
		pRenderer->vertexAttributes[4].offset = offsetof(fr_resource_mesh_chunk_skin_t, weights);
	}
	
	// create descriptor set layout
	if(res == FR_RESULT_OK)
	{
		// uniform buffer (UBO)
		VkDescriptorSetLayoutBinding uboLayoutBinding = {};
		uboLayoutBinding.binding = 0;
		uboLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
		uboLayoutBinding.descriptorCount = 1;
		uboLayoutBinding.stageFlags = VK_SHADER_STAGE_VERTEX_BIT;
		uboLayoutBinding.pImmutableSamplers = NULL;
		
		// uniform buffer (UBO)
		VkDescriptorSetLayoutBinding skinLayoutBinding = {};
		skinLayoutBinding.binding = 1;
		skinLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
		skinLayoutBinding.descriptorCount = 1;
		skinLayoutBinding.stageFlags = VK_SHADER_STAGE_VERTEX_BIT;
		skinLayoutBinding.pImmutableSamplers = NULL;
		
		// sampler
		VkDescriptorSetLayoutBinding samplerLayoutBinding = {};
		samplerLayoutBinding.binding = 2;
		samplerLayoutBinding.descriptorCount = NUM_TEXTURES_IN_ARRAY;
		samplerLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
		samplerLayoutBinding.pImmutableSamplers = NULL;
		samplerLayoutBinding.stageFlags = VK_SHADER_STAGE_FRAGMENT_BIT;
		
		const uint32_t numBindings = 3;
		VkDescriptorSetLayoutBinding bindings[numBindings] = { uboLayoutBinding, skinLayoutBinding, samplerLayoutBinding };
		
		VkDescriptorSetLayoutCreateInfo layoutInfo = {};
		layoutInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
		layoutInfo.bindingCount = numBindings;
		layoutInfo.pBindings = bindings;
		
		if (vkCreateDescriptorSetLayout(pRenderer->device, &layoutInfo, NULL, &pRenderer->descriptorSetLayout) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create uniform buffer descriptor layout");
			res = FR_RESULT_ERROR_GPU;
		}
	}
	
	// create uniform buffer
	if(res == FR_RESULT_OK)
	{
		fr_buffer_desc_t desc;
		desc.size = sizeof(fr_uniform_buffer_t);
		desc.usage = VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT;
		desc.properties = VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT;
		
		for(uint32_t i=0; i<3; ++i)
		{
			fr_buffer_create(pRenderer->device, pRenderer->physicalDevice, &desc, &pRenderer->aUniformBuffer[i], pAllocCallbacks);
		}
	}
	
	// create skinning buffer
	if(res == FR_RESULT_OK)
	{
		fr_buffer_desc_t desc;
		desc.size = sizeof(fr_skinning_buffer_t);
		desc.usage = VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT;
		desc.properties = VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT;
		
		for(uint32_t i=0; i<3; ++i)
		{
			fr_buffer_create(pRenderer->device, pRenderer->physicalDevice, &desc, &pRenderer->aSkinningBuffer[i], pAllocCallbacks);
		}
	}
	
	const VkFormat depthFormat = VK_FORMAT_D24_UNORM_S8_UINT;
	
	// create render stages
	if(res == FR_RESULT_OK)
	{
		VkPipelineLayoutCreateInfo pipelineLayoutInfo = {};
		fr_pso_init_layout(&pRenderer->descriptorSetLayout, &pipelineLayoutInfo);
		
		if (vkCreatePipelineLayout(pRenderer->device, &pipelineLayoutInfo, NULL, &pRenderer->pipelineLayout) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create pipeline layout");
			res = FR_RESULT_ERROR_GPU;
		}
		
		// create render pass
		if (fr_render_pass_create_color_depth(pRenderer->device, pRenderer->swapChainSurfaceFormat,
											  depthFormat, &pRenderer->renderPass, pAllocCallbacks) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create render pass");
			res = FR_RESULT_ERROR_GPU;
		}
		
		// create pipeline state object (PSO)
		VkPipelineShaderStageCreateInfo shaderStages[2] = {};
		
		fr_pso_init_shader_stages_simple(pRenderer->vertexShaderModule, "main",
										 pRenderer->fragmentShaderModule, "main",
										 shaderStages);
		
		VkPipelineVertexInputStateCreateInfo vertexInputInfo = {};
		vertexInputInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
		vertexInputInfo.vertexBindingDescriptionCount = 2;
		vertexInputInfo.vertexAttributeDescriptionCount = 5;
		vertexInputInfo.pVertexBindingDescriptions = pRenderer->bindingDescription;
		vertexInputInfo.pVertexAttributeDescriptions = pRenderer->vertexAttributes;
		
		// create rasterizer state
		VkPipelineRasterizationStateCreateInfo rasterizer = {};
		fr_pso_init_rasterization_state_polygon_fill(&rasterizer);
		
		// create input assembly state
		VkPipelineInputAssemblyStateCreateInfo inputAssembly = {};
		fr_pso_init_input_assembly_state_triangle_list(&inputAssembly);
		
		// create viewport
		VkViewport viewport = {};
		fr_pso_init_viewport((float)pRenderer->swapChainExtent.width,
							 (float)pRenderer->swapChainExtent.height,
							 &viewport);
		
		VkRect2D scissor = {};
		fr_pso_init_scissor(pRenderer->swapChainExtent, &scissor);
		
		VkPipelineViewportStateCreateInfo viewportState = {};
		fr_pso_init_viewport_state(&viewport, &scissor, &viewportState);
		
		// create multi sampling state
		VkPipelineMultisampleStateCreateInfo multisampling = {};
		fr_pso_init_multisampling_state(&multisampling);
		
		// depth and stencil state
		VkPipelineColorBlendAttachmentState colorBlendAttachment = {};
		fr_pso_init_color_blend_attachment_state(&colorBlendAttachment);
		
		// for blending use fr_pso_init_color_blend_attachment_state_blending
		
		VkPipelineColorBlendStateCreateInfo colorBlending = {};
		fr_pso_init_color_blend_state(&colorBlendAttachment, &colorBlending);
		
		VkPipelineDepthStencilStateCreateInfo depthStencil = {};
		fr_pso_init_depth_stencil_state(&depthStencil);
		
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
		pipelineInfo.pDepthStencilState = &depthStencil;
		pipelineInfo.pColorBlendState = &colorBlending;
		
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
	
	// create debug draw PSO
	if(res == FR_RESULT_OK)
	{
		// create pipeline state object (PSO)
		VkPipelineShaderStageCreateInfo shaderStages[2] = {};
		
		fr_pso_init_shader_stages_simple(pRenderer->debugVertexShaderModule, "main",
										 pRenderer->debugFragmentShaderModule, "main",
										 shaderStages);
		
		VkPipelineVertexInputStateCreateInfo vertexInputInfo = {};
		vertexInputInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
		vertexInputInfo.vertexBindingDescriptionCount = 1;
		vertexInputInfo.vertexAttributeDescriptionCount = 2;
		vertexInputInfo.pVertexBindingDescriptions = &pRenderer->debugLinesVertexBindingDescription;
		vertexInputInfo.pVertexAttributeDescriptions = pRenderer->debugLinesVertexAttributes;
		
		// create rasterizer state
		VkPipelineRasterizationStateCreateInfo rasterizer = {};
		fr_pso_init_rasterization_state_wireframe_no_cull(&rasterizer);
		
		// create input assembly state
		VkPipelineInputAssemblyStateCreateInfo inputAssembly = {};
		fr_pso_init_input_assembly_state_line_list(&inputAssembly);
		
		// create viewport
		VkViewport viewport = {};
		fr_pso_init_viewport((float)pRenderer->swapChainExtent.width,
							 (float)pRenderer->swapChainExtent.height,
							 &viewport);
		
		VkRect2D scissor = {};
		fr_pso_init_scissor(pRenderer->swapChainExtent, &scissor);
		
		VkPipelineViewportStateCreateInfo viewportState = {};
		fr_pso_init_viewport_state(&viewport, &scissor, &viewportState);
		
		// create multi sampling state
		VkPipelineMultisampleStateCreateInfo multisampling = {};
		fr_pso_init_multisampling_state(&multisampling);
		
		// depth and stencil state
		VkPipelineColorBlendAttachmentState colorBlendAttachment = {};
		fr_pso_init_color_blend_attachment_state(&colorBlendAttachment);
		
		// for blending use fr_pso_init_color_blend_attachment_state_blending
		
		VkPipelineColorBlendStateCreateInfo colorBlending = {};
		fr_pso_init_color_blend_state(&colorBlendAttachment, &colorBlending);
		
		VkPipelineDepthStencilStateCreateInfo depthStencil = {};
		fr_pso_init_depth_stencil_state(&depthStencil);
		
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
		pipelineInfo.pDepthStencilState = &depthStencil;
		pipelineInfo.pColorBlendState = &colorBlending;
		
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
		pipelineInfo.pDynamicState = NULL; // Optional
		
		pipelineInfo.layout = pRenderer->pipelineLayout;
		
		pipelineInfo.renderPass = pRenderer->renderPass;
		pipelineInfo.subpass = 0;
		
		pipelineInfo.basePipelineHandle = VK_NULL_HANDLE; // Optional
		pipelineInfo.basePipelineIndex = 0; // Optional
		
		// create lines debug PSO
		if (vkCreateGraphicsPipelines(pRenderer->device, VK_NULL_HANDLE, 1, &pipelineInfo, NULL, &pRenderer->debugLinesPSO) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create debug lines PSO");
			res = FR_RESULT_ERROR_GPU;
		}
		
		// create triangles debug PSO
		fr_pso_init_rasterization_state_polygon_fill(&rasterizer);
		fr_pso_init_input_assembly_state_triangle_list(&inputAssembly);
		if (vkCreateGraphicsPipelines(pRenderer->device, VK_NULL_HANDLE, 1, &pipelineInfo, NULL, &pRenderer->debugTrianglesPSO) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create debug triangles PSO");
			res = FR_RESULT_ERROR_GPU;
		}
	}
	
	// create depth buffer
	if(res == FR_RESULT_OK)
	{
		const VkDeviceSize depthImageSize = 4 * pRenderer->swapChainExtent.width * pRenderer->swapChainExtent.height;
		
		fr_image_desc_t desc = {};
		desc.size = depthImageSize;
		desc.width = pRenderer->swapChainExtent.width;
		desc.height = pRenderer->swapChainExtent.height;
		desc.format = depthFormat;
		desc.usage = VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT;
		desc.properties = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
		
		fr_image_create(pRenderer->device, pRenderer->physicalDevice, &desc, &pRenderer->depthImage, pAllocCallbacks);
	}
	
	// load character mesh and rig
	const char* depotPath = "../../../../../";
	const char* characterMeshPath = "assets/characters/zelda/mesh/zelda_mesh.fbx";
	const char* characterRigPath = "assets/characters/zelda/mesh/zelda_rig.fbx";
	
	const char* anim_zelda_stand = "assets/characters/zelda/animations/zelda-idle-stand-01.fbx";
	const char* anim_zelda_look = "assets/characters/zelda/animations/zelda-idle-stand-look-around.fbx";
	
	if(res == FR_RESULT_OK)
	{
		fi_depot_t depot;
		depot.path = depotPath;
		
		{
			fi_import_mesh_ctx_t ctx;
			ctx.path = characterMeshPath;
			
			fi_import_mesh(&depot, &ctx, &pRenderer->pMesh, pAllocCallbacks);
		}

		{
			fi_import_rig_ctx_t ctx;
			ctx.path = characterRigPath;
			
			fi_import_rig(&depot, &ctx, &pRenderer->pRig, pAllocCallbacks);
		}
		
		{
			fi_import_anim_clip_ctx_t ctx;
			ctx.path = anim_zelda_stand;
			
			fi_import_anim_clip(&depot, &ctx, &pRenderer->pAnimClip, pAllocCallbacks);
		}
		
		{
			fi_import_anim_clip_ctx_t ctx;
			ctx.path = anim_zelda_look;
			
			fi_import_anim_clip(&depot, &ctx, &pRenderer->pAnimClip2, pAllocCallbacks);
		}
	}
	
	// create mesh and its chunks
	if(res == FR_RESULT_OK)
	{
		const fr_resource_mesh_t* meshResource = pRenderer->pMesh;
		fr_mesh_t* mesh = &pRenderer->mesh;
		
		const uint32_t numChunks = meshResource->numChunks;
		mesh->chunks = (fr_mesh_chunk_t*)FUR_ALLOC_AND_ZERO(sizeof(fr_mesh_chunk_t) * numChunks, 16, FC_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
		mesh->numChunks = numChunks;
		
		const uint32_t numTextureIndices = 7;
		FUR_ASSERT(numChunks == numTextureIndices);
		int32_t textureIndices[numTextureIndices] = {0, 0, 1, 0, 2, 0, 1};
		
		for(uint32_t i=0; i<numChunks; ++i)
		{
			const fr_resource_mesh_chunk_t* meshChunkResource = &meshResource->chunks[i];
			const uint32_t numVertices = meshChunkResource->numVertices;
			const VkDeviceSize sizeIndices = numVertices * sizeof(uint32_t);
			const VkDeviceSize sizeVertices = numVertices * sizeof(fr_vertex_t);
			const VkDeviceSize sizeSkin = numVertices * sizeof(fr_resource_mesh_chunk_skin_t);
			
			fr_mesh_chunk_t* meshChunk = &mesh->chunks[i];
			meshChunk->numIndices = numVertices;
			meshChunk->textureIndex = textureIndices[i];
			
			// data buffer
			{
				fr_buffer_desc_t desc = {};
				desc.size = sizeIndices + sizeVertices + sizeSkin;
				desc.usage = FR_VERTEX_BUFFER_USAGE_FLAGS | VK_BUFFER_USAGE_INDEX_BUFFER_BIT;
				desc.properties = FR_VERTEX_BUFFER_MEMORY_FLAGS;
				
				fr_buffer_create(pRenderer->device, pRenderer->physicalDevice, &desc, &meshChunk->data, pAllocCallbacks);
				
				meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_INDICES] = 0;
				meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_VERTICES] = sizeIndices;
				meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_SKIN] = sizeIndices + sizeVertices;
			}
		}
	}
	
	// create debug lines vertex buffer
	if(res == FR_RESULT_OK)
	{
		fr_buffer_desc_t desc = {};
		desc.size = fc_dbg_line_buffer_size();
		desc.usage = FR_VERTEX_BUFFER_USAGE_FLAGS;
		desc.properties = FR_STAGING_BUFFER_MEMORY_FLAGS;	// use vertex buffer usage, but staging buffer properties
		
		for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			fr_buffer_create(pRenderer->device, pRenderer->physicalDevice, &desc, &pRenderer->debugLinesVertexBuffer[i], pAllocCallbacks);
		}
	}
	
	// create debug triangles vertex buffer
	if(res == FR_RESULT_OK)
	{
		fr_buffer_desc_t desc = {};
		desc.size = fc_dbg_triangle_buffer_size();
		desc.usage = FR_VERTEX_BUFFER_USAGE_FLAGS;
		desc.properties = FR_STAGING_BUFFER_MEMORY_FLAGS;	// use vertex buffer usage, but staging buffer properties
		
		for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			fr_buffer_create(pRenderer->device, pRenderer->physicalDevice, &desc, &pRenderer->debugTrianglesVertexBuffer[i], pAllocCallbacks);
		}
	}
	
	// create frame buffers
	if(res == FR_RESULT_OK)
	{
		for (size_t i = 0; i < NUM_SWAP_CHAIN_IMAGES; i++)
		{
			const uint32_t numAttachments = 2;
			VkImageView attachments[numAttachments] =
			{
				pRenderer->aSwapChainImagesViews[i],
				pRenderer->depthImage.view
			};
			
			VkFramebufferCreateInfo framebufferInfo = {};
			framebufferInfo.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
			framebufferInfo.renderPass = pRenderer->renderPass;
			framebufferInfo.attachmentCount = numAttachments;
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
	
	// create descriptor pool (for uniform buffer & sampler)
	if(res == FR_RESULT_OK)
	{
		uint32_t numBindings = 2;
		VkDescriptorPoolSize poolSizes[numBindings];
		poolSizes[0].type = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
		poolSizes[0].descriptorCount = NUM_SWAP_CHAIN_IMAGES;
		
		poolSizes[1].type = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
		poolSizes[1].descriptorCount = NUM_SWAP_CHAIN_IMAGES;
		
		VkDescriptorPoolCreateInfo poolInfo = {};
		poolInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
		poolInfo.poolSizeCount = numBindings;
		poolInfo.pPoolSizes = poolSizes;
		poolInfo.maxSets = NUM_SWAP_CHAIN_IMAGES;
		
		if (vkCreateDescriptorPool(pRenderer->device, &poolInfo, NULL, &pRenderer->descriptorPool) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create descriptor pool for uniform buffers");
			res = FR_RESULT_ERROR_GPU;
		}
	}
	
	// prepare staging buffer builder
	fr_staging_buffer_builder_t stagingBuilder;
	fr_staging_init(&stagingBuilder);
	
	const VkFormat textureImageFormat = VK_FORMAT_R8G8B8A8_UNORM;
	
	VkDeviceSize imageOffsetInBufferZeldaDiff = 0;
	int texZeldaDiffWidth = 0;
	int texZeldaDiffHeight = 0;
	
	{
		VkDeviceSize imageSize = 0;
		
		// load texture
		{
			int texChannels;
			stbi_uc* pixels = stbi_load(g_texturePathZeldaDiff, &texZeldaDiffWidth, &texZeldaDiffHeight, &texChannels, STBI_rgb_alpha);
			imageSize = texZeldaDiffWidth * texZeldaDiffHeight * 4;
			
			if(!pixels)
			{
				fur_set_last_error("Can't load texture");
				res = FR_RESULT_ERROR_GPU;
			}
			else
			{
				imageOffsetInBufferZeldaDiff = stagingBuilder.totalSize;
				fr_staging_add(&stagingBuilder, pixels, (uint32_t)imageSize, NULL, fr_pixels_free_func);
			}
		}

		// create texture image
		if(res == FR_RESULT_OK)
		{
			fr_image_desc_t desc = {};
			desc.size = imageSize;
			desc.width = texZeldaDiffWidth;
			desc.height = texZeldaDiffHeight;
			desc.format = textureImageFormat;
			desc.usage = VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT;
			desc.properties = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
			
			fr_image_create(pRenderer->device, pRenderer->physicalDevice, &desc, &pRenderer->textureZeldaDiff, pAllocCallbacks);
		}
	}
	
	VkDeviceSize imageOffsetInBufferHairDiff = 0;
	int texHairDiffWidth = 0;
	int texHairDiffHeight = 0;
	
	{
		VkDeviceSize imageSize = 0;
		
		// load texture
		{
			int texChannels;
			stbi_uc* pixels = stbi_load(g_texturePathHairDiff, &texHairDiffWidth, &texHairDiffHeight, &texChannels, STBI_rgb_alpha);
			imageSize = texHairDiffWidth * texHairDiffHeight * 4;
			
			if(!pixels)
			{
				fur_set_last_error("Can't load texture");
				res = FR_RESULT_ERROR_GPU;
			}
			else
			{
				imageOffsetInBufferHairDiff = stagingBuilder.totalSize;
				fr_staging_add(&stagingBuilder, pixels, (uint32_t)imageSize, NULL, fr_pixels_free_func);
			}
		}
		
		// create texture image
		if(res == FR_RESULT_OK)
		{
			fr_image_desc_t desc = {};
			desc.size = imageSize;
			desc.width = texHairDiffWidth;
			desc.height = texHairDiffHeight;
			desc.format = textureImageFormat;
			desc.usage = VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT;
			desc.properties = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
			
			fr_image_create(pRenderer->device, pRenderer->physicalDevice, &desc, &pRenderer->textureHairDiff, pAllocCallbacks);
		}
	}
	
	VkDeviceSize imageOffsetInBufferEyesDiff = 0;
	int texEyesDiffWidth = 0;
	int texEyesDiffHeight = 0;
	
	{
		VkDeviceSize imageSize = 0;
		
		// load texture
		{
			int texChannels;
			stbi_uc* pixels = stbi_load(g_texturePathEyesDiff, &texEyesDiffWidth, &texEyesDiffHeight, &texChannels, STBI_rgb_alpha);
			imageSize = texEyesDiffWidth * texEyesDiffHeight * 4;
			
			if(!pixels)
			{
				fur_set_last_error("Can't load texture");
				res = FR_RESULT_ERROR_GPU;
			}
			else
			{
				imageOffsetInBufferEyesDiff = stagingBuilder.totalSize;
				fr_staging_add(&stagingBuilder, pixels, (uint32_t)imageSize, NULL, fr_pixels_free_func);
			}
		}
		
		// create texture image
		if(res == FR_RESULT_OK)
		{
			fr_image_desc_t desc = {};
			desc.size = imageSize;
			desc.width = texEyesDiffWidth;
			desc.height = texEyesDiffHeight;
			desc.format = textureImageFormat;
			desc.usage = VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT;
			desc.properties = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
			
			fr_image_create(pRenderer->device, pRenderer->physicalDevice, &desc, &pRenderer->textureEyesDiff, pAllocCallbacks);
		}
	}
		
	// create texture sampler
	if(res == FR_RESULT_OK)
	{
		VkSamplerCreateInfo samplerInfo = {};
		samplerInfo.sType = VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;
		samplerInfo.magFilter = VK_FILTER_LINEAR;
		samplerInfo.minFilter = VK_FILTER_LINEAR;
		samplerInfo.addressModeU = VK_SAMPLER_ADDRESS_MODE_REPEAT;
		samplerInfo.addressModeV = VK_SAMPLER_ADDRESS_MODE_REPEAT;
		samplerInfo.addressModeW = VK_SAMPLER_ADDRESS_MODE_REPEAT;
		samplerInfo.anisotropyEnable = VK_TRUE;
		samplerInfo.maxAnisotropy = 16;
		samplerInfo.borderColor = VK_BORDER_COLOR_INT_OPAQUE_BLACK;
		samplerInfo.unnormalizedCoordinates = VK_FALSE;
		samplerInfo.compareEnable = VK_FALSE;
		samplerInfo.compareOp = VK_COMPARE_OP_ALWAYS;
		samplerInfo.mipmapMode = VK_SAMPLER_MIPMAP_MODE_LINEAR;
		samplerInfo.mipLodBias = 0.0f;
		samplerInfo.minLod = 0.0f;
		samplerInfo.maxLod = 0.0f;
		
		if (vkCreateSampler(pRenderer->device, &samplerInfo, NULL, &pRenderer->textureSampler) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create texture sampler");
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
			VkDescriptorBufferInfo bufferInfo[2] = {};
			bufferInfo[0].buffer = pRenderer->aUniformBuffer[i].buffer;
			bufferInfo[0].offset = 0;
			bufferInfo[0].range = sizeof(fr_uniform_buffer_t);
			
			bufferInfo[1].buffer = pRenderer->aSkinningBuffer[i].buffer;
			bufferInfo[1].offset = 0;
			bufferInfo[1].range = sizeof(fr_skinning_buffer_t);
			
			VkDescriptorImageInfo imageInfo[3] = {};
			imageInfo[0].imageLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
			imageInfo[0].imageView = pRenderer->textureZeldaDiff.view;
			imageInfo[0].sampler = pRenderer->textureSampler;
			
			imageInfo[1].imageLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
			imageInfo[1].imageView = pRenderer->textureHairDiff.view;
			imageInfo[1].sampler = pRenderer->textureSampler;
			
			imageInfo[2].imageLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
			imageInfo[2].imageView = pRenderer->textureEyesDiff.view;
			imageInfo[2].sampler = pRenderer->textureSampler;
			
			const uint32_t numBindings = 3;
			VkWriteDescriptorSet descriptorWrites[numBindings] = {};
			
			descriptorWrites[0].sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
			descriptorWrites[0].dstSet = pRenderer->aDescriptorSets[i];
			descriptorWrites[0].dstBinding = 0;
			descriptorWrites[0].dstArrayElement = 0;
			descriptorWrites[0].descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
			descriptorWrites[0].descriptorCount = 1;
			descriptorWrites[0].pBufferInfo = &bufferInfo[0];
			descriptorWrites[0].pImageInfo = NULL; // Optional
			descriptorWrites[0].pTexelBufferView = NULL; // Optional
			
			descriptorWrites[1].sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
			descriptorWrites[1].dstSet = pRenderer->aDescriptorSets[i];
			descriptorWrites[1].dstBinding = 1;
			descriptorWrites[1].dstArrayElement = 0;
			descriptorWrites[1].descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
			descriptorWrites[1].descriptorCount = 1;
			descriptorWrites[1].pBufferInfo = &bufferInfo[1];
			descriptorWrites[1].pImageInfo = NULL; // Optional
			descriptorWrites[1].pTexelBufferView = NULL; // Optional
			
			descriptorWrites[2].sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
			descriptorWrites[2].dstSet = pRenderer->aDescriptorSets[i];
			descriptorWrites[2].dstBinding = 2;
			descriptorWrites[2].dstArrayElement = 0;
			descriptorWrites[2].descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
			descriptorWrites[2].descriptorCount = NUM_TEXTURES_IN_ARRAY;	// number of textures in array goes here
			descriptorWrites[2].pImageInfo = imageInfo;

			vkUpdateDescriptorSets(pRenderer->device, numBindings, descriptorWrites, 0, NULL);
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
			
			const uint32_t numClearValues = 2;
			VkClearValue clearColor[numClearValues] = {};
			clearColor[0].color.float32[0] = 0.0f;
			clearColor[0].color.float32[1] = 0.0f;
			clearColor[0].color.float32[2] = 0.0f;
			clearColor[0].color.float32[3] = 1.0f;
			
			clearColor[1].depthStencil.depth = 1.0f;
			clearColor[1].depthStencil.stencil = 0;
			
			renderPassInfo.clearValueCount = numClearValues;
			renderPassInfo.pClearValues = clearColor;
			
			vkCmdBeginRenderPass(pRenderer->aCommandBuffers[i], &renderPassInfo, VK_SUBPASS_CONTENTS_INLINE);
			
			vkCmdBindPipeline(pRenderer->aCommandBuffers[i], VK_PIPELINE_BIND_POINT_GRAPHICS, pRenderer->graphicsPipeline);
			
			// bind uniform buffer
			vkCmdBindDescriptorSets(pRenderer->aCommandBuffers[i], VK_PIPELINE_BIND_POINT_GRAPHICS,
									pRenderer->pipelineLayout, 0, 1, &pRenderer->aDescriptorSets[i], 0, NULL);
			
			VkDeviceSize offsets[] = {0};

			// bind and draw mesh chunks
			const fr_mesh_t* mesh = &pRenderer->mesh;
			for(uint32_t idxChunk=0; idxChunk<mesh->numChunks; ++idxChunk)
			{
				const fr_mesh_chunk_t* meshChunk = &mesh->chunks[idxChunk];
				
				// bind vertex and index buffers
				if(meshChunk->data.buffer)	// todo: previously checking 'has skin'
				{
					VkDeviceSize offsets[] = {meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_VERTICES], meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_SKIN]};
					VkBuffer vertexBuffers[] = {meshChunk->data.buffer, meshChunk->data.buffer};
					vkCmdBindVertexBuffers(pRenderer->aCommandBuffers[i], 0, 2, vertexBuffers, offsets);
				}
				else
				{
					FUR_ASSERT(false);	// implement 'has skinning'
					VkDeviceSize offsets[] = {meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_VERTICES]};
					VkBuffer vertexBuffers[] = {meshChunk->data.buffer};
					vkCmdBindVertexBuffers(pRenderer->aCommandBuffers[i], 0, 1, vertexBuffers, offsets);
				}
				
				vkCmdBindIndexBuffer(pRenderer->aCommandBuffers[i], meshChunk->data.buffer, meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_INDICES], VK_INDEX_TYPE_UINT32);
				
				vkCmdPushConstants(pRenderer->aCommandBuffers[i], pRenderer->pipelineLayout,
								   VK_SHADER_STAGE_FRAGMENT_BIT, 0, sizeof(int32_t), &meshChunk->textureIndex);
				
				// draw the mesh chunk
				vkCmdDrawIndexed(pRenderer->aCommandBuffers[i], meshChunk->numIndices, 1, 0, 0, 0);
			}
			 
			// debug draw lines
			vkCmdBindPipeline(pRenderer->aCommandBuffers[i], VK_PIPELINE_BIND_POINT_GRAPHICS, pRenderer->debugLinesPSO);
			vkCmdBindVertexBuffers(pRenderer->aCommandBuffers[i], 0, 1, &pRenderer->debugLinesVertexBuffer[i].buffer, offsets);
			vkCmdDraw(pRenderer->aCommandBuffers[i], fc_dbg_line_num_total_vertices(), 0, 0, 0);
			
			// debug draw triangles
			vkCmdBindPipeline(pRenderer->aCommandBuffers[i], VK_PIPELINE_BIND_POINT_GRAPHICS, pRenderer->debugTrianglesPSO);
			vkCmdBindVertexBuffers(pRenderer->aCommandBuffers[i], 0, 1, &pRenderer->debugTrianglesVertexBuffer[i].buffer, offsets);
			vkCmdDraw(pRenderer->aCommandBuffers[i], fc_dbg_triangles_num_total_vertices(), 0, 0, 0);
			
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
		for(uint32_t i=0; i<pRenderer->pMesh->numChunks; ++i)
		{
			fr_resource_mesh_chunk_t* meshChunk = &pRenderer->pMesh->chunks[i];
			
			fr_staging_add(&stagingBuilder, (void*)meshChunk->dataIndices, sizeof(uint32_t) * meshChunk->numIndices, NULL, NULL);
			fr_staging_add(&stagingBuilder, (void*)meshChunk->dataVertices, sizeof(fr_vertex_t) * meshChunk->numVertices, NULL, NULL);
			
			// if skinned mesh
			if(meshChunk->dataSkinning != NULL)
				fr_staging_add(&stagingBuilder, (void*)meshChunk->dataSkinning, sizeof(fr_resource_mesh_chunk_skin_t) * meshChunk->numVertices, NULL, NULL);
		}
		
		// create staging buffer & release memory of source data
		{
			pRenderer->stagingBuffer.size = stagingBuilder.totalSize;
			fr_staging_build(&stagingBuilder, pRenderer->device, pRenderer->physicalDevice, &pRenderer->stagingBuffer.buffer, &pRenderer->stagingBuffer.memory, pAllocCallbacks);
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
			
			const uint32_t numBuffersPerMeshChunk = 3;
			const uint32_t numBuffersPerNonSkinnedMeshChunk = 2;
			
			// copy vertex buffer region
			for(uint32_t i=0; i<pRenderer->mesh.numChunks; ++i)
			{
				fr_mesh_chunk_t* meshChunk = &pRenderer->mesh.chunks[i];
				fr_resource_mesh_chunk_t* sourceMeshChunk = &pRenderer->pMesh->chunks[i];
				
				if(sourceMeshChunk->dataSkinning)
				{
					uint32_t srcStagingIndices[numBuffersPerMeshChunk] = {3+i*numBuffersPerMeshChunk, 4+i*numBuffersPerMeshChunk, 5+i*numBuffersPerMeshChunk};
					VkBuffer dstBuffers[numBuffersPerMeshChunk] = {meshChunk->data.buffer, meshChunk->data.buffer, meshChunk->data.buffer};
					VkDeviceSize dstOffsets[numBuffersPerMeshChunk] = {meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_INDICES], meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_VERTICES], meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_SKIN]};
					
					fr_staging_record_copy_commands(&stagingBuilder, commandBuffer, pRenderer->stagingBuffer.buffer, srcStagingIndices, dstBuffers, dstOffsets, numBuffersPerMeshChunk);
				}
				else
				{
					uint32_t srcStagingIndices[numBuffersPerNonSkinnedMeshChunk] = {3+i*numBuffersPerNonSkinnedMeshChunk, 4+i*numBuffersPerNonSkinnedMeshChunk};
					VkBuffer dstBuffers[numBuffersPerNonSkinnedMeshChunk] = {meshChunk->data.buffer, meshChunk->data.buffer};
					VkDeviceSize dstOffsets[numBuffersPerNonSkinnedMeshChunk] = {meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_INDICES], meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_VERTICES]};
					
					fr_staging_record_copy_commands(&stagingBuilder, commandBuffer, pRenderer->stagingBuffer.buffer, srcStagingIndices, dstBuffers, dstOffsets, numBuffersPerNonSkinnedMeshChunk);
				}
			}
			
			fr_end_simple_commands(pRenderer->device, pRenderer->graphicsQueue, commandBuffer, pRenderer->stagingCommandPool, pAllocCallbacks);
		}
		
		// depth layout transitions
		{
			fr_transition_image_layout(pRenderer->device, pRenderer->graphicsQueue, pRenderer->stagingCommandPool, depthFormat,
									   VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, pRenderer->depthImage.image, pAllocCallbacks);
		}
		
		// image layout transitions
		{
			// zelda_diff
			fr_transition_image_layout(pRenderer->device, pRenderer->graphicsQueue, pRenderer->stagingCommandPool, textureImageFormat,
									   VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, pRenderer->textureZeldaDiff.image, pAllocCallbacks);
			fr_copy_buffer_to_image(pRenderer->device, pRenderer->graphicsQueue, pRenderer->stagingCommandPool, pRenderer->stagingBuffer.buffer,
									imageOffsetInBufferZeldaDiff, pRenderer->textureZeldaDiff.image, texZeldaDiffWidth, texZeldaDiffHeight, pAllocCallbacks);
			 
			// hair_diff
			fr_transition_image_layout(pRenderer->device, pRenderer->graphicsQueue, pRenderer->stagingCommandPool, textureImageFormat,
									   VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, pRenderer->textureHairDiff.image, pAllocCallbacks);
			fr_copy_buffer_to_image(pRenderer->device, pRenderer->graphicsQueue, pRenderer->stagingCommandPool, pRenderer->stagingBuffer.buffer,
									imageOffsetInBufferHairDiff, pRenderer->textureHairDiff.image, texHairDiffWidth, texHairDiffHeight, pAllocCallbacks);
			
			// eyes_diff
			fr_transition_image_layout(pRenderer->device, pRenderer->graphicsQueue, pRenderer->stagingCommandPool, textureImageFormat,
									   VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, pRenderer->textureEyesDiff.image, pAllocCallbacks);
			fr_copy_buffer_to_image(pRenderer->device, pRenderer->graphicsQueue, pRenderer->stagingCommandPool, pRenderer->stagingBuffer.buffer,
									imageOffsetInBufferEyesDiff, pRenderer->textureEyesDiff.image, texEyesDiffWidth, texEyesDiffHeight, pAllocCallbacks);
		}
		
		// release staging buffer
		{
			fr_buffer_release(pRenderer->device, &pRenderer->stagingBuffer, pAllocCallbacks);
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
	
	// clear debug fragments buffers buffer with zeros
	{
		for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			fr_clear_data_in_buffer(pRenderer->device, pRenderer->debugLinesVertexBuffer[i].memory, 0, fc_dbg_line_buffer_size());
		}
		
		for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			fr_clear_data_in_buffer(pRenderer->device, pRenderer->debugTrianglesVertexBuffer[i].memory, 0, fc_dbg_triangle_buffer_size());
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
	pRenderer->cameraZoom = 1.0f;
	
	pRenderer->scratchpadBufferSize = 128 * 1024;
	pRenderer->scratchpadBuffer = FUR_ALLOC_AND_ZERO(pRenderer->scratchpadBufferSize, 16, FC_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
	
	// create skinning mapping
	{
		const fr_resource_mesh_chunk_t* meshChunk = &pRenderer->pMesh->chunks[0];
		const uint32_t numBones = meshChunk->numBones;
		pRenderer->skinningMapping.indicesMapping = FUR_ALLOC_ARRAY_AND_ZERO(uint32_t, numBones, 0, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
		pRenderer->skinningMapping.count = numBones;
		
		uint32_t* mapping = pRenderer->skinningMapping.indicesMapping;
		const fa_rig_t* rig = pRenderer->pRig;
		const uint32_t rigNumBones = rig->numBones;
		
		for(uint32_t i=0; i<numBones; ++i)
		{
			mapping[i] = 0;
			for(uint32_t r=0; r<rigNumBones; ++r)
			{
				if(meshChunk->boneNameHashes[i] == rig->boneNameHashes[r])
				{
					mapping[i] = r;
				}
			}
		}
	}
	
	return res;
}

enum fr_result_t fr_release_renderer(struct fr_renderer_t* pRenderer,
					   struct fc_alloc_callbacks_t*	pAllocCallbacks)
{
	fi_input_manager_release(pRenderer->pInputManager, pAllocCallbacks);	// todo: move out of renderer
	
	FUR_FREE(pRenderer->scratchpadBuffer, pAllocCallbacks);
	
	FUR_FREE(pRenderer->skinningMapping.indicesMapping, pAllocCallbacks);
	
	// release character mesh
	if(pRenderer->pMesh)
	{
		fr_release_mesh(&pRenderer->pMesh, pAllocCallbacks);
	}
	
	// this should be in clean-up swap chain
	{
		// destroy uniform buffer
		for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			fr_buffer_release(pRenderer->device, &pRenderer->aUniformBuffer[i], pAllocCallbacks);
			fr_buffer_release(pRenderer->device, &pRenderer->aSkinningBuffer[i], pAllocCallbacks);
		}
		
		vkDestroyDescriptorPool(pRenderer->device, pRenderer->descriptorPool, NULL);
	}
	
	// destroy depth image
	fr_image_release(pRenderer->device, &pRenderer->depthImage, pAllocCallbacks);
	
	// destroy image
	fr_image_release(pRenderer->device, &pRenderer->textureZeldaDiff, pAllocCallbacks);
	fr_image_release(pRenderer->device, &pRenderer->textureHairDiff, pAllocCallbacks);
	fr_image_release(pRenderer->device, &pRenderer->textureEyesDiff, pAllocCallbacks);
	
	vkDestroySampler(pRenderer->device, pRenderer->textureSampler, NULL);
	
	// destroy mesh
	for(uint32_t i=0; i<pRenderer->mesh.numChunks; ++i)
	{
		fr_mesh_chunk_t* meshChunk = &pRenderer->mesh.chunks[i];
		
		// destroy vertex buffer
		fr_buffer_release(pRenderer->device, &meshChunk->data, pAllocCallbacks);
	}
	
	FUR_FREE(pRenderer->mesh.chunks, pAllocCallbacks);
	
	// destroy debug fragments vertex buffer
	for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
	{
		fr_buffer_release(pRenderer->device, &pRenderer->debugLinesVertexBuffer[i], pAllocCallbacks);
		fr_buffer_release(pRenderer->device, &pRenderer->debugTrianglesVertexBuffer[i], pAllocCallbacks);
	}
	
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
	
	// destroy debug PSO
	vkDestroyPipeline(pRenderer->device, pRenderer->debugLinesPSO, NULL);
	vkDestroyPipeline(pRenderer->device, pRenderer->debugTrianglesPSO, NULL);
	
	// destroy shaders
	vkDestroyShaderModule(pRenderer->device, pRenderer->vertexShaderModule, NULL);
	vkDestroyShaderModule(pRenderer->device, pRenderer->fragmentShaderModule, NULL);
	
	vkDestroyShaderModule(pRenderer->device, pRenderer->debugVertexShaderModule, NULL);
	vkDestroyShaderModule(pRenderer->device, pRenderer->debugFragmentShaderModule, NULL);
	
	vkDestroyShaderModule(pRenderer->device, pRenderer->textVertexShaderModule, NULL);
	vkDestroyShaderModule(pRenderer->device, pRenderer->textFragmentShaderModule, NULL);
	
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

const float g_rotationSpeed = FM_DEG_TO_RAD(90);
const float g_zoomSpeed = 0.2f;
const fm_vec4 g_eye = {0, -3, 1.4, 0};
const fm_vec4 g_at = {0, 0, 1.5, 0};
const fm_vec4 g_up = {0, 0, 1, 0};

double g_timeDelta = 0.0f;
double g_time = 0.0f;

void fr_dbg_draw_mat4(const fm_mat4_t* m)
{
	const float pos[3] = {m->w.x, m->w.y, m->w.z};
	const float scale = 0.1f;
	const float axisX[3] = {pos[0] + m->x.x * scale, pos[1] + m->x.y * scale, pos[2] + m->x.z * scale};
	const float axisY[3] = {pos[0] + m->y.x * scale, pos[1] + m->y.y * scale, pos[2] + m->y.z * scale};
	const float axisZ[3] = {pos[0] + m->z.x * scale, pos[1] + m->z.y * scale, pos[2] + m->z.z * scale};
	
	const float red[4] = FUR_COLOR_RED;
	const float green[4] = FUR_COLOR_GREEN;
	const float blue[4] = FUR_COLOR_BLUE;
	
	fc_dbg_line(pos, axisX, red);
	fc_dbg_line(pos, axisY, green);
	fc_dbg_line(pos, axisZ, blue);
}

float g_blend = 0.0f;

void fr_update_renderer(struct fr_renderer_t* pRenderer, const struct fr_update_context_t* ctx)
{
	g_timeDelta = ctx->dt;
	g_time += g_timeDelta;
	
	fi_update_input_manager(pRenderer->pInputManager, g_time);	// todo: move out of renderer
	
	static float actionRotationLeftX = 0.0f;
	static float actionZoomIn = 0.0f;
	static float actionZoomOut = 0.0f;
	
	fi_input_event_t inputEvents[10];
	const uint32_t numEventsCollected = fi_get_input_events(pRenderer->pInputManager, inputEvents, 10, 0);
	for(uint32_t i=0; i<numEventsCollected; ++i)
	{
		if(inputEvents[i].eventID == Gamepad_rightAnalogX)
		{
			actionRotationLeftX = fm_snap_near_zero(inputEvents[i].value, 0.05f);
		}
		else if(inputEvents[i].eventID == Gamepad_rightTrigger)
		{
			actionZoomIn = fm_snap_near_zero(inputEvents[i].value, 0.05f);
		}
		else if(inputEvents[i].eventID == Gamepad_leftTrigger)
		{
			actionZoomOut = fm_snap_near_zero(inputEvents[i].value, 0.05f);
		}
	}
	
	g_blend = fm_clamp((sinf(g_time / 5.0f) + 1.0f) / 2.0f, 0.0f, 1.0f);
	
	pRenderer->rotationAngle += g_rotationSpeed * ctx->dt * actionRotationLeftX;
	pRenderer->cameraZoom += g_zoomSpeed * ctx->dt * (actionZoomOut - actionZoomIn);
};

uint32_t g_prevImageIndex = 0;

void fr_draw_frame(struct fr_renderer_t* pRenderer)
{
	fr_skinning_buffer_t skinBuffer = {};
	
	if(pRenderer->pRig)
	{
		const uint32_t numBones = pRenderer->pRig->numBones;
		const int16_t* parentIndices = pRenderer->pRig->parents;
		
		fa_pose_stack_t poseStack;
		memset(&poseStack, 0, sizeof(poseStack));
		
		// init pose stack
		{
			fa_pose_stack_desc_t desc;
			memset(&desc, 0, sizeof(fa_pose_stack_desc_t));
			
			desc.numBonesPerPose = pRenderer->pRig->numBones;
			desc.numTracksPerPose = 0;
			desc.numMaxPoses = 4;
			
			fa_pose_stack_init(&poseStack, &desc, pRenderer->scratchpadBuffer, pRenderer->scratchpadBufferSize);
		}
		
		fa_pose_t refPose;
		refPose.numTracks = 0;
		refPose.numXforms = pRenderer->pRig->numBones;
		refPose.xforms = pRenderer->pRig->refPose;
		refPose.tracks = NULL;
		refPose.weightsXforms = NULL;
		refPose.weightsTracks = NULL;
		refPose.flags = 0;
		
		fm_mat4_t mat;
		
		fa_pose_stack_push(&poseStack, 4);
		
		fa_pose_t modelPose;
		fa_pose_stack_get(&poseStack, &modelPose, 0);
		
		fa_pose_t pose_stand;
		fa_pose_stack_get(&poseStack, &pose_stand, 1);
		
		fa_pose_t pose_look;
		fa_pose_stack_get(&poseStack, &pose_look, 2);
		
		fa_pose_t pose_temp;
		fa_pose_stack_get(&poseStack, &pose_temp, 3);
		
		fa_pose_copy(&modelPose, &refPose);
		fa_pose_copy(&pose_stand, &refPose);
		fa_pose_copy(&pose_look, &refPose);
		fa_pose_copy(&pose_temp, &refPose);
		
		const float animTime = fmodf(g_time, pRenderer->pAnimClip->duration);
		fa_anim_clip_sample(pRenderer->pAnimClip, animTime, &pose_stand);
		
		const float animTime2 = fmodf(g_time, pRenderer->pAnimClip2->duration);
		fa_anim_clip_sample(pRenderer->pAnimClip2, animTime2, &pose_look);
		
		fa_pose_blend_linear(&pose_temp, &pose_stand, &pose_look, g_blend);
		
		fa_pose_local_to_model(&modelPose, &pose_temp, parentIndices);
		
		float color[4] = FUR_COLOR_CYAN;
		
		for(uint32_t i=0; i<numBones; ++i)
		{
			fm_xform_to_mat4(&modelPose.xforms[i], &mat);
			//fr_dbg_draw_mat4(&mat);
			
			int16_t idxParent = parentIndices[i];
			if(idxParent >= 0)
			{
				//fc_dbg_line(&modelPose.xforms[i].pos.x, &modelPose.xforms[idxParent].pos.x, color);
			}
		}
		
		// pass skinning
		const uint32_t skinNumBones = pRenderer->skinningMapping.count;
		for(uint32_t i=0; i<skinNumBones; ++i)
		{
			const uint32_t srcBoneIndex = pRenderer->skinningMapping.indicesMapping[i];
			fm_xform_to_mat4(&modelPose.xforms[srcBoneIndex], &skinBuffer.bones[i]);
		}
		
		const fm_mat4* bindPose = pRenderer->pMesh->chunks[0].bindPose;
		fm_mat4 testMatrices[400];
		
		for(uint32_t i=0; i<skinNumBones; ++i)
		{
			const uint32_t srcBoneIndex = pRenderer->skinningMapping.indicesMapping[i];
			fm_xform_to_mat4(&modelPose.xforms[srcBoneIndex], &skinBuffer.bones[i]);
			fm_mat4_mul(&bindPose[i], &skinBuffer.bones[i], &testMatrices[i]);
			skinBuffer.bones[i] = testMatrices[i];
		}
		
		fa_pose_stack_pop(&poseStack, 4);
		
		fm_quat g_rotX;
		fm_vec4 g_vx;
		
		fm_euler_angles angles;
		angles.yaw = FM_DEG_TO_RAD(45);
		angles.pitch = FM_DEG_TO_RAD(0);
		angles.roll = FM_DEG_TO_RAD(0);
		fm_quat_make_from_euler_angles_yzpxry(&angles, &g_rotX);
		g_vx.x = 0.0f;
		g_vx.y = 1.0f;
		g_vx.z = 0.0f;
		g_vx.w = 0.0f;
		
		fm_vec4 v;
		fm_quat_rot(&g_rotX, &g_vx, &v);
		
		fm_vec4 vzero;
		fm_vec4_zeros(&vzero);
		
		fm_xform root;
		fm_xform_identity(&root);
		root.pos.x = 1.0f;
		
		fm_quat_make_from_euler_angles_yzpxry(&angles, &root.rot);
		
		fm_xform hips;
		fm_xform_identity(&hips);
		
		fm_xform hips_model;
		fm_xform hips_model2;
		
		fm_xform_mul(&root, &hips, &hips_model);
		fm_xform_mul(&root, &hips_model, &hips_model2);
		
		//fm_xform_to_mat4(&hips_model, &mat);
		//fm_xform_to_mat4(&hips_model2, &mat);
		
		//fr_dbg_draw_mat4(&mat);
		//fr_dbg_draw_mat4(&mat);
		//fc_dbg_line(&vzero.x, &g_vx.x, color2);
		//fc_dbg_line(&vzero.x, &v.x, color);
	}
	
	/*
	const uint32_t numChunks = pRenderer->pMesh->numChunks;
	for(uint32_t i=0; i<numChunks; ++i)
	{
		const fr_resource_mesh_chunk_t* chunk = &pRenderer->pMesh->chunks[i];
		for(uint32_t b=0; b<chunk->numBones; ++b)
		{
			const fm_mat4_t* m = &chunk->bindPose[b];
			fm_mat4_t tm = *m;
			fm_mat4_transpose(&tm);
			fr_dbg_draw_mat4(&tm);
		}
	}
	 */
	
	uint32_t imageIndex;
	vkAcquireNextImageKHR(pRenderer->device, pRenderer->swapChain, (uint64_t)-1,
						  pRenderer->imageAvailableSemaphore, VK_NULL_HANDLE, &imageIndex);
	
	// pass skinning matrices to skin buffer
	{
		fr_copy_data_to_buffer(pRenderer->device, pRenderer->aSkinningBuffer[imageIndex].memory, &skinBuffer, 0, sizeof(fr_skinning_buffer_t));
	}
	
	if(imageIndex != g_prevImageIndex)
	{
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
		
		fm_vec4 eye = g_eye;
		fm_vec4_sub(&eye, &g_at, &eye);
		fm_vec4_mulf(&eye, pRenderer->cameraZoom, &eye);
		fm_vec4_add(&eye, &g_at, &eye);
		
		fm_mat4_lookat(&eye, &g_at, &g_up, &tempView);
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
		
		fr_copy_data_to_buffer(pRenderer->device, pRenderer->aUniformBuffer[imageIndex].memory, &ubo, 0, sizeof(fr_uniform_buffer_t));
	}
	
	const float begin[3] = {0.0f, 0.0f, 0.0f};
	const float axisX[3] = {1.0f, 0.0f, 0.0f};
	const float axisY[3] = {0.0f, 1.0f, 0.0f};
	const float axisZ[3] = {0.0f, 0.0f, 1.0f};
	const float colorRed[4] = FUR_COLOR_RED;
	const float colorGreen[4] = FUR_COLOR_GREEN;
	const float colorBlue[4] = FUR_COLOR_BLUE;
	fc_dbg_line(begin, axisX, colorRed);
	fc_dbg_line(begin, axisY, colorGreen);
	fc_dbg_line(begin, axisZ, colorBlue);
	
	// update debug lines buffer
	{
		fc_dbg_buffers_lock();	// lock so no-one adds lines while retriving the buffer
		
		// copy debug lines buffer into vertex buffer
		fc_dbg_buffer_desc_t desc;
		fc_dbg_get_buffers(&desc);
		
		if(desc.linesDataSize > 0)
		{
			fr_clear_data_in_buffer(pRenderer->device, pRenderer->debugLinesVertexBuffer[imageIndex].memory, 0, desc.linesDataSize);
			fr_copy_data_to_buffer(pRenderer->device, pRenderer->debugLinesVertexBuffer[imageIndex].memory, desc.linesData, 0, desc.linesDataSize);
		}
		
		if(desc.trianglesDataSize > 0)
		{
			fr_clear_data_in_buffer(pRenderer->device, pRenderer->debugTrianglesVertexBuffer[imageIndex].memory, 0, desc.trianglesDataSize);
			fr_copy_data_to_buffer(pRenderer->device, pRenderer->debugTrianglesVertexBuffer[imageIndex].memory, desc.trianglesData, 0, desc.trianglesDataSize);
		}
		
		fc_dbg_buffers_clear();	// clear the buffer for next frame
		
		fc_dbg_buffers_unlock();	// release lock, so now on everyone can use debug fragments again
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
