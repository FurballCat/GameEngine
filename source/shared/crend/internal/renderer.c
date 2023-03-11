/* Copyright (c) 2016-2019 Furball Cat */

#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "ccore/public.h"
#include "ccore/textParsing.h"
#include "ccore/buffer.h"

#include "renderer.h"
#include "vulkan.h"
#include "glfw/glfw3.h"

#include "renderBuffer.h"
#include "image.h"
#include "renderUtils.h"
#include "psoUtils.h"
#include "pvs.h"

#include "cimport/public.h"
#include "ccore/serialize.h"

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

#define MAX(a,b) a > b ? a : b
#define MIN(a,b) a < b ? a : b

#define S1(x) #x
#define S2(x) S1(x)

#define USE_PVS 1

/*************************************************************/

#define FC_ERROR_MESSAGE_MAX_LENGTH 512
char g_lastError[FC_ERROR_MESSAGE_MAX_LENGTH] = {0};

const char* fr_get_last_error(void)
{
	return g_lastError;
}

void fur_set_last_error(const char* error)
{
	uint64_t length = strlen(error);
	FUR_ASSERT(length < FC_ERROR_MESSAGE_MAX_LENGTH);
	memcpy((char*)g_lastError, error, length);
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
	struct fr_app_t* pApp = FUR_ALLOC(sizeof(struct fr_app_t), 8, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
	
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

#define FR_FONT_FLOATS_PER_GLYPH_VERTEX 7

typedef struct fr_font_glyph_t
{
	char character;
	float uvMin[2];
	float uvMax[2];
	uint32_t size[2];	// width and height in pixels
} fr_font_glyph_t;

typedef struct fr_font_t
{
	fr_font_glyph_t* glyphs;	// sorted by character in ASCII, use character as index
	uint32_t numGlyphs;
	uint32_t offsetGlyphs;
	
	fr_image_t atlas;
	uint32_t atlasWidth;
	uint32_t atlasHeight;
	
	void* pixelsData;	// used for staging buffer
} fr_font_t;

typedef struct fr_font_desc_t
{
	const char* atlasPath;		// image with all glyphs
	const char* glyphsInfoPath;	// UV and sizes of each glyph and character mapping
} fr_font_desc_t;

void fr_font_release(VkDevice device, fr_font_t* font, fc_alloc_callbacks_t* pAllocCallbacks)
{
	fr_image_release(device, &font->atlas, pAllocCallbacks);
	
	if(font->pixelsData)	// this is optional, can be released earlier during staging buffer phase
	{
		FUR_FREE(font->pixelsData, pAllocCallbacks);
	}
	
	FUR_FREE(font->glyphs, pAllocCallbacks);
	
	memset(font, 0, sizeof(fr_font_t));
}

enum fr_result_t fr_font_create(VkDevice device, VkPhysicalDevice physicalDevice, const fr_font_desc_t* desc, fr_font_t* font, fc_alloc_callbacks_t* pAllocCallbacks)
{
	enum fr_result_t res = FR_RESULT_OK;
	
	// load glyph atlas
	{
		VkDeviceSize imageSize = 0;
		int32_t width = 0;
		int32_t height = 0;
		
		// load texture
		{
			int texChannels;
			font->pixelsData = (void*)stbi_load(desc->atlasPath, &width, &height, &texChannels, STBI_rgb_alpha);
			font->atlasWidth = width;
			font->atlasHeight = height;
			
			imageSize = width * height * 4;
			
			if(!font->pixelsData)
			{
				char txt[256];
				sprintf(txt, "Can't load font atlas \'%s\'", desc->atlasPath);
				fur_set_last_error(txt);
				res = FR_RESULT_ERROR;
			}
		}
		
		// create texture image
		if(res == FR_RESULT_OK)
		{
			fr_image_desc_t desc = {0};
			desc.size = imageSize;
			desc.width = font->atlasWidth;
			desc.height = font->atlasHeight;
			desc.format = VK_FORMAT_R8G8B8A8_UNORM;
			desc.usage = VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT;
			desc.properties = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
			
			fr_image_create(device, physicalDevice, &desc, &font->atlas, pAllocCallbacks);
		}
	}
	
	// read glyph infos
	if(res == FR_RESULT_OK)
	{
		fc_text_buffer_t textBuffer = {0};
		if(!fc_load_text_file_into_text_buffer(desc->glyphsInfoPath, &textBuffer, pAllocCallbacks))
		{
			char txt[256];
			sprintf(txt, "Can't load file %s", desc->glyphsInfoPath);
			fur_set_last_error(txt);
			return FR_RESULT_ERROR;
		}

		const char* streamBegin = textBuffer.pData;
		const char* streamEnd = textBuffer.pData + textBuffer.size;
		
		// count number of glyphs
		uint32_t numGlyphs = 0;
		
		{
			fc_text_stream_ro_t stream = {streamBegin, streamEnd};
			while(!fc_text_stream_is_eof(&stream, 0))
			{
				if(!fc_text_parse_skip_line(&stream))
					break;
				
				++numGlyphs;
			}
		}
		
		FUR_ASSERT(numGlyphs > 0);
		font->glyphs = FUR_ALLOC_ARRAY_AND_ZERO(fr_font_glyph_t, numGlyphs, 8, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
		font->numGlyphs = numGlyphs;
		
		// parse content
		{
			int32_t parsingErrorGlyphIndex = -1;
			
			fc_text_stream_ro_t stream = {streamBegin, streamEnd};
			for(uint32_t i=0; i<numGlyphs; ++i)
			{
				int32_t asciiNumber = 0;
				if(!fc_text_parse_int32(&stream, &asciiNumber))
				{
					parsingErrorGlyphIndex = i;
					break;
				}
				
				char asciiChar = '\0';
				
				if(!fc_text_parse_character(&stream, &asciiChar))	// skip first char
				{
					parsingErrorGlyphIndex = i;
					break;
				}
				
				if(!fc_text_parse_character(&stream, &asciiChar))
				{
					parsingErrorGlyphIndex = i;
					break;
				}
				
				float uv[2] = {0};
				
				if(!fc_text_parse_float(&stream, &uv[0]))
				{
					parsingErrorGlyphIndex = i;
					break;
				}
				
				if(!fc_text_parse_float(&stream, &uv[1]))
				{
					parsingErrorGlyphIndex = i;
					break;
				}
				
				int32_t pixelWidth = 0;
				if(!fc_text_parse_int32(&stream, &pixelWidth))
				{
					parsingErrorGlyphIndex = i;
					break;
				}
				
				font->glyphs[i].character = (char)asciiNumber;
				font->glyphs[i].uvMin[0] = uv[0];
				font->glyphs[i].uvMin[1] = uv[1];
				font->glyphs[i].uvMax[0] = uv[0] + ((float)pixelWidth) / ((float)font->atlasWidth);
				font->glyphs[i].uvMax[1] = 1.0f;
				font->glyphs[i].size[0] = pixelWidth;
				font->glyphs[i].size[1] = font->atlasHeight;
			}
			
			FUR_ASSERT(parsingErrorGlyphIndex == -1);
			font->offsetGlyphs = font->glyphs[0].character;	// keep the offset in ASCII to first character for later get_glyph
		}
		
		fc_release_text_buffer(&textBuffer, pAllocCallbacks);
	}
	
	return res;
}

const fr_font_glyph_t* fr_font_get_glyph(const fr_font_t* font, char chr)
{
	const uint32_t offsetGlyphs = font->offsetGlyphs;
	if(chr < offsetGlyphs)
		return NULL;
	
	const uint32_t idxGlyph = (uint32_t)chr - offsetGlyphs;
	if(idxGlyph >= font->numGlyphs)
		return NULL;
	
	return &font->glyphs[idxGlyph];
}

uint32_t fr_font_fill_vertex_buffer(const fr_font_t* font, const char* text, const float textPos[2], const float textColor[3], float* vertices, uint32_t numMaxVertices, float scale)
{
	FUR_ASSERT(FR_FONT_FLOATS_PER_GLYPH_VERTEX == 7);
	
	const uint32_t length = (uint32_t)strlen(text);
	const uint32_t vertexStride = FR_FONT_FLOATS_PER_GLYPH_VERTEX;
	const uint32_t numVerticesPerGlyph = 6;
	const uint32_t numVerticesRequired = length * numVerticesPerGlyph * vertexStride;
	
	if(numVerticesRequired > numMaxVertices)
		return 0;
	
	uint32_t verticesUsed = 0;
	
	float cursor[2] = {textPos[0], textPos[1]};
	for(uint32_t i=0; i<length; ++i)
	{
		const fr_font_glyph_t* glyph = fr_font_get_glyph(font, text[i]);
		
		if(glyph->size[0] == 0)
		{
			cursor[0] += 12.0f * scale;	// move cursor by X pixels for space character
		}
		
		verticesUsed += numVerticesPerGlyph * FR_FONT_FLOATS_PER_GLYPH_VERTEX;
		
		const float glyphWidth = (float)glyph->size[0];
		const float glyphWidthPlusOne = (float)glyph->size[0] + 1;
		const float glyphHeight = (float)glyph->size[1];
		
		const float scaledGlyphWidth = glyphWidth * scale;
		const float scaledGlyphWidthPlusOne = glyphWidthPlusOne * scale;
		const float scaledGlyphHeight = glyphHeight * scale;
		
		//  / Z
		// o----X
		// |
		// |
		// Y
		
		// top-right CCW triangle
		// O---
		//  \  |
		//   \ |
		//    \|
		//
		{
			float* pos = vertices;
			float* uv = vertices + 2;
			float* color = vertices + 4;
			
			pos[0] = cursor[0];
			pos[1] = cursor[1];
			uv[0] = glyph->uvMin[0];
			uv[1] = 0.0f;
			color[0] = textColor[0];
			color[1] = textColor[1];
			color[2] = textColor[2];
		}
		
		vertices += vertexStride;
		
		//  ---
		//  \  |
		//   \ |
		//    \|
		//     O
		{
			float* pos = vertices;
			float* uv = vertices + 2;
			float* color = vertices + 4;
			
			pos[0] = cursor[0] + scaledGlyphWidth;
			pos[1] = cursor[1] - scaledGlyphHeight;
			uv[0] = glyph->uvMin[1];
			uv[1] = 1.0f;
			color[0] = textColor[0];
			color[1] = textColor[1];
			color[2] = textColor[2];
		}
		
		vertices += vertexStride;
		
		//  ---O
		//  \  |
		//   \ |
		//    \|
		//
		{
			float* pos = vertices;
			float* uv = vertices + 2;
			float* color = vertices + 4;
			
			pos[0] = cursor[0] + scaledGlyphWidth;
			pos[1] = cursor[1];
			uv[0] = glyph->uvMin[1];
			uv[1] = 0.0f;
			color[0] = textColor[0];
			color[1] = textColor[1];
			color[2] = textColor[2];
		}
		
		vertices += vertexStride;
		
		// bottom-left CCW triangle
		// O
		// |\
		// | \
		// |  \
		//  ---
		{
			// same as vertex 0
			float* pos = vertices;
			float* uv = vertices + 2;
			float* color = vertices + 4;
			
			pos[0] = cursor[0];
			pos[1] = cursor[1];
			uv[0] = glyph->uvMin[0];
			uv[1] = 0.0f;
			color[0] = textColor[0];
			color[1] = textColor[1];
			color[2] = textColor[2];
		}
		
		vertices += vertexStride;
		
		//
		// |\
		// | \
		// |  \
		// O---
		{
			float* pos = vertices;
			float* uv = vertices + 2;
			float* color = vertices + 4;
			
			pos[0] = cursor[0];
			pos[1] = cursor[1] - scaledGlyphHeight;
			uv[0] = glyph->uvMin[0];
			uv[1] = 1.0f;
			color[0] = textColor[0];
			color[1] = textColor[1];
			color[2] = textColor[2];
		}
		
		vertices += vertexStride;
		
		//
		// |\
		// | \
		// |  \
		//  ---O
		{
			// same as vertex 1
			float* pos = vertices;
			float* uv = vertices + 2;
			float* color = vertices + 4;
			
			pos[0] = cursor[0] + scaledGlyphWidth;
			pos[1] = cursor[1] - scaledGlyphHeight;
			uv[0] = glyph->uvMin[1];
			uv[1] = 1.0f;
			color[0] = textColor[0];
			color[1] = textColor[1];
			color[2] = textColor[2];
		}
		
		vertices += vertexStride;
		
		cursor[0] += scaledGlyphWidthPlusOne;
	}
	
	return verticesUsed;
}

/*************************************************************/

enum fr_result_t fr_create_shader_module(VkDevice device, const char* path, VkShaderModule* pShader, struct fc_alloc_callbacks_t* pAllocCallbacks)
{
	struct fc_binary_buffer_t buffer;
	memset(&buffer, 0, sizeof(struct fc_binary_buffer_t));
	
	if(fc_load_binary_file_into_binary_buffer(path, &buffer, pAllocCallbacks))
	{
		VkShaderModuleCreateInfo createInfo = {0};
		createInfo.sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
		createInfo.codeSize = buffer.size;
		createInfo.pCode = buffer.pData;
		
		VkResult res = vkCreateShaderModule(device, &createInfo, NULL, pShader);
		fc_release_binary_buffer(&buffer, pAllocCallbacks);
		
		if (res != VK_SUCCESS)
		{
			fur_set_last_error(path);
			return FR_RESULT_ERROR_SHADER_MODULE_CREATION;
		}
		
		return FR_RESULT_OK;
	}
	else
	{
		char txt[256];
		sprintf(txt, "Can't load file %s", path);
		fur_set_last_error(txt);
		return FR_RESULT_ERROR;
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
	fm_mat4 model;
	fm_mat4 view;
	fm_mat4 proj;
	fm_mat4 padding;	// when using offset for buffers, it has to be a multiple of 256
} fr_uniform_buffer_t;

typedef struct fr_skinning_buffer_t
{
	fm_mat4 bones[512];
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
#define NUM_MAX_MESH_UNIFORM_BUFFERS 40
#define NUM_MAX_PROXIES_ALLOCATED 256

/*************************************************************/

#define NUM_SWAP_CHAIN_IMAGES 3

typedef struct fr_skinning_mapping_t
{
	uint32_t* indicesMapping;
	uint32_t count;
} fr_skinning_mapping_t;

typedef struct fr_renderer_t
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
	VkShaderModule vertexShaderNoSkinModule;
	VkShaderModule fragmentShaderModule;
	
	VkRenderPass renderPass;
	VkDescriptorSetLayout descriptorSetLayout;	// for uniform buffer
	VkPipelineLayout pipelineLayout;
	VkPipelineLayout textPipelineLayout;
	VkPipeline graphicsPipeline;
	VkPipeline graphicsPipelineNoSkin;
	
	VkFramebuffer aSwapChainFrameBuffers[NUM_SWAP_CHAIN_IMAGES];
	
	VkCommandPool commandPool;
	
	VkCommandPool stagingCommandPool;
	VkCommandBuffer stagingCommandBuffer;
	
	VkSemaphore imageAvailableSemaphore;
	VkSemaphore renderFinishedSemaphore;
	
	// debug draw
	VkPipeline debugLinesPSO;
	VkPipeline debugTrianglesPSO;
	VkPipeline debugRectsPSO;
	VkPipeline debugTextPSO;
	
	VkShaderModule debugVertexShaderModule;
	VkShaderModule debugFragmentShaderModule;
	
	VkShaderModule textVertexShaderModule;
	VkShaderModule textFragmentShaderModule;
	
	VkVertexInputBindingDescription debugLinesVertexBindingDescription;
	VkVertexInputAttributeDescription debugLinesVertexAttributes[2];
	
	VkVertexInputBindingDescription debugTextVertexBindingDescription;
	VkVertexInputAttributeDescription debugTextVertexAttributes[3];
	
	// debug 3D lines and triangles
	fr_buffer_t debugLinesVertexBuffer[NUM_SWAP_CHAIN_IMAGES];
	fr_buffer_t debugTrianglesVertexBuffer[NUM_SWAP_CHAIN_IMAGES];
	
	// debug 2D rects
	VkDescriptorSetLayout rectDescriptorSetLayout;	// for uniform buffer
	VkDescriptorSet aRectDescriptorSets[NUM_SWAP_CHAIN_IMAGES];
	VkVertexInputBindingDescription debugRectVertexBindingDescription;
	VkVertexInputAttributeDescription debugRectVertexAttributes[2];
	VkPipelineLayout rectsPipelineLayout;
	VkShaderModule rectVertexShaderModule;
	VkShaderModule rectFragmentShaderModule;
	fr_buffer_t aRectsVertexBuffer[NUM_SWAP_CHAIN_IMAGES];
	fr_buffer_t aRectsUniformBuffer[NUM_SWAP_CHAIN_IMAGES];
	
	// debug 2D text
	fr_buffer_t textVertexBuffer[NUM_SWAP_CHAIN_IMAGES];
	fr_font_t textFont;
	VkDescriptorSetLayout textDescriptorSetLayout;	// for uniform buffer
	VkDescriptorSet aTextDescriptorSets[NUM_SWAP_CHAIN_IMAGES];
	fr_buffer_t aTextUniformBuffer[NUM_SWAP_CHAIN_IMAGES];
	
	// test geometry
	VkVertexInputBindingDescription bindingDescription[2];
	VkVertexInputAttributeDescription vertexAttributes[5];
	
	// Zelda mesh
	fr_skinning_mapping_t skinningMapping;
	
	// texture samplers
	VkSampler textureSampler;
	VkSampler textTextureSampler;
	
	fr_buffer_t stagingBuffer;
	
	VkDescriptorPool descriptorPool;
	
	VkDescriptorPool pvsDescriptorPool;
	fr_pvs_t pvs[NUM_SWAP_CHAIN_IMAGES];
	
	// all proxies
	fr_proxy_t proxies[NUM_MAX_PROXIES_ALLOCATED];
	uint32_t numProxies;
} fr_renderer_t;

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
	struct fr_renderer_t* pRenderer = FUR_ALLOC(sizeof(struct fr_renderer_t), 8, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
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
		
		VkApplicationInfo appInfo = {0};
		appInfo.sType = VK_STRUCTURE_TYPE_APPLICATION_INFO;
		appInfo.pApplicationName = pDesc->pApp->title;
		appInfo.applicationVersion = VK_MAKE_VERSION(1, 0, 0);
		appInfo.pEngineName = "Furball Cat Game Engine";
		appInfo.engineVersion = VK_MAKE_VERSION(1, 0, 0);
		appInfo.apiVersion = VK_API_VERSION_1_0;
		
		VkInstanceCreateInfo createInfo = {0};
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
		
		VkPhysicalDevice* devices = FUR_ALLOC(numDevices * sizeof(VkPhysicalDevice), 8, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
		
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
		
		VkExtensionProperties* extensions = FUR_ALLOC(numExtensions * sizeof(struct VkExtensionProperties), 8, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
		
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
		
		VkQueueFamilyProperties* queueFamilies = FUR_ALLOC(numQueueFamilies * sizeof(struct VkQueueFamilyProperties), 8, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
		
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
		
		VkDeviceQueueCreateInfo queueCreateInfo[2] = {0};
		queueCreateInfo[0].sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
		queueCreateInfo[0].queueFamilyIndex = idxQueueGraphics;
		queueCreateInfo[0].queueCount = 1;
		queueCreateInfo[0].pQueuePriorities = &queuePriority;
		
		queueCreateInfo[1].sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
		queueCreateInfo[1].queueFamilyIndex = idxQueuePresent;
		queueCreateInfo[1].queueCount = 1;
		queueCreateInfo[1].pQueuePriorities = &queuePriority;
		
		VkPhysicalDeviceFeatures deviceFeatures = {0};
		
		VkDeviceCreateInfo createInfo = {0};
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
		
		fc_dbg_screen_info_t dbgScreenInfo = { 0 };
		dbgScreenInfo.width = (float)extent.width;
		dbgScreenInfo.height = (float)extent.height;

		fc_dbg_set_screen_info(&dbgScreenInfo);

		const VkFormat surfaceFormat = VK_FORMAT_B8G8R8A8_UNORM;
		
		VkSwapchainCreateInfoKHR createInfo = {0};
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
			VkImageViewCreateInfo createInfo = {0};
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
#ifdef PLATFORM_WINDOWS
	const char* basicVertexShaderPath = "../../../shaders/compiled/basic_vs.spv";
	const char* basicVertexShaderNoSkinPath = "../../../shaders/compiled/basic_vs_no_skin.spv";
	const char* basicFragmentShaderPath = "../../../shaders/compiled/basic_fs.spv";
	const char* debugVertexShaderPath = "../../../shaders/compiled/debug_vs.spv";
	const char* debugFragmentShaderPath = "../../../shaders/compiled/debug_fs.spv";
	const char* textVertexShaderPath = "../../../shaders/compiled/text_vs.spv";
	const char* textFragmentShaderPath = "../../../shaders/compiled/text_fs.spv";
	const char* rectVertexShaderPath = "../../../shaders/compiled/rect_vs.spv";
	const char* rectFragmentShaderPath = "../../../shaders/compiled/rect_fs.spv";
#elif PLATFORM_MAC
	const char* basicVertexShaderPath = "../../../../../shaders/compiled/basic_vs.spv";
	const char* basicVertexShaderNoSkinPath = "../../../../../shaders/compiled/basic_vs_no_skin.spv";
	const char* basicFragmentShaderPath = "../../../../../shaders/compiled/basic_fs.spv";
	const char* debugVertexShaderPath = "../../../../../shaders/compiled/debug_vs.spv";
	const char* debugFragmentShaderPath = "../../../../../shaders/compiled/debug_fs.spv";
	const char* textVertexShaderPath = "../../../../../shaders/compiled/text_vs.spv";
	const char* textFragmentShaderPath = "../../../../../shaders/compiled/text_fs.spv";
	const char* rectVertexShaderPath = "../../../../../shaders/compiled/rect_vs.spv";
	const char* rectFragmentShaderPath = "../../../../../shaders/compiled/rect_fs.spv";
#endif
	
	if(res == FR_RESULT_OK)
	{
		res = fr_create_shader_module(pRenderer->device, basicVertexShaderPath, &pRenderer->vertexShaderModule, pAllocCallbacks);
	}
	
	if(res == FR_RESULT_OK)
	{
		res = fr_create_shader_module(pRenderer->device, basicVertexShaderNoSkinPath, &pRenderer->vertexShaderNoSkinModule, pAllocCallbacks);
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
	
	if(res == FR_RESULT_OK)
	{
		res = fr_create_shader_module(pRenderer->device, rectVertexShaderPath, &pRenderer->rectVertexShaderModule, pAllocCallbacks);
	}
	
	if(res == FR_RESULT_OK)
	{
		res = fr_create_shader_module(pRenderer->device, rectFragmentShaderPath, &pRenderer->rectFragmentShaderModule, pAllocCallbacks);
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
	
	// debug 2D rect draw bindings
	if(res == FR_RESULT_OK)
	{
		pRenderer->debugRectVertexBindingDescription.binding = 0;
		pRenderer->debugRectVertexBindingDescription.stride = fc_dbg_rect_num_floats_per_vertex() * sizeof(float);
		pRenderer->debugRectVertexBindingDescription.inputRate = VK_VERTEX_INPUT_RATE_VERTEX;
		
		pRenderer->debugRectVertexAttributes[0].binding = 0;
		pRenderer->debugRectVertexAttributes[0].location = 0;
		pRenderer->debugRectVertexAttributes[0].format = VK_FORMAT_R32G32_SFLOAT;
		pRenderer->debugRectVertexAttributes[0].offset = 0;
		
		pRenderer->debugRectVertexAttributes[1].binding = 0;
		pRenderer->debugRectVertexAttributes[1].location = 1;
		pRenderer->debugRectVertexAttributes[1].format = VK_FORMAT_R32G32B32A32_SFLOAT;
		pRenderer->debugRectVertexAttributes[1].offset = 2 * sizeof(float);
	}
	
	// debug text draw bindings
	if(res == FR_RESULT_OK)
	{
		pRenderer->debugTextVertexBindingDescription.binding = 0;
		pRenderer->debugTextVertexBindingDescription.stride = FR_FONT_FLOATS_PER_GLYPH_VERTEX * sizeof(float);
		pRenderer->debugTextVertexBindingDescription.inputRate = VK_VERTEX_INPUT_RATE_VERTEX;
		
		pRenderer->debugTextVertexAttributes[0].binding = 0;
		pRenderer->debugTextVertexAttributes[0].location = 0;
		pRenderer->debugTextVertexAttributes[0].format = VK_FORMAT_R32G32_SFLOAT;
		pRenderer->debugTextVertexAttributes[0].offset = 0;
		
		pRenderer->debugTextVertexAttributes[1].binding = 0;
		pRenderer->debugTextVertexAttributes[1].location = 1;
		pRenderer->debugTextVertexAttributes[1].format = VK_FORMAT_R32G32_SFLOAT;
		pRenderer->debugTextVertexAttributes[1].offset = 2 * sizeof(float);
		
		pRenderer->debugTextVertexAttributes[2].binding = 0;
		pRenderer->debugTextVertexAttributes[2].location = 2;
		pRenderer->debugTextVertexAttributes[2].format = VK_FORMAT_R32G32B32_SFLOAT;
		pRenderer->debugTextVertexAttributes[2].offset = 4 * sizeof(float);
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
		VkDescriptorSetLayoutBinding uboLayoutBinding = {0};
		uboLayoutBinding.binding = 0;
		uboLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
		uboLayoutBinding.descriptorCount = 1;
		uboLayoutBinding.stageFlags = VK_SHADER_STAGE_VERTEX_BIT;
		uboLayoutBinding.pImmutableSamplers = NULL;
		
		// uniform buffer (UBO)
		VkDescriptorSetLayoutBinding skinLayoutBinding = {0};
		skinLayoutBinding.binding = 1;
		skinLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
		skinLayoutBinding.descriptorCount = 1;
		skinLayoutBinding.stageFlags = VK_SHADER_STAGE_VERTEX_BIT;
		skinLayoutBinding.pImmutableSamplers = NULL;
		
		// sampler
		VkDescriptorSetLayoutBinding samplerLayoutBinding = {0};
		samplerLayoutBinding.binding = 2;
		samplerLayoutBinding.descriptorCount = NUM_TEXTURES_IN_ARRAY;
		samplerLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
		samplerLayoutBinding.pImmutableSamplers = NULL;
		samplerLayoutBinding.stageFlags = VK_SHADER_STAGE_FRAGMENT_BIT;
		
		VkDescriptorSetLayoutBinding bindings[3] = { uboLayoutBinding, skinLayoutBinding, samplerLayoutBinding };
		const uint32_t numBindings = 3;
		
		VkDescriptorSetLayoutCreateInfo layoutInfo = {0};
		layoutInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
		layoutInfo.bindingCount = numBindings;
		layoutInfo.pBindings = bindings;
		
		if (vkCreateDescriptorSetLayout(pRenderer->device, &layoutInfo, NULL, &pRenderer->descriptorSetLayout) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create uniform buffer descriptor layout");
			res = FR_RESULT_ERROR_GPU;
		}
	}
	
	// create descriptor set layout for 2D rect drawing
	if(res == FR_RESULT_OK)
	{
		// uniform buffer (UBO)
		VkDescriptorSetLayoutBinding uboLayoutBinding = {0};
		uboLayoutBinding.binding = 0;
		uboLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
		uboLayoutBinding.descriptorCount = 1;
		uboLayoutBinding.stageFlags = VK_SHADER_STAGE_VERTEX_BIT;
		uboLayoutBinding.pImmutableSamplers = NULL;
		
		VkDescriptorSetLayoutBinding bindings[1] = { uboLayoutBinding };
		const uint32_t numBindings = 1;
		
		VkDescriptorSetLayoutCreateInfo layoutInfo = {0};
		layoutInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
		layoutInfo.bindingCount = numBindings;
		layoutInfo.pBindings = bindings;
		
		if (vkCreateDescriptorSetLayout(pRenderer->device, &layoutInfo, NULL, &pRenderer->rectDescriptorSetLayout) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create descriptor layout for 2D rect drawing");
			res = FR_RESULT_ERROR_GPU;
		}
	}
	
	// create descriptor set layout for text drawing
	if(res == FR_RESULT_OK)
	{
		// uniform buffer (UBO)
		VkDescriptorSetLayoutBinding uboLayoutBinding = {0};
		uboLayoutBinding.binding = 0;
		uboLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
		uboLayoutBinding.descriptorCount = 1;
		uboLayoutBinding.stageFlags = VK_SHADER_STAGE_VERTEX_BIT;
		uboLayoutBinding.pImmutableSamplers = NULL;
		
		// sampler
		VkDescriptorSetLayoutBinding samplerLayoutBinding = {0};
		samplerLayoutBinding.binding = 1;
		samplerLayoutBinding.descriptorCount = 1;
		samplerLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
		samplerLayoutBinding.pImmutableSamplers = NULL;
		samplerLayoutBinding.stageFlags = VK_SHADER_STAGE_FRAGMENT_BIT;
		
		VkDescriptorSetLayoutBinding bindings[2] = { uboLayoutBinding, samplerLayoutBinding };
		const uint32_t numBindings = 2;
		
		VkDescriptorSetLayoutCreateInfo layoutInfo = {0};
		layoutInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
		layoutInfo.bindingCount = numBindings;
		layoutInfo.pBindings = bindings;
		
		if (vkCreateDescriptorSetLayout(pRenderer->device, &layoutInfo, NULL, &pRenderer->textDescriptorSetLayout) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create descriptor layout for text drawing");
			res = FR_RESULT_ERROR_GPU;
		}
	}
	
	// create 2D rects uniform buffer
	if(res == FR_RESULT_OK)
	{
		fr_buffer_desc_t desc;
		desc.size = sizeof(fr_uniform_buffer_t);
		desc.usage = VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT;
		desc.properties = VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT;
		
		for(uint32_t i=0; i<3; ++i)
		{
			fr_buffer_create(pRenderer->device, pRenderer->physicalDevice, &desc, &pRenderer->aRectsUniformBuffer[i], pAllocCallbacks);
		}
	}
	
	// create text uniform buffer
	if(res == FR_RESULT_OK)
	{
		fr_buffer_desc_t desc;
		desc.size = sizeof(fr_uniform_buffer_t);
		desc.usage = VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT;
		desc.properties = VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT;
		
		for(uint32_t i=0; i<3; ++i)
		{
			fr_buffer_create(pRenderer->device, pRenderer->physicalDevice, &desc, &pRenderer->aTextUniformBuffer[i], pAllocCallbacks);
		}
	}
	
	const VkFormat depthFormat = VK_FORMAT_D24_UNORM_S8_UINT;
	
	// create render stages
	if(res == FR_RESULT_OK)
	{
		VkPipelineLayoutCreateInfo pipelineLayoutInfo = {0};
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
		VkPipelineShaderStageCreateInfo shaderStages[2] = {0};
		
		fr_pso_init_shader_stages_simple(pRenderer->vertexShaderModule, "main",
										 pRenderer->fragmentShaderModule, "main",
										 shaderStages);
		
		VkPipelineVertexInputStateCreateInfo vertexInputInfo = {0};
		vertexInputInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
		vertexInputInfo.vertexBindingDescriptionCount = 2;
		vertexInputInfo.vertexAttributeDescriptionCount = 5;
		vertexInputInfo.pVertexBindingDescriptions = pRenderer->bindingDescription;
		vertexInputInfo.pVertexAttributeDescriptions = pRenderer->vertexAttributes;
		
		// create rasterizer state
		VkPipelineRasterizationStateCreateInfo rasterizer = {0};
		fr_pso_init_rasterization_state_polygon_fill(&rasterizer);
		
		// create input assembly state
		VkPipelineInputAssemblyStateCreateInfo inputAssembly = {0};
		fr_pso_init_input_assembly_state_triangle_list(&inputAssembly);
		
		// create viewport
		VkViewport viewport = {0};
		fr_pso_init_viewport((float)pRenderer->swapChainExtent.width,
							 (float)pRenderer->swapChainExtent.height,
							 &viewport);
		
		VkRect2D scissor = {0};
		fr_pso_init_scissor(pRenderer->swapChainExtent, &scissor);
		
		VkPipelineViewportStateCreateInfo viewportState = {0};
		fr_pso_init_viewport_state(&viewport, &scissor, &viewportState);
		
		// create multi sampling state
		VkPipelineMultisampleStateCreateInfo multisampling = {0};
		fr_pso_init_multisampling_state(&multisampling);
		
		// depth and stencil state
		VkPipelineColorBlendAttachmentState colorBlendAttachment = {0};
		fr_pso_init_color_blend_attachment_state(&colorBlendAttachment);
		
		// for blending use fr_pso_init_color_blend_attachment_state_blending
		
		VkPipelineColorBlendStateCreateInfo colorBlending = {0};
		fr_pso_init_color_blend_state(&colorBlendAttachment, &colorBlending);
		
		VkPipelineDepthStencilStateCreateInfo depthStencil = {0};
		fr_pso_init_depth_stencil_state(&depthStencil);
		
		// create graphics pipeline
		VkGraphicsPipelineCreateInfo pipelineInfo = {0};
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
		
		// create no skin pipeline
		fr_pso_init_shader_stages_simple(pRenderer->vertexShaderNoSkinModule, "main",
										 pRenderer->fragmentShaderModule, "main",
										 shaderStages);
		
		if (vkCreateGraphicsPipelines(pRenderer->device, VK_NULL_HANDLE, 1, &pipelineInfo, NULL, &pRenderer->graphicsPipelineNoSkin) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create graphics pipeline 'no skin'");
			res = FR_RESULT_ERROR_GPU;
		}
	}
	
	// create debug draw PSO
	if(res == FR_RESULT_OK)
	{
		// create pipeline state object (PSO)
		VkPipelineShaderStageCreateInfo shaderStages[2] = {0};
		
		fr_pso_init_shader_stages_simple(pRenderer->debugVertexShaderModule, "main",
										 pRenderer->debugFragmentShaderModule, "main",
										 shaderStages);
		
		VkPipelineVertexInputStateCreateInfo vertexInputInfo = {0};
		vertexInputInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
		vertexInputInfo.vertexBindingDescriptionCount = 1;
		vertexInputInfo.vertexAttributeDescriptionCount = 2;
		vertexInputInfo.pVertexBindingDescriptions = &pRenderer->debugLinesVertexBindingDescription;
		vertexInputInfo.pVertexAttributeDescriptions = pRenderer->debugLinesVertexAttributes;
		
		// create rasterizer state
		VkPipelineRasterizationStateCreateInfo rasterizer = {0};
		fr_pso_init_rasterization_state_wireframe_no_cull(&rasterizer);
		
		// create input assembly state
		VkPipelineInputAssemblyStateCreateInfo inputAssembly = {0};
		fr_pso_init_input_assembly_state_line_list(&inputAssembly);
		
		// create viewport
		VkViewport viewport = {0};
		fr_pso_init_viewport((float)pRenderer->swapChainExtent.width,
							 (float)pRenderer->swapChainExtent.height,
							 &viewport);
		
		VkRect2D scissor = {0};
		fr_pso_init_scissor(pRenderer->swapChainExtent, &scissor);
		
		VkPipelineViewportStateCreateInfo viewportState = {0};
		fr_pso_init_viewport_state(&viewport, &scissor, &viewportState);
		
		// create multi sampling state
		VkPipelineMultisampleStateCreateInfo multisampling = {0};
		fr_pso_init_multisampling_state(&multisampling);
		
		// depth and stencil state - for blending use fr_pso_init_color_blend_attachment_state_blending
		VkPipelineColorBlendAttachmentState colorBlendAttachment = {0};
		fr_pso_init_color_blend_attachment_state(&colorBlendAttachment);
		
		VkPipelineColorBlendStateCreateInfo colorBlending = {0};
		fr_pso_init_color_blend_state(&colorBlendAttachment, &colorBlending);
		
		VkPipelineDepthStencilStateCreateInfo depthStencil = {0};
		fr_pso_init_depth_stencil_state(&depthStencil);
		
		VkPipelineDepthStencilStateCreateInfo depthStencilNoDepth = {0};
		fr_pso_init_depth_stencil_state_no_depth_test(&depthStencilNoDepth);
		
		// create graphics pipeline
		VkGraphicsPipelineCreateInfo pipelineInfo = {0};
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
		
		// create 3D lines debug PSO
		if (vkCreateGraphicsPipelines(pRenderer->device, VK_NULL_HANDLE, 1, &pipelineInfo, NULL, &pRenderer->debugLinesPSO) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create debug lines PSO");
			res = FR_RESULT_ERROR_GPU;
		}
		
		// create 3D triangles debug PSO
		fr_pso_init_rasterization_state_polygon_fill(&rasterizer);
		fr_pso_init_input_assembly_state_triangle_list(&inputAssembly);
		if (vkCreateGraphicsPipelines(pRenderer->device, VK_NULL_HANDLE, 1, &pipelineInfo, NULL, &pRenderer->debugTrianglesPSO) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create debug triangles PSO");
			res = FR_RESULT_ERROR_GPU;
		}
		
		// depth and stencil state - for blending use fr_pso_init_color_blend_attachment_state_blending
		VkPipelineColorBlendAttachmentState colorBlendAttachmentBlending = {0};
		fr_pso_init_color_blend_attachment_state_blending(&colorBlendAttachmentBlending);
		
		VkPipelineColorBlendStateCreateInfo colorBlendingDoBlending = {0};
		fr_pso_init_color_blend_state(&colorBlendAttachmentBlending, &colorBlendingDoBlending);
		
		pipelineInfo.pColorBlendState = &colorBlendingDoBlending;
		
		// override depth stencil to no depth test
		pipelineInfo.pDepthStencilState = &depthStencilNoDepth;
		
		// create 2D rects debug PSO
		{
			VkPipelineLayoutCreateInfo pipelineLayoutInfo = {0};
			fr_pso_init_layout(&pRenderer->rectDescriptorSetLayout, &pipelineLayoutInfo);
			
			if (vkCreatePipelineLayout(pRenderer->device, &pipelineLayoutInfo, NULL, &pRenderer->rectsPipelineLayout) != VK_SUCCESS)
			{
				fur_set_last_error("Can't create rects pipeline layout");
				res = FR_RESULT_ERROR_GPU;
			}
			
			pipelineInfo.layout = pRenderer->rectsPipelineLayout;
			
			fr_pso_init_rasterization_state_polygon_fill(&rasterizer);
			fr_pso_init_input_assembly_state_triangle_list(&inputAssembly);
			
			fr_pso_init_shader_stages_simple(pRenderer->rectVertexShaderModule, "main",
											 pRenderer->rectFragmentShaderModule, "main",
											 shaderStages);
			
			pipelineInfo.stageCount = 2;
			pipelineInfo.pStages = shaderStages;
			
			vertexInputInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
			vertexInputInfo.vertexBindingDescriptionCount = 1;
			vertexInputInfo.vertexAttributeDescriptionCount = 2;
			vertexInputInfo.pVertexBindingDescriptions = &pRenderer->debugRectVertexBindingDescription;
			vertexInputInfo.pVertexAttributeDescriptions = pRenderer->debugRectVertexAttributes;
			
			if (vkCreateGraphicsPipelines(pRenderer->device, VK_NULL_HANDLE, 1, &pipelineInfo, NULL, &pRenderer->debugRectsPSO) != VK_SUCCESS)
			{
				fur_set_last_error("Can't create debug rects PSO");
				res = FR_RESULT_ERROR_GPU;
			}
		}
		
		// create 2D text debug PSO
		{
			VkPipelineLayoutCreateInfo pipelineLayoutInfo = {0};
			fr_pso_init_layout(&pRenderer->textDescriptorSetLayout, &pipelineLayoutInfo);
			
			if (vkCreatePipelineLayout(pRenderer->device, &pipelineLayoutInfo, NULL, &pRenderer->textPipelineLayout) != VK_SUCCESS)
			{
				fur_set_last_error("Can't create text pipeline layout");
				res = FR_RESULT_ERROR_GPU;
			}
			
			VkPipelineDepthStencilStateCreateInfo depthStencil = {0};
			fr_pso_init_depth_stencil_state_no_depth_test(&depthStencil);
			
			pipelineInfo.layout = pRenderer->textPipelineLayout;
			
			fr_pso_init_rasterization_state_polygon_fill(&rasterizer);
			fr_pso_init_input_assembly_state_triangle_list(&inputAssembly);
			
			fr_pso_init_shader_stages_simple(pRenderer->textVertexShaderModule, "main",
											 pRenderer->textFragmentShaderModule, "main",
											 shaderStages);
			
			pipelineInfo.stageCount = 2;
			pipelineInfo.pStages = shaderStages;
			
			vertexInputInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
			vertexInputInfo.vertexBindingDescriptionCount = 1;
			vertexInputInfo.vertexAttributeDescriptionCount = 3;
			vertexInputInfo.pVertexBindingDescriptions = &pRenderer->debugTextVertexBindingDescription;
			vertexInputInfo.pVertexAttributeDescriptions = pRenderer->debugTextVertexAttributes;
			
			if (vkCreateGraphicsPipelines(pRenderer->device, VK_NULL_HANDLE, 1, &pipelineInfo, NULL, &pRenderer->debugTextPSO) != VK_SUCCESS)
			{
				fur_set_last_error("Can't create debug text PSO");
				res = FR_RESULT_ERROR_GPU;
			}
		}
	}
	
	// load debug font
	if(res == FR_RESULT_OK)
	{
		fr_font_desc_t desc = {0};
		desc.atlasPath = "../../../data/font/debug-font-2.png";
		desc.glyphsInfoPath = "../../../data/font/debug-font-data-2.txt";
		
		res = fr_font_create(pRenderer->device, pRenderer->physicalDevice, &desc, &pRenderer->textFont, pAllocCallbacks);
	}
	
	// create depth buffer
	if(res == FR_RESULT_OK)
	{
		const VkDeviceSize depthImageSize = 4 * pRenderer->swapChainExtent.width * pRenderer->swapChainExtent.height;
		
		fr_image_desc_t desc = {0};
		desc.size = depthImageSize;
		desc.width = pRenderer->swapChainExtent.width;
		desc.height = pRenderer->swapChainExtent.height;
		desc.format = depthFormat;
		desc.usage = VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT;
		desc.properties = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
		
		fr_image_create(pRenderer->device, pRenderer->physicalDevice, &desc, &pRenderer->depthImage, pAllocCallbacks);
	}
	
	// create debug lines vertex buffer
	if(res == FR_RESULT_OK)
	{
		fr_buffer_desc_t desc = {0};
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
		fr_buffer_desc_t desc = {0};
		desc.size = fc_dbg_triangle_buffer_size();
		desc.usage = FR_VERTEX_BUFFER_USAGE_FLAGS;
		desc.properties = FR_STAGING_BUFFER_MEMORY_FLAGS;	// use vertex buffer usage, but staging buffer properties
		
		for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			fr_buffer_create(pRenderer->device, pRenderer->physicalDevice, &desc, &pRenderer->debugTrianglesVertexBuffer[i], pAllocCallbacks);
		}
	}
	
	// create debug 2D rects vertex buffer
	if(res == FR_RESULT_OK)
	{
		fr_buffer_desc_t desc = {0};
		desc.size = fc_dbg_rects_buffer_size();
		desc.usage = FR_VERTEX_BUFFER_USAGE_FLAGS;
		desc.properties = FR_STAGING_BUFFER_MEMORY_FLAGS;	// use vertex buffer usage, but staging buffer properties
		
		for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			fr_buffer_create(pRenderer->device, pRenderer->physicalDevice, &desc, &pRenderer->aRectsVertexBuffer[i], pAllocCallbacks);
		}
	}
	
	// create debug text vertex buffer
	if(res == FR_RESULT_OK)
	{
		fr_buffer_desc_t desc = {0};
		desc.size = fc_dbg_text_characters_capacity() * FR_FONT_FLOATS_PER_GLYPH_VERTEX * 6 * sizeof(float);
		desc.usage = FR_VERTEX_BUFFER_USAGE_FLAGS;
		desc.properties = FR_STAGING_BUFFER_MEMORY_FLAGS;	// use vertex buffer usage, but staging buffer properties
		
		for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			fr_buffer_create(pRenderer->device, pRenderer->physicalDevice, &desc, &pRenderer->textVertexBuffer[i], pAllocCallbacks);
		}
	}
	
	// create frame buffers
	if(res == FR_RESULT_OK)
	{
		for (size_t i = 0; i < NUM_SWAP_CHAIN_IMAGES; i++)
		{
			VkImageView attachments[2] =
			{
				pRenderer->aSwapChainImagesViews[i],
				pRenderer->depthImage.view
			};

			const uint32_t numAttachments = 2;
			
			VkFramebufferCreateInfo framebufferInfo = {0};
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
		VkDescriptorPoolSize poolSizes[2];
		uint32_t numBindings = 2;

		poolSizes[0].type = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
		poolSizes[0].descriptorCount = NUM_SWAP_CHAIN_IMAGES;
		
		poolSizes[1].type = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
		poolSizes[1].descriptorCount = NUM_SWAP_CHAIN_IMAGES;
		
		VkDescriptorPoolCreateInfo poolInfo = {0};
		poolInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
		poolInfo.poolSizeCount = numBindings;
		poolInfo.pPoolSizes = poolSizes;
		poolInfo.maxSets = NUM_SWAP_CHAIN_IMAGES * 4;	// * 4 because part of it is also for text drawing and also for prop descriptors and also for rects drawing
		
		if (vkCreateDescriptorPool(pRenderer->device, &poolInfo, NULL, &pRenderer->descriptorPool) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create descriptor pool for uniform buffers");
			res = FR_RESULT_ERROR_GPU;
		}
	}
	
	// prepare staging buffer builder
	fr_staging_buffer_builder_t stagingBuilder;
	fr_staging_init(&stagingBuilder);
	
	uint32_t numTexturesInStagingBuffer = 0;
	
	const VkFormat textureImageFormat = VK_FORMAT_R8G8B8A8_UNORM;
	
	VkDeviceSize imageOffsetInBufferFontAtlas = 0;
	int texFontAtlasWidth = 0;
	int texFontAtlasHeight = 0;
	
	// pass font atlas to staging buffer
	if(res == FR_RESULT_OK)
	{
		if(!pRenderer->textFont.pixelsData)
		{
			fur_set_last_error("Can't load font atlas");
			res = FR_RESULT_ERROR_GPU;
		}
		else
		{
			imageOffsetInBufferFontAtlas = stagingBuilder.totalSize;
			texFontAtlasWidth = pRenderer->textFont.atlasWidth;
			texFontAtlasHeight = pRenderer->textFont.atlasHeight;
			
			void* pixels = pRenderer->textFont.pixelsData;
			uint32_t imageSize = texFontAtlasWidth * texFontAtlasHeight * 4;
			
			fr_staging_add(&stagingBuilder, pixels, imageSize, NULL, fr_pixels_free_func);
			numTexturesInStagingBuffer++;
			pRenderer->textFont.pixelsData = NULL;	// the ownership has been passed to staging builder
		}
	}
	
	// create texture sampler
	if(res == FR_RESULT_OK)
	{
		VkSamplerCreateInfo samplerInfo = {0};
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
	
	// create text texture sampler
	if(res == FR_RESULT_OK)
	{
		VkSamplerCreateInfo samplerInfo = {0};
		samplerInfo.sType = VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;
		samplerInfo.magFilter = VK_FILTER_NEAREST;
		samplerInfo.minFilter = VK_FILTER_NEAREST;
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
		
		if (vkCreateSampler(pRenderer->device, &samplerInfo, NULL, &pRenderer->textTextureSampler) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create texture sampler");
			res = FR_RESULT_ERROR_GPU;
		}
	}
	
	// create descriptor sets for 2D rect drawing
	if(res == FR_RESULT_OK)
	{
		VkDescriptorSetLayout layouts[NUM_SWAP_CHAIN_IMAGES] = {pRenderer->rectDescriptorSetLayout,
			pRenderer->rectDescriptorSetLayout, pRenderer->rectDescriptorSetLayout};
		
		VkDescriptorSetAllocateInfo allocInfo = {0};
		allocInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
		allocInfo.descriptorPool = pRenderer->descriptorPool;
		allocInfo.descriptorSetCount = NUM_SWAP_CHAIN_IMAGES;
		allocInfo.pSetLayouts = layouts;
		
		if (vkAllocateDescriptorSets(pRenderer->device, &allocInfo, pRenderer->aRectDescriptorSets) != VK_SUCCESS)
		{
			fur_set_last_error("Can't allocate descriptor sets for 2D rect drawing uniform buffers");
			res = FR_RESULT_ERROR_GPU;
		}
		
		for (size_t i = 0; i < NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			VkDescriptorBufferInfo bufferInfo[1] = {0};
			bufferInfo[0].buffer = pRenderer->aRectsUniformBuffer[i].buffer;
			bufferInfo[0].offset = 0;
			bufferInfo[0].range = sizeof(fr_uniform_buffer_t);
			
			VkWriteDescriptorSet descriptorWrites[1] = {0};
			const uint32_t numBindings = 1;
			
			descriptorWrites[0].sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
			descriptorWrites[0].dstSet = pRenderer->aRectDescriptorSets[i];
			descriptorWrites[0].dstBinding = 0;
			descriptorWrites[0].dstArrayElement = 0;
			descriptorWrites[0].descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
			descriptorWrites[0].descriptorCount = 1;
			descriptorWrites[0].pBufferInfo = &bufferInfo[0];
			descriptorWrites[0].pImageInfo = NULL; // Optional
			descriptorWrites[0].pTexelBufferView = NULL; // Optional
			
			vkUpdateDescriptorSets(pRenderer->device, numBindings, descriptorWrites, 0, NULL);
		}
	}
	
	// create descriptor sets for text drawing
	if(res == FR_RESULT_OK)
	{
		VkDescriptorSetLayout layouts[NUM_SWAP_CHAIN_IMAGES] = {pRenderer->textDescriptorSetLayout,
			pRenderer->textDescriptorSetLayout, pRenderer->textDescriptorSetLayout};
		
		VkDescriptorSetAllocateInfo allocInfo = {0};
		allocInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
		allocInfo.descriptorPool = pRenderer->descriptorPool;
		allocInfo.descriptorSetCount = NUM_SWAP_CHAIN_IMAGES;
		allocInfo.pSetLayouts = layouts;
		
		if (vkAllocateDescriptorSets(pRenderer->device, &allocInfo, pRenderer->aTextDescriptorSets) != VK_SUCCESS)
		{
			fur_set_last_error("Can't allocate descriptor sets for uniform buffers");
			res = FR_RESULT_ERROR_GPU;
		}
		
		for (size_t i = 0; i < NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			VkDescriptorBufferInfo bufferInfo[2] = {0};
			bufferInfo[0].buffer = pRenderer->aTextUniformBuffer[i].buffer;
			bufferInfo[0].offset = 0;
			bufferInfo[0].range = sizeof(fr_uniform_buffer_t);
			
			VkDescriptorImageInfo imageInfo = {0};
			imageInfo.imageLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
			imageInfo.imageView = pRenderer->textFont.atlas.view;
			imageInfo.sampler = pRenderer->textTextureSampler;
			
			VkWriteDescriptorSet descriptorWrites[2] = {0};
			const uint32_t numBindings = 2;
			
			descriptorWrites[0].sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
			descriptorWrites[0].dstSet = pRenderer->aTextDescriptorSets[i];
			descriptorWrites[0].dstBinding = 0;
			descriptorWrites[0].dstArrayElement = 0;
			descriptorWrites[0].descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
			descriptorWrites[0].descriptorCount = 1;
			descriptorWrites[0].pBufferInfo = &bufferInfo[0];
			descriptorWrites[0].pImageInfo = NULL; // Optional
			descriptorWrites[0].pTexelBufferView = NULL; // Optional
			
			descriptorWrites[1].sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
			descriptorWrites[1].dstSet = pRenderer->aTextDescriptorSets[i];
			descriptorWrites[1].dstBinding = 1;
			descriptorWrites[1].dstArrayElement = 0;
			descriptorWrites[1].descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
			descriptorWrites[1].descriptorCount = 1;	// number of textures in array goes here
			descriptorWrites[1].pImageInfo = &imageInfo;
			
			vkUpdateDescriptorSets(pRenderer->device, numBindings, descriptorWrites, 0, NULL);
		}
	}
	
	// create descriptor pool for PVS (Potentially Visible Set)
	if(res == FR_RESULT_OK)
	{
		VkDescriptorPoolSize poolSizes[2] = {0};
		uint32_t numBindings = 2;

		poolSizes[0].type = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
		poolSizes[0].descriptorCount = NUM_SWAP_CHAIN_IMAGES;
		
		poolSizes[1].type = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
		poolSizes[1].descriptorCount = NUM_SWAP_CHAIN_IMAGES;
		
		VkDescriptorPoolCreateInfo poolInfo = {0};
		poolInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
		poolInfo.poolSizeCount = numBindings;
		poolInfo.pPoolSizes = poolSizes;
		poolInfo.maxSets = NUM_SWAP_CHAIN_IMAGES * NUM_MAX_MESH_UNIFORM_BUFFERS;
		
		if (vkCreateDescriptorPool(pRenderer->device, &poolInfo, NULL, &pRenderer->pvsDescriptorPool) != VK_SUCCESS)
		{
			FUR_ASSERT(false);	// can't create descriptor pool for PVS for some reason
		}
	}
	
	// prepare PVS
	if (res == FR_RESULT_OK)
	{
		for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			fr_pvs_t* pvs = &pRenderer->pvs[i];
			pvs->pvsIndex = i;
			pvs->device = pRenderer->device;
			pvs->defaultTextureSampler = pRenderer->textureSampler;
			pvs->numMaxDescriptorSets = NUM_MAX_MESH_UNIFORM_BUFFERS;
			pvs->descriptorSets = FUR_ALLOC_ARRAY_AND_ZERO(VkDescriptorSet, NUM_MAX_MESH_UNIFORM_BUFFERS, 0, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
			pvs->proxies = FUR_ALLOC_ARRAY_AND_ZERO(const fr_proxy_t*, NUM_MAX_MESH_UNIFORM_BUFFERS, 0, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
			pvs->proxiesFlags = FUR_ALLOC_ARRAY_AND_ZERO(uint32_t, NUM_MAX_MESH_UNIFORM_BUFFERS, 0, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
			pvs->skinningMatrices = FUR_ALLOC_ARRAY_AND_ZERO(fm_mat4, FUR_MAX_SKIN_MATRICES_IN_BUFFER, 0, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
			
			// allocate descriptor sets
			VkDescriptorSetLayout layouts[20] = {0};	// max layouts
			for(uint32_t i=0; i<NUM_MAX_MESH_UNIFORM_BUFFERS; ++i)
			{
				layouts[i] = pRenderer->descriptorSetLayout;
			}
			
			VkDescriptorSetAllocateInfo allocInfo = {0};
			allocInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
			allocInfo.descriptorPool = pRenderer->pvsDescriptorPool;
			allocInfo.descriptorSetCount = NUM_MAX_MESH_UNIFORM_BUFFERS;
			allocInfo.pSetLayouts = layouts;
			
			if (vkAllocateDescriptorSets(pRenderer->device, &allocInfo, pvs->descriptorSets) != VK_SUCCESS)
			{
				FUR_ASSERT(false); // can't allocate descriptor sets for some reason
			}
			
			// create uniform buffer for PVS
			if(res == FR_RESULT_OK)
			{
				fr_buffer_desc_t desc;
				desc.size = sizeof(fr_uniform_buffer_t) * NUM_MAX_MESH_UNIFORM_BUFFERS;
				desc.usage = VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT;
				desc.properties = VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT;
				
				for(uint32_t i=0; i<3; ++i)
				{
					fr_buffer_create(pRenderer->device, pRenderer->physicalDevice, &desc, &pvs->worldViewProj, pAllocCallbacks);
				}
			}
			
			// create skinning buffer for PVS
			if(res == FR_RESULT_OK)
			{
				fr_buffer_desc_t desc;
				desc.size = FUR_MAX_SKIN_MATRICES_IN_BUFFER * sizeof(fm_mat4);
				desc.usage = VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT;
				desc.properties = VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT;
				
				for(uint32_t i=0; i<3; ++i)
				{
					fr_buffer_create(pRenderer->device, pRenderer->physicalDevice, &desc, &pvs->skinningBuffer, pAllocCallbacks);
				}
			}
		}
	}
	
	// create command pool
	if(res == FR_RESULT_OK)
	{
		VkCommandPoolCreateInfo poolInfo = {0};
		poolInfo.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
		poolInfo.queueFamilyIndex = pRenderer->idxQueueGraphics;
		poolInfo.flags = 0; // Optional
		
		if (vkCreateCommandPool(pRenderer->device, &poolInfo, NULL, &pRenderer->commandPool) != VK_SUCCESS)
		{
			fur_set_last_error("Can't create graphics command pool");
			res = FR_RESULT_ERROR_GPU;
		}
	}
	
	// do staging pass - copy vertex data from staging buffer to vertex buffer
	if(res == FR_RESULT_OK)
	{
		// create staging buffer & release memory of source data
		{
			pRenderer->stagingBuffer.size = stagingBuilder.totalSize;
			fr_staging_build(&stagingBuilder, pRenderer->device, pRenderer->physicalDevice, &pRenderer->stagingBuffer.buffer, &pRenderer->stagingBuffer.memory, pAllocCallbacks);
			fr_staging_release_builder(&stagingBuilder);
		}
		
		// create staging command pool
		{
			VkCommandPoolCreateInfo poolInfo = {0};
			poolInfo.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
			poolInfo.queueFamilyIndex = pRenderer->idxQueueGraphics;
			poolInfo.flags = VK_COMMAND_POOL_CREATE_TRANSIENT_BIT; // Optional
			
			if (vkCreateCommandPool(pRenderer->device, &poolInfo, NULL, &pRenderer->stagingCommandPool) != VK_SUCCESS)
			{
				FUR_ASSERT(false);
			}
		}
		
		// depth layout transitions
		{
			fr_transition_image_layout(pRenderer->device, pRenderer->graphicsQueue, pRenderer->stagingCommandPool, depthFormat,
									   VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, pRenderer->depthImage.image, pAllocCallbacks);
		}
		
		// image layout transitions
		{
			// font atlas
			fr_transition_image_layout(pRenderer->device, pRenderer->graphicsQueue, pRenderer->stagingCommandPool, textureImageFormat,
									   VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, pRenderer->textFont.atlas.image, pAllocCallbacks);
			fr_copy_buffer_to_image(pRenderer->device, pRenderer->graphicsQueue, pRenderer->stagingCommandPool, pRenderer->stagingBuffer.buffer,
									imageOffsetInBufferFontAtlas, pRenderer->textFont.atlas.image, texFontAtlasWidth, texFontAtlasHeight, pAllocCallbacks);
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
	
	// clear debug fragments buffers with zeros
	if(res == FR_RESULT_OK)
	{
		for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			fr_clear_data_in_buffer(pRenderer->device, pRenderer->debugLinesVertexBuffer[i].memory, 0, fc_dbg_line_buffer_size());
		}
		
		for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			fr_clear_data_in_buffer(pRenderer->device, pRenderer->debugTrianglesVertexBuffer[i].memory, 0, fc_dbg_triangle_buffer_size());
		}
		
		for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			fr_clear_data_in_buffer(pRenderer->device, pRenderer->textVertexBuffer[i].memory, 0, fc_dbg_text_characters_capacity() * FR_FONT_FLOATS_PER_GLYPH_VERTEX * 6 * sizeof(float));
		}
		
		for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			fr_clear_data_in_buffer(pRenderer->device, pRenderer->aRectsVertexBuffer[i].memory, 0, fc_dbg_rects_buffer_size());
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
	
	// create skinning mapping
	if(res == FR_RESULT_OK)
	{
		
	}
	
	return res;
}

enum fr_result_t fr_release_renderer(struct fr_renderer_t* pRenderer,
					   struct fc_alloc_callbacks_t*	pAllocCallbacks)
{
	// release PVS
	{
		for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			fr_pvs_t* pvs = &pRenderer->pvs[i];
			fr_buffer_release(pRenderer->device, &pvs->worldViewProj, pAllocCallbacks);
			fr_buffer_release(pRenderer->device, &pvs->skinningBuffer, pAllocCallbacks);
			
			FUR_FREE(pvs->descriptorSets, pAllocCallbacks);
			FUR_FREE(pvs->proxies, pAllocCallbacks);
			FUR_FREE(pvs->proxiesFlags, pAllocCallbacks);
			FUR_FREE(pvs->skinningMatrices, pAllocCallbacks);
		}
		
		vkDestroyDescriptorPool(pRenderer->device, pRenderer->pvsDescriptorPool, NULL);
	}
	
	if(pRenderer->skinningMapping.indicesMapping)
	{
		FUR_FREE(pRenderer->skinningMapping.indicesMapping, pAllocCallbacks);
	}
	
	// this should be in clean-up swap chain
	{
		// destroy uniform buffer
		for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
		{
			fr_buffer_release(pRenderer->device, &pRenderer->aTextUniformBuffer[i], pAllocCallbacks);
			fr_buffer_release(pRenderer->device, &pRenderer->aRectsUniformBuffer[i], pAllocCallbacks);
		}
		
		vkDestroyDescriptorPool(pRenderer->device, pRenderer->descriptorPool, NULL);
	}
	
	// destroy depth image
	fr_image_release(pRenderer->device, &pRenderer->depthImage, pAllocCallbacks);
	
	// destroy image
	vkDestroySampler(pRenderer->device, pRenderer->textureSampler, NULL);
	vkDestroySampler(pRenderer->device, pRenderer->textTextureSampler, NULL);
	
	// destroy debug fragments vertex buffer
	for(uint32_t i=0; i<NUM_SWAP_CHAIN_IMAGES; ++i)
	{
		fr_buffer_release(pRenderer->device, &pRenderer->debugLinesVertexBuffer[i], pAllocCallbacks);
		fr_buffer_release(pRenderer->device, &pRenderer->debugTrianglesVertexBuffer[i], pAllocCallbacks);
		fr_buffer_release(pRenderer->device, &pRenderer->textVertexBuffer[i], pAllocCallbacks);
		fr_buffer_release(pRenderer->device, &pRenderer->aRectsVertexBuffer[i], pAllocCallbacks);
	}
	
	fr_font_release(pRenderer->device, &pRenderer->textFont, pAllocCallbacks);
	
	// destroy descriptor set layout
	vkDestroyDescriptorSetLayout(pRenderer->device, pRenderer->descriptorSetLayout, NULL);
	vkDestroyDescriptorSetLayout(pRenderer->device, pRenderer->textDescriptorSetLayout, NULL);
	vkDestroyDescriptorSetLayout(pRenderer->device, pRenderer->rectDescriptorSetLayout, NULL);
	
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
	vkDestroyPipeline(pRenderer->device, pRenderer->graphicsPipelineNoSkin, NULL);
	vkDestroyPipelineLayout(pRenderer->device, pRenderer->pipelineLayout, NULL);
	vkDestroyRenderPass(pRenderer->device, pRenderer->renderPass, NULL);
	
	// destroy debug PSO
	vkDestroyPipeline(pRenderer->device, pRenderer->debugLinesPSO, NULL);
	vkDestroyPipeline(pRenderer->device, pRenderer->debugTrianglesPSO, NULL);
	vkDestroyPipeline(pRenderer->device, pRenderer->debugTextPSO, NULL);
	
	// destroy shaders
	vkDestroyShaderModule(pRenderer->device, pRenderer->vertexShaderModule, NULL);
	vkDestroyShaderModule(pRenderer->device, pRenderer->vertexShaderNoSkinModule, NULL);
	vkDestroyShaderModule(pRenderer->device, pRenderer->fragmentShaderModule, NULL);
	
	vkDestroyShaderModule(pRenderer->device, pRenderer->debugVertexShaderModule, NULL);
	vkDestroyShaderModule(pRenderer->device, pRenderer->debugFragmentShaderModule, NULL);
	
	vkDestroyShaderModule(pRenderer->device, pRenderer->textVertexShaderModule, NULL);
	vkDestroyShaderModule(pRenderer->device, pRenderer->textFragmentShaderModule, NULL);
	
	vkDestroyShaderModule(pRenderer->device, pRenderer->rectVertexShaderModule, NULL);
	vkDestroyShaderModule(pRenderer->device, pRenderer->rectFragmentShaderModule, NULL);
	
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

void fr_dbg_draw_mat4(const fm_mat4* m)
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

fr_pvs_t* fr_acquire_free_pvs(fr_renderer_t* pRenderer, const fm_mat4* camera, float fov)
{
	uint32_t imageIndex;
	vkAcquireNextImageKHR(pRenderer->device, pRenderer->swapChain, (uint64_t)-1,
						  pRenderer->imageAvailableSemaphore, VK_NULL_HANDLE, &imageIndex);
	
	fr_pvs_t* pvs = &pRenderer->pvs[imageIndex];
	fr_pvs_clear(pvs);
	pvs->camera = *camera;
	
	const float aspectRatio = pRenderer->swapChainExtent.width / (float)pRenderer->swapChainExtent.height;
	
	fm_mat4 tempView = pvs->camera;
	
	fm_mat4_projection_fov(fov, aspectRatio, 0.1f, 1000.0f, &pvs->projection);
	
	fm_mat4 rot_x_vulkan_correction;
	fm_mat4_rot_x(FM_DEG_TO_RAD(-90), &rot_x_vulkan_correction);
	fm_mat4_mul(&rot_x_vulkan_correction, &tempView, &pvs->view);
	
	pvs->projection.y.y *= -1.0f;	// flipping from right-handed (Blender) to left-handed (Vulkan)?
	
	fm_mat4_transpose(&pvs->projection);
	fm_mat4_transpose(&pvs->view);
	
	return pvs;
}

void fr_draw_frame(struct fr_renderer_t* pRenderer, const fr_draw_frame_context_t* ctx, fc_alloc_callbacks_t* pAllocCallbacks)
{
	uint32_t imageIndex = 0;
	
	fr_pvs_t* pvs = ctx->pvs;
	FUR_ASSERT(pvs);
	imageIndex = pvs->pvsIndex;

	FUR_ASSERT(imageIndex < NUM_SWAP_CHAIN_IMAGES);
	
	FUR_PROFILE("rend-skinning")
	{
		if(pvs->numSkinningMatrices > 0)
		{	
			// pass skinning matrices to skin buffer
			fr_copy_data_to_buffer(pRenderer->device, pvs->skinningBuffer.memory, pvs->skinningMatrices, 0,
								   (uint32_t)pvs->numSkinningMatrices * sizeof(fm_mat4));
		}
	}
	
	// update uniform buffers for debug draw (3D and 2D)
	FUR_PROFILE("rend-update-ubs")
	{
		fr_uniform_buffer_t ubo;
		ubo.proj = pvs->projection;
		ubo.view = pvs->view;
		
		fm_mat4_transpose(&ubo.model);
		
		const float height = pRenderer->swapChainExtent.height;
		const float width = pRenderer->swapChainExtent.width;
		fm_mat4_ortho_projection(height, 0.0f, 0.0f, width, 0.0f, 1.0f, &ubo.proj);
		fm_mat4_identity(&ubo.model);
		fm_mat4_identity(&ubo.view);
		fm_mat4_transpose(&ubo.proj);
		
		fr_copy_data_to_buffer(pRenderer->device, pRenderer->aTextUniformBuffer[imageIndex].memory, &ubo, 0, sizeof(fr_uniform_buffer_t));
		fr_copy_data_to_buffer(pRenderer->device, pRenderer->aRectsUniformBuffer[imageIndex].memory, &ubo, 0, sizeof(fr_uniform_buffer_t));
	}
	
	// update debug lines buffer
	FUR_PROFILE("rend-update-debug")
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
		else
		{
			fr_clear_data_in_buffer(pRenderer->device, pRenderer->debugLinesVertexBuffer[imageIndex].memory, 0, (uint32_t)pRenderer->debugLinesVertexBuffer[imageIndex].size);
		}
		
		if(desc.trianglesDataSize > 0)
		{
			fr_clear_data_in_buffer(pRenderer->device, pRenderer->debugTrianglesVertexBuffer[imageIndex].memory, 0, desc.trianglesDataSize);
			fr_copy_data_to_buffer(pRenderer->device, pRenderer->debugTrianglesVertexBuffer[imageIndex].memory, desc.trianglesData, 0, desc.trianglesDataSize);
		}
		else
		{
			fr_clear_data_in_buffer(pRenderer->device, pRenderer->debugTrianglesVertexBuffer[imageIndex].memory, 0, (uint32_t)pRenderer->debugTrianglesVertexBuffer[imageIndex].size);
		}
		
		if(desc.rectsDataSize > 0)
		{
			fr_clear_data_in_buffer(pRenderer->device, pRenderer->aRectsVertexBuffer[imageIndex].memory, 0, fc_dbg_rects_buffer_size());
			fr_copy_data_to_buffer(pRenderer->device, pRenderer->aRectsVertexBuffer[imageIndex].memory, desc.rectsData, 0, desc.rectsDataSize);
		}
		else
		{
			fr_clear_data_in_buffer(pRenderer->device, pRenderer->aRectsVertexBuffer[imageIndex].memory, 0, fc_dbg_rects_buffer_size());
		}
		
		if(desc.textLinesCount > 0)
		{
			const uint32_t numFloatsCapacity = fc_dbg_text_characters_capacity() * FR_FONT_FLOATS_PER_GLYPH_VERTEX * 6;
			const uint32_t dataSize = numFloatsCapacity * sizeof(float);
			fr_clear_data_in_buffer(pRenderer->device, pRenderer->textVertexBuffer[imageIndex].memory, 0, dataSize);
			
			VkDeviceMemory dst = pRenderer->textVertexBuffer[imageIndex].memory;
			const uint32_t offset = 0;
			
			void* dataRaw;
			vkMapMemory(pRenderer->device, dst, offset, dataSize, 0, &dataRaw);
			
			float* verticesFloats = (float*)dataRaw;
			uint32_t dataSizeLeft = numFloatsCapacity;
			
			// convert from text info to glyphs
			for(uint32_t itl=0; itl<desc.textLinesCount; ++itl)
			{
				const uint32_t offsetLoc = itl * 5;
				const uint32_t offsetRange = itl * 2;
				const float* dataLoc = desc.textLocationData + offsetLoc;
				const uint32_t* dataRange = desc.textRangeData + offsetRange;
				
				const float* pos = dataLoc;
				const float* color = dataLoc + 2;
				const uint32_t offsetText = dataRange[0];
				
				const char* text = desc.textCharactersData + offsetText;
				
				const float scale = desc.textScaleData[itl];

				const uint32_t numFloatsWritten = fr_font_fill_vertex_buffer(&pRenderer->textFont, text, pos, color, verticesFloats, dataSizeLeft, scale);
				verticesFloats += numFloatsWritten;
				dataSizeLeft -= numFloatsWritten;
			}
			
			vkUnmapMemory(pRenderer->device, dst);
		}
		else
		{
			const uint32_t numFloatsCapacity = fc_dbg_text_characters_capacity() * FR_FONT_FLOATS_PER_GLYPH_VERTEX * 6;
			const uint32_t dataSize = numFloatsCapacity * sizeof(float);
			fr_clear_data_in_buffer(pRenderer->device, pRenderer->textVertexBuffer[imageIndex].memory, 0, dataSize);
		}
		
		fc_dbg_buffers_clear();	// clear the buffer for next frame
		
		fc_dbg_buffers_unlock();	// release lock, so now on everyone can use debug fragments again
	}
	
	// draw
	// begin primary command buffer
	VkCommandBuffer primaryCommandBuffer = fr_begin_primary_disposable_command_buffer(pRenderer->device, pRenderer->commandPool, pAllocCallbacks);
	
	// record PVS commands for each proxy
	FUR_PROFILE("pvs-rec-cmds")
	{
		VkRenderPassBeginInfo renderPassInfo = {0};
		renderPassInfo.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
		renderPassInfo.renderPass = pRenderer->renderPass;
		renderPassInfo.framebuffer = pRenderer->aSwapChainFrameBuffers[imageIndex];
		
		renderPassInfo.renderArea.offset.x = 0;
		renderPassInfo.renderArea.offset.y = 0;
		renderPassInfo.renderArea.extent = pRenderer->swapChainExtent;
		
		VkClearValue clearColor[2] = {0};
		const uint32_t numClearValues = 2;

		clearColor[0].color.float32[0] = 1.0f;
		clearColor[0].color.float32[1] = 1.0f;
		clearColor[0].color.float32[2] = 1.0f;
		clearColor[0].color.float32[3] = 1.0f;
		
		clearColor[1].depthStencil.depth = 1.0f;
		clearColor[1].depthStencil.stencil = 0;
		
		renderPassInfo.clearValueCount = numClearValues;
		renderPassInfo.pClearValues = clearColor;
		
		vkCmdBeginRenderPass(primaryCommandBuffer, &renderPassInfo, VK_SUBPASS_CONTENTS_INLINE);
		
		// bind and draw skinned meshes
		vkCmdBindPipeline(primaryCommandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pRenderer->graphicsPipeline);
		
		for(uint32_t idxProxy=0; idxProxy<pvs->numProxies; ++idxProxy)
		{
			// exclude non-skinned proxies
			if((pvs->proxiesFlags[idxProxy] & FR_PVS_PROXY_FLAG_SKINNED) == 0)
				continue;
			
			// bind uniform buffer
			vkCmdBindDescriptorSets(primaryCommandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS,
									pRenderer->pipelineLayout, 0, 1, &pvs->descriptorSets[idxProxy], 0, NULL);
			
			const fr_proxy_t* proxy = pvs->proxies[idxProxy];
			fr_mesh_t* mesh = proxy->mesh;
			for(uint32_t idxChunk=0; idxChunk<mesh->numChunks; ++idxChunk)
			{
				const fr_mesh_chunk_t* meshChunk = &mesh->chunks[idxChunk];
				
				// bind vertex and index buffers
				{
					VkDeviceSize offsets[] = {meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_VERTICES], meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_SKIN]};
					VkBuffer vertexBuffers[] = {meshChunk->data.buffer, meshChunk->data.buffer};
					vkCmdBindVertexBuffers(primaryCommandBuffer, 0, 2, vertexBuffers, offsets);
				}
				
				vkCmdBindIndexBuffer(primaryCommandBuffer, meshChunk->data.buffer, meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_INDICES], VK_INDEX_TYPE_UINT32);
				
				vkCmdPushConstants(primaryCommandBuffer, pRenderer->pipelineLayout,
								   VK_SHADER_STAGE_FRAGMENT_BIT, 0, sizeof(int32_t), &meshChunk->textureIndex);
				
				// draw the mesh chunk
				vkCmdDrawIndexed(primaryCommandBuffer, meshChunk->numIndices, 1, 0, 0, 0);
			}
		}
		
		// bind and draw static meshes
		vkCmdBindPipeline(primaryCommandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pRenderer->graphicsPipelineNoSkin);
		
		for(uint32_t idxProxy=0; idxProxy<pvs->numProxies; ++idxProxy)
		{
			// exclude skinned proxies
			if((pvs->proxiesFlags[idxProxy] & FR_PVS_PROXY_FLAG_SKINNED) == 1)
				continue;
			
			// bind prop uniform buffer
			vkCmdBindDescriptorSets(primaryCommandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS,
									pRenderer->pipelineLayout, 0, 1, &pvs->descriptorSets[idxProxy], 0, NULL);
			
			const fr_proxy_t* proxy = pvs->proxies[idxProxy];
			fr_mesh_t* mesh = proxy->mesh;
			for(uint32_t idxChunk=0; idxChunk<mesh->numChunks; ++idxChunk)
			{
				const fr_mesh_chunk_t* meshChunk = &mesh->chunks[idxChunk];
				
				// bind vertex and index buffers
				{
					VkDeviceSize offsets[] = {meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_VERTICES]};
					VkBuffer vertexBuffers[] = {meshChunk->data.buffer};
					vkCmdBindVertexBuffers(primaryCommandBuffer, 0, 1, vertexBuffers, offsets);
				}
				
				vkCmdBindIndexBuffer(primaryCommandBuffer, meshChunk->data.buffer, meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_INDICES], VK_INDEX_TYPE_UINT32);
				
				// todo: add constant for skinning of another object (offset in skinning buffer)
				vkCmdPushConstants(primaryCommandBuffer, pRenderer->pipelineLayout,
								   VK_SHADER_STAGE_FRAGMENT_BIT, 0, sizeof(int32_t), &meshChunk->textureIndex);
				
				// draw the mesh chunk
				vkCmdDrawIndexed(primaryCommandBuffer, meshChunk->numIndices, 1, 0, 0, 0);
			}
		}
		 
		VkDeviceSize offsets[] = {0};
		
		// debug draw 3D lines
		vkCmdBindPipeline(primaryCommandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pRenderer->debugLinesPSO);
		vkCmdBindVertexBuffers(primaryCommandBuffer, 0, 1, &pRenderer->debugLinesVertexBuffer[imageIndex].buffer, offsets);
		vkCmdDraw(primaryCommandBuffer, fc_dbg_line_num_total_vertices(), 1, 0, 0);
		
		// debug draw 3D triangles
		vkCmdBindPipeline(primaryCommandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pRenderer->debugTrianglesPSO);
		vkCmdBindVertexBuffers(primaryCommandBuffer, 0, 1, &pRenderer->debugTrianglesVertexBuffer[imageIndex].buffer, offsets);
		vkCmdDraw(primaryCommandBuffer, fc_dbg_triangles_num_total_vertices(), 1, 0, 0);
		
		// debug draw 2D rects
		vkCmdBindPipeline(primaryCommandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pRenderer->debugRectsPSO);
		
		vkCmdBindDescriptorSets(primaryCommandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS,
								pRenderer->rectsPipelineLayout, 0, 1, &pRenderer->aRectDescriptorSets[imageIndex], 0, NULL);
		
		vkCmdBindVertexBuffers(primaryCommandBuffer, 0, 1, &pRenderer->aRectsVertexBuffer[imageIndex].buffer, offsets);
		vkCmdDraw(primaryCommandBuffer, fc_dbg_rects_num_total_vertices(), 1, 0, 0);
		
		// debug draw text
		vkCmdBindPipeline(primaryCommandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pRenderer->debugTextPSO);
		
		vkCmdBindDescriptorSets(primaryCommandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS,
								pRenderer->textPipelineLayout, 0, 1, &pRenderer->aTextDescriptorSets[imageIndex], 0, NULL);
		
		vkCmdBindVertexBuffers(primaryCommandBuffer, 0, 1, &pRenderer->textVertexBuffer[imageIndex].buffer, offsets);
		vkCmdDraw(primaryCommandBuffer, fc_dbg_text_num_total_characters() * 6, 1, 0, 0);	// 6 - number of vertices per glyph
		
		vkCmdEndRenderPass(primaryCommandBuffer);
	}
	
	// end and submit primary command buffer, not waiting for GPU
	FUR_PROFILE("rend-submit-cmd")
	{
		fr_end_primary_disposable_command_buffer(pRenderer->device, pRenderer->graphicsQueue, primaryCommandBuffer, pRenderer->commandPool, pRenderer->imageAvailableSemaphore, pRenderer->renderFinishedSemaphore, pAllocCallbacks);
	}
	
	// present
	FUR_PROFILE("rend-present")
	{
		VkPresentInfoKHR presentInfo = {0};
		presentInfo.sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
		
		presentInfo.waitSemaphoreCount = 1;
		VkSemaphore signalSemaphores[] = {pRenderer->renderFinishedSemaphore};
		presentInfo.pWaitSemaphores = signalSemaphores;
		
		VkSwapchainKHR swapChains[] = {pRenderer->swapChain};
		presentInfo.swapchainCount = 1;
		presentInfo.pSwapchains = swapChains;
		presentInfo.pImageIndices = &imageIndex;
		
		presentInfo.pResults = NULL; // Optional
		
		vkQueuePresentKHR(pRenderer->presentQueue, &presentInfo);
	}
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

fr_proxy_t* fr_load_mesh(fr_renderer_t* pRenderer, const fi_depot_t* depot, const fr_load_mesh_ctx_t* ctx, fc_alloc_callbacks_t* pAllocCallbacks)
{
	fr_resource_mesh_t* meshResource = NULL;
	
	char pathEngine[256] = {0};
	fc_path_concat(pathEngine, depot->path, ctx->path, ctx->fileName, ".mesh");
	FILE* engineFile = fopen(pathEngine, "rb");
	
	// load mesh
	if(!engineFile)
	{
		char pathImport[256] = {0};
		fc_path_concat(pathImport, "", ctx->path, ctx->fileName, ".fbx");
		
		fi_import_mesh_ctx_t loadCtx;
		loadCtx.path = pathImport;
		
		fi_import_mesh(depot, &loadCtx, &meshResource, pAllocCallbacks);
	}
	
	// load or save serialized file
	{
		fc_serializer_t ser = {0};
		ser.file = engineFile ? engineFile : fopen(pathEngine, "wb");
		ser.isWriting = (engineFile == NULL);
		
		if(!ser.isWriting)
		{
			meshResource = FUR_ALLOC_AND_ZERO(sizeof(fr_resource_mesh_t), 8, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
		}
		
		fr_resource_mesh_serialize(&ser, meshResource, pAllocCallbacks);
		
		fclose(ser.file);
	}
	
	fr_mesh_t* mesh = FUR_ALLOC_AND_ZERO(sizeof(fr_mesh_t), 0, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
	
#define TEMP_MAX_TEXTURES_NUM 40

	VkDeviceSize imageStagingOffset[TEMP_MAX_TEXTURES_NUM] = {0};
	const uint32_t maxTextures = TEMP_MAX_TEXTURES_NUM;
	FUR_ASSERT(ctx->numTextures <= maxTextures);

	int32_t imageWidth[TEMP_MAX_TEXTURES_NUM] = {0};
	int32_t imageHeight[TEMP_MAX_TEXTURES_NUM] = {0};
	fr_image_t* textures = FUR_ALLOC_ARRAY_AND_ZERO(fr_image_t, ctx->numTextures, 0, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
	
	// prepare staging buffer builder
	fr_staging_buffer_builder_t stagingBuilder;
	fr_staging_init(&stagingBuilder);
	
	const VkFormat textureImageFormat = VK_FORMAT_R8G8B8A8_UNORM;
	uint32_t numTexturesInStagingBuffer = 0;
	
	uint32_t currentStagingIndex = 0;
	
	// load textures
	for(uint32_t i=0; i<ctx->numTextures; ++i)
	{
		VkDeviceSize imageSize = 0;
		
		// load texture
		{
			const char* texturePathRelative = ctx->texturePaths[i];
			char texturePath[256] = {0};
			sprintf(texturePath, "%s%s", depot->path, texturePathRelative);
			
			int texChannels;
			stbi_uc* pixels = stbi_load(texturePath, &imageWidth[i], &imageHeight[i], &texChannels, STBI_rgb_alpha);
			imageSize = imageWidth[i] * imageHeight[i] * 4;
			
			FUR_ASSERT(pixels);
			
			imageStagingOffset[i] = stagingBuilder.totalSize;
			fr_staging_add(&stagingBuilder, pixels, (uint32_t)imageSize, NULL, fr_pixels_free_func);
			numTexturesInStagingBuffer++;
			currentStagingIndex++;
		}
		
		{
			fr_image_desc_t desc = {0};
			desc.size = imageSize;
			desc.width = imageWidth[i];
			desc.height = imageHeight[i];
			desc.format = textureImageFormat;
			desc.usage = VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT;
			desc.properties = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
			
			fr_image_create(pRenderer->device, pRenderer->physicalDevice, &desc, &textures[i], pAllocCallbacks);
		}
	}
	
	// create mesh and its chunks
	{
		const uint32_t numChunks = meshResource->numChunks;
		mesh->chunks = (fr_mesh_chunk_t*)FUR_ALLOC_AND_ZERO(sizeof(fr_mesh_chunk_t) * numChunks, 16, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
		mesh->numChunks = numChunks;
		
		const uint32_t numTextureIndices = ctx->numTextureIndices;
		FUR_ASSERT(numChunks == numTextureIndices);
		const int32_t* textureIndices = ctx->textureIndices;
		
		for(uint32_t i=0; i<numChunks; ++i)
		{
			const fr_resource_mesh_chunk_t* meshChunkResource = &meshResource->chunks[i];
			const uint32_t numVertices = meshChunkResource->numVertices;
			const VkDeviceSize sizeIndices = numVertices * sizeof(uint32_t);
			const VkDeviceSize sizeVertices = numVertices * sizeof(fr_vertex_t);
			const VkDeviceSize sizeSkin = numVertices * sizeof(fr_resource_mesh_chunk_skin_t);	// optional
			
			fr_mesh_chunk_t* meshChunk = &mesh->chunks[i];
			meshChunk->numIndices = numVertices;
			meshChunk->textureIndex = textureIndices[i];
			
			// data buffer
			{
				fr_buffer_desc_t desc = {0};
				desc.size = sizeIndices + sizeVertices;
				
				// skinning data
				if(ctx->isSkinned)
					desc.size += sizeSkin;
				
				desc.usage = FR_VERTEX_BUFFER_USAGE_FLAGS | VK_BUFFER_USAGE_INDEX_BUFFER_BIT;
				desc.properties = FR_VERTEX_BUFFER_MEMORY_FLAGS;
				
				fr_buffer_create(pRenderer->device, pRenderer->physicalDevice, &desc, &meshChunk->data, pAllocCallbacks);
				
				meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_INDICES] = 0;
				meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_VERTICES] = sizeIndices;
				
				if(ctx->isSkinned)
					meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_SKIN] = sizeIndices + sizeVertices;
				else
					meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_SKIN] = 0;
			}
		}
	}
	
	const uint32_t proxyIndex = pRenderer->numProxies;
	pRenderer->numProxies += 1;
	
	fr_proxy_t* proxy = &pRenderer->proxies[proxyIndex];
	proxy->numTextures = ctx->numTextures;
	proxy->mesh = mesh;
	proxy->textures = textures;
	
	const fr_resource_mesh_chunk_t* masterSkinnedMeshChunk = &meshResource->chunks[0]; // todo: fix that, it's just an assumption chunk[0] works
	const uint32_t meshNumBones = masterSkinnedMeshChunk->numBones;
	
	if(meshNumBones > 0)
	{
		FUR_ASSERT(ctx->boneNames);
		
		proxy->numBones = meshNumBones;
		proxy->invBindPose = FUR_ALLOC_ARRAY(fm_mat4, meshNumBones, 0, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
		proxy->skinningMappinng = FUR_ALLOC_ARRAY_AND_ZERO(int16_t, meshNumBones, 0, FC_MEMORY_SCOPE_RENDER, pAllocCallbacks);
		
		memcpy(proxy->invBindPose, masterSkinnedMeshChunk->bindPose, meshNumBones * sizeof(fm_mat4));
		
		int16_t* mapping = proxy->skinningMappinng;
		for(uint32_t i=0; i<meshNumBones; ++i)	// note that rig num bones doesn't have to equal mesh num bones
		{
			mapping[i] = 0;
			for(uint32_t r=0; r<ctx->numBones; ++r)
			{
				if(masterSkinnedMeshChunk->boneNameHashes[i] == ctx->boneNames[r])
				{
					mapping[i] = (int16_t)r;
				}
			}
		}
	}
	
	// load data onto GPU
	{
		fr_buffer_t stagingBuffer = {0};
		
		// add vertices & indices data to staging buffer for mesh
		for(uint32_t i=0; i<meshResource->numChunks; ++i)
		{
			fr_resource_mesh_chunk_t* meshChunk = &meshResource->chunks[i];
			
			fr_staging_add(&stagingBuilder, (void*)meshChunk->dataIndices, sizeof(uint32_t) * meshChunk->numIndices, NULL, NULL);
			fr_staging_add(&stagingBuilder, (void*)meshChunk->dataVertices, sizeof(fr_vertex_t) * meshChunk->numVertices, NULL, NULL);
			
			// if skinned mesh
			if(meshChunk->dataSkinning != NULL && ctx->isSkinned)
				fr_staging_add(&stagingBuilder, (void*)meshChunk->dataSkinning, sizeof(fr_resource_mesh_chunk_skin_t) * meshChunk->numVertices, NULL, NULL);
		}
		
		// create staging buffer & release memory of source data
		{
			stagingBuffer.size = stagingBuilder.totalSize;
			fr_staging_build(&stagingBuilder, pRenderer->device, pRenderer->physicalDevice, &stagingBuffer.buffer, &stagingBuffer.memory, pAllocCallbacks);
			fr_staging_release_builder(&stagingBuilder);
		}
		
		// create staging command pool
		VkCommandPool stagingCommandPool = NULL;
		
		{
			VkCommandPoolCreateInfo poolInfo = {0};
			poolInfo.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
			poolInfo.queueFamilyIndex = pRenderer->idxQueueGraphics;
			poolInfo.flags = VK_COMMAND_POOL_CREATE_TRANSIENT_BIT; // Optional
			
			if (vkCreateCommandPool(pRenderer->device, &poolInfo, NULL, &stagingCommandPool) != VK_SUCCESS)
			{
				FUR_ASSERT(false);
			}
		}
		
		// record and execute staging command buffer
		{
			VkCommandBuffer commandBuffer = fr_begin_simple_commands(pRenderer->device, stagingCommandPool, pAllocCallbacks);
			
			// copy vertex buffer region
			for(uint32_t i=0; i<mesh->numChunks; ++i)
			{
				fr_mesh_chunk_t* meshChunk = &mesh->chunks[i];
	
				fr_resource_mesh_chunk_t* sourceMeshChunk = &meshResource->chunks[i];
				if(sourceMeshChunk->dataSkinning && ctx->isSkinned)	// todo: dataSkinning is probably always there, need to check
				{
					uint32_t srcStagingIndices[3] = {currentStagingIndex, currentStagingIndex+1, currentStagingIndex+2};
					VkBuffer dstBuffers[3] = {meshChunk->data.buffer, meshChunk->data.buffer, meshChunk->data.buffer};
					VkDeviceSize dstOffsets[3] = {meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_INDICES], meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_VERTICES], meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_SKIN]};
					
					fr_staging_record_copy_commands(&stagingBuilder, commandBuffer, stagingBuffer.buffer, srcStagingIndices, dstBuffers, dstOffsets, 3);
					
					currentStagingIndex += 3;
				}
				else
				{
					uint32_t srcStagingIndices[2] = {currentStagingIndex, currentStagingIndex+1};
					VkBuffer dstBuffers[2] = {meshChunk->data.buffer, meshChunk->data.buffer};
					VkDeviceSize dstOffsets[2] = {meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_INDICES], meshChunk->offsets[FR_MESH_CHUNK_BUFFER_OFFSET_VERTICES]};
					
					fr_staging_record_copy_commands(&stagingBuilder, commandBuffer, stagingBuffer.buffer, srcStagingIndices, dstBuffers, dstOffsets, 2);
					
					currentStagingIndex += 2;
				}
			}
			
			fr_end_simple_commands(pRenderer->device, pRenderer->graphicsQueue, commandBuffer, stagingCommandPool, pAllocCallbacks);
			
			// textures
			for(uint32_t i=0; i<ctx->numTextures; ++i)
			{
				fr_transition_image_layout(pRenderer->device, pRenderer->graphicsQueue, stagingCommandPool, textureImageFormat,
										   VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, textures[i].image, pAllocCallbacks);
				fr_copy_buffer_to_image(pRenderer->device, pRenderer->graphicsQueue, stagingCommandPool, stagingBuffer.buffer,
										imageStagingOffset[i], textures[i].image, imageWidth[i], imageHeight[i], pAllocCallbacks);
			}
		}
		
		fr_buffer_release(pRenderer->device, &stagingBuffer, pAllocCallbacks);
		
		vkDestroyCommandPool(pRenderer->device, stagingCommandPool, NULL);
	}
	
	fr_mesh_release(meshResource, pAllocCallbacks);
	
	return proxy;
}

void fr_release_proxy(fr_renderer_t* pRenderer, fr_proxy_t* proxy, fc_alloc_callbacks_t* pAllocCallbacks)
{
	// release mesh
	for(uint32_t i=0; i<proxy->mesh->numChunks; ++i)
	{
		fr_mesh_chunk_t* meshChunk = &proxy->mesh->chunks[i];
		
		// destroy vertex buffer
		fr_buffer_release(pRenderer->device, &meshChunk->data, pAllocCallbacks);
	}
	
	// release textures
	for(uint32_t i=0; i<proxy->numTextures; ++i)
	{
		fr_image_release(pRenderer->device, &proxy->textures[i], pAllocCallbacks);
	}
	
	FUR_FREE(proxy->mesh->chunks, pAllocCallbacks);
	FUR_FREE(proxy->mesh, pAllocCallbacks);
	FUR_FREE(proxy->textures, pAllocCallbacks);
	FUR_FREE(proxy->invBindPose, pAllocCallbacks);
	FUR_FREE(proxy->skinningMappinng, pAllocCallbacks);
}
