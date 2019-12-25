/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

struct fr_allocation_callbacks_t;

// --------------------
// STAGING BUFFER UTILS

typedef void (*fr_staging_free_data_func_t)(void* 		pData,
											 size_t 	size,
											 void* 		pUserData);

// one entry of staging buffer builder represents one buffer (vertex buffer, index buffer, texture buffer, ubo etc.)
typedef struct fr_staging_buffer_entry_t
{
	void* pData;
	uint32_t size;
	uint32_t offset;
	
	fr_staging_free_data_func_t fnFree;
	void* pUserData;
	
} fr_staging_buffer_entry_t;

#define FR_STAGIN_BUFFER_MAX_COUNT 128

// helper for building staging buffer, add entries with release callback, build everything in one go
typedef struct fr_staging_buffer_builder_t
{
	fr_staging_buffer_entry_t entries[FR_STAGIN_BUFFER_MAX_COUNT];
	
	uint32_t count;
	uint32_t totalSize;
	
} fr_staging_buffer_builder_t;

// prepare staging buffer builder for adding entries
void fr_staging_init(fr_staging_buffer_builder_t* builder);

// add entry + optionally how to release its memory, pass NULL to fnFree if no need for release callback
void fr_staging_add(fr_staging_buffer_builder_t* builder, void* pData, uint32_t size,
					void* pUserData, fr_staging_free_data_func_t fnFree);

// build staging buffer and copy data into
void fr_staging_build(fr_staging_buffer_builder_t* builder,
					  VkDevice device, VkPhysicalDevice physicalDevice,
					  VkBuffer* buffer, VkDeviceMemory* bufferMemory,
					  struct fr_allocation_callbacks_t* pAllocCallbacks);

// release every entry memory
void fr_staging_release_builder(fr_staging_buffer_builder_t* builder);

// copy region 'srcRegionIndex' from staging buffer to dstBuffer (from staging to vertex buffer for example), supports arrays of buffers
void fr_staging_record_copy_commands(fr_staging_buffer_builder_t* builder, VkCommandBuffer commandBuffer, VkBuffer stagingBuffer,
									 uint32_t* aSrcRegionIndex, VkBuffer* aDstBuffer, uint32_t numBuffers);

// -------------
// COMMAND UTILS

// allocate and begin one time command buffer
VkCommandBuffer fr_begin_simple_commands(VkDevice device, VkCommandPool commandPool, struct fr_allocation_callbacks_t* pAllocCallbacks);

// end, submit, and free one time command buffer
void fr_end_simple_commands(VkDevice device, VkQueue graphicsQueue, VkCommandBuffer commandBuffer, VkCommandPool commandPool, struct fr_allocation_callbacks_t* pAllocCallbacks);

// transition image layout
void fr_transition_image_layout(VkDevice device, VkQueue graphicsQueue, VkCommandPool commandPool, VkFormat format, VkImageLayout oldLayout, VkImageLayout newLayout, VkImage image, struct fr_allocation_callbacks_t* pAllocCallbacks);

// copy buffer to image
void fr_copy_buffer_to_image(VkDevice device, VkQueue graphicsQueue, VkCommandPool commandPool, VkBuffer buffer, VkDeviceSize bufferOffset, VkImage image, uint32_t width, uint32_t height, struct fr_allocation_callbacks_t* pAllocCallbacks);
