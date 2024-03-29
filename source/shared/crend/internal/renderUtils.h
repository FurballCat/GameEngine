/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

struct fc_alloc_callbacks_t;

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
					  struct fc_alloc_callbacks_t* pAllocCallbacks);

// release every entry memory
void fr_staging_release_builder(fr_staging_buffer_builder_t* builder);

// copy region 'srcRegionIndex' from staging buffer to dstBuffer (from staging to vertex buffer for example), supports arrays of buffers
void fr_staging_record_copy_commands(fr_staging_buffer_builder_t* builder, VkCommandBuffer commandBuffer, VkBuffer stagingBuffer,
									 uint32_t* aSrcRegionIndex, VkBuffer* aDstBuffer, const VkDeviceSize* aDstOffsets, uint32_t numBuffers);

// ----------------
// DESCRIPTOR UTILS

typedef struct fr_buffer_t fr_buffer_t;
typedef struct fr_image_t fr_image_t;

typedef struct fr_alloc_descriptor_sets_mesh_ctx_t
{
	VkDescriptorSetLayout layout;	// same layout for all descritors
	VkDescriptorPool descriptorPool;	// remember to have space for descriptors in the pool
	
	// per descriptor data
	uint32_t numDescriptors;	// also numUniformBuffers and numSkinningBuffers
	VkDescriptorSet* outDescriptorSets;
	fr_buffer_t* uniformBuffers;
	fr_buffer_t* skinningBuffers;
	size_t uniformBufferSize;
	size_t uniformBufferOffset;
	size_t skinningBufferSize;
	size_t skinningBufferOffset;
	
	// for all descritors
	uint32_t numTextures;
	fr_image_t* textures;
	VkSampler* samplers;
} fr_alloc_descriptor_sets_mesh_ctx_t;

void fr_alloc_descriptor_sets_mesh(VkDevice device, fr_alloc_descriptor_sets_mesh_ctx_t* ctx);

typedef struct fr_write_descriptor_set_ctx_t
{
	fr_buffer_t* uniformBuffer;
	fr_buffer_t* skinningBuffer;
	size_t uniformBufferSize;
	size_t uniformBufferOffset;
	size_t skinningBufferSize;
	size_t skinningBufferOffset;
	
	// for all descritors
	uint32_t numTextures;
	fr_image_t* textures;
	VkSampler* samplers;
} fr_write_descriptor_set_ctx_t;

void fr_write_descriptor_set(VkDevice device, fr_write_descriptor_set_ctx_t* ctx, VkDescriptorSet descriptor);

// -------------
// COMMAND UTILS

// allocate and begin one time command buffer
VkCommandBuffer fr_begin_simple_commands(VkDevice device, VkCommandPool commandPool, struct fc_alloc_callbacks_t* pAllocCallbacks);

// end, submit, and free one time command buffer
void fr_end_simple_commands(VkDevice device, VkQueue graphicsQueue, VkCommandBuffer commandBuffer, VkCommandPool commandPool, struct fc_alloc_callbacks_t* pAllocCallbacks);

// begin primary command buffer that will be disposed immediately after submission, does not wait for GPU
VkCommandBuffer fr_begin_primary_disposable_command_buffer(VkDevice device, VkCommandPool commandPool, struct fc_alloc_callbacks_t* pAllocCallbacks);

// end primary command buffer and submit to GPU, dispose after
void fr_end_primary_disposable_command_buffer(VkDevice device, VkQueue graphicsQueue, VkCommandBuffer commandBuffer, VkCommandPool commandPool,
											  VkSemaphore imageAvailableSemaphore, VkSemaphore renderFinishedSemaphore,struct fc_alloc_callbacks_t* pAllocCallbacks);

// transition image layout
void fr_transition_image_layout(VkDevice device, VkQueue graphicsQueue, VkCommandPool commandPool, VkFormat format, VkImageLayout oldLayout, VkImageLayout newLayout, VkImage image, struct fc_alloc_callbacks_t* pAllocCallbacks);

// copy buffer to image
void fr_copy_buffer_to_image(VkDevice device, VkQueue graphicsQueue, VkCommandPool commandPool, VkBuffer buffer, VkDeviceSize bufferOffset, VkImage image, uint32_t width, uint32_t height, struct fc_alloc_callbacks_t* pAllocCallbacks);

// -------------
// MEMORY UTILS
uint32_t fr_find_memory_type(const VkPhysicalDevice physicalDevice, uint32_t typeFilter, VkMemoryPropertyFlags propertyFlags);
