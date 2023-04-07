/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#include "ccore/types.h"

typedef struct FcAllocator FcAllocator;

// --------------------
// STAGING BUFFER UTILS

typedef void (*FcRenderStagingFreeDataFn)(void* 		pData,
											 u64 	size,
											 void* 		pUserData);

// one entry of staging buffer builder represents one buffer (vertex buffer, index buffer, texture buffer, ubo etc.)
typedef struct FcRenderStagingBufferEntry
{
	void* pData;
	u32 size;
	u32 offset;
	
	FcRenderStagingFreeDataFn fnFree;
	void* pUserData;
	
} FcRenderStagingBufferEntry;

#define FR_STAGIN_BUFFER_MAX_COUNT 128

// helper for building staging buffer, add entries with release callback, build everything in one go
typedef struct FcRenderStagingBufferBuilder
{
	FcRenderStagingBufferEntry entries[FR_STAGIN_BUFFER_MAX_COUNT];
	
	u32 count;
	u32 totalSize;
	
} FcRenderStagingBufferBuilder;

// prepare staging buffer builder for adding entries
void fcRenderStagingInit(FcRenderStagingBufferBuilder* builder);

// add entry + optionally how to release its memory, pass NULL to fnFree if no need for release callback
void fcRenderStagingAdd(FcRenderStagingBufferBuilder* builder, void* pData, u32 size,
					void* pUserData, FcRenderStagingFreeDataFn fnFree);

// build staging buffer and copy data into
void fcRenderStagingBuild(FcRenderStagingBufferBuilder* builder,
					  VkDevice device, VkPhysicalDevice physicalDevice,
					  VkBuffer* buffer, VkDeviceMemory* bufferMemory,
					  FcAllocator* allocator);

// release every entry memory
void fcRenderStagingBufferBuilderRelease(FcRenderStagingBufferBuilder* builder);

// copy region 'srcRegionIndex' from staging buffer to dstBuffer (from staging to vertex buffer for example), supports arrays of buffers
void fcRenderStagingRecordCompyCommands(FcRenderStagingBufferBuilder* builder, VkCommandBuffer commandBuffer, VkBuffer stagingBuffer,
									 u32* aSrcRegionIndex, VkBuffer* aDstBuffer, const VkDeviceSize* aDstOffsets, u32 numBuffers);

// ----------------
// DESCRIPTOR UTILS

typedef struct FcRenderBuffer FcRenderBuffer;
typedef struct FcImage FcImage;

typedef struct FcRenderAllocDescriptorSetsMeshCtx
{
	VkDescriptorSetLayout layout;	// same layout for all descritors
	VkDescriptorPool descriptorPool;	// remember to have space for descriptors in the pool
	
	// per descriptor data
	u32 numDescriptors;	// also numUniformBuffers and numSkinningBuffers
	VkDescriptorSet* outDescriptorSets;
	FcRenderBuffer* uniformBuffers;
	FcRenderBuffer* skinningBuffers;
	u64 uniformBufferSize;
	u64 uniformBufferOffset;
	u64 skinningBufferSize;
	u64 skinningBufferOffset;
	
	// for all descritors
	u32 numTextures;
	FcImage* textures;
	VkSampler* samplers;
} FcRenderAllocDescriptorSetsMeshCtx;

void fcRenderAllocDescriptorSetsMesh(VkDevice device, FcRenderAllocDescriptorSetsMeshCtx* ctx);

typedef struct FcRenderWriteDescriptorSetsCtx
{
	FcRenderBuffer* uniformBuffer;
	FcRenderBuffer* skinningBuffer;
	u64 uniformBufferSize;
	u64 uniformBufferOffset;
	u64 skinningBufferSize;
	u64 skinningBufferOffset;
	
	// for all descritors
	u32 numTextures;
	FcImage* textures;
	VkSampler* samplers;
} FcRenderWriteDescriptorSetsCtx;

void fcRenderWriteDescriptorSets(VkDevice device, FcRenderWriteDescriptorSetsCtx* ctx, VkDescriptorSet descriptor);

// -------------
// COMMAND UTILS

// allocate and begin one time command buffer
VkCommandBuffer fcRenderBeginSimpleCommands(VkDevice device, VkCommandPool commandPool, struct FcAllocator* allocator);

// end, submit, and free one time command buffer
void fcRenderEndSimpleCommands(VkDevice device, VkQueue graphicsQueue, VkCommandBuffer commandBuffer, VkCommandPool commandPool, struct FcAllocator* allocator);

// begin primary command buffer that will be disposed immediately after submission, does not wait for GPU
VkCommandBuffer fcRenderBeginPrimaryDisposableCommandBuffer(VkDevice device, VkCommandPool commandPool, struct FcAllocator* allocator);

// end primary command buffer and submit to GPU, dispose after
void fcRenderEndPrimaryDisposableCommandBuffer(VkDevice device, VkQueue graphicsQueue, VkCommandBuffer commandBuffer, VkCommandPool commandPool,
											  VkSemaphore imageAvailableSemaphore, VkSemaphore renderFinishedSemaphore,struct FcAllocator* allocator);

// transition image layout
void fcRenderTransitionImageLayout(VkDevice device, VkQueue graphicsQueue, VkCommandPool commandPool, VkFormat format, VkImageLayout oldLayout, VkImageLayout newLayout, VkImage image, struct FcAllocator* allocator);

// copy buffer to image
void fcRenderCopyBufferToImage(VkDevice device, VkQueue graphicsQueue, VkCommandPool commandPool, VkBuffer buffer, VkDeviceSize bufferOffset, VkImage image, u32 width, u32 height, struct FcAllocator* allocator);

// -------------
// MEMORY UTILS
u32 fcRenderFindMemoryType(const VkPhysicalDevice physicalDevice, u32 typeFilter, VkMemoryPropertyFlags propertyFlags);
