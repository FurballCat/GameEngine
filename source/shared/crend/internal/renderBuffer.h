/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#include "ccore/types.h"

struct FcAllocator;

// desc for buffer creation
typedef struct FcRenderBufferDesc
{
	VkDeviceSize size;
	VkBufferUsageFlags usage;
	VkMemoryPropertyFlags properties;
} FcRenderBufferDesc;

// buffer compact handle and data
typedef struct FcRenderBuffer
{
	VkBuffer buffer;
	VkDeviceMemory memory;
	VkDeviceSize size;
} FcRenderBuffer;

// buffer creation, allocates memory
void fcRenderBufferCreate(VkDevice device, VkPhysicalDevice physicalDevice, const FcRenderBufferDesc* pDesc,
					  FcRenderBuffer* pBuffer, struct FcAllocator* allocator);

// buffer release, deallocates memory
void fcRenderBufferRelease(VkDevice device, FcRenderBuffer* pBuffer, struct FcAllocator* allocator);

void fcRenderCreateBuffer(VkDevice device, VkPhysicalDevice physicalDevice,
					VkDeviceSize size, VkBufferUsageFlags usage,
					VkMemoryPropertyFlags properties,
					VkBuffer* buffer, VkDeviceMemory* bufferMemory,
					struct FcAllocator* allocator);

void fcRenderCreateImage(VkDevice device, VkPhysicalDevice physicalDevice,
					 VkDeviceSize size, VkFormat format, VkBufferUsageFlags usage,
					 VkMemoryPropertyFlags properties, u32 width, u32 height,
					 VkImage* textureImage, VkDeviceMemory* textureImageMemory,
					 struct FcAllocator* allocator);

void fcRenderCopyDataToBuffer(VkDevice device, VkDeviceMemory dst, const void* src, u32 offset, u32 size);
void fcRenderClearDataInBuffer(VkDevice device, VkDeviceMemory dst, u32 offset, u32 size);

#define FR_STAGING_BUFFER_USAGE_FLAGS VK_BUFFER_USAGE_TRANSFER_SRC_BIT
#define FR_STAGING_BUFFER_MEMORY_FLAGS VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT

#define FR_VERTEX_BUFFER_USAGE_FLAGS VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
#define FR_VERTEX_BUFFER_MEMORY_FLAGS VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
