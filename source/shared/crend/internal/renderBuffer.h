/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#include <inttypes.h>

struct fr_allocation_callbacks_t;

// desc for buffer creation
typedef struct fr_buffer_desc_t
{
	VkDeviceSize size;
	VkBufferUsageFlags usage;
	VkMemoryPropertyFlags properties;
} fr_buffer_desc_t;

// buffer compact handle and data
typedef struct fr_buffer_t
{
	VkBuffer buffer;
	VkDeviceMemory memory;
	VkDeviceSize size;
} fr_buffer_t;

// buffer creation, allocates memory
void fr_buffer_create(VkDevice device, VkPhysicalDevice physicalDevice, const fr_buffer_desc_t* pDesc,
					  fr_buffer_t* pBuffer, struct fr_allocation_callbacks_t* pAllocCallbacks);

// buffer release, deallocates memory
void fr_buffer_release(VkDevice device, fr_buffer_t* pBuffer, struct fr_allocation_callbacks_t* pAllocCallbacks);

uint32_t frFindMemoryType(const VkPhysicalDevice physicalDevice,
						  uint32_t typeFilter, VkMemoryPropertyFlags propertyFlags);

void fr_create_buffer(VkDevice device, VkPhysicalDevice physicalDevice,
					VkDeviceSize size, VkBufferUsageFlags usage,
					VkMemoryPropertyFlags properties,
					VkBuffer* buffer, VkDeviceMemory* bufferMemory,
					struct fr_allocation_callbacks_t* pAllocCallbacks);

void fr_create_image(VkDevice device, VkPhysicalDevice physicalDevice,
					 VkDeviceSize size, VkFormat format, VkBufferUsageFlags usage,
					 VkMemoryPropertyFlags properties, uint32_t width, uint32_t height,
					 VkImage* textureImage, VkDeviceMemory* textureImageMemory,
					 struct fr_allocation_callbacks_t* pAllocCallbacks);

void fr_copy_data_to_buffer(VkDevice device, VkDeviceMemory dst, const void* src, uint32_t offset, uint32_t size);

#define FR_STAGING_BUFFER_USAGE_FLAGS VK_BUFFER_USAGE_TRANSFER_SRC_BIT
#define FR_STAGING_BUFFER_MEMORY_FLAGS VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT

#define FR_VERTEX_BUFFER_USAGE_FLAGS VK_BUFFER_USAGE_TRANSFER_DST_BIT | VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
#define FR_VERTEX_BUFFER_MEMORY_FLAGS VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
