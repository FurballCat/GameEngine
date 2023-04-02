/* Copyright (c) 2016-2019 Furball Cat */

#include "vulkan.h"
#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include "renderBuffer.h"
#include "renderUtils.h"

#define FUR_ASSERT(x) assert(x)

void fcRenderBufferCreate(VkDevice device, VkPhysicalDevice physicalDevice, const FcRenderBufferDesc* pDesc,
					  FcRenderBuffer* pBuffer, struct FcAllocator* pAllocCallbacks)
{
	fcRenderCreateBuffer(device, physicalDevice, pDesc->size, pDesc->usage, pDesc->properties, &pBuffer->buffer, &pBuffer->memory, pAllocCallbacks);
	pBuffer->size = pDesc->size;
}

void fcRenderBufferRelease(VkDevice device, FcRenderBuffer* pBuffer, struct FcAllocator* pAllocCallbacks)
{
	vkDestroyBuffer(device, pBuffer->buffer, NULL);
	vkFreeMemory(device, pBuffer->memory, NULL);
	memset(pBuffer, 0, sizeof(FcRenderBuffer));
}

void fcRenderCreateBuffer(VkDevice device, VkPhysicalDevice physicalDevice,
					VkDeviceSize size, VkBufferUsageFlags usage,
					VkMemoryPropertyFlags properties,
					VkBuffer* buffer, VkDeviceMemory* bufferMemory,
					struct FcAllocator* pAllocCallbacks)
{
	VkBufferCreateInfo bufferInfo = {0};
	bufferInfo.sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
	bufferInfo.size = size;
	bufferInfo.usage = usage;
	bufferInfo.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
	
	if (vkCreateBuffer(device, &bufferInfo, NULL, buffer) != VK_SUCCESS)
	{
		FUR_ASSERT(false);
	}
	
	VkMemoryRequirements memRequirements;
	vkGetBufferMemoryRequirements(device, *buffer, &memRequirements);
	
	VkMemoryAllocateInfo allocInfo = {0};
	allocInfo.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
	allocInfo.allocationSize = memRequirements.size;
	allocInfo.memoryTypeIndex = fcRenderFindMemoryType(physicalDevice, memRequirements.memoryTypeBits, properties);
	
	if (vkAllocateMemory(device, &allocInfo, NULL, bufferMemory) != VK_SUCCESS)
	{
		FUR_ASSERT(false);
	}
	
	vkBindBufferMemory(device, *buffer, *bufferMemory, 0);
}

void fcRenderCreateImage(VkDevice device, VkPhysicalDevice physicalDevice,
					  VkDeviceSize size, VkFormat format, VkBufferUsageFlags usage,
					  VkMemoryPropertyFlags properties, u32 width, u32 height,
					  VkImage* textureImage, VkDeviceMemory* textureImageMemory,
					  struct FcAllocator* pAllocCallbacks)
{
	VkImageCreateInfo imageInfo = {0};
	imageInfo.sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
	imageInfo.imageType = VK_IMAGE_TYPE_2D;
	imageInfo.extent.width = width;
	imageInfo.extent.height = height;
	imageInfo.extent.depth = 1;
	imageInfo.mipLevels = 1;
	imageInfo.arrayLayers = 1;
	
	imageInfo.format = format;
	imageInfo.tiling = VK_IMAGE_TILING_OPTIMAL;
	imageInfo.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
	imageInfo.usage = usage;
	imageInfo.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
	imageInfo.samples = VK_SAMPLE_COUNT_1_BIT;
	imageInfo.flags = 0; // Optional
	
	if (vkCreateImage(device, &imageInfo, NULL, textureImage) != VK_SUCCESS)
	{
		FUR_ASSERT(false);
	}
	
	VkMemoryRequirements memRequirements;
	vkGetImageMemoryRequirements(device, *textureImage, &memRequirements);
	
	VkMemoryAllocateInfo allocInfo = {0};
	allocInfo.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
	allocInfo.allocationSize = memRequirements.size;
	allocInfo.memoryTypeIndex = fcRenderFindMemoryType(physicalDevice, memRequirements.memoryTypeBits, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);
	
	if (vkAllocateMemory(device, &allocInfo, NULL, textureImageMemory) != VK_SUCCESS)
	{
		FUR_ASSERT(false);
	}
	
	vkBindImageMemory(device, *textureImage, *textureImageMemory, 0);
}

void fcRenderCopyDataToBuffer(VkDevice device, VkDeviceMemory dst, const void* src, u32 offset, u32 size)
{
	void* data;
	vkMapMemory(device, dst, offset, size, 0, &data);
	memcpy(data, src, size);
	vkUnmapMemory(device, dst);
}

void fcRenderClearDataInBuffer(VkDevice device, VkDeviceMemory dst, u32 offset, u32 size)
{
	void* data;
	vkMapMemory(device, dst, offset, size, 0, &data);
	memset(data, 0, size);
	vkUnmapMemory(device, dst);
}
