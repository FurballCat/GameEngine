/* Copyright (c) 2016-2019 Furball Cat */

#include "vulkansdk/macOS/include/vulkan/vulkan.h"
#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include "renderBuffer.h"

#define FUR_ASSERT(x) assert(x)

uint32_t frFindMemoryType(const VkPhysicalDevice physicalDevice, uint32_t typeFilter, VkMemoryPropertyFlags propertyFlags)
{
	VkPhysicalDeviceMemoryProperties memProperties;
	vkGetPhysicalDeviceMemoryProperties(physicalDevice, &memProperties);
	
	for (uint32_t i = 0; i < memProperties.memoryTypeCount; ++i)
	{
		if ((typeFilter & (1 << i)) && (memProperties.memoryTypes[i].propertyFlags & propertyFlags) == propertyFlags)
		{
			return i;
		}
	}
	
	FUR_ASSERT(false);
	
	return (uint32_t)-1;
}

void fr_buffer_create(VkDevice device, VkPhysicalDevice physicalDevice, const fr_buffer_desc_t* pDesc,
					  fr_buffer_t* pBuffer, struct fr_allocation_callbacks_t* pAllocCallbacks)
{
	fr_create_buffer(device, physicalDevice, pDesc->size, pDesc->usage, pDesc->properties, &pBuffer->buffer, &pBuffer->memory, pAllocCallbacks);
	pBuffer->size = pDesc->size;
}

void fr_buffer_release(VkDevice device, fr_buffer_t* pBuffer, struct fr_allocation_callbacks_t* pAllocCallbacks)
{
	vkDestroyBuffer(device, pBuffer->buffer, NULL);
	vkFreeMemory(device, pBuffer->memory, NULL);
	memset(pBuffer, 0, sizeof(fr_buffer_t));
}

void fr_create_buffer(VkDevice device, VkPhysicalDevice physicalDevice,
					VkDeviceSize size, VkBufferUsageFlags usage,
					VkMemoryPropertyFlags properties,
					VkBuffer* buffer, VkDeviceMemory* bufferMemory,
					struct fr_allocation_callbacks_t* pAllocCallbacks)
{
	VkBufferCreateInfo bufferInfo = {};
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
	
	VkMemoryAllocateInfo allocInfo = {};
	allocInfo.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
	allocInfo.allocationSize = memRequirements.size;
	allocInfo.memoryTypeIndex = frFindMemoryType(physicalDevice, memRequirements.memoryTypeBits, properties);
	
	if (vkAllocateMemory(device, &allocInfo, NULL, bufferMemory) != VK_SUCCESS)
	{
		FUR_ASSERT(false);
	}
	
	vkBindBufferMemory(device, *buffer, *bufferMemory, 0);
}

void fr_create_image(VkDevice device, VkPhysicalDevice physicalDevice,
					  VkDeviceSize size, VkFormat format, VkBufferUsageFlags usage,
					  VkMemoryPropertyFlags properties, uint32_t width, uint32_t height,
					  VkImage* textureImage, VkDeviceMemory* textureImageMemory,
					  struct fr_allocation_callbacks_t* pAllocCallbacks)
{
	VkImageCreateInfo imageInfo = {};
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
	
	VkMemoryAllocateInfo allocInfo = {};
	allocInfo.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
	allocInfo.allocationSize = memRequirements.size;
	allocInfo.memoryTypeIndex = frFindMemoryType(physicalDevice, memRequirements.memoryTypeBits, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);
	
	if (vkAllocateMemory(device, &allocInfo, NULL, textureImageMemory) != VK_SUCCESS)
	{
		FUR_ASSERT(false);
	}
	
	vkBindImageMemory(device, *textureImage, *textureImageMemory, 0);
}

void fr_copy_data_to_buffer(VkDevice device, VkDeviceMemory dst, const void* src, uint32_t offset, uint32_t size)
{
	void* data;
	vkMapMemory(device, dst, offset, size, 0, &data);
	memcpy(data, src, size);
	vkUnmapMemory(device, dst);
}
