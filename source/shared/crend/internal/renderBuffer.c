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

void frCreateBuffer(VkDevice device, VkPhysicalDevice physicalDevice,
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

void frCopyDataToBuffer(VkDevice device, VkDeviceMemory dst, const void* src, uint32_t offset, uint32_t size)
{
	void* data;
	vkMapMemory(device, dst, offset, size, 0, &data);
	memcpy(data, src, size);
	vkUnmapMemory(device, dst);
}
