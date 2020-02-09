/* Copyright (c) 2016-2019 Furball Cat */

#include "vulkansdk/macOS/include/vulkan/vulkan.h"
#include <assert.h>
#include <stdbool.h>

#include "image.h"
#include "renderUtils.h"

#define FUR_ASSERT(x) assert(x)

void fr_image_create(VkDevice device, VkPhysicalDevice physicalDevice, const fr_image_desc_t* pDesc,
					 fr_image_t* pImage, struct fc_alloc_callbacks_t* pAllocCallbacks)
{
	VkImageCreateInfo imageInfo = {};
	imageInfo.sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
	imageInfo.imageType = VK_IMAGE_TYPE_2D;
	imageInfo.extent.width = pDesc->width;
	imageInfo.extent.height = pDesc->height;
	imageInfo.extent.depth = 1;
	imageInfo.mipLevels = 1;
	imageInfo.arrayLayers = 1;
	
	imageInfo.format = pDesc->format;
	imageInfo.tiling = VK_IMAGE_TILING_OPTIMAL;
	imageInfo.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
	imageInfo.usage = pDesc->usage;
	imageInfo.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
	imageInfo.samples = VK_SAMPLE_COUNT_1_BIT;
	imageInfo.flags = 0; // Optional
	
	if (vkCreateImage(device, &imageInfo, NULL, &pImage->image) != VK_SUCCESS)
	{
		FUR_ASSERT(false);
	}
	
	VkMemoryRequirements memRequirements;
	vkGetImageMemoryRequirements(device, pImage->image, &memRequirements);
	
	VkMemoryAllocateInfo allocInfo = {};
	allocInfo.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
	allocInfo.allocationSize = memRequirements.size;
	allocInfo.memoryTypeIndex = fr_find_memory_type(physicalDevice, memRequirements.memoryTypeBits, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);
	
	if (vkAllocateMemory(device, &allocInfo, NULL, &pImage->memory) != VK_SUCCESS)
	{
		FUR_ASSERT(false);
	}
	
	vkBindImageMemory(device, pImage->image, pImage->memory, 0);
	
	VkImageViewCreateInfo viewInfo = {};
	viewInfo.sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
	viewInfo.image = pImage->image;
	viewInfo.viewType = VK_IMAGE_VIEW_TYPE_2D;
	viewInfo.format = pDesc->format;
	viewInfo.subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
	viewInfo.subresourceRange.baseMipLevel = 0;
	viewInfo.subresourceRange.levelCount = 1;
	viewInfo.subresourceRange.baseArrayLayer = 0;
	viewInfo.subresourceRange.layerCount = 1;
	
	if(vkCreateImageView(device, &viewInfo, NULL, &pImage->view) != VK_SUCCESS)
	{
		FUR_ASSERT(false);
	}
}

void fr_image_release(VkDevice device, fr_image_t* pImage, struct fc_alloc_callbacks_t* pAllocCallbacks)
{
	vkDestroyImageView(device, pImage->view, NULL);
	vkDestroyImage(device, pImage->image, NULL);
	vkFreeMemory(device, pImage->memory, NULL);
}
