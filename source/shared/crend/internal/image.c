/* Copyright (c) 2016-2019 Furball Cat */

#include "vulkan.h"
#include <assert.h>
#include <stdbool.h>

#include "image.h"
#include "renderUtils.h"

#define FUR_ASSERT(x) assert(x)

void fcImageCreate(VkDevice device, VkPhysicalDevice physicalDevice, const FcImageDesc* pDesc,
					 FcImage* pImage, struct FcAllocator* pAllocCallbacks)
{
	VkImageCreateInfo imageInfo = {0};
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
	
	VkMemoryAllocateInfo allocInfo = {0};
	allocInfo.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
	allocInfo.allocationSize = memRequirements.size;
	allocInfo.memoryTypeIndex = fcRenderFindMemoryType(physicalDevice, memRequirements.memoryTypeBits, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);
	
	if (vkAllocateMemory(device, &allocInfo, NULL, &pImage->memory) != VK_SUCCESS)
	{
		FUR_ASSERT(false);
	}
	
	vkBindImageMemory(device, pImage->image, pImage->memory, 0);
	
	VkImageViewCreateInfo viewInfo = {0};
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

void fcImageRelease(VkDevice device, FcImage* pImage, struct FcAllocator* pAllocCallbacks)
{
	vkDestroyImageView(device, pImage->view, NULL);
	vkDestroyImage(device, pImage->image, NULL);
	vkFreeMemory(device, pImage->memory, NULL);
}
