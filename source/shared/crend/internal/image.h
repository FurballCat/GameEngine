/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#include "ccore/types.h"

typedef struct FcAllocator FcAllocator;

// desc for image creation
typedef struct FcImageDesc
{
	VkDeviceSize size;
	VkFormat format;
	VkBufferUsageFlags usage;
	VkMemoryPropertyFlags properties;
	u32 width;
	u32 height;
} FcImageDesc;

// image compact handle and data
typedef struct FcImage
{
	VkImage image;
	VkDeviceMemory memory;
	VkImageView view;
	VkDeviceSize size;
} FcImage;

// image creation, allocates memory
void fcImageCreate(VkDevice device, VkPhysicalDevice physicalDevice, const FcImageDesc* pDesc,
					  FcImage* pImage, FcAllocator* pAllocCallbacks);

// image release, deallocates memory
void fcImageRelease(VkDevice device, FcImage* pImage, struct FcAllocator* pAllocCallbacks);
