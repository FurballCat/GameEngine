/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#include "ccore/types.h"

typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;

// desc for image creation
typedef struct fr_image_desc_t
{
	VkDeviceSize size;
	VkFormat format;
	VkBufferUsageFlags usage;
	VkMemoryPropertyFlags properties;
	u32 width;
	u32 height;
} fr_image_desc_t;

// image compact handle and data
typedef struct fr_image_t
{
	VkImage image;
	VkDeviceMemory memory;
	VkImageView view;
	VkDeviceSize size;
} fr_image_t;

// image creation, allocates memory
void fr_image_create(VkDevice device, VkPhysicalDevice physicalDevice, const fr_image_desc_t* pDesc,
					  fr_image_t* pImage, fc_alloc_callbacks_t* pAllocCallbacks);

// image release, deallocates memory
void fr_image_release(VkDevice device, fr_image_t* pImage, struct fc_alloc_callbacks_t* pAllocCallbacks);
