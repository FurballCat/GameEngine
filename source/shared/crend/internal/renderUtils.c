/* Copyright (c) 2016-2019 Furball Cat */

#include "vulkansdk/macOS/include/vulkan/vulkan.h"
#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include "renderUtils.h"
#include "renderBuffer.h"

#define FUR_ASSERT(x) assert(x)

void fr_staging_init(fr_staging_buffer_builder_t* builder)
{
	memset(builder, 0, sizeof(fr_staging_buffer_builder_t));
}

void fr_staging_add(fr_staging_buffer_builder_t* builder, const void* pData, uint32_t size,
					void* pUserData, fr_staging_free_data_func_t fnFree)
{
	FUR_ASSERT(builder);
	FUR_ASSERT(pData);
	FUR_ASSERT(builder->count < FR_STAGIN_BUFFER_MAX_COUNT);
	
	fr_staging_buffer_entry_t* entry = &builder->entries[builder->count];
	
	// buffer info & data
	entry->pData = pData;
	entry->size = size;
	entry->offset = builder->totalSize;
	
	// data release callback
	entry->pUserData = pUserData;
	entry->fnFree = fnFree;
	
	// staging buffer info
	builder->totalSize += size;
	builder->count += 1;
}

void fr_staging_build(fr_staging_buffer_builder_t* builder,
					  VkDevice device, VkPhysicalDevice physicalDevice,
					  VkBuffer* buffer, VkDeviceMemory* bufferMemory,
					  struct fr_allocation_callbacks_t* pAllocCallbacks)
{
	fr_create_buffer(device, physicalDevice, builder->totalSize,
					 FR_STAGING_BUFFER_USAGE_FLAGS, FR_STAGING_BUFFER_MEMORY_FLAGS,
					 buffer, bufferMemory, pAllocCallbacks);
	
	uint32_t currentSize = 0;
	
	for(uint32_t i=0; i<builder->count; ++i)
	{
		fr_staging_buffer_entry_t* entry = &builder->entries[i];
		
		FUR_ASSERT(entry->pData);
		fr_copy_data_to_buffer(device, *bufferMemory, entry->pData, currentSize, entry->size);
		
		currentSize += entry->size;
	}
	
	FUR_ASSERT(currentSize == builder->totalSize);
}

void fr_staging_release_builder(fr_staging_buffer_builder_t* builder)
{
	for(uint32_t i=0; i<builder->count; ++i)
	{
		fr_staging_buffer_entry_t* entry = &builder->entries[i];
		
		if(entry->fnFree)
		{
			entry->fnFree(entry->pData, entry->size, entry->pUserData);
			entry->pData = NULL;
		}
	}
}

void fr_staging_record_copy_commands(fr_staging_buffer_builder_t* builder, VkCommandBuffer commandBuffer, VkBuffer stagingBuffer,
									 uint32_t* aSrcRegionIndex, VkBuffer* aDstBuffer, uint32_t numBuffers)
{
	for(uint32_t i=0; i<numBuffers; ++i)
	{
		const uint32_t srcIndex = aSrcRegionIndex[i];
		const fr_staging_buffer_entry_t* entry = &builder->entries[srcIndex];
		
		VkBufferCopy copyRegion = {};
		copyRegion.srcOffset = entry->offset; // Optional
		copyRegion.dstOffset = 0; // Optional
		copyRegion.size = entry->size;
		
		vkCmdCopyBuffer(commandBuffer, stagingBuffer, aDstBuffer[i], 1, &copyRegion);
	}
}
