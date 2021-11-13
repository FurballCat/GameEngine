/* Copyright (c) 2016-2019 Furball Cat */

#include "vulkansdk/macOS/include/vulkan/vulkan.h"
#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include "renderUtils.h"
#include "renderBuffer.h"
#include "image.h"

#define FUR_ASSERT(x) assert(x)

void fr_staging_init(fr_staging_buffer_builder_t* builder)
{
	memset(builder, 0, sizeof(fr_staging_buffer_builder_t));
}

void fr_staging_add(fr_staging_buffer_builder_t* builder, void* pData, uint32_t size,
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
					  struct fc_alloc_callbacks_t* pAllocCallbacks)
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
									 uint32_t* aSrcRegionIndex, VkBuffer* aDstBuffer, const VkDeviceSize* aDstOffsets, uint32_t numBuffers)
{
	for(uint32_t i=0; i<numBuffers; ++i)
	{
		const uint32_t srcIndex = aSrcRegionIndex[i];
		FUR_ASSERT(srcIndex < builder->count);
		
		const fr_staging_buffer_entry_t* entry = &builder->entries[srcIndex];
		
		VkBufferCopy copyRegion = {};
		copyRegion.srcOffset = entry->offset; // Optional
		copyRegion.dstOffset = aDstOffsets[i]; // Optional
		copyRegion.size = entry->size;
		
		vkCmdCopyBuffer(commandBuffer, stagingBuffer, aDstBuffer[i], 1, &copyRegion);
	}
}

void fr_alloc_descriptor_sets_mesh(VkDevice device, fr_alloc_descriptor_sets_mesh_ctx_t* ctx)
{
	// allocate descriptor sets - remember that the descriptorPool needs space for them
	VkDescriptorSetLayout layouts[20] = {};	// max layouts
	for(uint32_t i=0; i<ctx->numDescriptors; ++i)
	{
		layouts[i] = ctx->layout;
	}
	
	VkDescriptorSetAllocateInfo allocInfo = {};
	allocInfo.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
	allocInfo.descriptorPool = ctx->descriptorPool;
	allocInfo.descriptorSetCount = ctx->numDescriptors;
	allocInfo.pSetLayouts = layouts;
	
	if (vkAllocateDescriptorSets(device, &allocInfo, ctx->outDescriptorSets) != VK_SUCCESS)
	{
		FUR_ASSERT(false); // can't allocate descriptor sets for some reason
	}
	
	// textures
	VkDescriptorImageInfo imageInfo[20] = {};
	for(uint32_t i=0; i<ctx->numTextures; ++i)
	{
		imageInfo[i].imageLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
		imageInfo[i].imageView = ctx->textures[i].view;
		imageInfo[i].sampler = ctx->samplers[i];
	}
	
	// do writes for each of the descriptor set
	for (size_t i = 0; i < ctx->numDescriptors; ++i)
	{
		VkDescriptorBufferInfo bufferInfo[2] = {};
		bufferInfo[0].buffer = ctx->uniformBuffers[i].buffer;
		bufferInfo[0].offset = ctx->uniformBufferOffset;
		bufferInfo[0].range = ctx->uniformBufferSize;
		
		bufferInfo[1].buffer = ctx->skinningBuffers[i].buffer;	// this is just because of layout - prop does not require skinning
		bufferInfo[1].offset = ctx->skinningBufferOffset;
		bufferInfo[1].range = ctx->skinningBufferSize;
		
		const uint32_t numBindings = 3;
		VkWriteDescriptorSet descriptorWrites[numBindings] = {};
		
		descriptorWrites[0].sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
		descriptorWrites[0].dstSet = ctx->outDescriptorSets[i];
		descriptorWrites[0].dstBinding = 0;
		descriptorWrites[0].dstArrayElement = 0;
		descriptorWrites[0].descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
		descriptorWrites[0].descriptorCount = 1;
		descriptorWrites[0].pBufferInfo = &bufferInfo[0];
		descriptorWrites[0].pImageInfo = NULL; // Optional
		descriptorWrites[0].pTexelBufferView = NULL; // Optional
		
		descriptorWrites[1].sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
		descriptorWrites[1].dstSet = ctx->outDescriptorSets[i];
		descriptorWrites[1].dstBinding = 1;
		descriptorWrites[1].dstArrayElement = 0;
		descriptorWrites[1].descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
		descriptorWrites[1].descriptorCount = 1;
		descriptorWrites[1].pBufferInfo = &bufferInfo[1];
		descriptorWrites[1].pImageInfo = NULL; // Optional
		descriptorWrites[1].pTexelBufferView = NULL; // Optional
		
		descriptorWrites[2].sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
		descriptorWrites[2].dstSet = ctx->outDescriptorSets[i];
		descriptorWrites[2].dstBinding = 2;
		descriptorWrites[2].dstArrayElement = 0;
		descriptorWrites[2].descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
		descriptorWrites[2].descriptorCount = ctx->numTextures;	// number of textures in array goes here
		descriptorWrites[2].pImageInfo = imageInfo;

		vkUpdateDescriptorSets(device, numBindings, descriptorWrites, 0, NULL);
	}
}

void fr_write_descriptor_set(VkDevice device, fr_write_descriptor_set_ctx_t* ctx, VkDescriptorSet descriptor)
{
	// textures
	VkDescriptorImageInfo imageInfo[20] = {};
	for(uint32_t i=0; i<ctx->numTextures; ++i)
	{
		imageInfo[i].imageLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
		imageInfo[i].imageView = ctx->textures[i].view;
		imageInfo[i].sampler = ctx->samplers[i];
	}
	
	VkDescriptorBufferInfo bufferInfo[2] = {};
	bufferInfo[0].buffer = ctx->uniformBuffer->buffer;
	bufferInfo[0].offset = ctx->uniformBufferOffset;
	bufferInfo[0].range = ctx->uniformBufferSize;
	
	bufferInfo[1].buffer = ctx->skinningBuffer->buffer;	// this is just because of layout - prop does not require skinning
	bufferInfo[1].offset = ctx->skinningBufferOffset;
	bufferInfo[1].range = ctx->skinningBufferSize;
	
	const uint32_t numBindings = 3;
	VkWriteDescriptorSet descriptorWrites[numBindings] = {};
	
	descriptorWrites[0].sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
	descriptorWrites[0].dstSet = descriptor;
	descriptorWrites[0].dstBinding = 0;
	descriptorWrites[0].dstArrayElement = 0;
	descriptorWrites[0].descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
	descriptorWrites[0].descriptorCount = 1;
	descriptorWrites[0].pBufferInfo = &bufferInfo[0];
	descriptorWrites[0].pImageInfo = NULL; // Optional
	descriptorWrites[0].pTexelBufferView = NULL; // Optional
	
	descriptorWrites[1].sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
	descriptorWrites[1].dstSet = descriptor;
	descriptorWrites[1].dstBinding = 1;
	descriptorWrites[1].dstArrayElement = 0;
	descriptorWrites[1].descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
	descriptorWrites[1].descriptorCount = 1;
	descriptorWrites[1].pBufferInfo = &bufferInfo[1];
	descriptorWrites[1].pImageInfo = NULL; // Optional
	descriptorWrites[1].pTexelBufferView = NULL; // Optional
	
	descriptorWrites[2].sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
	descriptorWrites[2].dstSet = descriptor;
	descriptorWrites[2].dstBinding = 2;
	descriptorWrites[2].dstArrayElement = 0;
	descriptorWrites[2].descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
	descriptorWrites[2].descriptorCount = ctx->numTextures;	// number of textures in array goes here
	descriptorWrites[2].pImageInfo = imageInfo;

	vkUpdateDescriptorSets(device, numBindings, descriptorWrites, 0, NULL);
}

// --------------------

VkCommandBuffer fr_begin_simple_commands(VkDevice device, VkCommandPool commandPool, struct fc_alloc_callbacks_t* pAllocCallbacks)
{
	VkCommandBufferAllocateInfo allocInfo = {};
	allocInfo.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
	allocInfo.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
	allocInfo.commandPool = commandPool;
	allocInfo.commandBufferCount = 1;
	
	VkCommandBuffer commandBuffer;
	if(vkAllocateCommandBuffers(device, &allocInfo, &commandBuffer) != VK_SUCCESS)
	{
		FUR_ASSERT(false);
	}
	
	VkCommandBufferBeginInfo beginInfo = {};
	beginInfo.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
	beginInfo.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
	beginInfo.pInheritanceInfo = NULL; // Optional
	
	if(vkBeginCommandBuffer(commandBuffer, &beginInfo) != VK_SUCCESS)
	{
		FUR_ASSERT(false);
	}
	
	return commandBuffer;
}

void fr_end_simple_commands(VkDevice device, VkQueue graphicsQueue, VkCommandBuffer commandBuffer, VkCommandPool commandPool, struct fc_alloc_callbacks_t* pAllocCallbacks)
{
	if(vkEndCommandBuffer(commandBuffer) != VK_SUCCESS)
	{
		FUR_ASSERT(false);
	}
	
	VkSubmitInfo submitInfo = {};
	submitInfo.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
	submitInfo.commandBufferCount = 1;
	submitInfo.pCommandBuffers = &commandBuffer;
	
	vkQueueSubmit(graphicsQueue, 1, &submitInfo, VK_NULL_HANDLE);
	vkQueueWaitIdle(graphicsQueue);
	
	vkFreeCommandBuffers(device, commandPool, 1, &commandBuffer);
}

// --------------------

bool fr_has_stencil_component(VkFormat format)
{
	return format == VK_FORMAT_D32_SFLOAT_S8_UINT || format == VK_FORMAT_D24_UNORM_S8_UINT;
}

void fr_transition_image_layout(VkDevice device, VkQueue graphicsQueue, VkCommandPool commandPool, VkFormat format, VkImageLayout oldLayout, VkImageLayout newLayout, VkImage image, struct fc_alloc_callbacks_t* pAllocCallbacks)
{
	VkCommandBuffer commandBuffer = fr_begin_simple_commands(device, commandPool, pAllocCallbacks);
	
	VkImageMemoryBarrier barrier = {};
	barrier.sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
	barrier.oldLayout = oldLayout;
	barrier.newLayout = newLayout;
	
	barrier.srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
	barrier.dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
	
	barrier.image = image;
	barrier.subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
	barrier.subresourceRange.baseMipLevel = 0;
	barrier.subresourceRange.levelCount = 1;
	barrier.subresourceRange.baseArrayLayer = 0;
	barrier.subresourceRange.layerCount = 1;
	
	barrier.srcAccessMask = 0; // TODO
	barrier.dstAccessMask = 0; // TODO
	
	if (newLayout == VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL)
	{
		barrier.subresourceRange.aspectMask = VK_IMAGE_ASPECT_DEPTH_BIT;
		
		if (fr_has_stencil_component(format))
		{
			barrier.subresourceRange.aspectMask |= VK_IMAGE_ASPECT_STENCIL_BIT;
		}
	}
	else
	{
		barrier.subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
	}
	
	VkPipelineStageFlags sourceStage;
	VkPipelineStageFlags destinationStage;
	
	if (oldLayout == VK_IMAGE_LAYOUT_UNDEFINED && newLayout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL)
	{
		barrier.srcAccessMask = 0;
		barrier.dstAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
		
		sourceStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
		destinationStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
	}
	else if (oldLayout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL && newLayout == VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)
	{
		barrier.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
		barrier.dstAccessMask = VK_ACCESS_SHADER_READ_BIT;
		
		sourceStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
		destinationStage = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;
	}
	else if (oldLayout == VK_IMAGE_LAYOUT_UNDEFINED && newLayout == VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL)
	{
		barrier.srcAccessMask = 0;
		barrier.dstAccessMask = VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT | VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
		
		sourceStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
		destinationStage = VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT;
	}
	else
	{
		FUR_ASSERT(false);
	}
	
	vkCmdPipelineBarrier(
						 commandBuffer,
						 sourceStage, destinationStage,
						 0,
						 0, NULL,
						 0, NULL,
						 1, &barrier
						 );
	
	fr_end_simple_commands(device, graphicsQueue, commandBuffer, commandPool, pAllocCallbacks);
}

void fr_copy_buffer_to_image(VkDevice device, VkQueue graphicsQueue, VkCommandPool commandPool, VkBuffer buffer,
							 VkDeviceSize bufferOffset, VkImage image, uint32_t width, uint32_t height, struct fc_alloc_callbacks_t* pAllocCallbacks)
{
	VkCommandBuffer commandBuffer = fr_begin_simple_commands(device, commandPool, pAllocCallbacks);
	
	VkBufferImageCopy region = {};
	region.bufferOffset = bufferOffset;
	region.bufferRowLength = 0;
	region.bufferImageHeight = 0;
	
	region.imageSubresource.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
	region.imageSubresource.mipLevel = 0;
	region.imageSubresource.baseArrayLayer = 0;
	region.imageSubresource.layerCount = 1;
	
	region.imageOffset.x = 0;
	region.imageOffset.y = 0;
	region.imageOffset.z = 0;
	region.imageExtent.width = width;
	region.imageExtent.height = height;
	region.imageExtent.depth = 1;
	
	vkCmdCopyBufferToImage(commandBuffer, buffer, image, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, &region);
	
	fr_end_simple_commands(device, graphicsQueue, commandBuffer, commandPool, pAllocCallbacks);
}

uint32_t fr_find_memory_type(const VkPhysicalDevice physicalDevice, uint32_t typeFilter, VkMemoryPropertyFlags propertyFlags)
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
