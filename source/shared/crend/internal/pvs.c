/* Copyright (c) 2016-2019 Furball Cat */

#include "vulkansdk/macOS/include/vulkan/vulkan.h"
#include <assert.h>
#include <stdbool.h>

#include "pvs.h"
#include "renderUtils.h"

#define FUR_ASSERT(x) assert(x)

typedef struct fr_world_view_proj_t
{
	fm_mat4 world;
	fm_mat4 view;
	fm_mat4 proj;
	fm_mat4 padding;	// when using offset for buffers, it has to be a multiple of 256
} fr_world_view_proj_t;

typedef struct fr_skinning_buffer_t
{
	fm_mat4_t bones[512];
} fr_skinning_buffer_t;

// add renderable thing to given potentially visible set and pass skinning matrices for it
void fr_pvs_add_and_skin(fr_pvs_t* pvs, fr_proxy_t* proxy, const fm_mat4* locator, const fm_mat4* skinMatrices)
{
	FUR_ASSERT(pvs->numProxies < pvs->numMaxDescriptorSets);
	
	VkDevice device = pvs->device;
	
	FUR_ASSERT(locator);
	
	fr_world_view_proj_t ubo = {};
	ubo.world = *locator;
	ubo.view = pvs->view;
	ubo.proj = pvs->projection;
	
	const size_t size = sizeof(fr_world_view_proj_t);
	const uint32_t offset = pvs->worldViewProjOffset;
	const uint32_t descriptorIndex = pvs->numProxies;
	
	pvs->worldViewProjOffset += size;
	pvs->numProxies += 1;
	
	fr_copy_data_to_buffer(device, pvs->worldViewProj.memory, &ubo, offset, size);
	
	pvs->proxies[descriptorIndex] = proxy;
	
	fr_write_descriptor_set_ctx_t desc = {};
	desc.uniformBuffer = &pvs->worldViewProj;
	desc.uniformBufferSize = size;
	desc.uniformBufferOffset = offset;
	
	FUR_ASSERT(proxy->numTextures < 20);
	VkSampler samplers[20] = {};
	for(uint32_t i=0; i<20; ++i)
	{
		samplers[i] = pvs->defaultTextureSampler;
	}
	
	desc.samplers = samplers;
	desc.numTextures = proxy->numTextures;
	desc.textures = proxy->textures;
	
	// skinning is optional
	if(skinMatrices != NULL)
	{
		desc.skinningBuffer = &pvs->skinningBuffer;
		desc.skinningBufferSize = sizeof(fr_skinning_buffer_t);
		desc.skinningBufferOffset = pvs->skinningBufferOffset;
		
		pvs->skinningBufferOffset += sizeof(fr_skinning_buffer_t);
		pvs->proxiesFlags[descriptorIndex] |= FR_PVS_PROXY_FLAG_SKINNED;
	}
	
	fr_write_descriptor_set(device, &desc, pvs->descriptorSets[descriptorIndex]);
}

// add renderable thing to given potentially visible set
void fr_pvs_add(fr_pvs_t* pvs, fr_proxy_t* proxy, const fm_mat4* locator)
{
	fr_pvs_add_and_skin(pvs, proxy, locator, NULL);
}
