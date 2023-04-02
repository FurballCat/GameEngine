/* Copyright (c) 2016-2019 Furball Cat */

#include "vulkan.h"
#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include "pvs.h"
#include "renderUtils.h"
#include "cmath/public.h"

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
	fm_mat4* bones;
} fr_skinning_buffer_t;

// add renderable thing to given potentially visible set and pass skinning matrices for it
void fcRenderPVSAddAndSkin(FcRenderPVS* pvs, FcRenderProxy* proxy, const fm_mat4* locator, const fm_mat4* skinMatrices, i32 numSkinMatrices)
{
	FUR_ASSERT(pvs->numProxies < pvs->numMaxDescriptorSets);
	
	VkDevice device = pvs->device;
	
	FUR_ASSERT(locator);
	
	fr_world_view_proj_t ubo = {0};
	ubo.world = *locator;
	ubo.view = pvs->view;
	ubo.proj = pvs->projection;
	
	const u64 size = sizeof(fr_world_view_proj_t);
	const u32 offset = pvs->worldViewProjOffset;
	const u32 descriptorIndex = pvs->numProxies;
	
	pvs->worldViewProjOffset += size;
	pvs->numProxies += 1;
	
	fcRenderCopyDataToBuffer(device, pvs->worldViewProj.memory, &ubo, offset, size);
	
	pvs->proxies[descriptorIndex] = proxy;
	
	FcRenderWriteDescriptorSetsCtx desc = {0};
	desc.uniformBuffer = &pvs->worldViewProj;
	desc.uniformBufferSize = size;
	desc.uniformBufferOffset = offset;
	
	FUR_ASSERT(proxy->numTextures < 20);
	VkSampler samplers[20] = {0};
	for(u32 i=0; i<20; ++i)
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
		desc.skinningBufferSize = proxy->numBones * sizeof(fm_mat4);
		desc.skinningBufferOffset = pvs->numSkinningMatrices * sizeof(fm_mat4);
		
		// copy matrices into temporary CPU buffer with all skinning matrices
		FUR_ASSERT(pvs->numSkinningMatrices + numSkinMatrices < FUR_MAX_SKIN_MATRICES_IN_BUFFER);
		
		const fm_mat4* invBindPose = proxy->invBindPose;
		FUR_ASSERT(invBindPose);
		
		fm_mat4* skinBufferMatrices = pvs->skinningMatrices + pvs->numSkinningMatrices;
		
		FUR_ASSERT(proxy->numBones <= numSkinMatrices); // make sure we have enough matrices to choose from
		
		for(u32 i=0; i<proxy->numBones; ++i)
		{
			const int16_t srcBoneIndex = proxy->skinningMappinng[i];
			fm_mat4_mul(&invBindPose[i], &skinMatrices[srcBoneIndex], &skinBufferMatrices[i]);
		}
		
		pvs->numSkinningMatrices += numSkinMatrices;
		
		pvs->proxiesFlags[descriptorIndex] |= FR_PVS_PROXY_FLAG_SKINNED;
	}
	
	fcRenderWriteDescriptorSets(device, &desc, pvs->descriptorSets[descriptorIndex]);
}

// add renderable thing to given potentially visible set
void fcRenderPVSAdd(FcRenderPVS* pvs, FcRenderProxy* proxy, const fm_mat4* locator)
{
	fcRenderPVSAddAndSkin(pvs, proxy, locator, NULL, 0);
}

void fcRenderPVSClear(FcRenderPVS* pvs)
{
	pvs->numProxies = 0;
	pvs->numSkinningMatrices = 0;
	pvs->worldViewProjOffset = 0;
}
