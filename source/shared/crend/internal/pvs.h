/* Copyright (c) 2016-2021 Furball Cat */

#pragma once

#include <inttypes.h>
#include "cmath/public.h"
#include "renderBuffer.h"

typedef struct fr_buffer_t fr_buffer_t;
typedef struct fr_mesh_t fr_mesh_t;
typedef struct fr_image_t fr_image_t;

typedef struct fr_proxy_t
{
	fr_mesh_t* mesh;
	fr_image_t* textures;
	uint32_t numTextures;
} fr_proxy_t;

typedef enum fr_pvs_proxy_flag_t
{
	FR_PVS_PROXY_FLAG_NONE = 0,
	FR_PVS_PROXY_FLAG_SKINNED = 0x1,
} fr_pvs_proxy_flag_t;

// potentially visible set - collects render proxies that are visible this frame
// the public API is in renderer.h
typedef struct fr_pvs_t
{
	// GPU resources assigned to this PVS
	VkDevice device;					// required to write to buffers and descriptor sets
	fr_buffer_t worldViewProj;			// world, view, and projection matrices for all objects
	fr_buffer_t skinningBuffer;			// skinning bones for all skinned meshes
	VkDescriptorSet* descriptorSets;	// there's limited amount of proxies that can be added to single PVS
	uint32_t numMaxDescriptorSets;		// this is equal to maxumum number of proxies in PVS
	VkSampler defaultTextureSampler;
	
	// constants given this frame
	fm_mat4 view;
	fm_mat4 projection;
	
	// values modified with adding proxies
	uint32_t worldViewProjOffset;		// adding proxies moves the offset
	size_t skinningBufferOffset;	// same here, adding skinned proxies moves the offset in skinning buffer
	
	const fr_proxy_t** proxies;			// collection of all proxies visible this frame
	uint32_t* proxiesFlags;			// flags like - is skinned
	uint32_t numProxies;			// this is the current number of proxies added to PVS during frame
} fr_pvs_t;
