/* Copyright (c) 2016-2021 Furball Cat */

#pragma once

#include <inttypes.h>
#include "cmath/mathtypes.h"
#include "renderBuffer.h"

typedef struct fr_buffer_t fr_buffer_t;
typedef struct fr_mesh_t fr_mesh_t;
typedef struct fr_image_t fr_image_t;
typedef struct fr_resource_mesh_t fr_resource_mesh_t;

#define FUR_MAX_SKIN_MATRICES_IN_BUFFER 16384	// 32 * 512

typedef struct fr_proxy_t
{
	fr_mesh_t* mesh;
	fr_image_t* textures;
	uint32_t numTextures;
	
	// only for skinned meshes
	fm_mat4* invBindPose;
	int16_t* skinningMappinng;
	int32_t numBones;
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
	fm_mat4 camera;			// view and projection is created based on that, it's initiated when acquiring the PVS
	
	// values modified with adding proxies
	uint32_t worldViewProjOffset;	// adding proxies moves the offset
	uint32_t numSkinningMatrices;	// same here, adding skinned proxies moves the number of skinned matrices, then mapped to buffer offset
	fm_mat4* skinningMatrices;		// all the matrices for all skinned objects
	
	const fr_proxy_t** proxies;		// collection of all proxies visible this frame
	uint32_t* proxiesFlags;			// flags like - is skinned
	uint32_t numProxies;			// this is the current number of proxies added to PVS during frame
	
	uint32_t pvsIndex;	// used for tripple buffering
} fr_pvs_t;

void fr_pvs_clear(fr_pvs_t* pvs);
