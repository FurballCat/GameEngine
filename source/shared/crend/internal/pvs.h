/* Copyright (c) 2016-2021 Furball Cat */

#pragma once

#include <inttypes.h>
#include "cmath/mathtypes.h"
#include "renderBuffer.h"

typedef struct FcRenderBuffer FcRenderBuffer;
typedef struct FcRenderMesh FcRenderMesh;
typedef struct FcImage FcImage;
typedef struct FcMeshResource FcMeshResource;

#define FUR_MAX_SKIN_MATRICES_IN_BUFFER 16384	// 32 * 512

typedef struct FcRenderProxy
{
	FcRenderMesh* mesh;
	FcImage* textures;
	u32 numTextures;
	
	// only for skinned meshes
	fm_mat4* invBindPose;
	int16_t* skinningMappinng;
	i32 numBones;
} FcRenderProxy;

typedef enum FcRenderPVSFloat
{
	FR_PVS_PROXY_FLAG_NONE = 0,
	FR_PVS_PROXY_FLAG_SKINNED = 0x1,
} FcRenderPVSFloat;

// potentially visible set - collects render proxies that are visible this frame
// the public API is in renderer.h
typedef struct FcRenderPVS
{
	// GPU resources assigned to this PVS
	VkDevice device;					// required to write to buffers and descriptor sets
	FcRenderBuffer worldViewProj;			// world, view, and projection matrices for all objects
	FcRenderBuffer skinningBuffer;			// skinning bones for all skinned meshes
	VkDescriptorSet* descriptorSets;	// there's limited amount of proxies that can be added to single PVS
	u32 numMaxDescriptorSets;		// this is equal to maxumum number of proxies in PVS
	VkSampler defaultTextureSampler;
	
	// constants given this frame
	fm_mat4 view;
	fm_mat4 projection;
	fm_mat4 camera;			// view and projection is created based on that, it's initiated when acquiring the PVS
	
	// values modified with adding proxies
	u32 worldViewProjOffset;	// adding proxies moves the offset
	u32 numSkinningMatrices;	// same here, adding skinned proxies moves the number of skinned matrices, then mapped to buffer offset
	fm_mat4* skinningMatrices;		// all the matrices for all skinned objects
	
	const FcRenderProxy** proxies;		// collection of all proxies visible this frame
	u32* proxiesFlags;			// flags like - is skinned
	u32 numProxies;			// this is the current number of proxies added to PVS during frame
	
	u32 pvsIndex;	// used for tripple buffering
} FcRenderPVS;

void fcRenderPVSClear(FcRenderPVS* pvs);
