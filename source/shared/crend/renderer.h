/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include "ccore/types.h"

typedef struct FcAllocator FcAllocator;
typedef struct fm_mat4 fm_mat4;
typedef u32 FcStringId;
typedef struct FcDepot FcDepot;
typedef u64 FcFilePath;
	
// If render result code is not OK then this function returns additional info
CREND_API const char* fcGetLastError(void);

typedef struct FcApplication FcApplication;

typedef struct FcApplicationCreateInfo
{
	u32 viewportWidth;
	u32 viewportHeight;
	const char* appTitle;

	FcDepot* depot;
	FcFilePath iconPath;
} FcApplicationCreateInfo;
	
CREND_API FcResult fcCreateApplication(const FcApplicationCreateInfo* pDesc, const FcAllocator* allocator, FcApplication** ppApp);
	
CREND_API FcResult fcDestroyApplication(FcApplication* pApp, const FcAllocator* allocator);

// Returns 0 on exit
CREND_API u32 fcApplicationUpdate(FcApplication* pApp);
	
// Renderer structure
typedef struct FcRenderer FcRenderer;

// Renderer creation description
typedef struct FcRendererCreateInfo
{
	struct FcApplication* pApp;
	FcDepot* depot;
} FcRendererCreateInfo;

CREND_API FcResult fcCreateRenderer(const FcRendererCreateInfo* pDesc, const FcAllocator* allocator, FcRenderer** ppRenderer);
CREND_API FcResult fcDestroyRenderer(FcRenderer* pRenderer, const FcAllocator* allocator);

CREND_API void fcRendererWaitForDevice(FcRenderer* pRenderer);

// render proxy - can be a mesh, a particle system, anything that can be rendered and has position
typedef struct FcRenderProxy FcRenderProxy;

typedef u64 FcFilePath;

typedef struct FcRenderMeshLoadCtx
{
	FcFilePath path;

	const i32* textureIndices;
	i32 numTextureIndices;
	
	FcFilePath* texturePaths;
	u32 numTextures;
	
	bool isSkinned;
	FcStringId* boneNames;
	i32 numBones;	// rig num bones might be higher than mesh num bones
} FcRenderMeshLoadCtx;

// load mesh, the ownership is kept inside renderer, so no need to
CREND_API FcRenderProxy* fcRendererLoadMesh(FcRenderer* pRenderer, FcDepot* depot, const FcRenderMeshLoadCtx* ctx, const FcAllocator* allocator);

// release proxy, might also release the associated data (meshes, textures, etc.)
CREND_API void fcRendererReleaseProxy(FcRenderer* pRenderer, FcRenderProxy* proxy, const FcAllocator* allocator);

// potentially visible set - defines render proxies that are visible this frame
typedef struct FcRenderPVS FcRenderPVS;

// at frame update, acquire PVS and relink proxies to it, keep it with frame data
CREND_API FcRenderPVS* fcRendererAcquireFreePVS(FcRenderer* pRenderer, const fm_mat4* camera, f32 fov);

// add renderable thing to given potentially visible set
CREND_API void fcRenderPVSAdd(FcRenderPVS* pvs, FcRenderProxy* proxy, const fm_mat4* locator);

// add renderable thing to given potentially visible set and pass skinning matrices for it
CREND_API void fcRenderPVSAddAndSkin(FcRenderPVS* pvs, FcRenderProxy* proxy, const fm_mat4* locator,
								   const fm_mat4* skinMatrices, i32 numSkinMatrices);

typedef struct FcRendererDrawFrameCtx
{
	FcRenderPVS* pvs;	// what's visible in this frame
} FcRendererDrawFrameCtx;
	
CREND_API void fcRendererDrawFrame(struct FcRenderer* pRenderer, const FcRendererDrawFrameCtx* ctx, const FcAllocator* allocator);

#ifdef __cplusplus
}
#endif // __cplusplus
