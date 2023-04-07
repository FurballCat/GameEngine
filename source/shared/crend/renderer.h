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

// Render result code
enum FcResult
{
	FR_RESULT_OK = 0,
	FR_RESULT_ERROR,
	FR_RESULT_ERROR_SHADER_MODULE_CREATION,
	FR_RESULT_ERROR_GPU,
	FR_RESULT_PHYSICS_INIT_ERROR,
};
	
// If render result code is not OK then this function returns additional info
CREND_API const char* fcGetLastError(void);

typedef struct FcApplication FcApplication;

typedef struct FcApplicationDesc
{
	u32 viewportWidth;
	u32 viewportHeight;
	const char* appTitle;

	FcDepot* depot;
	FcFilePath iconPath;
} FcApplicationDesc;
	
CREND_API enum FcResult fcApplicationCreate(const FcApplicationDesc* pDesc,
									FcApplication** ppApp,
									FcAllocator* allocator);
	
CREND_API enum FcResult fcApplicationRelease(FcApplication* pApp,
									 FcAllocator* allocator);

// Returns 0 on exit
CREND_API u32 fcApplicationUpdate(struct FcApplication* pApp);
	
// Renderer structure
typedef struct FcRenderer FcRenderer;

// Renderer creation description
typedef struct FcRendererDesc
{
	struct FcApplication* pApp;
	FcDepot* depot;
} FcRendererDesc;

CREND_API enum FcResult fcRendererCreate(const FcRendererDesc*	pDesc,
					   FcRenderer**						ppRenderer,
					   FcAllocator*		allocator);

CREND_API enum FcResult fcRendererRelease(struct FcRenderer* 			pRenderer,
						struct FcAllocator*	allocator);

CREND_API void fcRendererWaitForDevice(struct FcRenderer* pRenderer);
	
struct fr_scene_t;
	
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
CREND_API FcRenderProxy* fcRendererLoadMesh(FcRenderer* pRenderer, FcDepot* depot, const FcRenderMeshLoadCtx* ctx, FcAllocator* allocator);

// release proxy, might also release the associated data (meshes, textures, etc.)
CREND_API void fcRendererReleaseProxy(FcRenderer* pRenderer, FcRenderProxy* proxy, FcAllocator* allocator);

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
	
CREND_API void fcRendererDrawFrame(struct FcRenderer* pRenderer, const FcRendererDrawFrameCtx* ctx, FcAllocator* allocator);

#ifdef __cplusplus
}
#endif // __cplusplus
