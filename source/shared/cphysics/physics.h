/* Copyright (c) 2016-2020 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include "ccore/types.h"
#include "cmath/mathtypes.h"
	
typedef struct FcAllocator FcAllocator;
typedef struct FcMemArenaAllocator FcMemArenaAllocator;
	
typedef struct fm_xform fm_xform;
typedef struct fm_vec4 fm_vec4;
	
typedef struct FcPhysics FcPhysics;
typedef struct FcPhysicsScene FcPhysicsScene;

CPHYSICS_API FcResult fcCreatePhysics(const FcAllocator* allocator, FcPhysics** physics);
CPHYSICS_API void fcPhysicsRelease(FcPhysics* physics, const FcAllocator* allocator);

CPHYSICS_API void fcPhysicsAddStaticBox(FcPhysics* physics, const fm_xform* worldLocation,
											const fm_vec3* halfExtents, const FcAllocator* allocator);

typedef struct FcPhysicsUpdateCtx
{
	f32 dt;
	fm_vec4* playerDisplacement;
} FcPhysicsUpdateCtx;
	
CPHYSICS_API void fcPhysicsUpdate(FcPhysics* physics, const FcPhysicsUpdateCtx* pCtx);
	
typedef struct FcPhysicsPlayerInfo
{
	fm_xform* locator;
} FcPhysicsPlayerInfo;

CPHYSICS_API void fcPhysicsGetPlayerInfo(FcPhysics* physics, FcPhysicsPlayerInfo* playerInfo);

typedef struct FcRaycastHit
{
	fm_vec4 pos;
	f32 distance;
} FcRaycastHit;

CPHYSICS_API bool fcPhysicsRaycast(FcPhysics* physics, const fm_vec4* start, const fm_vec4* dir,
									 f32 distance, FcRaycastHit* hit);

// ----- BOUNDING VOLUME HIERARCHY -----
typedef struct fm_box fm_box;
typedef struct FcBoundingVolumeHierarchyNode FcBoundingVolumeHierarchyNode;

typedef struct FcBoundingVolumeHierarchy
{
	FcBoundingVolumeHierarchyNode* nodes;
	u32 numNodes;
} FcBoundingVolumeHierarchy;

typedef struct FcBoundingVolumeHierarchyDesc
{
	// scratchpad memory to be used during build, to avoid unnecessary dynamic allocations
	FcMemArenaAllocator* arenaAlloc;
	
	// boxes for leaf objects, also IDs will be based on this array
	fm_box* objectBoxes;
	u32 numObjects;
} FcBoundingVolumeHierarchyDesc;

CPHYSICS_API void fcBoundingVolumeHierarchyCreate(const FcBoundingVolumeHierarchyDesc* ctx, FcBoundingVolumeHierarchy* bvh, const FcAllocator* allocator);
CPHYSICS_API void fcBoundingVolumeHiearchyRelease(FcBoundingVolumeHierarchy* bvh, const FcAllocator* allocator);
CPHYSICS_API void fcBoundingVolumeHiearchyDebugDraw(const FcBoundingVolumeHierarchy* bvh);

#ifdef __cplusplus
}
#endif // __cplusplus
