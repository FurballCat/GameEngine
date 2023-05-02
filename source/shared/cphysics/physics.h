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
CPHYSICS_API void fcDestroyPhysics(FcPhysics* physics, const FcAllocator* allocator);

CPHYSICS_API void fcPhysicsAddStaticBox(FcPhysics* physics, const fm_xform* worldLocation,
											const fm_vec3* halfExtents, const FcAllocator* allocator);

typedef struct FcPhysicsUpdateCtx
{
	f32 dt;
} FcPhysicsUpdateCtx;
	
CPHYSICS_API void fcPhysicsUpdate(FcPhysics* physics, const FcPhysicsUpdateCtx* pCtx);

// character controller API
typedef struct FcCreateCapsuleControllerInfo
{
	fm_vec3 initPosition;
	f32 height;
	f32 radius;
} FcCreateCapsuleControllerInfo;

typedef struct FcCapsuleController FcCapsuleController;

CPHYSICS_API void fcCreateCapsuleController(FcPhysics* physics, const FcCreateCapsuleControllerInfo* info,
	const FcAllocator* allocator, FcCapsuleController** outController);
CPHYSICS_API void fcDestroyCapsuleController(FcPhysics* physics, const FcAllocator* allocator, FcCapsuleController* controller);

CPHYSICS_API void fcCapsuleControllerMove(FcCapsuleController* controller, const fm_vec3* displacement, f32 dt);
CPHYSICS_API void fcCapsuleControllerGetLocator(const FcCapsuleController* controller, fm_xform* locator);

// raycast API
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
