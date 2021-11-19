/* Copyright (c) 2016-2020 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include <inttypes.h>
	
typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;
	
typedef struct fm_xform fm_xform;
typedef struct fm_vec4 fm_vec4;
	
typedef struct fp_physics_t fp_physics_t;
typedef struct fp_physics_scene_t fp_physics_scene_t;

CPHYSICS_API uint32_t fp_init_physics(fp_physics_t** ppPhysics, fc_alloc_callbacks_t* pAllocCallbacks);
CPHYSICS_API void fp_release_physics(fp_physics_t* pPhysics, fc_alloc_callbacks_t* pAllocCallbacks);
	
CPHYSICS_API uint32_t fp_physics_scene_create(fp_physics_t* pPhysics, fp_physics_scene_t** ppScene, fc_alloc_callbacks_t* pAllocCallbacks);
CPHYSICS_API void fp_physics_scene_release(fp_physics_t* pPhysics, fp_physics_scene_t* pScene, fc_alloc_callbacks_t* pAllocCallbacks);

typedef struct fp_physics_update_ctx_t
{
	float dt;
	fm_vec4* playerDisplacement;
} fp_physics_update_ctx_t;
	
CPHYSICS_API void fp_physics_update(fp_physics_t* pPhysics, fp_physics_scene_t* pScene, const fp_physics_update_ctx_t* pCtx);
	
typedef struct fp_physics_player_info_t
{
	fm_xform* locator;
} fp_physics_player_info_t;

CPHYSICS_API void fp_physics_get_player_info(fp_physics_t* pPhysics, fp_physics_scene_t* pScene, fp_physics_player_info_t* playerInfo);

// ----- BOUNDING VOLUME HIERARCHY -----
typedef struct fm_box fm_box;
typedef struct fp_bvh_node_t fm_bvh_node_t;

typedef struct fp_bvh_t
{
	fp_bvh_node_t* nodes;
	uint32_t numNodes;
} fm_bvh_t;

typedef struct fp_bvh_build_ctx_t
{
	// scratchpad memory to be used during build, to avoid unnecessary dynamic allocations
	void* scratchpad;
	uint32_t scratchpadSize;
	
	// boxes for leaf objects, also IDs will be based on this array
	fm_box* objectBoxes;
	uint32_t numObjects;
} fp_bvh_build_ctx_t;

CPHYSICS_API void fp_bvh_build(const fp_bvh_build_ctx_t* ctx, fp_bvh_t* bvh, fc_alloc_callbacks_t* pAllocCallbacks);
CPHYSICS_API void fp_bvh_release(fp_bvh_t* bvh, fc_alloc_callbacks_t* pAllocCallbacks);
CPHYSICS_API void fp_bvh_debug_draw(const fp_bvh_t* bvh);

#ifdef __cplusplus
}
#endif // __cplusplus
