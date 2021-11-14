/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include <inttypes.h>
#include "api.h"

typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;
typedef struct fm_mat4 fm_mat4;

// Render result code
enum fr_result_t
{
	FR_RESULT_OK = 0,
	FR_RESULT_ERROR,
	FR_RESULT_ERROR_SHADER_MODULE_CREATION,
	FR_RESULT_ERROR_GPU,
	FR_RESULT_PHYSICS_INIT_ERROR,
};
	
// If render result code is not OK then this function returns additional info
CREND_API const char* fr_get_last_error(void);

struct fr_app_t;

struct fr_app_desc_t
{
	uint32_t viewportWidth;
	uint32_t viewportHeight;
	const char* appTitle;
};
	
CREND_API enum fr_result_t fr_create_app(const struct fr_app_desc_t* pDesc,
									struct fr_app_t** ppApp,
									struct fc_alloc_callbacks_t* pAllocCallbacks);
	
CREND_API enum fr_result_t fr_release_app(struct fr_app_t* pApp,
									 struct fc_alloc_callbacks_t* pAllocCallbacks);

// Returns 0 on exit
CREND_API uint32_t fr_update_app(struct fr_app_t* pApp);
	
// Renderer structure
typedef struct fr_renderer_t fr_renderer_t;

// Renderer creation description
struct fr_renderer_desc_t
{
	struct fr_app_t* pApp;
};

CREND_API enum fr_result_t fr_create_renderer(const struct fr_renderer_desc_t*	pDesc,
					   struct fr_renderer_t**						ppRenderer,
					   struct fc_alloc_callbacks_t*		pAllocCallbacks);

CREND_API enum fr_result_t fr_release_renderer(struct fr_renderer_t* 			pRenderer,
						struct fc_alloc_callbacks_t*	pAllocCallbacks);

CREND_API void fr_wait_for_device(struct fr_renderer_t* pRenderer);
	
struct fr_scene_t;
	
// render proxy - can be a mesh, a particle system, anything that can be rendered and has position
typedef struct fr_proxy_t fr_proxy_t;

typedef struct fi_depot_t fi_depot_t;

typedef struct fr_load_mesh_ctx_t
{
	const char* path;
	
	const int32_t* textureIndices;
	uint32_t numTextures;
	
	const char** texturePaths;
} fr_load_mesh_ctx_t;

// load mesh, the ownership is kept inside renderer, so no need to
CREND_API fr_proxy_t* fr_load_mesh(fr_renderer_t* pRenderer, const fi_depot_t* depot, const fr_load_mesh_ctx_t* ctx, fc_alloc_callbacks_t* pAllocCallbacks);

// release proxy, might also release the associated data (meshes, textures, etc.)
CREND_API void fr_release_proxy(fr_renderer_t* pRenderer, fr_proxy_t* proxy, fc_alloc_callbacks_t* pAllocCallbacks);

// potentially visible set - defines render proxies that are visible this frame
typedef struct fr_pvs_t fr_pvs_t;

// at frame update, acquire PVS and relink proxies to it, keep it with frame data
CREND_API fr_pvs_t* fr_acquire_free_pvs(fr_renderer_t* pRenderer, const fm_mat4* camera);

// add renderable thing to given potentially visible set
CREND_API void fr_pvs_add(fr_pvs_t* pvs, fr_proxy_t* proxy, const fm_mat4* locator);

// add renderable thing to given potentially visible set and pass skinning matrices for it
CREND_API void fr_pvs_add_and_skin(fr_pvs_t* pvs, fr_proxy_t* proxy, const fm_mat4* locator, const fm_mat4* skinMatrices);

typedef struct fr_draw_frame_context_t
{
	const fm_mat4* skinMatrices;
	uint32_t numSkinMatrices;
	
	const fm_mat4* zeldaMatrix;
	const fm_mat4* propMatrix;
	
	fr_pvs_t* pvs;	// what's visible in this frame
} fr_draw_frame_context_t;
	
CREND_API void fr_draw_frame(struct fr_renderer_t* pRenderer, const fr_draw_frame_context_t* ctx, fc_alloc_callbacks_t* pAllocCallbacks);

CREND_API void fr_temp_create_skinning_mapping(struct fr_renderer_t* pRenderer, const uint32_t* boneNameHashes, uint32_t numBones, fc_alloc_callbacks_t* pAllocCallbacks);
	
#ifdef __cplusplus
}
#endif // __cplusplus
