/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include <inttypes.h>
#include "api.h"
	
// Render memory interface
enum fr_memory_type_t
{
	FR_MEMORY_TYPE_DEFAULT = 0
};

enum fr_memory_scope_t
{
	FR_MEMORY_SCOPE_DEFAULT = 0
};

typedef void* (*fr_memory_allocate_function_t)(void* 							pUserData,
												 size_t							size,
												 size_t							alignment,
												 enum fr_memory_scope_t		scope);

typedef void* (*fr_memory_reallocate_function_t)(void*						pUserData,
												   void*						pOriginalMemory,
												   size_t						size,
												   size_t						alignment,
												   enum fr_memory_scope_t	scope);

typedef void (*fr_memory_free_function_t)(void*	pUserData,
											void*	pMemory);

typedef void (*fr_memory_internal_allocation_notification_t)(void*						pUserData,
															  size_t					size,
															  enum fr_memory_type_t	type,
															  enum fr_memory_scope_t	scope);

typedef void (*fr_memory_internal_free_notification_t)(void*	pUserData,
														size_t	size);

struct fr_allocation_callbacks_t
{
	void* 									pUserData;
	fr_memory_allocate_function_t 				pfnAllocate;
	fr_memory_reallocate_function_t				pfnReallocate;
	fr_memory_free_function_t					pfnFree;
	fr_memory_internal_allocation_notification_t 	pfnInternalAllocate;
	fr_memory_internal_free_notification_t 		pfnInternalFree;
};

// Render result code
enum fr_result_t
{
	FR_RESULT_OK = 0,
	FR_RESULT_ERROR,
	FR_RESULT_ERROR_SHADER_MODULE_CREATION,
	FR_RESULT_ERROR_GPU,
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
									struct fr_allocation_callbacks_t* pAllocCallbacks);
	
CREND_API enum fr_result_t fr_release_app(struct fr_app_t* pApp,
									 struct fr_allocation_callbacks_t* pAllocCallbacks);

// Returns 0 on exit
CREND_API uint32_t fr_update_app(struct fr_app_t* pApp);
	
// Renderer structure
struct fr_renderer_t;

// Renderer creation description
struct fr_renderer_desc_t
{
	struct fr_app_t* pApp;
};

CREND_API enum fr_result_t fr_create_renderer(const struct fr_renderer_desc_t*	pDesc,
					   struct fr_renderer_t**						ppRenderer,
					   struct fr_allocation_callbacks_t*		pAllocCallbacks);

CREND_API enum fr_result_t fr_release_renderer(struct fr_renderer_t* 			pRenderer,
						struct fr_allocation_callbacks_t*	pAllocCallbacks);

CREND_API void fr_wait_for_device(struct fr_renderer_t* pRenderer);
	
struct fr_scene_t;
	
// Render proxy
struct fr_render_proxy_mesh_t;

struct FrRenderProxyDesc_Mesh;
	
CREND_API enum fr_result_t fr_create_render_proxy_mesh(struct fr_scene_t* pRenderScene,
										 const struct FrRenderProxyDesc_Mesh* pDesc,
										 struct fr_render_proxy_mesh_t** ppProxy);

CREND_API enum fr_result_t fr_release_render_proxy_mesh(struct fr_scene_t* pRenderScene,
										  struct fr_render_proxy_mesh_t* pProxy);

struct fr_update_context_t
{
	float dt;
};
	
CREND_API void fr_update_renderer(struct fr_renderer_t* pRenderer, const struct fr_update_context_t* ctx);
	
CREND_API void fr_draw_frame(struct fr_renderer_t* pRenderer);
	
#ifdef __cplusplus
}
#endif // __cplusplus
