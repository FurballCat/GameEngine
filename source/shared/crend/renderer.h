/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include <inttypes.h>
#include "api.h"
	
// Render memory interface
enum FrMemoryType
{
	FR_MEMORY_TYPE_DEFAULT = 0
};

enum FrMemoryScope
{
	FR_MEMORY_SCOPE_DEFAULT = 0
};

typedef void* (*FrMemoryAllocateFunction)(void* 							pUserData,
												 size_t							size,
												 size_t							alignment,
												 enum FrMemoryScope		scope);

typedef void* (*FrMemoryReallocateFunction)(void*						pUserData,
												   void*						pOriginalMemory,
												   size_t						size,
												   size_t						alignment,
												   enum FrMemoryScope	scope);

typedef void (*FrMemoryFreeFunction)(void*	pUserData,
											void*	pMemory);

typedef void (*FrMemoryInternalAllocationNotification)(void*						pUserData,
															  size_t					size,
															  enum FrMemoryType	type,
															  enum FrMemoryScope	scope);

typedef void (*FrMemoryInternalFreeNotification)(void*	pUserData,
														size_t	size);

struct FrAllocationCallbacks
{
	void* 									pUserData;
	FrMemoryAllocateFunction 				pfnAllocate;
	FrMemoryReallocateFunction				pfnReallocate;
	FrMemoryFreeFunction					pfnFree;
	FrMemoryInternalAllocationNotification 	pfnInternalAllocate;
	FrMemoryInternalFreeNotification 		pfnInternalFree;
};

// Render result code
enum FrResult
{
	FR_RESULT_OK = 0,
	FR_RESULT_ERROR,
	FR_RESULT_ERROR_SHADER_MODULE_CREATION,
	FR_RESULT_ERROR_GPU,
};
	
// If render result code is not OK then this function returns additional info
CREND_API const char* frGetLastError(void);

struct FrApp;

struct FrAppDesc
{
	uint32_t viewportWidth;
	uint32_t viewportHeight;
	const char* appTitle;
};
	
CREND_API enum FrResult frCreateApp(const struct FrAppDesc* pDesc,
									struct FrApp** ppApp,
									struct FrAllocationCallbacks* pAllocCallbacks);
	
CREND_API enum FrResult frReleaseApp(struct FrApp* pApp,
									 struct FrAllocationCallbacks* pAllocCallbacks);

// Returns 0 on exit
CREND_API uint32_t frUpdateApp(struct FrApp* pApp);
	
// Renderer structure
struct FrRenderer;

// Renderer creation description
struct FrRendererDesc
{
	struct FrApp* pApp;
};

CREND_API enum FrResult frCreateRenderer(const struct FrRendererDesc*	pDesc,
					   struct FrRenderer**						ppRenderer,
					   struct FrAllocationCallbacks*		pAllocCallbacks);

CREND_API enum FrResult frReleaseRenderer(struct FrRenderer* 			pRenderer,
						struct FrAllocationCallbacks*	pAllocCallbacks);

CREND_API void frWaitForDevice(struct FrRenderer* pRenderer);
	
struct FrScene;
	
// Render proxy
struct FrRenderProxy_Mesh;

struct FrRenderProxyDesc_Mesh;
	
CREND_API enum FrResult frCreateRenderProxy_Mesh(struct FrScene* pRenderScene,
										 const struct FrRenderProxyDesc_Mesh* pDesc,
										 struct FrRenderProxy_Mesh** ppProxy);

CREND_API enum FrResult frReleaseRenderProxy_Mesh(struct FrScene* pRenderScene,
										  struct FrRenderProxy_Mesh* pProxy);

struct FrUpdateContext
{
	float dt;
};
	
CREND_API void frUpdateRenderer(struct FrRenderer* pRenderer, const struct FrUpdateContext* ctx);
	
CREND_API void frDrawFrame(struct FrRenderer* pRenderer);
	
#ifdef __cplusplus
}
#endif // __cplusplus
