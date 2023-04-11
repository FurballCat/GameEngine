/* Copyright (c) Furball Cat */

#pragma once

#include "api.h"
#include "ccore/types.h"
#include "ccore/platform.h"

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

typedef struct FcAllocator FcAllocator;
typedef struct FcRenderer FcRenderer;
typedef struct FcAnimSystem FcAnimSystem;
typedef struct FcInputManager FcInputManager;
typedef struct FcPhysics FcPhysics;
typedef struct FcDepot FcDepot;
typedef struct FcApplication FcApplication;
typedef struct FcWorld FcWorld;
typedef struct FcCameraSystem FcCameraSystem;

typedef struct FcEngine
{
	FcApplication* app;
	FcDepot* depot;
	FcInputManager* inputManager;

	FcRenderer* renderer;
	FcAnimSystem* animSystem;
	FcPhysics* physics;

	FcWorld* world;
	FcCameraSystem* cameraSystem;

	FcTimeval lastFrameGlobalTime;

} FcEngine;

typedef struct FcEngineCreateInfo
{
	FcApplication* app;
	FcDepot* depot;
} FcEngineCreateInfo;

CGAME_API FcResult fcCreateEngine(const FcEngineCreateInfo* info, const FcAllocator* allocator, FcEngine** engine);
CGAME_API void fcDestroyEngine(FcEngine* engine, const FcAllocator* allocator);
CGAME_API void fcEnterEngineMainLoop(FcEngine* engine, const FcAllocator* allocator);

#ifdef __cplusplus
}
#endif // __cplusplus
