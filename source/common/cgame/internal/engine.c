/* Copyright (c) Furball Cat */

#include "engine.h"

#include "ccore/public.h"
#include "canim/public.h"
#include "crend/public.h"
#include "cinput/public.h"
#include "ccore/jobs.h"
#include "cphysics/public.h"

#include "camera.h"
#include "world.h"
#include "script.h"

FcResult fcCreateEngine(const FcEngineCreateInfo* info, const FcAllocator* allocator, FcEngine** ppEngine)
{
	// init fundamentals
	fcProfilerInit(allocator);
	fcStringIdRegisterInit(allocator);
	fcJobSystemInit(allocator);
	fcScriptInit(allocator);

	// init engine systems
	FcEngine* engine = FUR_ALLOC_AND_ZERO(sizeof(FcEngine), 8, FC_MEMORY_SCOPE_GAME, allocator);
	*ppEngine = engine;
	
	// input
	{
		FcResult res = fcCreateInputManager(allocator, &engine->inputManager);
		if (res != FC_SUCCESS)
			return res;
	}

	// renderer
	{
		FcRendererCreateInfo desc;
		desc.pApp = info->app;
		desc.depot = info->depot;

		FcResult res = fcCreateRenderer(&desc, allocator, &engine->renderer);
		if (res != FC_SUCCESS)
			return res;
	}

	// physics
	{
		FcResult res = fcCreatePhysics(allocator, &engine->physics);
		if (res != FC_SUCCESS)
			return res;
	}

	// animation
	{
		FcResult res = fcCreateAnimSystem(allocator, &engine->animSystem);
		if (res != FC_SUCCESS)
			return res;
	}

	// camera
	{
		FcResult res = fcCreateCameraSystem(allocator, &engine->cameraSystem);
		if (res != FC_SUCCESS)
			return res;
	}

	// world
	{
		FcWorldCreateInfo worldDesc = { 0 };
		worldDesc.systems.renderer = engine->renderer;
		worldDesc.systems.animation = engine->animSystem;
		worldDesc.systems.inputAction = NULL;

		FcResult res = fcCreateWorld(&worldDesc, allocator, &engine->world);
		if (res != FC_SUCCESS)
			return res;
	}

	return FC_SUCCESS;
}

void fcDestroyEngine(FcEngine* engine, const FcAllocator* allocator)
{
	// destroy engine systems
	fcDestroyInputManager(engine->inputManager, allocator);
	fcDestroyRenderer(engine->renderer, allocator);
	fcDestroyPhysics(engine->physics, allocator);
	fcDestroyAnimSystem(engine->animSystem, allocator);
	fcDestroyCameraSystem(engine->cameraSystem, allocator);
	fcDestroyWorld(engine->world, allocator);

	FUR_FREE(engine, allocator);

	// release fundamentals
	fcScriptRelease(allocator);
	fcJobSystemRelease(allocator);
	fcStringIdRegisterRelease(allocator);
	fcProfilerRelease(allocator);
}

typedef struct FcMainThreadUserData
{
	FcEngine* engine;
	const FcAllocator* allocator;
} FcMainThreadUserData;

void fcMainEngineLoop(FcEngine* engine, f32 dt, const FcAllocator* allocator)
{

}

FUR_JOB_ENTRY_POINT(fcMainEngineLoopJob)
{
	FcMainThreadUserData* userData = FUR_JOB_USER_DATA(FcMainThreadUserData);

	FcEngine* engine = userData->engine;
	const FcAllocator* allocator = userData->allocator;

	fcTimevalNow(&engine->lastFrameGlobalTime);

	while (fcApplicationUpdate(engine->app))
	{
		// calculate this frame's delta time
		FcTimeval timeNow = {0};
		fcTimevalNow(&timeNow);

		const f32 dt = fcTimevalDiffToDeltaSeconds(&engine->lastFrameGlobalTime, &timeNow);
		engine->lastFrameGlobalTime = timeNow;

		fcProfilerStartFrame();

		FUR_PROFILE("frame")
		{
			fcMainEngineLoop(engine, dt, allocator);
		}

		fcProfilerEndFrame();
	}

	fcRendererWaitForDevice(engine->renderer);

	fcJobSystemExitAllJobs();
}

void fcEnterEngineMainLoop(FcEngine* engine, const FcAllocator* allocator)
{
	FcMainThreadUserData data = { engine, allocator };

	FcJobDecl mainThreadJob = {0};
	mainThreadJob.userData = &data;
	mainThreadJob.func = fcMainEngineLoopJob;
	fcJobSystemSetupMainThreadJob(&mainThreadJob);

	// see fcMainEngineLoopJob for the actual main thread loop
	fcJobSystemEnterWorkerThreadMode();
}
