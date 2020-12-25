#include "pch.h"
#include "launcher.h"
#include <chrono>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "crend/public.h"
#include "cphysics/public.h"

/**************** FURBALL CAT GAME ENGINE ****************/

struct FurMainAppDesc
{
	uint32_t m_width;
	uint32_t m_height;
	const char* m_title;
};

struct FurGameEngineDesc
{
	FurMainAppDesc m_mainApp;
};

// Furball Cat - Init engine functions

const char* g_furLastDetailedError = "";

void furSetLastError(const char* error)
{
	g_furLastDetailedError = error;
}

const char* FurGetLastError()
{
	return g_furLastDetailedError;
}

struct FurGameEngine
{
	struct fr_app_t* pApp;
	struct fr_renderer_t* pRenderer;
	fp_physics_t* pPhysics;
	
	std::chrono::system_clock::time_point prevTimePoint;
	
	fp_physics_scene_t* pPhysicsScene;
};

// Furball Cat - Platform
bool furMainEngineInit(const FurGameEngineDesc& desc, FurGameEngine** ppEngine)
{
	FurGameEngine* pEngine = (FurGameEngine*)malloc(sizeof(FurGameEngine));
	memset(pEngine, 0, sizeof(FurGameEngine));
	
	fr_app_desc_t appDesc;
	appDesc.appTitle = desc.m_mainApp.m_title;
	appDesc.viewportWidth = desc.m_mainApp.m_width;
	appDesc.viewportHeight = desc.m_mainApp.m_height;
	
	fr_result_t res = fr_create_app(&appDesc, &pEngine->pApp, NULL);
	
	if(res == FR_RESULT_OK)
	{
		fr_renderer_desc_t rendererDesc;
		rendererDesc.pApp = pEngine->pApp;
		
		res = fr_create_renderer(&rendererDesc, &pEngine->pRenderer, NULL);
	}
	
	if(res == FR_RESULT_OK)
	{
		res = fp_init_physics(&pEngine->pPhysics, NULL) == 0 ? FR_RESULT_OK : FR_RESULT_PHYSICS_INIT_ERROR;
	}
	
	if(res == FR_RESULT_OK)
	{
		*ppEngine = pEngine;
	}
	else
	{
		furSetLastError(fr_get_last_error());
		free(pEngine);
		return false;
	}
	
	// create physics scene
	if(res == FR_RESULT_OK)
	{
		fp_physics_scene_create(pEngine->pPhysics, &pEngine->pPhysicsScene, NULL);
	}
	
	return true;
}

void furMainEngineLoop(FurGameEngine* pEngine)
{
	pEngine->prevTimePoint = std::chrono::system_clock::now();
	
	while(fr_update_app(pEngine->pApp))
	{
		std::chrono::system_clock::time_point timeNow = std::chrono::system_clock::now();
		std::chrono::duration<float> dt = timeNow - pEngine->prevTimePoint;
		pEngine->prevTimePoint = timeNow;
		
		fr_update_context_t ctx;
		ctx.dt = dt.count();
		
		fp_physics_update_ctx_t physicsCtx;
		physicsCtx.dt = dt.count();
		
		fp_physics_update(pEngine->pPhysics, pEngine->pPhysicsScene, &physicsCtx);
		fr_update_renderer(pEngine->pRenderer, &ctx);
		fr_draw_frame(pEngine->pRenderer);
	}
	
	fr_wait_for_device(pEngine->pRenderer);
}

bool furMainEngineTerminate(FurGameEngine* pEngine)
{
	// check for memory leaks
	//FUR_ASSERT(furValidateAllocatorGeneral(&pEngine->m_memory._defaultInternals));
	
	fp_physics_scene_release(pEngine->pPhysics, pEngine->pPhysicsScene, NULL);
	
	fp_release_physics(pEngine->pPhysics, NULL);
	fr_release_renderer(pEngine->pRenderer, NULL);
	fr_release_app(pEngine->pApp, NULL);
	
	free(pEngine);	// rest of the deallocations should happen through allocators
	
	return true;
}

int main()
{
	FurGameEngineDesc desc = {};
	
	desc.m_mainApp.m_width = 1600;
	desc.m_mainApp.m_height = 900;
	desc.m_mainApp.m_title = "Duena";
	
	FurGameEngine* pEngine = NULL;
	
	// initialize most basic engine components
	bool initResult = furMainEngineInit(desc, &pEngine);
	if(!initResult)
	{
		printf("Engine initialization error. Last known error: %s.\n", FurGetLastError());
		return initResult;
	}
	
	// main engine loop
	furMainEngineLoop(pEngine);
	
	// terminate most basic engine components
	bool result = furMainEngineTerminate(pEngine);
	if(!result)
	{
		return 1;
	}
	
	return 0;
}
