/* Copyright (c) 2016-2020 Furball Cat */

#include "physics.h"
#include "ccore/public.h"
#include "cmath/public.h"

#define NDEBUG
#include "PxPhysicsAPI.h"

using namespace physx;

typedef struct fp_physics_t
{
	PxTolerancesScale tolerancesScale;
	PxFoundation* foundation;
	PxPhysics* physics;
} fp_physics_t;

static PxDefaultAllocator g_defaultAllocator;
static PxDefaultErrorCallback g_defaultErrorCallback;

uint32_t fp_init_physics(fp_physics_t** ppPhysics, fc_alloc_callbacks_t* pAllocCallbacks)
{
	*ppPhysics = (fp_physics_t*)FUR_ALLOC_AND_ZERO(sizeof(fp_physics_t), 0, FC_MEMORY_SCOPE_PHYSICS, pAllocCallbacks);
	
	(*ppPhysics)->tolerancesScale = PxTolerancesScale();
	(*ppPhysics)->foundation = PxCreateFoundation(PX_PHYSICS_VERSION, g_defaultAllocator, g_defaultErrorCallback);
	(*ppPhysics)->physics = PxCreatePhysics(PX_PHYSICS_VERSION, *(*ppPhysics)->foundation, (*ppPhysics)->tolerancesScale);
	
	return 0;
}

void fp_release_physics(fp_physics_t* pPhysics, fc_alloc_callbacks_t* pAllocCallbacks)
{
	pPhysics->foundation->release();
	pPhysics->physics->release();
}

uint32_t fp_physics_scene_create(fp_physics_t* pPhysics, fp_physics_scene_t** ppScene, fc_alloc_callbacks_t* pAllocCallbacks)
{
	PxSceneDesc desc(pPhysics->tolerancesScale);
	PxScene* pScene = pPhysics->physics->createScene(desc);
	
	*ppScene = (fp_physics_scene_t*)pScene;
	
	return 0;
}

void fp_physics_scene_release(fp_physics_t* pPhysics, fp_physics_scene_t* pScene, fc_alloc_callbacks_t* pAllocCallbacks)
{
	PxScene* scene = (PxScene*)pScene;
	scene->release();
}
