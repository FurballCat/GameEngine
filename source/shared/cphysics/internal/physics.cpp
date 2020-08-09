/* Copyright (c) 2016-2020 Furball Cat */

#include "physics.h"
#include "ccore/public.h"
#include "cmath/public.h"

#define NDEBUG
#include "PxPhysicsAPI.h"

using namespace physx;

struct fp_physics_engine
{
	PxFoundation* foundation;
	PxPhysics* physics;
};

static PxDefaultAllocator g_defaultAllocator;
static PxDefaultErrorCallback g_defaultErrorCallback;

fp_physics_engine g_physicsEngine;

uint32_t fp_init_physics(fp_physics_t** ppPhysics, fc_alloc_callbacks_t* pAllocCallbacks)
{
	*ppPhysics = NULL;
	
	g_physicsEngine.foundation = PxCreateFoundation(PX_PHYSICS_VERSION, g_defaultAllocator, g_defaultErrorCallback);
	g_physicsEngine.physics = PxCreatePhysics(PX_PHYSICS_VERSION, *g_physicsEngine.foundation, PxTolerancesScale());
	
	return 0;
}

void fp_release_physics(fp_physics_t* pPhysics, fc_alloc_callbacks_t* pAllocCallbacks)
{
	g_physicsEngine.foundation->release();
	g_physicsEngine.physics->release();
}
