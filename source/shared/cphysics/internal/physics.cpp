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

void fp_init_physics()
{
	g_physicsEngine.foundation = PxCreateFoundation(PX_PHYSICS_VERSION, g_defaultAllocator, g_defaultErrorCallback);
	g_physicsEngine.physics = PxCreatePhysics(PX_PHYSICS_VERSION, *g_physicsEngine.foundation, PxTolerancesScale());
}

void fp_release_physics()
{
	g_physicsEngine.foundation->release();
	g_physicsEngine.physics->release();
}
