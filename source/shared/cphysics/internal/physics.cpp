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
	PxRigidStatic* worldPlane;
	PxRigidDynamic* testCapsule;
	PxMaterial* testMaterial;
} fp_physics_t;

static PxDefaultAllocator g_defaultAllocator;
static PxDefaultErrorCallback g_defaultErrorCallback;
static PxDefaultCpuDispatcher* g_defaultCPUDispatcher;

inline PxScene* fp_physics_scene_to_px_scene(fp_physics_scene_t* pScene)
{
	return (PxScene*)pScene;
}

uint32_t fp_init_physics(fp_physics_t** ppPhysics, fc_alloc_callbacks_t* pAllocCallbacks)
{
	*ppPhysics = (fp_physics_t*)FUR_ALLOC_AND_ZERO(sizeof(fp_physics_t), 0, FC_MEMORY_SCOPE_PHYSICS, pAllocCallbacks);
	
	(*ppPhysics)->tolerancesScale = PxTolerancesScale();
	(*ppPhysics)->foundation = PxCreateFoundation(PX_PHYSICS_VERSION, g_defaultAllocator, g_defaultErrorCallback);
	(*ppPhysics)->physics = PxCreatePhysics(PX_PHYSICS_VERSION, *(*ppPhysics)->foundation, (*ppPhysics)->tolerancesScale);
	
	g_defaultCPUDispatcher = PxDefaultCpuDispatcherCreate(1);
	
	return 0;
}

void fp_release_physics(fp_physics_t* pPhysics, fc_alloc_callbacks_t* pAllocCallbacks)
{
	pPhysics->foundation->release();
	pPhysics->physics->release();
	
	g_defaultCPUDispatcher->release();
}

uint32_t fp_physics_scene_create(fp_physics_t* pPhysics, fp_physics_scene_t** ppScene, fc_alloc_callbacks_t* pAllocCallbacks)
{
	PxSceneDesc desc(pPhysics->tolerancesScale);
	desc.gravity = {0.0f, 0.0f, -9.81f};
	desc.cpuDispatcher = g_defaultCPUDispatcher;
	desc.filterShader = PxDefaultSimulationFilterShader;
	PxScene* pScene = pPhysics->physics->createScene(desc);
	
	*ppScene = (fp_physics_scene_t*)pScene;
	
	pPhysics->testMaterial = pPhysics->physics->createMaterial(1.0f, 0.5f, 0.6f);
	
	// world plane test
	PxTransform xform(PxVec3(0.0f, 0.0f, 0.0f), PxQuat(-PxHalfPi, PxVec3(0,1,0)));
	pPhysics->worldPlane = pPhysics->physics->createRigidStatic(xform);
	PxPlaneGeometry plane;
	PxRigidActorExt::createExclusiveShape(*pPhysics->worldPlane, plane, &pPhysics->testMaterial, 1);
	pScene->addActor(*pPhysics->worldPlane);
	
	// capsule test
	pPhysics->testCapsule = pPhysics->physics->createRigidDynamic(PxTransform(PxVec3(-2.0f, 0.0f, 2.0f), PxQuat(0.1f, PxVec3(0,1,0))));
	pPhysics->testCapsule->setRigidBodyFlag(PxRigidBodyFlag::eKINEMATIC, false);
	pPhysics->testCapsule->setAngularDamping(0.0f);
	PxTransform relativePose(PxQuat(-PxHalfPi, PxVec3(0,1,0)));
	PxShape* aCapsuleShape = PxRigidActorExt::createExclusiveShape(*pPhysics->testCapsule, PxCapsuleGeometry(0.3f, 0.9f), &pPhysics->testMaterial, 1);
	aCapsuleShape->setLocalPose(relativePose);
	PxRigidBodyExt::updateMassAndInertia(*pPhysics->testCapsule, 1.0f);
	//pPhysics->testCapsule->setMass(40.0f);
	pScene->addActor(*pPhysics->testCapsule);
	
	return 0;
}

void fp_physics_scene_release(fp_physics_t* pPhysics, fp_physics_scene_t* pScene, fc_alloc_callbacks_t* pAllocCallbacks)
{
	PxScene* scene = (PxScene*)pScene;
	scene->release();
}

void fp_physics_update(fp_physics_t* pPhysics, fp_physics_scene_t* pScene, const fp_physics_update_ctx_t* pCtx)
{
	PxScene* scene = fp_physics_scene_to_px_scene(pScene);
	scene->simulate(pCtx->dt);
	scene->fetchResults();
	
	const PxTransform t = pPhysics->testCapsule->getGlobalPose();
	const PxVec3 x = t.q.getBasisVector0();
	const PxVec3 y = t.q.getBasisVector1();
	const PxVec3 z = t.q.getBasisVector2();
	
	const float start[3] = {t.p.x, t.p.y, t.p.z};
	const float end_x[3] = {t.p.x + x.x, t.p.y + x.y, t.p.z + x.z};
	const float end_y[3] = {t.p.x + y.x, t.p.y + y.y, t.p.z + y.z};
	const float end_z[3] = {t.p.x + z.x, t.p.y + z.y, t.p.z + z.z};
	const float red[4] = FUR_COLOR_RED;
	const float green[4] = FUR_COLOR_GREEN;
	const float blue[4] = FUR_COLOR_BLUE;
	
	fc_dbg_line(start, end_x, red);
	fc_dbg_line(start, end_y, green);
	fc_dbg_line(start, end_z, blue);
	
	const float planeCenter[3] = {0.0f, 0.0f, 0.0f};
	const float planeHalfLength = 10.0f;
	const float planeColor[4] = FUR_COLOR_DARK_GREY;
	
	fc_dbg_plane(planeCenter, planeHalfLength, planeColor);
}
