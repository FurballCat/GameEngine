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
	
	PxControllerManager* controllerManager;
	PxController* controller;
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
	
	FUR_FREE(pPhysics, pAllocCallbacks);
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
	
	// todo: refactor, probably shouldn't be created here
	{
		FUR_ASSERT(pPhysics->controllerManager == NULL);
		FUR_ASSERT(pPhysics->controller == NULL);
		pPhysics->controllerManager = PxCreateControllerManager(*pScene);
		PxControllerManager* mgr = pPhysics->controllerManager;
		mgr->setOverlapRecoveryModule(true);	// make sure character is not created in an initial overlap state
		mgr->setPreciseSweeps(true);	//use precise sweep tests
		
		PxCapsuleControllerDesc controllerDesc;
		controllerDesc.setToDefault();
		controllerDesc.height = 1.3f;
		controllerDesc.radius = 0.2f;
		controllerDesc.stepOffset = 0.04f;	// must be smaller than height + 2 * radius
		controllerDesc.material = pPhysics->testMaterial;
		controllerDesc.position = {0.0f, 0.0f, 1.0f};
		controllerDesc.upDirection = {0.0f, 0.0f, 1.0f};
		
		FUR_ASSERT(controllerDesc.isValid());
		pPhysics->controller = mgr->createController(controllerDesc);
	}
	
	return 0;
}

void fp_physics_scene_release(fp_physics_t* pPhysics, fp_physics_scene_t* pScene, fc_alloc_callbacks_t* pAllocCallbacks)
{
	if(pPhysics->controller)
		pPhysics->controller->release();
	
	pPhysics->controllerManager->release();
	
	PxScene* scene = (PxScene*)pScene;
	scene->release();
}

void fp_physics_update(fp_physics_t* pPhysics, fp_physics_scene_t* pScene, const fp_physics_update_ctx_t* pCtx)
{
	// character controller update
	{
		PxVec3 disp = {pCtx->playerDisplacement->x, pCtx->playerDisplacement->y, pCtx->playerDisplacement->z};
		PxControllerFilters filters;
		pPhysics->controller->move(disp, 0.00001f, pCtx->dt, filters);
	}
	
	// scene simulation
	{
		PxScene* scene = fp_physics_scene_to_px_scene(pScene);
		scene->simulate(pCtx->dt);
		scene->fetchResults(true); // true to block the thread until simulate is finished
	}
	
	const float red[4] = FUR_COLOR_RED;
	const float green[4] = FUR_COLOR_GREEN;
	const float blue[4] = FUR_COLOR_BLUE;
	
	// text capsule rigid body
	{
		const PxTransform t = pPhysics->testCapsule->getGlobalPose();
		const PxVec3 x = t.q.getBasisVector0();
		const PxVec3 y = t.q.getBasisVector1();
		const PxVec3 z = t.q.getBasisVector2();
		
		const float start[3] = {t.p.x, t.p.y, t.p.z};
		const float end_x[3] = {t.p.x + x.x, t.p.y + x.y, t.p.z + x.z};
		const float end_y[3] = {t.p.x + y.x, t.p.y + y.y, t.p.z + y.z};
		const float end_z[3] = {t.p.x + z.x, t.p.y + z.y, t.p.z + z.z};
		
		/*
		fc_dbg_line(start, end_x, red);
		fc_dbg_line(start, end_y, green);
		fc_dbg_line(start, end_z, blue);
		 */
	}
	
	// plane collider
	{
		const float planeHalfLength = 1.0f;
		const float spacing = planeHalfLength / 0.5f;
		const uint32_t gridSize = 20;
		
		for(uint32_t x=0; x<gridSize; ++x)
		{
			for(uint32_t y=0; y<gridSize; ++y)
			{
				const float planeCenter[3] = {spacing * x - spacing * gridSize * 0.5f, spacing * y - spacing * gridSize * 0.5f, 0.0f};
				float planeColor[4] = FUR_COLOR_DARK_GREY;
				
				if((x + y) % 2)
				{
					planeColor[0] = 0.4f;
					planeColor[1] = 0.4f;
					planeColor[2] = 0.4f;
					planeColor[3] = 0.4f;
				}
				
				fc_dbg_plane(planeCenter, planeHalfLength, planeColor);
			}
		}
	}
	
	// character controller
	{
		const PxTransform t = pPhysics->controller->getActor()->getGlobalPose();
		const PxVec3 x = t.q.getBasisVector0();
		const PxVec3 y = t.q.getBasisVector1();
		const PxVec3 z = t.q.getBasisVector2();
		
		const PxExtendedVec3 footPos = pPhysics->controller->getFootPosition();
		
		const float start[3] = {t.p.x, t.p.y, t.p.z};
		const float end_x[3] = {t.p.x + x.x, t.p.y + x.y, t.p.z + x.z};
		const float end_y[3] = {t.p.x + y.x, t.p.y + y.y, t.p.z + y.z};
		const float end_z[3] = {t.p.x + z.x, t.p.y + z.y, t.p.z + z.z};
		const float footPosf[3] = {(float)footPos.x, (float)footPos.y, (float)footPos.z};
		
		const float yellow[4] = FUR_COLOR_YELLOW;
		
		/*
		fc_dbg_line(start, end_x, red);
		fc_dbg_line(start, end_x, red);
		fc_dbg_line(start, end_y, green);
		fc_dbg_line(start, end_z, blue);
		fc_dbg_line(start, footPosf, yellow);
		 */
	}
}

void fp_physics_get_player_info(fp_physics_t* pPhysics, fp_physics_scene_t* pScene, fp_physics_player_info_t* playerInfo)
{
	const PxExtendedVec3 footPos = pPhysics->controller->getFootPosition();
	playerInfo->locator->pos.x = footPos.x;
	playerInfo->locator->pos.y = footPos.y;
	playerInfo->locator->pos.z = footPos.z;
	playerInfo->locator->pos.w = 1.0f;
	
	//const PxTransform t = pPhysics->controller->getActor()->getGlobalPose();
	//const PxVec3 x = t.q.getBasisVector0();
	//const PxVec3 y = t.q.getBasisVector1();
	//const PxVec3 z = t.q.getBasisVector2();
	fm_quat_identity(&playerInfo->locator->rot);
}
