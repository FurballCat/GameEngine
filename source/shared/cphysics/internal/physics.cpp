/* Copyright (c) 2016-2020 Furball Cat */

#include "physics.h"
#include "ccore/public.h"
#include "cmath/public.h"

#include <stdlib.h>

#include "PxPhysicsAPI.h"

using namespace physx;

void fc_sort(void* arrayData, u64 numElems, u64 size, void* context, int(*compar)(void*, const void*, const void*))
{
#if PLATFORM_OSX
	qsort_r(arrayData, numElems, size, context, compar);
#elif PLATFORM_WINDOWS
	qsort_s(arrayData, numElems, size, compar, context);
#else
	#error No implementation of fc_sort for this platform
#endif
}

typedef struct fp_physics_t
{
	PxScene* scene;
	
	PxTolerancesScale tolerancesScale;
	PxFoundation* foundation;
	PxPhysics* physics;
	PxRigidStatic* worldPlane;
	PxRigidDynamic* testCapsule;
	PxRigidDynamic* testCapsule2;
	PxRevoluteJoint* testJoint;
	PxMaterial* testMaterial;
	
	PxControllerManager* controllerManager;
	PxController* controller;
} fp_physics_t;

static PxDefaultAllocator g_defaultAllocator;
static PxDefaultErrorCallback g_defaultErrorCallback;
static PxDefaultCpuDispatcher* g_defaultCPUDispatcher;

void fp_physics_init_scene(fp_physics_t* physics, fc_alloc_callbacks_t* pAllocCallbacks)
{
	PxSceneDesc desc(physics->tolerancesScale);
	desc.gravity = {0.0f, 0.0f, -9.81f};
	desc.cpuDispatcher = g_defaultCPUDispatcher;
	desc.filterShader = PxDefaultSimulationFilterShader;
	PxScene* pScene = physics->physics->createScene(desc);
	
	physics->scene = pScene;
	
	physics->testMaterial = physics->physics->createMaterial(1.0f, 0.5f, 0.6f);
	
	// world plane test
	PxTransform xform(PxVec3(0.0f, 0.0f, 0.0f), PxQuat(-PxHalfPi, PxVec3(0,1,0)));
	physics->worldPlane = physics->physics->createRigidStatic(xform);
	PxPlaneGeometry plane;
	PxRigidActorExt::createExclusiveShape(*physics->worldPlane, plane, &physics->testMaterial, 1);
	pScene->addActor(*physics->worldPlane);
	
	// capsule test
	{
		physics->testCapsule = physics->physics->createRigidDynamic(PxTransform(PxVec3(-2.0f, 0.0f, 2.0f), PxQuat(0.1f, PxVec3(0,1,0))));
		physics->testCapsule->setRigidBodyFlag(PxRigidBodyFlag::eKINEMATIC, false);
		physics->testCapsule->setAngularDamping(0.0f);
		PxTransform relativePose(PxQuat(-PxHalfPi, PxVec3(0,1,0)));
		PxShape* aCapsuleShape = PxRigidActorExt::createExclusiveShape(*physics->testCapsule, PxCapsuleGeometry(0.3f, 0.9f), &physics->testMaterial, 1);
		aCapsuleShape->setLocalPose(relativePose);
		PxRigidBodyExt::updateMassAndInertia(*physics->testCapsule, 1.0f);
		//pPhysics->testCapsule->setMass(40.0f);
		pScene->addActor(*physics->testCapsule);
	}
	
	// another capsule test
	{
		physics->testCapsule2 = physics->physics->createRigidDynamic(PxTransform(PxVec3(-2.0f, 0.0f, 2.0f), PxQuat(0.1f, PxVec3(0,1,0))));
		physics->testCapsule2->setRigidBodyFlag(PxRigidBodyFlag::eKINEMATIC, false);
		physics->testCapsule2->setAngularDamping(0.0f);
		PxTransform relativePose(PxQuat(-PxHalfPi, PxVec3(0,1,0)));
		PxShape* aCapsuleShape = PxRigidActorExt::createExclusiveShape(*physics->testCapsule2, PxCapsuleGeometry(0.3f, 0.9f), &physics->testMaterial, 1);
		aCapsuleShape->setLocalPose(relativePose);
		PxRigidBodyExt::updateMassAndInertia(*physics->testCapsule2, 1.0f);
		//pPhysics->testCapsule->setMass(40.0f);
		pScene->addActor(*physics->testCapsule2);
	}
	
	// test joint between capsules
//	{
//		PxSphericalJointCreate(physics->physics, physics->testCapsule, <#const PxTransform &localFrame0#>, <#PxRigidActor *actor1#>, <#const PxTransform &localFrame1#>)
//	}
	
	// todo: refactor, probably shouldn't be created here
	{
		FUR_ASSERT(physics->controllerManager == NULL);
		FUR_ASSERT(physics->controller == NULL);
		physics->controllerManager = PxCreateControllerManager(*pScene);
		PxControllerManager* mgr = physics->controllerManager;
		mgr->setOverlapRecoveryModule(true);	// make sure character is not created in an initial overlap state
		mgr->setPreciseSweeps(true);	//use precise sweep tests
		
		PxCapsuleControllerDesc controllerDesc;
		controllerDesc.setToDefault();
		controllerDesc.height = 1.3f;
		controllerDesc.radius = 0.2f;
		controllerDesc.stepOffset = 0.04f;	// must be smaller than height + 2 * radius
		controllerDesc.material = physics->testMaterial;
		controllerDesc.position = {0.0f, 0.0f, 1.0f};
		controllerDesc.upDirection = {0.0f, 0.0f, 1.0f};
		
		FUR_ASSERT(controllerDesc.isValid());
		physics->controller = mgr->createController(controllerDesc);
	}
}

fp_physics_t* fp_physics_create(fc_alloc_callbacks_t* pAllocCallbacks)
{
	fp_physics_t* physics = (fp_physics_t*)FUR_ALLOC_AND_ZERO(sizeof(fp_physics_t), 0, FC_MEMORY_SCOPE_PHYSICS, pAllocCallbacks);
	
	physics->tolerancesScale = PxTolerancesScale();
	physics->foundation = PxCreateFoundation(PX_PHYSICS_VERSION, g_defaultAllocator, g_defaultErrorCallback);
	physics->physics = PxCreatePhysics(PX_PHYSICS_VERSION, *(physics->foundation), physics->tolerancesScale);
	
	g_defaultCPUDispatcher = PxDefaultCpuDispatcherCreate(1);
	
	fp_physics_init_scene(physics, pAllocCallbacks);
	
	return physics;
}

void fp_physics_release(fp_physics_t* physics, fc_alloc_callbacks_t* pAllocCallbacks)
{
	if(physics->controller)
		physics->controller->release();
	
	physics->controllerManager->release();
	
	physics->scene->release();
	
	physics->foundation->release();
	physics->physics->release();
	
	g_defaultCPUDispatcher->release();
	
	FUR_FREE(physics, pAllocCallbacks);
}

void fp_physics_add_static_box(fp_physics_t* physics, const fm_xform* worldLocation,
							   const fm_vec3* halfExtents, fc_alloc_callbacks_t* pAllocCallbacks)
{
	PxScene* scene = physics->scene;
	
	PxTransform xform(PxVec3(worldLocation->pos.x, worldLocation->pos.y, worldLocation->pos.z),
					  PxQuat(worldLocation->rot.i, worldLocation->rot.j, worldLocation->rot.k, worldLocation->rot.r));
	PxRigidStatic* obj = physics->physics->createRigidStatic(xform);
	
	PxBoxGeometry box(halfExtents->x, halfExtents->y, halfExtents->z);
	PxRigidActorExt::createExclusiveShape(*obj, box, &physics->testMaterial, 1);
	
	scene->addActor(*obj);
}

bool fp_physics_raycast(fp_physics_t* physics, const fm_vec4* start, const fm_vec4* dir, const f32 distance, fp_physics_raycast_hit_t* hit)
{
	PxScene* scene = physics->scene;
	
	PxRaycastBuffer hitPx;
	
	PxQueryFilterData fd;
	fd.flags |= PxQueryFlag::eANY_HIT; // note the OR with the default value
	bool status = scene->raycast(PxVec3(start->x, start->y, start->z), PxVec3(dir->x, dir->y, dir->z), distance, hitPx, PxHitFlags(PxHitFlag::eDEFAULT), fd);
	
	if(hitPx.hasBlock)
	{
		const PxRaycastHit& rHit = hitPx.getAnyHit(0);
		hit->pos.x = rHit.position.x;
		hit->pos.y = rHit.position.y;
		hit->pos.z = rHit.position.z;
		hit->distance = rHit.distance;
	}
	
	return status;
}

void fp_physics_update(fp_physics_t* physics, const fp_physics_update_ctx_t* pCtx)
{
	// character controller update
	{
		PxVec3 disp = {pCtx->playerDisplacement->x, pCtx->playerDisplacement->y, pCtx->playerDisplacement->z};
		PxControllerFilters filters;
		physics->controller->move(disp, 0.00001f, pCtx->dt, filters);
	}
	
	// scene simulation
	{
		PxScene* scene = physics->scene;
		scene->simulate(pCtx->dt);
		scene->fetchResults(true); // true to block the thread until simulate is finished
	}
	
	//const f32 red[4] = FUR_COLOR_RED;
	//const f32 green[4] = FUR_COLOR_GREEN;
	//const f32 blue[4] = FUR_COLOR_BLUE;
	
	// text capsule rigid body
	{
		//const PxTransform t = pPhysics->testCapsule->getGlobalPose();
		//const PxVec3 x = t.q.getBasisVector0();
		//const PxVec3 y = t.q.getBasisVector1();
		//const PxVec3 z = t.q.getBasisVector2();
		
		//const f32 start[3] = {t.p.x, t.p.y, t.p.z};
		//const f32 end_x[3] = {t.p.x + x.x, t.p.y + x.y, t.p.z + x.z};
		//const f32 end_y[3] = {t.p.x + y.x, t.p.y + y.y, t.p.z + y.z};
		//const f32 end_z[3] = {t.p.x + z.x, t.p.y + z.y, t.p.z + z.z};
		
		/*
		fc_dbg_line(start, end_x, red);
		fc_dbg_line(start, end_y, green);
		fc_dbg_line(start, end_z, blue);
		 */
	}
	
	// plane collider
	{
		const f32 planeHalfLength = 1.0f;
		const f32 spacing = planeHalfLength / 0.5f;
		const u32 gridSize = 20;
		
		for(u32 x=0; x<gridSize; ++x)
		{
			for(u32 y=0; y<gridSize; ++y)
			{
				const f32 planeCenter[3] = {spacing * x - spacing * gridSize * 0.5f, spacing * y - spacing * gridSize * 0.5f, 0.0f};
				f32 planeColor[4] = FUR_COLOR_DARK_GREY;
				
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
	/*{
		const PxTransform t = pPhysics->controller->getActor()->getGlobalPose();
		const PxVec3 x = t.q.getBasisVector0();
		const PxVec3 y = t.q.getBasisVector1();
		const PxVec3 z = t.q.getBasisVector2();
		
		const PxExtendedVec3 footPos = pPhysics->controller->getFootPosition();
		
		const f32 start[3] = {t.p.x, t.p.y, t.p.z};
		const f32 end_x[3] = {t.p.x + x.x, t.p.y + x.y, t.p.z + x.z};
		const f32 end_y[3] = {t.p.x + y.x, t.p.y + y.y, t.p.z + y.z};
		const f32 end_z[3] = {t.p.x + z.x, t.p.y + z.y, t.p.z + z.z};
		const f32 footPosf[3] = {(f32)footPos.x, (f32)footPos.y, (f32)footPos.z};
		
		const f32 yellow[4] = FUR_COLOR_YELLOW;
		
		fc_dbg_line(start, end_x, red);
		fc_dbg_line(start, end_x, red);
		fc_dbg_line(start, end_y, green);
		fc_dbg_line(start, end_z, blue);
		fc_dbg_line(start, footPosf, yellow);
	}*/
}

void fp_physics_get_player_info(fp_physics_t* physics, fp_physics_player_info_t* playerInfo)
{
	const PxExtendedVec3 footPos = physics->controller->getFootPosition();
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

// ----- BOUNDING VOLUME HIERARCHY -----
typedef struct fp_bvh_node_t
{
	fm_box bound;
	u32 numChildren;	// if 0, then next property is an object ID
	u32 idxFirstChildOrObjectID;
} fp_bvh_node_t;

int fp_sort_comp_x(void* inAllBoxes, const void* inIdxA, const void* inIdxB)
{
	const fm_box* allBoxes = (const fm_box*)inAllBoxes;
	const u32 idxA = *(const u32*)inIdxA;
	const u32 idxB = *(const u32*)inIdxB;
	
	return allBoxes[idxA].center.x > allBoxes[idxB].center.x;
}

int fp_sort_comp_y(void* inAllBoxes, const void* inIdxA, const void* inIdxB)
{
	const fm_box* allBoxes = (const fm_box*)inAllBoxes;
	const u32 idxA = *(const u32*)inIdxA;
	const u32 idxB = *(const u32*)inIdxB;
	
	return allBoxes[idxA].center.y > allBoxes[idxB].center.y;
}

int fp_sort_comp_z(void* inAllBoxes, const void* inIdxA, const void* inIdxB)
{
	const fm_box* allBoxes = (const fm_box*)inAllBoxes;
	const u32 idxA = *(const u32*)inIdxA;
	const u32 idxB = *(const u32*)inIdxB;
	
	return allBoxes[idxA].center.z > allBoxes[idxB].center.z;
}

void fp_bvh_box_recursive_build(fp_bvh_node_t* nodes, u32 maxNodes, u32 curNode, u32* nextFreeNodeIdx,
								fm_box* allObjectBoxes, u32* objectIndices, u32 numObjects)
{
	// stop condition - single object in the BVH node
	if(numObjects == 1)
	{
		nodes[curNode].bound = allObjectBoxes[objectIndices[0]];
		nodes[curNode].idxFirstChildOrObjectID = objectIndices[0];
		nodes[curNode].numChildren = 0;
		return;
	}
	
	// calculate the box bounding all the objects in the list
	fm_box box = allObjectBoxes[objectIndices[0]];
	for(u32 i=1; i<numObjects; ++i)
	{
		fm_box_append(&box, &allObjectBoxes[objectIndices[i]]);
	}
	
	// check which axis of box is the longest and sort objects along this axis
	if(box.extent.x > box.extent.y && box.extent.x > box.extent.z)	// x-axis
	{
		fc_sort(objectIndices, numObjects, sizeof(u32), allObjectBoxes, fp_sort_comp_x);
	}
	else if(box.extent.y > box.extent.z)	// y-axis
	{
		fc_sort(objectIndices, numObjects, sizeof(u32), allObjectBoxes, fp_sort_comp_y);
	}
	else	// z-axis
	{
		fc_sort(objectIndices, numObjects, sizeof(u32), allObjectBoxes, fp_sort_comp_z);
	}
	
	// fill in this node
	nodes[curNode].bound = box;
	nodes[curNode].idxFirstChildOrObjectID = *nextFreeNodeIdx;
	nodes[curNode].numChildren = 2;
	
	// create children nodes
	const u32 childIdxA = nodes[curNode].idxFirstChildOrObjectID;
	const u32 childIdxB = nodes[curNode].idxFirstChildOrObjectID + 1;
	
	const u32 numObjectsA = numObjects / 2;
	const u32 numObjectsB = numObjects - numObjectsA;
	
	FUR_ASSERT(*nextFreeNodeIdx + 2 < maxNodes);
	*nextFreeNodeIdx += 2;
	
	fp_bvh_box_recursive_build(nodes, maxNodes, childIdxA, nextFreeNodeIdx, allObjectBoxes, objectIndices, numObjectsA);
	fp_bvh_box_recursive_build(nodes, maxNodes, childIdxB, nextFreeNodeIdx, allObjectBoxes, objectIndices + numObjectsA, numObjectsB);
}

void fp_bvh_build(const fp_bvh_build_ctx_t* ctx, fp_bvh_t* bvh, fc_alloc_callbacks_t* pAllocCallbacks)
{
	// allocate array of indices on scratchpad
	const u32 sizeMemIndices = ctx->numObjects * sizeof(u32);
	u32* objectIndices = (u32*)fc_mem_arena_alloc(ctx->arenaAlloc, sizeMemIndices, 0);
	
	for(u32 i=0; i<ctx->numObjects; ++i)
	{
		objectIndices[i] = i;
	}
	
	// allocate array of temporary BVH nodes on scratchpad, later on they will be copied to proper memory
	const u32 numMaxNodes = (ctx->arenaAlloc->capacity - ctx->arenaAlloc->size) / sizeof(fp_bvh_node_t);
	FUR_ASSERT(numMaxNodes > 0);
	
	// init nodes by zero
	fp_bvh_node_t* tmpNodes = (fp_bvh_node_t*)fc_mem_arena_alloc_and_zero(ctx->arenaAlloc, sizeof(fp_bvh_node_t) * numMaxNodes, 0);
	
	u32 nextFreeIndex = 1; // 1 because root is assumed to be already allocated
	
	// recursively iterate objects to build BVH
	fp_bvh_box_recursive_build(tmpNodes, numMaxNodes, 0, &nextFreeIndex, ctx->objectBoxes, objectIndices, ctx->numObjects);
	
	FUR_ASSERT(nextFreeIndex > 0);	// something got processed - yey!
	
	const u32 numFinalNodes = nextFreeIndex;
	
	bvh->nodes = FUR_ALLOC_ARRAY(fp_bvh_node_t, numFinalNodes, 0, FC_MEMORY_SCOPE_PHYSICS, pAllocCallbacks);
	bvh->numNodes = numFinalNodes;
	
	memcpy(bvh->nodes, tmpNodes, sizeof(fp_bvh_node_t) * numFinalNodes);
}

void fp_bvh_release(fp_bvh_t* bvh, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(bvh->nodes, pAllocCallbacks);
	bvh->nodes = NULL;
	bvh->numNodes = 0;
}

void fp_bvh_debug_draw(const fp_bvh_t* bvh)
{
	const u32 numNodes = bvh->numNodes;

	f32 color[4] = FUR_COLOR_RED;

	for(u32 i=0; i<numNodes; ++i)
	{
		const fm_box* box = &bvh->nodes[i].bound;
		fc_dbg_box_wire((const f32*)&box->center, (const f32*)&box->extent, color);
	}
}
