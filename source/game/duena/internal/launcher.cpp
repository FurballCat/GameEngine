#include "pch.h"
#include "launcher.h"
#include <chrono>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "crend/public.h"
#include "cphysics/public.h"
#include "canim/public.h"
#include "cimport/public.h"
#include "cmath/public.h"
#include "ccore/public.h"
#include "ccore/buffer.h"
#include "ccore/jobs.h"
#include "ccore/textParsing.h"
#include "ccore/serialize.h"
#include "ccore/fileio.h"
#include "cinput/public.h"
#include "cgame/world.h"
#include "cgame/camera.h"
#include "cgame/script.h"

/**************** FURBALL CAT GAME ENGINE ****************/

struct FurMainAppDesc
{
	u32 m_width;
	u32 m_height;
	const char* m_title;
};

struct FcGameEngineCreateInfo
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

typedef struct FcAnimateActionSlots
{
	FcAnimActionAnimate slot[32];
} FcAnimateActionSlots;

FcAnimActionAnimate* fcAnimateActionSlotsGetFree(FcAnimateActionSlots* slots)
{
	for(u32 i=0; i<32; ++i)
	{
		if(slots->slot[i].reserved == false)
		{
			return &slots->slot[i];
		}
	}
	
	return NULL;
}

const FcAnimClip* fcResourceRegisterLoadAnim(FcResourceRegister* reg, FcDepot* depot, const char* name,
								   const FcRig* rig, const FcAllocator* allocator)
{
	FcAnimClip* animClip = NULL;
	
	{
		const char* directory = "data/anim/";
		const char* extension = ".fbx";
		const char* engineExtension = ".anim";
		const u64 dirLength = strlen(directory);
		const u64 nameLength = strlen(name);
		
		char pathEngine[256] = {};
		fcPathConcat(pathEngine, "", "data/anim/", name, ".anim");

		FcFilePath file_path = fcDepotGetFilePath(depot, pathEngine);
		FcFile* file = fcFileOpen(depot, file_path, "rb");
		
		// try loading or saving engine file
		FcSerializer serializer = {};
		serializer.file = file;
		serializer.isWriting = false;
			
		if(!serializer.isWriting)
		{
			animClip = (FcAnimClip*)FUR_ALLOC_AND_ZERO(sizeof(FcAnimClip), 0, FC_MEMORY_SCOPE_ANIMATION, allocator);
		}
			
		fcAnimClipSerialize(&serializer, animClip, allocator);

		fcFileClose(file);
		
		// register animation
		fcResourceRegisterAddAnimClip(reg, SID_REG(name), animClip);
	}
	
	return animClip;
}

// game object types
typedef struct FcGameObject_Zelda
{
	FcGameObject info;	// this property must be the first one in every game object
	
	FcScriptLambda script;
	
	FcAnimateActionSlots animateActionSlots;
	FcAnimCharacter* animCharacter;
	const FcRenderProxy* mesh;
	
	fm_vec4 velocity;
	fm_vec4 logicMove;
	
	FcStringId playerState;
	bool playerWeaponEquipped;
	bool playerWindProtecting;
	bool equipItemNow;
	bool showAnimStateDebug;
	bool isJump;
	bool isGrounded;

} FcGameObject_Zelda;

bool fcGameObject_ZeldaInit(FcGameObject* gameObject, FcGameObjectInitCtx* ctx)
{
	FUR_PROFILE("init-zelda")
	{
		FcGameObject_Zelda* zelda = (FcGameObject_Zelda*)gameObject;
		
		zelda->playerState = fcSpawnInfoGetStringId(ctx->info, SID("state"), SID("idle"));
		
		FcAnimCharacterDesc desc = {};
		desc.globalTime = ctx->globalTime;
		desc.rig = fcResourceRegisterFindRig(ctx->resources, SID("zelda-rig"));
		zelda->animCharacter = fcCreateAnimCharacter(&desc, ctx->stackAlloc);
		fcAnimSystemAddCharacter(ctx->systems->animation, zelda->animCharacter);
		
		zelda->mesh = fcResourceRegisterFindMesh(ctx->resources, SID("zelda-mesh"));
		
		const FcStringId scriptName = fcSpawnInfoGetStringId(ctx->info, SID("state-script"), SID("none"));
		zelda->script.scriptBlob = fcResourceRegisterFindScript(ctx->resources, scriptName);
		zelda->script.lambdaName = zelda->playerState;
		zelda->script.state = zelda->playerState;
		zelda->script.eventName = SID("start");
		zelda->script.numSkipOps = 0;
		zelda->script.waitSeconds = 0.0f;
		zelda->script.selfGameObject = gameObject;
		zelda->script.isActive = true;
	}
	
	return true;
}

void fcGameObject_ZeldaUpdate(FcGameObject* gameObject, FcGameObjectUpdateCtx* ctx)
{
	FUR_PROFILE("update-zelda")
	{
		FcGameObject_Zelda* zelda = (FcGameObject_Zelda*)gameObject;

		fcScriptUpdateLambda(&zelda->script, ctx->world, ctx->dt);

		zelda->playerState = zelda->script.state;
	}
}

FcVariant fcGameObject_ZeldaGetVar(FcGameObject* gameObject, FcStringId varName)
{
	FcGameObject_Zelda* zelda = (FcGameObject_Zelda*)gameObject;

	FcVariant result = { 0 };

	if (varName == SID("wind-protecting"))
	{
		result.asBool = zelda->playerWindProtecting;
	}
	else if (varName == SID("weapon-equipped"))
	{
		result.asBool = zelda->playerWeaponEquipped;
	}
	else if (varName == SID("idle-anim-name"))
	{
		result.asStringHash = SID("zelda-funny-pose-4");
	}
	else if (varName == SID("is-running"))
	{
		result.asBool = fm_vec4_mag2(&zelda->logicMove) > 0.0f;
	}
	else if (varName == SID("is-jump"))
	{
		result.asBool = zelda->isJump;
	}
	else if (varName == SID("is-grounded"))
	{
		result.asBool = zelda->isGrounded;
	}

	return result;
}

void fcGameObject_ZeldaSetVar(FcGameObject* gameObject, FcStringId varName, FcVariant value)
{
	FcGameObject_Zelda* zelda = (FcGameObject_Zelda*)gameObject;

	FcVariant result = { 0 };

	if (varName == SID("wind-protecting"))
	{
		zelda->playerWindProtecting = value.asBool;
	}
	else if (varName == SID("weapon-equipped"))
	{
		zelda->playerWeaponEquipped = value.asBool;
	}
}

void fcGameObject_ZeldaAnimate(FcGameObject* gameObject, const FcGameObjectAnimateCtx* ctx)
{
	FcGameObject_Zelda* zelda = (FcGameObject_Zelda*)gameObject;

	FcAnimActionAnimate* animateSlot = fcAnimateActionSlotsGetFree(&zelda->animateActionSlots);
	FUR_ASSERT(animateSlot);

	animateSlot->animation = ctx->animClip;
	animateSlot->forceLoop = ctx->forceLoop;
	animateSlot->reserved = true;
	animateSlot->useLoco = ctx->useLocomotion;

	fcAnimCharacterScheduleActionAnimate(zelda->animCharacter, animateSlot, ctx->animArgs);
}

void fcRegisterGameObjectFactories()
{
	// zelda
	{
		FcGameObjectFactory factory = {};
		factory.updateBucket = FG_UPDATE_BUCKET_CHARACTERS;
		factory.fn.init = fcGameObject_ZeldaInit;
		factory.fn.update = fcGameObject_ZeldaUpdate;
		factory.fn.getVar = fcGameObject_ZeldaGetVar;
		factory.fn.animate = fcGameObject_ZeldaAnimate;
		factory.memoryMaxSize = sizeof(FcGameObject_Zelda);
		
		fcGameObjectFactoryRegisterNew(SID("zelda"), factory);
	}
}

struct FcApplication;
struct FcRenderer;

typedef struct FcInputActionSystem
{
	fm_vec2 rightStick;
	fm_vec2 leftStick;
	float rightTrigger;
	float leftTrigger;
	bool triangle;
	bool square;
	bool cross;
	bool circle;

} FcInputActionSystem;

// the engine
struct FcGameEngine
{
	FcApplication* pApp;
	FcDepot* depot;
	FcRenderer* pRenderer;
	FcPhysics* pPhysics;
	FcAnimSystem* animSystem;
	
	std::chrono::system_clock::time_point prevTimePoint;
	f32 globalTime;
	
	FcWorld* pWorld;
	
	FcInputManager* pInputManager;
	FcInputActionSystem inputActionSystem;

	// resource path hashes
	FcFilePath zeldaScriptPath;
	
	// animation
	FcRig* pRig;
	const FcAnimClip* pAnimClipWindProtect;
	const FcAnimClip* pAnimClipHoldSword;
	
	fm_vec4 playerMove;
	
	// camera
	FcCameraSystem* cameraSystem;

	FcGameObject_Zelda* zeldaGameObject;
	
	// gameplay animation states
	FcAnimActionAnimate actionWeaponEquipped;
	FcAnimActionAnimate actionWindProtect;
	
	// skinning
	fm_mat4 skinMatrices[512];
	
	// meshes
	FcRenderProxy* swordMesh;
	FcRenderProxy* chestMesh;
	
	FcRenderProxy* rockMeshes[5];
	FcRenderProxy* blockMesh;
	
	fm_xform blockPositions[2];
	
	FcRenderProxy* zeldaMesh;
	
	// update memory (scratchpad)
	void* scratchpadBuffer;
	u32 scratchpadBufferSize;
	
	// scripts temp
	FcBinaryBuffer zeldaStateScript;

	// hair dangles
	FcPBDDangle* zeldaDangleHairLeft;
	FcPBDDangle* zeldaDangleHairRight;
	u32 zeldaDangleHairLeftIdx1;
	u32 zeldaDangleHairLeftIdx2;
	u32 zeldaDangleHairRightIdx1;
	u32 zeldaDangleHairRightIdx2;
	u32 zeldaHeadIdx;
	u32 zeldaHandRightIdx;
	
	// cape dangles
	FcPBDDangle* zeldaCapeL;
	u32 zeldaCapeIdxL[4];
	
	FcPBDDangle* zeldaCapeC;
	u32 zeldaCapeIdxC[4];
	
	FcPBDDangle* zeldaCapeR;
	u32 zeldaCapeIdxR[4];
	
	u32 zeldaSpineIdx;
	
	// wind
	fm_vec3 windVelocity;
	
	// test bvh
	FcBoundingVolumeHierarchy testBVH;
	
	// debug
	bool debugIsSlowTime;
	bool debugShowFPS;
	bool debugShowMemoryStats;
};

typedef struct FcMainThreadUserData
{
	FcGameEngine* pEngine;
	const FcAllocator* allocator;
} FcMainThreadUserData;

// Furball Cat - Platform
bool fcCreateGameEngine(const FcGameEngineCreateInfo& desc, const FcAllocator* allocator, FcGameEngine** ppEngine)
{
	fcProfilerInit(allocator);
	
	FcGameEngine* pEngine = (FcGameEngine*)FUR_ALLOC_AND_ZERO(sizeof(FcGameEngine), 0, FC_MEMORY_SCOPE_CORE, allocator);
	
	fcStringIdRegisterInit(allocator);

	// mount loose file depot
	{
		FcDepotDesc desc = { 0 };
		desc.type = FC_DEPOT_LOOSE_FILES;

#if PLATFORM_OSX
		desc.path = "../../../../../";
#elif PLATFORM_WINDOWS
		desc.path = "../../../";
#endif

		pEngine->depot = fcDepotMount(&desc, allocator);
	}

	FcApplicationCreateInfo appDesc;
	appDesc.appTitle = desc.m_mainApp.m_title;
	appDesc.viewportWidth = desc.m_mainApp.m_width;
	appDesc.viewportHeight = desc.m_mainApp.m_height;
	appDesc.iconPath = fcDepotGetFilePath(pEngine->depot, "data/icon/furball-cat-icon-128x128.png");
	appDesc.depot = pEngine->depot;

	FcResult res = fcCreateApplication(&appDesc, allocator, &pEngine->pApp);

	if(res == FC_SUCCESS)
	{
		fcCreateInputManager(allocator, &pEngine->pInputManager);
	}
	
	if(res == FC_SUCCESS)
	{
		FcRendererCreateInfo rendererDesc;
		rendererDesc.pApp = pEngine->pApp;
		rendererDesc.depot = pEngine->depot;
		
		res = fcCreateRenderer(&rendererDesc, allocator, &pEngine->pRenderer);
	}
	
	if(res == FC_SUCCESS)
	{
		fcCreatePhysics(allocator, &pEngine->pPhysics);
	}
	
	if(res == FC_SUCCESS)
	{
		fcJobSystemInit(allocator);
	}

	fcScriptInit(allocator);
	
	if(res == FC_SUCCESS)
	{
		*ppEngine = pEngine;
	}
	else
	{
		furSetLastError(fcGetLastError());
		FUR_FREE(pEngine, allocator);
		return false;
	}
	
	// init anim system
	pEngine->animSystem;
	fcCreateAnimSystem(allocator, &pEngine->animSystem);
	
	// create world
	{
		FcWorldCreateInfo worldDesc = {};
		worldDesc.systems.renderer = pEngine->pRenderer;
		worldDesc.systems.animation = pEngine->animSystem;
		worldDesc.systems.camera = pEngine->cameraSystem;

		fcCreateWorld(&worldDesc, allocator, &pEngine->pWorld);
	}
	
	// load scripts
	{
		pEngine->zeldaScriptPath = fcDepotGetFilePath(pEngine->depot, "scripts/zelda-state-script.bin");

		fcBinaryBufferLoad(pEngine->depot, pEngine->zeldaScriptPath, &pEngine->zeldaStateScript, allocator);
		fcResourceRegisterAddScript(&pEngine->pWorld->resources, SID("ss-zelda"), &pEngine->zeldaStateScript);
	}
	
	// init camera
	fcCreateCameraSystem(allocator, &pEngine->cameraSystem);
	
	// default camera
	{
		FcCameraParamsFollow params = {};
		params.height = 0.9f;
		params.zoom = 1.0f;
		params.poleLength = 3.0f;
		params.fov = 45.0f;
		fcCameraEnableFollow(pEngine->cameraSystem, &params, 0.0f);
	}
	
	// init scratchpad buffer
	pEngine->scratchpadBufferSize = 256 * 1024;
	pEngine->scratchpadBuffer = FUR_ALLOC_AND_ZERO(pEngine->scratchpadBufferSize, 16, FC_MEMORY_SCOPE_GLOBAL, allocator);
	
	// init type factories
	fcRegisterGameObjectFactories();
	
	// load resources
	{
#if PLATFORM_OSX
		const char* depotPath = "../../../../../";
#elif PLATFORM_WINDOWS
		const char* depotPath = "../../../";
#endif

		// load rig
		{
			const FcFilePath rigPath = fcDepotGetFilePath(pEngine->depot, "data/rig/zelda-a-pose.rig");
			FcFile* file = fcFileOpen(pEngine->depot, rigPath, "rb");
			
			if(file)
			{
				FcSerializer ser = {};
				ser.file = file;
				ser.isWriting = false;
				pEngine->pRig = (FcRig*)FUR_ALLOC_AND_ZERO(sizeof(FcRig), 0, FC_MEMORY_SCOPE_ANIMATION, allocator);
				fcRigSerialize(&ser, pEngine->pRig, allocator);
			}
		}

		pEngine->pAnimClipWindProtect = fcResourceRegisterLoadAnim(&pEngine->pWorld->resources, pEngine->depot, "zelda-upper-wind-protect", pEngine->pRig, allocator);
		pEngine->pAnimClipHoldSword = fcResourceRegisterLoadAnim(&pEngine->pWorld->resources, pEngine->depot, "zelda-upper-hold-sword", pEngine->pRig, allocator);
		
		fcResourceRegisterLoadAnim(&pEngine->pWorld->resources, pEngine->depot, "zelda-idle-stand-relaxed", pEngine->pRig, allocator);
		fcResourceRegisterLoadAnim(&pEngine->pWorld->resources, pEngine->depot, "zelda-funny-poses", pEngine->pRig, allocator);
		fcResourceRegisterLoadAnim(&pEngine->pWorld->resources, pEngine->depot, "zelda-funny-pose-2", pEngine->pRig, allocator);
		fcResourceRegisterLoadAnim(&pEngine->pWorld->resources, pEngine->depot, "zelda-funny-pose-3", pEngine->pRig, allocator);
		fcResourceRegisterLoadAnim(&pEngine->pWorld->resources, pEngine->depot, "zelda-funny-pose-4", pEngine->pRig, allocator);
		fcResourceRegisterLoadAnim(&pEngine->pWorld->resources, pEngine->depot, "zelda-loco-run-relaxed", pEngine->pRig, allocator);
		fcResourceRegisterLoadAnim(&pEngine->pWorld->resources, pEngine->depot, "zelda-run-to-idle-sharp", pEngine->pRig, allocator);
		fcResourceRegisterLoadAnim(&pEngine->pWorld->resources, pEngine->depot, "zelda-loco-idle-to-run-0", pEngine->pRig, allocator);
		fcResourceRegisterLoadAnim(&pEngine->pWorld->resources, pEngine->depot, "zelda-loco-jump-in-place", pEngine->pRig, allocator);
		fcResourceRegisterLoadAnim(&pEngine->pWorld->resources, pEngine->depot, "zelda-loco-jump", pEngine->pRig, allocator);
		fcResourceRegisterLoadAnim(&pEngine->pWorld->resources, pEngine->depot, "zelda-additive", pEngine->pRig, allocator);
		fcResourceRegisterLoadAnim(&pEngine->pWorld->resources, pEngine->depot, "zelda-a-pose", pEngine->pRig, allocator);
		fcResourceRegisterLoadAnim(&pEngine->pWorld->resources, pEngine->depot, "zelda-face-idle", pEngine->pRig, allocator);
		fcResourceRegisterLoadAnim(&pEngine->pWorld->resources, pEngine->depot, "zelda-idle-stand-01", pEngine->pRig, allocator);
		fcResourceRegisterLoadAnim(&pEngine->pWorld->resources, pEngine->depot, "zelda-hands-idle", pEngine->pRig, allocator);
		fcResourceRegisterLoadAnim(&pEngine->pWorld->resources, pEngine->depot, "zelda-wind-01", pEngine->pRig, allocator);
		fcResourceRegisterLoadAnim(&pEngine->pWorld->resources, pEngine->depot, "zelda-jump-loop", pEngine->pRig, allocator);
		
		// load meshes
		{
			FcRenderMeshLoadCtx meshCtx = {};
			meshCtx.path = fcDepotGetFilePath(pEngine->depot, "data/mesh/zelda-sword.mesh");
			FcFilePath texturePaths[] = {fcDepotGetFilePath(pEngine->depot, "data/texture/melee_diff.png")};
			const i32 textureIndices[] = {0};
			meshCtx.texturePaths = texturePaths;
			meshCtx.numTextures = FUR_ARRAY_SIZE(texturePaths);
			meshCtx.textureIndices = textureIndices;
			meshCtx.numTextureIndices = FUR_ARRAY_SIZE(textureIndices);
			pEngine->swordMesh = fcRendererLoadMesh(pEngine->pRenderer, pEngine->depot, &meshCtx, allocator);
		}
		
		// load chest mesh
		{
			FcRenderMeshLoadCtx meshCtx = {};
			meshCtx.path = fcDepotGetFilePath(pEngine->depot, "data/mesh/chest.mesh");
			FcFilePath texturePaths[] = { fcDepotGetFilePath(pEngine->depot, "data/texture/chest_albedo.png")};
			const i32 textureIndices[] = {0};
			meshCtx.texturePaths = texturePaths;
			meshCtx.numTextures = FUR_ARRAY_SIZE(texturePaths);
			meshCtx.textureIndices = textureIndices;
			meshCtx.numTextureIndices = FUR_ARRAY_SIZE(textureIndices);
			pEngine->chestMesh = fcRendererLoadMesh(pEngine->pRenderer, pEngine->depot, &meshCtx, allocator);
		}
		
		// load block mesh
		{
			FcRenderMeshLoadCtx meshCtx = {};
			meshCtx.path = fcDepotGetFilePath(pEngine->depot, "data/mesh/skull_block_PBR_fc.mesh");
			FcFilePath texturePaths[] = { fcDepotGetFilePath(pEngine->depot, "data/texture/b_stone1_Color.png")};
			const i32 textureIndices[] = {0};
			meshCtx.texturePaths = texturePaths;
			meshCtx.numTextures = FUR_ARRAY_SIZE(texturePaths);
			meshCtx.textureIndices = textureIndices;
			meshCtx.numTextureIndices = FUR_ARRAY_SIZE(textureIndices);
			pEngine->blockMesh = fcRendererLoadMesh(pEngine->pRenderer, pEngine->depot, &meshCtx, allocator);
		}
		
		// set block positions and create colliders
		{
			pEngine->blockPositions[0] = {{4.0f, 4.0f, 0.0f, 1.0f}, {0.0f, 0.0f, 0.0f, 1.0f}};
			pEngine->blockPositions[1] = {{6.0f, 4.0f, 0.5f, 1.0f}, {0.0f, 0.0f, 0.0f, 1.0f}};
			
			const fm_vec3 halfExtents = {0.5f, 0.5f, 0.5f};
			
			for(i32 i=0; i<2; ++i)
			{
				fcPhysicsAddStaticBox(pEngine->pPhysics, &pEngine->blockPositions[i], &halfExtents, allocator);
			}
		}
		
		// load rock meshes
		for(u32 i=0; i<5; ++i)
		{
			char txtPath[256];
			sprintf(txtPath, "data/mesh/rock-0%i.mesh", i+1);
			
			char txtTexturePath[256];
			sprintf(txtTexturePath, "data/texture/rock-0%i.png", i+1);
			
			FcRenderMeshLoadCtx meshCtx = {};
			meshCtx.path = fcDepotGetFilePath(pEngine->depot, txtPath);
			FcFilePath texturePaths[] = { fcDepotGetFilePath(pEngine->depot, txtTexturePath) };
			const i32 textureIndices[] = {0};
			meshCtx.texturePaths = texturePaths;
			meshCtx.numTextures = FUR_ARRAY_SIZE(texturePaths);
			meshCtx.textureIndices = textureIndices;
			meshCtx.numTextureIndices = FUR_ARRAY_SIZE(textureIndices);
			pEngine->rockMeshes[i] = fcRendererLoadMesh(pEngine->pRenderer, pEngine->depot, &meshCtx, allocator);
		}
		
		// load zelda mesh
		{
			FcRenderMeshLoadCtx meshCtx = {};
			meshCtx.path = fcDepotGetFilePath(pEngine->depot, "data/mesh/zelda-mesh.mesh");
			FcFilePath texturePaths[] = {
				fcDepotGetFilePath(pEngine->depot, "data/texture/zelda_diff.png"),
				fcDepotGetFilePath(pEngine->depot, "data/texture/hair_diff.png"),
				fcDepotGetFilePath(pEngine->depot, "data/texture/eyes_diff2.png")
			};
			const i32 textureIndices[] = {0, 0, 1, 0, 2, 0, 1};
			meshCtx.texturePaths = texturePaths;
			meshCtx.numTextures = FUR_ARRAY_SIZE(texturePaths);
			meshCtx.textureIndices = textureIndices;
			meshCtx.numTextureIndices = FUR_ARRAY_SIZE(textureIndices);
			meshCtx.isSkinned = true;
			meshCtx.boneNames = pEngine->pRig->boneNameHashes;
			meshCtx.numBones = pEngine->pRig->numBones;
			pEngine->zeldaMesh = fcRendererLoadMesh(pEngine->pRenderer, pEngine->depot, &meshCtx, allocator);
		}
	}
	
	// add resources to world resource register
	{
		fcResourceRegisterAddRig(&pEngine->pWorld->resources, SID("zelda-rig"), pEngine->pRig);
		fcResourceRegisterAddMesh(&pEngine->pWorld->resources, SID("zelda-mesh"), pEngine->zeldaMesh);
	}
	
	// init game
	{
		// create Zelda legacy code
		pEngine->actionWeaponEquipped.animation = pEngine->pAnimClipHoldSword;
		pEngine->actionWeaponEquipped.forceLoop = true;
		
		pEngine->actionWindProtect.animation = pEngine->pAnimClipWindProtect;
		pEngine->actionWindProtect.forceLoop = true;
	}

	// init special bone indices
	{
		const FcStringId handRight = SID("Bip001_Hand_R");
		
		for(i32 i=0; i<pEngine->pRig->numBones; ++i)
		{
			if(pEngine->pRig->boneNameHashes[i] == handRight)
				pEngine->zeldaHandRightIdx = i;
		}
	}
	
	// init hair dangles
	{
		FcPBDDangleDesc desc;
		desc.frequency = 60.0f;
		desc.numParticles = 3;
		desc.dampingCoef = 0.96f;
		
		fcCreatePBDDangle(&desc, allocator, &pEngine->zeldaDangleHairLeft);
		fcCreatePBDDangle(&desc, allocator, &pEngine->zeldaDangleHairRight);
		
		const FcStringId hair_r = SID("Bip001_Hair_R");
		const FcStringId hair_r2 = SID("Bip001_Hair1_R");
		const FcStringId hair_l = SID("Bip001_Hair_L");
		const FcStringId hair_l2 = SID("Bip001_Hair1_L");
		const FcStringId head = SID("Bip001_Head");
		const FcStringId spine = SID("Bip001_Spine");
		
		for(i32 i=0; i<pEngine->pRig->numBones; ++i)
		{
			const FcStringId name = pEngine->pRig->boneNameHashes[i];
			if(name == hair_r)
				pEngine->zeldaDangleHairRightIdx1 = i;
			else if(name == hair_r2)
				pEngine->zeldaDangleHairRightIdx2 = i;
			else if(name == hair_l)
				pEngine->zeldaDangleHairLeftIdx1 = i;
			else if(name == hair_l2)
				pEngine->zeldaDangleHairLeftIdx2 = i;
			else if(name == head)
				pEngine->zeldaHeadIdx = i;
			else if(name == spine)
				pEngine->zeldaSpineIdx = i;
		}
		
		fm_xform refPoseLeft2 = pEngine->pRig->refPose[pEngine->zeldaDangleHairLeftIdx2];
		fm_xform refPoseRight2 = pEngine->pRig->refPose[pEngine->zeldaDangleHairRightIdx2];
		
		refPoseLeft2.pos.w = 0.0f;
		refPoseRight2.pos.w = 0.0f;
		
		const f32 dLeft = fm_vec4_mag(&refPoseLeft2.pos);
		const f32 dRight = fm_vec4_mag(&refPoseRight2.pos);
		
		pEngine->zeldaDangleHairLeft->d[0] = dLeft;
		pEngine->zeldaDangleHairLeft->d[1] = dLeft;
		
		pEngine->zeldaDangleHairRight->d[0] = dRight;
		pEngine->zeldaDangleHairRight->d[1] = dRight;
	}
	
	// init cape dangles
	{
		FcPBDDangleDesc desc;
		desc.frequency = 60.0f;
		desc.numParticles = 4;
		desc.dampingCoef = 0.96f;
		
		fcCreatePBDDangle(&desc, allocator, &pEngine->zeldaCapeL);
		fcCreatePBDDangle(&desc, allocator, &pEngine->zeldaCapeC);
		fcCreatePBDDangle(&desc, allocator, &pEngine->zeldaCapeR);
		
		const FcStringId cape_names_l[4] = {
			SID("Bip001_Cape_L"),
			SID("Bip001_Cape1_L"),
			SID("Bip001_Cape2_L"),
			SID("Bip001_Cape3_L")
		};
		
		const FcStringId cape_names_c[4] = {
			SID("Bip001_Cape_C"),
			SID("Bip001_Cape1_C"),
			SID("Bip001_Cape2_C"),
			SID("Bip001_Cape3_C")
		};
		
		const FcStringId cape_names_r[4] = {
			SID("Bip001_Cape_R"),
			SID("Bip001_Cape1_R"),
			SID("Bip001_Cape2_R"),
			SID("Bip001_Cape3_R")
		};
		
		for(i32 i=0; i<pEngine->pRig->numBones; ++i)
		{
			const FcStringId name = pEngine->pRig->boneNameHashes[i];
			
			for(i32 j=0; j<4; ++j)
			{
				if(name == cape_names_l[j])
				{
					pEngine->zeldaCapeIdxL[j] = i;
				}
				else if(name == cape_names_c[j])
				{
					pEngine->zeldaCapeIdxC[j] = i;
				}
				else if(name == cape_names_r[j])
				{
					pEngine->zeldaCapeIdxR[j] = i;
				}
			}
		}
		
		for(u32 j=0; j<4; ++j)
		{
			fm_xform refPose = pEngine->pRig->refPose[pEngine->zeldaCapeIdxL[j]];
			refPose.pos.w = 0.0f;
			pEngine->zeldaCapeL->d[j] = fm_vec4_mag(&refPose.pos);
		}
		
		for(u32 j=0; j<4; ++j)
		{
			fm_xform refPose = pEngine->pRig->refPose[pEngine->zeldaCapeIdxC[j]];
			refPose.pos.w = 0.0f;
			pEngine->zeldaCapeC->d[j] = fm_vec4_mag(&refPose.pos);
		}
		
		for(u32 j=0; j<4; ++j)
		{
			fm_xform refPose = pEngine->pRig->refPose[pEngine->zeldaCapeIdxR[j]];
			refPose.pos.w = 0.0f;
			pEngine->zeldaCapeR->d[j] = fm_vec4_mag(&refPose.pos);
		}
	}
	
	// spawn zelda game object
	{
		const i32 props_num = 1;
		static FcStringId prop_names[props_num] = {SID("state-script")};
		static FcSpawninfoPropValue prop_values[props_num] = {};
		prop_values[0].asStringId = SID("ss-zelda");
		
		static FcSpawner spawner = {};
		spawner.name = SID("zelda");
		spawner.typeName = SID("zelda");
		spawner.info.gameObjectName = SID("zelda");
		spawner.info.props.num = props_num;
		spawner.info.props.names = prop_names;
		spawner.info.props.values = prop_values;

		FcSpawnDesc spawnDesc = {};
		spawnDesc.spawner = &spawner;
		fm_xform_identity(&spawnDesc.initialTransform);
		
		fcSpawn(&spawnDesc, pEngine->pWorld);
	}
	
	// test BVH
	{
		fm_box boxes[] = {
			{{0.0f, 0.0f, 0.0f}, {1.0f, 1.0f, 1.0f}},
			{{4.0f, 0.0f, 0.0f}, {1.0f, 1.0f, 1.0f}},
			{{0.0f, 4.0f, 0.0f}, {1.0f, 1.0f, 1.0f}},
			{{0.4f, 6.0f, 1.0f}, {0.5f, 0.4f, 0.3f}},
			{{-1.3f, -2.0f, 1.0f}, {0.7f, 0.2f, 0.5f}},
			{{-6.0f, -7.0f, 3.0f}, {0.8f, 1.3f, 2.1f}},
			{{-3.0f, -4.0f, 0.7f}, {1.4f, 1.3f, 1.2f}},
			{{0.0f, 0.0f, 4.0f}, {1.0f, 1.0f, 1.0f}}
		};
		
		const u32 numBoxes = FUR_ARRAY_SIZE(boxes);
		
		FcMemArenaAllocator memArena = fcMemArenaMake(pEngine->scratchpadBuffer, pEngine->scratchpadBufferSize);
		
		FcBoundingVolumeHierarchyDesc bvhCtx = {};
		bvhCtx.numObjects = numBoxes;
		bvhCtx.objectBoxes = boxes;
		bvhCtx.arenaAlloc = &memArena;
		
		fcBoundingVolumeHierarchyCreate(&bvhCtx, &pEngine->testBVH, allocator);
	}
		
	return true;
}

bool g_drawDevMenu = false;
i32 g_devMenuOption = 0;
bool g_devMenuOptionClick = false;

void fcInputActionsUpdate(FcGameEngine* pEngine, f32 dt)
{
	bool actionPressed = false;
	
	bool triangleActionPressed = false;
	bool circleActionPressed = false;
	bool squareActionPressed = false;
	bool leftThumbPressed = false;
	bool startPressed = false;

	static bool wasLeftThumbPressed = false;
	static bool wasStartPressed = false;
	
	bool dpadUpPressed = false;
	bool dpadDownPressed = false;
	
	g_devMenuOptionClick = false;
	
	static f32 rightAnalogX = 0.0f;
	static f32 rightAnalogY = 0.0f;
	static f32 leftAnalogX = 0.0f;
	static f32 leftAnalogY = 0.0f;
	static f32 rightTrigger = 0.0f;
	static f32 leftTrigger = 0.0f;
	
	FcInputEvent inputEvents[40];
	const u32 numEventsCollected = fcInputManagerGetEvents(pEngine->pInputManager, inputEvents, 40, 0);
	for(u32 i=0; i<numEventsCollected; ++i)
	{
		if(inputEvents[i].eventID == Gamepad_leftThumb)
		{
			leftThumbPressed = inputEvents[i].value > 0.5f;
			wasLeftThumbPressed = leftThumbPressed;
		}
		else if(inputEvents[i].eventID == Gamepad_start)
		{
			startPressed = inputEvents[i].value > 0.5f;
			wasStartPressed = startPressed;
		}
		else if(inputEvents[i].eventID == Gamepad_dpadUp)
		{
			dpadUpPressed = inputEvents[i].value > 0.5f;
		}
		else if(inputEvents[i].eventID == Gamepad_dpadDown)
		{
			dpadDownPressed = inputEvents[i].value > 0.5f;
		}
		else if(inputEvents[i].eventID == Gamepad_cross)
		{
			actionPressed = inputEvents[i].value > 0.5f;
		}
		else if(inputEvents[i].eventID == Gamepad_circle)
		{
			circleActionPressed = inputEvents[i].value > 0.5f;
		}
		else if (inputEvents[i].eventID == Gamepad_square)
		{
			squareActionPressed = inputEvents[i].value > 0.5f;
		}
		else if(inputEvents[i].eventID == Gamepad_triangle)
		{
			triangleActionPressed = inputEvents[i].value > 0.5f;
		}
		else if(inputEvents[i].eventID == Gamepad_rightAnalogX)
		{
			rightAnalogX = fm_snap_near_zero(inputEvents[i].value, 0.15f);
		}
		else if(inputEvents[i].eventID == Gamepad_rightAnalogY)
		{
			rightAnalogY = fm_snap_near_zero(inputEvents[i].value, 0.15f);
		}
		else if(inputEvents[i].eventID == Gamepad_leftAnalogX)
		{
			leftAnalogX = -fm_snap_near_zero(inputEvents[i].value, 0.15f);
		}
		else if(inputEvents[i].eventID == Gamepad_leftAnalogY)
		{
			leftAnalogY = fm_snap_near_zero(inputEvents[i].value, 0.15f);
		}
		else if(inputEvents[i].eventID == Gamepad_rightTrigger)
		{
			rightTrigger = fm_snap_near_zero(inputEvents[i].value, 0.15f);
		}
		else if(inputEvents[i].eventID == Gamepad_leftTrigger)
		{
			leftTrigger = fm_snap_near_zero(inputEvents[i].value, 0.15f);
		}
	}
	
	if((startPressed && wasLeftThumbPressed) || (wasStartPressed && leftThumbPressed))
	{
		g_drawDevMenu = !g_drawDevMenu;
	}

	if(g_drawDevMenu)
	{
		if(dpadDownPressed)
		{
			g_devMenuOption += 1;
		}

		if(dpadUpPressed)
		{
			g_devMenuOption -= 1;
		}
		
		if(actionPressed)
		{
			g_devMenuOptionClick = actionPressed;
		}
	}
	else if(fcProfilerIsDrawOn())
	{
		if (actionPressed)
		{
			fcProfilerTogglePause();
		}

		const f32 zoomDelta = leftTrigger - rightTrigger;
		const f32 panDelta = rightAnalogX;
		fcProfilerZoomAndPanDelta(zoomDelta, panDelta);
	}
	else // block all the actions when debug menu is enabled
	{
		FcInputActionSystem* actionSystem = &pEngine->inputActionSystem;
		actionSystem->rightStick.x = rightAnalogX;
		actionSystem->rightStick.y = rightAnalogY;
		actionSystem->leftStick.x = leftAnalogX;
		actionSystem->leftStick.y = leftAnalogY;
		actionSystem->triangle = triangleActionPressed;
		actionSystem->circle = circleActionPressed;
		actionSystem->cross = actionPressed;
		actionSystem->square = squareActionPressed;
		actionSystem->rightTrigger = rightTrigger;
		actionSystem->leftTrigger = leftTrigger;
	}
}

void fcDevMenuReloatScripts(FcGameEngine* pEngine, const FcAllocator* allocator)
{
	fcBinaryBufferRelease(&pEngine->zeldaStateScript, allocator);
	fcBinaryBufferLoad(pEngine->depot, pEngine->zeldaScriptPath, &pEngine->zeldaStateScript, allocator);
}

void fcDevMenuShowPlayerAnimState(FcGameEngine* pEngine, const FcAllocator* allocator)
{
	//pEngine->zeldaGameObject.showAnimStateDebug = !pEngine->zeldaGameObject.showAnimStateDebug;
}

void fcDevMenuShowProfiler(FcGameEngine* pEngine, const FcAllocator* allocator)
{
	fcProfilerToggleDraw();
}

void fcDevMenuSlowTime(FcGameEngine* pEngine, const FcAllocator* allocator)
{
	pEngine->debugIsSlowTime = !pEngine->debugIsSlowTime;
}

void fcDevMenuShowFPS(FcGameEngine* pEngine, const FcAllocator* allocator)
{
	pEngine->debugShowFPS = !pEngine->debugShowFPS;
}

void fcDevMenuShowMemoryStats(FcGameEngine* pEngine, const FcAllocator* allocator)
{
	pEngine->debugShowMemoryStats = !pEngine->debugShowMemoryStats;
}

typedef struct FcDevMenuOption
{
	const char* name;
	void (*func)(FcGameEngine* pEngine, const FcAllocator* allocator);
} FcDevMenuOption;

void fcDrawDebugMenu(FcGameEngine* pEngine, const FcAllocator* allocator)
{
	const f32 color[4] = {0.8f, 0.8f, 0.8f, 1.0f};
	const f32 colorCursor[4] = {0.9f, 0.9f, 0.9f, 1.0f};
	const f32 colorLabel[4] = FUR_COLOR_CYAN;
	const f32 colorSelected[4] = FUR_COLOR_YELLOW;
	
	if(g_drawDevMenu)
	{
		const f32 text_scale = 0.7f;
		const f32 lineHeight = fcDebugGetTextLineHeight(text_scale);
		const f32 ident = fcDebugGetTextLineHeight(text_scale);

		f32 x = 70.0f;
		f32 y = 70.0f;
		fcDebugApplyAnchor(&x, &y, FC_DBG_ANCHOR_LEFT_UP_CORNER);
		
		FcDevMenuOption options[] = {
			{"quick-fps-mem", fcDevMenuShowFPS},
			{"reload-scripts", fcDevMenuReloatScripts},
			{"slow-time", fcDevMenuSlowTime},
			{"show-player-anim-state", fcDevMenuShowPlayerAnimState},
			{"profiler", fcDevMenuShowProfiler},
			{"memory-stats", fcDevMenuShowMemoryStats}
		};
		
		const u32 numOptions = FUR_ARRAY_SIZE(options);
		
		if(g_devMenuOption < 0)
			g_devMenuOption = numOptions-1;
		else if(g_devMenuOption >= numOptions)
			g_devMenuOption = 0;
		
		const f32 bgColor[4] = {0.2f, 0.2f, 0.2f, 0.8f};
		
		fcDebugRect(x - 20.0f, y - 20.0f, 450.0f, 110.0f + 28.0f * numOptions, bgColor);
		
		fcDebugText(x, y, "Dev Menu:", colorLabel, text_scale);
		
		for(u32 i=0; i<numOptions; ++i)
		{
			const bool isSelected = (i == g_devMenuOption);
			
			fcDebugText(x + ident, y + lineHeight * (1+i), options[i].name, isSelected ? colorSelected : color, text_scale);
			
			if(isSelected)
			{
				fcDebugText(x + ident * 0.2f, y + lineHeight * (1+i), ">", colorCursor, text_scale);
			}
			
			// execute option
			if(isSelected && g_devMenuOptionClick)
			{
				if(options[i].func)
				{
					(*options[i].func)(pEngine, allocator);
				}
			}
		}
	}
}

void fcGameplayUpdate(FcGameEngine* pEngine, f32 dt)
{
	uint64_t globalTime = (uint64_t)(pEngine->globalTime * 1000000);
	FcGameObject_Zelda* zelda = pEngine->zeldaGameObject;

	if (!zelda)
		return;

	zelda->animCharacter->globalTime = globalTime;
	
	if(zelda->playerState == SID("idle"))
	{
		// on update - upper-body layer in this case
		if(pEngine->inputActionSystem.triangle || zelda->equipItemNow)
		{
			zelda->equipItemNow = false;
			
			if(zelda->playerWindProtecting)
			{
				zelda->playerWindProtecting = false;
			}
			
			if(zelda->playerWeaponEquipped)
			{
				FcAnimActionArgs args = {};
				args.fadeInSec = 0.5f;
				args.layer = FA_CHAR_LAYER_PARTIAL;
				fcAnimCharacterScheduleActionNone(zelda->animCharacter, &args);
				
				zelda->playerWeaponEquipped = false;
			}
			else
			{
				FcAnimActionArgs args = {};
				args.fadeInSec = 0.5f;
				args.layer = FA_CHAR_LAYER_PARTIAL;
				fcAnimCharacterScheduleActionAnimate(zelda->animCharacter, &pEngine->actionWeaponEquipped, &args);
				
				zelda->playerWeaponEquipped = true;
			}
		}
	}
	
	if(pEngine->inputActionSystem.circle)
	{
		if(zelda->playerWeaponEquipped)
		{
			zelda->playerWeaponEquipped = false;
		}
		
		if(zelda->playerWindProtecting)
		{
			FcAnimActionArgs args = {};
			args.fadeInSec = 0.5f;
			args.layer = FA_CHAR_LAYER_PARTIAL;
			fcAnimCharacterScheduleActionNone(zelda->animCharacter, &args);
			
			zelda->playerWindProtecting = false;
		}
		else
		{
			FcAnimActionArgs args = {};
			args.fadeInSec = 0.5f;
			args.layer = FA_CHAR_LAYER_PARTIAL;
			fcAnimCharacterScheduleActionAnimate(zelda->animCharacter, &pEngine->actionWindProtect, &args);
			
			zelda->playerWindProtecting = true;
		}
	}
	
	// is grounded raycast check
	{
		bool isGrounded = false;
		
		fm_vec4 pos = zelda->info.transform.pos;
		pos.z += 0.1f;
		const fm_vec4 dir = {0.0f, 0.0f, -1.0f, 0.0f};
		const f32 distance = 0.12f;
		
		FcRaycastHit hit = {};
		const bool isHit = fcPhysicsRaycast(pEngine->pPhysics, &pos, &dir, distance, &hit);
		if(isHit)
		{
			isGrounded = true;
			const f32 extent[3] = {0.02f, 0.02f, 0.02f};
			const f32 color[4] = FUR_COLOR_RED;
			fcDebugBoxWire(&hit.pos.x, extent, color);
		}
		
		zelda->isGrounded = isGrounded;
	}
	
	// jumping
	if(pEngine->inputActionSystem.cross && zelda->isGrounded)
	{
		zelda->isJump = true;
	}
	else
	{
		zelda->isJump = false;
	}
}

void fcAnimationUpdate(FcGameEngine* pEngine, f32 dt)
{
	// set last known world position for character
	fm_xform playerLocator;
	FcPhysicsPlayerInfo playerPhysics;
	playerPhysics.locator = &playerLocator;
	fcPhysicsGetPlayerInfo(pEngine->pPhysics, &playerPhysics);

	FcGameObject_Zelda* zelda = pEngine->zeldaGameObject;
	
	zelda->animCharacter->animInfo.worldPos.x = playerLocator.pos.x;
	zelda->animCharacter->animInfo.worldPos.y = playerLocator.pos.y;
	zelda->animCharacter->animInfo.worldPos.z = playerLocator.pos.z;
	
	FcMemArenaAllocator arenaAlloc = fcMemArenaMake(pEngine->scratchpadBuffer, pEngine->scratchpadBufferSize);
	
	FcAnimSystemUpdateCtx animCtx = {};
	animCtx.dt = dt;
	animCtx.globalTime = pEngine->globalTime;
	animCtx.arenaAlloc = &arenaAlloc;
	
	fcAnimSystemUpdate(pEngine->animSystem, &animCtx);
}

void fcDrawDebugMat4(const fm_mat4* m)
{
	const f32 pos[3] = {m->w.x, m->w.y, m->w.z};
	const f32 scale = 0.1f;
	const f32 axisX[3] = {pos[0] + m->x.x * scale, pos[1] + m->x.y * scale, pos[2] + m->x.z * scale};
	const f32 axisY[3] = {pos[0] + m->y.x * scale, pos[1] + m->y.y * scale, pos[2] + m->y.z * scale};
	const f32 axisZ[3] = {pos[0] + m->z.x * scale, pos[1] + m->z.y * scale, pos[2] + m->z.z * scale};
	
	const f32 red[4] = FUR_COLOR_RED;
	const f32 green[4] = FUR_COLOR_GREEN;
	const f32 blue[4] = FUR_COLOR_BLUE;
	
	fcDebugLine(pos, axisX, red);
	fcDebugLine(pos, axisY, green);
	fcDebugLine(pos, axisZ, blue);
}

void fcDebugStatsForMemToText(FcMemoryScope scope, char* txt, const char* displayName)
{
	FcMemStats stats = fcMemoryStatsForScope(scope);
	
	const f32 numMBs = ((f32)stats.numBytesUsed) / (1024.0f * 1024.0f);
	const f32 numCapacityMBs = ((f32)stats.numBytesCapacity) / (1024.0f * 1024.0f);
	
	const char* name = fcMemoryGetScopeDebugName(scope);
	
	sprintf(txt, "%s: %1.2f / %1.2f MBs (%u allocs)", displayName ? displayName : name, numMBs, numCapacityMBs, stats.numAllocs);
}

FUR_JOB_ENTRY_POINT(test_job)
{
	const i32 numStuff = *FUR_JOB_USER_DATA(i32);
	
	FUR_PROFILE("test-job")
	{
		fm_mat4 mats[12];
		fm_mat4 res;
		
		for(i32 i=0; i<12; ++i)
		{
			fm_mat4_identity(&mats[i]);
		}
		
		for(i32 i=0; i<numStuff; ++i)
		{
			for(i32 j=0; j<numStuff; ++j)
			{
				fm_mat4_mul(&mats[i%12], &mats[j%12], &res);
				fm_mat4_identity(&res);
			}
		}
	}
}

FUR_JOB_ENTRY_POINT(test_job_with_sub_job)
{
	const i32 numStuff = *FUR_JOB_USER_DATA(i32);
	
	FUR_PROFILE("test-job-sub")
	{
		fm_mat4 mats[12];
		fm_mat4 res;
		
		for(i32 i=0; i<12; ++i)
		{
			fm_mat4_identity(&mats[i]);
		}
		
		{
			i32 subNumStuff = 40;
			FcJobDecl job = {test_job, &subNumStuff};
			FcJobCounter* counter = {};
			fcRunJobs(&job, 1, &counter);
			fcWaitForCounterAndFree(counter);
		}
		
		for(i32 i=0; i<numStuff; ++i)
		{
			for(i32 j=0; j<numStuff; ++j)
			{
				fm_mat4_mul(&mats[i%12], &mats[j%12], &res);
			}
		}
	}
}

void fcGameEngineMainUpdate(FcGameEngine* pEngine, f32 dt, const FcAllocator* allocator)
{
	// show debug FPS
	if(pEngine->debugShowFPS)
	{
		f32 text_scale = 0.7f;
		const f32 offset_line = fcDebugGetTextLineHeight(text_scale);

		f32 x = 0;
		f32 y = 0;
		fcDebugApplyAnchor(&x, &y, FC_DBG_ANCHOR_LEFT_UP_CORNER);

		{
			const f32 fps = 1.0f / dt;
			const f32 ms = dt * 1000.0f;
			
			char txt[50];
			sprintf(txt, "CPU: %1.1f fps (%1.1f ms)", fps, ms);
			
			const f32 green[4] = FUR_COLOR_GREEN;
			const f32 yellow[4] = FUR_COLOR_YELLOW;
			const f32 red[4] = FUR_COLOR_RED;
			fcDebugText(x, y, txt, ms < 16 ? green : ms < 33 ? yellow : red, text_scale);
		}
		{
			FcMemStats stats = fcMemoryStats();
			
			const f32 numMBs = ((f32)stats.numBytesUsed) / (1024.0f * 1024.0f);
			const f32 numCapacityMBs = ((f32)stats.numBytesCapacity) / (1024.0f * 1024.0f);
			
			char txt[50];
			sprintf(txt, "MEM: %1.2f / %1.2f MBs (%u allocs)", numMBs, numCapacityMBs, stats.numAllocs);
			
			const f32 green[4] = FUR_COLOR_GREEN;
			const f32 yellow[4] = FUR_COLOR_YELLOW;
			const f32 red[4] = FUR_COLOR_RED;
			fcDebugText(x, y + offset_line, txt, numMBs < numCapacityMBs * 0.95f ? green : numMBs < numCapacityMBs ? yellow : red, text_scale);
		}
	}
	
	// show memory statistics for each memory scope
	if(pEngine->debugShowMemoryStats)
	{
		const f32 white[4] = FUR_COLOR_WHITE;
		i32 textLineCounter = 0;

		f32 x = 20.0f;
		f32 y = 400.0f;
		fcDebugApplyAnchor(&x, &y, FC_DBG_ANCHOR_LEFT_UP_CORNER);
		const f32 text_scale = 0.7f;
		const f32 line_height = fcDebugGetTextLineHeight(text_scale);
		
		fcDebugText(x, y - line_height, "-[ Retail Memory ]-----------------------------------------------", white, text_scale);
		
		char txt[128] = {};
		
		fcDebugStatsForMemToText(FC_MEMORY_SCOPE_SYSTEM, txt, "OS Memory");
		fcDebugText(x, y + textLineCounter * line_height, txt, white, text_scale);
		textLineCounter++;
		
		fcDebugStatsForMemToText(FC_MEMORY_SCOPE_GLOBAL, txt, "Code And Static Data");
		fcDebugText(x, y + textLineCounter * line_height, txt, white, text_scale);
		textLineCounter++;
		
		textLineCounter++;
		fcDebugText(x, y + textLineCounter * line_height, "-[ Debug Memory ]------------------------------------------------", white, text_scale);
		textLineCounter++;
		
		fcDebugStatsForMemToText(FC_MEMORY_SCOPE_DEBUG, txt, "Debug Memory");
		fcDebugText(x, y + textLineCounter * line_height, txt, white, text_scale);
		textLineCounter++;
	}
	
	// slow-time debug mode - this needs to be after debugShowFPS
	if(pEngine->debugIsSlowTime)
	{
		const f32 color[4] = FUR_COLOR_RED;
		const f32 scale = 0.7f;
		f32 x = -7.0f * fcDebugGetTextLineHeight(scale);
		f32 y = -fcDebugGetTextLineHeight(scale);
		fcDebugApplyAnchor(&x, &y, FC_DBG_ANCHOR_RIGHT_BOTTOM_CORNER);
		fcDebugText(x, y, "slow-time ON", color, scale);
		dt *= 0.3f;
	}
	
	pEngine->globalTime += dt;
	
	// input
	FUR_PROFILE("actions-update")
	{
		fcInputManagerUpdate(pEngine->pInputManager, pEngine->globalTime);
		fcInputActionsUpdate(pEngine, dt);
	}
	
	// debug/dev menu
	fcDrawDebugMenu(pEngine, allocator);
	
	FUR_PROFILE("spawning")
	{
		fcUpdateSpawning(pEngine->pWorld);
		if (!pEngine->zeldaGameObject)
		{
			for (i32 i = 0; i < pEngine->pWorld->gameObjects.num; ++i)
			{
				if (pEngine->pWorld->gameObjects.ptr[i]->name == SID("zelda"))
				{
					pEngine->zeldaGameObject = (FcGameObject_Zelda*)pEngine->pWorld->gameObjects.ptr[i];
					break;
				}
			}
		}
	}

	// game
	FUR_PROFILE("gameplay-update")
	{
		fcGameplayUpdate(pEngine, dt);
		
		FcWorldUpdateCtx worldUpdateCtx = {};
		worldUpdateCtx.dt = dt;
		fcWorldUpdate(pEngine->pWorld, &worldUpdateCtx, FG_UPDATE_BUCKET_CHARACTERS);
	}
	
	// test set look-at point
	{
		// get zelda position
		fm_xform playerLocator;
		FcPhysicsPlayerInfo playerPhysics;
		playerPhysics.locator = &playerLocator;
		fcPhysicsGetPlayerInfo(pEngine->pPhysics, &playerPhysics);
		
		static f32 time = 0.0f;
		time += dt;
		
		// convert world space look-at point to model space of player
		fm_vec4 lookAtPoint = {2.0f * sinf(time), 2.0f * cosf(time), 1.0f + 1.0f * sinf(time * 0.6f), 1.0f};	// in world space
		
		f32 color[4] = FUR_COLOR_MAGENTA;
		fcDebugSphereWire(&lookAtPoint.x, 0.1f, color);

		FcGameObject_Zelda* zelda = pEngine->zeldaGameObject;
		
		const f32 distanceToLookAtPoint = fm_vec4_distance(&lookAtPoint, &playerLocator.pos);
		
		if(zelda->animCharacter->animInfo.useLookAt)
		{
			zelda->animCharacter->animInfo.useLookAt = distanceToLookAtPoint < 10.0f;
		}
		else
		{
			zelda->animCharacter->animInfo.useLookAt = distanceToLookAtPoint < 6.0f;
		}
		
		zelda->animCharacter->animInfo.lookAtPoint.x = lookAtPoint.x;
		zelda->animCharacter->animInfo.lookAtPoint.y = lookAtPoint.y;
		zelda->animCharacter->animInfo.lookAtPoint.z = lookAtPoint.z;
	}
	
	// animation
	FUR_PROFILE("animation-update")
	{
		fcAnimationUpdate(pEngine, dt);
	}
	
	{
		const fm_vec4 zeros = {0.0f, 0.0f, 0.0f, 0.0f};
		const fm_vec4 axisX = {1.0f, 0.0f, 0.0f, 0.0f};
		const fm_vec4 axisY = {0.0f, 1.0f, 0.0f, 0.0f};
		const fm_vec4 axisZ = {0.0f, 0.0f, 1.0f, 0.0f};
		
		const f32 red[4] = FUR_COLOR_RED;
		const f32 green[4] = FUR_COLOR_GREEN;
		const f32 blue[4] = FUR_COLOR_BLUE;
		
		fcDebugLine(&zeros.x, &axisX.x, red);
		fcDebugLine(&zeros.x, &axisY.x, green);
		fcDebugLine(&zeros.x, &axisZ.x, blue);
	}
	
	// physics
	FUR_PROFILE("physics-update")
	{
		FcPhysicsUpdateCtx physicsCtx = {};
		physicsCtx.dt = dt;

		FcGameObject_Zelda* zelda = pEngine->zeldaGameObject;

		fm_vec4 playerDisplacement = zelda->velocity;
		
		// apply root motion from anim info to physics
		if(zelda->isJump)
		{
			zelda->velocity.z = 4.0f;
		}
		else if(zelda->isGrounded)
		{
			zelda->velocity.x = zelda->animCharacter->animInfo.rootMotionDelta.x / dt;
			zelda->velocity.y = zelda->animCharacter->animInfo.rootMotionDelta.y / dt;
			zelda->velocity.z = 0.0f;
		}
		else
		{
			const f32 speed = fm_vec4_mag(&zelda->velocity);
			if(speed > 0.0f)
			{
				zelda->velocity.x += zelda->animCharacter->animInfo.desiredMove.x * 1.2f;
				zelda->velocity.y += zelda->animCharacter->animInfo.desiredMove.y * 1.2f;
				fm_vec4_norm(&zelda->velocity);
				fm_vec4_mulf(&zelda->velocity, speed, &zelda->velocity);
			}
		}
		
		// apply gravity
		zelda->velocity.z += -9.81f * dt;
		
		fm_vec4_mulf(&zelda->velocity, dt, &playerDisplacement);
		physicsCtx.playerDisplacement = &playerDisplacement;
		fcPhysicsUpdate(pEngine->pPhysics, &physicsCtx);
		
		FcPhysicsPlayerInfo playerPhysics;
		playerPhysics.locator = &zelda->info.transform;
		fcPhysicsGetPlayerInfo(pEngine->pPhysics, &playerPhysics);

		if(zelda->playerWindProtecting)
		{
			pEngine->windVelocity.x = 1.4f;
			pEngine->windVelocity.y = 3.7f;
		}
		else
		{
			pEngine->windVelocity.x = 0.0f;
			pEngine->windVelocity.y = 0.0f;
		}
		
		// adjust wind by inv player movement
		{
			fm_vec4 playerMove = pEngine->playerMove;
			
			fm_mat4 playerMat;
			fm_mat4_rot_z(zelda->animCharacter->animInfo.currentYaw, &playerMat);
			
			fm_vec4 invPlayerMove;
			fm_mat4_transform(&playerMat, &playerMove, &invPlayerMove);
			
			pEngine->windVelocity.x -= invPlayerMove.x;
			pEngine->windVelocity.y -= invPlayerMove.y;
			pEngine->windVelocity.z -= invPlayerMove.z;
		}
		
		FUR_PROFILE("skinning")
		{
			FcGameObject_Zelda* zelda = pEngine->zeldaGameObject;

			const fm_xform* poseMS = zelda->animCharacter->poseMS;
			const u32 numSkinMatrices = zelda->animCharacter->rig->numBones;
			for (u32 i = 0; i < numSkinMatrices; ++i)
			{
				fm_xform_to_mat4(&poseMS[i], &pEngine->skinMatrices[i]);
			}
		}

		// simulate hair dangles
		{
			FcPBDDangleCtx simCtx {};
			simCtx.dt = dt;
			
			fm_vec4 spherePos = pEngine->skinMatrices[pEngine->zeldaHeadIdx].w;
			const f32 sphereRadius = 0.08f;
			pEngine->zeldaDangleHairLeft->spherePos = &spherePos;
			pEngine->zeldaDangleHairLeft->sphereRadius = sphereRadius;
			pEngine->zeldaDangleHairRight->spherePos = &spherePos;
			pEngine->zeldaDangleHairRight->sphereRadius = sphereRadius;
			
			pEngine->zeldaDangleHairLeft->x0[0] = pEngine->skinMatrices[pEngine->zeldaDangleHairLeftIdx1].w;
			pEngine->zeldaDangleHairRight->x0[0] = pEngine->skinMatrices[pEngine->zeldaDangleHairRightIdx1].w;
			
			// add wind velocity
			for(u32 i=0; i<3; ++i)
			{
				pEngine->zeldaDangleHairLeft->v[i].x += pEngine->windVelocity.x * dt;
				pEngine->zeldaDangleHairLeft->v[i].y += pEngine->windVelocity.y * dt;
				pEngine->zeldaDangleHairLeft->v[i].z += pEngine->windVelocity.z * dt;
				
				pEngine->zeldaDangleHairRight->v[i].x += pEngine->windVelocity.x * dt;
				pEngine->zeldaDangleHairRight->v[i].y += pEngine->windVelocity.y * dt;
				pEngine->zeldaDangleHairRight->v[i].z += pEngine->windVelocity.z * dt;
			}
			
			fcPBDDangleSimulate(&simCtx, pEngine->zeldaDangleHairLeft);
			fcPBDDangleSimulate(&simCtx, pEngine->zeldaDangleHairRight);
			
			fm_mat4 m[3] = {};
			
			m[0] = pEngine->skinMatrices[pEngine->zeldaDangleHairLeftIdx1];
			fcPBDDangleToMatricesYDown(pEngine->zeldaDangleHairLeft, &m[0], m);
			pEngine->skinMatrices[pEngine->zeldaDangleHairLeftIdx1] = m[0];
			pEngine->skinMatrices[pEngine->zeldaDangleHairLeftIdx2] = m[1];
			
			m[0] = pEngine->skinMatrices[pEngine->zeldaDangleHairRightIdx1];
			fcPBDDangleToMatricesYDown(pEngine->zeldaDangleHairRight, &m[0], m);
			pEngine->skinMatrices[pEngine->zeldaDangleHairRightIdx1] = m[0];
			pEngine->skinMatrices[pEngine->zeldaDangleHairRightIdx2] = m[1];
		}
		
		// simulate cape dangles
		{
			FcPBDDangleCtx simCtx {};
			simCtx.dt = dt;
			
			fm_vec4 spherePos = pEngine->skinMatrices[pEngine->zeldaSpineIdx].w;
			spherePos.z -= 0.25f;
			spherePos.x = 0.6f;
			const f32 sphereRadius = 0.8f;
			pEngine->zeldaCapeL->spherePos = &spherePos;
			pEngine->zeldaCapeL->sphereRadius = sphereRadius;
			pEngine->zeldaCapeC->spherePos = &spherePos;
			pEngine->zeldaCapeC->sphereRadius = sphereRadius;
			pEngine->zeldaCapeR->spherePos = &spherePos;
			pEngine->zeldaCapeR->sphereRadius = sphereRadius;
			
			pEngine->zeldaCapeL->x0[0] = pEngine->skinMatrices[pEngine->zeldaCapeIdxL[0]].w;
			pEngine->zeldaCapeC->x0[0] = pEngine->skinMatrices[pEngine->zeldaCapeIdxC[0]].w;
			pEngine->zeldaCapeR->x0[0] = pEngine->skinMatrices[pEngine->zeldaCapeIdxR[0]].w;
			
			// add wind velocity
			for(u32 i=0; i<4; ++i)
			{
				pEngine->zeldaCapeL->v[i].x += pEngine->windVelocity.x * dt;
				pEngine->zeldaCapeL->v[i].y += pEngine->windVelocity.y * dt;
				pEngine->zeldaCapeL->v[i].z += pEngine->windVelocity.z * dt;
				
				pEngine->zeldaCapeC->v[i].x += pEngine->windVelocity.x * dt;
				pEngine->zeldaCapeC->v[i].y += pEngine->windVelocity.y * dt;
				pEngine->zeldaCapeC->v[i].z += pEngine->windVelocity.z * dt;
				
				pEngine->zeldaCapeR->v[i].x += pEngine->windVelocity.x * dt;
				pEngine->zeldaCapeR->v[i].y += pEngine->windVelocity.y * dt;
				pEngine->zeldaCapeR->v[i].z += pEngine->windVelocity.z * dt;
			}
			
			fcPBDDangleSimulate(&simCtx, pEngine->zeldaCapeL);
			fcPBDDangleSimulate(&simCtx, pEngine->zeldaCapeC);
			fcPBDDangleSimulate(&simCtx, pEngine->zeldaCapeR);
			
			fm_mat4 m[4] = {};
			
			m[0] = pEngine->skinMatrices[pEngine->zeldaCapeIdxL[0]];
			fcPBDDangleToMatricesYDown(pEngine->zeldaCapeL, &m[0], m);
			
			for(u32 i=0; i<4; ++i)
			{
				pEngine->skinMatrices[pEngine->zeldaCapeIdxL[i]] = m[i];
			}
			
			m[0] = pEngine->skinMatrices[pEngine->zeldaCapeIdxC[0]];
			fcPBDDangleToMatricesYDown(pEngine->zeldaCapeC, &m[0], m);
			
			for(u32 i=0; i<4; ++i)
			{
				pEngine->skinMatrices[pEngine->zeldaCapeIdxC[i]] = m[i];
			}
			
			m[0] = pEngine->skinMatrices[pEngine->zeldaCapeIdxR[0]];
			fcPBDDangleToMatricesYDown(pEngine->zeldaCapeR, &m[0], m);
			
			for(u32 i=0; i<4; ++i)
			{
				pEngine->skinMatrices[pEngine->zeldaCapeIdxR[i]] = m[i];
			}
		}
	}
	
	// test jobs
	{
		i32 numStuff = 40;
		
		FcJobDecl jobs[40] = {};
		for(i32 i=0; i<40; ++i)
		{
			jobs[i].func = test_job;
			jobs[i].userData = &numStuff;
		}
		
		FcJobCounter* counter = {};
		fcRunJobs(jobs, 40, &counter);
		fcWaitForCounterAndFree(counter);
	}
	
	// rendering
	FUR_PROFILE("render-update")
	{
		FcGameObject_Zelda* zelda = pEngine->zeldaGameObject;

		// get zelda position
		fm_xform playerLocator = zelda->info.transform;
		
		fm_mat4 zeldaMat;
		fm_mat4_rot_z(zelda->animCharacter->animInfo.currentYaw, &zeldaMat);
		zeldaMat.w = playerLocator.pos;
		
		// get zelda right hand slot
		fm_mat4 zeldaRightHand = pEngine->skinMatrices[pEngine->zeldaHandRightIdx];
		fm_mat4 slotLS;
		fm_mat4_identity(&slotLS);
		slotLS.w.x -= 0.02f;
		slotLS.w.y += 0.06f;
		fm_mat4 slotMS;
		fm_mat4_mul(&slotLS, &zeldaRightHand, &slotMS);
		fm_mat4_mul(&slotMS, &zeldaMat, &slotMS);
		
		// camera
		FUR_PROFILE("camera-update")
		{
			FcCameraSystemUpdateCtx cameraCtx = {};
			cameraCtx.dt = dt;
			cameraCtx.rotationYaw = pEngine->inputActionSystem.rightStick.x;
			cameraCtx.rotationPitch = pEngine->inputActionSystem.rightStick.y;
			cameraCtx.zoom = pEngine->inputActionSystem.leftTrigger - pEngine->inputActionSystem.rightTrigger;
			
			// adjust camera by player position
			fcCameraSystemAdjustByPlayerMovement(pEngine->cameraSystem, &zeldaMat);
			
			fcCameraSystemUpdate(pEngine->cameraSystem, &cameraCtx);
		}
		
		// player movement
		const f32 maxSpeed = 5.0f;
		
		fm_vec4 dirForward = {};
		fm_vec4 dirLeft = {};
		fcCameraSystemGetDirections(pEngine->cameraSystem, &dirForward, &dirLeft);
		
		fm_vec4 playerMoveForward;
		fm_vec4_mulf(&dirForward, maxSpeed * pEngine->inputActionSystem.leftStick.y, &playerMoveForward);
		fm_vec4 playerMoveLeft;
		fm_vec4_mulf(&dirLeft, maxSpeed * pEngine->inputActionSystem.leftStick.x, &playerMoveLeft);
		fm_vec4 playerMove;
		fm_vec4_add(&playerMoveForward, &playerMoveLeft, &playerMove);
		
		zelda->animCharacter->animInfo.animToLogicMotionRotationAlpha = 1.0f;
		zelda->animCharacter->animInfo.animToLogicMotionTranslationAlpha = 0.0f;
		zelda->animCharacter->animInfo.desiredMove.x = playerMove.x * dt;
		zelda->animCharacter->animInfo.desiredMove.y = playerMove.y * dt;
		
		zelda->logicMove.x = playerMove.x * dt;
		zelda->logicMove.y = playerMove.y * dt;
		zelda->logicMove.z = 0.0f;
		zelda->logicMove.w = 0.0f;
		
		pEngine->playerMove = playerMove;
		
		fm_mat4 cameraMatrix;
		fcCameraSystemViewMatrix(pEngine->cameraSystem, &cameraMatrix);
		
		if(!zelda->playerWeaponEquipped)
		{
			fm_mat4_identity(&slotMS);
			slotMS.w.x = 4.0f;
			slotMS.w.z = -4.0f;
		}
		
		// acquire this frame's PVS (Potentially Visible Set) to fill it with data
		FcRenderPVS* framePVS = fcRendererAcquireFreePVS(pEngine->pRenderer, &cameraMatrix, fcCameraSystemGetFOV(pEngine->cameraSystem));
		
		fcRenderPVSAdd(framePVS, pEngine->swordMesh, &slotMS);
		
		fm_mat4 staticMeshesLocator;
		fm_mat4_identity(&staticMeshesLocator);
		
		// chest
		staticMeshesLocator.w.x = 2.0f;
		staticMeshesLocator.w.y = 2.0f;
		fcRenderPVSAdd(framePVS, pEngine->chestMesh, &staticMeshesLocator);
		
		// block mesh
		staticMeshesLocator.w.x = pEngine->blockPositions[0].pos.x;
		staticMeshesLocator.w.y = pEngine->blockPositions[0].pos.y;
		staticMeshesLocator.w.z = pEngine->blockPositions[0].pos.z;
		fcRenderPVSAdd(framePVS, pEngine->blockMesh, &staticMeshesLocator);
		
		staticMeshesLocator.w.x = pEngine->blockPositions[1].pos.x;
		staticMeshesLocator.w.y = pEngine->blockPositions[1].pos.y;
		staticMeshesLocator.w.z = pEngine->blockPositions[1].pos.z;
		fcRenderPVSAdd(framePVS, pEngine->blockMesh, &staticMeshesLocator);
		
		staticMeshesLocator.w.z = 0.0f;
		
		// rocks
		staticMeshesLocator.w.x = 14.0f;
		staticMeshesLocator.w.y = -15.0f;
		fcRenderPVSAdd(framePVS, pEngine->rockMeshes[0], &staticMeshesLocator);
		
		staticMeshesLocator.w.x = 15.0f;
		staticMeshesLocator.w.y = 13.0f;
		fcRenderPVSAdd(framePVS, pEngine->rockMeshes[1], &staticMeshesLocator);
		
		staticMeshesLocator.w.x = -11.0f;
		staticMeshesLocator.w.y = 7.0f;
		fcRenderPVSAdd(framePVS, pEngine->rockMeshes[2], &staticMeshesLocator);
		
		staticMeshesLocator.w.x = -13.0f;
		staticMeshesLocator.w.y = -4.0f;
		fcRenderPVSAdd(framePVS, pEngine->rockMeshes[3], &staticMeshesLocator);
		
		staticMeshesLocator.w.x = 2.0f;
		staticMeshesLocator.w.y = -14.0f;
		fcRenderPVSAdd(framePVS, pEngine->rockMeshes[4], &staticMeshesLocator);
		
		staticMeshesLocator.w.x = 0.0f;
		staticMeshesLocator.w.y = 0.0f;
		fcRenderPVSAddAndSkin(framePVS, pEngine->zeldaMesh, &zeldaMat, pEngine->skinMatrices, pEngine->pRig->numBones);
		
		// draw frame
		FcRendererDrawFrameCtx renderCtx = {};
		renderCtx.pvs = framePVS;
		fcRendererDrawFrame(pEngine->pRenderer, &renderCtx, allocator);
	}
}

FUR_JOB_ENTRY_POINT(fur_engine_main_thread_loop)
{
	FcMainThreadUserData* userData = FUR_JOB_USER_DATA(FcMainThreadUserData);
	
	FcGameEngine* pEngine = userData->pEngine;
	const FcAllocator* allocator = userData->allocator;
	
	pEngine->prevTimePoint = std::chrono::system_clock::now();
	
	while(fcApplicationUpdate(pEngine->pApp))
	{
		std::chrono::system_clock::time_point timeNow = std::chrono::system_clock::now();
		std::chrono::duration<f32> dtOrig = timeNow - pEngine->prevTimePoint;
		pEngine->prevTimePoint = timeNow;
		
		const f32 dt = dtOrig.count();
		
		fcProfilerStartFrame();
		
		FUR_PROFILE("frame")
		{
			fcGameEngineMainUpdate(pEngine, dt, allocator);
		}
		
		fcProfilerEndFrame();
	}
	
	fcRendererWaitForDevice(pEngine->pRenderer);
	
	fcJobSystemExitAllJobs();
}

void furMainEngineLoop(FcGameEngine* pEngine, const FcAllocator* allocator)
{
	FcMainThreadUserData data = {pEngine, allocator};
	
	FcJobDecl mainThreadJob = {};
	mainThreadJob.userData = &data;
	mainThreadJob.func = fur_engine_main_thread_loop;
	fcJobSystemSetupMainThreadJob(&mainThreadJob);
	
	// see fur_engine_main_thread_loop for the actual main thread loop
	fcJobSystemEnterWorkerThreadMode();
}

bool furMainEngineTerminate(FcGameEngine* pEngine, const FcAllocator* allocator)
{
	// release text BVH
	fcBoundingVolumeHiearchyRelease(&pEngine->testBVH, allocator);
	
	// release meshes
	fcRendererReleaseProxy(pEngine->pRenderer, pEngine->zeldaMesh, allocator);
	fcRendererReleaseProxy(pEngine->pRenderer, pEngine->swordMesh, allocator);
	fcRendererReleaseProxy(pEngine->pRenderer, pEngine->chestMesh, allocator);
	fcRendererReleaseProxy(pEngine->pRenderer, pEngine->blockMesh, allocator);
	
	// load rock meshes
	for(u32 i=0; i<5; ++i)
	{
		fcRendererReleaseProxy(pEngine->pRenderer, pEngine->rockMeshes[i], allocator);
	}
	
	fcDestroyPBDDangle(pEngine->zeldaDangleHairLeft, allocator);
	fcDestroyPBDDangle(pEngine->zeldaDangleHairRight, allocator);
	
	fcDestroyPBDDangle(pEngine->zeldaCapeL, allocator);
	fcDestroyPBDDangle(pEngine->zeldaCapeC, allocator);
	fcDestroyPBDDangle(pEngine->zeldaCapeR, allocator);
	
	fcDestroyWorld(pEngine->pWorld, allocator);
	FUR_FREE(pEngine->pWorld, allocator);
	
	FUR_FREE(pEngine->scratchpadBuffer, allocator);
	
	fcDestroyAnimSystem(pEngine->animSystem, allocator);
	
	fcDestroyCameraSystem(pEngine->cameraSystem, allocator);
	
	fcScriptRelease(allocator);

	fcJobSystemRelease(allocator);
	fcDestroyPhysics(pEngine->pPhysics, allocator);
	fcDestroyRenderer(pEngine->pRenderer, allocator);
	
	fcDestroyInputManager(pEngine->pInputManager, allocator);
	
	fcDepotUnmount(pEngine->depot, allocator);

	fcStringIdRegisterRelease(allocator);
	
	fcProfilerRelease(allocator);
	
	// release all memory before this call, otherwise it might be treated as memory leak
	fcDestroyApplication(pEngine->pApp, allocator);
	
	FUR_FREE(pEngine, allocator);	// rest of the deallocations should happen through allocators
	
	return true;
}

int main()
{
	FcGameEngineCreateInfo desc = {};
	
	desc.m_mainApp.m_width = 1600;
	desc.m_mainApp.m_height = 900;
	desc.m_mainApp.m_title = "Furball Cat Game Engine";
	
	FcGameEngine* pEngine = NULL;
	
	FcAllocator* allocator = NULL;	// todo: if NULL, then uses default alloc callbacks
	
	// initialize most basic engine components
	bool initResult = fcCreateGameEngine(desc, allocator, &pEngine);
	if(!initResult)
	{
		printf("Engine initialization error. Last known error: %s.\n", FurGetLastError());
		return initResult;
	}
	
	// main engine loop
	furMainEngineLoop(pEngine, allocator);
	
	// terminate most basic engine components
	bool result = furMainEngineTerminate(pEngine, allocator);
	if(!result)
	{
		return 1;
	}

	// validate memory - after this line there should be no fur_alloc/fur_free functions called
	// all memory deallocations should be already done at this point
	FUR_ASSERT(fcValidateMemory());
	
	return 0;
}
