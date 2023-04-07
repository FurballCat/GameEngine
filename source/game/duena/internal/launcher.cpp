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

#include "camera.h"

/**************** FURBALL CAT GAME ENGINE ****************/

typedef union FcVariant
{
	FcStringId asStringHash;
	i32 asInt32;
	bool asBool;
	f32 asFloat;
} FcVariant;

// ***** scripts core ***** //

typedef struct FcGameObjectLegacy FcGameObjectLegacy;

typedef struct FcGameObjectLegacyRegister
{
	FcGameObjectLegacy** objects;
	FcStringId* ids;
	u32 numObjects;	// also numIds
	u32 capacity;
	
	FcGameObjectLegacy* pPlayer; // for quick access
} FcGameObjectLegacyRegister;

typedef enum FcScriptSegmentId
{
	FS_SEG_ID_UNKNOWN = 0,
	FS_SEG_ID_C_FUNC_CALL = 1,	// indicates function call, read FcScriptOpHeader next
	FS_SEG_ID_C_FUNC_ARG = 2,	// indicates function argument, read FcVariant next
	FS_SEG_ID_LAMBDA = 3,	// indicates whole lambda (sequence of function calls), read FcScriptSegmentHeader next
	FS_SEG_ID_STATE = 4,
	FS_SEG_ID_STATE_ON_EVENT = 5,
	FS_SEG_ID_STATE_TRACK = 6,
	FS_SEG_ID_STATE_SCRIPT = 7,
} FcScriptSegmentId;

typedef struct FcScriptCtx
{
	FcStringId state;
	FcStringId stateEventToCall;
	
	FcStringId nextState;
	FcVariant lastResult;
	f32 waitSeconds;
	u32 numSkipOps;
	
	FcGameObjectLegacy* self;
	FcGameObjectLegacyRegister* gameObjectRegister;
	FcWorld* world;
	
	// systems
	FcCameraSystem* sysCamera;
} FcScriptCtx;

// todo: move it somewhere else
FcVariant fcScriptNative_Animate(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);
FcVariant fcScriptNative_WaitAnimate(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);
FcVariant fcScriptNative_EquipItem(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);
FcVariant fcScriptNative_WaitSeconds(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);
FcVariant fcScriptNative_GetVariable(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);
FcVariant fcScriptNative_Go(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);
FcVariant fcScriptNative_GoWhen(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);
FcVariant fcScriptNative_CmpGt(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);
FcVariant fcScriptNative_CmpEq(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);

// camera script functions
FcVariant fcScriptNative_CameraEnable(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);

typedef FcVariant (*FcScriptNativeFn)(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args);

typedef struct FcScriptNativeFnEntry
{
	FcStringId name;
	FcScriptNativeFn func;
	u32 numArgs;
} FcScriptNativeFnEntry;

FcStringId g_scriptNullOpCode = SID("__null");

FcScriptNativeFnEntry g_nativeFuncLookUp[] = {
	{ SID("animate"), fcScriptNative_Animate, 2 },
	{ SID("wait-animate"), fcScriptNative_WaitAnimate, 2 },
	{ SID("equip-item"), fcScriptNative_EquipItem, 2 },
	{ SID("wait-seconds"), fcScriptNative_WaitSeconds, 1 },
	{ SID("get-variable"), fcScriptNative_GetVariable, 2 },
	{ SID("go"), fcScriptNative_Go, 1 },
	{ SID("go-when"), fcScriptNative_GoWhen, 2 },
	{ SID("cmp-gt"), fcScriptNative_CmpGt, 2 },
	{ SID("cmp-eq"), fcScriptNative_CmpEq, 2 },
	{ SID("camera-enable"), fcScriptNative_CameraEnable, 3 },
	{ g_scriptNullOpCode, NULL, 0 }
};

typedef struct FcScriptOp
{
	FcScriptNativeFn func;
	FcVariant* args;	// not owning this memory, it's just a pointer
	u32 numArgs;
} FcScriptOp;

typedef struct FcScriptData
{
	FcScriptOp* ops;	// sequence of operations
	FcVariant* allArgs; // owning memory to all args for all calls
	
	u32 numOps;
	u32 numAllArgs;
} FcScriptData;

typedef struct FcScriptState
{
	u32 idxOp;
} FcScriptState;

typedef enum FcScriptParsingStage
{
	SPS_NONE = 0,
	SPS_READING,
	SPS_END,
} FcScriptParsingStage;

/*
 
 Header
 Arg1
 Arg2
 Header
 Arg1
 ...
 
 **/

typedef struct FcScriptOpHeader
{
	FcStringId opCode;
	u32 flags;
	u32 numArgs;
} FcScriptOpHeader;

typedef struct FcScriptSegmentHeader
{
	u8 segmentId;		// type of segment data (what to expect next)
	u8 padding;
	u16 dataSize; 		// segment size in bytes
	FcStringId name;	// unique name in segment scope (to know what to look for)
} FcScriptSegmentHeader;

typedef struct FcScriptExecutionCtx
{
	FcBinaryBufferStream scriptBufferStream;
	FcScriptCtx* scriptCtx;
	u32 numOpsExecuted;
	bool endOflambda;
} FcScriptExecutionCtx;

FcVariant fcScriptExecuteStep(FcScriptExecutionCtx* ctx)
{
	FcBinaryBufferStream* stream = &ctx->scriptBufferStream;
	
	// peek what's next in buffer, but do not advance the stream
	u8 op_pre_flag = FS_SEG_ID_UNKNOWN;
	fcBinaryBufferStreamPeek(stream, sizeof(u8), &op_pre_flag);
	
	FcVariant result = {};
	
	if(op_pre_flag == FS_SEG_ID_C_FUNC_CALL)
	{
		// advance the stream to skip the peeked part
		u32 bytesRead = fcBinaryBufferStreamRead(stream, sizeof(u8), &op_pre_flag);
		FUR_ASSERT(bytesRead);
		
		ctx->numOpsExecuted += 1;
		
		// read operation header
		FcScriptOpHeader opHeader = {};
		bytesRead = fcBinaryBufferStreamRead(stream, sizeof(FcScriptOpHeader), &opHeader);
		FUR_ASSERT(bytesRead);
		
		// read arguments
		FUR_ASSERT(opHeader.numArgs < 20);
		FcVariant args[20];
		
		for(u32 i=0; i<opHeader.numArgs; ++i)
		{
			args[i] = fcScriptExecuteStep(ctx);
		}
		
		// execute operation
		if(ctx->scriptCtx->numSkipOps == 0)
		{
			// find operation/function pointer - todo: implement simple cache
			const u32 numFuncs = FUR_ARRAY_SIZE(g_nativeFuncLookUp);
			u32 idxFunc = 0;
			for(idxFunc=0; idxFunc<numFuncs; ++idxFunc)
			{
				if(opHeader.opCode == g_nativeFuncLookUp[idxFunc].name)
				{
					break;
				}
			}
			
			FUR_ASSERT(idxFunc < numFuncs);	// op code not found
			
			// call function
			result = g_nativeFuncLookUp[idxFunc].func(ctx->scriptCtx, opHeader.numArgs, args);
		}
		else if(ctx->scriptCtx->numSkipOps > 0)
		{
			ctx->scriptCtx->numSkipOps -= 1;
		}
	}
	else if(op_pre_flag == FS_SEG_ID_C_FUNC_ARG)
	{
		// advance the stream to skip the peeked part
		u32 bytesRead = fcBinaryBufferStreamRead(stream, sizeof(u8), &op_pre_flag);
		FUR_ASSERT(bytesRead);
		
		bytesRead = fcBinaryBufferStreamRead(stream, sizeof(FcVariant), &result);
		FUR_ASSERT(bytesRead);
	}
	else
	{
		ctx->endOflambda = true;
	}
	
	return result;
}

FcScriptSegmentId fcScriptReadSegmentHeader(FcScriptExecutionCtx* ctx, FcScriptSegmentHeader* header)
{
	// read segment header to know what's in
	u32 bytesRead = fcBinaryBufferStreamRead(&ctx->scriptBufferStream, sizeof(FcScriptSegmentHeader), header);
	FUR_ASSERT(bytesRead);
	
	return (FcScriptSegmentId)header->segmentId;
}

FcStringId fcScriptSkipToNextSegment(FcScriptExecutionCtx* ctx, FcScriptSegmentId segmentType)
{
	FcScriptSegmentHeader header = {};
	bool found = false;
	
	while(ctx->scriptBufferStream.pos <= ctx->scriptBufferStream.endPos)
	{
		// read segment header to know what's in
		u32 bytesRead = fcBinaryBufferStreamRead(&ctx->scriptBufferStream, sizeof(FcScriptSegmentHeader), &header);
		FUR_ASSERT(bytesRead);
		
		// is it the segment we are looking for?
		FcScriptSegmentId thisSegmentType = (FcScriptSegmentId)header.segmentId;
		if(thisSegmentType == segmentType)
		{
			found = true;
			return header.name;
		}
		
		// no? ok, let's skip to the next segment
		bytesRead = fcBinaryBufferStreamRead(&ctx->scriptBufferStream, header.dataSize, NULL);
		FUR_ASSERT(bytesRead);
	}
	
	return 0;
}

bool fcScriptSkipUntilSegment(FcScriptExecutionCtx* ctx, FcScriptSegmentId segmentType, FcStringId segmentName)
{
	FcStringId name = 0;
	
	do
	{
		name = fcScriptSkipToNextSegment(ctx, segmentType);
		if(name == segmentName)
			return true;
	}
	while(name != 0);

	return false;
}

void fcScriptExecuteLambda(FcScriptExecutionCtx* ctx)
{
	do
	{
		fcScriptExecuteStep(ctx);
		
		if(ctx->scriptCtx->waitSeconds > 0.0f)
		{
			ctx->scriptCtx->numSkipOps = ctx->numOpsExecuted;
			break;
		}
		else if(ctx->scriptCtx->nextState != 0)
		{
			break;
		}
	}
	while(!ctx->endOflambda);
}

void fcScriptExecute(const FcBinaryBuffer* scriptBuffer, FcScriptCtx* scriptCtx)
{
	FcScriptExecutionCtx ctx = {};
	ctx.scriptCtx = scriptCtx;
	fcBinaryBufferStreamInit(scriptBuffer, &ctx.scriptBufferStream);
	
	FcStringId segName = fcScriptSkipToNextSegment(&ctx, FS_SEG_ID_STATE_SCRIPT);
	FUR_ASSERT(segName);
	
	bool found = fcScriptSkipUntilSegment(&ctx, FS_SEG_ID_STATE, scriptCtx->state);
	FUR_ASSERT(found);
	
	found = fcScriptSkipUntilSegment(&ctx, FS_SEG_ID_STATE_ON_EVENT, scriptCtx->stateEventToCall);
	FUR_ASSERT(found);
	
	FcScriptSegmentHeader segmentHeader = {};
	fcScriptReadSegmentHeader(&ctx, &segmentHeader);
	
	if(segmentHeader.segmentId == FS_SEG_ID_STATE_TRACK)
	{
		// todo
		fcScriptReadSegmentHeader(&ctx, &segmentHeader);
		FUR_ASSERT(segmentHeader.segmentId == FS_SEG_ID_LAMBDA);
	}
	
	// run lambda
	fcScriptExecuteLambda(&ctx);
}

typedef struct FcScriptLambda
{
	f32 waitSeconds;
	u32 numSkipOps;
	bool isActive;
	FcStringId lambdaName;	// or state name
	FcStringId eventName;
	FcGameObjectLegacy* selfGameObject;
	const FcBinaryBuffer* scriptBlob;
} FcScriptLambda;

// ******************* //

struct FurMainAppDesc
{
	u32 m_width;
	u32 m_height;
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

typedef struct FcAnimateActionSlots
{
	FcAnimActionAnimate slot[32];
} FcAnimateActionSlots;

FcAnimActionAnimate* fcAnimateActionSlitsGetFree(FcAnimateActionSlots* slots)
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

typedef struct FcGameObjectLegacy
{
	fm_xform worldTransform;
	FcStringId name;	// access in scripts example: 'zelda
	FcAnimateActionSlots animateActionSlots;
	
	FcAnimCharacter* animCharacter;
	
	fm_vec4 logicMove;
	
	FcStringId playerState;
	bool playerWeaponEquipped;
	bool playerWindProtecting;
	bool equipItemNow;
	bool showAnimStateDebug;
	bool isJump;
	bool isGrounded;
	fm_vec4 velocity;
	
} FcGameObjectLegacy;

const FcAnimClip* fcResourceRegisterLoadAnim(FcResourceRegister* reg, FcDepot* depot, const char* name,
								   const FcRig* rig, FcAllocator* allocator)
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

		FcFilePath file_path = fcFilePathCreate(depot, pathEngine);
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
	
	FcAnimCharacter* animCharacter;
	const FcRenderProxy* mesh;
	
	fm_vec4 logicMove;
	
	FcStringId playerState;
	bool playerWeaponEquipped;
	bool playerWindProtecting;
	bool equipItemNow;
	bool showAnimStateDebug;
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
		zelda->animCharacter = fcAnimCharacterCreate(&desc, ctx->stackAlloc);
		fcAnimSystemAddCharacter(ctx->systems->animation, zelda->animCharacter);
		
		zelda->mesh = fcResourceRegisterFindMesh(ctx->resources, SID("zelda-mesh"));
		
		const FcStringId scriptName = fcSpawnInfoGetStringId(ctx->info, SID("state-script"), SID("none"));
		zelda->script.scriptBlob = fcResourceRegisterFindScript(ctx->resources, scriptName);
		zelda->script.lambdaName = zelda->playerState;
		zelda->script.isActive = true;
		zelda->script.eventName = SID("start");
	}
	
	return true;
}

void fcGameObject_ZeldaUpdate(FcGameObject* gameObject, FcGameObjectUpdateCtx* ctx)
{
	FUR_PROFILE("update-zelda")
	{
		
	}
}

void fcRegisterGameObjectFactories()
{
	// zelda
	{
		FcGameObjectFactory factory = {};
		factory.updateBucket = FG_UPDATE_BUCKET_CHARACTERS;
		factory.fn.init = fcGameObject_ZeldaInit;
		factory.fn.update = fcGameObject_ZeldaUpdate;
		factory.memoryMaxSize = sizeof(FcGameObject_Zelda);
		
		fcGameObjectFactoryRegisterNew(SID("zelda"), factory);
	}
}

struct FcApplication;
struct FcRenderer;

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

	// resource path hashes
	FcFilePath zeldaScriptPath;
	
	// animation
	FcRig* pRig;
	const FcAnimClip* pAnimClipWindProtect;
	const FcAnimClip* pAnimClipHoldSword;
	
	// input actions
	bool inActionPressed;
	bool inputTriangleActionPressed;
	bool inputCircleActionPressed;
	bool inputXPressed;
	f32 actionRotationLeftX;
	f32 actionRotationLeftY;
	f32 actionZoomIn;
	f32 actionZoomOut;
	f32 actionMoveX;
	f32 actionMoveY;
	
	fm_vec4 playerMove;
	
	// camera
	FcCameraSystem* cameraSystem;
	
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
	
	// game objects
	FcGameObjectLegacyRegister gameObjectRegister;
	
	// scripts temp
	FcBinaryBuffer zeldaStateScript;
	FcGameObjectLegacy zeldaGameObject;
	
	FcScriptLambda scriptLambdas[128];
	
	// test dangle
	FcPBDDangle dangle;
	
	// hair dangles
	FcPBDDangle zeldaDangleHairLeft;
	FcPBDDangle zeldaDangleHairRight;
	u32 zeldaDangleHairLeftIdx1;
	u32 zeldaDangleHairLeftIdx2;
	u32 zeldaDangleHairRightIdx1;
	u32 zeldaDangleHairRightIdx2;
	u32 zeldaHeadIdx;
	u32 zeldaHandRightIdx;
	
	// cape dangles
	FcPBDDangle zeldaCapeL;
	u32 zeldaCapeIdxL[4];
	
	FcPBDDangle zeldaCapeC;
	u32 zeldaCapeIdxC[4];
	
	FcPBDDangle zeldaCapeR;
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
	FcAllocator* allocator;
} FcMainThreadUserData;

// Furball Cat - Platform
bool furMainEngineInit(const FurGameEngineDesc& desc, FcGameEngine** ppEngine, FcAllocator* allocator)
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

	FcApplicationDesc appDesc;
	appDesc.appTitle = desc.m_mainApp.m_title;
	appDesc.viewportWidth = desc.m_mainApp.m_width;
	appDesc.viewportHeight = desc.m_mainApp.m_height;
	appDesc.iconPath = fcFilePathCreate(pEngine->depot, "data/icon/furball-cat-icon-128x128.png");
	appDesc.depot = pEngine->depot;

	FcResult res = fcApplicationCreate(&appDesc, &pEngine->pApp, allocator);

	if(res == FR_RESULT_OK)
	{
		pEngine->pInputManager = fcInputManagerCreate(allocator);
	}
	
	if(res == FR_RESULT_OK)
	{
		FcRendererDesc rendererDesc;
		rendererDesc.pApp = pEngine->pApp;
		rendererDesc.depot = pEngine->depot;
		
		res = fcRendererCreate(&rendererDesc, &pEngine->pRenderer, allocator);
	}
	
	if(res == FR_RESULT_OK)
	{
		pEngine->pPhysics = fcPhysicsCreate(allocator);
	}
	
	if(res == FR_RESULT_OK)
	{
		fcJobSystemInit(allocator);
	}
	
	if(res == FR_RESULT_OK)
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
	pEngine->animSystem = fcAnimSystemInit(allocator);
	
	// create world
	pEngine->pWorld = (FcWorld*)FUR_ALLOC_AND_ZERO(sizeof(FcWorld), 0, FC_MEMORY_SCOPE_GAME, allocator);
	fcWorldInit(pEngine->pWorld, allocator);
	pEngine->pWorld->systems.renderer = pEngine->pRenderer;
	pEngine->pWorld->systems.animation = pEngine->animSystem;
	
	// load scripts
	{
		pEngine->zeldaScriptPath = fcFilePathCreate(pEngine->depot, "scripts/zelda-state-script.bin");

		fcBinaryBufferLoad(pEngine->depot, pEngine->zeldaScriptPath, &pEngine->zeldaStateScript, allocator);
		fcResourceRegisterAddScript(&pEngine->pWorld->resources, SID("ss-zelda"), &pEngine->zeldaStateScript);
	}
	
	// init camera
	pEngine->cameraSystem = fcCameraSystemCreate(allocator);
	
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
			const FcFilePath rigPath = fcFilePathCreate(pEngine->depot, "data/rig/zelda-a-pose.rig");
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
		
		// import rig
#if 0
		if(!pEngine->pRig)
		{
			fi_import_rig_ctx_t ctx = {};
			ctx.path = characterRigPath;
			
			fcImportRig(&depot, &ctx, &pEngine->pRig, allocator);
			
			// apply rig properties
			{
				// set locomotion joint to track and apply root motion
				{
					pEngine->pRig->idxLocoJoint = fcRigFindBoneIdx(pEngine->pRig, SID("motion"));
				}
				
				// left leg IK setup
				{
					FcAnimIKSetup* ik = &pEngine->pRig->ikLeftLeg;
					ik->idxBeginParent = fcRigFindBoneIdx(pEngine->pRig, SID("Bip001_Pelvis"));
					ik->idxBegin = fcRigFindBoneIdx(pEngine->pRig, SID("Bip001_Thigh_L"));
					ik->idxMid = fcRigFindBoneIdx(pEngine->pRig, SID("Bip001_Calf_L"));
					ik->idxEnd = fcRigFindBoneIdx(pEngine->pRig, SID("Bip001_Foot_L"));
					ik->hingeAxisMid = FM_AXIS_Z;
					ik->minAngle = 0.02f;
					ik->maxAngle = 2.8f;
				}
				
				// right leg IK setup
				{
					FcAnimIKSetup* ik = &pEngine->pRig->ikRightLeg;
					ik->idxBeginParent = fcRigFindBoneIdx(pEngine->pRig, SID("Bip001_Pelvis"));
					ik->idxBegin = fcRigFindBoneIdx(pEngine->pRig, SID("Bip001_Thigh_R"));
					ik->idxMid = fcRigFindBoneIdx(pEngine->pRig, SID("Bip001_Calf_R"));
					ik->idxEnd = fcRigFindBoneIdx(pEngine->pRig, SID("Bip001_Foot_R"));
					ik->hingeAxisMid = FM_AXIS_NEG_Z;
					ik->minAngle = 0.02f;
					ik->maxAngle = 2.8f;
				}
				
				// head look-at setup
				{
					FcAnimLookAtSetup* lookAt = &pEngine->pRig->headLookAt;
					lookAt->idxHead = fcRigFindBoneIdx(pEngine->pRig, SID("Bip001_Head"));
					lookAt->idxNeck = fcRigFindBoneIdx(pEngine->pRig, SID("Bip001_Neck"));
					lookAt->idxSpine3 = fcRigFindBoneIdx(pEngine->pRig, SID("Bip001_Spine3"));
					lookAt->limitYaw = FM_DEG_TO_RAD(60.0f);
					lookAt->limitPitchDown = FM_DEG_TO_RAD(25.0f);
					lookAt->limitPitchUp = FM_DEG_TO_RAD(45.0f);
				}
				
				// masks
				{
					pEngine->pRig->maskUpperBody = FUR_ALLOC_ARRAY_AND_ZERO(u8, pEngine->pRig->numBones, 0, FC_MEMORY_SCOPE_ANIMATION, allocator);
					const int16_t idxSpine = fcRigFindBoneIdx(pEngine->pRig, SID("Bip001_Spine"));
					const FcStringId hashes[9] = {
						SID("Bip001_Pelvis"),
						SID("Bip001_Thigh_L"),
						SID("Bip001_Calf_L"),
						SID("Bip001_Foot_L"),
						SID("Bip001_Thigh_R"),
						SID("Bip001_Calf_R"),
						SID("Bip001_Foot_R"),
						SID("Bip001_Spine"),
						SID("Bip001_Spine1")
					};
					
					const FcStringId hashesPartial[18] = {
						SID("Bip001_UpperArm_L"),
						SID("Bip001_UpperArm3_L"),
						SID("Bip001_Hand_L"),
						SID("Bip001_Thumb1_L"),
						SID("Bip001_Thumb2_L"),
						SID("Bip001_Thumb3_L"),
						SID("Bip001_Index1_L"),
						SID("Bip001_Index2_L"),
						SID("Bip001_Index3_L"),
						SID("Bip001_Middle1_L"),
						SID("Bip001_Middle2_L"),
						SID("Bip001_Middle3_L"),
						SID("Bip001_Ring1_L"),
						SID("Bip001_Ring2_L"),
						SID("Bip001_Ring3_L"),
						SID("Bip001_Pinky1_L"),
						SID("Bip001_Pinky2_L"),
						SID("Bip001_Pinky3_L")
					};
					
					const FcStringId noHashes[2] = {
						SID("rootTransform"),
						SID("motion")
					};
					
					if(idxSpine != -1)
					{
						for(u32 i=0; i<pEngine->pRig->numBones; ++i)
						{
							u8 w = 220;
							for(u32 j=0; j<9; ++j)
							{
								if(pEngine->pRig->boneNameHashes[i] == hashes[j])
								{
									w = 0;
									break;
								}
							}
							for(u32 j=0; j<18; ++j)
							{
								if(pEngine->pRig->boneNameHashes[i] == hashesPartial[j])
								{
									w = 100;
								}
							}
							for(u32 j=0; j<2; ++j)
							{
								if(pEngine->pRig->boneNameHashes[i] == noHashes[j])
								{
									w = 0;
								}
							}
							pEngine->pRig->maskUpperBody[i] = w;
						}
					}
				}
				
				// face mask
				{
					pEngine->pRig->maskFace = FUR_ALLOC_ARRAY_AND_ZERO(u8, pEngine->pRig->numBones, 0, FC_MEMORY_SCOPE_ANIMATION, allocator);
					const int16_t idxSpine = fcRigFindBoneIdx(pEngine->pRig, SID("Bip001_Spine"));
					const FcStringId hashes[] = {
						//SID("Bip001_Neck"),
						//SID("Bip001_Head"),
						SID("Bip001_Jaw"),
						SID("Bip001_Chin"),
						SID("Bip001_LipLower_C"),
						SID("Bip001_LipLower1_R"),
						SID("Bip001_LipLower1_L"),
						SID("Bip001_LipLower2_R"),
						SID("Bip001_LipLower2_L"),
						SID("Bip001_TeethLower"),
						SID("Bip001_Tongue"),
						SID("Bip001_Tongue1"),
						SID("Bip001_Tongue2"),
						SID("Bip001_Tongue3"),
						SID("Bip001_LipUpper_C"),
						SID("Bip001_LipUpper1_R"),
						SID("Bip001_LipUpper1_L"),
						SID("Bip001_LipUpper2_R"),
						SID("Bip001_LipUpper2_L"),
						SID("Bip001_LipCorner_R"),
						SID("Bip001_LipCorner_L"),
						SID("Bip001_Cheek_R"),
						SID("Bip001_Cheek_L"),
						SID("Bip001_Cheek1_R"),
						SID("Bip001_Cheek1_L"),
						SID("Bip001_Cheek2_R"),
						SID("Bip001_Cheek2_L"),
						SID("Bip001_Smile_R"),
						SID("Bip001_Smile_L"),
						SID("Bip001_Frown_R"),
						SID("Bip001_Frown_L"),
						SID("Bip001_TeethUpper"),
						SID("Bip001_BrowInner_L"),
						SID("Bip001_BrowInner_R"),
						SID("Bip001_BrowMid_L"),
						SID("Bip001_BrowMid_R"),
						SID("Bip001_BrowOuter_L"),
						SID("Bip001_BrowOuter_R"),
						SID("Bip001_Nostril_L"),
						SID("Bip001_Nostril_R"),
						SID("Bip001_Nose"),
						SID("Bip001_EyelidUp_L"),
						SID("Bip001_EyelidUp_R"),
						SID("Bip001_EyelidDown_L"),
						SID("Bip001_EyelidDown_R"),
						SID("Bip001_Eye_L"),
						SID("Bip001_EyeSpec_L"),
						SID("Bip001_EyeSpec1_L"),
						SID("Bip001_EyeSpec2_L"),
						SID("Bip001_EyeSpec3_L"),
						SID("Bip001_Eye_R"),
						SID("Bip001_EyeSpec_R"),
						SID("Bip001_EyeSpec1_R"),
						SID("Bip001_EyeSpec2_R"),
						SID("Bip001_Ear_L"),
						SID("Bip001_Ear_R"),
						SID("Bip001_BrowFlesh_L"),
						SID("Bip001_BrowFlesh_R"),
						SID("Bip001_EyelidCrevace_L"),
						SID("Bip001_EyelidCrevace_R"),
					};
					
					if(idxSpine != -1)
					{
						for(u32 i=0; i<pEngine->pRig->numBones; ++i)
						{
							u8 w = 0;
							const u32 numHashes = FUR_ARRAY_SIZE(hashes);
							for(u32 j=0; j<numHashes; ++j)
							{
								if(pEngine->pRig->boneNameHashes[i] == hashes[j])
								{
									w = 255;
									break;
								}
							}
							pEngine->pRig->maskFace[i] = w;
						}
					}
				}
				
				// hands mask
				{
					pEngine->pRig->maskHands = FUR_ALLOC_ARRAY_AND_ZERO(u8, pEngine->pRig->numBones, 0, FC_MEMORY_SCOPE_ANIMATION, allocator);
					const int16_t idxSpine = fcRigFindBoneIdx(pEngine->pRig, SID("Bip001_Spine"));
					const FcStringId hashes[] = {
						SID("Bip001_Index1_L"),
						SID("Bip001_Index2_L"),
						SID("Bip001_Index3_L"),
						SID("Bip001_Middle1_L"),
						SID("Bip001_Middle2_L"),
						SID("Bip001_Middle3_L"),
						SID("Bip001_Ring1_L"),
						SID("Bip001_Ring2_L"),
						SID("Bip001_Ring3_L"),
						SID("Bip001_Pinky1_L"),
						SID("Bip001_Pinky2_L"),
						SID("Bip001_Pinky3_L"),
						SID("Bip001_Thumb_L"),
						SID("Bip001_Thumb1_L"),
						SID("Bip001_Thumb2_L"),
						SID("Bip001_Thumb3_L"),
						SID("Bip001_Index1_R"),
						SID("Bip001_Index2_R"),
						SID("Bip001_Index3_R"),
						SID("Bip001_Middle1_R"),
						SID("Bip001_Middle2_R"),
						SID("Bip001_Middle3_R"),
						SID("Bip001_Ring1_R"),
						SID("Bip001_Ring2_R"),
						SID("Bip001_Ring3_R"),
						SID("Bip001_Pinky1_R"),
						SID("Bip001_Pinky2_R"),
						SID("Bip001_Pinky3_R"),
						SID("Bip001_Thumb_R"),
						SID("Bip001_Thumb1_R"),
						SID("Bip001_Thumb2_R"),
						SID("Bip001_Thumb3_R"),
					};
					
					if(idxSpine != -1)
					{
						for(u32 i=0; i<pEngine->pRig->numBones; ++i)
						{
							u8 w = 0;
							const u32 numHashes = FUR_ARRAY_SIZE(hashes);
							for(u32 j=0; j<numHashes; ++j)
							{
								if(pEngine->pRig->boneNameHashes[i] == hashes[j])
								{
									w = 255;
									break;
								}
							}
							pEngine->pRig->maskHands[i] = w;
						}
					}
				}
			}
			
			FILE* engineFile = fopen(pathRigEngine, "wb");
			
			FcSerializer ser = {};
			ser.file = engineFile;
			ser.isWriting = true;
			fcRigSerialize(&ser, pEngine->pRig, allocator);
		}
#endif

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
			meshCtx.path = fcFilePathCreate(pEngine->depot, "data/mesh/zelda-sword.mesh");
			FcFilePath texturePaths[] = {fcFilePathCreate(pEngine->depot, "data/texture/melee_diff.png")};
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
			meshCtx.path = fcFilePathCreate(pEngine->depot, "data/mesh/chest.mesh");
			FcFilePath texturePaths[] = { fcFilePathCreate(pEngine->depot, "data/texture/chest_albedo.png")};
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
			meshCtx.path = fcFilePathCreate(pEngine->depot, "data/mesh/skull_block_PBR_fc.mesh");
			FcFilePath texturePaths[] = { fcFilePathCreate(pEngine->depot, "data/texture/b_stone1_Color.png")};
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
			meshCtx.path = fcFilePathCreate(pEngine->depot, txtPath);
			FcFilePath texturePaths[] = { fcFilePathCreate(pEngine->depot, txtTexturePath) };
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
			meshCtx.path = fcFilePathCreate(pEngine->depot, "data/mesh/zelda-mesh.mesh");
			FcFilePath texturePaths[] = {
				fcFilePathCreate(pEngine->depot, "data/texture/zelda_diff.png"),
				fcFilePathCreate(pEngine->depot, "data/texture/hair_diff.png"),
				fcFilePathCreate(pEngine->depot, "data/texture/eyes_diff2.png")
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
		pEngine->gameObjectRegister.capacity = 128;
		pEngine->gameObjectRegister.objects = FUR_ALLOC_ARRAY_AND_ZERO(FcGameObjectLegacy*, pEngine->gameObjectRegister.capacity, 0, FC_MEMORY_SCOPE_SCRIPT, allocator);
		pEngine->gameObjectRegister.ids = FUR_ALLOC_ARRAY_AND_ZERO(FcStringId, pEngine->gameObjectRegister.capacity, 0, FC_MEMORY_SCOPE_SCRIPT, allocator);
		pEngine->gameObjectRegister.numObjects = 0;
		
		// create Zelda
		pEngine->actionWeaponEquipped.animation = pEngine->pAnimClipHoldSword;
		pEngine->actionWeaponEquipped.forceLoop = true;
		
		pEngine->actionWindProtect.animation = pEngine->pAnimClipWindProtect;
		pEngine->actionWindProtect.forceLoop = true;
		
		FcAnimActionArgs args = {};
		args.fadeInSec = 0.3f;
		args.ikMode = FA_IK_MODE_LEGS;
		//fcAnimCharacterScheduleActionAnimate(&pEngine->animCharacterZelda, &pEngine->animSimpleAction, &args);
		
		FcAnimCharacterDesc animCharacterDesc = {};
		animCharacterDesc.rig = pEngine->pRig;
		animCharacterDesc.globalTime = pEngine->globalTime;
		
		pEngine->zeldaGameObject.name = SID_REG("zelda");
		pEngine->zeldaGameObject.animCharacter = fcAnimCharacterCreate(&animCharacterDesc, allocator);
		fcAnimSystemAddCharacter(pEngine->animSystem, pEngine->zeldaGameObject.animCharacter);
		pEngine->zeldaGameObject.animCharacter->skinMatrices = pEngine->skinMatrices;
		
		// register Zelda (player) game object
		pEngine->gameObjectRegister.objects[pEngine->gameObjectRegister.numObjects] = &pEngine->zeldaGameObject;
		pEngine->gameObjectRegister.ids[pEngine->gameObjectRegister.numObjects] = pEngine->zeldaGameObject.name;
		pEngine->gameObjectRegister.numObjects += 1;
		
		// reguster player game object
		pEngine->gameObjectRegister.pPlayer = &pEngine->zeldaGameObject;
	}
	
	// init dangle
	{
		const u32 numParticles = 4;
		const f32 segmentLength = 0.2f;
		
		FcPBDDangleDesc desc;
		desc.frequency = 60.0f;
		desc.numParticles = numParticles;
		desc.dampingCoef = 0.96f;
		
		fcPBDDangleCreate(&desc, &pEngine->dangle, allocator);
		
		fm_vec4 pos = {0.0f, 0.0f, 1.0f};
		for(u32 i=0; i<numParticles; ++i)
		{
			pEngine->dangle.x0[i] = pos;
			pos.x += segmentLength;
		}
		
		for(u32 i=0; i<numParticles; ++i)
		{
			pEngine->dangle.d[i] = segmentLength;
		}
	}
	
	// init special bone indices
	{
		const FcStringId handRight = SID("Bip001_Hand_R");
		
		for(u32 i=0; i<pEngine->pRig->numBones; ++i)
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
		
		fcPBDDangleCreate(&desc, &pEngine->zeldaDangleHairLeft, allocator);
		fcPBDDangleCreate(&desc, &pEngine->zeldaDangleHairRight, allocator);
		
		const FcStringId hair_r = SID("Bip001_Hair_R");
		const FcStringId hair_r2 = SID("Bip001_Hair1_R");
		const FcStringId hair_l = SID("Bip001_Hair_L");
		const FcStringId hair_l2 = SID("Bip001_Hair1_L");
		const FcStringId head = SID("Bip001_Head");
		const FcStringId spine = SID("Bip001_Spine");
		
		for(u32 i=0; i<pEngine->pRig->numBones; ++i)
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
		
		pEngine->zeldaDangleHairLeft.d[0] = dLeft;
		pEngine->zeldaDangleHairLeft.d[1] = dLeft;
		
		pEngine->zeldaDangleHairRight.d[0] = dRight;
		pEngine->zeldaDangleHairRight.d[1] = dRight;
	}
	
	// init cape dangles
	{
		FcPBDDangleDesc desc;
		desc.frequency = 60.0f;
		desc.numParticles = 4;
		desc.dampingCoef = 0.96f;
		
		fcPBDDangleCreate(&desc, &pEngine->zeldaCapeL, allocator);
		fcPBDDangleCreate(&desc, &pEngine->zeldaCapeC, allocator);
		fcPBDDangleCreate(&desc, &pEngine->zeldaCapeR, allocator);
		
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
		
		for(u32 i=0; i<pEngine->pRig->numBones; ++i)
		{
			const FcStringId name = pEngine->pRig->boneNameHashes[i];
			
			for(u32 j=0; j<4; ++j)
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
			pEngine->zeldaCapeL.d[j] = fm_vec4_mag(&refPose.pos);
		}
		
		for(u32 j=0; j<4; ++j)
		{
			fm_xform refPose = pEngine->pRig->refPose[pEngine->zeldaCapeIdxC[j]];
			refPose.pos.w = 0.0f;
			pEngine->zeldaCapeC.d[j] = fm_vec4_mag(&refPose.pos);
		}
		
		for(u32 j=0; j<4; ++j)
		{
			fm_xform refPose = pEngine->pRig->refPose[pEngine->zeldaCapeIdxR[j]];
			refPose.pos.w = 0.0f;
			pEngine->zeldaCapeR.d[j] = fm_vec4_mag(&refPose.pos);
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
		
		fcSpawn(&spawner, pEngine->pWorld);
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

FcGameObjectLegacy* fcScriptLookUpGameObject(FcScriptCtx* ctx, FcStringId name)
{
	if(name == SID("self"))
	{
		return ctx->self;
	}
	else if(name == SID("player"))
	{
		return ctx->gameObjectRegister->pPlayer;
	}
	else
	{
		for(u32 i=0; i<ctx->gameObjectRegister->numObjects; ++i)
		{
			if(ctx->gameObjectRegister->ids[i] == name)
			{
				return ctx->gameObjectRegister->objects[i];
			}
		}
	}
	
	return NULL;
}

typedef enum FcScriptAnimateArg
{
	FS_ANIMATE_ARG_FADE_IN_CURVE = 0,
	FS_ANIMATE_ARG_FADE_IN_SEC,
	FS_ANIMATE_ARG_FADE_OUT_CURVE,
	FS_ANIMATE_ARG_FADE_OUT_SEC,
	FS_ANIMATE_ARG_IK_MODE,
	FS_ANIMATE_ARG_LAYER,
	FS_ANIMATE_ARG_LAYER_NAME,
} FcScriptAnimateArg;

FcVariant fcScriptNative_Animate(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs >= 2);
	const FcStringId objectName = args[0].asStringHash;
	const FcStringId animName = args[1].asStringHash;
	
	// find game object
	FcGameObjectLegacy* gameObj = fcScriptLookUpGameObject(ctx, objectName);
	FUR_ASSERT(gameObj);
	
	FcAnimActionAnimate* animateSlot = fcAnimateActionSlitsGetFree(&gameObj->animateActionSlots);
	FUR_ASSERT(animateSlot);
	
	const FcAnimClip* animClip = fcResourceRegisterFindAnimClip(&ctx->world->resources, animName);
	FUR_ASSERT(animClip);
	
	animateSlot->animation = animClip;
	animateSlot->forceLoop = true;
	animateSlot->reserved = true;
	
	FcAnimActionArgs animArgs = {};
	
	// get variadic arguments - iterate every 2 arguments, as we need (animate-arg ENUM) VALUE, this is two variadic arguments
	for(u32 i=2; i+1<numArgs; i += 2)
	{
		const FcScriptAnimateArg arg_enum = (FcScriptAnimateArg)args[i].asInt32;
		switch(arg_enum)
		{
			case FS_ANIMATE_ARG_FADE_IN_SEC:
				animArgs.fadeInSec = args[i+1].asFloat;
				break;
			case FS_ANIMATE_ARG_FADE_IN_CURVE:
				animArgs.fadeInCurve = (FcCurveType)args[i+1].asInt32;
				break;
			case FS_ANIMATE_ARG_FADE_OUT_SEC:
				animArgs.fadeOutSec = args[i+1].asFloat;
				break;
			case FS_ANIMATE_ARG_FADE_OUT_CURVE:
				animArgs.fadeOutCurve = (FcCurveType)args[i+1].asInt32;
				break;
			case FS_ANIMATE_ARG_IK_MODE:
				animArgs.ikMode = (FcAnimIKMode)args[i+1].asInt32;
				break;
			case FS_ANIMATE_ARG_LAYER:
				animArgs.layer = (FcAnimLayerType)args[i+1].asInt32;
				break;
			case FS_ANIMATE_ARG_LAYER_NAME:
				animArgs.layerName = args[i+1].asStringHash;
				break;
			default:
				break;
		}
	}
	
	if(animArgs.layerName == SID("full-body") || animArgs.layerName == 0)
	{
		animateSlot->useLoco = true;
	}
	else
	{
		animateSlot->useLoco = false;
	}
	
	fcAnimCharacterScheduleActionAnimate(gameObj->animCharacter, animateSlot, &animArgs);
	
	ctx->waitSeconds = FM_MAX(ctx->waitSeconds - animArgs.fadeOutSec, 0.0f);
	
	FcVariant result = {};
	return result;
};

FcVariant fcScriptNative_WaitAnimate(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs >= 2);
	const FcStringId animName = args[1].asStringHash;
	
	const FcAnimClip* animClip = fcResourceRegisterFindAnimClip(&ctx->world->resources, animName);
	FUR_ASSERT(animClip);
	
	ctx->waitSeconds = animClip->duration;
	
	return fcScriptNative_Animate(ctx, numArgs, args);
}

FcVariant fcScriptNative_EquipItem(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs >= 2);
	const FcStringId objectName = args[0].asStringHash;
	//const FcStringId itemName = args[1].asStringHash;
	
	// find game object
	FcGameObjectLegacy* gameObj = fcScriptLookUpGameObject(ctx, objectName);
	FUR_ASSERT(gameObj);
	
	gameObj->equipItemNow = true;
	
	FcVariant result = {};
	return result;
}

FcVariant fcScriptNative_WaitSeconds(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs == 1);
	const f32 timeInSeconds = args[0].asFloat;
	
	ctx->waitSeconds = timeInSeconds;
	
	FcVariant result = {};
	return result;
}

FcVariant fcScriptNative_GetVariable(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs == 2);
	const FcStringId objectName = args[0].asStringHash;
	const FcStringId varName = args[1].asStringHash;
	
	// find game object
	FcGameObjectLegacy* gameObj = fcScriptLookUpGameObject(ctx, objectName);
	FUR_ASSERT(gameObj);
	
	FcVariant result = {};
	
	if(varName == SID("wind-protecting"))
	{
		result.asBool = gameObj->playerWindProtecting;
	}
	else if(varName == SID("weapon-equipped"))
	{
		result.asBool = gameObj->playerWeaponEquipped;
	}
	else if(varName == SID("idle-anim-name"))
	{
		result.asStringHash = SID("zelda-funny-pose-4");
	}
	else if(varName == SID("is-running"))
	{
		result.asBool = fm_vec4_mag2(&gameObj->logicMove) > 0.0f;
	}
	else if(varName == SID("is-jump"))
	{
		result.asBool = gameObj->isJump;
	}
	else if(varName == SID("is-grounded"))
	{
		result.asBool = gameObj->isGrounded;
	}
	
	return result;
}

FcVariant fcScriptNative_Go(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs == 1);
	const FcStringId goToState = args[0].asStringHash;
	
	ctx->nextState = goToState;
	
	FcVariant result = {};
	return result;
}

FcVariant fcScriptNative_GoWhen(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs == 2);
	const FcStringId goToState = args[0].asStringHash;
	const bool condition = args[1].asBool;
	
	if(condition)
	{
		ctx->nextState = goToState;
	}
	
	return args[1];
}

FcVariant fcScriptNative_CmpGt(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs == 2);
	const i32 a = args[0].asInt32;
	const i32 b = args[1].asInt32;
	
	FcVariant result;
	result.asBool = a > b;
	return result;
}

FcVariant fcScriptNative_CmpEq(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs == 2);
	const i32 a = args[0].asInt32;
	const i32 b = args[1].asInt32;
	
	FcVariant result;
	result.asBool = (a == b);
	return result;
}

FcVariant fcScriptNative_CameraEnable(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs == 3);
	const FcStringId objectName = args[0].asStringHash;
	const FcStringId cameraType = args[1].asStringHash;
	const f32 fadeInSec = args[2].asFloat;
	
	FcGameObjectLegacy* gameObj = fcScriptLookUpGameObject(ctx, objectName);
	FUR_ASSERT(gameObj);
	
	if(cameraType == SID("follow"))
	{
		FcCameraParamsFollow params = {};
		params.height = 1.2f;
		params.zoom = 1.5f;
		params.poleLength = 1.5f;
		params.fov = 70.0f;
		fcCameraEnableFollow(ctx->sysCamera, &params, fadeInSec);
	}
	else if(cameraType == SID("follow-vista"))
	{
		FcCameraParamsFollow params = {};
		params.height = 1.5f;
		params.zoom = 1.5f;
		params.poleLength = 1.0f;
		params.fov = 60.0f;
		fcCameraEnableFollow(ctx->sysCamera, &params, fadeInSec);
	}
	
	FcVariant result = {};
	return result;
}

void fcScriptsUpdate(FcGameEngine* pEngine, f32 dt)
{
	// todo
}

bool g_drawDevMenu = false;
i32 g_devMenuOption = 0;
bool g_devMenuOptionClick = false;

void fcInputActionsUpdate(FcGameEngine* pEngine, f32 dt)
{
	bool actionPressed = false;
	static bool actionWasPressed = false;
	
	bool triangleActionPressed = false;
	static bool triangleActionWasPressed = false;
	bool circleActionPressed = false;
	static bool circleActionWasPressed = false;
	bool xPressed = false;
	static bool xWasPressed = false;
	
	static bool wasLeftThumbPressed = false;
	bool leftThumbPressed = false;
	static bool wasStartPressed = false;
	bool startPressed = false;
	
	static bool wasDpadUpPressed = false;
	bool dpadUpPressed = false;
	static bool wasDpadDownPressed = false;
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
			wasLeftThumbPressed = inputEvents[i].value > 0.5f;
			leftThumbPressed = wasLeftThumbPressed;
		}
		else if(inputEvents[i].eventID == Gamepad_start)
		{
			wasStartPressed = inputEvents[i].value > 0.5f;
			startPressed = wasStartPressed;
		}
		else if(inputEvents[i].eventID == Gamepad_dpadUp)
		{
			wasDpadUpPressed = inputEvents[i].value > 0.5f;
			dpadUpPressed = wasDpadUpPressed;
		}
		else if(inputEvents[i].eventID == Gamepad_dpadDown)
		{
			wasDpadDownPressed = inputEvents[i].value > 0.5f;
			dpadDownPressed = wasDpadDownPressed;
		}
		else if(inputEvents[i].eventID == Gamepad_cross)
		{
			actionWasPressed = inputEvents[i].value > 0.5f;
			actionPressed = actionWasPressed;
		}
		else if(inputEvents[i].eventID == Gamepad_circle)
		{
			circleActionWasPressed = inputEvents[i].value > 0.5f;
			circleActionPressed = circleActionWasPressed;
		}
		else if(inputEvents[i].eventID == Gamepad_triangle)
		{
			triangleActionWasPressed = inputEvents[i].value > 0.5f;
			triangleActionPressed = triangleActionWasPressed;
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
	
	if((wasLeftThumbPressed && startPressed) || (wasStartPressed && leftThumbPressed))
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
		pEngine->actionRotationLeftX = rightAnalogX;
		pEngine->actionRotationLeftY = rightAnalogY;
		pEngine->actionMoveX = leftAnalogX;
		pEngine->actionMoveY = leftAnalogY;
		pEngine->actionZoomIn = rightTrigger;
		pEngine->actionZoomOut = leftTrigger;
		
		pEngine->inActionPressed = actionPressed;
		pEngine->inputTriangleActionPressed = triangleActionPressed;
		pEngine->inputCircleActionPressed = circleActionPressed;
		pEngine->inputXPressed = xPressed;
	}
}

void fcDevMenuReloatScripts(FcGameEngine* pEngine, FcAllocator* allocator)
{
	fcBinaryBufferRelease(&pEngine->zeldaStateScript, allocator);
	fcBinaryBufferLoad(pEngine->depot, pEngine->zeldaScriptPath, &pEngine->zeldaStateScript, allocator);
}

void fcDevMenuShowPlayerAnimState(FcGameEngine* pEngine, FcAllocator* allocator)
{
	pEngine->zeldaGameObject.showAnimStateDebug = !pEngine->zeldaGameObject.showAnimStateDebug;
}

void fcDevMenuShowProfiler(FcGameEngine* pEngine, FcAllocator* allocator)
{
	fcProfilerToggleDraw();
}

void fcDevMenuSlowTime(FcGameEngine* pEngine, FcAllocator* allocator)
{
	pEngine->debugIsSlowTime = !pEngine->debugIsSlowTime;
}

void fcDevMenuShowFPS(FcGameEngine* pEngine, FcAllocator* allocator)
{
	pEngine->debugShowFPS = !pEngine->debugShowFPS;
}

void fcDevMenuShowMemoryStats(FcGameEngine* pEngine, FcAllocator* allocator)
{
	pEngine->debugShowMemoryStats = !pEngine->debugShowMemoryStats;
}

typedef struct FcDevMenuOption
{
	const char* name;
	void (*func)(FcGameEngine* pEngine, FcAllocator* allocator);
} FcDevMenuOption;

void fcDrawDebugMenu(FcGameEngine* pEngine, FcAllocator* allocator)
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
	pEngine->zeldaGameObject.animCharacter->globalTime = globalTime;
	
	static u32 actionRandomizer = 0;
	static u32 actionRandomizer2 = 0;
	
	if(pEngine->inActionPressed)
		actionRandomizer += 1;
	
	if(pEngine->inputTriangleActionPressed)
		actionRandomizer2 += 1;
	
	// inital player state
	static bool isInit = true;
	if(isInit)
	{
		isInit = false;
		
		pEngine->zeldaGameObject.playerState = SID("idle");
		
		// find free lambda slot
		i32 lambdaSlot = 512;
		for(i32 i=0; i<128; ++i)
		{
			if(pEngine->scriptLambdas[i].isActive == false)
			{
				lambdaSlot = i;
				break;
			}
		}
		
		FcScriptLambda* lambda = &pEngine->scriptLambdas[lambdaSlot];
		lambda->isActive = true;
		lambda->numSkipOps = 0;
		lambda->waitSeconds = 0.0f;
		lambda->lambdaName = pEngine->zeldaGameObject.playerState;
		lambda->eventName = SID("start");
		lambda->selfGameObject = &pEngine->zeldaGameObject;
		lambda->scriptBlob = &pEngine->zeldaStateScript;
	}
	
	for(u32 i=0; i<128; ++i)
	{
		FcScriptLambda* lambda = &pEngine->scriptLambdas[i];
		
		if(lambda->isActive == false)
			continue;
		
		if(lambda->waitSeconds > 0.0f)
		{
			lambda->waitSeconds = FM_MAX(lambda->waitSeconds - dt, 0.0f);
			
			if(lambda->waitSeconds > 0.0f)
			{
				continue;
			}
		}
		
		FcScriptCtx scriptCtx = {};
		scriptCtx.world = pEngine->pWorld;
		scriptCtx.gameObjectRegister = &pEngine->gameObjectRegister;
		scriptCtx.self = lambda->selfGameObject;
		scriptCtx.state = lambda->lambdaName;
		scriptCtx.stateEventToCall = lambda->eventName;
		scriptCtx.numSkipOps = lambda->numSkipOps;
		scriptCtx.sysCamera = pEngine->cameraSystem;
		fcScriptExecute(lambda->scriptBlob, &scriptCtx);
		
		if(scriptCtx.waitSeconds > 0.0f)
		{
			lambda->waitSeconds = scriptCtx.waitSeconds;
			lambda->numSkipOps = scriptCtx.numSkipOps;
		}
		else if(scriptCtx.nextState != 0)
		{
			lambda->selfGameObject->playerState = scriptCtx.nextState;
			lambda->lambdaName = scriptCtx.nextState;
			lambda->eventName = SID("start");
			lambda->numSkipOps = 0;
			lambda->waitSeconds = 0.0f;
		}
		else if(lambda->eventName == SID("start"))
		{
			lambda->eventName = SID("update");
			lambda->numSkipOps = 0;
			lambda->waitSeconds = 0.0f;
		}
	}
	
	if(pEngine->zeldaGameObject.playerState == SID("idle"))
	{
		// on update - upper-body layer in this case
		if(pEngine->inputTriangleActionPressed || pEngine->zeldaGameObject.equipItemNow)
		{
			pEngine->zeldaGameObject.equipItemNow = false;
			
			if(pEngine->zeldaGameObject.playerWindProtecting)
			{
				pEngine->zeldaGameObject.playerWindProtecting = false;
			}
			
			if(pEngine->zeldaGameObject.playerWeaponEquipped)
			{
				FcAnimActionArgs args = {};
				args.fadeInSec = 0.5f;
				args.layer = FA_CHAR_LAYER_PARTIAL;
				fcAnimCharacterScheduleActionNone(pEngine->zeldaGameObject.animCharacter, &args);
				
				pEngine->zeldaGameObject.playerWeaponEquipped = false;
			}
			else
			{
				FcAnimActionArgs args = {};
				args.fadeInSec = 0.5f;
				args.layer = FA_CHAR_LAYER_PARTIAL;
				fcAnimCharacterScheduleActionAnimate(pEngine->zeldaGameObject.animCharacter, &pEngine->actionWeaponEquipped, &args);
				
				pEngine->zeldaGameObject.playerWeaponEquipped = true;
			}
		}
	}
	
	if(pEngine->inputCircleActionPressed)
	{
		if(pEngine->zeldaGameObject.playerWeaponEquipped)
		{
			pEngine->zeldaGameObject.playerWeaponEquipped = false;
		}
		
		if(pEngine->zeldaGameObject.playerWindProtecting)
		{
			FcAnimActionArgs args = {};
			args.fadeInSec = 0.5f;
			args.layer = FA_CHAR_LAYER_PARTIAL;
			fcAnimCharacterScheduleActionNone(pEngine->zeldaGameObject.animCharacter, &args);
			
			pEngine->zeldaGameObject.playerWindProtecting = false;
		}
		else
		{
			FcAnimActionArgs args = {};
			args.fadeInSec = 0.5f;
			args.layer = FA_CHAR_LAYER_PARTIAL;
			fcAnimCharacterScheduleActionAnimate(pEngine->zeldaGameObject.animCharacter, &pEngine->actionWindProtect, &args);
			
			pEngine->zeldaGameObject.playerWindProtecting = true;
		}
	}
	
	// is grounnded raycast check
	{
		bool isGrounded = false;
		
		fm_vec4 pos = pEngine->zeldaGameObject.worldTransform.pos;
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
		
		pEngine->zeldaGameObject.isGrounded = isGrounded;
	}
	
	// jumping
	if(pEngine->inActionPressed && pEngine->zeldaGameObject.isGrounded)
	{
		pEngine->zeldaGameObject.isJump = true;
	}
	else
	{
		pEngine->zeldaGameObject.isJump = false;
	}
}

void fcAnimationUpdate(FcGameEngine* pEngine, f32 dt)
{
	// set last known world position for character
	fm_xform playerLocator;
	FcPhysicsPlayerInfo playerPhysics;
	playerPhysics.locator = &playerLocator;
	fcPhysicsGetPlayerInfo(pEngine->pPhysics, &playerPhysics);
	
	pEngine->zeldaGameObject.animCharacter->animInfo.worldPos.x = playerLocator.pos.x;
	pEngine->zeldaGameObject.animCharacter->animInfo.worldPos.y = playerLocator.pos.y;
	pEngine->zeldaGameObject.animCharacter->animInfo.worldPos.z = playerLocator.pos.z;
	
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

void fcGameEngineMainUpdate(FcGameEngine* pEngine, f32 dt, FcAllocator* allocator)
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
	
	// game
	FUR_PROFILE("gameplay-update")
	{
		fcScriptsUpdate(pEngine, dt);
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
		
		const f32 distanceToLookAtPoint = fm_vec4_distance(&lookAtPoint, &playerLocator.pos);
		
		if(pEngine->zeldaGameObject.animCharacter->animInfo.useLookAt)
		{
			pEngine->zeldaGameObject.animCharacter->animInfo.useLookAt = distanceToLookAtPoint < 10.0f;
		}
		else
		{
			pEngine->zeldaGameObject.animCharacter->animInfo.useLookAt = distanceToLookAtPoint < 6.0f;
		}
		
		pEngine->zeldaGameObject.animCharacter->animInfo.lookAtPoint.x = lookAtPoint.x;
		pEngine->zeldaGameObject.animCharacter->animInfo.lookAtPoint.y = lookAtPoint.y;
		pEngine->zeldaGameObject.animCharacter->animInfo.lookAtPoint.z = lookAtPoint.z;
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
		
		fm_vec4 playerDisplacement = pEngine->zeldaGameObject.velocity;
		
		// apply root motion from anim info to physics
		if(pEngine->zeldaGameObject.isJump)
		{
			pEngine->zeldaGameObject.velocity.z = 4.0f;
		}
		else if(pEngine->zeldaGameObject.isGrounded)
		{
			pEngine->zeldaGameObject.velocity.x = pEngine->zeldaGameObject.animCharacter->animInfo.rootMotionDelta.x / dt;
			pEngine->zeldaGameObject.velocity.y = pEngine->zeldaGameObject.animCharacter->animInfo.rootMotionDelta.y / dt;
			pEngine->zeldaGameObject.velocity.z = 0.0f;
		}
		else
		{
			const f32 speed = fm_vec4_mag(&pEngine->zeldaGameObject.velocity);
			if(speed > 0.0f)
			{
				pEngine->zeldaGameObject.velocity.x += pEngine->zeldaGameObject.animCharacter->animInfo.desiredMove.x * 1.2f;
				pEngine->zeldaGameObject.velocity.y += pEngine->zeldaGameObject.animCharacter->animInfo.desiredMove.y * 1.2f;
				fm_vec4_norm(&pEngine->zeldaGameObject.velocity);
				fm_vec4_mulf(&pEngine->zeldaGameObject.velocity, speed, &pEngine->zeldaGameObject.velocity);
			}
		}
		
		// apply gravity
		pEngine->zeldaGameObject.velocity.z += -9.81f * dt;
		
		fm_vec4_mulf(&pEngine->zeldaGameObject.velocity, dt, &playerDisplacement);
		physicsCtx.playerDisplacement = &playerDisplacement;
		fcPhysicsUpdate(pEngine->pPhysics, &physicsCtx);
		
		FcPhysicsPlayerInfo playerPhysics;
		playerPhysics.locator = &pEngine->zeldaGameObject.worldTransform;
		fcPhysicsGetPlayerInfo(pEngine->pPhysics, &playerPhysics);

		if(pEngine->zeldaGameObject.playerWindProtecting)
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
			fm_mat4_rot_z(pEngine->zeldaGameObject.animCharacter->animInfo.currentYaw, &playerMat);
			
			fm_vec4 invPlayerMove;
			fm_mat4_transform(&playerMat, &playerMove, &invPlayerMove);
			
			pEngine->windVelocity.x -= invPlayerMove.x;
			pEngine->windVelocity.y -= invPlayerMove.y;
			pEngine->windVelocity.z -= invPlayerMove.z;
		}
		
		// simulate hair dangles
		{
			FcPBDDangleCtx simCtx {};
			simCtx.dt = dt;
			
			fm_vec4 spherePos = pEngine->skinMatrices[pEngine->zeldaHeadIdx].w;
			const f32 sphereRadius = 0.08f;
			pEngine->zeldaDangleHairLeft.spherePos = &spherePos;
			pEngine->zeldaDangleHairLeft.sphereRadius = sphereRadius;
			pEngine->zeldaDangleHairRight.spherePos = &spherePos;
			pEngine->zeldaDangleHairRight.sphereRadius = sphereRadius;
			
			pEngine->zeldaDangleHairLeft.x0[0] = pEngine->skinMatrices[pEngine->zeldaDangleHairLeftIdx1].w;
			pEngine->zeldaDangleHairRight.x0[0] = pEngine->skinMatrices[pEngine->zeldaDangleHairRightIdx1].w;
			
			// add wind velocity
			for(u32 i=0; i<3; ++i)
			{
				pEngine->zeldaDangleHairLeft.v[i].x += pEngine->windVelocity.x * dt;
				pEngine->zeldaDangleHairLeft.v[i].y += pEngine->windVelocity.y * dt;
				pEngine->zeldaDangleHairLeft.v[i].z += pEngine->windVelocity.z * dt;
				
				pEngine->zeldaDangleHairRight.v[i].x += pEngine->windVelocity.x * dt;
				pEngine->zeldaDangleHairRight.v[i].y += pEngine->windVelocity.y * dt;
				pEngine->zeldaDangleHairRight.v[i].z += pEngine->windVelocity.z * dt;
			}
			
			fcPBDDangleSimulate(&simCtx, &pEngine->zeldaDangleHairLeft);
			fcPBDDangleSimulate(&simCtx, &pEngine->zeldaDangleHairRight);
			
			fm_mat4 m[3] = {};
			
			m[0] = pEngine->skinMatrices[pEngine->zeldaDangleHairLeftIdx1];
			fcPBDDangleToMatricesYDown(&pEngine->zeldaDangleHairLeft, &m[0], m);
			pEngine->skinMatrices[pEngine->zeldaDangleHairLeftIdx1] = m[0];
			pEngine->skinMatrices[pEngine->zeldaDangleHairLeftIdx2] = m[1];
			
			m[0] = pEngine->skinMatrices[pEngine->zeldaDangleHairRightIdx1];
			fcPBDDangleToMatricesYDown(&pEngine->zeldaDangleHairRight, &m[0], m);
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
			pEngine->zeldaCapeL.spherePos = &spherePos;
			pEngine->zeldaCapeL.sphereRadius = sphereRadius;
			pEngine->zeldaCapeC.spherePos = &spherePos;
			pEngine->zeldaCapeC.sphereRadius = sphereRadius;
			pEngine->zeldaCapeR.spherePos = &spherePos;
			pEngine->zeldaCapeR.sphereRadius = sphereRadius;
			
			pEngine->zeldaCapeL.x0[0] = pEngine->skinMatrices[pEngine->zeldaCapeIdxL[0]].w;
			pEngine->zeldaCapeC.x0[0] = pEngine->skinMatrices[pEngine->zeldaCapeIdxC[0]].w;
			pEngine->zeldaCapeR.x0[0] = pEngine->skinMatrices[pEngine->zeldaCapeIdxR[0]].w;
			
			// add wind velocity
			for(u32 i=0; i<4; ++i)
			{
				pEngine->zeldaCapeL.v[i].x += pEngine->windVelocity.x * dt;
				pEngine->zeldaCapeL.v[i].y += pEngine->windVelocity.y * dt;
				pEngine->zeldaCapeL.v[i].z += pEngine->windVelocity.z * dt;
				
				pEngine->zeldaCapeC.v[i].x += pEngine->windVelocity.x * dt;
				pEngine->zeldaCapeC.v[i].y += pEngine->windVelocity.y * dt;
				pEngine->zeldaCapeC.v[i].z += pEngine->windVelocity.z * dt;
				
				pEngine->zeldaCapeR.v[i].x += pEngine->windVelocity.x * dt;
				pEngine->zeldaCapeR.v[i].y += pEngine->windVelocity.y * dt;
				pEngine->zeldaCapeR.v[i].z += pEngine->windVelocity.z * dt;
			}
			
			fcPBDDangleSimulate(&simCtx, &pEngine->zeldaCapeL);
			fcPBDDangleSimulate(&simCtx, &pEngine->zeldaCapeC);
			fcPBDDangleSimulate(&simCtx, &pEngine->zeldaCapeR);
			
			fm_mat4 m[4] = {};
			
			m[0] = pEngine->skinMatrices[pEngine->zeldaCapeIdxL[0]];
			fcPBDDangleToMatricesYDown(&pEngine->zeldaCapeL, &m[0], m);
			
			for(u32 i=0; i<4; ++i)
			{
				pEngine->skinMatrices[pEngine->zeldaCapeIdxL[i]] = m[i];
			}
			
			m[0] = pEngine->skinMatrices[pEngine->zeldaCapeIdxC[0]];
			fcPBDDangleToMatricesYDown(&pEngine->zeldaCapeC, &m[0], m);
			
			for(u32 i=0; i<4; ++i)
			{
				pEngine->skinMatrices[pEngine->zeldaCapeIdxC[i]] = m[i];
			}
			
			m[0] = pEngine->skinMatrices[pEngine->zeldaCapeIdxR[0]];
			fcPBDDangleToMatricesYDown(&pEngine->zeldaCapeR, &m[0], m);
			
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
		// get zelda position
		fm_xform playerLocator = pEngine->zeldaGameObject.worldTransform;
		
		fm_mat4 zeldaMat;
		fm_mat4_rot_z(pEngine->zeldaGameObject.animCharacter->animInfo.currentYaw, &zeldaMat);
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
			cameraCtx.rotationYaw = pEngine->actionRotationLeftX;
			cameraCtx.rotationPitch = pEngine->actionRotationLeftY;
			cameraCtx.zoom = pEngine->actionZoomOut - pEngine->actionZoomIn;
			
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
		fm_vec4_mulf(&dirForward, maxSpeed * pEngine->actionMoveY, &playerMoveForward);
		fm_vec4 playerMoveLeft;
		fm_vec4_mulf(&dirLeft, maxSpeed * pEngine->actionMoveX, &playerMoveLeft);
		fm_vec4 playerMove;
		fm_vec4_add(&playerMoveForward, &playerMoveLeft, &playerMove);
		
		pEngine->zeldaGameObject.animCharacter->animInfo.animToLogicMotionRotationAlpha = 1.0f;
		pEngine->zeldaGameObject.animCharacter->animInfo.animToLogicMotionTranslationAlpha = 0.0f;
		pEngine->zeldaGameObject.animCharacter->animInfo.desiredMove.x = playerMove.x * dt;
		pEngine->zeldaGameObject.animCharacter->animInfo.desiredMove.y = playerMove.y * dt;
		
		pEngine->zeldaGameObject.logicMove.x = playerMove.x * dt;
		pEngine->zeldaGameObject.logicMove.y = playerMove.y * dt;
		pEngine->zeldaGameObject.logicMove.z = 0.0f;
		pEngine->zeldaGameObject.logicMove.w = 0.0f;
		
		pEngine->playerMove = playerMove;
		
		fm_mat4 cameraMatrix;
		fcCameraSystemViewMatrix(pEngine->cameraSystem, &cameraMatrix);
		
		if(!pEngine->zeldaGameObject.playerWeaponEquipped)
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
	FcAllocator* allocator = userData->allocator;
	
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

void furMainEngineLoop(FcGameEngine* pEngine, FcAllocator* allocator)
{
	FcMainThreadUserData data = {pEngine, allocator};
	
	FcJobDecl mainThreadJob = {};
	mainThreadJob.userData = &data;
	mainThreadJob.func = fur_engine_main_thread_loop;
	fcJobSystemSetupMainThreadJob(&mainThreadJob);
	
	// see fur_engine_main_thread_loop for the actual main thread loop
	fcJobSystemEnterWorkerThreadMode();
}

bool furMainEngineTerminate(FcGameEngine* pEngine, FcAllocator* allocator)
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
	
	fcPBDDangleRelease(&pEngine->dangle, allocator);
	fcPBDDangleRelease(&pEngine->zeldaDangleHairLeft, allocator);
	fcPBDDangleRelease(&pEngine->zeldaDangleHairRight, allocator);
	
	fcPBDDangleRelease(&pEngine->zeldaCapeL, allocator);
	fcPBDDangleRelease(&pEngine->zeldaCapeC, allocator);
	fcPBDDangleRelease(&pEngine->zeldaCapeR, allocator);
	
	fcAnimSystemRemoveCharacter(pEngine->animSystem, pEngine->zeldaGameObject.animCharacter);
	fcAnimSystemAnimCharacterRelease(pEngine->zeldaGameObject.animCharacter, allocator);
	
	fcWorldRelease(pEngine->pWorld, allocator);
	FUR_FREE(pEngine->pWorld, allocator);
	
	FUR_FREE(pEngine->gameObjectRegister.objects, allocator);
	FUR_FREE(pEngine->gameObjectRegister.ids, allocator);
	
	FUR_FREE(pEngine->scratchpadBuffer, allocator);
	
	fcAnimSystemRelease(pEngine->animSystem, allocator);
	
	fcCameraSystemRelease(pEngine->cameraSystem, allocator);
	
	fcJobSystemRelease(allocator);
	fcPhysicsRelease(pEngine->pPhysics, allocator);
	fcRendererRelease(pEngine->pRenderer, allocator);
	
	fcInputManagerRelease(pEngine->pInputManager, allocator);
	
	fcDepotUnmount(pEngine->depot, allocator);

	fcStringIdRegisterRelease(allocator);
	
	fcProfilerRelease(allocator);
	
	// release all memory before this call, otherwise it might be treated as memory leak
	fcApplicationRelease(pEngine->pApp, allocator);
	
	FUR_FREE(pEngine, allocator);	// rest of the deallocations should happen through allocators
	
	return true;
}

int main()
{
	FurGameEngineDesc desc = {};
	
	desc.m_mainApp.m_width = 1600;
	desc.m_mainApp.m_height = 900;
	desc.m_mainApp.m_title = "Furball Cat Game Engine";
	
	FcGameEngine* pEngine = NULL;
	
	FcAllocator* allocator = NULL;	// todo: if NULL, then uses default alloc callbacks
	
	// initialize most basic engine components
	bool initResult = furMainEngineInit(desc, &pEngine, allocator);
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
