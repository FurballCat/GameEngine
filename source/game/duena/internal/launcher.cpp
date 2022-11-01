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
#include "ccore/textParsing.h"
#include "ccore/serialize.h"
#include "cinput/public.h"
#include "cgame/world.h"

#include "camera.h"

/**************** FURBALL CAT GAME ENGINE ****************/

typedef union fs_variant_t
{
	fc_string_hash_t asStringHash;
	int32_t asInt32;
	bool asBool;
	float asFloat;
} fs_variant_t;

// ***** scripts core ***** //

typedef struct fg_game_object_t fg_game_object_t;

typedef struct fg_game_object_register_t
{
	fg_game_object_t** objects;
	fc_string_hash_t* ids;
	uint32_t numObjects;	// also numIds
	uint32_t capacity;
	
	fg_game_object_t* pPlayer; // for quick access
} fg_game_object_register_t;

typedef enum fs_seg_id_t
{
	FS_SEG_ID_UNKNOWN = 0,
	FS_SEG_ID_C_FUNC_CALL = 1,	// indicates function call, read fs_script_op_header next
	FS_SEG_ID_C_FUNC_ARG = 2,	// indicates function argument, read fs_variant_t next
	FS_SEG_ID_LAMBDA = 3,	// indicates whole lambda (sequence of function calls), read fs_lambda_header_t next
	FS_SEG_ID_STATE = 4,
	FS_SEG_ID_STATE_ON_EVENT = 5,
	FS_SEG_ID_STATE_TRACK = 6,
	FS_SEG_ID_STATE_SCRIPT = 7,
} fs_op_pre_flag_t;

typedef struct fs_script_ctx_t
{
	fc_string_hash_t state;
	fc_string_hash_t stateEventToCall;
	
	fc_string_hash_t nextState;
	fs_variant_t lastResult;
	float waitSeconds;
	uint32_t numSkipOps;
	
	fg_game_object_t* self;
	fg_game_object_register_t* gameObjectRegister;
	fg_world_t* world;
	
	// systems
	fg_camera_system_t* sysCamera;
} fs_script_ctx_t;

// todo: move it somewhere else
fs_variant_t fs_native_animate(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args);
fs_variant_t fs_native_wait_animate(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args);
fs_variant_t fs_native_equip_item(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args);
fs_variant_t fs_native_wait_seconds(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args);
fs_variant_t fs_native_get_variable(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args);
fs_variant_t fs_native_go(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args);
fs_variant_t fs_native_go_when(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args);
fs_variant_t fs_native_cmp_gt(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args);
fs_variant_t fs_native_cmp_eq(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args);

// camera script functions
fs_variant_t fs_native_camera_enable(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args);

typedef fs_variant_t (*fs_script_navitve_func_t)(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args);

typedef struct fs_native_func_entry_t
{
	fc_string_hash_t name;
	fs_script_navitve_func_t func;
	uint32_t numArgs;
} fs_native_func_entry_t;

fc_string_hash_t g_scriptNullOpCode = SID("__null");

fs_native_func_entry_t g_nativeFuncLookUp[] = {
	{ SID("animate"), fs_native_animate, 2 },
	{ SID("wait-animate"), fs_native_wait_animate, 2 },
	{ SID("equip-item"), fs_native_equip_item, 2 },
	{ SID("wait-seconds"), fs_native_wait_seconds, 1 },
	{ SID("get-variable"), fs_native_get_variable, 2 },
	{ SID("go"), fs_native_go, 1 },
	{ SID("go-when"), fs_native_go_when, 2 },
	{ SID("cmp-gt"), fs_native_cmp_gt, 2 },
	{ SID("cmp-eq"), fs_native_cmp_eq, 2 },
	{ SID("camera-enable"), fs_native_camera_enable, 3 },
	{ g_scriptNullOpCode, NULL, 0 }
};

typedef struct fs_script_op_t
{
	fs_script_navitve_func_t func;
	fs_variant_t* args;	// not owning this memory, it's just a pointer
	uint32_t numArgs;
} fs_script_op_t;

typedef struct fs_script_data_t
{
	fs_script_op_t* ops;	// sequence of operations
	fs_variant_t* allArgs; // owning memory to all args for all calls
	
	uint32_t numOps;
	uint32_t numAllArgs;
} fs_script_data_t;

typedef struct fs_script_state_t
{
	uint32_t idxOp;
} fs_script_state_t;

typedef enum fs_script_parsing_stage_t
{
	SPS_NONE = 0,
	SPS_READING,
	SPS_END,
} fs_script_parsing_stage_t;

/*
 
 Header
 Arg1
 Arg2
 Header
 Arg1
 ...
 
 **/

typedef struct fs_script_op_header
{
	fc_string_hash_t opCode;
	uint32_t flags;
	uint32_t numArgs;
} fs_script_op_header;

typedef struct fs_segment_header_t
{
	uint8_t segmentId;		// type of segment data (what to expect next)
	uint8_t padding;
	uint16_t dataSize; 		// segment size in bytes
	fc_string_hash_t name;	// unique name in segment scope (to know what to look for)
} fs_lambda_header_t;

typedef struct fs_script_execution_ctx_t
{
	fc_binary_buffer_stream_t scriptBufferStream;
	fs_script_ctx_t* scriptCtx;
	uint32_t numOpsExecuted;
	bool endOflambda;
} fs_script_execution_ctx_t;

fs_variant_t fs_execute_script_step(fs_script_execution_ctx_t* ctx)
{
	fc_binary_buffer_stream_t* stream = &ctx->scriptBufferStream;
	
	// peek what's next in buffer, but do not advance the stream
	uint8_t op_pre_flag = FS_SEG_ID_UNKNOWN;
	fc_peek_binary_buffer(stream, sizeof(uint8_t), &op_pre_flag);
	
	fs_variant_t result = {};
	
	if(op_pre_flag == FS_SEG_ID_C_FUNC_CALL)
	{
		// advance the stream to skip the peeked part
		uint32_t bytesRead = fc_read_binary_buffer(stream, sizeof(uint8_t), &op_pre_flag);
		FUR_ASSERT(bytesRead);
		
		ctx->numOpsExecuted += 1;
		
		// read operation header
		fs_script_op_header opHeader = {};
		bytesRead = fc_read_binary_buffer(stream, sizeof(fs_script_op_header), &opHeader);
		FUR_ASSERT(bytesRead);
		
		// read arguments
		FUR_ASSERT(opHeader.numArgs < 20);
		fs_variant_t args[20];
		
		for(uint32_t i=0; i<opHeader.numArgs; ++i)
		{
			args[i] = fs_execute_script_step(ctx);
		}
		
		// execute operation
		if(ctx->scriptCtx->numSkipOps == 0)
		{
			// find operation/function pointer - todo: implement simple cache
			const uint32_t numFuncs = FUR_ARRAY_SIZE(g_nativeFuncLookUp);
			uint32_t idxFunc = 0;
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
		uint32_t bytesRead = fc_read_binary_buffer(stream, sizeof(uint8_t), &op_pre_flag);
		FUR_ASSERT(bytesRead);
		
		bytesRead = fc_read_binary_buffer(stream, sizeof(fs_variant_t), &result);
		FUR_ASSERT(bytesRead);
	}
	else
	{
		ctx->endOflambda = true;
	}
	
	return result;
}

fs_seg_id_t fs_read_segment_header(fs_script_execution_ctx_t* ctx, fs_segment_header_t* header)
{
	// read segment header to know what's in
	uint32_t bytesRead = fc_read_binary_buffer(&ctx->scriptBufferStream, sizeof(fs_segment_header_t), header);
	FUR_ASSERT(bytesRead);
	
	return (fs_seg_id_t)header->segmentId;
}

fc_string_hash_t fs_skip_to_next_segment(fs_script_execution_ctx_t* ctx, fs_seg_id_t segmentType)
{
	fs_segment_header_t header = {};
	bool found = false;
	
	while(ctx->scriptBufferStream.pos <= ctx->scriptBufferStream.endPos)
	{
		// read segment header to know what's in
		uint32_t bytesRead = fc_read_binary_buffer(&ctx->scriptBufferStream, sizeof(fs_segment_header_t), &header);
		FUR_ASSERT(bytesRead);
		
		// is it the segment we are looking for?
		fs_seg_id_t thisSegmentType = (fs_seg_id_t)header.segmentId;
		if(thisSegmentType == segmentType)
		{
			found = true;
			return header.name;
		}
		
		// no? ok, let's skip to the next segment
		bytesRead = fc_read_binary_buffer(&ctx->scriptBufferStream, header.dataSize, NULL);
		FUR_ASSERT(bytesRead);
	}
	
	return 0;
}

bool fs_skip_until_segment(fs_script_execution_ctx_t* ctx, fs_seg_id_t segmentType, fc_string_hash_t segmentName)
{
	fc_string_hash_t name = 0;
	
	do
	{
		name = fs_skip_to_next_segment(ctx, segmentType);
		if(name == segmentName)
			return true;
	}
	while(name != 0);

	return false;
}

void fs_execute_script_lamba(fs_script_execution_ctx_t* ctx)
{
	do
	{
		fs_execute_script_step(ctx);
		
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

void fs_execute_script(const fc_binary_buffer_t* scriptBuffer, fs_script_ctx_t* scriptCtx)
{
	fs_script_execution_ctx_t ctx = {};
	ctx.scriptCtx = scriptCtx;
	fc_init_binary_buffer_stream(scriptBuffer, &ctx.scriptBufferStream);
	
	fc_string_hash_t segName = fs_skip_to_next_segment(&ctx, FS_SEG_ID_STATE_SCRIPT);
	FUR_ASSERT(segName);
	
	bool found = fs_skip_until_segment(&ctx, FS_SEG_ID_STATE, scriptCtx->state);
	FUR_ASSERT(found);
	
	found = fs_skip_until_segment(&ctx, FS_SEG_ID_STATE_ON_EVENT, scriptCtx->stateEventToCall);
	FUR_ASSERT(found);
	
	fs_segment_header_t segmentHeader = {};
	fs_read_segment_header(&ctx, &segmentHeader);
	
	if(segmentHeader.segmentId == FS_SEG_ID_STATE_TRACK)
	{
		// todo
		fs_read_segment_header(&ctx, &segmentHeader);
		FUR_ASSERT(segmentHeader.segmentId == FS_SEG_ID_LAMBDA);
	}
	
	// run lambda
	fs_execute_script_lamba(&ctx);
}

typedef struct fs_script_lambda_t
{
	float waitSeconds;
	uint32_t numSkipOps;
	bool isActive;
	fc_string_hash_t lambdaName;	// or state name
	fc_string_hash_t eventName;
	fg_game_object_t* selfGameObject;
	const fc_binary_buffer_t* scriptBlob;
} fs_script_lambda_t;

// ******************* //

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

typedef struct fg_animate_action_slots_t
{
	fa_action_animate_t slot[32];
} fg_animate_action_slots_t;

fa_action_animate_t* fg_animate_action_slots_get_free(fg_animate_action_slots_t* slots)
{
	for(uint32_t i=0; i<32; ++i)
	{
		if(slots->slot[i].reserved == false)
		{
			return &slots->slot[i];
		}
	}
	
	return NULL;
}

typedef struct fg_game_object_t
{
	fm_xform worldTransform;
	fc_string_hash_t name;	// access in scripts example: 'zelda
	fg_animate_action_slots_t animateActionSlots;
	
	fa_character_t* animCharacter;
	
	fm_vec4 logicMove;
	
	fc_string_hash_t playerState;
	bool playerWeaponEquipped;
	bool playerWindProtecting;
	bool equipItemNow;
	bool showAnimStateDebug;
	bool isJump;
	bool isGrounded;
	fm_vec4 velocity;
	
} fg_game_object_t;

const fc_binary_buffer_t* fg_load_script(fg_resource_register_t* reg, const char* path,
										 fc_string_hash_t name, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_ASSERT(reg->numScripts + 1 < FG_MAX_NUM_SCRIPTS);
	
	// load
	fc_binary_buffer_t* script = (fc_binary_buffer_t*)FUR_ALLOC_AND_ZERO(sizeof(fc_binary_buffer_t), 0, FC_MEMORY_SCOPE_SCRIPT, pAllocCallbacks);
	fc_load_binary_file_into_binary_buffer(path, script, pAllocCallbacks);
	
	// register
	const int32_t idx = reg->numScripts;
	reg->scriptsNames[idx] = name;
	reg->scripts[idx] = script;
	reg->numScripts++;
	
	return script;
}

const fa_anim_clip_t* fg_load_anim(fg_resource_register_t* reg, const fi_depot_t* depot, const char* name,
								   const fa_rig_t* rig, fc_alloc_callbacks_t* pAllocCallbacks)
{
	fa_anim_clip_t* animClip = NULL;
	
	{
		const char* directory = "assets/characters/zelda/animations/";
		const char* extension = ".fbx";
		const char* engineExtension = ".anim";
		const size_t dirLength = strlen(directory);
		const size_t nameLength = strlen(name);
		
		char pathEngine[256] = {};
		memcpy(pathEngine, depot->path, strlen(depot->path));
		memcpy(pathEngine + strlen(depot->path), directory, dirLength);
		memcpy(pathEngine + strlen(depot->path) + dirLength, name, nameLength);
		memcpy(pathEngine + strlen(depot->path) + dirLength + nameLength, engineExtension, strlen(engineExtension));
		
		FILE* engineFile = fopen(pathEngine, "rb");
		
		// import animation if not done yet
		if(!engineFile)
		{
			char pathImport[256] = {};
			memcpy(pathImport, directory, dirLength);
			memcpy(pathImport + dirLength, name, nameLength);
			memcpy(pathImport + dirLength + nameLength, extension, strlen(extension));
			
			fi_import_anim_clip_ctx_t ctx = {};
			ctx.path = pathImport;
			ctx.extractRootMotion = true;
			ctx.rig = rig;
			
			fi_import_anim_clip(depot, &ctx, &animClip, pAllocCallbacks);
		}
		
		// try loading or saving engine file
		{
			fc_serializer_t serializer = {};
			serializer.file = engineFile ? engineFile : fopen(pathEngine, "wb");
			serializer.isWriting = !engineFile;
			
			if(!serializer.isWriting)
			{
				animClip = (fa_anim_clip_t*)FUR_ALLOC_AND_ZERO(sizeof(fa_anim_clip_t), 0, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
			}
			
			fa_anim_clip_serialize(&serializer, animClip, pAllocCallbacks);
			
			fclose(serializer.file);
		}
		
		// register animation
		const int32_t idxAnim = reg->numAnimations;
		FUR_ASSERT(idxAnim < FG_MAX_NUM_ANIMATIONS);
		reg->animationsNames[idxAnim] = SID_REG(name);
		reg->animations[idxAnim] = animClip;
		reg->numAnimations++;
	}
	
	return animClip;
}

// game object types
typedef struct fg_go_zelda_t
{
	fg_game_object_2_t info;	// this property must be the first one in every game object
	
	fs_script_lambda_t script;
	
	fg_animate_action_slots_t animateActionSlots;
	
	fa_character_t* animCharacter;
	fr_proxy_t* mesh;
	
	fm_vec4 logicMove;
	
	fc_string_hash_t playerState;
	bool playerWeaponEquipped;
	bool playerWindProtecting;
	bool equipItemNow;
	bool showAnimStateDebug;
} fg_game_object_zelda_t;

bool fg_go_zelda_init(fg_game_object_2_t* gameObject, fg_game_object_init_ctx_t* ctx)
{
	FUR_PROFILE("init-zelda")
	{
		fg_go_zelda_t* zelda = (fg_go_zelda_t*)gameObject;
		
		zelda->playerState = fg_spawn_info_get_string_hash(ctx->info, SID("state"), SID("idle"));
	}
	
	return true;
}

void fg_go_zelda_update(fg_game_object_2_t* gameObject, fg_game_object_update_ctx_t* ctx)
{
	FUR_PROFILE("update-zelda")
	{
		
	}
}

void fg_register_type_factories()
{
	// zelda
	{
		fg_type_factory_t factory = {};
		factory.updateBucket = FG_UPDATE_BUCKET_CHARACTERS;
		factory.fn.init = fg_go_zelda_init;
		factory.fn.update = fg_go_zelda_update;
		factory.memoryMaxSize = sizeof(fg_go_zelda_t);
		
		fg_type_factory_register_new(SID("zelda"), factory);
	}
}

// the engine
struct FurGameEngine
{
	struct fr_app_t* pApp;
	struct fr_renderer_t* pRenderer;
	fp_physics_t* pPhysics;
	
	std::chrono::system_clock::time_point prevTimePoint;
	float globalTime;
	
	fg_world_t* pWorld;
	
	fi_input_manager_t* pInputManager;
	
	// animation
	fa_rig_t* pRig;
	const fa_anim_clip_t* pAnimClipWindProtect;
	const fa_anim_clip_t* pAnimClipHoldSword;
	
	// input actions
	bool inActionPressed;
	bool inputTriangleActionPressed;
	bool inputCircleActionPressed;
	bool inputXPressed;
	float actionRotationLeftX;
	float actionRotationLeftY;
	float actionZoomIn;
	float actionZoomOut;
	float actionMoveX;
	float actionMoveY;
	
	fm_vec4 playerMove;
	
	// camera
	fg_camera_system_t* cameraSystem;
	
	// gameplay animation states
	fa_character_t animCharacterZelda;
	fa_action_animate_t actionWeaponEquipped;
	fa_action_animate_t actionWindProtect;
	
	// skinning
	fm_mat4 skinMatrices[512];
	
	// meshes
	fr_proxy_t* swordMesh;
	fr_proxy_t* chestMesh;
	
	fr_proxy_t* rockMeshes[5];
	fr_proxy_t* blockMesh;
	
	fm_xform blockPositions[2];
	
	fr_proxy_t* zeldaMesh;
	
	// update memory (scratchpad)
	void* scratchpadBuffer;
	uint32_t scratchpadBufferSize;
	
	// game objects
	fg_game_object_register_t gameObjectRegister;
	
	// scripts temp
	fc_binary_buffer_t zeldaStateScript;
	fg_game_object_t zeldaGameObject;
	
	fs_script_lambda_t scriptLambdas[128];
	
	// test dangle
	fa_dangle dangle;
	
	// hair dangles
	fa_dangle zeldaDangleHairLeft;
	fa_dangle zeldaDangleHairRight;
	uint32_t zeldaDangleHairLeftIdx1;
	uint32_t zeldaDangleHairLeftIdx2;
	uint32_t zeldaDangleHairRightIdx1;
	uint32_t zeldaDangleHairRightIdx2;
	uint32_t zeldaHeadIdx;
	uint32_t zeldaHandRightIdx;
	
	// cape dangles
	fa_dangle zeldaCapeL;
	uint32_t zeldaCapeIdxL[4];
	
	fa_dangle zeldaCapeC;
	uint32_t zeldaCapeIdxC[4];
	
	fa_dangle zeldaCapeR;
	uint32_t zeldaCapeIdxR[4];
	
	uint32_t zeldaSpineIdx;
	
	// wind
	fm_vec3 windVelocity;
	
	// test bvh
	fp_bvh_t testBVH;
	
	// debug
	bool debugIsSlowTime;
	bool debugShowFPS;
	bool debugShowMemoryStats;
};

// Furball Cat - Platform
bool furMainEngineInit(const FurGameEngineDesc& desc, FurGameEngine** ppEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
	fc_profiler_init(pAllocCallbacks);
	
	FurGameEngine* pEngine = (FurGameEngine*)malloc(sizeof(FurGameEngine));
	memset(pEngine, 0, sizeof(FurGameEngine));
	
	fr_app_desc_t appDesc;
	appDesc.appTitle = desc.m_mainApp.m_title;
	appDesc.viewportWidth = desc.m_mainApp.m_width;
	appDesc.viewportHeight = desc.m_mainApp.m_height;
	
	fr_result_t res = fr_create_app(&appDesc, &pEngine->pApp, pAllocCallbacks);
	
	fc_string_hash_register_init(pAllocCallbacks);
	
	if(res == FR_RESULT_OK)
	{
		pEngine->pInputManager = fi_input_manager_create(pAllocCallbacks);
	}
	
	if(res == FR_RESULT_OK)
	{
		fr_renderer_desc_t rendererDesc;
		rendererDesc.pApp = pEngine->pApp;
		
		res = fr_create_renderer(&rendererDesc, &pEngine->pRenderer, pAllocCallbacks);
	}
	
	if(res == FR_RESULT_OK)
	{
		pEngine->pPhysics = fp_physics_create(pAllocCallbacks);
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
	
	// create world
	pEngine->pWorld = (fg_world_t*)FUR_ALLOC_AND_ZERO(sizeof(fg_world_t), 0, FC_MEMORY_SCOPE_GAME, pAllocCallbacks);
	fg_world_init(pEngine->pWorld, pAllocCallbacks);
	pEngine->pWorld->systems.renderer = pEngine->pRenderer;
	
	// load scripts
	{
		fc_load_binary_file_into_binary_buffer("../../../../../scripts/zelda.fs", &pEngine->zeldaStateScript, pAllocCallbacks);
	}
	
	// init camera
	pEngine->cameraSystem = fg_camera_system_create(pAllocCallbacks);
	
	// default camera
	{
		fg_camera_params_follow_t params = {};
		params.height = 0.9f;
		params.zoom = 1.0f;
		params.poleLength = 3.0f;
		params.fov = 45.0f;
		fg_camera_system_enable_camera_follow(pEngine->cameraSystem, &params, 0.0f);
	}
	
	// init scratchpad buffer
	pEngine->scratchpadBufferSize = 256 * 1024;
	pEngine->scratchpadBuffer = FUR_ALLOC_AND_ZERO(pEngine->scratchpadBufferSize, 16, FC_MEMORY_SCOPE_GLOBAL, pAllocCallbacks);
	
	// init type factories
	fg_register_type_factories();
	
	// load resources
	{
		const char* depotPath = "../../../../../";

		fi_depot_t depot;
		depot.path = depotPath;

		const char* characterRigPath = "assets/characters/zelda/animations/zelda-a-pose.fbx";
		char pathRigEngine[256] = {};
		fc_path_concat(pathRigEngine, depotPath, "assets/characters/zelda/animations/", "zelda-a-pose", ".rig");
		
		// load rig
		{
			FILE* engineFile = fopen(pathRigEngine, "rb");
			
			if(engineFile)
			{
				fc_serializer_t ser = {};
				ser.file = engineFile;
				ser.isWriting = false;
				pEngine->pRig = (fa_rig_t*)FUR_ALLOC_AND_ZERO(sizeof(fa_rig_t), 0, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
				fa_rig_serialize(&ser, pEngine->pRig, pAllocCallbacks);
			}
		}
		
		// import rig
		if(!pEngine->pRig)
		{
			fi_import_rig_ctx_t ctx = {};
			ctx.path = characterRigPath;
			
			fi_import_rig(&depot, &ctx, &pEngine->pRig, pAllocCallbacks);
			
			// apply rig properties
			{
				// set locomotion joint to track and apply root motion
				{
					pEngine->pRig->idxLocoJoint = fa_rig_find_bone_idx(pEngine->pRig, SID("motion"));
				}
				
				// left leg IK setup
				{
					fa_ik_setup_t* ik = &pEngine->pRig->ikLeftLeg;
					ik->idxBeginParent = fa_rig_find_bone_idx(pEngine->pRig, SID("Bip001_Pelvis"));
					ik->idxBegin = fa_rig_find_bone_idx(pEngine->pRig, SID("Bip001_Thigh_L"));
					ik->idxMid = fa_rig_find_bone_idx(pEngine->pRig, SID("Bip001_Calf_L"));
					ik->idxEnd = fa_rig_find_bone_idx(pEngine->pRig, SID("Bip001_Foot_L"));
					ik->hingeAxisMid = FM_AXIS_Z;
					ik->minAngle = 0.02f;
					ik->maxAngle = 2.8f;
				}
				
				// right leg IK setup
				{
					fa_ik_setup_t* ik = &pEngine->pRig->ikRightLeg;
					ik->idxBeginParent = fa_rig_find_bone_idx(pEngine->pRig, SID("Bip001_Pelvis"));
					ik->idxBegin = fa_rig_find_bone_idx(pEngine->pRig, SID("Bip001_Thigh_R"));
					ik->idxMid = fa_rig_find_bone_idx(pEngine->pRig, SID("Bip001_Calf_R"));
					ik->idxEnd = fa_rig_find_bone_idx(pEngine->pRig, SID("Bip001_Foot_R"));
					ik->hingeAxisMid = FM_AXIS_NEG_Z;
					ik->minAngle = 0.02f;
					ik->maxAngle = 2.8f;
				}
				
				// head look-at setup
				{
					fa_look_at_setup_t* lookAt = &pEngine->pRig->headLookAt;
					lookAt->idxHead = fa_rig_find_bone_idx(pEngine->pRig, SID("Bip001_Head"));
					lookAt->idxNeck = fa_rig_find_bone_idx(pEngine->pRig, SID("Bip001_Neck"));
					lookAt->idxSpine3 = fa_rig_find_bone_idx(pEngine->pRig, SID("Bip001_Spine3"));
					lookAt->limitYaw = FM_DEG_TO_RAD(60.0f);
					lookAt->limitPitchDown = FM_DEG_TO_RAD(25.0f);
					lookAt->limitPitchUp = FM_DEG_TO_RAD(45.0f);
				}
				
				// masks
				{
					pEngine->pRig->maskUpperBody = FUR_ALLOC_ARRAY_AND_ZERO(uint8_t, pEngine->pRig->numBones, 0, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
					const int16_t idxSpine = fa_rig_find_bone_idx(pEngine->pRig, SID("Bip001_Spine"));
					const fc_string_hash_t hashes[9] = {
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
					
					const fc_string_hash_t hashesPartial[18] = {
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
					
					const fc_string_hash_t noHashes[2] = {
						SID("rootTransform"),
						SID("motion")
					};
					
					if(idxSpine != -1)
					{
						for(uint32_t i=0; i<pEngine->pRig->numBones; ++i)
						{
							uint8_t w = 220;
							for(uint32_t j=0; j<9; ++j)
							{
								if(pEngine->pRig->boneNameHashes[i] == hashes[j])
								{
									w = 0;
									break;
								}
							}
							for(uint32_t j=0; j<18; ++j)
							{
								if(pEngine->pRig->boneNameHashes[i] == hashesPartial[j])
								{
									w = 100;
								}
							}
							for(uint32_t j=0; j<2; ++j)
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
					pEngine->pRig->maskFace = FUR_ALLOC_ARRAY_AND_ZERO(uint8_t, pEngine->pRig->numBones, 0, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
					const int16_t idxSpine = fa_rig_find_bone_idx(pEngine->pRig, SID("Bip001_Spine"));
					const fc_string_hash_t hashes[] = {
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
						for(uint32_t i=0; i<pEngine->pRig->numBones; ++i)
						{
							uint8_t w = 0;
							const uint32_t numHashes = FUR_ARRAY_SIZE(hashes);
							for(uint32_t j=0; j<numHashes; ++j)
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
					pEngine->pRig->maskHands = FUR_ALLOC_ARRAY_AND_ZERO(uint8_t, pEngine->pRig->numBones, 0, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
					const int16_t idxSpine = fa_rig_find_bone_idx(pEngine->pRig, SID("Bip001_Spine"));
					const fc_string_hash_t hashes[] = {
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
						for(uint32_t i=0; i<pEngine->pRig->numBones; ++i)
						{
							uint8_t w = 0;
							const uint32_t numHashes = FUR_ARRAY_SIZE(hashes);
							for(uint32_t j=0; j<numHashes; ++j)
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
			
			fc_serializer_t ser = {};
			ser.file = engineFile;
			ser.isWriting = true;
			fa_rig_serialize(&ser, pEngine->pRig, pAllocCallbacks);
		}

		pEngine->pAnimClipWindProtect = fg_load_anim(&pEngine->pWorld->resources, &depot, "zelda-upper-wind-protect", pEngine->pRig, pAllocCallbacks);
		pEngine->pAnimClipHoldSword = fg_load_anim(&pEngine->pWorld->resources, &depot, "zelda-upper-hold-sword", pEngine->pRig, pAllocCallbacks);
		
		fg_load_anim(&pEngine->pWorld->resources, &depot, "zelda-idle-stand-relaxed", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, &depot, "zelda-funny-poses", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, &depot, "zelda-funny-pose-2", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, &depot, "zelda-funny-pose-3", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, &depot, "zelda-funny-pose-4", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, &depot, "zelda-loco-run-relaxed", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, &depot, "zelda-run-to-idle-sharp", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, &depot, "zelda-loco-idle-to-run-0", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, &depot, "zelda-loco-jump-in-place", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, &depot, "zelda-loco-jump", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, &depot, "zelda-additive", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, &depot, "zelda-a-pose", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, &depot, "zelda-face-idle", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, &depot, "zelda-idle-stand-01", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, &depot, "zelda-hands-idle", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, &depot, "zelda-wind-01", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, &depot, "zelda-jump-loop", pEngine->pRig, pAllocCallbacks);
		
		// load meshes
		{
			fr_load_mesh_ctx_t meshCtx = {};
			meshCtx.path = "assets/characters/zelda/mesh/";
			meshCtx.fileName = "zelda_sword";
			const char* texturePaths[] = {"assets/characters/zelda/mesh/textures/melee_diff.png"};
			const int32_t textureIndices[] = {0};
			meshCtx.texturePaths = texturePaths;
			meshCtx.numTextures = FUR_ARRAY_SIZE(texturePaths);
			meshCtx.textureIndices = textureIndices;
			meshCtx.numTextureIndices = FUR_ARRAY_SIZE(textureIndices);
			pEngine->swordMesh = fr_load_mesh(pEngine->pRenderer, &depot, &meshCtx, pAllocCallbacks);
		}
		
		// load chest mesh
		{
			fr_load_mesh_ctx_t meshCtx = {};
			meshCtx.path = "assets/chest/";
			meshCtx.fileName = "chest";
			const char* texturePaths[] = {"assets/chest/chest_albedo.png"};
			const int32_t textureIndices[] = {0};
			meshCtx.texturePaths = texturePaths;
			meshCtx.numTextures = FUR_ARRAY_SIZE(texturePaths);
			meshCtx.textureIndices = textureIndices;
			meshCtx.numTextureIndices = FUR_ARRAY_SIZE(textureIndices);
			pEngine->chestMesh = fr_load_mesh(pEngine->pRenderer, &depot, &meshCtx, pAllocCallbacks);
		}
		
		// load block mesh
		{
			fr_load_mesh_ctx_t meshCtx = {};
			meshCtx.path = "assets/sketchfab/cube-world-stone-block-1-pbr-series/source/";
			meshCtx.fileName = "skull_block_PBR_fc";
			const char* texturePaths[] = {"assets/sketchfab/cube-world-stone-block-1-pbr-series/textures/b_stone1_Color.png"};
			const int32_t textureIndices[] = {0};
			meshCtx.texturePaths = texturePaths;
			meshCtx.numTextures = FUR_ARRAY_SIZE(texturePaths);
			meshCtx.textureIndices = textureIndices;
			meshCtx.numTextureIndices = FUR_ARRAY_SIZE(textureIndices);
			pEngine->blockMesh = fr_load_mesh(pEngine->pRenderer, &depot, &meshCtx, pAllocCallbacks);
		}
		
		// set block positions and create colliders
		{
			pEngine->blockPositions[0] = {{4.0f, 4.0f, 0.0f, 1.0f}, {0.0f, 0.0f, 0.0f, 1.0f}};
			pEngine->blockPositions[1] = {{6.0f, 4.0f, 0.5f, 1.0f}, {0.0f, 0.0f, 0.0f, 1.0f}};
			
			const fm_vec3 halfExtents = {0.5f, 0.5f, 0.5f};
			
			for(int32_t i=0; i<2; ++i)
			{
				fp_physics_add_static_box(pEngine->pPhysics, &pEngine->blockPositions[i], &halfExtents, pAllocCallbacks);
			}
		}
		
		// load rock meshes
		for(uint32_t i=0; i<5; ++i)
		{
			char txtPath[256];
			sprintf(txtPath, "rock-0%i", i+1);
			
			char txtTexturePath[256];
			sprintf(txtTexturePath, "assets/rocks/rock-0%i.png", i+1);
			
			fr_load_mesh_ctx_t meshCtx = {};
			meshCtx.path = "assets/rocks/";
			meshCtx.fileName = txtPath;
			const char* texturePaths[] = {txtTexturePath};
			const int32_t textureIndices[] = {0};
			meshCtx.texturePaths = texturePaths;
			meshCtx.numTextures = FUR_ARRAY_SIZE(texturePaths);
			meshCtx.textureIndices = textureIndices;
			meshCtx.numTextureIndices = FUR_ARRAY_SIZE(textureIndices);
			pEngine->rockMeshes[i] = fr_load_mesh(pEngine->pRenderer, &depot, &meshCtx, pAllocCallbacks);
		}
		
		// load zelda mesh
		{
			fr_load_mesh_ctx_t meshCtx = {};
			meshCtx.path = "assets/characters/zelda/mesh/";
			meshCtx.fileName = "zelda_mesh";
			const char* texturePaths[] = {"assets/characters/zelda/mesh/textures/zelda_diff.png",
				"assets/characters/zelda/mesh/textures/hair_diff.png",
				"assets/characters/zelda/mesh/textures/eyes_diff2.png"
			};
			const int32_t textureIndices[] = {0, 0, 1, 0, 2, 0, 1};
			meshCtx.texturePaths = texturePaths;
			meshCtx.numTextures = FUR_ARRAY_SIZE(texturePaths);
			meshCtx.textureIndices = textureIndices;
			meshCtx.numTextureIndices = FUR_ARRAY_SIZE(textureIndices);
			meshCtx.isSkinned = true;
			meshCtx.boneNames = pEngine->pRig->boneNameHashes;
			meshCtx.numBones = pEngine->pRig->numBones;
			pEngine->zeldaMesh = fr_load_mesh(pEngine->pRenderer, &depot, &meshCtx, pAllocCallbacks);
		}
	}
	
	// init game
	{
		pEngine->gameObjectRegister.capacity = 128;
		pEngine->gameObjectRegister.objects = FUR_ALLOC_ARRAY_AND_ZERO(fg_game_object_t*, pEngine->gameObjectRegister.capacity, 0, FC_MEMORY_SCOPE_SCRIPT, pAllocCallbacks);
		pEngine->gameObjectRegister.ids = FUR_ALLOC_ARRAY_AND_ZERO(fc_string_hash_t, pEngine->gameObjectRegister.capacity, 0, FC_MEMORY_SCOPE_SCRIPT, pAllocCallbacks);
		pEngine->gameObjectRegister.numObjects = 0;
		
		// create Zelda
		fa_character_init(&pEngine->animCharacterZelda, pEngine->pRig, pAllocCallbacks);
		
		pEngine->actionWeaponEquipped.animation = pEngine->pAnimClipHoldSword;
		pEngine->actionWeaponEquipped.forceLoop = true;
		
		pEngine->actionWindProtect.animation = pEngine->pAnimClipWindProtect;
		pEngine->actionWindProtect.forceLoop = true;
		
		pEngine->animCharacterZelda.globalTime = pEngine->globalTime;
		
		fa_action_args_t args = {};
		args.fadeInSec = 0.3f;
		args.ikMode = FA_IK_MODE_LEGS;
		//fa_character_schedule_action_simple(&pEngine->animCharacterZelda, &pEngine->animSimpleAction, &args);
		
		pEngine->zeldaGameObject.name = SID_REG("zelda");
		pEngine->zeldaGameObject.animCharacter = &pEngine->animCharacterZelda;
		
		// register Zelda (player) game object
		pEngine->gameObjectRegister.objects[pEngine->gameObjectRegister.numObjects] = &pEngine->zeldaGameObject;
		pEngine->gameObjectRegister.ids[pEngine->gameObjectRegister.numObjects] = pEngine->zeldaGameObject.name;
		pEngine->gameObjectRegister.numObjects += 1;
		
		// reguster player game object
		pEngine->gameObjectRegister.pPlayer = &pEngine->zeldaGameObject;
	}
	
	// init dangle
	{
		const uint32_t numParticles = 4;
		const float segmentLength = 0.2f;
		
		fa_dangle_desc desc;
		desc.frequency = 60.0f;
		desc.numParticles = numParticles;
		desc.dampingCoef = 0.96f;
		
		fa_dangle_create(&desc, &pEngine->dangle, pAllocCallbacks);
		
		fm_vec4 pos = {0.0f, 0.0f, 1.0f};
		for(uint32_t i=0; i<numParticles; ++i)
		{
			pEngine->dangle.x0[i] = pos;
			pos.x += segmentLength;
		}
		
		for(uint32_t i=0; i<numParticles; ++i)
		{
			pEngine->dangle.d[i] = segmentLength;
		}
	}
	
	// init special bone indices
	{
		const fc_string_hash_t handRight = SID("Bip001_Hand_R");
		
		for(uint32_t i=0; i<pEngine->pRig->numBones; ++i)
		{
			if(pEngine->pRig->boneNameHashes[i] == handRight)
				pEngine->zeldaHandRightIdx = i;
		}
	}
	
	// init hair dangles
	{
		fa_dangle_desc desc;
		desc.frequency = 60.0f;
		desc.numParticles = 3;
		desc.dampingCoef = 0.96f;
		
		fa_dangle_create(&desc, &pEngine->zeldaDangleHairLeft, pAllocCallbacks);
		fa_dangle_create(&desc, &pEngine->zeldaDangleHairRight, pAllocCallbacks);
		
		const fc_string_hash_t hair_r = SID("Bip001_Hair_R");
		const fc_string_hash_t hair_r2 = SID("Bip001_Hair1_R");
		const fc_string_hash_t hair_l = SID("Bip001_Hair_L");
		const fc_string_hash_t hair_l2 = SID("Bip001_Hair1_L");
		const fc_string_hash_t head = SID("Bip001_Head");
		const fc_string_hash_t spine = SID("Bip001_Spine");
		
		for(uint32_t i=0; i<pEngine->pRig->numBones; ++i)
		{
			const fc_string_hash_t name = pEngine->pRig->boneNameHashes[i];
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
		
		const float dLeft = fm_vec4_mag(&refPoseLeft2.pos);
		const float dRight = fm_vec4_mag(&refPoseRight2.pos);
		
		pEngine->zeldaDangleHairLeft.d[0] = dLeft;
		pEngine->zeldaDangleHairLeft.d[1] = dLeft;
		
		pEngine->zeldaDangleHairRight.d[0] = dRight;
		pEngine->zeldaDangleHairRight.d[1] = dRight;
	}
	
	// init cape dangles
	{
		fa_dangle_desc desc;
		desc.frequency = 60.0f;
		desc.numParticles = 4;
		desc.dampingCoef = 0.96f;
		
		fa_dangle_create(&desc, &pEngine->zeldaCapeL, pAllocCallbacks);
		fa_dangle_create(&desc, &pEngine->zeldaCapeC, pAllocCallbacks);
		fa_dangle_create(&desc, &pEngine->zeldaCapeR, pAllocCallbacks);
		
		const fc_string_hash_t cape_names_l[4] = {
			SID("Bip001_Cape_L"),
			SID("Bip001_Cape1_L"),
			SID("Bip001_Cape2_L"),
			SID("Bip001_Cape3_L")
		};
		
		const fc_string_hash_t cape_names_c[4] = {
			SID("Bip001_Cape_C"),
			SID("Bip001_Cape1_C"),
			SID("Bip001_Cape2_C"),
			SID("Bip001_Cape3_C")
		};
		
		const fc_string_hash_t cape_names_r[4] = {
			SID("Bip001_Cape_R"),
			SID("Bip001_Cape1_R"),
			SID("Bip001_Cape2_R"),
			SID("Bip001_Cape3_R")
		};
		
		for(uint32_t i=0; i<pEngine->pRig->numBones; ++i)
		{
			const fc_string_hash_t name = pEngine->pRig->boneNameHashes[i];
			
			for(uint32_t j=0; j<4; ++j)
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
		
		for(uint32_t j=0; j<4; ++j)
		{
			fm_xform refPose = pEngine->pRig->refPose[pEngine->zeldaCapeIdxL[j]];
			refPose.pos.w = 0.0f;
			pEngine->zeldaCapeL.d[j] = fm_vec4_mag(&refPose.pos);
		}
		
		for(uint32_t j=0; j<4; ++j)
		{
			fm_xform refPose = pEngine->pRig->refPose[pEngine->zeldaCapeIdxC[j]];
			refPose.pos.w = 0.0f;
			pEngine->zeldaCapeC.d[j] = fm_vec4_mag(&refPose.pos);
		}
		
		for(uint32_t j=0; j<4; ++j)
		{
			fm_xform refPose = pEngine->pRig->refPose[pEngine->zeldaCapeIdxR[j]];
			refPose.pos.w = 0.0f;
			pEngine->zeldaCapeR.d[j] = fm_vec4_mag(&refPose.pos);
		}
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
		
		const uint32_t numBoxes = FUR_ARRAY_SIZE(boxes);
		
		fc_mem_arena_alloc_t memArena = fc_mem_arena_make(pEngine->scratchpadBuffer, pEngine->scratchpadBufferSize);
		
		fp_bvh_build_ctx_t bvhCtx = {};
		bvhCtx.numObjects = numBoxes;
		bvhCtx.objectBoxes = boxes;
		bvhCtx.arenaAlloc = &memArena;
		
		fp_bvh_build(&bvhCtx, &pEngine->testBVH, pAllocCallbacks);
	}
		
	return true;
}

fg_game_object_t* fs_look_up_game_object(fs_script_ctx_t* ctx, fc_string_hash_t name)
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
		for(uint32_t i=0; i<ctx->gameObjectRegister->numObjects; ++i)
		{
			if(ctx->gameObjectRegister->ids[i] == name)
			{
				return ctx->gameObjectRegister->objects[i];
			}
		}
	}
	
	return NULL;
}

typedef enum fs_animate_arg_t
{
	FS_ANIMATE_ARG_FADE_IN_CURVE = 0,
	FS_ANIMATE_ARG_FADE_IN_SEC,
	FS_ANIMATE_ARG_FADE_OUT_CURVE,
	FS_ANIMATE_ARG_FADE_OUT_SEC,
	FS_ANIMATE_ARG_IK_MODE,
	FS_ANIMATE_ARG_LAYER,
	FS_ANIMATE_ARG_LAYER_NAME,
} fs_animate_arg_t;

fs_variant_t fs_native_animate(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args)
{
	FUR_ASSERT(numArgs >= 2);
	const fc_string_hash_t objectName = args[0].asStringHash;
	const fc_string_hash_t animName = args[1].asStringHash;
	
	// find game object
	fg_game_object_t* gameObj = fs_look_up_game_object(ctx, objectName);
	FUR_ASSERT(gameObj);
	
	fa_action_animate_t* animateSlot = fg_animate_action_slots_get_free(&gameObj->animateActionSlots);
	FUR_ASSERT(animateSlot);
	
	const fa_anim_clip_t* animClip = fg_resource_find_anim(&ctx->world->resources, animName);
	FUR_ASSERT(animClip);
	
	animateSlot->animation = animClip;
	animateSlot->forceLoop = true;
	animateSlot->reserved = true;
	
	fa_action_args_t animArgs = {};
	
	// get variadic arguments - iterate every 2 arguments, as we need (animate-arg ENUM) VALUE, this is two variadic arguments
	for(uint32_t i=2; i+1<numArgs; i += 2)
	{
		const fs_animate_arg_t arg_enum = (fs_animate_arg_t)args[i].asInt32;
		switch(arg_enum)
		{
			case FS_ANIMATE_ARG_FADE_IN_SEC:
				animArgs.fadeInSec = args[i+1].asFloat;
				break;
			case FS_ANIMATE_ARG_FADE_IN_CURVE:
				animArgs.fadeInCurve = (fa_curve_type_t)args[i+1].asInt32;
				break;
			case FS_ANIMATE_ARG_FADE_OUT_SEC:
				animArgs.fadeOutSec = args[i+1].asFloat;
				break;
			case FS_ANIMATE_ARG_FADE_OUT_CURVE:
				animArgs.fadeOutCurve = (fa_curve_type_t)args[i+1].asInt32;
				break;
			case FS_ANIMATE_ARG_IK_MODE:
				animArgs.ikMode = (fa_ik_mode_t)args[i+1].asInt32;
				break;
			case FS_ANIMATE_ARG_LAYER:
				animArgs.layer = (fa_character_layer_t)args[i+1].asInt32;
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
	
	fa_character_schedule_action_simple(gameObj->animCharacter, animateSlot, &animArgs);
	
	ctx->waitSeconds = MAX(ctx->waitSeconds - animArgs.fadeOutSec, 0.0f);
	
	fs_variant_t result = {};
	return result;
};

fs_variant_t fs_native_wait_animate(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args)
{
	FUR_ASSERT(numArgs >= 2);
	const fc_string_hash_t animName = args[1].asStringHash;
	
	const fa_anim_clip_t* animClip = fg_resource_find_anim(&ctx->world->resources, animName);
	FUR_ASSERT(animClip);
	
	ctx->waitSeconds = animClip->duration;
	
	return fs_native_animate(ctx, numArgs, args);
}

fs_variant_t fs_native_equip_item(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args)
{
	FUR_ASSERT(numArgs >= 2);
	const fc_string_hash_t objectName = args[0].asStringHash;
	//const fc_string_hash_t itemName = args[1].asStringHash;
	
	// find game object
	fg_game_object_t* gameObj = fs_look_up_game_object(ctx, objectName);
	FUR_ASSERT(gameObj);
	
	gameObj->equipItemNow = true;
	
	fs_variant_t result = {};
	return result;
}

fs_variant_t fs_native_wait_seconds(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args)
{
	FUR_ASSERT(numArgs == 1);
	const float timeInSeconds = args[0].asFloat;
	
	ctx->waitSeconds = timeInSeconds;
	
	fs_variant_t result = {};
	return result;
}

fs_variant_t fs_native_get_variable(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args)
{
	FUR_ASSERT(numArgs == 2);
	const fc_string_hash_t objectName = args[0].asStringHash;
	const fc_string_hash_t varName = args[1].asStringHash;
	
	// find game object
	fg_game_object_t* gameObj = fs_look_up_game_object(ctx, objectName);
	FUR_ASSERT(gameObj);
	
	fs_variant_t result = {};
	
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

fs_variant_t fs_native_go(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args)
{
	FUR_ASSERT(numArgs == 1);
	const fc_string_hash_t goToState = args[0].asStringHash;
	
	ctx->nextState = goToState;
	
	fs_variant_t result = {};
	return result;
}

fs_variant_t fs_native_go_when(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args)
{
	FUR_ASSERT(numArgs == 2);
	const fc_string_hash_t goToState = args[0].asStringHash;
	const bool condition = args[1].asBool;
	
	if(condition)
	{
		ctx->nextState = goToState;
	}
	
	return args[1];
}

fs_variant_t fs_native_cmp_gt(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args)
{
	FUR_ASSERT(numArgs == 2);
	const int32_t a = args[0].asInt32;
	const int32_t b = args[1].asInt32;
	
	fs_variant_t result;
	result.asBool = a > b;
	return result;
}

fs_variant_t fs_native_cmp_eq(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args)
{
	FUR_ASSERT(numArgs == 2);
	const int32_t a = args[0].asInt32;
	const int32_t b = args[1].asInt32;
	
	fs_variant_t result;
	result.asBool = (a == b);
	return result;
}

fs_variant_t fs_native_camera_enable(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args)
{
	FUR_ASSERT(numArgs == 3);
	const fc_string_hash_t objectName = args[0].asStringHash;
	const fc_string_hash_t cameraType = args[1].asStringHash;
	const float fadeInSec = args[2].asFloat;
	
	fg_game_object_t* gameObj = fs_look_up_game_object(ctx, objectName);
	FUR_ASSERT(gameObj);
	
	if(cameraType == SID("follow"))
	{
		fg_camera_params_follow_t params = {};
		params.height = 1.2f;
		params.zoom = 1.5f;
		params.poleLength = 1.5f;
		params.fov = 70.0f;
		fg_camera_system_enable_camera_follow(ctx->sysCamera, &params, fadeInSec);
	}
	else if(cameraType == SID("follow-vista"))
	{
		fg_camera_params_follow_t params = {};
		params.height = 1.5f;
		params.zoom = 1.5f;
		params.poleLength = 1.0f;
		params.fov = 60.0f;
		fg_camera_system_enable_camera_follow(ctx->sysCamera, &params, fadeInSec);
	}
	
	fs_variant_t result = {};
	return result;
}

void fg_scripts_update(FurGameEngine* pEngine, float dt)
{
	// todo
}

bool g_drawDevMenu = false;
int32_t g_devMenuOption = 0;
bool g_devMenuOptionClick = false;

void fg_input_actions_update(FurGameEngine* pEngine, float dt)
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
	
	static float rightAnalogX = 0.0f;
	static float rightAnalogY = 0.0f;
	static float leftAnalogX = 0.0f;
	static float leftAnalogY = 0.0f;
	static float rightTrigger = 0.0f;
	static float leftTrigger = 0.0f;
	
	fi_input_event_t inputEvents[10];
	const uint32_t numEventsCollected = fi_get_input_events(pEngine->pInputManager, inputEvents, 10, 0);
	for(uint32_t i=0; i<numEventsCollected; ++i)
	{
		if(inputEvents[i].eventID == Gamepad_leftThumb)
		{
			leftThumbPressed = true;
		}
		else if(inputEvents[i].eventID == Gamepad_specialRight)
		{
			startPressed = true;
		}
		else if(inputEvents[i].eventID == Gamepad_dpadUp)
		{
			dpadUpPressed = true;
		}
		else if(inputEvents[i].eventID == Gamepad_dpadDown)
		{
			dpadDownPressed = true;
		}
		else if(inputEvents[i].eventID == Gamepad_faceButtonBottom)
		{
			actionPressed = true;
		}
		else if(inputEvents[i].eventID == Gamepad_faceButtonRight)
		{
			circleActionPressed = true;
		}
		else if(inputEvents[i].eventID == Gamepad_faceButtonTop)
		{
			triangleActionPressed = true;
		}
		else if(inputEvents[i].eventID == Gamepad_rightAnalogX)
		{
			rightAnalogX = fm_snap_near_zero(inputEvents[i].value, 0.05f);
		}
		else if(inputEvents[i].eventID == Gamepad_rightAnalogY)
		{
			rightAnalogY = fm_snap_near_zero(inputEvents[i].value, 0.05f);
		}
		else if(inputEvents[i].eventID == Gamepad_leftAnalogX)
		{
			leftAnalogX = -fm_snap_near_zero(inputEvents[i].value, 0.05f);
		}
		else if(inputEvents[i].eventID == Gamepad_leftAnalogY)
		{
			leftAnalogY = fm_snap_near_zero(inputEvents[i].value, 0.05f);
		}
		else if(inputEvents[i].eventID == Gamepad_rightTrigger)
		{
			rightTrigger = fm_snap_near_zero(inputEvents[i].value, 0.05f);
		}
		else if(inputEvents[i].eventID == Gamepad_leftTrigger)
		{
			leftTrigger = fm_snap_near_zero(inputEvents[i].value, 0.05f);
		}
	}
	
	bool leftThumbPressedThisFrame = false;
	bool startPressedThisFrame = false;
	
	if(wasLeftThumbPressed != leftThumbPressed)
	{
		wasLeftThumbPressed = leftThumbPressed;
		leftThumbPressedThisFrame = leftThumbPressed;
	}
	
	if(wasStartPressed != startPressed)
	{
		wasStartPressed = startPressed;
		startPressedThisFrame = startPressed;
	}
	
	if((wasLeftThumbPressed && startPressedThisFrame) || (wasStartPressed && leftThumbPressedThisFrame))
	{
		g_drawDevMenu = !g_drawDevMenu;
	}
	
	if(g_drawDevMenu)
	{
		if(wasDpadDownPressed != dpadDownPressed)
		{
			wasDpadDownPressed = dpadDownPressed;
			if(dpadDownPressed)
			{
				g_devMenuOption += 1;
			}
		}

		if(wasDpadUpPressed != dpadUpPressed)
		{
			wasDpadUpPressed = dpadUpPressed;
			if(dpadUpPressed)
			{
				g_devMenuOption -= 1;
			}
		}
		
		if(actionWasPressed != actionPressed)
		{
			g_devMenuOptionClick = actionPressed;
			actionWasPressed = actionPressed;
		}
	}
	else if(fc_profiler_is_draw_on())
	{
		if(actionWasPressed != actionPressed)
		{
			if(actionPressed)
			{
				fc_profiler_toggle_pause();
			}
			actionWasPressed = actionPressed;
		}
		else
		{
			pEngine->inActionPressed = false;
		}
		
		const float zoomDelta = leftTrigger - rightTrigger;
		const float panDelta = rightAnalogX;
		fc_profiler_zoom_and_pan_delta(zoomDelta, panDelta);
	}
	else // block all the actions when debug menu is enabled
	{
		pEngine->actionRotationLeftX = rightAnalogX;
		pEngine->actionRotationLeftY = rightAnalogY;
		pEngine->actionMoveX = leftAnalogX;
		pEngine->actionMoveY = leftAnalogY;
		pEngine->actionZoomIn = rightTrigger;
		pEngine->actionZoomOut = leftTrigger;
		
		if(actionWasPressed != actionPressed)
		{
			pEngine->inActionPressed = actionPressed;
			actionWasPressed = actionPressed;
		}
		else
		{
			pEngine->inActionPressed = false;
		}
		
		if(triangleActionWasPressed != triangleActionPressed)
		{
			pEngine->inputTriangleActionPressed = triangleActionPressed;
			triangleActionWasPressed = triangleActionPressed;
		}
		else
		{
			pEngine->inputTriangleActionPressed = false;
		}
		
		if(circleActionWasPressed != circleActionPressed)
		{
			pEngine->inputCircleActionPressed = circleActionPressed;
			circleActionWasPressed = circleActionPressed;
		}
		else
		{
			pEngine->inputCircleActionPressed = false;
		}
		
		if(xWasPressed != xPressed)
		{
			pEngine->inputXPressed = xPressed;
			xPressed = xPressed;
		}
		else
		{
			pEngine->inputXPressed = false;
		}
	}
}

void fc_dev_menu_reload_scripts(FurGameEngine* pEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
	fc_release_binary_buffer(&pEngine->zeldaStateScript, pAllocCallbacks);
	fc_load_binary_file_into_binary_buffer("../../../../../scripts/zelda.fs", &pEngine->zeldaStateScript, pAllocCallbacks);
}

void fc_dev_menu_show_player_anim_state(FurGameEngine* pEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
	pEngine->zeldaGameObject.showAnimStateDebug = !pEngine->zeldaGameObject.showAnimStateDebug;
}

void fc_dev_menu_show_profiler(FurGameEngine* pEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
	fc_profiler_toggle_draw();
}

void fc_dev_menu_slow_time(FurGameEngine* pEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
	pEngine->debugIsSlowTime = !pEngine->debugIsSlowTime;
}

void fc_dev_menu_show_fps(FurGameEngine* pEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
	pEngine->debugShowFPS = !pEngine->debugShowFPS;
}

void fc_dev_menu_show_memory_stats(FurGameEngine* pEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
	pEngine->debugShowMemoryStats = !pEngine->debugShowMemoryStats;
}

typedef struct fc_dev_menu_option_t
{
	const char* name;
	void (*func)(FurGameEngine* pEngine, fc_alloc_callbacks_t* pAllocCallbacks);
} fc_dev_menu_option_t;

void fc_draw_debug_menu(FurGameEngine* pEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
	const float color[4] = {0.8f, 0.8f, 0.8f, 1.0f};
	const float colorCursor[4] = {0.9f, 0.9f, 0.9f, 1.0f};
	const float colorLabel[4] = FUR_COLOR_CYAN;
	const float colorSelected[4] = FUR_COLOR_YELLOW;
	
	if(g_drawDevMenu)
	{
		const float x = -1000.0f;
		const float y = 440;
		const float lineHeight = 28.0f;
		const float ident = 28.0f;
		
		fc_dev_menu_option_t options[] = {
			{"quick-fps-mem", fc_dev_menu_show_fps},
			{"reload-scripts", fc_dev_menu_reload_scripts},
			{"slow-time", fc_dev_menu_slow_time},
			{"show-player-anim-state", fc_dev_menu_show_player_anim_state},
			{"profiler", fc_dev_menu_show_profiler},
			{"memory-stats", fc_dev_menu_show_memory_stats}
		};
		
		const uint32_t numOptions = FUR_ARRAY_SIZE(options);
		
		if(g_devMenuOption < 0)
			g_devMenuOption = numOptions-1;
		else if(g_devMenuOption >= numOptions)
			g_devMenuOption = 0;
		
		const float bgColor[4] = {0.2f, 0.2f, 0.2f, 0.8f};
		
		fc_dbg_rect(x - 40.0f, y + 40.0f, 450.0f, 110.0f + 28.0f * numOptions, bgColor);
		
		fc_dbg_text(x, y, "Dev Menu:", colorLabel);
		
		for(uint32_t i=0; i<numOptions; ++i)
		{
			const bool isSelected = (i == g_devMenuOption);
			
			fc_dbg_text(x + ident, y - lineHeight * (1+i), options[i].name, isSelected ? colorSelected : color);
			
			if(isSelected)
			{
				fc_dbg_text(x + ident - 18.0f, y - lineHeight * (1+i), ">", colorCursor);
			}
			
			// execute option
			if(isSelected && g_devMenuOptionClick)
			{
				if(options[i].func)
				{
					(*options[i].func)(pEngine, pAllocCallbacks);
				}
			}
		}
	}
}

void fg_gameplay_update(FurGameEngine* pEngine, float dt)
{
	uint64_t globalTime = (uint64_t)(pEngine->globalTime * 1000000);
	pEngine->animCharacterZelda.globalTime = globalTime;
	
	static uint32_t actionRandomizer = 0;
	static uint32_t actionRandomizer2 = 0;
	
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
		int32_t lambdaSlot = 512;
		for(int32_t i=0; i<128; ++i)
		{
			if(pEngine->scriptLambdas[i].isActive == false)
			{
				lambdaSlot = i;
				break;
			}
		}
		
		fs_script_lambda_t* lambda = &pEngine->scriptLambdas[lambdaSlot];
		lambda->isActive = true;
		lambda->numSkipOps = 0;
		lambda->waitSeconds = 0.0f;
		lambda->lambdaName = pEngine->zeldaGameObject.playerState;
		lambda->eventName = SID("start");
		lambda->selfGameObject = &pEngine->zeldaGameObject;
		lambda->scriptBlob = &pEngine->zeldaStateScript;
	}
	
	for(uint32_t i=0; i<128; ++i)
	{
		fs_script_lambda_t* lambda = &pEngine->scriptLambdas[i];
		
		if(lambda->isActive == false)
			continue;
		
		if(lambda->waitSeconds > 0.0f)
		{
			lambda->waitSeconds = MAX(lambda->waitSeconds - dt, 0.0f);
			
			if(lambda->waitSeconds > 0.0f)
			{
				continue;
			}
		}
		
		fs_script_ctx_t scriptCtx = {};
		scriptCtx.world = pEngine->pWorld;
		scriptCtx.gameObjectRegister = &pEngine->gameObjectRegister;
		scriptCtx.self = lambda->selfGameObject;
		scriptCtx.state = lambda->lambdaName;
		scriptCtx.stateEventToCall = lambda->eventName;
		scriptCtx.numSkipOps = lambda->numSkipOps;
		scriptCtx.sysCamera = pEngine->cameraSystem;
		fs_execute_script(lambda->scriptBlob, &scriptCtx);
		
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
				fa_action_args_t args = {};
				args.fadeInSec = 0.5f;
				args.layer = FA_CHAR_LAYER_PARTIAL;
				fa_character_schedule_none_action(&pEngine->animCharacterZelda, &args);
				
				pEngine->zeldaGameObject.playerWeaponEquipped = false;
			}
			else
			{
				fa_action_args_t args = {};
				args.fadeInSec = 0.5f;
				args.layer = FA_CHAR_LAYER_PARTIAL;
				fa_character_schedule_action_simple(&pEngine->animCharacterZelda, &pEngine->actionWeaponEquipped, &args);
				
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
			fa_action_args_t args = {};
			args.fadeInSec = 0.5f;
			args.layer = FA_CHAR_LAYER_PARTIAL;
			fa_character_schedule_none_action(&pEngine->animCharacterZelda, &args);
			
			pEngine->zeldaGameObject.playerWindProtecting = false;
		}
		else
		{
			fa_action_args_t args = {};
			args.fadeInSec = 0.5f;
			args.layer = FA_CHAR_LAYER_PARTIAL;
			fa_character_schedule_action_simple(&pEngine->animCharacterZelda, &pEngine->actionWindProtect, &args);
			
			pEngine->zeldaGameObject.playerWindProtecting = true;
		}
	}
	
	// is grounnded raycast check
	{
		bool isGrounded = false;
		
		fm_vec4 pos = pEngine->zeldaGameObject.worldTransform.pos;
		pos.z += 0.1f;
		const fm_vec4 dir = {0.0f, 0.0f, -1.0f, 0.0f};
		const float distance = 0.12f;
		
		fp_physics_raycast_hit_t hit = {};
		const bool isHit = fp_physics_raycast(pEngine->pPhysics, &pos, &dir, distance, &hit);
		if(isHit)
		{
			isGrounded = true;
			const float extent[3] = {0.02f, 0.02f, 0.02f};
			const float color[4] = FUR_COLOR_RED;
			fc_dbg_box_wire(&hit.pos.x, extent, color);
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

void fg_animation_update(FurGameEngine* pEngine, float dt)
{
	// set last known world position for character
	fm_xform playerLocator;
	fp_physics_player_info_t playerPhysics;
	playerPhysics.locator = &playerLocator;
	fp_physics_get_player_info(pEngine->pPhysics, &playerPhysics);
	
	pEngine->animCharacterZelda.animInfo.worldPos.x = playerLocator.pos.x;
	pEngine->animCharacterZelda.animInfo.worldPos.y = playerLocator.pos.y;
	pEngine->animCharacterZelda.animInfo.worldPos.z = playerLocator.pos.z;
	
	fc_mem_arena_alloc_t arenaAlloc = fc_mem_arena_make(pEngine->scratchpadBuffer, pEngine->scratchpadBufferSize);
	
	// animation states update
	fa_character_animate_ctx_t animateCtx = {};
	animateCtx.dt = dt;
	animateCtx.arenaAlloc = &arenaAlloc;
	animateCtx.showDebug = pEngine->zeldaGameObject.showAnimStateDebug;
	
	fa_character_animate(&pEngine->animCharacterZelda, &animateCtx);
	
	// skinning
	const fm_xform* poseMS = pEngine->animCharacterZelda.poseMS;
	const uint32_t numSkinMatrices = pEngine->pRig->numBones;
	for(uint32_t i=0; i<numSkinMatrices; ++i)
	{
		fm_xform_to_mat4(&poseMS[i], &pEngine->skinMatrices[i]);
	}
}

void fc_dbg_mat4(const fm_mat4* m)
{
	const float pos[3] = {m->w.x, m->w.y, m->w.z};
	const float scale = 0.1f;
	const float axisX[3] = {pos[0] + m->x.x * scale, pos[1] + m->x.y * scale, pos[2] + m->x.z * scale};
	const float axisY[3] = {pos[0] + m->y.x * scale, pos[1] + m->y.y * scale, pos[2] + m->y.z * scale};
	const float axisZ[3] = {pos[0] + m->z.x * scale, pos[1] + m->z.y * scale, pos[2] + m->z.z * scale};
	
	const float red[4] = FUR_COLOR_RED;
	const float green[4] = FUR_COLOR_GREEN;
	const float blue[4] = FUR_COLOR_BLUE;
	
	fc_dbg_line(pos, axisX, red);
	fc_dbg_line(pos, axisY, green);
	fc_dbg_line(pos, axisZ, blue);
}

void fc_dbg_stats_for_mem_to_text(fc_memory_scope_t scope, char* txt, const char* displayName)
{
	fc_mem_stats_t stats = fc_memory_stats_for_scope(scope);
	
	const float numMBs = ((float)stats.numBytesUsed) / (1024.0f * 1024.0f);
	const float numCapacityMBs = ((float)stats.numBytesCapacity) / (1024.0f * 1024.0f);
	
	const char* name = fc_memory_get_scope_debug_name(scope);
	
	sprintf(txt, "%s: %1.2f / %1.2f MBs (%u allocs)", displayName ? displayName : name, numMBs, numCapacityMBs, stats.numAllocs);
}

void furMainEngineGameUpdate(FurGameEngine* pEngine, float dt, fc_alloc_callbacks_t* pAllocCallbacks)
{
	// test BVH debug draw
	{
		fp_bvh_debug_draw(&pEngine->testBVH);
	}
	
	// show debug FPS
	if(pEngine->debugShowFPS)
	{
		{
			const float fps = 1.0f / dt;
			const float ms = dt * 1000.0f;
			
			char txt[50];
			sprintf(txt, "CPU: %1.1f fps (%1.1f ms)", fps, ms);
			
			const float green[4] = FUR_COLOR_GREEN;
			const float yellow[4] = FUR_COLOR_YELLOW;
			const float red[4] = FUR_COLOR_RED;
			fc_dbg_text(-1050, 628, txt, ms < 16 ? green : ms < 33 ? yellow : red);
		}
		{
			fc_mem_stats_t stats = fc_memory_stats();
			
			const float numMBs = ((float)stats.numBytesUsed) / (1024.0f * 1024.0f);
			const float numCapacityMBs = ((float)stats.numBytesCapacity) / (1024.0f * 1024.0f);
			
			char txt[50];
			sprintf(txt, "MEM: %1.2f / %1.2f MBs (%u allocs)", numMBs, numCapacityMBs, stats.numAllocs);
			
			const float green[4] = FUR_COLOR_GREEN;
			const float yellow[4] = FUR_COLOR_YELLOW;
			const float red[4] = FUR_COLOR_RED;
			fc_dbg_text(-1050, 600, txt, numMBs < numCapacityMBs * 0.95f ? green : numMBs < numCapacityMBs ? yellow : red);
		}
	}
	
	// show memory statistics for each memory scope
	if(pEngine->debugShowMemoryStats)
	{
		const float white[4] = FUR_COLOR_WHITE;
		int32_t textLineCounter = 0;
		
		fc_dbg_text(-1050, 500 + 24, "-[ Retail Memory ]-----------------------------------------------", white);
		
		char txt[128] = {};
		
		fc_dbg_stats_for_mem_to_text(FC_MEMORY_SCOPE_SYSTEM, txt, "OS Memory");
		fc_dbg_text(-1050, 500 - textLineCounter*24, txt, white);
		textLineCounter++;
		
		fc_dbg_stats_for_mem_to_text(FC_MEMORY_SCOPE_GLOBAL, txt, "Code And Static Data");
		fc_dbg_text(-1050, 500 - textLineCounter*24, txt, white);
		textLineCounter++;
		
		textLineCounter++;
		fc_dbg_text(-1050, 500 - textLineCounter*24, "-[ Debug Memory ]------------------------------------------------", white);
		textLineCounter++;
		
		fc_dbg_stats_for_mem_to_text(FC_MEMORY_SCOPE_DEBUG, txt, "Debug Memory");
		fc_dbg_text(-1050, 500 - textLineCounter*24, txt, white);
		textLineCounter++;
	}
	
	// slow-time debug mode - this needs to be after debugShowFPS
	if(pEngine->debugIsSlowTime)
	{
		const float color[4] = FUR_COLOR_RED;
		fc_dbg_text(920, 600, "slow-time ON", color);
		dt *= 0.3f;
	}
	
	pEngine->globalTime += dt;
	
	// input
	FUR_PROFILE("actions-update")
	{
		fi_update_input_manager(pEngine->pInputManager, pEngine->globalTime);
		fg_input_actions_update(pEngine, dt);
	}
	
	// debug/dev menu
	fc_draw_debug_menu(pEngine, pAllocCallbacks);
	
	// game
	FUR_PROFILE("gameplay-update")
	{
		fg_scripts_update(pEngine, dt);
		fg_gameplay_update(pEngine, dt);
	}
	
	// test set look-at point
	{
		// get zelda position
		fm_xform playerLocator;
		fp_physics_player_info_t playerPhysics;
		playerPhysics.locator = &playerLocator;
		fp_physics_get_player_info(pEngine->pPhysics, &playerPhysics);
		
		static float time = 0.0f;
		time += dt;
		
		// convert world space look-at point to model space of player
		fm_vec4 lookAtPoint = {2.0f * sinf(time), 2.0f * cosf(time), 1.0f + 1.0f * sinf(time * 0.6f), 1.0f};	// in world space
		
		float color[4] = FUR_COLOR_MAGENTA;
		float extent[] = {0.1f, 0.1f, 0.1f};
		fc_dbg_box_wire(&lookAtPoint.x, extent, color);
		
		extent[0] = 0.07f;
		extent[1] = 0.07f;
		extent[2] = 0.07f;
		fc_dbg_box_wire(&lookAtPoint.x, extent, color);
		
		extent[0] = 0.04f;
		extent[1] = 0.04f;
		extent[2] = 0.04f;
		fc_dbg_box_wire(&lookAtPoint.x, extent, color);
		
		const float distanceToLookAtPoint = fm_vec4_distance(&lookAtPoint, &playerLocator.pos);
		
		if(pEngine->animCharacterZelda.animInfo.useLookAt)
		{
			pEngine->animCharacterZelda.animInfo.useLookAt = distanceToLookAtPoint < 10.0f;
		}
		else
		{
			pEngine->animCharacterZelda.animInfo.useLookAt = distanceToLookAtPoint < 6.0f;
		}
		
		pEngine->animCharacterZelda.animInfo.lookAtPoint.x = lookAtPoint.x;
		pEngine->animCharacterZelda.animInfo.lookAtPoint.y = lookAtPoint.y;
		pEngine->animCharacterZelda.animInfo.lookAtPoint.z = lookAtPoint.z;
	}
	
	// animation
	FUR_PROFILE("animation-update")
	{
		fg_animation_update(pEngine, dt);
	}
	
	{
		const fm_vec4 zeros = {0.0f, 0.0f, 0.0f, 0.0f};
		const fm_vec4 axisX = {1.0f, 0.0f, 0.0f, 0.0f};
		const fm_vec4 axisY = {0.0f, 1.0f, 0.0f, 0.0f};
		const fm_vec4 axisZ = {0.0f, 0.0f, 1.0f, 0.0f};
		
		const float red[4] = FUR_COLOR_RED;
		const float green[4] = FUR_COLOR_GREEN;
		const float blue[4] = FUR_COLOR_BLUE;
		
		fc_dbg_line(&zeros.x, &axisX.x, red);
		fc_dbg_line(&zeros.x, &axisY.x, green);
		fc_dbg_line(&zeros.x, &axisZ.x, blue);
	}
	
	// physics
	FUR_PROFILE("physics-update")
	{
		fp_physics_update_ctx_t physicsCtx = {};
		physicsCtx.dt = dt;
		
		fm_vec4 playerDisplacement = pEngine->zeldaGameObject.velocity;
		
		// apply root motion from anim info to physics
		if(pEngine->zeldaGameObject.isJump)
		{
			pEngine->zeldaGameObject.velocity.z = 4.0f;
		}
		else if(pEngine->zeldaGameObject.isGrounded)
		{
			pEngine->zeldaGameObject.velocity.x = pEngine->animCharacterZelda.animInfo.rootMotionDelta.x / dt;
			pEngine->zeldaGameObject.velocity.y = pEngine->animCharacterZelda.animInfo.rootMotionDelta.y / dt;
			pEngine->zeldaGameObject.velocity.z = 0.0f;
		}
		else
		{
			const float speed = fm_vec4_mag(&pEngine->zeldaGameObject.velocity);
			if(speed > 0.0f)
			{
				pEngine->zeldaGameObject.velocity.x += pEngine->animCharacterZelda.animInfo.desiredMove.x * 1.2f;
				pEngine->zeldaGameObject.velocity.y += pEngine->animCharacterZelda.animInfo.desiredMove.y * 1.2f;
				fm_vec4_normalize(&pEngine->zeldaGameObject.velocity);
				fm_vec4_mulf(&pEngine->zeldaGameObject.velocity, speed, &pEngine->zeldaGameObject.velocity);
			}
		}
		
		// apply gravity
		pEngine->zeldaGameObject.velocity.z += -9.81f * dt;
		
		fm_vec4_mulf(&pEngine->zeldaGameObject.velocity, dt, &playerDisplacement);
		physicsCtx.playerDisplacement = &playerDisplacement;
		fp_physics_update(pEngine->pPhysics, &physicsCtx);
		
		fp_physics_player_info_t playerPhysics;
		playerPhysics.locator = &pEngine->zeldaGameObject.worldTransform;
		fp_physics_get_player_info(pEngine->pPhysics, &playerPhysics);

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
			fm_mat4_rot_z(pEngine->animCharacterZelda.animInfo.currentYaw, &playerMat);
			
			fm_vec4 invPlayerMove;
			fm_mat4_transform(&playerMat, &playerMove, &invPlayerMove);
			
			pEngine->windVelocity.x -= invPlayerMove.x;
			pEngine->windVelocity.y -= invPlayerMove.y;
			pEngine->windVelocity.z -= invPlayerMove.z;
		}
		
		// simulate hair dangles
		{
			fa_dangle_sim_ctx simCtx {};
			simCtx.dt = dt;
			
			fm_vec4 spherePos = pEngine->skinMatrices[pEngine->zeldaHeadIdx].w;
			const float sphereRadius = 0.08f;
			pEngine->zeldaDangleHairLeft.spherePos = &spherePos;
			pEngine->zeldaDangleHairLeft.sphereRadius = sphereRadius;
			pEngine->zeldaDangleHairRight.spherePos = &spherePos;
			pEngine->zeldaDangleHairRight.sphereRadius = sphereRadius;
			
			pEngine->zeldaDangleHairLeft.x0[0] = pEngine->skinMatrices[pEngine->zeldaDangleHairLeftIdx1].w;
			pEngine->zeldaDangleHairRight.x0[0] = pEngine->skinMatrices[pEngine->zeldaDangleHairRightIdx1].w;
			
			// add wind velocity
			for(uint32_t i=0; i<3; ++i)
			{
				pEngine->zeldaDangleHairLeft.v[i].x += pEngine->windVelocity.x * dt;
				pEngine->zeldaDangleHairLeft.v[i].y += pEngine->windVelocity.y * dt;
				pEngine->zeldaDangleHairLeft.v[i].z += pEngine->windVelocity.z * dt;
				
				pEngine->zeldaDangleHairRight.v[i].x += pEngine->windVelocity.x * dt;
				pEngine->zeldaDangleHairRight.v[i].y += pEngine->windVelocity.y * dt;
				pEngine->zeldaDangleHairRight.v[i].z += pEngine->windVelocity.z * dt;
			}
			
			fa_dangle_simulate(&simCtx, &pEngine->zeldaDangleHairLeft);
			fa_dangle_simulate(&simCtx, &pEngine->zeldaDangleHairRight);
			
			fm_mat4 m[3] = {};
			
			m[0] = pEngine->skinMatrices[pEngine->zeldaDangleHairLeftIdx1];
			fa_dangle_to_matrices_y_down(&pEngine->zeldaDangleHairLeft, &m[0], m);
			pEngine->skinMatrices[pEngine->zeldaDangleHairLeftIdx1] = m[0];
			pEngine->skinMatrices[pEngine->zeldaDangleHairLeftIdx2] = m[1];
			
			m[0] = pEngine->skinMatrices[pEngine->zeldaDangleHairRightIdx1];
			fa_dangle_to_matrices_y_down(&pEngine->zeldaDangleHairRight, &m[0], m);
			pEngine->skinMatrices[pEngine->zeldaDangleHairRightIdx1] = m[0];
			pEngine->skinMatrices[pEngine->zeldaDangleHairRightIdx2] = m[1];
		}
		
		// simulate cape dangles
		{
			fa_dangle_sim_ctx simCtx {};
			simCtx.dt = dt;
			
			fm_vec4 spherePos = pEngine->skinMatrices[pEngine->zeldaSpineIdx].w;
			spherePos.z -= 0.25f;
			spherePos.x = 0.6f;
			const float sphereRadius = 0.8f;
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
			for(uint32_t i=0; i<4; ++i)
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
			
			fa_dangle_simulate(&simCtx, &pEngine->zeldaCapeL);
			fa_dangle_simulate(&simCtx, &pEngine->zeldaCapeC);
			fa_dangle_simulate(&simCtx, &pEngine->zeldaCapeR);
			
			fm_mat4 m[4] = {};
			
			m[0] = pEngine->skinMatrices[pEngine->zeldaCapeIdxL[0]];
			fa_dangle_to_matrices_y_down(&pEngine->zeldaCapeL, &m[0], m);
			
			for(uint32_t i=0; i<4; ++i)
			{
				pEngine->skinMatrices[pEngine->zeldaCapeIdxL[i]] = m[i];
			}
			
			m[0] = pEngine->skinMatrices[pEngine->zeldaCapeIdxC[0]];
			fa_dangle_to_matrices_y_down(&pEngine->zeldaCapeC, &m[0], m);
			
			for(uint32_t i=0; i<4; ++i)
			{
				pEngine->skinMatrices[pEngine->zeldaCapeIdxC[i]] = m[i];
			}
			
			m[0] = pEngine->skinMatrices[pEngine->zeldaCapeIdxR[0]];
			fa_dangle_to_matrices_y_down(&pEngine->zeldaCapeR, &m[0], m);
			
			for(uint32_t i=0; i<4; ++i)
			{
				pEngine->skinMatrices[pEngine->zeldaCapeIdxR[i]] = m[i];
			}
		}
	}
	
	// rendering
	FUR_PROFILE("render-update")
	{
		// get zelda position
		fm_xform playerLocator = pEngine->zeldaGameObject.worldTransform;
		
		fm_mat4 zeldaMat;
		fm_mat4_rot_z(pEngine->animCharacterZelda.animInfo.currentYaw, &zeldaMat);
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
			fg_camera_system_update_ctx cameraCtx = {};
			cameraCtx.dt = dt;
			cameraCtx.rotationYaw = pEngine->actionRotationLeftX;
			cameraCtx.rotationPitch = pEngine->actionRotationLeftY;
			cameraCtx.zoom = pEngine->actionZoomOut - pEngine->actionZoomIn;
			
			// adjust camera by player position
			fg_camera_adjust_by_player_movement(pEngine->cameraSystem, &zeldaMat);
			
			fg_camera_system_update(pEngine->cameraSystem, &cameraCtx);
		}
		
		// player movement
		const float maxSpeed = 5.0f;
		
		fm_vec4 dirForward = {};
		fm_vec4 dirLeft = {};
		fg_camera_get_directions(pEngine->cameraSystem, &dirForward, &dirLeft);
		
		fm_vec4 playerMoveForward;
		fm_vec4_mulf(&dirForward, maxSpeed * pEngine->actionMoveY, &playerMoveForward);
		fm_vec4 playerMoveLeft;
		fm_vec4_mulf(&dirLeft, maxSpeed * pEngine->actionMoveX, &playerMoveLeft);
		fm_vec4 playerMove;
		fm_vec4_add(&playerMoveForward, &playerMoveLeft, &playerMove);
		
		pEngine->animCharacterZelda.animInfo.animToLogicMotionRotationAlpha = 1.0f;
		pEngine->animCharacterZelda.animInfo.animToLogicMotionTranslationAlpha = 0.0f;
		pEngine->animCharacterZelda.animInfo.desiredMove.x = playerMove.x * dt;
		pEngine->animCharacterZelda.animInfo.desiredMove.y = playerMove.y * dt;
		
		pEngine->zeldaGameObject.logicMove.x = playerMove.x * dt;
		pEngine->zeldaGameObject.logicMove.y = playerMove.y * dt;
		pEngine->zeldaGameObject.logicMove.z = 0.0f;
		pEngine->zeldaGameObject.logicMove.w = 0.0f;
		
		pEngine->playerMove = playerMove;
		
		fm_mat4 cameraMatrix;
		fg_camera_view_matrix(pEngine->cameraSystem, &cameraMatrix);
		
		if(!pEngine->zeldaGameObject.playerWeaponEquipped)
		{
			fm_mat4_identity(&slotMS);
			slotMS.w.x = 4.0f;
			slotMS.w.z = -4.0f;
		}
		
		// acquire this frame's PVS (Potentially Visible Set) to fill it with data
		fr_pvs_t* framePVS = fr_acquire_free_pvs(pEngine->pRenderer, &cameraMatrix, fg_camera_get_fov(pEngine->cameraSystem));
		
		fr_pvs_add(framePVS, pEngine->swordMesh, &slotMS);
		
		fm_mat4 staticMeshesLocator;
		fm_mat4_identity(&staticMeshesLocator);
		
		// chest
		staticMeshesLocator.w.x = 2.0f;
		staticMeshesLocator.w.y = 2.0f;
		fr_pvs_add(framePVS, pEngine->chestMesh, &staticMeshesLocator);
		
		// block mesh
		staticMeshesLocator.w.x = pEngine->blockPositions[0].pos.x;
		staticMeshesLocator.w.y = pEngine->blockPositions[0].pos.y;
		staticMeshesLocator.w.z = pEngine->blockPositions[0].pos.z;
		fr_pvs_add(framePVS, pEngine->blockMesh, &staticMeshesLocator);
		
		staticMeshesLocator.w.x = pEngine->blockPositions[1].pos.x;
		staticMeshesLocator.w.y = pEngine->blockPositions[1].pos.y;
		staticMeshesLocator.w.z = pEngine->blockPositions[1].pos.z;
		fr_pvs_add(framePVS, pEngine->blockMesh, &staticMeshesLocator);
		
		staticMeshesLocator.w.z = 0.0f;
		
		// rocks
		staticMeshesLocator.w.x = 14.0f;
		staticMeshesLocator.w.y = -15.0f;
		fr_pvs_add(framePVS, pEngine->rockMeshes[0], &staticMeshesLocator);
		
		staticMeshesLocator.w.x = 15.0f;
		staticMeshesLocator.w.y = 13.0f;
		fr_pvs_add(framePVS, pEngine->rockMeshes[1], &staticMeshesLocator);
		
		staticMeshesLocator.w.x = -11.0f;
		staticMeshesLocator.w.y = 7.0f;
		fr_pvs_add(framePVS, pEngine->rockMeshes[2], &staticMeshesLocator);
		
		staticMeshesLocator.w.x = -13.0f;
		staticMeshesLocator.w.y = -4.0f;
		fr_pvs_add(framePVS, pEngine->rockMeshes[3], &staticMeshesLocator);
		
		staticMeshesLocator.w.x = 2.0f;
		staticMeshesLocator.w.y = -14.0f;
		fr_pvs_add(framePVS, pEngine->rockMeshes[4], &staticMeshesLocator);
		
		staticMeshesLocator.w.x = 0.0f;
		staticMeshesLocator.w.y = 0.0f;
		fr_pvs_add_and_skin(framePVS, pEngine->zeldaMesh, &zeldaMat, pEngine->skinMatrices, pEngine->pRig->numBones);
		
		// draw frame
		fr_draw_frame_context_t renderCtx = {};
		renderCtx.pvs = framePVS;
		fr_draw_frame(pEngine->pRenderer, &renderCtx, pAllocCallbacks);
	}
}

void furMainEngineLoop(FurGameEngine* pEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
	pEngine->prevTimePoint = std::chrono::system_clock::now();
	
	while(fr_update_app(pEngine->pApp))
	{
		std::chrono::system_clock::time_point timeNow = std::chrono::system_clock::now();
		std::chrono::duration<float> dtOrig = timeNow - pEngine->prevTimePoint;
		pEngine->prevTimePoint = timeNow;
		
		const float dt = dtOrig.count();
		
		fc_profiler_start_frame();
		
		FUR_PROFILE("frame")
		{
			furMainEngineGameUpdate(pEngine, dt, pAllocCallbacks);
		}
		
		fc_profiler_end_frame();
	}
	
	fr_wait_for_device(pEngine->pRenderer);
}

bool furMainEngineTerminate(FurGameEngine* pEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
	// release text BVH
	fp_bvh_release(&pEngine->testBVH, pAllocCallbacks);
	
	// release meshes
	fr_release_proxy(pEngine->pRenderer, pEngine->zeldaMesh, pAllocCallbacks);
	fr_release_proxy(pEngine->pRenderer, pEngine->swordMesh, pAllocCallbacks);
	fr_release_proxy(pEngine->pRenderer, pEngine->chestMesh, pAllocCallbacks);
	fr_release_proxy(pEngine->pRenderer, pEngine->blockMesh, pAllocCallbacks);
	
	// load rock meshes
	for(uint32_t i=0; i<5; ++i)
	{
		fr_release_proxy(pEngine->pRenderer, pEngine->rockMeshes[i], pAllocCallbacks);
	}
	
	// release scripts
	fc_release_binary_buffer(&pEngine->zeldaStateScript, pAllocCallbacks);
	
	fa_dangle_release(&pEngine->dangle, pAllocCallbacks);
	fa_dangle_release(&pEngine->zeldaDangleHairLeft, pAllocCallbacks);
	fa_dangle_release(&pEngine->zeldaDangleHairRight, pAllocCallbacks);
	
	fa_dangle_release(&pEngine->zeldaCapeL, pAllocCallbacks);
	fa_dangle_release(&pEngine->zeldaCapeC, pAllocCallbacks);
	fa_dangle_release(&pEngine->zeldaCapeR, pAllocCallbacks);
	
	fa_character_release(&pEngine->animCharacterZelda, pAllocCallbacks);
	
	fg_world_release(pEngine->pWorld, pAllocCallbacks);
	FUR_FREE(pEngine->pWorld, pAllocCallbacks);
	
	FUR_FREE(pEngine->gameObjectRegister.objects, pAllocCallbacks);
	FUR_FREE(pEngine->gameObjectRegister.ids, pAllocCallbacks);
	
	FUR_FREE(pEngine->scratchpadBuffer, pAllocCallbacks);
	fa_rig_release(pEngine->pRig, pAllocCallbacks);
	
	fg_camera_system_release(pEngine->cameraSystem, pAllocCallbacks);
	
	fp_physics_release(pEngine->pPhysics, pAllocCallbacks);
	fr_release_renderer(pEngine->pRenderer, pAllocCallbacks);
	
	fi_input_manager_release(pEngine->pInputManager, pAllocCallbacks);
	
	fc_string_hash_register_release(pAllocCallbacks);
	
	fc_profiler_release(pAllocCallbacks);
	
	// release all memory before this call, otherwise it might be treated as memory leak
	fr_release_app(pEngine->pApp, pAllocCallbacks);
	
	free(pEngine);	// rest of the deallocations should happen through allocators
	
	return true;
}

int main()
{
	FurGameEngineDesc desc = {};
	
	desc.m_mainApp.m_width = 1600;
	desc.m_mainApp.m_height = 900;
	desc.m_mainApp.m_title = "Furball Cat Game Engine";
	
	FurGameEngine* pEngine = NULL;
	
	fc_alloc_callbacks_t* pAllocCallbacks = NULL;	// todo: if NULL, then uses default alloc callbacks
	
	// initialize most basic engine components
	bool initResult = furMainEngineInit(desc, &pEngine, pAllocCallbacks);
	if(!initResult)
	{
		printf("Engine initialization error. Last known error: %s.\n", FurGetLastError());
		return initResult;
	}
	
	// main engine loop
	furMainEngineLoop(pEngine, pAllocCallbacks);
	
	// terminate most basic engine components
	bool result = furMainEngineTerminate(pEngine, pAllocCallbacks);
	if(!result)
	{
		return 1;
	}
	
	return 0;
}
