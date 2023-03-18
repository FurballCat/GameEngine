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

typedef union fs_variant_t
{
	fc_string_hash_t asStringHash;
	i32 asInt32;
	bool asBool;
	f32 asFloat;
} fs_variant_t;

// ***** scripts core ***** //

typedef struct fg_game_object_t fg_game_object_t;

typedef struct fg_game_object_register_t
{
	fg_game_object_t** objects;
	fc_string_hash_t* ids;
	u32 numObjects;	// also numIds
	u32 capacity;
	
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
	f32 waitSeconds;
	u32 numSkipOps;
	
	fg_game_object_t* self;
	fg_game_object_register_t* gameObjectRegister;
	fg_world_t* world;
	
	// systems
	fg_camera_system_t* sysCamera;
} fs_script_ctx_t;

// todo: move it somewhere else
fs_variant_t fs_native_animate(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args);
fs_variant_t fs_native_wait_animate(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args);
fs_variant_t fs_native_equip_item(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args);
fs_variant_t fs_native_wait_seconds(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args);
fs_variant_t fs_native_get_variable(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args);
fs_variant_t fs_native_go(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args);
fs_variant_t fs_native_go_when(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args);
fs_variant_t fs_native_cmp_gt(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args);
fs_variant_t fs_native_cmp_eq(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args);

// camera script functions
fs_variant_t fs_native_camera_enable(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args);

typedef fs_variant_t (*fs_script_navitve_func_t)(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args);

typedef struct fs_native_func_entry_t
{
	fc_string_hash_t name;
	fs_script_navitve_func_t func;
	u32 numArgs;
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
	u32 numArgs;
} fs_script_op_t;

typedef struct fs_script_data_t
{
	fs_script_op_t* ops;	// sequence of operations
	fs_variant_t* allArgs; // owning memory to all args for all calls
	
	u32 numOps;
	u32 numAllArgs;
} fs_script_data_t;

typedef struct fs_script_state_t
{
	u32 idxOp;
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
	u32 flags;
	u32 numArgs;
} fs_script_op_header;

typedef struct fs_segment_header_t
{
	u8 segmentId;		// type of segment data (what to expect next)
	u8 padding;
	u16 dataSize; 		// segment size in bytes
	fc_string_hash_t name;	// unique name in segment scope (to know what to look for)
} fs_lambda_header_t;

typedef struct fs_script_execution_ctx_t
{
	fc_binary_buffer_stream_t scriptBufferStream;
	fs_script_ctx_t* scriptCtx;
	u32 numOpsExecuted;
	bool endOflambda;
} fs_script_execution_ctx_t;

fs_variant_t fs_execute_script_step(fs_script_execution_ctx_t* ctx)
{
	fc_binary_buffer_stream_t* stream = &ctx->scriptBufferStream;
	
	// peek what's next in buffer, but do not advance the stream
	u8 op_pre_flag = FS_SEG_ID_UNKNOWN;
	fc_peek_binary_buffer(stream, sizeof(u8), &op_pre_flag);
	
	fs_variant_t result = {};
	
	if(op_pre_flag == FS_SEG_ID_C_FUNC_CALL)
	{
		// advance the stream to skip the peeked part
		u32 bytesRead = fc_read_binary_buffer(stream, sizeof(u8), &op_pre_flag);
		FUR_ASSERT(bytesRead);
		
		ctx->numOpsExecuted += 1;
		
		// read operation header
		fs_script_op_header opHeader = {};
		bytesRead = fc_read_binary_buffer(stream, sizeof(fs_script_op_header), &opHeader);
		FUR_ASSERT(bytesRead);
		
		// read arguments
		FUR_ASSERT(opHeader.numArgs < 20);
		fs_variant_t args[20];
		
		for(u32 i=0; i<opHeader.numArgs; ++i)
		{
			args[i] = fs_execute_script_step(ctx);
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
		u32 bytesRead = fc_read_binary_buffer(stream, sizeof(u8), &op_pre_flag);
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
	u32 bytesRead = fc_read_binary_buffer(&ctx->scriptBufferStream, sizeof(fs_segment_header_t), header);
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
		u32 bytesRead = fc_read_binary_buffer(&ctx->scriptBufferStream, sizeof(fs_segment_header_t), &header);
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
	f32 waitSeconds;
	u32 numSkipOps;
	bool isActive;
	fc_string_hash_t lambdaName;	// or state name
	fc_string_hash_t eventName;
	fg_game_object_t* selfGameObject;
	const fc_binary_buffer_t* scriptBlob;
} fs_script_lambda_t;

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

typedef struct fg_animate_action_slots_t
{
	fa_action_animate_t slot[32];
} fg_animate_action_slots_t;

fa_action_animate_t* fg_animate_action_slots_get_free(fg_animate_action_slots_t* slots)
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

const fa_anim_clip_t* fg_load_anim(fg_resource_register_t* reg, fc_depot_t* depot, const char* name,
								   const fa_rig_t* rig, fc_alloc_callbacks_t* pAllocCallbacks)
{
	fa_anim_clip_t* animClip = NULL;
	
	{
		const char* directory = "data/anim/";
		const char* extension = ".fbx";
		const char* engineExtension = ".anim";
		const u64 dirLength = strlen(directory);
		const u64 nameLength = strlen(name);
		
		char pathEngine[256] = {};
		fc_path_concat(pathEngine, "", "data/anim/", name, ".anim");

		fc_file_path_t file_path = fc_file_path_create(depot, pathEngine);
		fc_file_t* file = fc_file_open(depot, file_path, "rb");
		
		// try loading or saving engine file
		fc_serializer_t serializer = {};
		serializer.file = file;
		serializer.isWriting = false;
			
		if(!serializer.isWriting)
		{
			animClip = (fa_anim_clip_t*)FUR_ALLOC_AND_ZERO(sizeof(fa_anim_clip_t), 0, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
		}
			
		fa_anim_clip_serialize(&serializer, animClip, pAllocCallbacks);

		fc_file_close(file);
		
		// register animation
		const i32 idxAnim = reg->numAnimations;
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
	
	fa_character_t* animCharacter;
	const fr_proxy_t* mesh;
	
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
		
		fa_character_desc_t desc = {};
		desc.globalTime = ctx->globalTime;
		desc.rig = fg_resource_find_rig(ctx->resources, SID("zelda-rig"));
		zelda->animCharacter = fa_anim_sys_create_character(&desc, ctx->stackAlloc);
		fa_anim_sys_add_character(ctx->systems->animation, zelda->animCharacter);
		
		zelda->mesh = fg_resource_find_mesh(ctx->resources, SID("zelda-mesh"));
		
		const fc_string_hash_t scriptName = fg_spawn_info_get_string_hash(ctx->info, SID("state-script"), SID("none"));
		zelda->script.scriptBlob = fg_resource_find_script(ctx->resources, scriptName);
		zelda->script.lambdaName = zelda->playerState;
		zelda->script.isActive = true;
		zelda->script.eventName = SID("start");
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
	fc_depot_t* depot;
	struct fr_renderer_t* pRenderer;
	fp_physics_t* pPhysics;
	fa_anim_sys_t* animSystem;
	
	std::chrono::system_clock::time_point prevTimePoint;
	f32 globalTime;
	
	fg_world_t* pWorld;
	
	fi_input_manager_t* pInputManager;

	// resource path hashes
	fc_file_path_t zeldaScriptPath;
	
	// animation
	fa_rig_t* pRig;
	const fa_anim_clip_t* pAnimClipWindProtect;
	const fa_anim_clip_t* pAnimClipHoldSword;
	
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
	fg_camera_system_t* cameraSystem;
	
	// gameplay animation states
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
	u32 scratchpadBufferSize;
	
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
	u32 zeldaDangleHairLeftIdx1;
	u32 zeldaDangleHairLeftIdx2;
	u32 zeldaDangleHairRightIdx1;
	u32 zeldaDangleHairRightIdx2;
	u32 zeldaHeadIdx;
	u32 zeldaHandRightIdx;
	
	// cape dangles
	fa_dangle zeldaCapeL;
	u32 zeldaCapeIdxL[4];
	
	fa_dangle zeldaCapeC;
	u32 zeldaCapeIdxC[4];
	
	fa_dangle zeldaCapeR;
	u32 zeldaCapeIdxR[4];
	
	u32 zeldaSpineIdx;
	
	// wind
	fm_vec3 windVelocity;
	
	// test bvh
	fp_bvh_t testBVH;
	
	// debug
	bool debugIsSlowTime;
	bool debugShowFPS;
	bool debugShowMemoryStats;
};

typedef struct fc_main_thread_user_data_t
{
	FurGameEngine* pEngine;
	fc_alloc_callbacks_t* pAllocCallbacks;
} fc_main_thread_user_data_t;

// Furball Cat - Platform
bool furMainEngineInit(const FurGameEngineDesc& desc, FurGameEngine** ppEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
	fc_profiler_init(pAllocCallbacks);
	
	FurGameEngine* pEngine = (FurGameEngine*)FUR_ALLOC_AND_ZERO(sizeof(FurGameEngine), 0, FC_MEMORY_SCOPE_CORE, pAllocCallbacks);
	
	fc_string_hash_register_init(pAllocCallbacks);

	// mount loose file depot
	{
		fc_depot_desc_t desc = { 0 };
		desc.type = FC_DEPOT_LOOSE_FILES;

#if PLATFORM_OSX
		desc.path = "../../../../../";
#elif PLATFORM_WINDOWS
		desc.path = "../../../";
#endif

		pEngine->depot = fc_depot_mount(&desc, pAllocCallbacks);
	}

	fr_app_desc_t appDesc;
	appDesc.appTitle = desc.m_mainApp.m_title;
	appDesc.viewportWidth = desc.m_mainApp.m_width;
	appDesc.viewportHeight = desc.m_mainApp.m_height;
	appDesc.iconPath = fc_file_path_create(pEngine->depot, "data/icon/furball-cat-icon-128x128.png");
	appDesc.depot = pEngine->depot;

	fr_result_t res = fr_create_app(&appDesc, &pEngine->pApp, pAllocCallbacks);

	if(res == FR_RESULT_OK)
	{
		pEngine->pInputManager = fi_input_manager_create(pAllocCallbacks);
	}
	
	if(res == FR_RESULT_OK)
	{
		fr_renderer_desc_t rendererDesc;
		rendererDesc.pApp = pEngine->pApp;
		rendererDesc.depot = pEngine->depot;
		
		res = fr_create_renderer(&rendererDesc, &pEngine->pRenderer, pAllocCallbacks);
	}
	
	if(res == FR_RESULT_OK)
	{
		pEngine->pPhysics = fp_physics_create(pAllocCallbacks);
	}
	
	if(res == FR_RESULT_OK)
	{
		fc_job_system_init(pAllocCallbacks);
	}
	
	if(res == FR_RESULT_OK)
	{
		*ppEngine = pEngine;
	}
	else
	{
		furSetLastError(fr_get_last_error());
		FUR_FREE(pEngine, pAllocCallbacks);
		return false;
	}
	
	// init anim system
	pEngine->animSystem = fa_anim_sys_init(pAllocCallbacks);
	
	// create world
	pEngine->pWorld = (fg_world_t*)FUR_ALLOC_AND_ZERO(sizeof(fg_world_t), 0, FC_MEMORY_SCOPE_GAME, pAllocCallbacks);
	fg_world_init(pEngine->pWorld, pAllocCallbacks);
	pEngine->pWorld->systems.renderer = pEngine->pRenderer;
	pEngine->pWorld->systems.animation = pEngine->animSystem;
	
	// load scripts
	{
		pEngine->zeldaScriptPath = fc_file_path_create(pEngine->depot, "scripts/zelda-state-script.bin");

		fc_load_binary_file_into_binary_buffer(pEngine->depot, pEngine->zeldaScriptPath, &pEngine->zeldaStateScript, pAllocCallbacks);
		fg_resource_add_script(&pEngine->pWorld->resources, SID("ss-zelda"), &pEngine->zeldaStateScript);
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
#if PLATFORM_OSX
		const char* depotPath = "../../../../../";
#elif PLATFORM_WINDOWS
		const char* depotPath = "../../../";
#endif

		// load rig
		{
			const fc_file_path_t rigPath = fc_file_path_create(pEngine->depot, "data/rig/zelda-a-pose.rig");
			fc_file_t* file = fc_file_open(pEngine->depot, rigPath, "rb");
			
			if(file)
			{
				fc_serializer_t ser = {};
				ser.file = file;
				ser.isWriting = false;
				pEngine->pRig = (fa_rig_t*)FUR_ALLOC_AND_ZERO(sizeof(fa_rig_t), 0, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
				fa_rig_serialize(&ser, pEngine->pRig, pAllocCallbacks);
			}
		}
		
		// import rig
#if 0
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
					pEngine->pRig->maskUpperBody = FUR_ALLOC_ARRAY_AND_ZERO(u8, pEngine->pRig->numBones, 0, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
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
					pEngine->pRig->maskFace = FUR_ALLOC_ARRAY_AND_ZERO(u8, pEngine->pRig->numBones, 0, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
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
					pEngine->pRig->maskHands = FUR_ALLOC_ARRAY_AND_ZERO(u8, pEngine->pRig->numBones, 0, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
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
			
			fc_serializer_t ser = {};
			ser.file = engineFile;
			ser.isWriting = true;
			fa_rig_serialize(&ser, pEngine->pRig, pAllocCallbacks);
		}
#endif

		pEngine->pAnimClipWindProtect = fg_load_anim(&pEngine->pWorld->resources, pEngine->depot, "zelda-upper-wind-protect", pEngine->pRig, pAllocCallbacks);
		pEngine->pAnimClipHoldSword = fg_load_anim(&pEngine->pWorld->resources, pEngine->depot, "zelda-upper-hold-sword", pEngine->pRig, pAllocCallbacks);
		
		fg_load_anim(&pEngine->pWorld->resources, pEngine->depot, "zelda-idle-stand-relaxed", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, pEngine->depot, "zelda-funny-poses", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, pEngine->depot, "zelda-funny-pose-2", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, pEngine->depot, "zelda-funny-pose-3", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, pEngine->depot, "zelda-funny-pose-4", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, pEngine->depot, "zelda-loco-run-relaxed", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, pEngine->depot, "zelda-run-to-idle-sharp", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, pEngine->depot, "zelda-loco-idle-to-run-0", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, pEngine->depot, "zelda-loco-jump-in-place", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, pEngine->depot, "zelda-loco-jump", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, pEngine->depot, "zelda-additive", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, pEngine->depot, "zelda-a-pose", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, pEngine->depot, "zelda-face-idle", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, pEngine->depot, "zelda-idle-stand-01", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, pEngine->depot, "zelda-hands-idle", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, pEngine->depot, "zelda-wind-01", pEngine->pRig, pAllocCallbacks);
		fg_load_anim(&pEngine->pWorld->resources, pEngine->depot, "zelda-jump-loop", pEngine->pRig, pAllocCallbacks);
		
		// load meshes
		{
			fr_load_mesh_ctx_t meshCtx = {};
			meshCtx.path = fc_file_path_create(pEngine->depot, "data/mesh/zelda-sword.mesh");
			fc_file_path_t texturePaths[] = {fc_file_path_create(pEngine->depot, "data/texture/melee_diff.png")};
			const i32 textureIndices[] = {0};
			meshCtx.texturePaths = texturePaths;
			meshCtx.numTextures = FUR_ARRAY_SIZE(texturePaths);
			meshCtx.textureIndices = textureIndices;
			meshCtx.numTextureIndices = FUR_ARRAY_SIZE(textureIndices);
			pEngine->swordMesh = fr_load_mesh(pEngine->pRenderer, pEngine->depot, &meshCtx, pAllocCallbacks);
		}
		
		// load chest mesh
		{
			fr_load_mesh_ctx_t meshCtx = {};
			meshCtx.path = fc_file_path_create(pEngine->depot, "data/mesh/chest.mesh");
			fc_file_path_t texturePaths[] = { fc_file_path_create(pEngine->depot, "data/texture/chest_albedo.png")};
			const i32 textureIndices[] = {0};
			meshCtx.texturePaths = texturePaths;
			meshCtx.numTextures = FUR_ARRAY_SIZE(texturePaths);
			meshCtx.textureIndices = textureIndices;
			meshCtx.numTextureIndices = FUR_ARRAY_SIZE(textureIndices);
			pEngine->chestMesh = fr_load_mesh(pEngine->pRenderer, pEngine->depot, &meshCtx, pAllocCallbacks);
		}
		
		// load block mesh
		{
			fr_load_mesh_ctx_t meshCtx = {};
			meshCtx.path = fc_file_path_create(pEngine->depot, "data/mesh/skull_block_PBR_fc.mesh");
			fc_file_path_t texturePaths[] = { fc_file_path_create(pEngine->depot, "data/texture/b_stone1_Color.png")};
			const i32 textureIndices[] = {0};
			meshCtx.texturePaths = texturePaths;
			meshCtx.numTextures = FUR_ARRAY_SIZE(texturePaths);
			meshCtx.textureIndices = textureIndices;
			meshCtx.numTextureIndices = FUR_ARRAY_SIZE(textureIndices);
			pEngine->blockMesh = fr_load_mesh(pEngine->pRenderer, pEngine->depot, &meshCtx, pAllocCallbacks);
		}
		
		// set block positions and create colliders
		{
			pEngine->blockPositions[0] = {{4.0f, 4.0f, 0.0f, 1.0f}, {0.0f, 0.0f, 0.0f, 1.0f}};
			pEngine->blockPositions[1] = {{6.0f, 4.0f, 0.5f, 1.0f}, {0.0f, 0.0f, 0.0f, 1.0f}};
			
			const fm_vec3 halfExtents = {0.5f, 0.5f, 0.5f};
			
			for(i32 i=0; i<2; ++i)
			{
				fp_physics_add_static_box(pEngine->pPhysics, &pEngine->blockPositions[i], &halfExtents, pAllocCallbacks);
			}
		}
		
		// load rock meshes
		for(u32 i=0; i<5; ++i)
		{
			char txtPath[256];
			sprintf(txtPath, "data/mesh/rock-0%i.mesh", i+1);
			
			char txtTexturePath[256];
			sprintf(txtTexturePath, "data/texture/rock-0%i.png", i+1);
			
			fr_load_mesh_ctx_t meshCtx = {};
			meshCtx.path = fc_file_path_create(pEngine->depot, txtPath);
			fc_file_path_t texturePaths[] = { fc_file_path_create(pEngine->depot, txtTexturePath) };
			const i32 textureIndices[] = {0};
			meshCtx.texturePaths = texturePaths;
			meshCtx.numTextures = FUR_ARRAY_SIZE(texturePaths);
			meshCtx.textureIndices = textureIndices;
			meshCtx.numTextureIndices = FUR_ARRAY_SIZE(textureIndices);
			pEngine->rockMeshes[i] = fr_load_mesh(pEngine->pRenderer, pEngine->depot, &meshCtx, pAllocCallbacks);
		}
		
		// load zelda mesh
		{
			fr_load_mesh_ctx_t meshCtx = {};
			meshCtx.path = fc_file_path_create(pEngine->depot, "data/mesh/zelda-mesh.mesh");
			fc_file_path_t texturePaths[] = {
				fc_file_path_create(pEngine->depot, "data/texture/zelda_diff.png"),
				fc_file_path_create(pEngine->depot, "data/texture/hair_diff.png"),
				fc_file_path_create(pEngine->depot, "data/texture/eyes_diff2.png")
			};
			const i32 textureIndices[] = {0, 0, 1, 0, 2, 0, 1};
			meshCtx.texturePaths = texturePaths;
			meshCtx.numTextures = FUR_ARRAY_SIZE(texturePaths);
			meshCtx.textureIndices = textureIndices;
			meshCtx.numTextureIndices = FUR_ARRAY_SIZE(textureIndices);
			meshCtx.isSkinned = true;
			meshCtx.boneNames = pEngine->pRig->boneNameHashes;
			meshCtx.numBones = pEngine->pRig->numBones;
			pEngine->zeldaMesh = fr_load_mesh(pEngine->pRenderer, pEngine->depot, &meshCtx, pAllocCallbacks);
		}
	}
	
	// add resources to world resource register
	{
		fg_resource_add_rig(&pEngine->pWorld->resources, SID("zelda-rig"), pEngine->pRig);
		fg_resource_add_mesh(&pEngine->pWorld->resources, SID("zelda-mesh"), pEngine->zeldaMesh);
	}
	
	// init game
	{
		pEngine->gameObjectRegister.capacity = 128;
		pEngine->gameObjectRegister.objects = FUR_ALLOC_ARRAY_AND_ZERO(fg_game_object_t*, pEngine->gameObjectRegister.capacity, 0, FC_MEMORY_SCOPE_SCRIPT, pAllocCallbacks);
		pEngine->gameObjectRegister.ids = FUR_ALLOC_ARRAY_AND_ZERO(fc_string_hash_t, pEngine->gameObjectRegister.capacity, 0, FC_MEMORY_SCOPE_SCRIPT, pAllocCallbacks);
		pEngine->gameObjectRegister.numObjects = 0;
		
		// create Zelda
		pEngine->actionWeaponEquipped.animation = pEngine->pAnimClipHoldSword;
		pEngine->actionWeaponEquipped.forceLoop = true;
		
		pEngine->actionWindProtect.animation = pEngine->pAnimClipWindProtect;
		pEngine->actionWindProtect.forceLoop = true;
		
		fa_action_args_t args = {};
		args.fadeInSec = 0.3f;
		args.ikMode = FA_IK_MODE_LEGS;
		//fa_character_schedule_action_simple(&pEngine->animCharacterZelda, &pEngine->animSimpleAction, &args);
		
		fa_character_desc_t animCharacterDesc = {};
		animCharacterDesc.rig = pEngine->pRig;
		animCharacterDesc.globalTime = pEngine->globalTime;
		
		pEngine->zeldaGameObject.name = SID_REG("zelda");
		pEngine->zeldaGameObject.animCharacter = fa_anim_sys_create_character(&animCharacterDesc, pAllocCallbacks);
		fa_anim_sys_add_character(pEngine->animSystem, pEngine->zeldaGameObject.animCharacter);
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
		
		fa_dangle_desc desc;
		desc.frequency = 60.0f;
		desc.numParticles = numParticles;
		desc.dampingCoef = 0.96f;
		
		fa_dangle_create(&desc, &pEngine->dangle, pAllocCallbacks);
		
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
		const fc_string_hash_t handRight = SID("Bip001_Hand_R");
		
		for(u32 i=0; i<pEngine->pRig->numBones; ++i)
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
		
		for(u32 i=0; i<pEngine->pRig->numBones; ++i)
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
		
		const f32 dLeft = fm_vec4_mag(&refPoseLeft2.pos);
		const f32 dRight = fm_vec4_mag(&refPoseRight2.pos);
		
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
		
		for(u32 i=0; i<pEngine->pRig->numBones; ++i)
		{
			const fc_string_hash_t name = pEngine->pRig->boneNameHashes[i];
			
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
		static fc_string_hash_t prop_names[props_num] = {SID("state-script")};
		static fg_spawn_info_prop_value_t prop_values[props_num] = {};
		prop_values[0].asStringHash = SID("ss-zelda");
		
		static fg_spawner_t spawner = {};
		spawner.name = SID("zelda");
		spawner.typeName = SID("zelda");
		spawner.info.gameObjectName = SID("zelda");
		spawner.info.props.num = props_num;
		spawner.info.props.names = prop_names;
		spawner.info.props.values = prop_values;
		
		fg_spawn(&spawner, pEngine->pWorld);
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

fs_variant_t fs_native_animate(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args)
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
	for(u32 i=2; i+1<numArgs; i += 2)
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

fs_variant_t fs_native_wait_animate(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args)
{
	FUR_ASSERT(numArgs >= 2);
	const fc_string_hash_t animName = args[1].asStringHash;
	
	const fa_anim_clip_t* animClip = fg_resource_find_anim(&ctx->world->resources, animName);
	FUR_ASSERT(animClip);
	
	ctx->waitSeconds = animClip->duration;
	
	return fs_native_animate(ctx, numArgs, args);
}

fs_variant_t fs_native_equip_item(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args)
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

fs_variant_t fs_native_wait_seconds(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args)
{
	FUR_ASSERT(numArgs == 1);
	const f32 timeInSeconds = args[0].asFloat;
	
	ctx->waitSeconds = timeInSeconds;
	
	fs_variant_t result = {};
	return result;
}

fs_variant_t fs_native_get_variable(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args)
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

fs_variant_t fs_native_go(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args)
{
	FUR_ASSERT(numArgs == 1);
	const fc_string_hash_t goToState = args[0].asStringHash;
	
	ctx->nextState = goToState;
	
	fs_variant_t result = {};
	return result;
}

fs_variant_t fs_native_go_when(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args)
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

fs_variant_t fs_native_cmp_gt(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args)
{
	FUR_ASSERT(numArgs == 2);
	const i32 a = args[0].asInt32;
	const i32 b = args[1].asInt32;
	
	fs_variant_t result;
	result.asBool = a > b;
	return result;
}

fs_variant_t fs_native_cmp_eq(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args)
{
	FUR_ASSERT(numArgs == 2);
	const i32 a = args[0].asInt32;
	const i32 b = args[1].asInt32;
	
	fs_variant_t result;
	result.asBool = (a == b);
	return result;
}

fs_variant_t fs_native_camera_enable(fs_script_ctx_t* ctx, u32 numArgs, const fs_variant_t* args)
{
	FUR_ASSERT(numArgs == 3);
	const fc_string_hash_t objectName = args[0].asStringHash;
	const fc_string_hash_t cameraType = args[1].asStringHash;
	const f32 fadeInSec = args[2].asFloat;
	
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

void fg_scripts_update(FurGameEngine* pEngine, f32 dt)
{
	// todo
}

bool g_drawDevMenu = false;
i32 g_devMenuOption = 0;
bool g_devMenuOptionClick = false;

void fg_input_actions_update(FurGameEngine* pEngine, f32 dt)
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
	
	fi_input_event_t inputEvents[40];
	const u32 numEventsCollected = fi_get_input_events(pEngine->pInputManager, inputEvents, 40, 0);
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
	else if(fc_profiler_is_draw_on())
	{
		if (actionPressed)
		{
			fc_profiler_toggle_pause();
		}

		const f32 zoomDelta = leftTrigger - rightTrigger;
		const f32 panDelta = rightAnalogX;
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
		
		pEngine->inActionPressed = actionPressed;
		pEngine->inputTriangleActionPressed = triangleActionPressed;
		pEngine->inputCircleActionPressed = circleActionPressed;
		pEngine->inputXPressed = xPressed;
	}
}

void fc_dev_menu_reload_scripts(FurGameEngine* pEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
	fc_release_binary_buffer(&pEngine->zeldaStateScript, pAllocCallbacks);
	fc_load_binary_file_into_binary_buffer(pEngine->depot, pEngine->zeldaScriptPath, &pEngine->zeldaStateScript, pAllocCallbacks);
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
	const f32 color[4] = {0.8f, 0.8f, 0.8f, 1.0f};
	const f32 colorCursor[4] = {0.9f, 0.9f, 0.9f, 1.0f};
	const f32 colorLabel[4] = FUR_COLOR_CYAN;
	const f32 colorSelected[4] = FUR_COLOR_YELLOW;
	
	if(g_drawDevMenu)
	{
		const f32 text_scale = 0.7f;
		const f32 lineHeight = fc_dbg_get_text_line_height(text_scale);
		const f32 ident = fc_dbg_get_text_line_height(text_scale);

		f32 x = 70.0f;
		f32 y = 70.0f;
		fc_dbg_apply_anchor(&x, &y, FC_DBG_ANCHOR_LEFT_UP_CORNER);
		
		fc_dev_menu_option_t options[] = {
			{"quick-fps-mem", fc_dev_menu_show_fps},
			{"reload-scripts", fc_dev_menu_reload_scripts},
			{"slow-time", fc_dev_menu_slow_time},
			{"show-player-anim-state", fc_dev_menu_show_player_anim_state},
			{"profiler", fc_dev_menu_show_profiler},
			{"memory-stats", fc_dev_menu_show_memory_stats}
		};
		
		const u32 numOptions = FUR_ARRAY_SIZE(options);
		
		if(g_devMenuOption < 0)
			g_devMenuOption = numOptions-1;
		else if(g_devMenuOption >= numOptions)
			g_devMenuOption = 0;
		
		const f32 bgColor[4] = {0.2f, 0.2f, 0.2f, 0.8f};
		
		fc_dbg_rect(x - 20.0f, y - 20.0f, 450.0f, 110.0f + 28.0f * numOptions, bgColor);
		
		fc_dbg_text(x, y, "Dev Menu:", colorLabel, text_scale);
		
		for(u32 i=0; i<numOptions; ++i)
		{
			const bool isSelected = (i == g_devMenuOption);
			
			fc_dbg_text(x + ident, y + lineHeight * (1+i), options[i].name, isSelected ? colorSelected : color, text_scale);
			
			if(isSelected)
			{
				fc_dbg_text(x + ident * 0.2f, y + lineHeight * (1+i), ">", colorCursor, text_scale);
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

void fg_gameplay_update(FurGameEngine* pEngine, f32 dt)
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
		
		fs_script_lambda_t* lambda = &pEngine->scriptLambdas[lambdaSlot];
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
				fa_character_schedule_none_action(pEngine->zeldaGameObject.animCharacter, &args);
				
				pEngine->zeldaGameObject.playerWeaponEquipped = false;
			}
			else
			{
				fa_action_args_t args = {};
				args.fadeInSec = 0.5f;
				args.layer = FA_CHAR_LAYER_PARTIAL;
				fa_character_schedule_action_simple(pEngine->zeldaGameObject.animCharacter, &pEngine->actionWeaponEquipped, &args);
				
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
			fa_character_schedule_none_action(pEngine->zeldaGameObject.animCharacter, &args);
			
			pEngine->zeldaGameObject.playerWindProtecting = false;
		}
		else
		{
			fa_action_args_t args = {};
			args.fadeInSec = 0.5f;
			args.layer = FA_CHAR_LAYER_PARTIAL;
			fa_character_schedule_action_simple(pEngine->zeldaGameObject.animCharacter, &pEngine->actionWindProtect, &args);
			
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
		
		fp_physics_raycast_hit_t hit = {};
		const bool isHit = fp_physics_raycast(pEngine->pPhysics, &pos, &dir, distance, &hit);
		if(isHit)
		{
			isGrounded = true;
			const f32 extent[3] = {0.02f, 0.02f, 0.02f};
			const f32 color[4] = FUR_COLOR_RED;
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

void fg_animation_update(FurGameEngine* pEngine, f32 dt)
{
	// set last known world position for character
	fm_xform playerLocator;
	fp_physics_player_info_t playerPhysics;
	playerPhysics.locator = &playerLocator;
	fp_physics_get_player_info(pEngine->pPhysics, &playerPhysics);
	
	pEngine->zeldaGameObject.animCharacter->animInfo.worldPos.x = playerLocator.pos.x;
	pEngine->zeldaGameObject.animCharacter->animInfo.worldPos.y = playerLocator.pos.y;
	pEngine->zeldaGameObject.animCharacter->animInfo.worldPos.z = playerLocator.pos.z;
	
	fc_mem_arena_alloc_t arenaAlloc = fc_mem_arena_make(pEngine->scratchpadBuffer, pEngine->scratchpadBufferSize);
	
	fa_anim_sys_update_ctx_t animCtx = {};
	animCtx.dt = dt;
	animCtx.globalTime = pEngine->globalTime;
	animCtx.arenaAlloc = &arenaAlloc;
	
	fa_anim_sys_update(pEngine->animSystem, &animCtx);
}

void fc_dbg_mat4(const fm_mat4* m)
{
	const f32 pos[3] = {m->w.x, m->w.y, m->w.z};
	const f32 scale = 0.1f;
	const f32 axisX[3] = {pos[0] + m->x.x * scale, pos[1] + m->x.y * scale, pos[2] + m->x.z * scale};
	const f32 axisY[3] = {pos[0] + m->y.x * scale, pos[1] + m->y.y * scale, pos[2] + m->y.z * scale};
	const f32 axisZ[3] = {pos[0] + m->z.x * scale, pos[1] + m->z.y * scale, pos[2] + m->z.z * scale};
	
	const f32 red[4] = FUR_COLOR_RED;
	const f32 green[4] = FUR_COLOR_GREEN;
	const f32 blue[4] = FUR_COLOR_BLUE;
	
	fc_dbg_line(pos, axisX, red);
	fc_dbg_line(pos, axisY, green);
	fc_dbg_line(pos, axisZ, blue);
}

void fc_dbg_stats_for_mem_to_text(fc_memory_scope_t scope, char* txt, const char* displayName)
{
	fc_mem_stats_t stats = fc_memory_stats_for_scope(scope);
	
	const f32 numMBs = ((f32)stats.numBytesUsed) / (1024.0f * 1024.0f);
	const f32 numCapacityMBs = ((f32)stats.numBytesCapacity) / (1024.0f * 1024.0f);
	
	const char* name = fc_memory_get_scope_debug_name(scope);
	
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
			fc_job_decl_t job = {test_job, &subNumStuff};
			fc_job_counter_t* counter = {};
			fc_run_jobs(&job, 1, &counter);
			fc_wait_for_counter_and_free(counter);
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

void furMainEngineGameUpdate(FurGameEngine* pEngine, f32 dt, fc_alloc_callbacks_t* pAllocCallbacks)
{
	// show debug FPS
	if(pEngine->debugShowFPS)
	{
		f32 text_scale = 0.7f;
		const f32 offset_line = fc_dbg_get_text_line_height(text_scale);

		f32 x = 0;
		f32 y = 0;
		fc_dbg_apply_anchor(&x, &y, FC_DBG_ANCHOR_LEFT_UP_CORNER);

		{
			const f32 fps = 1.0f / dt;
			const f32 ms = dt * 1000.0f;
			
			char txt[50];
			sprintf(txt, "CPU: %1.1f fps (%1.1f ms)", fps, ms);
			
			const f32 green[4] = FUR_COLOR_GREEN;
			const f32 yellow[4] = FUR_COLOR_YELLOW;
			const f32 red[4] = FUR_COLOR_RED;
			fc_dbg_text(x, y, txt, ms < 16 ? green : ms < 33 ? yellow : red, text_scale);
		}
		{
			fc_mem_stats_t stats = fc_memory_stats();
			
			const f32 numMBs = ((f32)stats.numBytesUsed) / (1024.0f * 1024.0f);
			const f32 numCapacityMBs = ((f32)stats.numBytesCapacity) / (1024.0f * 1024.0f);
			
			char txt[50];
			sprintf(txt, "MEM: %1.2f / %1.2f MBs (%u allocs)", numMBs, numCapacityMBs, stats.numAllocs);
			
			const f32 green[4] = FUR_COLOR_GREEN;
			const f32 yellow[4] = FUR_COLOR_YELLOW;
			const f32 red[4] = FUR_COLOR_RED;
			fc_dbg_text(x, y + offset_line, txt, numMBs < numCapacityMBs * 0.95f ? green : numMBs < numCapacityMBs ? yellow : red, text_scale);
		}
	}
	
	// show memory statistics for each memory scope
	if(pEngine->debugShowMemoryStats)
	{
		const f32 white[4] = FUR_COLOR_WHITE;
		i32 textLineCounter = 0;

		f32 x = 20.0f;
		f32 y = 400.0f;
		fc_dbg_apply_anchor(&x, &y, FC_DBG_ANCHOR_LEFT_UP_CORNER);
		const f32 text_scale = 0.7f;
		const f32 line_height = fc_dbg_get_text_line_height(text_scale);
		
		fc_dbg_text(x, y - line_height, "-[ Retail Memory ]-----------------------------------------------", white, text_scale);
		
		char txt[128] = {};
		
		fc_dbg_stats_for_mem_to_text(FC_MEMORY_SCOPE_SYSTEM, txt, "OS Memory");
		fc_dbg_text(x, y + textLineCounter * line_height, txt, white, text_scale);
		textLineCounter++;
		
		fc_dbg_stats_for_mem_to_text(FC_MEMORY_SCOPE_GLOBAL, txt, "Code And Static Data");
		fc_dbg_text(x, y + textLineCounter * line_height, txt, white, text_scale);
		textLineCounter++;
		
		textLineCounter++;
		fc_dbg_text(x, y + textLineCounter * line_height, "-[ Debug Memory ]------------------------------------------------", white, text_scale);
		textLineCounter++;
		
		fc_dbg_stats_for_mem_to_text(FC_MEMORY_SCOPE_DEBUG, txt, "Debug Memory");
		fc_dbg_text(x, y + textLineCounter * line_height, txt, white, text_scale);
		textLineCounter++;
	}
	
	// slow-time debug mode - this needs to be after debugShowFPS
	if(pEngine->debugIsSlowTime)
	{
		const f32 color[4] = FUR_COLOR_RED;
		const f32 scale = 0.7f;
		f32 x = -7.0f * fc_dbg_get_text_line_height(scale);
		f32 y = -fc_dbg_get_text_line_height(scale);
		fc_dbg_apply_anchor(&x, &y, FC_DBG_ANCHOR_RIGHT_BOTTOM_CORNER);
		fc_dbg_text(x, y, "slow-time ON", color, scale);
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
		
		fg_world_update_ctx_t worldUpdateCtx = {};
		worldUpdateCtx.dt = dt;
		fg_world_update(pEngine->pWorld, &worldUpdateCtx, FG_UPDATE_BUCKET_CHARACTERS);
	}
	
	// test set look-at point
	{
		// get zelda position
		fm_xform playerLocator;
		fp_physics_player_info_t playerPhysics;
		playerPhysics.locator = &playerLocator;
		fp_physics_get_player_info(pEngine->pPhysics, &playerPhysics);
		
		static f32 time = 0.0f;
		time += dt;
		
		// convert world space look-at point to model space of player
		fm_vec4 lookAtPoint = {2.0f * sinf(time), 2.0f * cosf(time), 1.0f + 1.0f * sinf(time * 0.6f), 1.0f};	// in world space
		
		f32 color[4] = FUR_COLOR_MAGENTA;
		fc_dbg_sphere_wire(&lookAtPoint.x, 0.1f, color);
		
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
		fg_animation_update(pEngine, dt);
	}
	
	{
		const fm_vec4 zeros = {0.0f, 0.0f, 0.0f, 0.0f};
		const fm_vec4 axisX = {1.0f, 0.0f, 0.0f, 0.0f};
		const fm_vec4 axisY = {0.0f, 1.0f, 0.0f, 0.0f};
		const fm_vec4 axisZ = {0.0f, 0.0f, 1.0f, 0.0f};
		
		const f32 red[4] = FUR_COLOR_RED;
		const f32 green[4] = FUR_COLOR_GREEN;
		const f32 blue[4] = FUR_COLOR_BLUE;
		
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
			fm_mat4_rot_z(pEngine->zeldaGameObject.animCharacter->animInfo.currentYaw, &playerMat);
			
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
			
			fa_dangle_simulate(&simCtx, &pEngine->zeldaCapeL);
			fa_dangle_simulate(&simCtx, &pEngine->zeldaCapeC);
			fa_dangle_simulate(&simCtx, &pEngine->zeldaCapeR);
			
			fm_mat4 m[4] = {};
			
			m[0] = pEngine->skinMatrices[pEngine->zeldaCapeIdxL[0]];
			fa_dangle_to_matrices_y_down(&pEngine->zeldaCapeL, &m[0], m);
			
			for(u32 i=0; i<4; ++i)
			{
				pEngine->skinMatrices[pEngine->zeldaCapeIdxL[i]] = m[i];
			}
			
			m[0] = pEngine->skinMatrices[pEngine->zeldaCapeIdxC[0]];
			fa_dangle_to_matrices_y_down(&pEngine->zeldaCapeC, &m[0], m);
			
			for(u32 i=0; i<4; ++i)
			{
				pEngine->skinMatrices[pEngine->zeldaCapeIdxC[i]] = m[i];
			}
			
			m[0] = pEngine->skinMatrices[pEngine->zeldaCapeIdxR[0]];
			fa_dangle_to_matrices_y_down(&pEngine->zeldaCapeR, &m[0], m);
			
			for(u32 i=0; i<4; ++i)
			{
				pEngine->skinMatrices[pEngine->zeldaCapeIdxR[i]] = m[i];
			}
		}
	}
	
	// test jobs
	{
		i32 numStuff = 40;
		
		fc_job_decl_t jobs[40] = {};
		for(i32 i=0; i<40; ++i)
		{
			jobs[i].func = test_job;
			jobs[i].userData = &numStuff;
		}
		
		fc_job_counter_t* counter = {};
		fc_run_jobs(jobs, 40, &counter);
		fc_wait_for_counter_and_free(counter);
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
		const f32 maxSpeed = 5.0f;
		
		fm_vec4 dirForward = {};
		fm_vec4 dirLeft = {};
		fg_camera_get_directions(pEngine->cameraSystem, &dirForward, &dirLeft);
		
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

FUR_JOB_ENTRY_POINT(fur_engine_main_thread_loop)
{
	fc_main_thread_user_data_t* userData = FUR_JOB_USER_DATA(fc_main_thread_user_data_t);
	
	FurGameEngine* pEngine = userData->pEngine;
	fc_alloc_callbacks_t* pAllocCallbacks = userData->pAllocCallbacks;
	
	pEngine->prevTimePoint = std::chrono::system_clock::now();
	
	while(fr_update_app(pEngine->pApp))
	{
		std::chrono::system_clock::time_point timeNow = std::chrono::system_clock::now();
		std::chrono::duration<f32> dtOrig = timeNow - pEngine->prevTimePoint;
		pEngine->prevTimePoint = timeNow;
		
		const f32 dt = dtOrig.count();
		
		fc_profiler_start_frame();
		
		FUR_PROFILE("frame")
		{
			furMainEngineGameUpdate(pEngine, dt, pAllocCallbacks);
		}
		
		fc_profiler_end_frame();
	}
	
	fr_wait_for_device(pEngine->pRenderer);
	
	fc_job_system_exit_all_jobs();
}

void furMainEngineLoop(FurGameEngine* pEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
	fc_main_thread_user_data_t data = {pEngine, pAllocCallbacks};
	
	fc_job_decl_t mainThreadJob = {};
	mainThreadJob.userData = &data;
	mainThreadJob.func = fur_engine_main_thread_loop;
	fc_job_system_setup_main_thread_job(&mainThreadJob);
	
	// see fur_engine_main_thread_loop for the actual main thread loop
	fc_job_system_enter_worker_thread_mode();
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
	for(u32 i=0; i<5; ++i)
	{
		fr_release_proxy(pEngine->pRenderer, pEngine->rockMeshes[i], pAllocCallbacks);
	}
	
	fa_dangle_release(&pEngine->dangle, pAllocCallbacks);
	fa_dangle_release(&pEngine->zeldaDangleHairLeft, pAllocCallbacks);
	fa_dangle_release(&pEngine->zeldaDangleHairRight, pAllocCallbacks);
	
	fa_dangle_release(&pEngine->zeldaCapeL, pAllocCallbacks);
	fa_dangle_release(&pEngine->zeldaCapeC, pAllocCallbacks);
	fa_dangle_release(&pEngine->zeldaCapeR, pAllocCallbacks);
	
	fa_anim_sys_remove_character(pEngine->animSystem, pEngine->zeldaGameObject.animCharacter);
	fa_anim_sys_release_character(pEngine->zeldaGameObject.animCharacter, pAllocCallbacks);
	
	fg_world_release(pEngine->pWorld, pAllocCallbacks);
	FUR_FREE(pEngine->pWorld, pAllocCallbacks);
	
	FUR_FREE(pEngine->gameObjectRegister.objects, pAllocCallbacks);
	FUR_FREE(pEngine->gameObjectRegister.ids, pAllocCallbacks);
	
	FUR_FREE(pEngine->scratchpadBuffer, pAllocCallbacks);
	
	fa_anim_sys_release(pEngine->animSystem, pAllocCallbacks);
	
	fg_camera_system_release(pEngine->cameraSystem, pAllocCallbacks);
	
	fc_job_system_release(pAllocCallbacks);
	fp_physics_release(pEngine->pPhysics, pAllocCallbacks);
	fr_release_renderer(pEngine->pRenderer, pAllocCallbacks);
	
	fi_input_manager_release(pEngine->pInputManager, pAllocCallbacks);
	
	fc_depot_unmount(pEngine->depot, pAllocCallbacks);

	fc_string_hash_register_release(pAllocCallbacks);
	
	fc_profiler_release(pAllocCallbacks);
	
	// release all memory before this call, otherwise it might be treated as memory leak
	fr_release_app(pEngine->pApp, pAllocCallbacks);
	
	FUR_FREE(pEngine, pAllocCallbacks);	// rest of the deallocations should happen through allocators
	
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

	// validate memory - after this line there should be no fur_alloc/fur_free functions called
	// all memory deallocations should be already done at this point
	FUR_ASSERT(fc_validate_memory());
	
	return 0;
}
