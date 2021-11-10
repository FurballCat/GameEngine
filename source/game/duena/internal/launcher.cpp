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
#include "cinput/public.h"

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
} fg_game_object_register_t;

typedef struct fg_animations_register_t
{
	fc_string_hash_t animNames[128];
	fa_anim_clip_t* animClips[128];
	
	uint32_t numAnims;
} fg_animations_register_t;

void fg_animations_register_add_anim(fg_animations_register_t* reg, fa_anim_clip_t* clip)
{
	FUR_ASSERT(reg->numAnims < 128);
	
	const uint32_t idx = reg->numAnims;
	reg->numAnims += 1;
	
	reg->animNames[idx] = clip->name;
	reg->animClips[idx] = clip;
}

void fg_animations_register_release_anims(fg_animations_register_t* reg, fc_alloc_callbacks_t* pAllocCallbacks)
{
	for(uint32_t i=0; i<reg->numAnims; ++i)
	{
		fa_anim_clip_release(reg->animClips[i], pAllocCallbacks);
	}
	
	memset(reg, 0, sizeof(fg_animations_register_t));
}

const fa_anim_clip_t* fg_animations_register_find_anim(const fg_animations_register_t* reg, fc_string_hash_t name)
{
	for(uint32_t i=0; i<reg->numAnims; ++i)
	{
		if(reg->animClips[i]->name == name)
		{
			return reg->animClips[i];
		}
	}
	
	return NULL;
}

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
	fg_animations_register_t* allAnimations;
} fs_script_ctx_t;

// todo: move it somewhere else
fs_variant_t fs_native_animate(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args);
fs_variant_t fs_native_wait_animate(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args);
fs_variant_t fs_native_equip_item(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args);
fs_variant_t fs_native_wait_seconds(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args);
fs_variant_t fs_native_get_variable(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args);
fs_variant_t fs_native_go(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args);

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
		if(header.segmentId == segmentType)
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
	fc_string_hash_t name;	// access in scripts example: 'zelda
	fg_animate_action_slots_t animateActionSlots;
	
	fa_character_t* animCharacter;
	
	fc_string_hash_t playerState;
	bool playerWeaponEquipped;
	bool playerWindProtecting;
	bool equipItemNow;
	bool showAnimStateDebug;
	
} fg_game_object_t;

struct FurGameEngine
{
	struct fr_app_t* pApp;
	struct fr_renderer_t* pRenderer;
	fp_physics_t* pPhysics;
	
	std::chrono::system_clock::time_point prevTimePoint;
	float globalTime;
	float blendAlpha;
	
	fp_physics_scene_t* pPhysicsScene;
	
	fi_input_manager_t* pInputManager;
	
	// animation
	fa_rig_t* pRig;
	fa_anim_clip_t* pAnimClipIdleStand;
	fa_anim_clip_t* pAnimClipIdle;
	fa_anim_clip_t* pAnimClipIdle2;
	fa_anim_clip_t* pAnimClipIdle3;
	fa_anim_clip_t* pAnimClipIdle4;
	fa_anim_clip_t* pAnimClipRun;
	fa_anim_clip_t* pAnimClipRunToIdleSharp;
	fa_anim_clip_t* pAnimClipIdleToRun0;
	fa_anim_clip_t* pAnimClipAdditive;
	fa_anim_clip_t* pAnimClipAPose;
	fa_anim_clip_t* pAnimClipWindProtect;
	fa_anim_clip_t* pAnimClipHoldSword;
	fa_anim_clip_t* pAnimClipJumpInPlace;
	fa_anim_clip_t* pAnimClipJump;
	
	// input actions
	bool inActionPressed;
	bool inputTriangleActionPressed;
	bool inputCircleActionPressed;
	float actionRotationLeftX;
	float actionZoomIn;
	float actionZoomOut;
	float actionMoveX;
	float actionMoveY;
	
	fm_vec4 playerMove;
	
	// camera
	fg_camera_t* camera;
	
	// gameplay animation states
	fa_character_t animCharacterZelda;
	fa_action_animate_t actionIdle;
	fa_action_animate_t actionLoco;
	fa_action_animate_t actionWeaponEquipped;
	fa_action_animate_t actionWindProtect;
	fa_action_player_jump_t actionJump;
	fa_action_player_loco_t actionPlayerLoco;
	
	fa_action_player_loco_start_t actionLocoStart;
	fa_action_player_loco_start_t actionLocoStop;
	
	// skinning
	fm_mat4 skinMatrices[512];
	
	// update memory (scratchpad)
	void* scratchpadBuffer;
	uint32_t scratchpadBufferSize;
	
	// game objects
	fg_game_object_register_t gameObjectRegister;
	
	// all animations register
	fg_animations_register_t gameAnimationsRegister;
	
	// scripts temp
	fc_binary_buffer_t zeldaStateScript;
	fg_game_object_t zeldaGameObject;
	
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
	
	// debug
	bool debugIsSlowTime;
	bool debugShowFPS;
};

fa_anim_clip_t* fe_load_anim_clip(const fi_depot_t* depot, const char* name, FurGameEngine* pEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
	const char* directory = "assets/characters/zelda/animations/";
	const char* extension = ".fbx";
	const size_t dirLength = strlen(directory);
	const size_t nameLenght = strlen(name);
	
	char path[256] = {};
	memcpy(path, directory, dirLength);
	memcpy(path + dirLength, name, nameLenght);
	memcpy(path + dirLength + nameLenght, extension, strlen(extension));
	
	fi_import_anim_clip_ctx_t ctx = {};
	ctx.path = path;
	ctx.extractRootMotion = true;
	
	fa_anim_clip_t* animClip = NULL;
	
	fi_import_anim_clip(depot, &ctx, &animClip, pAllocCallbacks);
	fg_animations_register_add_anim(&pEngine->gameAnimationsRegister, animClip);
	
	return animClip;
}

// Furball Cat - Platform
bool furMainEngineInit(const FurGameEngineDesc& desc, FurGameEngine** ppEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
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
		res = fp_init_physics(&pEngine->pPhysics, pAllocCallbacks) == 0 ? FR_RESULT_OK : FR_RESULT_PHYSICS_INIT_ERROR;
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
	
	// create physics scene
	if(res == FR_RESULT_OK)
	{
		fp_physics_scene_create(pEngine->pPhysics, &pEngine->pPhysicsScene, pAllocCallbacks);
	}
	
	// load scripts
	{
		fc_load_binary_file_into_binary_buffer("../../../../../scripts/zelda.fs", &pEngine->zeldaStateScript, pAllocCallbacks);
	}
	
	// init camera
	fg_camera_create(&pEngine->camera, pAllocCallbacks);
	
	// init scratchpad buffer
	pEngine->scratchpadBufferSize = 256 * 1024;
	pEngine->scratchpadBuffer = FUR_ALLOC_AND_ZERO(pEngine->scratchpadBufferSize, 16, FC_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
	
	// load resources
	{
		const char* depotPath = "../../../../../";

		fi_depot_t depot;
		depot.path = depotPath;

		const char* characterRigPath = "assets/characters/zelda/animations/zelda-a-pose.fbx";

		// import animation resources
		{
			fi_import_rig_ctx_t ctx = {};
			ctx.path = characterRigPath;
			
			fi_import_rig(&depot, &ctx, &pEngine->pRig, pAllocCallbacks);
			
			// apply rig properties
			{
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
									w = 40;
								}
							}
							pEngine->pRig->maskUpperBody[i] = w;
						}
					}
				}
			}
		}

		pEngine->pAnimClipIdleStand = fe_load_anim_clip(&depot, "zelda-idle-stand-relaxed", pEngine, pAllocCallbacks);
		pEngine->pAnimClipIdle = fe_load_anim_clip(&depot, "zelda-funny-poses", pEngine, pAllocCallbacks);
		pEngine->pAnimClipIdle2 = fe_load_anim_clip(&depot, "zelda-funny-pose-2", pEngine, pAllocCallbacks);
		pEngine->pAnimClipIdle3 = fe_load_anim_clip(&depot, "zelda-funny-pose-3", pEngine, pAllocCallbacks);
		pEngine->pAnimClipIdle4 = fe_load_anim_clip(&depot, "zelda-funny-pose-4", pEngine, pAllocCallbacks);
		pEngine->pAnimClipRun = fe_load_anim_clip(&depot, "zelda-loco-run-relaxed", pEngine, pAllocCallbacks);
		pEngine->pAnimClipRunToIdleSharp = fe_load_anim_clip(&depot, "zelda-run-to-idle-sharp", pEngine, pAllocCallbacks);
		pEngine->pAnimClipIdleToRun0 = fe_load_anim_clip(&depot, "zelda-loco-idle-to-run-0", pEngine, pAllocCallbacks);
		pEngine->pAnimClipJumpInPlace = fe_load_anim_clip(&depot, "zelda-loco-jump-in-place", pEngine, pAllocCallbacks);
		pEngine->pAnimClipJump = fe_load_anim_clip(&depot, "zelda-loco-jump", pEngine, pAllocCallbacks);
		pEngine->pAnimClipAdditive = fe_load_anim_clip(&depot, "zelda-additive", pEngine, pAllocCallbacks);
		pEngine->pAnimClipAPose = fe_load_anim_clip(&depot, "zelda-a-pose", pEngine, pAllocCallbacks);
		pEngine->pAnimClipWindProtect = fe_load_anim_clip(&depot, "zelda-upper-wind-protect", pEngine, pAllocCallbacks);
		pEngine->pAnimClipHoldSword = fe_load_anim_clip(&depot, "zelda-upper-hold-sword", pEngine, pAllocCallbacks);

	}
	
	fr_temp_create_skinning_mapping(pEngine->pRenderer, pEngine->pRig->boneNameHashes, pEngine->pRig->numBones, pAllocCallbacks);
	
	// init game
	{
		pEngine->gameObjectRegister.capacity = 128;
		pEngine->gameObjectRegister.objects = FUR_ALLOC_ARRAY_AND_ZERO(fg_game_object_t*, pEngine->gameObjectRegister.capacity, 0, FC_MEMORY_SCOPE_SCRIPT, pAllocCallbacks);
		pEngine->gameObjectRegister.ids = FUR_ALLOC_ARRAY_AND_ZERO(fc_string_hash_t, pEngine->gameObjectRegister.capacity, 0, FC_MEMORY_SCOPE_SCRIPT, pAllocCallbacks);
		pEngine->gameObjectRegister.numObjects = 0;
		
		// create Zelda
		pEngine->animCharacterZelda.rig = pEngine->pRig;
		pEngine->animCharacterZelda.poseMS = FUR_ALLOC_ARRAY_AND_ZERO(fm_xform, pEngine->animCharacterZelda.rig->numBones, 16, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
		pEngine->animCharacterZelda.layers[0].poseCache.tempPose.xforms = FUR_ALLOC_ARRAY_AND_ZERO(fm_xform, pEngine->animCharacterZelda.rig->numBones, 16, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
		pEngine->animCharacterZelda.layers[0].poseCache.tempPose.weightsXforms = FUR_ALLOC_ARRAY_AND_ZERO(uint8_t, pEngine->animCharacterZelda.rig->numBones, 16, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
		pEngine->animCharacterZelda.layers[0].poseCache.tempPose.numXforms = pEngine->animCharacterZelda.rig->numBones;
		
		pEngine->animCharacterZelda.layers[1].poseCache.tempPose.xforms = FUR_ALLOC_ARRAY_AND_ZERO(fm_xform, pEngine->animCharacterZelda.rig->numBones, 16, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
		pEngine->animCharacterZelda.layers[1].poseCache.tempPose.weightsXforms = FUR_ALLOC_ARRAY_AND_ZERO(uint8_t, pEngine->animCharacterZelda.rig->numBones, 16, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
		pEngine->animCharacterZelda.layers[1].poseCache.tempPose.numXforms = pEngine->animCharacterZelda.rig->numBones;
		
		pEngine->animCharacterZelda.layers[FA_CHAR_LAYER_UPPER_BODY].maskID = FA_MASK_UPPER_BODY;
		
		pEngine->actionIdle.animation = pEngine->pAnimClipIdleStand;
		pEngine->actionIdle.forceLoop = true;
		
		pEngine->actionLoco.animation = pEngine->pAnimClipRun;
		pEngine->actionLoco.forceLoop = true;
		
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
		
		// run
		pEngine->actionPlayerLoco.anims[FA_ACTION_PLAYER_LOCO_ANIM_IDLE] = pEngine->pAnimClipIdleStand;
		pEngine->actionPlayerLoco.anims[FA_ACTION_PLAYER_LOCO_ANIM_RUN] = pEngine->pAnimClipRun;
		pEngine->actionPlayerLoco.anims[FA_ACTION_PLAYER_LOCO_ANIM_RUN_TO_IDLE_SHARP] = pEngine->pAnimClipRunToIdleSharp;
		pEngine->actionPlayerLoco.anims[FA_ACTION_PLAYER_LOCO_ANIM_IDLE_TO_RUN_0] = pEngine->pAnimClipIdleToRun0;
		
		// start/stop
		pEngine->actionLocoStart.anims[0] = pEngine->pAnimClipIdleToRun0;
		pEngine->actionLocoStart.finishFromEnd = 0.62f;
		pEngine->actionLocoStop.anims[0] = pEngine->pAnimClipRunToIdleSharp;
		pEngine->actionLocoStop.finishFromEnd = 0.3f;
		pEngine->actionLocoStop.ignoreYaw = true;
		
		// jump
		pEngine->actionJump.anims[0] = pEngine->pAnimClipJumpInPlace;
		pEngine->actionJump.anims[1] = pEngine->pAnimClipJump;
		
		// register Zelda (player) game object
		pEngine->gameObjectRegister.objects[pEngine->gameObjectRegister.numObjects] = &pEngine->zeldaGameObject;
		pEngine->gameObjectRegister.ids[pEngine->gameObjectRegister.numObjects] = pEngine->zeldaGameObject.name;
		pEngine->gameObjectRegister.numObjects += 1;
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
		
		for(uint32_t i=0; i<pEngine->pRig->numBones; ++i)
		{
			if(pEngine->pRig->boneNameHashes[i] == hair_r)
				pEngine->zeldaDangleHairRightIdx1 = i;
			else if(pEngine->pRig->boneNameHashes[i] == hair_r2)
				pEngine->zeldaDangleHairRightIdx2 = i;
			else if(pEngine->pRig->boneNameHashes[i] == hair_l)
				pEngine->zeldaDangleHairLeftIdx1 = i;
			else if(pEngine->pRig->boneNameHashes[i] == hair_l2)
				pEngine->zeldaDangleHairLeftIdx2 = i;
			else if(pEngine->pRig->boneNameHashes[i] == head)
				pEngine->zeldaHeadIdx = i;
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
	
	return true;
}

fs_variant_t fs_native_animate(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args)
{
	FUR_ASSERT(numArgs == 2);
	const fc_string_hash_t objectName = args[0].asStringHash;
	const fc_string_hash_t animName = args[1].asStringHash;
	
	// find game object
	fg_game_object_t* gameObj = NULL;
	if(objectName == SID("self"))
	{
		gameObj = ctx->self;
	}
	else
	{
		for(uint32_t i=0; i<ctx->gameObjectRegister->numObjects; ++i)
		{
			if(ctx->gameObjectRegister->ids[i] == objectName)
			{
				gameObj = ctx->gameObjectRegister->objects[i];
				break;
			}
		}
	}
	FUR_ASSERT(gameObj);
	
	fa_action_animate_t* animateSlot = fg_animate_action_slots_get_free(&gameObj->animateActionSlots);
	FUR_ASSERT(animateSlot);
	
	const fa_anim_clip_t* animClip = fg_animations_register_find_anim(ctx->allAnimations, animName);
	FUR_ASSERT(animClip);
	
	animateSlot->animation = animClip;
	animateSlot->forceLoop = true;
	animateSlot->reserved = true;
	
	fa_action_args_t animArgs = {};
	animArgs.fadeInSec = 0.3f;	// todo: take from script args
	fa_character_schedule_action_simple(gameObj->animCharacter, animateSlot, &animArgs);
	
	fs_variant_t result = {};
	return result;
};

fs_variant_t fs_native_wait_animate(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args)
{
	FUR_ASSERT(numArgs == 2);
	const fc_string_hash_t animName = args[1].asStringHash;
	
	const fa_anim_clip_t* animClip = fg_animations_register_find_anim(ctx->allAnimations, animName);
	FUR_ASSERT(animClip);
	
	ctx->waitSeconds = animClip->duration;
	
	return fs_native_animate(ctx, numArgs, args);
}

fs_variant_t fs_native_equip_item(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args)
{
	FUR_ASSERT(numArgs == 2);
	const fc_string_hash_t objectName = args[0].asStringHash;
	//const fc_string_hash_t itemName = args[1].asStringHash;
	
	// find game object
	fg_game_object_t* gameObj = NULL;
	if(objectName == SID("self"))
	{
		gameObj = ctx->self;
	}
	else
	{
		for(uint32_t i=0; i<ctx->gameObjectRegister->numObjects; ++i)
		{
			if(ctx->gameObjectRegister->ids[i] == objectName)
			{
				gameObj = ctx->gameObjectRegister->objects[i];
				break;
			}
		}
	}
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
	fg_game_object_t* gameObj = NULL;
	if(objectName == SID("self"))
	{
		gameObj = ctx->self;
	}
	else
	{
		for(uint32_t i=0; i<ctx->gameObjectRegister->numObjects; ++i)
		{
			if(ctx->gameObjectRegister->ids[i] == objectName)
			{
				gameObj = ctx->gameObjectRegister->objects[i];
				break;
			}
		}
	}
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
	
	static bool wasLeftThumbPressed = false;
	bool leftThumbPressed = false;
	static bool wasStartPressed = false;
	bool startPressed = false;
	
	static bool wasDpadUpPressed = false;
	bool dpadUpPressed = false;
	static bool wasDpadDownPressed = false;
	bool dpadDownPressed = false;
	
	g_devMenuOptionClick = false;
	
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
			pEngine->actionRotationLeftX = fm_snap_near_zero(inputEvents[i].value, 0.05f);
		}
		else if(inputEvents[i].eventID == Gamepad_leftAnalogX)
		{
			pEngine->actionMoveX = -fm_snap_near_zero(inputEvents[i].value, 0.05f);
		}
		else if(inputEvents[i].eventID == Gamepad_leftAnalogY)
		{
			pEngine->actionMoveY = fm_snap_near_zero(inputEvents[i].value, 0.05f);
		}
		else if(inputEvents[i].eventID == Gamepad_rightTrigger)
		{
			pEngine->actionZoomIn = fm_snap_near_zero(inputEvents[i].value, 0.05f);
		}
		else if(inputEvents[i].eventID == Gamepad_leftTrigger)
		{
			pEngine->actionZoomOut = fm_snap_near_zero(inputEvents[i].value, 0.05f);
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
	else // block all the actions when debug menu is enabled
	{
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

void fc_dev_menu_slow_time(FurGameEngine* pEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
	pEngine->debugIsSlowTime = !pEngine->debugIsSlowTime;
}

void fc_dev_menu_show_fps(FurGameEngine* pEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
	pEngine->debugShowFPS = !pEngine->debugShowFPS;
}

typedef struct fc_dev_menu_option_t
{
	const char* name;
	void (*func)(FurGameEngine* pEngine, fc_alloc_callbacks_t* pAllocCallbacks);
} fc_dev_menu_option_t;

void fc_draw_debug_menu(FurGameEngine* pEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
	const float color[4] = {0.8f, 0.8f, 0.8f, 1.0f};
	const float colorSelected[4] = FUR_COLOR_WHITE;
	
	if(g_drawDevMenu)
	{
		const float x = -1000.0f;
		const float y = 440;
		const float lineHeight = 28.0f;
		const float ident = 28.0f;
		
		fc_dev_menu_option_t options[] = {
			{"reload-scripts", fc_dev_menu_reload_scripts},
			{"show-player-anim-state", fc_dev_menu_show_player_anim_state},
			{"slow-time", fc_dev_menu_slow_time},
			{"fps", fc_dev_menu_show_fps}
		};
		
		const uint32_t numOptions = FUR_ARRAY_SIZE(options);
		
		if(g_devMenuOption < 0)
			g_devMenuOption = numOptions-1;
		else if(g_devMenuOption >= numOptions)
			g_devMenuOption = 0;
		
		const float bgColor[4] = {0.2f, 0.2f, 0.2f, 0.8f};
		
		fc_dbg_rect(x - 40.0f, y + 40.0f, 450.0f, 110.0f + 28.0f * numOptions, bgColor);
		
		fc_dbg_text(x, y, "Dev Menu:", color);
		
		for(uint32_t i=0; i<numOptions; ++i)
		{
			const bool isSelected = (i == g_devMenuOption);
			
			fc_dbg_text(x + ident, y - lineHeight * (1+i), options[i].name, isSelected ? colorSelected : color);
			
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
		
		fa_action_args_t args = {};
		args.fadeInSec = 0.0f;
		fa_character_schedule_action_simple(&pEngine->animCharacterZelda, &pEngine->actionIdle, &args);
		
		args.fadeInSec = 0.5f;
		args.layer = FA_CHAR_LAYER_UPPER_BODY;
		//fa_character_schedule_action_simple(&pEngine->animCharacterZelda, &pEngine->actionWeaponEquipped, &args);
	}
	
#if 0
	static float time = 0.0f;
	time += dt;
	static float interval = 1.0f;
	if(time < interval)
		pEngine->actionIdle.animation = pEngine->pAnimClipIdle;
	else if(time < 2.0f * interval)
		pEngine->actionIdle.animation = pEngine->pAnimClipIdle3;
	else if(time < 3.0f * interval)
		pEngine->actionIdle.animation = pEngine->pAnimClipIdle2;
	else if(time < 4.0f * interval)
		pEngine->actionIdle.animation = pEngine->pAnimClipIdle4;
	else
		time = 0.0f;
#endif
	
	static fc_string_hash_t prevState = SID("idle");
	
	static float waitSeconds = 0.0f;
	static uint32_t numSkipOps = 0;
	static fc_string_hash_t latentLambdaName = 0;
	static fc_string_hash_t latentEventName = 0;
	
	if(waitSeconds > 0.0f)
	{
		waitSeconds -= dt;
		
		if(waitSeconds <= 0.0f)
		{
			waitSeconds = 0.0f;
			
			fs_script_ctx_t scriptCtx = {};
			scriptCtx.allAnimations = &pEngine->gameAnimationsRegister;
			scriptCtx.gameObjectRegister = &pEngine->gameObjectRegister;
			scriptCtx.self = &pEngine->zeldaGameObject;
			scriptCtx.numSkipOps = numSkipOps;
			scriptCtx.state = latentLambdaName;
			scriptCtx.stateEventToCall = latentEventName;
			fs_execute_script(&pEngine->zeldaStateScript, &scriptCtx);
			
			if(scriptCtx.waitSeconds > 0.0f)
			{
				waitSeconds = scriptCtx.waitSeconds;
				numSkipOps = scriptCtx.numSkipOps;
				latentLambdaName = scriptCtx.state;
				latentEventName = scriptCtx.stateEventToCall;
			}
			
			if(scriptCtx.nextState != 0)
			{
				pEngine->zeldaGameObject.playerState = scriptCtx.nextState;
			}
		}
	}
	
	if(pEngine->zeldaGameObject.playerState == SID("idle"))
	{
		// on enter
		if(prevState != pEngine->zeldaGameObject.playerState)
		{
			prevState = pEngine->zeldaGameObject.playerState;
			
			fs_script_ctx_t scriptCtx = {};
			scriptCtx.allAnimations = &pEngine->gameAnimationsRegister;
			scriptCtx.gameObjectRegister = &pEngine->gameObjectRegister;
			scriptCtx.self = &pEngine->zeldaGameObject;
			scriptCtx.state = SID("idle");
			scriptCtx.stateEventToCall = SID("start");
			fs_execute_script(&pEngine->zeldaStateScript, &scriptCtx);
			
			if(scriptCtx.waitSeconds > 0.0f)
			{
				waitSeconds = scriptCtx.waitSeconds;
				numSkipOps = scriptCtx.numSkipOps;
				latentLambdaName = scriptCtx.state;
				latentEventName = scriptCtx.stateEventToCall;
			}
			
			if(scriptCtx.nextState != 0)
			{
				pEngine->zeldaGameObject.playerState = scriptCtx.nextState;
			}
		}
		
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
				args.layer = FA_CHAR_LAYER_UPPER_BODY;
				fa_character_schedule_none_action(&pEngine->animCharacterZelda, &args);
				
				pEngine->zeldaGameObject.playerWeaponEquipped = false;
			}
			else
			{
				fa_action_args_t args = {};
				args.fadeInSec = 0.5f;
				args.layer = FA_CHAR_LAYER_UPPER_BODY;
				fa_character_schedule_action_simple(&pEngine->animCharacterZelda, &pEngine->actionWeaponEquipped, &args);
				
				pEngine->zeldaGameObject.playerWeaponEquipped = true;
			}
		}
		
		// transitions
		const float moveX = pEngine->actionMoveX;
		const float moveY = pEngine->actionMoveY;
		
		if(fabsf(moveX) > 0.2f || fabsf(moveY) > 0.2f)
		{
			pEngine->zeldaGameObject.playerState = SID("start-loco");
		}
		
		if(pEngine->inActionPressed)
		{
			pEngine->zeldaGameObject.playerState = SID("jump");
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
			args.layer = FA_CHAR_LAYER_UPPER_BODY;
			fa_character_schedule_none_action(&pEngine->animCharacterZelda, &args);
			
			pEngine->zeldaGameObject.playerWindProtecting = false;
		}
		else
		{
			fa_action_args_t args = {};
			args.fadeInSec = 0.5f;
			args.layer = FA_CHAR_LAYER_UPPER_BODY;
			fa_character_schedule_action_simple(&pEngine->animCharacterZelda, &pEngine->actionWindProtect, &args);
			
			pEngine->zeldaGameObject.playerWindProtecting = true;
		}
	}
	
	if(pEngine->zeldaGameObject.playerState == SID("start-loco"))
	{
		// on enter
		if(prevState != pEngine->zeldaGameObject.playerState)
		{
			fa_action_args_t args = {};
			args.fadeInSec = 0.2f;
			
			pEngine->actionLocoStart.isFinished = false;
			
			fa_action_schedule_data_t data;
			data.userData = &pEngine->actionLocoStart;
			data.fnGetAnims = fa_action_player_loco_start_get_anims_func;
			data.fnUpdate = fa_action_player_loco_start_update;
			
			fa_character_schedule_action(&pEngine->animCharacterZelda, &data, &args);
			
			prevState = pEngine->zeldaGameObject.playerState;
		}
		
		// on update
		if(pEngine->actionLocoStart.isFinished)
		{
			pEngine->zeldaGameObject.playerState = SID("run");
		}
		
		// transitions
		const float moveX = pEngine->actionMoveX;
		const float moveY = pEngine->actionMoveY;
		if(fabsf(moveX) < 0.2f && fabsf(moveY) < 0.2f)
		{
			pEngine->zeldaGameObject.playerState = SID("stop-loco");
		}
		
		if(pEngine->inActionPressed)
		{
			pEngine->zeldaGameObject.playerState = SID("jump");
		}
	}
	
	if(pEngine->zeldaGameObject.playerState == SID("run"))
	{
		// on enter
		if(prevState != pEngine->zeldaGameObject.playerState)
		{
			fa_action_args_t args = {};
			args.fadeInSec = 0.5f;
			
			fa_action_schedule_data_t data;
			data.userData = &pEngine->actionPlayerLoco;
			data.fnGetAnims = fa_action_player_loco_get_anims_func;
			data.fnUpdate = fa_action_player_loco_update;
			
			fa_character_schedule_action(&pEngine->animCharacterZelda, &data, &args);
			
			prevState = pEngine->zeldaGameObject.playerState;
		}
		
		// on update
		const float moveX = pEngine->actionMoveX;
		const float moveY = pEngine->actionMoveY;
		
		// transitions
		if(fabsf(moveX) < 0.2f && fabsf(moveY) < 0.2f)
		{
			pEngine->zeldaGameObject.playerState = SID("stop-loco");
		}
		
		if(pEngine->inActionPressed)
		{
			pEngine->zeldaGameObject.playerState = SID("jump");
		}
	}
	
	if(pEngine->zeldaGameObject.playerState == SID("stop-loco"))
	{
		// on enter
		if(prevState != pEngine->zeldaGameObject.playerState)
		{
			fa_action_args_t args = {};
			args.fadeInSec = 0.5f;
			
			pEngine->actionLocoStop.isFinished = false;
			
			fa_action_schedule_data_t data;
			data.userData = &pEngine->actionLocoStop;
			data.fnGetAnims = fa_action_player_loco_start_get_anims_func;
			data.fnUpdate = fa_action_player_loco_start_update;
			
			fa_character_schedule_action(&pEngine->animCharacterZelda, &data, &args);
			
			prevState = pEngine->zeldaGameObject.playerState;
		}
		
		// transitions
		if(pEngine->actionLocoStop.isFinished)
		{
			pEngine->zeldaGameObject.playerState = SID("idle");
		}
	}
	
	if(pEngine->zeldaGameObject.playerState == SID("jump"))
	{
		// on enter
		if(prevState != pEngine->zeldaGameObject.playerState)
		{
			pEngine->actionJump.progress = 0.0f;
			pEngine->actionJump.jumpType = 0;
			
			fa_action_args_t args = {};
			args.fadeInSec = 0.3f;
			
			fa_action_schedule_data_t data;
			data.userData = &pEngine->actionJump;
			data.fnGetAnims = fa_action_player_jump_get_anims_func;
			data.fnUpdate = fa_action_player_jump_update;
			
			fa_character_schedule_action(&pEngine->animCharacterZelda, &data, &args);
			
			prevState = pEngine->zeldaGameObject.playerState;
		}
		
		// transitions
		if(pEngine->actionJump.progress >= 0.8f)
		{
			if(pEngine->actionJump.jumpType == 1)
			{
				pEngine->zeldaGameObject.playerState = SID("idle");
			}
			else
			{
				pEngine->zeldaGameObject.playerState = SID("run");
			}
		}
	}
}

void fg_animation_update(FurGameEngine* pEngine, float dt)
{
	// animation states update
	fa_character_animate_ctx_t animateCtx = {};
	animateCtx.dt = dt;
	animateCtx.scratchpadBuffer = pEngine->scratchpadBuffer;
	animateCtx.scratchpadBufferSize = pEngine->scratchpadBufferSize;
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

void furMainEngineGameUpdate(FurGameEngine* pEngine, float dt, fc_alloc_callbacks_t* pAllocCallbacks)
{
	// show debug FPS
	if(pEngine->debugShowFPS)
	{
		const float fps = 1.0f / dt;
		const float ms = dt * 1000.0f;
		
		char txt[50];
		sprintf(txt, "CPU: %1.1f fps (%1.1f ms)", fps, ms);
		
		const float green[4] = FUR_COLOR_GREEN;
		const float yellow[4] = FUR_COLOR_YELLOW;
		const float red[4] = FUR_COLOR_RED;
		fc_dbg_text(-1050, 600, txt, ms < 16 ? green : ms < 33 ? yellow : red);
	}
	
	// slow-time debug mode - this needs to be after debugShowFPS
	if(pEngine->debugIsSlowTime)
	{
		const float color[4] = FUR_COLOR_RED;
		fc_dbg_text(920, 600, "slow-time ON", color);
		dt *= 0.3f;
	}
	
	pEngine->globalTime += dt;
	pEngine->blendAlpha = fm_clamp(((sinf(pEngine->globalTime * 0.4f) + 1.0f) / 2.0f), 0.0f, 1.0f);
	
	// input
	fi_update_input_manager(pEngine->pInputManager, pEngine->globalTime);
	fg_input_actions_update(pEngine, dt);
	
	// debug/dev menu
	fc_draw_debug_menu(pEngine, pAllocCallbacks);
	
	// game
	fg_scripts_update(pEngine, dt);
	fg_gameplay_update(pEngine, dt);
	
	// animation
	fg_animation_update(pEngine, dt);
	
	// physics
	fp_physics_update_ctx_t physicsCtx = {};
	physicsCtx.dt = dt;
	
	// apply root motion from anim info to physics
	fm_vec4 playerDisplacement;
	playerDisplacement.x = pEngine->animCharacterZelda.animInfo.rootMotionDeltaX;
	playerDisplacement.y = pEngine->animCharacterZelda.animInfo.rootMotionDeltaY;
	playerDisplacement.z = 0.0f;
	playerDisplacement.w = 0.0f;
	physicsCtx.playerDisplacement = &playerDisplacement;
	fp_physics_update(pEngine->pPhysics, pEngine->pPhysicsScene, &physicsCtx);

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
	
	// draw dangle
#if 0
	{
		const float color[4] = FUR_COLOR_BLACK;
		const float colorV[4] = FUR_COLOR_RED;
		
		fm_mat4 attachmentMatrix;
		fm_mat4_identity(&attachmentMatrix);
		attachmentMatrix.x = {-1.0f, 0.0f, 0.0f, 0.0f};

		fm_mat4 matrices[40];
		fa_dangle_to_matrices_y_down(&pEngine->dangle, &attachmentMatrix, matrices);
		
		fc_dbg_mat4(&matrices[0]);
		
		for(uint32_t i=1; i<pEngine->dangle.numParaticles; ++i)
		{
			const fm_vec4 p0 = pEngine->dangle.x0[i-1];
			const fm_vec4 p1 = pEngine->dangle.x0[i];
			fm_vec4 v = pEngine->dangle.v[i];
			fm_vec4_mulf(&v, 0.1f, &v);
			fm_vec4_add(&v, &p1, &v);
			
			fc_dbg_line(&p0.x, &p1.x, color);
			//fc_dbg_line(&p1.x, &v.x, colorV);
			
			fc_dbg_mat4(&matrices[i]);
		}
		
		//fc_dbg_mat4(&pEngine->skinMatrices[pEngine->zeldaDangleHairLeftIdx1]);
		//fc_dbg_mat4(&pEngine->skinMatrices[pEngine->zeldaDangleHairLeftIdx2]);
		//fc_dbg_mat4(&pEngine->skinMatrices[pEngine->zeldaDangleHairRightIdx1]);
		//fc_dbg_mat4(&pEngine->skinMatrices[pEngine->zeldaDangleHairRightIdx2]);
	}
#endif
	
	// rendering
	{
		// get zelda position
		fm_xform playerLocator;
		fp_physics_player_info_t playerPhysics;
		playerPhysics.locator = &playerLocator;
		fp_physics_get_player_info(pEngine->pPhysics, pEngine->pPhysicsScene, &playerPhysics);
		
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
		{
			fg_camera_update_orbit_ctx cameraCtx = {};
			cameraCtx.dt = dt;
			cameraCtx.rotationX = pEngine->actionRotationLeftX;
			cameraCtx.zoom = pEngine->actionZoomOut - pEngine->actionZoomIn;
			
			fg_camera_update_orbit(pEngine->camera, &cameraCtx);
		}
		
		// player movement
		const float maxSpeed = 5.0f;
		
		fm_vec4 dirForward = {};
		fm_vec4 dirLeft = {};
		fg_camera_get_directions(pEngine->camera, &dirForward, &dirLeft);
		
		fm_vec4 playerMoveForward;
		fm_vec4_mulf(&dirForward, maxSpeed * pEngine->actionMoveY, &playerMoveForward);
		fm_vec4 playerMoveLeft;
		fm_vec4_mulf(&dirLeft, maxSpeed * pEngine->actionMoveX, &playerMoveLeft);
		fm_vec4 playerMove;
		fm_vec4_add(&playerMoveForward, &playerMoveLeft, &playerMove);
		
		pEngine->animCharacterZelda.animInfo.desiredMoveX = playerMove.x;
		pEngine->animCharacterZelda.animInfo.desiredMoveY = playerMove.y;
		
		fm_vec4_mulf(&playerMove, dt, &playerMove);
		pEngine->playerMove = playerMove;
		
		// adjust camera by player position
		fg_camera_adjust_by_player_movement(pEngine->camera, &zeldaMat);
		
		fm_mat4 cameraMatrix;
		fg_camera_view_matrix(pEngine->camera, &cameraMatrix);
		
		// update rendering
		fr_update_context_t ctx = {};
		ctx.dt = dt;
		ctx.cameraMatrix = &cameraMatrix;
		fr_update_renderer(pEngine->pRenderer, &ctx);
		
		if(!pEngine->zeldaGameObject.playerWeaponEquipped)
		{
			fm_mat4_identity(&slotMS);
			slotMS.w.x = 4.0f;
			slotMS.w.z = -4.0f;
		}
		
		// draw frame
		fr_draw_frame_context_t renderCtx = {};
		renderCtx.zeldaMatrix = &zeldaMat;
		renderCtx.skinMatrices = pEngine->skinMatrices;
		renderCtx.numSkinMatrices = pEngine->pRig->numBones;
		renderCtx.propMatrix = &slotMS;
		fr_draw_frame(pEngine->pRenderer, &renderCtx);
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
		
		furMainEngineGameUpdate(pEngine, dt, pAllocCallbacks);
	}
	
	fr_wait_for_device(pEngine->pRenderer);
}

bool furMainEngineTerminate(FurGameEngine* pEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
	// check for memory leaks
	//FUR_ASSERT(furValidateAllocatorGeneral(&pEngine->m_memory._defaultInternals));
	
	// release scripts
	fc_release_binary_buffer(&pEngine->zeldaStateScript, pAllocCallbacks);
	
	fa_dangle_release(&pEngine->dangle, pAllocCallbacks);
	fa_dangle_release(&pEngine->zeldaDangleHairLeft, pAllocCallbacks);
	fa_dangle_release(&pEngine->zeldaDangleHairRight, pAllocCallbacks);
	
	FUR_FREE(pEngine->animCharacterZelda.poseMS, pAllocCallbacks);
	
	FUR_FREE(pEngine->animCharacterZelda.layers[0].poseCache.tempPose.xforms, pAllocCallbacks);
	FUR_FREE(pEngine->animCharacterZelda.layers[0].poseCache.tempPose.weightsXforms, pAllocCallbacks);
	
	FUR_FREE(pEngine->animCharacterZelda.layers[1].poseCache.tempPose.xforms, pAllocCallbacks);
	FUR_FREE(pEngine->animCharacterZelda.layers[1].poseCache.tempPose.weightsXforms, pAllocCallbacks);
	
	FUR_FREE(pEngine->gameObjectRegister.objects, pAllocCallbacks);
	FUR_FREE(pEngine->gameObjectRegister.ids, pAllocCallbacks);
	
	FUR_FREE(pEngine->scratchpadBuffer, pAllocCallbacks);
	fa_rig_release(pEngine->pRig, pAllocCallbacks);
	fg_animations_register_release_anims(&pEngine->gameAnimationsRegister, pAllocCallbacks);
	
	fp_physics_scene_release(pEngine->pPhysics, pEngine->pPhysicsScene, pAllocCallbacks);
	
	fg_camera_release(pEngine->camera, pAllocCallbacks);
	
	fp_release_physics(pEngine->pPhysics, pAllocCallbacks);
	fr_release_renderer(pEngine->pRenderer, pAllocCallbacks);
	
	fi_input_manager_release(pEngine->pInputManager, pAllocCallbacks);
	
	fc_string_hash_register_release(pAllocCallbacks);
	
	fr_release_app(pEngine->pApp, pAllocCallbacks);
	
	free(pEngine);	// rest of the deallocations should happen through allocators
	
	return true;
}

int main()
{
	FurGameEngineDesc desc = {};
	
	desc.m_mainApp.m_width = 1600;
	desc.m_mainApp.m_height = 900;
	desc.m_mainApp.m_title = "Duena";
	
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
