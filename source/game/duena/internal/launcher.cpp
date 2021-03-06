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

typedef struct fs_script_ctx_t
{
	fg_game_object_t* self;
	fg_game_object_register_t* gameObjectRegister;
} fs_script_ctx_t;

// todo: move it somewhere else
fs_variant_t fs_native_animate(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args);

typedef fs_variant_t (*fs_script_navitve_func_t)(fs_script_ctx_t* ctx, uint32_t numArgs, const fs_variant_t* args);

typedef struct fs_native_func_entry_t
{
	fc_string_hash_t name;
	fs_script_navitve_func_t func;
	uint32_t numArgs;
} fs_native_func_entry_t;

fs_native_func_entry_t g_nativeFuncLookUp[] = {
	{ SID("animate"), fs_native_animate, 2 },
	{ SID("__null"), NULL, 0 }
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

enum fs_script_parsing_stage_t
{
	SPS_NONE = 0,
	SPS_READING,
	SPS_END,
};

void fs_script_release(fs_script_data_t* script, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(script->ops, pAllocCallbacks);
	FUR_FREE(script->allArgs, pAllocCallbacks);
}

bool fs_script_data_load(const fi_depot_t* depot, const char* path, fs_script_data_t* pOutScript, fc_alloc_callbacks_t* pAllocCallbacks)
{
	const uint32_t maxPathLen = 256;
	char absolutePath[maxPathLen] = {};
	uint32_t depotPathLen = (uint32_t)strlen(depot->path);
	uint32_t pathLen = (uint32_t)strlen(path);
	
	FUR_ASSERT(depotPathLen + pathLen < maxPathLen);
	
	memcpy(absolutePath, depot->path, depotPathLen);
	memcpy(absolutePath + depotPathLen, path, pathLen);
	
	// todo: alloc proper number of ops and args
	const uint32_t tempMaxOpsAndArgs = 128;
	pOutScript->allArgs = FUR_ALLOC_ARRAY_AND_ZERO(fs_variant_t, tempMaxOpsAndArgs, 0, FC_MEMORY_SCOPE_SCRIPT, pAllocCallbacks);
	pOutScript->ops = FUR_ALLOC_ARRAY_AND_ZERO(fs_script_op_t, tempMaxOpsAndArgs, 0, FC_MEMORY_SCOPE_SCRIPT, pAllocCallbacks);
	
	fc_text_buffer_t buffer {};
	if(!fc_load_text_file_into_text_buffer(absolutePath, &buffer, pAllocCallbacks))
	{
		char txt[512];
		sprintf(txt, "Can't load script file %s", path);
		// todo: report error here
		return false;
	}
	
	enum fs_script_parsing_stage_t stage = SPS_NONE;
	uint32_t idxDataOp = 0;
	uint32_t idxDataArg = 0;
	
	fc_text_stream_ro_t stream {buffer.pData, buffer.pData + buffer.size};
	fc_text_parse_keyword(&stream, "(");
	while(!fc_text_parse_keyword(&stream, ")"))
	{
		fc_string_hash_t hash;
		bool parseRes = fc_text_parse_uint32(&stream, &hash);
		FUR_ASSERT(parseRes);
		
		if(hash == SID("__scriptbegin"))
		{
			fc_string_hash_t selfObjectName;
			parseRes = fc_text_parse_uint32(&stream, &selfObjectName);
			FUR_ASSERT(parseRes);
			stage = SPS_READING;
		}
		else if(stage == SPS_READING)
		{
			if(hash == SID("__funcbegin"))
			{
				fc_string_hash_t funcName;
				parseRes = fc_text_parse_uint32(&stream, &funcName);
				FUR_ASSERT(parseRes);
				
				const uint32_t numFuncs = FUR_ARRAY_SIZE(g_nativeFuncLookUp);
				
				uint32_t idxFunc = 0;
				for(idxFunc=0; idxFunc<numFuncs; ++idxFunc)
				{
					if(funcName == g_nativeFuncLookUp[idxFunc].name)
					{
						fs_script_op_t* op = &pOutScript->ops[idxDataOp];
						idxDataOp += 1;
						
						op->func = g_nativeFuncLookUp[idxFunc].func;
						
						const uint32_t numArgs = g_nativeFuncLookUp[idxFunc].numArgs;
						
						FUR_ASSERT(idxDataOp + 1 < tempMaxOpsAndArgs);	// we need +1 for NULL end op
						FUR_ASSERT(idxDataArg + numArgs < tempMaxOpsAndArgs);
						
						if(numArgs > 0)
						{
							op->args = &pOutScript->allArgs[idxDataArg];
							op->numArgs = numArgs;
						}
						else
						{
							op->args = NULL;
							op->numArgs = 0;
						}
						
						idxDataArg += numArgs;
						
						for(uint32_t idxArg=0; idxArg<numArgs; ++idxArg)
						{
							fc_string_hash_t argValue;
							parseRes = fc_text_parse_uint32(&stream, &argValue);
							FUR_ASSERT(parseRes);
							
							op->args[idxArg].asStringHash = argValue;
						}
						
						fc_string_hash_t fundEndValue = 0;
						parseRes = fc_text_parse_uint32(&stream, &fundEndValue);
						FUR_ASSERT(parseRes && fundEndValue == SID("__funcend"));
						break;
					}
				}
				FUR_ASSERT(idxFunc < numFuncs);	// if false, function was not found
			}
			else if(hash == SID("__scriptend"))
			{
				FUR_ASSERT(idxDataOp < tempMaxOpsAndArgs);
				pOutScript->ops[idxDataOp].func = NULL;
				pOutScript->ops[idxDataOp].args = NULL;
				pOutScript->ops[idxDataOp].numArgs = 0;
				stage = SPS_END;
				
				pOutScript->numOps = idxDataOp;
				pOutScript->numAllArgs = idxDataArg;
			}
			else
			{
				FUR_ASSERT(false);	// unknown operation
			}
		}
	}
	
	FUR_ASSERT(stage == SPS_END);
	
	fc_release_text_buffer(&buffer, pAllocCallbacks);
	
	return true;
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

typedef struct fg_game_object_t
{
	fc_string_hash_t id;	// name: like "zelda"
	fs_script_data_t* script;
	fs_script_state_t scriptState;
	
	// temp
	bool scriptTicked;
	
	fc_string_hash_t animToPlay;
	
} fg_game_object_t;

typedef enum fg_player_state_t
{
	FG_PLAYER_STATE_IDLE = 0,
	FG_PLAYER_STATE_START_LOCO,
	FG_PLAYER_STATE_LOCO_RUN,
	FG_PLAYER_STATE_STOP_LOCO,
	FG_PLAYER_STATE_JUMP,
	
	FG_PLAYER_STATE_NONE,
} fg_player_state_t;

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
	bool inActionEquipWeaponPressed;
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
	
	fg_player_state_t playerState;
	
	// skinning
	fm_mat4 skinMatrices[512];
	
	// update memory (scratchpad)
	void* scratchpadBuffer;
	uint32_t scratchpadBufferSize;
	
	// game objects
	fg_game_object_register_t gameObjectRegister;
	
	// scripts temp
	fs_script_data_t zeldaScript;
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
};

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

		{
			fi_import_anim_clip_ctx_t ctx = {};
			ctx.path = "assets/characters/zelda/animations/zelda-funny-poses.fbx";
			
			fi_import_anim_clip(&depot, &ctx, &pEngine->pAnimClipIdle, pAllocCallbacks);
		}
		
		{
			fi_import_anim_clip_ctx_t ctx = {};
			ctx.path = "assets/characters/zelda/animations/zelda-funny-pose-2.fbx";
			
			fi_import_anim_clip(&depot, &ctx, &pEngine->pAnimClipIdle2, pAllocCallbacks);
		}
		
		{
			fi_import_anim_clip_ctx_t ctx = {};
			ctx.path = "assets/characters/zelda/animations/zelda-funny-pose-3.fbx";
			
			fi_import_anim_clip(&depot, &ctx, &pEngine->pAnimClipIdle3, pAllocCallbacks);
		}
		
		{
			fi_import_anim_clip_ctx_t ctx = {};
			ctx.path = "assets/characters/zelda/animations/zelda-funny-pose-4.fbx";
			
			fi_import_anim_clip(&depot, &ctx, &pEngine->pAnimClipIdle4, pAllocCallbacks);
		}

		{
			fi_import_anim_clip_ctx_t ctx = {};
			ctx.path = "assets/characters/zelda/animations/zelda-loco-run-relaxed.fbx";
			
			fi_import_anim_clip(&depot, &ctx, &pEngine->pAnimClipRun, pAllocCallbacks);
		}
		
		{
			fi_import_anim_clip_ctx_t ctx = {};
			ctx.path = "assets/characters/zelda/animations/zelda-run-to-idle-sharp.fbx";
			
			fi_import_anim_clip(&depot, &ctx, &pEngine->pAnimClipRunToIdleSharp, pAllocCallbacks);
		}
		
		{
			fi_import_anim_clip_ctx_t ctx = {};
			ctx.path = "assets/characters/zelda/animations/zelda-loco-idle-to-run-0.fbx";
			ctx.extractRootMotion = true;
			
			fi_import_anim_clip(&depot, &ctx, &pEngine->pAnimClipIdleToRun0, pAllocCallbacks);
		}
		
		{
			fi_import_anim_clip_ctx_t ctx = {};
			ctx.path = "assets/characters/zelda/animations/zelda-loco-jump-in-place.fbx";
			
			fi_import_anim_clip(&depot, &ctx, &pEngine->pAnimClipJumpInPlace, pAllocCallbacks);
		}
		
		{
			fi_import_anim_clip_ctx_t ctx = {};
			ctx.path = "assets/characters/zelda/animations/zelda-loco-jump.fbx";
			ctx.extractRootMotion = true;
			
			fi_import_anim_clip(&depot, &ctx, &pEngine->pAnimClipJump, pAllocCallbacks);
		}
		
		{
			fi_import_anim_clip_ctx_t ctx = {};
			ctx.path = "assets/characters/zelda/animations/zelda-additive.fbx";
			
			fi_import_anim_clip(&depot, &ctx, &pEngine->pAnimClipAdditive, pAllocCallbacks);
		}
		
		{
			fi_import_anim_clip_ctx_t ctx = {};
			ctx.path = "assets/characters/zelda/animations/zelda-a-pose.fbx";
			
			fi_import_anim_clip(&depot, &ctx, &pEngine->pAnimClipAPose, pAllocCallbacks);
		}
		
		{
			fi_import_anim_clip_ctx_t ctx = {};
			ctx.path = "assets/characters/zelda/animations/zelda-upper-wind-protect.fbx";
			
			fi_import_anim_clip(&depot, &ctx, &pEngine->pAnimClipWindProtect, pAllocCallbacks);
		}
		
		{
			fi_import_anim_clip_ctx_t ctx = {};
			ctx.path = "assets/characters/zelda/animations/zelda-upper-hold-sword.fbx";
			
			fi_import_anim_clip(&depot, &ctx, &pEngine->pAnimClipHoldSword, pAllocCallbacks);
		}
		
		// import script data resources
		{
			fs_script_data_load(&depot, "scripts/zelda.txt", &pEngine->zeldaScript, pAllocCallbacks);
		}
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
		
		pEngine->actionIdle.animation = pEngine->pAnimClipIdle;
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
		
		pEngine->zeldaGameObject.id = SID_REG("zelda");
		pEngine->zeldaGameObject.script = &pEngine->zeldaScript;
		pEngine->zeldaGameObject.scriptState.idxOp = 0;
		pEngine->zeldaGameObject.scriptTicked = false;
		pEngine->zeldaGameObject.animToPlay = 0;
		
		// run
		pEngine->actionPlayerLoco.anims[FA_ACTION_PLAYER_LOCO_ANIM_IDLE] = pEngine->pAnimClipIdle;
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
		pEngine->gameObjectRegister.ids[pEngine->gameObjectRegister.numObjects] = pEngine->zeldaGameObject.id;
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
	
	gameObj->animToPlay = animName;
	
	fs_variant_t result = {};
	return result;
};

void fg_scripts_update(FurGameEngine* pEngine, float dt)
{
	for(uint32_t idxGO=0; idxGO<pEngine->gameObjectRegister.numObjects; ++idxGO)
	{
		fg_game_object_t* gameObj = pEngine->gameObjectRegister.objects[idxGO];
		
		// temp
		if(gameObj->scriptTicked)
		{
			break;
		}
		
		fs_script_ctx_t ctx = {};
		ctx.gameObjectRegister = &pEngine->gameObjectRegister;
		ctx.self = gameObj;
		
		uint32_t idxOp = gameObj->scriptState.idxOp;
		const fs_script_op_t* ops = gameObj->script->ops;
		while(ops[idxOp].func != NULL)
		{
			const fs_script_op_t op = gameObj->script->ops[idxOp];
			op.func(&ctx, op.numArgs, op.args);
			++idxOp;
		}
	}
}

void fg_input_actions_update(FurGameEngine* pEngine, float dt)
{
	bool actionPressed = false;
	static bool actionWasPressed = false;
	
	bool actionWeaponPressed = false;
	static bool actionWeaponWasPressed = false;
	
	fi_input_event_t inputEvents[10];
	const uint32_t numEventsCollected = fi_get_input_events(pEngine->pInputManager, inputEvents, 10, 0);
	for(uint32_t i=0; i<numEventsCollected; ++i)
	{
		if(inputEvents[i].eventID == Gamepad_faceButtonBottom)
		{
			actionPressed = true;
		}
		else if(inputEvents[i].eventID == Gamepad_faceButtonTop)
		{
			actionWeaponPressed = true;
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
	
	if(actionWasPressed != actionPressed)
	{
		pEngine->inActionPressed = actionPressed;
		actionWasPressed = actionPressed;
	}
	else
	{
		pEngine->inActionPressed = false;
	}
	
	if(actionWeaponWasPressed != actionWeaponPressed)
	{
		pEngine->inActionEquipWeaponPressed = actionWeaponPressed;
		actionWeaponWasPressed = actionWeaponPressed;
	}
	else
	{
		pEngine->inActionEquipWeaponPressed = false;
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
	
	if(pEngine->inActionEquipWeaponPressed)
		actionRandomizer2 += 1;
	
	// inital player state
	static bool isInit = true;
	if(isInit)
	{
		isInit = false;
		
		pEngine->playerState = FG_PLAYER_STATE_IDLE;
		
		fa_action_args_t args = {};
		args.fadeInSec = 0.0f;
		fa_character_schedule_action_simple(&pEngine->animCharacterZelda, &pEngine->actionIdle, &args);
		
		args.fadeInSec = 0.5f;
		args.layer = FA_CHAR_LAYER_UPPER_BODY;
		//fa_character_schedule_action_simple(&pEngine->animCharacterZelda, &pEngine->actionWeaponEquipped, &args);
	}
	
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
	
	static fg_player_state_t prevState = FG_PLAYER_STATE_IDLE;
	
	if(pEngine->playerState == FG_PLAYER_STATE_IDLE)
	{
		// on enter
		if(prevState != pEngine->playerState)
		{
			fa_action_args_t args = {};
			args.fadeInSec = 0.3f;
			fa_character_schedule_action_simple(&pEngine->animCharacterZelda, &pEngine->actionIdle, &args);
			
			prevState = pEngine->playerState;
		}
		
		// on update
		const float moveX = pEngine->actionMoveX;
		const float moveY = pEngine->actionMoveY;
		
		if(fabsf(moveX) > 0.2f || fabsf(moveY) > 0.2f)
		{
			pEngine->playerState = FG_PLAYER_STATE_START_LOCO;
		}
		
		if(pEngine->inActionPressed)
		{
			pEngine->playerState = FG_PLAYER_STATE_JUMP;
		}
	}
	
	if(pEngine->playerState == FG_PLAYER_STATE_START_LOCO)
	{
		// on enter
		if(prevState != pEngine->playerState)
		{
			fa_action_args_t args = {};
			args.fadeInSec = 0.2f;
			
			pEngine->actionLocoStart.isFinished = false;
			
			fa_action_schedule_data_t data;
			data.userData = &pEngine->actionLocoStart;
			data.fnGetAnims = fa_action_player_loco_start_get_anims_func;
			data.fnUpdate = fa_action_player_loco_start_update;
			
			fa_character_schedule_action(&pEngine->animCharacterZelda, &data, &args);
			
			prevState = pEngine->playerState;
		}
		
		// on update
		if(pEngine->actionLocoStart.isFinished)
		{
			pEngine->playerState = FG_PLAYER_STATE_LOCO_RUN;
		}
		
		const float moveX = pEngine->actionMoveX;
		const float moveY = pEngine->actionMoveY;
		if(fabsf(moveX) < 0.2f && fabsf(moveY) < 0.2f)
		{
			pEngine->playerState = FG_PLAYER_STATE_STOP_LOCO;
		}
		
		if(pEngine->inActionPressed)
		{
			pEngine->playerState = FG_PLAYER_STATE_JUMP;
		}
	}
	
	if(pEngine->playerState == FG_PLAYER_STATE_LOCO_RUN)
	{
		// on enter
		if(prevState != pEngine->playerState)
		{
			fa_action_args_t args = {};
			args.fadeInSec = 0.5f;
			
			fa_action_schedule_data_t data;
			data.userData = &pEngine->actionPlayerLoco;
			data.fnGetAnims = fa_action_player_loco_get_anims_func;
			data.fnUpdate = fa_action_player_loco_update;
			
			fa_character_schedule_action(&pEngine->animCharacterZelda, &data, &args);
			
			prevState = pEngine->playerState;
		}
		
		// on update
		const float moveX = pEngine->actionMoveX;
		const float moveY = pEngine->actionMoveY;
		
		if(fabsf(moveX) < 0.2f && fabsf(moveY) < 0.2f)
		{
			pEngine->playerState = FG_PLAYER_STATE_STOP_LOCO;
		}
		
		if(pEngine->inActionPressed)
		{
			pEngine->playerState = FG_PLAYER_STATE_JUMP;
		}
	}
	
	if(pEngine->playerState == FG_PLAYER_STATE_STOP_LOCO)
	{
		// on enter
		if(prevState != pEngine->playerState)
		{
			fa_action_args_t args = {};
			args.fadeInSec = 0.5f;
			
			pEngine->actionLocoStop.isFinished = false;
			
			fa_action_schedule_data_t data;
			data.userData = &pEngine->actionLocoStop;
			data.fnGetAnims = fa_action_player_loco_start_get_anims_func;
			data.fnUpdate = fa_action_player_loco_start_update;
			
			fa_character_schedule_action(&pEngine->animCharacterZelda, &data, &args);
			
			prevState = pEngine->playerState;
		}
		
		if(pEngine->actionLocoStop.isFinished)
		{
			pEngine->playerState = FG_PLAYER_STATE_IDLE;
		}
	}
	
	if(pEngine->playerState == FG_PLAYER_STATE_JUMP)
	{
		// on enter
		if(prevState != pEngine->playerState)
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
			
			prevState = pEngine->playerState;
		}
		
		if(pEngine->actionJump.progress >= 0.8f)
		{
			if(pEngine->actionJump.jumpType == 1)
			{
				pEngine->playerState = FG_PLAYER_STATE_IDLE;
			}
			else
			{
				pEngine->playerState = FG_PLAYER_STATE_LOCO_RUN;
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

void furMainEngineGameUpdate(FurGameEngine* pEngine, float dt)
{
	pEngine->globalTime += dt;
	pEngine->blendAlpha = fm_clamp(((sinf(pEngine->globalTime * 0.4f) + 1.0f) / 2.0f), 0.0f, 1.0f);
	
	// input
	fi_update_input_manager(pEngine->pInputManager, pEngine->globalTime);
	fg_input_actions_update(pEngine, dt);
	
	// game
	fg_scripts_update(pEngine, dt);
	fg_gameplay_update(pEngine, dt);
	
	// animation
	fg_animation_update(pEngine, dt);
	
	// physics
	fp_physics_update_ctx_t physicsCtx = {};
	physicsCtx.dt = dt;
	
	fm_vec4 playerDisplacement;
	playerDisplacement.x = pEngine->animCharacterZelda.animInfo.rootMotionDeltaX;
	playerDisplacement.y = pEngine->animCharacterZelda.animInfo.rootMotionDeltaY;
	playerDisplacement.z = 0.0f;
	playerDisplacement.w = 0.0f;
	physicsCtx.playerDisplacement = &playerDisplacement;
	fp_physics_update(pEngine->pPhysics, pEngine->pPhysicsScene, &physicsCtx);
	
	{
		fa_dangle_sim_ctx simCtx {};
		simCtx.dt = dt;
		fa_dangle_simulate(&simCtx, &pEngine->dangle);
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
		
		//if(!pEngine->actionTest2.equipWeapon)
#if 1
		{
			fm_mat4_identity(&slotMS);
			slotMS.w.x = 4.0f;
		}
#endif
		
		// draw frame
		fr_draw_frame_context_t renderCtx = {};
		renderCtx.zeldaMatrix = &zeldaMat;
		renderCtx.skinMatrices = pEngine->skinMatrices;
		renderCtx.numSkinMatrices = pEngine->pRig->numBones;
		renderCtx.propMatrix = &slotMS;
		fr_draw_frame(pEngine->pRenderer, &renderCtx);
	}
}

void furMainEngineLoop(FurGameEngine* pEngine)
{
	pEngine->prevTimePoint = std::chrono::system_clock::now();
	
	while(fr_update_app(pEngine->pApp))
	{
		std::chrono::system_clock::time_point timeNow = std::chrono::system_clock::now();
		std::chrono::duration<float> dtOrig = timeNow - pEngine->prevTimePoint;
		pEngine->prevTimePoint = timeNow;
		
		const float dt = dtOrig.count();
		
		furMainEngineGameUpdate(pEngine, dt);
	}
	
	fr_wait_for_device(pEngine->pRenderer);
}

bool furMainEngineTerminate(FurGameEngine* pEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
	// check for memory leaks
	//FUR_ASSERT(furValidateAllocatorGeneral(&pEngine->m_memory._defaultInternals));
	
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
	fa_anim_clip_release(pEngine->pAnimClipIdle, pAllocCallbacks);
	fa_anim_clip_release(pEngine->pAnimClipIdle2, pAllocCallbacks);
	fa_anim_clip_release(pEngine->pAnimClipIdle3, pAllocCallbacks);
	fa_anim_clip_release(pEngine->pAnimClipIdle4, pAllocCallbacks);
	fa_anim_clip_release(pEngine->pAnimClipRun, pAllocCallbacks);
	fa_anim_clip_release(pEngine->pAnimClipRunToIdleSharp, pAllocCallbacks);
	fa_anim_clip_release(pEngine->pAnimClipIdleToRun0, pAllocCallbacks);
	fa_anim_clip_release(pEngine->pAnimClipAdditive, pAllocCallbacks);
	fa_anim_clip_release(pEngine->pAnimClipAPose, pAllocCallbacks);
	fa_anim_clip_release(pEngine->pAnimClipWindProtect, pAllocCallbacks);
	fa_anim_clip_release(pEngine->pAnimClipHoldSword, pAllocCallbacks);
	fa_anim_clip_release(pEngine->pAnimClipJumpInPlace, pAllocCallbacks);
	fa_anim_clip_release(pEngine->pAnimClipJump, pAllocCallbacks);
	
	fp_physics_scene_release(pEngine->pPhysics, pEngine->pPhysicsScene, pAllocCallbacks);
	
	fg_camera_release(pEngine->camera, pAllocCallbacks);
	
	fp_release_physics(pEngine->pPhysics, pAllocCallbacks);
	fr_release_renderer(pEngine->pRenderer, pAllocCallbacks);
	
	fi_input_manager_release(pEngine->pInputManager, pAllocCallbacks);
	
	fs_script_release(&pEngine->zeldaScript, pAllocCallbacks);
	
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
	furMainEngineLoop(pEngine);
	
	// terminate most basic engine components
	bool result = furMainEngineTerminate(pEngine, pAllocCallbacks);
	if(!result)
	{
		return 1;
	}
	
	return 0;
}
