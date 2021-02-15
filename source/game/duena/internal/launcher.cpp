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
	fa_anim_clip_t* pAnimClipGesture;
	
	// input actions
	bool inActionPressed;
	float actionRotationLeftX;
	float actionZoomIn;
	float actionZoomOut;
	
	// gameplay animation states
	fa_character_t animCharacterZelda;
	fa_action_animate_t animSimpleAction;
	fa_action_animate_t animSimpleAction2;
	fa_action_animate_t animSimpleAction3;
	
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
	
	// init scratchpad buffer
	pEngine->scratchpadBufferSize = 256 * 1024;
	pEngine->scratchpadBuffer = FUR_ALLOC_AND_ZERO(pEngine->scratchpadBufferSize, 16, FC_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
	
	// load resources
	{
		const char* depotPath = "../../../../../";

		fi_depot_t depot;
		depot.path = depotPath;

		const char* characterRigPath = "assets/characters/zelda/mesh/zelda_rig.fbx";
		const char* anim_zelda_stand = "assets/characters/zelda/animations/zelda-idle-stand-01.fbx";
		const char* anim_zelda_look = "assets/characters/zelda/animations/zelda-idle-stand-look-around.fbx";

		// import animation resources
		{
			fi_import_rig_ctx_t ctx = {};
			ctx.path = characterRigPath;
			
			fi_import_rig(&depot, &ctx, &pEngine->pRig, pAllocCallbacks);
		}

		{
			fi_import_anim_clip_ctx_t ctx = {};
			ctx.path = anim_zelda_stand;
			
			fi_import_anim_clip(&depot, &ctx, &pEngine->pAnimClipIdle, pAllocCallbacks);
		}

		{
			fi_import_anim_clip_ctx_t ctx = {};
			ctx.path = anim_zelda_look;
			
			fi_import_anim_clip(&depot, &ctx, &pEngine->pAnimClipGesture, pAllocCallbacks);
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
		pEngine->animCharacterZelda.poseCache.tempPose.xforms = FUR_ALLOC_ARRAY_AND_ZERO(fm_xform, pEngine->animCharacterZelda.rig->numBones, 16, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
		pEngine->animCharacterZelda.poseCache.tempPose.weightsXforms = FUR_ALLOC_ARRAY_AND_ZERO(uint8_t, pEngine->animCharacterZelda.rig->numBones, 16, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
		pEngine->animCharacterZelda.poseCache.tempPose.numXforms = pEngine->animCharacterZelda.rig->numBones;
		
		pEngine->animSimpleAction.animation = pEngine->pAnimClipGesture;
		pEngine->animSimpleAction.forceLoop = true;
		
		fa_action_args_t args = {};
		args.fadeInSec = 0.3f;
		fa_character_schedule_action_simple(&pEngine->animCharacterZelda, &pEngine->animSimpleAction, &args, (uint64_t)(pEngine->globalTime * 1000000));
		
		pEngine->zeldaGameObject.id = SID_REG("zelda");
		pEngine->zeldaGameObject.script = &pEngine->zeldaScript;
		pEngine->zeldaGameObject.scriptState.idxOp = 0;
		pEngine->zeldaGameObject.scriptTicked = false;
		pEngine->zeldaGameObject.animToPlay = 0;
		
		// register Zelda (player) game object
		pEngine->gameObjectRegister.objects[pEngine->gameObjectRegister.numObjects] = &pEngine->zeldaGameObject;
		pEngine->gameObjectRegister.ids[pEngine->gameObjectRegister.numObjects] = pEngine->zeldaGameObject.id;
		pEngine->gameObjectRegister.numObjects += 1;
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
	
	fi_input_event_t inputEvents[10];
	const uint32_t numEventsCollected = fi_get_input_events(pEngine->pInputManager, inputEvents, 10, 0);
	for(uint32_t i=0; i<numEventsCollected; ++i)
	{
		if(inputEvents[i].eventID == Gamepad_faceButtonLeft)
		{
			actionPressed = true;
		}
		else if(inputEvents[i].eventID == Gamepad_rightAnalogX)
		{
			pEngine->actionRotationLeftX = fm_snap_near_zero(inputEvents[i].value, 0.05f);
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
}

void fg_gameplay_update(FurGameEngine* pEngine, float dt)
{
	uint64_t globalTime = (uint64_t)(pEngine->globalTime * 1000000);
	
	static uint32_t actionRandomizer = 0;
	
	if(pEngine->inActionPressed)
		actionRandomizer += 1;
	
	if(pEngine->inActionPressed && ((actionRandomizer % 2) == 1))
	{
		pEngine->animSimpleAction2.animation = pEngine->pAnimClipIdle;
		pEngine->animSimpleAction2.forceLoop = true;
		
		fa_action_args_t args = {};
		args.fadeInSec = 0.5f;
		fa_character_schedule_action_simple(&pEngine->animCharacterZelda, &pEngine->animSimpleAction2, &args, globalTime);
	}
	
	if(pEngine->inActionPressed &&((actionRandomizer % 2) == 0))
	{
		pEngine->animSimpleAction3.animation = pEngine->pAnimClipGesture;
		pEngine->animSimpleAction3.forceLoop = true;
		
		fa_action_args_t args = {};
		args.fadeInSec = 0.5f;
		fa_character_schedule_action_simple(&pEngine->animCharacterZelda, &pEngine->animSimpleAction3, &args, globalTime);
	}
}

void fg_animation_update(FurGameEngine* pEngine, float dt)
{
	// animation states update
	fa_character_animate_ctx_t animateCtx = {};
	animateCtx.dt = dt;
	animateCtx.globalTime = (uint64_t)(pEngine->globalTime * 1000000.0);
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
	fp_physics_update(pEngine->pPhysics, pEngine->pPhysicsScene, &physicsCtx);
	
	// rendering
	{
		const fm_vec4 g_eye = {0, -3, 1.4, 0};
		const fm_vec4 g_at = {0, 0, 1, 0};
		const fm_vec4 g_up = {0, 0, 1, 0};
		
		fr_update_context_t ctx = {};
		ctx.dt = dt;
		ctx.camera.eye[0] = g_eye.x;
		ctx.camera.eye[1] = g_eye.y;
		ctx.camera.eye[2] = g_eye.z;
		ctx.camera.at[0] = g_at.x;
		ctx.camera.at[1] = g_at.y;
		ctx.camera.at[2] = g_at.z;
		ctx.camera.up[0] = g_up.x;
		ctx.camera.up[1] = g_up.y;
		ctx.camera.up[2] = g_up.z;
		fr_update_renderer(pEngine->pRenderer, &ctx);
		
		fr_draw_frame_context_t renderCtx = {};
		renderCtx.skinMatrices = pEngine->skinMatrices;
		renderCtx.numSkinMatrices = pEngine->pRig->numBones;
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
	
	FUR_FREE(pEngine->animCharacterZelda.poseMS, pAllocCallbacks);
	FUR_FREE(pEngine->animCharacterZelda.poseCache.tempPose.xforms, pAllocCallbacks);
	FUR_FREE(pEngine->animCharacterZelda.poseCache.tempPose.weightsXforms, pAllocCallbacks);
	
	FUR_FREE(pEngine->gameObjectRegister.objects, pAllocCallbacks);
	FUR_FREE(pEngine->gameObjectRegister.ids, pAllocCallbacks);
	
	FUR_FREE(pEngine->scratchpadBuffer, pAllocCallbacks);
	fa_rig_release(pEngine->pRig, pAllocCallbacks);
	fa_anim_clip_release(pEngine->pAnimClipIdle, pAllocCallbacks);
	fa_anim_clip_release(pEngine->pAnimClipGesture, pAllocCallbacks);
	
	fp_physics_scene_release(pEngine->pPhysics, pEngine->pPhysicsScene, pAllocCallbacks);
	
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
