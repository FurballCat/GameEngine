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

/**************** FURBALL CAT GAME ENGINE ****************/

typedef union fs_variant_t
{
	fc_string_hash_t asStringHash;
} fs_variant_t;

// ***** script native functions ***** //

fs_variant_t fs_native_animate(uint32_t numArgs, const fs_variant_t* args)
{
	FUR_ASSERT(numArgs == 2);
	const fc_string_hash_t objectName = args[0].asStringHash;
	const fc_string_hash_t animName = args[1].asStringHash;
	
	fs_variant_t result = {};
	return result;
};

// ***** scripts core ***** //

typedef fs_variant_t (*fs_script_navitve_func_t)(uint32_t numArgs, const fs_variant_t* args);

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

struct FurGameEngine
{
	struct fr_app_t* pApp;
	struct fr_renderer_t* pRenderer;
	fp_physics_t* pPhysics;
	
	std::chrono::system_clock::time_point prevTimePoint;
	float globalTime;
	float blendAlpha;
	
	fp_physics_scene_t* pPhysicsScene;
	
	// animation
	fa_rig_t* pRig;
	fa_anim_clip_t* pAnimClipIdle;
	fa_anim_clip_t* pAnimClipGesture;
	
	fm_mat4 skinMatrices[512];
	
	void* scratchpadBuffer;
	uint32_t scratchpadBufferSize;
	
	// scripts
	fs_script_data_t zeldaScript;
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
	
	return true;
}

void furMainEngineGameUpdate(FurGameEngine* pEngine, float dt)
{
	pEngine->globalTime += dt;
	pEngine->blendAlpha = fm_clamp(((sinf(pEngine->globalTime * 0.4f) + 1.0f) / 2.0f), 0.0f, 1.0f);
	
	uint32_t scratchpadBufferSizeUsed = 0;
	void* scratchpadBufferPtr = pEngine->scratchpadBuffer;
	
	const uint32_t poseStackSize = 128 * 1024;
	void* animPoseStackMemory = NULL;
	{
		uint32_t sizeRequired = poseStackSize;
		FUR_ASSERT(scratchpadBufferSizeUsed + sizeRequired < pEngine->scratchpadBufferSize);
		
		animPoseStackMemory = scratchpadBufferPtr;
		
		uint8_t* ptr = (uint8_t*)scratchpadBufferPtr;
		ptr += sizeRequired;
		scratchpadBufferPtr = (void*)ptr;
		scratchpadBufferSizeUsed += sizeRequired;
	}
	
	const uint32_t animCmdBufferSize = 32 * 1024;
	void* animCmdBufferMemory = NULL;
	{
		uint32_t sizeRequired = animCmdBufferSize;
		FUR_ASSERT(scratchpadBufferSizeUsed + sizeRequired < pEngine->scratchpadBufferSize);
		
		animCmdBufferMemory = scratchpadBufferPtr;
		
		uint8_t* ptr = (uint8_t*)scratchpadBufferPtr;
		ptr += sizeRequired;
		scratchpadBufferPtr = (void*)ptr;
		scratchpadBufferSizeUsed += sizeRequired;
	}
	
	// update animation
	{
		const uint32_t numBones = pEngine->pRig->numBones;
		const int16_t* parentIndices = pEngine->pRig->parents;
		
		fa_cmd_buffer_t animCmdBuffer = { animCmdBufferMemory, animCmdBufferSize };
		fa_cmd_buffer_recorder_t recorder = {};
		fa_cmd_buffer_recorder_init(&recorder, animCmdBuffer.data, animCmdBuffer.size);
		
		const float colorWhite[4] = FUR_COLOR_WHITE;
		
		// record anim commands
		{
			fa_cmd_begin(&recorder);
			
			// idle action
			const float animTimeIdle = fmodf(pEngine->globalTime, pEngine->pAnimClipIdle->duration);
			fa_cmd_anim_sample(&recorder, animTimeIdle, 0);
			fc_dbg_text(-500.0f, 40.0f, "zelda-idle-stand-01", colorWhite);
			
			// gesture action
			if(pEngine->blendAlpha > 0.001f)
			{
				const float animTimeGesture = fmodf(pEngine->globalTime, pEngine->pAnimClipGesture->duration);
				fa_cmd_anim_sample(&recorder, animTimeGesture, 1);
				fc_dbg_text(-500.0f, 20.0f, "zelda-idle-stand-look-around", colorWhite);
				
				// transition
				fa_cmd_blend2(&recorder, pEngine->blendAlpha);
				
				char txt[64];
				sprintf(txt, "blend: %1.2f", pEngine->blendAlpha);
				fc_dbg_text(-500.0f, 0.0f, txt, colorWhite);
			}
			
			fa_cmd_end(&recorder);
		}
		
		// evaluate anim commands
		fa_pose_stack_t poseStack = {};
		
		// init pose stack
		{
			fa_pose_stack_desc_t desc = {};
			
			desc.numBonesPerPose = pEngine->pRig->numBones;
			desc.numTracksPerPose = 0;
			desc.numMaxPoses = 4;
			
			fa_pose_stack_init(&poseStack, &desc, animPoseStackMemory, poseStackSize);
		}
		
		fm_mat4_t mat;
		
		const uint32_t numAnimClips = 2;
		const fa_anim_clip_t* animClips[numAnimClips] = {pEngine->pAnimClipIdle, pEngine->pAnimClipGesture};
		
		fa_cmd_context_t animCtx = {};
		animCtx.animClips = animClips;
		animCtx.numAnimClips = numAnimClips;
		animCtx.rig = pEngine->pRig;
		animCtx.poseStack = &poseStack;
		
		fa_cmd_buffer_evaluate(&animCmdBuffer, &animCtx);
		
		fa_pose_t outPose;
		fa_pose_stack_get(&poseStack, &outPose, 0);
		
		fa_pose_stack_push(&poseStack, 1);
		fa_pose_t modelPose;
		fa_pose_stack_get(&poseStack, &modelPose, 0);
		
		fa_pose_local_to_model(&modelPose, &outPose, parentIndices);
		
		for(uint32_t i=0; i<numBones; ++i)
		{
			fm_xform_to_mat4(&modelPose.xforms[i], &mat);
			//fr_dbg_draw_mat4(&mat);
			
			int16_t idxParent = parentIndices[i];
			if(idxParent >= 0)
			{
				//fc_dbg_line(&modelPose.xforms[i].pos.x, &modelPose.xforms[idxParent].pos.x, color);
			}
		}
		
		uint32_t numSkinMatrices = pEngine->pRig->numBones;
		
		for(uint32_t i=0; i<numSkinMatrices; ++i)
		{
			fm_xform_to_mat4(&modelPose.xforms[i], &pEngine->skinMatrices[i]);
		}
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
		
		fr_update_context_t ctx = {};
		ctx.dt = dt;
		
		fp_physics_update_ctx_t physicsCtx = {};
		physicsCtx.dt = dt;
		
		furMainEngineGameUpdate(pEngine, dt);
		
		fp_physics_update(pEngine->pPhysics, pEngine->pPhysicsScene, &physicsCtx);
		fr_update_renderer(pEngine->pRenderer, &ctx);
		
		fr_draw_frame_context_t renderCtx = {};
		renderCtx.skinMatrices = pEngine->skinMatrices;
		renderCtx.numSkinMatrices = pEngine->pRig->numBones;
		
		fr_draw_frame(pEngine->pRenderer, &renderCtx);
	}
	
	fr_wait_for_device(pEngine->pRenderer);
}

bool furMainEngineTerminate(FurGameEngine* pEngine, fc_alloc_callbacks_t* pAllocCallbacks)
{
	// check for memory leaks
	//FUR_ASSERT(furValidateAllocatorGeneral(&pEngine->m_memory._defaultInternals));
	
	FUR_FREE(pEngine->scratchpadBuffer, pAllocCallbacks);
	fa_rig_release(pEngine->pRig, pAllocCallbacks);
	fa_anim_clip_release(pEngine->pAnimClipIdle, pAllocCallbacks);
	fa_anim_clip_release(pEngine->pAnimClipGesture, pAllocCallbacks);
	
	fp_physics_scene_release(pEngine->pPhysics, pEngine->pPhysicsScene, pAllocCallbacks);
	
	fp_release_physics(pEngine->pPhysics, pAllocCallbacks);
	fr_release_renderer(pEngine->pRenderer, pAllocCallbacks);
	
	fs_script_release(&pEngine->zeldaScript, pAllocCallbacks);
	
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
