/* Copyright (c) 2016-2022 Furball Cat */

#include "animCommands.h"
#include "animClip.h"
#include "pose.h"
#include "poseStack.h"
#include "rig.h"
#include "ccore/public.h"
#include "cmath/public.h"
#include <string.h>
#include <stdio.h>

void fa_cmd_buffer_evaluate(const fa_cmd_buffer_t* buffer, fa_cmd_context_t* ctx)
{
	fa_cmd_status_t status = FA_CMD_STATUS_OK;
	uint32_t dbgIndexCommand = 0;
	const void* cmdPointer = buffer->data;
	while(status == FA_CMD_STATUS_OK)
	{
		const fa_cmd_func_t* func = (const fa_cmd_func_t*)cmdPointer;
		const uint32_t* dataSize = (const uint32_t*)(cmdPointer + sizeof(fa_cmd_func_t));
		const void* cmdData = NULL;
		if(*dataSize > 0)
		{
			 cmdData = cmdPointer + sizeof(fa_cmd_func_t) + sizeof(uint32_t);
		}
		
		status = (*func)(ctx, cmdData);
		const uint32_t totalCommandSize = sizeof(fa_cmd_func_t) + sizeof(uint32_t) + (*dataSize);
		cmdPointer += totalCommandSize;
		++dbgIndexCommand;
		
		if(ctx->debug)
		{
			ctx->debug->cmdDrawCursorVerticalPos += 14;
		}
	}
}

// this is the cmd memory formatting function
void fa_cmd_buffer_write(fa_cmd_buffer_recorder_t* recorder, fa_cmd_func_t func, const void* data, uint32_t dataSize)
{
	const uint32_t sizeRequired = sizeof(fa_cmd_func_t) + sizeof(uint32_t) + dataSize;
	FUR_ASSERT(sizeRequired <= recorder->sizeLeft);

	// command function pointer
	fa_cmd_func_t* funcPtr = (fa_cmd_func_t*)recorder->currPointer;
	*funcPtr = func;
	
	// command data size (can be 0)
	uint32_t* dataSizePtr = (uint32_t*)(recorder->currPointer + sizeof(fa_cmd_func_t));
	*dataSizePtr = dataSize;
	
	// command data
	if(data)
	{
		void* dataPtr = recorder->currPointer + sizeof(fa_cmd_func_t) + sizeof(uint32_t);
		memcpy(dataPtr, data, dataSize);
	}
	
	recorder->currPointer += sizeRequired;
	recorder->sizeLeft -= sizeRequired;
	recorder->sizeRecorded += sizeRequired;
}

void fa_cmd_buffer_recorder_init(fa_cmd_buffer_recorder_t* recorder, void* outData, uint32_t maxSize)
{
	recorder->currPointer = outData;
	recorder->sizeLeft = maxSize;
	recorder->sizeRecorded = 0;
	recorder->poseStackSizeTracking = 0;
}

// begin command
void fa_cmd_begin(fa_cmd_buffer_recorder_t* recorder, uint32_t poseStackInitialSize)
{
	recorder->poseStackSizeTracking = poseStackInitialSize;
	recorder->poseStackInitialSize = poseStackInitialSize;
}

// end command
fa_cmd_status_t fa_cmd_impl_end(fa_cmd_context_t* ctx, const void* cmdData)
{
	return FA_CMD_STATUS_STOP;
}

void fa_cmd_end(fa_cmd_buffer_recorder_t* recorder)
{
	fa_cmd_buffer_write(recorder, fa_cmd_impl_end, NULL, 0); // writing a null cmd, similar to c-string having null character at the end
	
	FUR_ASSERT(recorder->poseStackSizeTracking >= 1);	// at the end of command buffer, we expect the post stack to have +1 pose
}

#define FA_DBG_TEXT_X -900.0f
#define FA_DBG_TEXT_Y(_pos) 150.0f - 2.0f * _pos
#define FA_DBG_COLOR FUR_COLOR_GREEN

// set reference pose command
fa_cmd_status_t fa_cmd_impl_ref_pose(fa_cmd_context_t* ctx, const void* cmdData)
{
	fa_pose_stack_push(ctx->poseStack, 1);
	
	fa_pose_t pose;
	fa_pose_stack_get(ctx->poseStack, &pose, 0);
	
	fa_pose_set_reference(ctx->rig, &pose, ctx->mask);
	
	if(ctx->debug)
	{
		const uint32_t pos = ctx->debug->cmdDrawCursorVerticalPos;
		const float color[4] = FA_DBG_COLOR;
		fc_dbg_text(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), "ref_pose", color);
	}
	
	return FA_CMD_STATUS_OK;
}

void fa_cmd_ref_pose(fa_cmd_buffer_recorder_t* recorder)
{
	fa_cmd_buffer_write(recorder, fa_cmd_impl_ref_pose, NULL, 0);
	recorder->poseStackSizeTracking += 1;
}

// set identity pose command
fa_cmd_status_t fa_cmd_impl_identity(fa_cmd_context_t* ctx, const void* cmdData)
{
	fa_pose_stack_push(ctx->poseStack, 1);
	
	fa_pose_t pose;
	fa_pose_stack_get(ctx->poseStack, &pose, 0);
	
	fa_pose_set_identity(&pose, ctx->mask);
	
	if(ctx->debug)
	{
		const uint32_t pos = ctx->debug->cmdDrawCursorVerticalPos;
		const float color[4] = FA_DBG_COLOR;
		fc_dbg_text(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), "identity", color);
	}
	
	return FA_CMD_STATUS_OK;
}

void fa_cmd_identity(fa_cmd_buffer_recorder_t* recorder)
{
	fa_cmd_buffer_write(recorder, fa_cmd_impl_identity, NULL, 0);
	recorder->poseStackSizeTracking += 1;
}

// sample animation command
typedef struct fa_cmd_anim_sample_data_t
{
	float time;
	uint16_t animClipId;
	bool asAdditive;
} fa_cmd_anim_sample_data_t;

fa_cmd_status_t fa_cmd_impl_anim_sample(fa_cmd_context_t* ctx, const void* cmdData)
{
	const fa_cmd_anim_sample_data_t* data = (fa_cmd_anim_sample_data_t*)cmdData;
	FUR_ASSERT(data->animClipId < ctx->numAnimClips);
	
	const fa_anim_clip_t* clip = ctx->animClips[data->animClipId];
	
	fa_pose_stack_push(ctx->poseStack, 1);
	fa_pose_t pose;
	fa_pose_stack_get(ctx->poseStack, &pose, 0);
	
	FUR_ASSERT(pose.numXforms == ctx->rig->numBones);
	
	fa_anim_clip_sample(clip, data->time, data->asAdditive, &pose, ctx->mask);
	
	if(ctx->debug)
	{
		const uint32_t pos = ctx->debug->cmdDrawCursorVerticalPos;
		const float color[4] = FA_DBG_COLOR;
		char txt[256];
		sprintf(txt, "anim_sample %s t=%1.2f", fc_string_hash_as_cstr_debug(clip->name), data->time);
		fc_dbg_text(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), txt, color);
	}
	
	// todo: check if required
	// just in case - reset loco joint
	const int16_t idxLocoJoint = ctx->rig->idxLocoJoint;
	if(idxLocoJoint != -1)
	{
		FUR_ASSERT(idxLocoJoint < ctx->rig->numBones);
		fm_quat_identity(&pose.xforms[idxLocoJoint].rot);
		fm_vec4_zeros(&pose.xforms[idxLocoJoint].pos);
	}
	
	return FA_CMD_STATUS_OK;
}

void fa_cmd_anim_sample(fa_cmd_buffer_recorder_t* recorder, float time, uint16_t animClipId)
{
	fa_cmd_anim_sample_data_t data = { time, animClipId, false };
	fa_cmd_buffer_write(recorder, fa_cmd_impl_anim_sample, &data, sizeof(fa_cmd_anim_sample_data_t));
	recorder->poseStackSizeTracking += 1;
}

void fa_cmd_anim_sample_additive(fa_cmd_buffer_recorder_t* recorder, float time, uint16_t animClipId)
{
	fa_cmd_anim_sample_data_t data = { time, animClipId, true };
	fa_cmd_buffer_write(recorder, fa_cmd_impl_anim_sample, &data, sizeof(fa_cmd_anim_sample_data_t));
	recorder->poseStackSizeTracking += 1;
}

// sample animation with locomotion command
typedef struct fa_cmd_anim_sample_with_locomotion_data_t
{
	float time;
	uint16_t animClipId;
	bool asAdditive;
	bool resetLoco;
	int32_t loops;
	float* prevLocoPos;	// fm_vec4
	float* prevLocoRot;	// fm_vec4
} fa_cmd_anim_sample_with_locomotion_data_t;

fa_cmd_status_t fa_cmd_impl_anim_sample_with_locomotion(fa_cmd_context_t* ctx, const void* cmdData)
{
	const fa_cmd_anim_sample_with_locomotion_data_t* data = (fa_cmd_anim_sample_with_locomotion_data_t*)cmdData;
	FUR_ASSERT(data->animClipId < ctx->numAnimClips);
	
	// get animation clip
	const fa_anim_clip_t* clip = ctx->animClips[data->animClipId];
	
	// push new pose onto stack
	fa_pose_stack_push(ctx->poseStack, 1);
	fa_pose_t pose;
	fa_pose_stack_get(ctx->poseStack, &pose, 0);
	
	FUR_ASSERT(pose.numXforms == ctx->rig->numBones);
	
	// sample the animation
	fa_anim_clip_sample(clip, data->time, data->asAdditive, &pose, ctx->mask);
	
	// optionally draw debug info
	if(ctx->debug)
	{
		const uint32_t pos = ctx->debug->cmdDrawCursorVerticalPos;
		const float color[4] = FA_DBG_COLOR;
		char txt[256];
		sprintf(txt, "anim_sample_with_locomotion %s t=%1.2f", fc_string_hash_as_cstr_debug(clip->name), data->time);
		fc_dbg_text(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), txt, color);
	}
	
	// process locomotion
	FUR_ASSERT(data->prevLocoPos);
	FUR_ASSERT(data->prevLocoRot);
	FUR_ASSERT(ctx->rig->idxLocoJoint != -1);
	
	const int16_t idxLocoJoint = ctx->rig->idxLocoJoint;
	FUR_ASSERT(idxLocoJoint < ctx->rig->numBones);
	
	fm_xform currLocoXform = pose.xforms[idxLocoJoint];
	
	// todo: remove, this is sanitising old animation data
	if(currLocoXform.rot.i == 0.0f &&
	   currLocoXform.rot.j == 0.0f &&
	   currLocoXform.rot.k == 0.0f &&
	   currLocoXform.rot.r == 0.0f)
	{
		fm_quat_identity(&currLocoXform.rot);
	}
	
	fm_quat currLocoRot = currLocoXform.rot;
	fm_vec4 currLocoPos = currLocoXform.pos;
	
	fm_quat prevLocoRot = {};
	fm_vec4 prevLocoPos = {};
	
	if(data->resetLoco)
	{
		prevLocoRot = currLocoRot;
		prevLocoPos = currLocoPos;
	}
	else
	{
		prevLocoRot.i = data->prevLocoRot[0];
		prevLocoRot.j = data->prevLocoRot[1];
		prevLocoRot.k = data->prevLocoRot[2];
		prevLocoRot.r = data->prevLocoRot[3];
		
		prevLocoPos.x = data->prevLocoPos[0];
		prevLocoPos.y = data->prevLocoPos[1];
		prevLocoPos.z = data->prevLocoPos[2];
		prevLocoPos.w = data->prevLocoPos[3];
	}
	
	// todo: remove
	if(prevLocoRot.i == 0.0f &&
	   prevLocoRot.j == 0.0f &&
	   prevLocoRot.k == 0.0f &&
	   prevLocoRot.r == 0.0f)
	{
		fm_quat_identity(&prevLocoRot);
	}
	
	// apply loops to locomotion (when time goes from duration back to 0)
	//if(clip->motionDelta != NULL) // todo: uncomment in future
	if(data->loops != 0)
	{
		fm_xform singleMotionLoop = {};
		singleMotionLoop.pos.x = clip->motionDelta[0];
		singleMotionLoop.pos.y = clip->motionDelta[1];
		singleMotionLoop.pos.z = clip->motionDelta[2];
		singleMotionLoop.pos.w = clip->motionDelta[3];
		singleMotionLoop.rot.i = clip->motionDelta[4];
		singleMotionLoop.rot.j = clip->motionDelta[5];
		singleMotionLoop.rot.k = clip->motionDelta[6];
		singleMotionLoop.rot.r = clip->motionDelta[7];
		
		int32_t loopsLeft = data->loops;
		if(loopsLeft > 0)
		{
			for(;loopsLeft>0; --loopsLeft)
			{
				fm_xform_apply(&singleMotionLoop, &currLocoPos, &currLocoPos);
				fm_quat_mul(&currLocoRot, &singleMotionLoop.rot, &currLocoRot);
			}
		}
		else if(loopsLeft < 0)
		{
			fm_quat singleMotionLoopRotConj = singleMotionLoop.rot;
			fm_quat_conj(&singleMotionLoopRotConj);
			
			for(;loopsLeft<0; ++loopsLeft)
			{
				fm_vec4_sub(&currLocoPos, &singleMotionLoop.pos, &currLocoPos);
				fm_quat_rot(&singleMotionLoopRotConj, &currLocoPos, &currLocoPos);
				
				fm_quat_mul(&singleMotionLoopRotConj, &currLocoRot, &currLocoRot);
			}
		}
	}
	
	fm_quat prevLocoRotConj = prevLocoRot;
	fm_quat_conj(&prevLocoRotConj);
	
	fm_quat_mul(&currLocoRot, &prevLocoRotConj, &currLocoRot);
	fm_vec4_sub(&currLocoPos, &prevLocoPos, &currLocoPos);
	fm_quat_rot(&prevLocoRotConj, &currLocoPos, &currLocoPos);
	
	fm_quat_norm(&currLocoRot);
	
	// save new locomotion to prev loco
	data->prevLocoRot[0] = currLocoXform.rot.i;
	data->prevLocoRot[1] = currLocoXform.rot.j;
	data->prevLocoRot[2] = currLocoXform.rot.k;
	data->prevLocoRot[3] = currLocoXform.rot.r;
	
	data->prevLocoPos[0] = currLocoXform.pos.x;
	data->prevLocoPos[1] = currLocoXform.pos.y;
	data->prevLocoPos[2] = currLocoXform.pos.z;
	data->prevLocoPos[3] = currLocoXform.pos.w;
	
	// override locomotion joint with motion delta
	pose.xforms[idxLocoJoint].rot = currLocoRot;
	pose.xforms[idxLocoJoint].pos = currLocoPos;
	
	return FA_CMD_STATUS_OK;
}

void fa_cmd_anim_sample_with_locomotion(fa_cmd_buffer_recorder_t* recorder, float time, uint16_t animClipId, bool resetLoco, int32_t loops, float* prevLocoPos, float* prevLocoRot)
{
	fa_cmd_anim_sample_with_locomotion_data_t data = { time, animClipId, false, loops, resetLoco, prevLocoPos, prevLocoRot };
	fa_cmd_buffer_write(recorder, fa_cmd_impl_anim_sample_with_locomotion, &data, sizeof(fa_cmd_anim_sample_with_locomotion_data_t));
	recorder->poseStackSizeTracking += 1;
}

// blend two poses command
typedef struct fa_cmd_blend2_data_t
{
	float alpha;
} fa_cmd_blend2_data_t;

fa_cmd_status_t fa_cmd_impl_blend2(fa_cmd_context_t* ctx, const void* cmdData)
{
	fa_cmd_blend2_data_t* data = (fa_cmd_blend2_data_t*)cmdData;
	
	fa_pose_t pose1;
	fa_pose_stack_get(ctx->poseStack, &pose1, 0);
	
	fa_pose_t pose2;
	fa_pose_stack_get(ctx->poseStack, &pose2, 1);
	
	fa_pose_blend_linear(&pose2, &pose1, &pose2, data->alpha);
	
	fa_pose_stack_pop(ctx->poseStack, 1);
	
	if(ctx->debug)
	{
		const uint32_t pos = ctx->debug->cmdDrawCursorVerticalPos;
		const float color[4] = FA_DBG_COLOR;
		char txt[128];
		sprintf(txt, "blend2 a=%1.2f", data->alpha);
		fc_dbg_text(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), txt, color);
	}
	
	return FA_CMD_STATUS_OK;
}

void fa_cmd_blend2(fa_cmd_buffer_recorder_t* recorder, float alpha)
{
	fa_cmd_blend2_data_t data = { alpha };
	fa_cmd_buffer_write(recorder, fa_cmd_impl_blend2, &data, sizeof(fa_cmd_blend2_data_t));
	recorder->poseStackSizeTracking -= 1;
}

// blend additive command, pose depth-0 is additive, pose depth-1 is base
typedef struct fa_cmd_apply_additive_data_t
{
	float weight;
} fa_cmd_apply_additive_data_t;

fa_cmd_status_t fa_cmd_impl_apply_additive(fa_cmd_context_t* ctx, const void* cmdData)
{
	fa_cmd_apply_additive_data_t* data = (fa_cmd_apply_additive_data_t*)cmdData;
	
	fa_pose_t poseAdd;
	fa_pose_stack_get(ctx->poseStack, &poseAdd, 0);
	
	fa_pose_t poseBase;
	fa_pose_stack_get(ctx->poseStack, &poseBase, 1);
	
	fa_pose_apply_additive(&poseBase, &poseBase, &poseAdd, data->weight);
	
	fa_pose_stack_pop(ctx->poseStack, 1);
	
	if(ctx->debug)
	{
		const uint32_t pos = ctx->debug->cmdDrawCursorVerticalPos;
		const float color[4] = FA_DBG_COLOR;
		char txt[128];
		sprintf(txt, "apply additive w=%1.2f", data->weight);
		fc_dbg_text(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), txt, color);
	}
	
	return FA_CMD_STATUS_OK;
}

void fa_cmd_apply_additive(fa_cmd_buffer_recorder_t* recorder, float weight)
{
	fa_cmd_apply_additive_data_t data = { weight };
	fa_cmd_buffer_write(recorder, fa_cmd_impl_apply_additive, &data, sizeof(fa_cmd_apply_additive_data_t));
	recorder->poseStackSizeTracking -= 1;
}

// use cached pose command
typedef struct fa_cmd_use_cached_pose_data_t
{
	uint16_t poseId;
} fa_cmd_use_cached_pose_data_t;

fa_cmd_status_t fa_cmd_impl_use_cached_pose(fa_cmd_context_t* ctx, const void* cmdData)
{
	fa_cmd_use_cached_pose_data_t* data = (fa_cmd_use_cached_pose_data_t*)cmdData;
	
	FUR_ASSERT(data->poseId == 0);
	
	fa_pose_stack_push(ctx->poseStack, 1);
	fa_pose_t pose;
	fa_pose_stack_get(ctx->poseStack, &pose, 0);
	
	fa_pose_copy(&pose, &ctx->poseCache->tempPose);
	
	if(ctx->debug)
	{
		const uint32_t pos = ctx->debug->cmdDrawCursorVerticalPos;
		const float color[4] = FA_DBG_COLOR;
		char txt[128];
		sprintf(txt, "use_cached_pose id=%i", data->poseId);
		fc_dbg_text(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), txt, color);
	}
	
	return FA_CMD_STATUS_OK;
}

void fa_cmd_use_cached_pose(fa_cmd_buffer_recorder_t* recorder, uint16_t poseId)
{
	fa_cmd_use_cached_pose_data_t data = { poseId };
	fa_cmd_buffer_write(recorder, fa_cmd_impl_use_cached_pose, &data, sizeof(fa_cmd_use_cached_pose_data_t));
	recorder->poseStackSizeTracking += 1;
}

// apply mask command
typedef struct fa_cmd_apply_mask_data_t
{
	uint16_t maskId;
} fa_cmd_apply_mask_data_t;

fa_cmd_status_t fa_cmd_impl_apply_mask(fa_cmd_context_t* ctx, const void* cmdData)
{
	fa_cmd_apply_mask_data_t* data = (fa_cmd_apply_mask_data_t*)cmdData;
	const uint8_t* mask = fa_rig_get_mask(ctx->rig, data->maskId);
	if(!mask)
	{
		if(ctx->debug)
		{
			const uint32_t pos = ctx->debug->cmdDrawCursorVerticalPos;
			const float color[4] = FUR_COLOR_RED;
			fc_dbg_text(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), "apply_mask id=<INNVALID>", color);
		}
		
		return FA_CMD_STATUS_OK;
	}
	
	fa_pose_t pose;
	fa_pose_stack_get(ctx->poseStack, &pose, 0);
	
	for(uint32_t i=0; i<pose.numXforms; ++i)
	{
		const uint16_t weight = ((uint16_t)pose.weightsXforms[i] * (uint16_t)mask[i]) / 255;
		pose.weightsXforms[i] = (uint8_t)weight;
	}
	
	if(ctx->debug)
	{
		const uint32_t pos = ctx->debug->cmdDrawCursorVerticalPos;
		const float color[4] = FA_DBG_COLOR;
		char txt[128];
		sprintf(txt, "apply_mask id=%i", data->maskId);
		fc_dbg_text(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), txt, color);
	}
	
	return FA_CMD_STATUS_OK;
}

void fa_cmd_apply_mask(fa_cmd_buffer_recorder_t* recorder, uint16_t maskId)
{
	fa_cmd_apply_mask_data_t data = { maskId };
	fa_cmd_buffer_write(recorder, fa_cmd_impl_apply_mask, &data, sizeof(fa_cmd_apply_mask_data_t));
}
