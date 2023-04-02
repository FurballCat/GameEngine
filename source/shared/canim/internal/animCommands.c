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

void fcAnimCmdBufferEvaluate(const FcAnimCommandBuffer* buffer, FcAnimCommandCtx* ctx)
{
	FcAnimCommandStatus status = FA_CMD_STATUS_OK;
	u32 dbgIndexCommand = 0;
	const u8* cmdPointer = (const u8*)buffer->data;
	while(status == FA_CMD_STATUS_OK)
	{
		const FcAnimCommandFn* func = (const FcAnimCommandFn*)cmdPointer;
		const u32* dataSize = (const u32*)(cmdPointer + sizeof(FcAnimCommandFn));
		const void* cmdData = NULL;
		if(*dataSize > 0)
		{
			 cmdData = cmdPointer + sizeof(FcAnimCommandFn) + sizeof(u32);
		}
		
		status = (*func)(ctx, cmdData);
		const u32 totalCommandSize = sizeof(FcAnimCommandFn) + sizeof(u32) + (*dataSize);
		cmdPointer += totalCommandSize;
		++dbgIndexCommand;
		
		if(ctx->debug)
		{
			ctx->debug->cmdDrawCursorVerticalPos += 14;
		}
	}
}

// this is the cmd memory formatting function
void fcAnimCmdBufferWrite(FcCommandBufferRecorder* recorder, FcAnimCommandFn func, const void* data, u32 dataSize)
{
	const u32 sizeRequired = sizeof(FcAnimCommandFn) + sizeof(u32) + dataSize;
	FUR_ASSERT(sizeRequired <= recorder->sizeLeft);

	u8* const currPointer = (u8* const)recorder->currPointer;

	// command function pointer
	FcAnimCommandFn* funcPtr = (FcAnimCommandFn*)currPointer;
	*funcPtr = func;
	
	// command data size (can be 0)
	u32* dataSizePtr = (u32*)(currPointer + sizeof(FcAnimCommandFn));
	*dataSizePtr = dataSize;
	
	// command data
	if(data)
	{
		void* dataPtr = currPointer + sizeof(FcAnimCommandFn) + sizeof(u32);
		memcpy(dataPtr, data, dataSize);
	}
	
	recorder->currPointer = (u8*)recorder->currPointer + sizeRequired;
	recorder->sizeLeft -= sizeRequired;
	recorder->sizeRecorded += sizeRequired;
}

void fcAnimCmdBufferRecorderInit(FcCommandBufferRecorder* recorder, void* outData, u32 maxSize)
{
	recorder->currPointer = outData;
	recorder->sizeLeft = maxSize;
	recorder->sizeRecorded = 0;
	recorder->poseStackSizeTracking = 0;
}

// begin command
void fcAnimCmdBegin(FcCommandBufferRecorder* recorder, u32 poseStackInitialSize)
{
	recorder->poseStackSizeTracking = poseStackInitialSize;
	recorder->poseStackInitialSize = poseStackInitialSize;
}

// end command
FcAnimCommandStatus fcAnimCmdImplEnd(FcAnimCommandCtx* ctx, const void* cmdData)
{
	return FA_CMD_STATUS_STOP;
}

void fcAnimCmdEnd(FcCommandBufferRecorder* recorder)
{
	fcAnimCmdBufferWrite(recorder, fcAnimCmdImplEnd, NULL, 0); // writing a null cmd, similar to c-string having null character at the end
	
	FUR_ASSERT(recorder->poseStackSizeTracking >= 1);	// at the end of command buffer, we expect the post stack to have +1 pose
}

#define FA_DBG_TEXT_X -900.0f
#define FA_DBG_TEXT_Y(_pos) 150.0f - 2.0f * _pos
#define FA_DBG_COLOR FUR_COLOR_GREEN

// set reference pose command
FcAnimCommandStatus fcAnimCmdImplRefPose(FcAnimCommandCtx* ctx, const void* cmdData)
{
	FcPoseStackPush(ctx->poseStack, 1);
	
	FcPose pose;
	FcPoseStackGet(ctx->poseStack, &pose, 0);
	
	FcPoseSetReference(ctx->rig, &pose, ctx->mask);
	
	if(ctx->debug)
	{
		const u32 pos = ctx->debug->cmdDrawCursorVerticalPos;
		const f32 color[4] = FA_DBG_COLOR;
		fcDebugText(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), "ref_pose", color, 0.5f);
	}
	
	return FA_CMD_STATUS_OK;
}

void fcAnimCmdRefPose(FcCommandBufferRecorder* recorder)
{
	fcAnimCmdBufferWrite(recorder, fcAnimCmdImplRefPose, NULL, 0);
	recorder->poseStackSizeTracking += 1;
}

// set identity pose command
FcAnimCommandStatus fcAnimCmdImplIdentity(FcAnimCommandCtx* ctx, const void* cmdData)
{
	FcPoseStackPush(ctx->poseStack, 1);
	
	FcPose pose;
	FcPoseStackGet(ctx->poseStack, &pose, 0);
	
	FcPoseSetIdentity(&pose, ctx->mask);
	
	if(ctx->debug)
	{
		const u32 pos = ctx->debug->cmdDrawCursorVerticalPos;
		const f32 color[4] = FA_DBG_COLOR;
		fcDebugText(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), "identity", color, 0.5f);
	}
	
	return FA_CMD_STATUS_OK;
}

void fcAnimCmdIdentity(FcCommandBufferRecorder* recorder)
{
	fcAnimCmdBufferWrite(recorder, fcAnimCmdImplIdentity, NULL, 0);
	recorder->poseStackSizeTracking += 1;
}

// sample animation command
typedef struct FcAnimCmdAnimSampleData
{
	f32 time;
	u16 animClipId;
	bool asAdditive;
} FcAnimCmdAnimSampleData;

FcAnimCommandStatus fcAnimCmdImpleAnimSample(FcAnimCommandCtx* ctx, const void* cmdData)
{
	const FcAnimCmdAnimSampleData* data = (FcAnimCmdAnimSampleData*)cmdData;
	FUR_ASSERT(data->animClipId < ctx->numAnimClips);
	
	const FcAnimClip* clip = ctx->animClips[data->animClipId];
	
	FcPoseStackPush(ctx->poseStack, 1);
	FcPose pose;
	FcPoseStackGet(ctx->poseStack, &pose, 0);
	
	FUR_ASSERT(pose.numXforms == ctx->rig->numBones);
	
	fcAnimClipSample(clip, data->time, data->asAdditive, &pose, ctx->mask);
	
	if(ctx->debug)
	{
		const u32 pos = ctx->debug->cmdDrawCursorVerticalPos;
		const f32 color[4] = FA_DBG_COLOR;
		char txt[256];
		sprintf(txt, "anim_sample %s t=%1.2f", fcStringIdAsDebugCstr(clip->name), data->time);
		fcDebugText(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), txt, color, 0.5f);
	}
	
	// todo: check if required
	// just in case - reset loco joint
	const int16_t idxLocoJoint = ctx->rig->idxLocoJoint;
	if(idxLocoJoint != -1)
	{
		FUR_ASSERT((i32)idxLocoJoint < ctx->rig->numBones);
		fm_quat_identity(&pose.xforms[idxLocoJoint].rot);
		fm_vec4_zeros(&pose.xforms[idxLocoJoint].pos);
	}
	
	return FA_CMD_STATUS_OK;
}

void fcAnimCmdSample(FcCommandBufferRecorder* recorder, f32 time, u16 animClipId)
{
	FcAnimCmdAnimSampleData data = { time, animClipId, false };
	fcAnimCmdBufferWrite(recorder, fcAnimCmdImpleAnimSample, &data, sizeof(FcAnimCmdAnimSampleData));
	recorder->poseStackSizeTracking += 1;
}

void fcAnimCmdSampleAdditive(FcCommandBufferRecorder* recorder, f32 time, u16 animClipId)
{
	FcAnimCmdAnimSampleData data = { time, animClipId, true };
	fcAnimCmdBufferWrite(recorder, fcAnimCmdImpleAnimSample, &data, sizeof(FcAnimCmdAnimSampleData));
	recorder->poseStackSizeTracking += 1;
}

// sample animation with locomotion command
typedef struct FcAnimCmdAnimSampleWithLocomotionData
{
	f32 time;
	u16 animClipId;
	bool asAdditive;
	bool resetLoco;
	i32 loops;
	f32* prevLocoPos;	// fm_vec4
	f32* prevLocoRot;	// fm_vec4
} FcAnimCmdAnimSampleWithLocomotionData;

FcAnimCommandStatus FcAnimCmdImplAnimSampleWithLocomotion(FcAnimCommandCtx* ctx, const void* cmdData)
{
	const FcAnimCmdAnimSampleWithLocomotionData* data = (FcAnimCmdAnimSampleWithLocomotionData*)cmdData;
	FUR_ASSERT(data->animClipId < ctx->numAnimClips);
	
	// get animation clip
	const FcAnimClip* clip = ctx->animClips[data->animClipId];
	
	// push new pose onto stack
	FcPoseStackPush(ctx->poseStack, 1);
	FcPose pose;
	FcPoseStackGet(ctx->poseStack, &pose, 0);
	
	FUR_ASSERT(pose.numXforms == ctx->rig->numBones);
	
	// sample the animation
	fcAnimClipSample(clip, data->time, data->asAdditive, &pose, ctx->mask);
	
	// optionally draw debug info
	if(ctx->debug)
	{
		const u32 pos = ctx->debug->cmdDrawCursorVerticalPos;
		const f32 color[4] = FA_DBG_COLOR;
		char txt[256];
		sprintf(txt, "anim_sample_with_locomotion %s t=%1.2f", fcStringIdAsDebugCstr(clip->name), data->time);
		fcDebugText(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), txt, color, 0.5f);
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
	
	fm_quat prevLocoRot = {0};
	fm_vec4 prevLocoPos = {0};
	
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
		fm_xform singleMotionLoop = {0};
		singleMotionLoop.pos.x = clip->motionDelta[0];
		singleMotionLoop.pos.y = clip->motionDelta[1];
		singleMotionLoop.pos.z = clip->motionDelta[2];
		singleMotionLoop.pos.w = clip->motionDelta[3];
		singleMotionLoop.rot.i = clip->motionDelta[4];
		singleMotionLoop.rot.j = clip->motionDelta[5];
		singleMotionLoop.rot.k = clip->motionDelta[6];
		singleMotionLoop.rot.r = clip->motionDelta[7];
		
		i32 loopsLeft = data->loops;
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

void fcAnimCmdSampleWithLocomotion(FcCommandBufferRecorder* recorder, f32 time, u16 animClipId, bool resetLoco, i32 loops, f32* prevLocoPos, f32* prevLocoRot)
{
	FcAnimCmdAnimSampleWithLocomotionData data = { time, animClipId, false, resetLoco, loops, prevLocoPos, prevLocoRot };
	fcAnimCmdBufferWrite(recorder, FcAnimCmdImplAnimSampleWithLocomotion, &data, sizeof(FcAnimCmdAnimSampleWithLocomotionData));
	recorder->poseStackSizeTracking += 1;
}

// blend two poses command
typedef struct FcAnimCmdBlendData
{
	f32 alpha;
} FcAnimCmdBlendData;

FcAnimCommandStatus fcAnimCmdImpleBlend(FcAnimCommandCtx* ctx, const void* cmdData)
{
	FcAnimCmdBlendData* data = (FcAnimCmdBlendData*)cmdData;
	
	FcPose pose1;
	FcPoseStackGet(ctx->poseStack, &pose1, 0);
	
	FcPose pose2;
	FcPoseStackGet(ctx->poseStack, &pose2, 1);
	
	FcPoseBlendLinear(&pose2, &pose1, &pose2, data->alpha);
	
	FcPoseStackPop(ctx->poseStack, 1);
	
	if(ctx->debug)
	{
		const u32 pos = ctx->debug->cmdDrawCursorVerticalPos;
		const f32 color[4] = FA_DBG_COLOR;
		char txt[128];
		sprintf(txt, "blend2 a=%1.2f", data->alpha);
		fcDebugText(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), txt, color, 0.5f);
	}
	
	return FA_CMD_STATUS_OK;
}

void fcAnimCmdBlend(FcCommandBufferRecorder* recorder, f32 alpha)
{
	FcAnimCmdBlendData data = { alpha };
	fcAnimCmdBufferWrite(recorder, fcAnimCmdImpleBlend, &data, sizeof(FcAnimCmdBlendData));
	recorder->poseStackSizeTracking -= 1;
}

// blend additive command, pose depth-0 is additive, pose depth-1 is base
typedef struct FcAnimCmdApplyAdditiveData
{
	f32 weight;
} FcAnimCmdApplyAdditiveData;

FcAnimCommandStatus fcAnimCmdImplApplyAdditive(FcAnimCommandCtx* ctx, const void* cmdData)
{
	FcAnimCmdApplyAdditiveData* data = (FcAnimCmdApplyAdditiveData*)cmdData;
	
	FcPose poseAdd;
	FcPoseStackGet(ctx->poseStack, &poseAdd, 0);
	
	FcPose poseBase;
	FcPoseStackGet(ctx->poseStack, &poseBase, 1);
	
	FcPoseApplyAdditive(&poseBase, &poseBase, &poseAdd, data->weight);
	
	FcPoseStackPop(ctx->poseStack, 1);
	
	if(ctx->debug)
	{
		const u32 pos = ctx->debug->cmdDrawCursorVerticalPos;
		const f32 color[4] = FA_DBG_COLOR;
		char txt[128];
		sprintf(txt, "apply additive w=%1.2f", data->weight);
		fcDebugText(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), txt, color, 0.5f);
	}
	
	return FA_CMD_STATUS_OK;
}

void fcAnimCmdApplyAdditive(FcCommandBufferRecorder* recorder, f32 weight)
{
	FcAnimCmdApplyAdditiveData data = { weight };
	fcAnimCmdBufferWrite(recorder, fcAnimCmdImplApplyAdditive, &data, sizeof(FcAnimCmdApplyAdditiveData));
	recorder->poseStackSizeTracking -= 1;
}

// use cached pose command
typedef struct FcAnimCmdUseCachedPoseData
{
	u16 poseId;
} FcAnimCmdUseCachedPoseData;

FcAnimCommandStatus fcAnimCmdImpleUseCachedPose(FcAnimCommandCtx* ctx, const void* cmdData)
{
	FcAnimCmdUseCachedPoseData* data = (FcAnimCmdUseCachedPoseData*)cmdData;
	
	FUR_ASSERT(data->poseId == 0);
	
	FcPoseStackPush(ctx->poseStack, 1);
	FcPose pose;
	FcPoseStackGet(ctx->poseStack, &pose, 0);
	
	FcPoseCopy(&pose, &ctx->poseCache->tempPose);
	
	if(ctx->debug)
	{
		const u32 pos = ctx->debug->cmdDrawCursorVerticalPos;
		const f32 color[4] = FA_DBG_COLOR;
		char txt[128];
		sprintf(txt, "use_cached_pose id=%i", data->poseId);
		fcDebugText(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), txt, color, 0.5f);
	}
	
	return FA_CMD_STATUS_OK;
}

void fcAnimCmdUseCachedPose(FcCommandBufferRecorder* recorder, u16 poseId)
{
	FcAnimCmdUseCachedPoseData data = { poseId };
	fcAnimCmdBufferWrite(recorder, fcAnimCmdImpleUseCachedPose, &data, sizeof(FcAnimCmdUseCachedPoseData));
	recorder->poseStackSizeTracking += 1;
}

// apply mask command
typedef struct FcAnimCmdApplyMaskData
{
	u16 maskId;
} FcAnimCmdApplyMaskData;

FcAnimCommandStatus fcAnimCmdImplApplyMask(FcAnimCommandCtx* ctx, const void* cmdData)
{
	FcAnimCmdApplyMaskData* data = (FcAnimCmdApplyMaskData*)cmdData;
	const u8* mask = fcRigGetMask(ctx->rig, data->maskId);
	if(!mask)
	{
		if(ctx->debug)
		{
			const u32 pos = ctx->debug->cmdDrawCursorVerticalPos;
			const f32 color[4] = FUR_COLOR_RED;
			fcDebugText(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), "apply_mask id=<INNVALID>", color, 0.5f);
		}
		
		return FA_CMD_STATUS_OK;
	}
	
	FcPose pose;
	FcPoseStackGet(ctx->poseStack, &pose, 0);
	
	for(u32 i=0; i<pose.numXforms; ++i)
	{
		const u16 weight = ((u16)pose.weightsXforms[i] * (u16)mask[i]) / 255;
		pose.weightsXforms[i] = (u8)weight;
	}
	
	if(ctx->debug)
	{
		const u32 pos = ctx->debug->cmdDrawCursorVerticalPos;
		const f32 color[4] = FA_DBG_COLOR;
		char txt[128];
		sprintf(txt, "apply_mask id=%i", data->maskId);
		fcDebugText(FA_DBG_TEXT_X, FA_DBG_TEXT_Y(pos), txt, color, 0.5f);
	}
	
	return FA_CMD_STATUS_OK;
}

void fcAnimCmdApplyMask(FcCommandBufferRecorder* recorder, u16 maskId)
{
	FcAnimCmdApplyMaskData data = { maskId };
	fcAnimCmdBufferWrite(recorder, fcAnimCmdImplApplyMask, &data, sizeof(FcAnimCmdApplyMaskData));
}
