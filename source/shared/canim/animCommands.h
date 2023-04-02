/* Copyright (c) 2016-2022 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include "pose.h"	// todo: remove
#include <inttypes.h>
#include <stdbool.h>

typedef struct FcPoseStack FcPoseStack;
typedef struct FcPoseCache FcPoseCache;
typedef struct FcRig FcRig;
typedef struct FcAnimClip FcAnimClip;

typedef struct FcPoseCache
{
	FcPose tempPose;
	f32 alpha;
} FcPoseCache;

typedef struct FcCommandBufferContextDebug
{
	u32 cmdDrawCursorVerticalPos;
} FcCommandBufferContextDebug;
	
typedef struct FcAnimCommandCtx
{
	FcPoseStack* poseStack;
	const FcPoseCache* poseCache;
	const FcRig* rig;
	const FcAnimClip** animClips;
	u32 numAnimClips;
	
	const u8* mask;	// optional, default is NULL
	
	FcCommandBufferContextDebug* debug;	// if NULL, don't use debug
} FcAnimCommandCtx;

typedef struct FcAnimCommandBuffer
{
	void* data;
	u32 size;
} FcAnimCommandBuffer;
	
typedef struct FcCommandBufferRecorder
{
	void* currPointer;
	u32 sizeLeft;
	u32 sizeRecorded;
	
	u32 poseStackInitialSize;
	u32 poseStackSizeTracking;
} FcCommandBufferRecorder;

typedef enum FcAnimCommandStatus
{
	FA_CMD_STATUS_OK = 0,
	FA_CMD_STATUS_STOP = 1,
} FcAnimCommandStatus;
	
typedef FcAnimCommandStatus (*FcAnimCommandFn)(FcAnimCommandCtx* ctx, const void* cmdData);

CANIM_API void fcAnimCmdBufferEvaluate(const FcAnimCommandBuffer* buffer, FcAnimCommandCtx* ctx);
	
CANIM_API void fcAnimCmdBufferRecorderInit(FcCommandBufferRecorder* recorder, void* outData, u32 maxSize);
CANIM_API void fcAnimCmdBegin(FcCommandBufferRecorder* recorder, u32 poseStackInitialSize);	// poseStackInitialSize = 0 by default
CANIM_API void fcAnimCmdEnd(FcCommandBufferRecorder* recorder);

CANIM_API void fcAnimCmdRefPose(FcCommandBufferRecorder* recorder);
CANIM_API void fcAnimCmdIdentity(FcCommandBufferRecorder* recorder);
CANIM_API void fcAnimCmdSample(FcCommandBufferRecorder* recorder, f32 time, u16 animClipId);
CANIM_API void fcAnimCmdSampleAdditive(FcCommandBufferRecorder* recorder, f32 time, u16 animClipId);
CANIM_API void fcAnimCmdApplyAdditive(FcCommandBufferRecorder* recorder, f32 weight);
CANIM_API void fcAnimCmdBlend(FcCommandBufferRecorder* recorder, f32 alpha);
CANIM_API void fcAnimCmdBlendMasked(FcCommandBufferRecorder* recorder, f32 alpha, u16 maskId);
CANIM_API void fcAnimCmdBlendAdditive(FcCommandBufferRecorder* recorder, f32 alpha);
CANIM_API void fcAnimCmdUseCachedPose(FcCommandBufferRecorder* recorder, u16 poseId);
CANIM_API void fcAnimCmdApplyMask(FcCommandBufferRecorder* recorder, u16 maskId);
CANIM_API void fcAnimCmdSampleWithLocomotion(FcCommandBufferRecorder* recorder, f32 time, u16 animClipId, bool resetLoco, i32 loops, f32* prevLocoPos, f32* prevLocoRot);

#ifdef __cplusplus
}
#endif // __cplusplus
