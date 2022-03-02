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

typedef struct fa_pose_stack_t fa_pose_stack_t;
typedef struct fa_pose_cache_t fa_pose_cache_t;
typedef struct fa_rig_t fa_rig_t;
typedef struct fa_anim_clip_t fa_anim_clip_t;

typedef struct fa_pose_cache_t
{
	fa_pose_t tempPose;
	float alpha;
} fa_pose_cache_t;

typedef struct fa_cmd_context_debug_t
{
	uint32_t cmdDrawCursorVerticalPos;
} fa_cmd_context_debug_t;
	
typedef struct fa_cmd_context_t
{
	fa_pose_stack_t* poseStack;
	const fa_pose_cache_t* poseCache;
	const fa_rig_t* rig;
	const fa_anim_clip_t** animClips;
	uint32_t numAnimClips;
	
	const uint8_t* mask;	// optional, default is NULL
	
	fa_cmd_context_debug_t* debug;	// if NULL, don't use debug
} fa_cmd_context_t;

typedef struct fa_cmd_buffer_t
{
	void* data;
	uint32_t size;
} fa_cmd_buffer_t;
	
typedef struct fa_cmd_buffer_recorder_t
{
	void* currPointer;
	uint32_t sizeLeft;
	uint32_t sizeRecorded;
	
	uint32_t poseStackInitialSize;
	uint32_t poseStackSizeTracking;
} fa_cmd_buffer_recorder_t;

typedef enum fa_cmd_status_t
{
	FA_CMD_STATUS_OK = 0,
	FA_CMD_STATUS_STOP = 1,
} fa_cmd_status_t;
	
typedef fa_cmd_status_t (*fa_cmd_func_t)(fa_cmd_context_t* ctx, const void* cmdData);

CANIM_API void fa_cmd_buffer_evaluate(const fa_cmd_buffer_t* buffer, fa_cmd_context_t* ctx);
	
CANIM_API void fa_cmd_buffer_recorder_init(fa_cmd_buffer_recorder_t* recorder, void* outData, uint32_t maxSize);
CANIM_API void fa_cmd_begin(fa_cmd_buffer_recorder_t* recorder, uint32_t poseStackInitialSize);	// poseStackInitialSize = 0 by default
CANIM_API void fa_cmd_end(fa_cmd_buffer_recorder_t* recorder);

CANIM_API void fa_cmd_ref_pose(fa_cmd_buffer_recorder_t* recorder);
CANIM_API void fa_cmd_identity(fa_cmd_buffer_recorder_t* recorder);
CANIM_API void fa_cmd_anim_sample(fa_cmd_buffer_recorder_t* recorder, float time, uint16_t animClipId);
CANIM_API void fa_cmd_anim_sample_additive(fa_cmd_buffer_recorder_t* recorder, float time, uint16_t animClipId);
CANIM_API void fa_cmd_blend2(fa_cmd_buffer_recorder_t* recorder, float alpha);
CANIM_API void fa_cmd_blend_override(fa_cmd_buffer_recorder_t* recorder, float alpha, uint16_t maskId);
CANIM_API void fa_cmd_blend_additive(fa_cmd_buffer_recorder_t* recorder, float alpha);
CANIM_API void fa_cmd_use_cached_pose(fa_cmd_buffer_recorder_t* recorder, uint16_t poseId);
CANIM_API void fa_cmd_apply_mask(fa_cmd_buffer_recorder_t* recorder, uint16_t maskId);
CANIM_API void fa_cmd_anim_sample_with_locomotion(fa_cmd_buffer_recorder_t* recorder, float time, uint16_t animClipId, bool resetLoco, int32_t loops, float* prevLocoPos, float* prevLocoRot);

#ifdef __cplusplus
}
#endif // __cplusplus
