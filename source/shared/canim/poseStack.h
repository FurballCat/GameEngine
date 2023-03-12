/* Copyright (c) 2016-2022 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include "ccore/types.h"

typedef struct fa_pose_t fa_pose_t;

typedef struct fa_pose_stack_t
{
	void* buffer;
	u32 bufferSize;
	u32 poseSize;

	u32 numPoses;
	u32 numMaxPoses;

	u32 numBones;
	u32 numTracks;

	u32 offsetTracks;
	u32 offsetWeightXforms;
	u32 offsetWeightTracks;
} fa_pose_stack_t;

typedef struct fa_pose_stack_desc_t
{
	u32 numMaxPoses;

	u32 numBonesPerPose;
	u32 numTracksPerPose;
} fa_pose_stack_desc_t;

CANIM_API void fa_pose_stack_init(fa_pose_stack_t* pStack, const fa_pose_stack_desc_t* desc, void* buffer, u32 bufferSize);
CANIM_API void fa_pose_stack_release(fa_pose_stack_t* pStack);

CANIM_API void fa_pose_stack_push(fa_pose_stack_t* pStack, u32 count);
CANIM_API void fa_pose_stack_pop(fa_pose_stack_t* pStack, u32 count);
CANIM_API void fa_pose_stack_get(const fa_pose_stack_t* pStack, fa_pose_t* pPose, u32 depth);

#ifdef __cplusplus
}
#endif // __cplusplus
