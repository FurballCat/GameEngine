/* Copyright (c) 2016-2022 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include <inttypes.h>

typedef struct fa_pose_t fa_pose_t;

typedef struct fa_pose_stack_t
{
	void* buffer;
	uint32_t bufferSize;
	uint32_t poseSize;

	uint32_t numPoses;
	uint32_t numMaxPoses;

	uint32_t numBones;
	uint32_t numTracks;

	uint32_t offsetTracks;
	uint32_t offsetWeightXforms;
	uint32_t offsetWeightTracks;
} fa_pose_stack_t;

typedef struct fa_pose_stack_desc_t
{
	uint32_t numMaxPoses;

	uint32_t numBonesPerPose;
	uint32_t numTracksPerPose;
} fa_pose_stack_desc_t;

CANIM_API void fa_pose_stack_init(fa_pose_stack_t* pStack, const fa_pose_stack_desc_t* desc, void* buffer, uint32_t bufferSize);
CANIM_API void fa_pose_stack_release(fa_pose_stack_t* pStack);

CANIM_API void fa_pose_stack_push(fa_pose_stack_t* pStack, uint32_t count);
CANIM_API void fa_pose_stack_pop(fa_pose_stack_t* pStack, uint32_t count);
CANIM_API void fa_pose_stack_get(const fa_pose_stack_t* pStack, fa_pose_t* pPose, uint32_t depth);

#ifdef __cplusplus
}
#endif // __cplusplus
