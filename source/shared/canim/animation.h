/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include <inttypes.h>
	
typedef struct fm_xform fm_xform;

typedef struct fa_rig_t
{
	uint64_t* boneNameHashes;
	int16_t* parents;
	fm_xform* refPose;
	uint32_t numBones;
} fa_rig_t;

typedef struct fa_anim_clip_key_t
{
	uint16_t keyTime;
	uint16_t idxBoneAndChannel;	// 2 bits for channel
	uint16_t keyData[3];
} fa_anim_clip_key_t;

typedef struct fa_anim_clip_t
{
	fa_anim_clip_key_t* keys;
	uint32_t duration;
	uint32_t numKeys;
} fa_anim_clip_t;
	
typedef struct fa_pose_t
{
	fm_xform* xforms;
	float* tracks;
	
	uint16_t numXforms;
	uint16_t numTracks;
} fa_pose_t;

// -----

CANIM_API void fa_pose_set_identity(fa_pose_t* pose);
CANIM_API void fa_pose_set_reference(const fa_rig_t* rig, fa_pose_t* pose);
	
// -----
	
typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;
typedef struct fa_pose_stack_t fa_pose_stack_t;	// implementation hidden

CANIM_API void fa_pose_stack_push(fa_pose_stack_t* stack, uint32_t count);
CANIM_API void fa_pose_stack_pop(fa_pose_stack_t* stack, uint32_t count);
	
CANIM_API void fa_pose_stack_get(const fa_pose_stack_t* stack, uint32_t depth, fa_pose_t* pose);

// -----
	
CANIM_API void fa_anim_clip_sample(const fa_anim_clip_t* clip, float time, fa_pose_t* pose);
	
#ifdef __cplusplus
}
#endif // __cplusplus
