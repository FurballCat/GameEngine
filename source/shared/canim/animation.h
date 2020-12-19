/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include <inttypes.h>
	
typedef struct fm_xform fm_xform;
typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;
	
typedef struct fa_rig_t
{
	uint64_t* boneNameHashes;
	int16_t* parents;
	fm_xform* refPose;
	uint32_t numBones;
} fa_rig_t;

typedef struct fa_anim_curve_key_t
{
	uint16_t keyTime;
	uint16_t keyData[3];
} fa_anim_curve_key_t;

typedef struct fa_anim_curve_t
{
	uint16_t index;
	uint16_t numKeys;
	fa_anim_curve_key_t* keys;
} fa_anim_curve_t;
	
typedef struct fa_anim_clip_t
{
	float duration;
	uint32_t numCurves;
	uint32_t numDataKeys;
	fa_anim_curve_t* curves;
	fa_anim_curve_key_t* dataKeys;	// all keys in the animation
} fa_anim_clip_t;
	
typedef enum fa_pose_flags_t
{
	PF_ADDITIVE = 0x1,
} fa_pose_flags_t;
	
typedef struct fa_pose_t
{
	fm_xform* xforms;
	float* tracks;
	uint8_t* weightsXforms;
	uint8_t* weightsTracks;
	
	uint16_t numXforms;
	uint16_t numTracks;
	
	uint32_t flags;
} fa_pose_t;
	
// -----

CANIM_API void fa_pose_set_identity(fa_pose_t* pose);
CANIM_API void fa_pose_set_reference(const fa_rig_t* rig, fa_pose_t* pose);
	
// -----
	
CANIM_API void fa_anim_clip_sample(const fa_anim_clip_t* clip, float time, fa_pose_t* pose);

CANIM_API void fa_pose_copy(const fa_pose_t* src, fa_pose_t* dest);
CANIM_API void fa_pose_local_to_model(const fa_pose_t* localPose, const int16_t* parentIndices, fa_pose_t* modelPose);
	
CANIM_API void fa_pose_blend_linear(fa_pose_t* out, const fa_pose_t* a, const fa_pose_t* b, float alpha);
	
// -----
	
typedef struct fa_command_buffer_t fa_command_buffer_t;
	
CANIM_API void fa_cmd_begin(fa_command_buffer_t* buffer);
CANIM_API void fa_cmd_end(fa_command_buffer_t* buffer);

CANIM_API void fa_cmd_ref_pose(fa_command_buffer_t* buffer);
CANIM_API void fa_cmd_identity(fa_command_buffer_t* buffer);
CANIM_API void fa_cmd_anim_sample(fa_command_buffer_t* buffer, float time, uint16_t animClipId);
CANIM_API void fa_cmd_blend2(fa_command_buffer_t* buffer, float alpha);
CANIM_API void fa_cmd_blend_override(fa_command_buffer_t* buffer, float alpha, uint16_t maskId);
CANIM_API void fa_cmd_blend_additive(fa_command_buffer_t* buffer, float alpha);

// -----
	
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
