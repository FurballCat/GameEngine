/* Copyright (c) 2016-2022 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include "ccore/types.h"

typedef struct fm_xform fm_xform;

typedef enum fa_pose_flags_t
{
	PF_ADDITIVE = 0x1,
} fa_pose_flags_t;
	
typedef struct fa_pose_t
{
	fm_xform* xforms;
	f32* tracks;
	u8* weightsXforms;
	u8* weightsTracks;
	
	u16 numXforms;
	u16 numTracks;
	
	u32 flags;
} fa_pose_t;
	
// -----

typedef struct fa_rig_t fa_rig_t;

CANIM_API void fa_pose_set_identity(fa_pose_t* pose, const u8* mask /* optional */);
CANIM_API void fa_pose_set_reference(const fa_rig_t* rig, fa_pose_t* pose, const u8* mask /* optional */);
	
// -----

CANIM_API void fa_pose_copy(fa_pose_t* dest, const fa_pose_t* src);
CANIM_API void fa_pose_local_to_model(fa_pose_t* modelPose, const fa_pose_t* localPose, const int16_t* parentIndices);
	
CANIM_API void fa_pose_blend_linear(fa_pose_t* out, const fa_pose_t* a, const fa_pose_t* b, f32 alpha);
CANIM_API void fa_pose_apply_additive(fa_pose_t* out, const fa_pose_t* base, const fa_pose_t* add, f32 weight);

// -----

#ifdef __cplusplus
}
#endif // __cplusplus
