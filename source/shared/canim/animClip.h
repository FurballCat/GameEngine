/* Copyright (c) 2016-2022 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include <inttypes.h>
#include <stdbool.h>

typedef struct fm_xform fm_xform;
typedef uint32_t fc_string_hash_t;
typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;
typedef struct fc_serializer_t fc_serializer_t;

typedef struct fa_anim_curve_key_t
{
	uint16_t keyTime;
	uint16_t keyData[3];
} fa_anim_curve_key_t;

typedef struct fa_anim_curve_t
{
	uint16_t index;
	uint16_t numRotKeys;
	uint16_t numPosKeys;
	fa_anim_curve_key_t* rotKeys;
	fa_anim_curve_key_t* posKeys;
} fa_anim_curve_t;
	
typedef struct fa_anim_clip_t
{
	fc_string_hash_t name;
	float duration;
	uint32_t numCurves;
	uint32_t numDataKeys;
	fa_anim_curve_t* curves;
	fa_anim_curve_key_t* dataKeys;	// all keys in the animation
	
	float motionDelta[8];	// single loop of motion for this anim clip (pos[4] xyzw, rot[4] ijkr)
} fa_anim_clip_t;

CANIM_API void fa_anim_clip_release(fa_anim_clip_t* clip, fc_alloc_callbacks_t* pAllocCallbacks);

CANIM_API void fa_anim_curve_sample(const fa_anim_curve_t* curve, float time, bool asAdditive, fm_xform* xform);

typedef struct fa_pose_t fa_pose_t;

CANIM_API void fa_anim_clip_sample(const fa_anim_clip_t* clip, float time, bool asAdditive, fa_pose_t* pose, const uint8_t* mask /* optional */);
CANIM_API void fa_anim_clip_sample_motion(const fa_anim_clip_t* clip, float timeBegin, float timeEnd, fm_xform* motion);

CANIM_API void fa_anim_clip_serialize(fc_serializer_t* pSerializer, fa_anim_clip_t* clip, fc_alloc_callbacks_t* pAllocCallbacks);

#ifdef __cplusplus
}
#endif // __cplusplus
