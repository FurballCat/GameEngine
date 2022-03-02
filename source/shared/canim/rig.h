/* Copyright (c) 2016-2022 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include "enums.h"
#include <inttypes.h>

typedef struct fm_xform fm_xform;
typedef struct fm_vec4 fm_vec4;
typedef uint32_t fc_string_hash_t;
typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;

void fm_axis_to_vec4(fm_axis_t axis, fm_vec4* v);

typedef struct fa_ik_setup_t
{
	uint16_t idxBeginParent;
	uint16_t idxBegin;
	uint16_t idxMid;
	uint16_t idxEnd;
	fm_axis_t hingeAxisMid;
	float minAngle;
	float maxAngle;
} fa_ik_setup_t;

typedef struct fa_look_at_setup_t
{
	uint16_t idxHead;
	uint16_t idxNeck;
	uint16_t idxSpine3;
	
	// half-angle limits in radians
	float limitYaw;
	float limitPitchDown;
	float limitPitchUp;
} fa_look_at_setup_t;

typedef struct fa_rig_t
{
	fc_string_hash_t* boneNameHashes;
	int16_t* parents;
	fm_xform* refPose;
	uint32_t numBones;
	
	// locomotion
	int16_t idxLocoJoint;	// root motion joint index
	
	// inverse kinematics
	fa_ik_setup_t ikLeftLeg;
	fa_ik_setup_t ikRightLeg;
	
	// look-at
	fa_look_at_setup_t headLookAt;
	
	// masks
	uint8_t* maskUpperBody;
	uint8_t* maskFace;
	uint8_t* maskHands;
} fa_rig_t;

CANIM_API void fa_rig_release(fa_rig_t* rig, fc_alloc_callbacks_t* pAllocCallbacks);
CANIM_API int16_t fa_rig_find_bone_idx(const fa_rig_t* rig, fc_string_hash_t name);
CANIM_API const uint8_t* fa_rig_get_mask(const fa_rig_t* rig, fa_mask_t mask);

#ifdef __cplusplus
}
#endif // __cplusplus
