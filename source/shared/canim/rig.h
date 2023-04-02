/* Copyright (c) 2016-2022 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include "enums.h"
#include "ccore/types.h"

typedef struct fm_xform fm_xform;
typedef struct fm_vec4 fm_vec4;
typedef u32 FcStringId;
typedef struct FcAllocator FcAllocator;
typedef struct FcSerializer FcSerializer;

void fm_axis_to_vec4(fm_axis_t axis, fm_vec4* v);

typedef struct FcAnimIKSetup
{
	u16 idxBeginParent;
	u16 idxBegin;
	u16 idxMid;
	u16 idxEnd;
	fm_axis_t hingeAxisMid;
	f32 minAngle;
	f32 maxAngle;
} FcAnimIKSetup;

typedef struct FcAnimLookAtSetup
{
	u16 idxHead;
	u16 idxNeck;
	u16 idxSpine3;
	
	// half-angle limits in radians
	f32 limitYaw;
	f32 limitPitchDown;
	f32 limitPitchUp;
} FcAnimLookAtSetup;

typedef struct FcRig
{
	FcStringId* boneNameHashes;
	int16_t* parents;
	fm_xform* refPose;
	i32 numBones;
	
	// locomotion
	int16_t idxLocoJoint;	// root motion joint index
	
	// inverse kinematics
	FcAnimIKSetup ikLeftLeg;
	FcAnimIKSetup ikRightLeg;
	
	// look-at
	FcAnimLookAtSetup headLookAt;
	
	// masks
	u8* maskUpperBody;
	u8* maskFace;
	u8* maskHands;
} FcRig;

CANIM_API void fcRigRelease(FcRig* rig, FcAllocator* pAllocCallbacks);
CANIM_API int16_t fcRigFindBoneIdx(const FcRig* rig, FcStringId name);
CANIM_API const u8* fcRigGetMask(const FcRig* rig, FcAnimMask mask);
CANIM_API void fcRigSerialize(FcSerializer* pSerializer, FcRig* rig, FcAllocator* pAllocCallbacks);

#ifdef __cplusplus
}
#endif // __cplusplus
