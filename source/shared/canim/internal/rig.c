/* Copyright (c) 2016-2020 Furball Cat */

#include "rig.h"
#include "ccore/public.h"
#include "ccore/serialize.h"
#include "cmath/public.h"
#include <string.h>
#include <stdio.h>

#define MIN(x, y) x < y ? x : y

void fcRigRelease(FcRig* rig, FcAllocator* pAllocCallbacks)
{
	FUR_FREE(rig->boneNameHashes, pAllocCallbacks);
	FUR_FREE(rig->parents, pAllocCallbacks);
	FUR_FREE(rig->refPose, pAllocCallbacks);
	
	if(rig->maskUpperBody)
		FUR_FREE(rig->maskUpperBody, pAllocCallbacks);
	if(rig->maskFace)
		FUR_FREE(rig->maskFace, pAllocCallbacks);
	if(rig->maskHands)
		FUR_FREE(rig->maskHands, pAllocCallbacks);
	
	FUR_FREE(rig, pAllocCallbacks);
}

int16_t fcRigFindBoneIdx(const FcRig* rig, FcStringId name)
{
	for(i32 i=0; i<rig->numBones; ++i)
	{
		if(rig->boneNameHashes[i] == name)
			return i;
	}
	
	return -1;
}

const u8* fcRigGetMask(const FcRig* rig, FcAnimMask mask)
{
	if(mask == FA_MASK_UPPER_BODY)
		return rig->maskUpperBody;
	else if(mask == FA_MASK_FACE)
		return rig->maskFace;
	else if(mask == FA_MASK_HANDS)
		return rig->maskHands;
	
	return NULL;
}

void fm_axis_to_vec4(fm_axis_t axis, fm_vec4* v)
{
	static fm_vec4 axes[6] = {
		{1, 0, 0, 0},
		{0, 1, 0, 0},
		{0, 0, 1, 0},
		{-1, 0, 0, 0},
		{0, -1, 0, 0},
		{0, 0, -1, 0},
	};
	
	*v = axes[axis];
}
/*
typedef struct FcRig
{
	FcStringId* boneNameHashes;
	int16_t* parents;
	fm_xform* refPose;
	u32 numBones;
	
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
*/

typedef enum FcRigVersion
{
	FA_RIG_VER_BASE = 0,
	FA_RIG_VER_LAST,
} FcRigVersion;

void fcRigIkSetupSerialize(FcSerializer* pSerializer, FcAnimIKSetup* ikSetup, FcAllocator* pAllocCallbacks)
{
	FUR_SER_ADD(FA_RIG_VER_BASE, ikSetup->idxBegin);
	FUR_SER_ADD(FA_RIG_VER_BASE, ikSetup->idxMid);
	FUR_SER_ADD(FA_RIG_VER_BASE, ikSetup->idxEnd);
	FUR_SER_ADD(FA_RIG_VER_BASE, ikSetup->idxBeginParent);
	FUR_SER_ADD(FA_RIG_VER_BASE, ikSetup->hingeAxisMid);
	FUR_SER_ADD(FA_RIG_VER_BASE, ikSetup->maxAngle);
	FUR_SER_ADD(FA_RIG_VER_BASE, ikSetup->minAngle);
}

void fcRigSerialize(FcSerializer* pSerializer, FcRig* rig, FcAllocator* pAllocCallbacks)
{
	FUR_SER_VERSION(FA_RIG_VER_LAST-1);
	
	// basics of rig
	FUR_SER_ADD(FA_RIG_VER_BASE, rig->numBones);
	FUR_SER_ADD(FA_RIG_VER_BASE, rig->idxLocoJoint);
	
	if(!pSerializer->isWriting)
	{
		rig->boneNameHashes = FUR_ALLOC_ARRAY_AND_ZERO(FcStringId, rig->numBones, 0, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
		rig->refPose = FUR_ALLOC_ARRAY_AND_ZERO(fm_xform, rig->numBones, 8, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
		rig->parents = FUR_ALLOC_ARRAY_AND_ZERO(i16, rig->numBones, 8, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
		rig->maskUpperBody = FUR_ALLOC_ARRAY_AND_ZERO(u8, rig->numBones, 8, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
		rig->maskHands = FUR_ALLOC_ARRAY_AND_ZERO(u8, rig->numBones, 8, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
		rig->maskFace = FUR_ALLOC_ARRAY_AND_ZERO(u8, rig->numBones, 8, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
	}
	
	FUR_SER_ADD_BUFFER(FA_RIG_VER_BASE, rig->boneNameHashes, sizeof(FcStringId) * rig-> numBones);
	FUR_SER_ADD_BUFFER(FA_RIG_VER_BASE, rig->refPose, sizeof(fm_xform) * rig-> numBones);
	FUR_SER_ADD_BUFFER(FA_RIG_VER_BASE, rig->parents, sizeof(i16) * rig-> numBones);
	FUR_SER_ADD_BUFFER(FA_RIG_VER_BASE, rig->maskUpperBody, sizeof(u8) * rig-> numBones);
	FUR_SER_ADD_BUFFER(FA_RIG_VER_BASE, rig->maskHands, sizeof(u8) * rig-> numBones);
	FUR_SER_ADD_BUFFER(FA_RIG_VER_BASE, rig->maskFace, sizeof(u8) * rig-> numBones);
	
	// inverse kinematics
	fcRigIkSetupSerialize(pSerializer, &rig->ikLeftLeg, pAllocCallbacks);
	fcRigIkSetupSerialize(pSerializer, &rig->ikRightLeg, pAllocCallbacks);
	
	// look at
	FUR_SER_ADD(FA_RIG_VER_BASE, rig->headLookAt.idxHead);
	FUR_SER_ADD(FA_RIG_VER_BASE, rig->headLookAt.idxNeck);
	FUR_SER_ADD(FA_RIG_VER_BASE, rig->headLookAt.idxSpine3);
	FUR_SER_ADD(FA_RIG_VER_BASE, rig->headLookAt.limitPitchDown);
	FUR_SER_ADD(FA_RIG_VER_BASE, rig->headLookAt.limitPitchUp);
	FUR_SER_ADD(FA_RIG_VER_BASE, rig->headLookAt.limitYaw);
}
