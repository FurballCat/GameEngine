/* Copyright (c) 2016-2020 Furball Cat */

#include "rig.h"
#include "ccore/public.h"
#include "ccore/serialize.h"
#include "cmath/public.h"
#include <string.h>
#include <stdio.h>

#define MIN(x, y) x < y ? x : y

void fa_rig_release(fa_rig_t* rig, fc_alloc_callbacks_t* pAllocCallbacks)
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

int16_t fa_rig_find_bone_idx(const fa_rig_t* rig, fc_string_hash_t name)
{
	for(int32_t i=0; i<rig->numBones; ++i)
	{
		if(rig->boneNameHashes[i] == name)
			return i;
	}
	
	return -1;
}

const uint8_t* fa_rig_get_mask(const fa_rig_t* rig, fa_mask_t mask)
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
*/

typedef enum fa_rig_version_t
{
	FA_RIG_VER_BASE = 0,
	FA_RIG_VER_LAST,
} fa_rig_version_t;

void fa_rig_ik_setup_serialize(fc_serializer_t* pSerializer, fa_ik_setup_t* ikSetup, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_SER_ADD(FA_RIG_VER_BASE, ikSetup->idxBegin);
	FUR_SER_ADD(FA_RIG_VER_BASE, ikSetup->idxMid);
	FUR_SER_ADD(FA_RIG_VER_BASE, ikSetup->idxEnd);
	FUR_SER_ADD(FA_RIG_VER_BASE, ikSetup->idxBeginParent);
	FUR_SER_ADD(FA_RIG_VER_BASE, ikSetup->hingeAxisMid);
	FUR_SER_ADD(FA_RIG_VER_BASE, ikSetup->maxAngle);
	FUR_SER_ADD(FA_RIG_VER_BASE, ikSetup->minAngle);
}

void fa_rig_serialize(fc_serializer_t* pSerializer, fa_rig_t* rig, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_SER_VERSION(FA_RIG_VER_LAST-1);
	
	// basics of rig
	FUR_SER_ADD(FA_RIG_VER_BASE, rig->numBones);
	FUR_SER_ADD(FA_RIG_VER_BASE, rig->idxLocoJoint);
	
	if(!pSerializer->isWriting)
	{
		rig->boneNameHashes = FUR_ALLOC_ARRAY_AND_ZERO(fc_string_hash_t, rig->numBones, 0, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
		rig->refPose = FUR_ALLOC_ARRAY_AND_ZERO(fm_xform, rig->numBones, 8, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
		rig->parents = FUR_ALLOC_ARRAY_AND_ZERO(int16_t, rig->numBones, 8, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
		rig->maskUpperBody = FUR_ALLOC_ARRAY_AND_ZERO(uint8_t, rig->numBones, 8, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
		rig->maskHands = FUR_ALLOC_ARRAY_AND_ZERO(uint8_t, rig->numBones, 8, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
		rig->maskFace = FUR_ALLOC_ARRAY_AND_ZERO(uint8_t, rig->numBones, 8, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
	}
	
	FUR_SER_ADD_BUFFER(FA_RIG_VER_BASE, rig->boneNameHashes, sizeof(fc_string_hash_t) * rig-> numBones);
	FUR_SER_ADD_BUFFER(FA_RIG_VER_BASE, rig->refPose, sizeof(fm_xform) * rig-> numBones);
	FUR_SER_ADD_BUFFER(FA_RIG_VER_BASE, rig->parents, sizeof(int16_t) * rig-> numBones);
	FUR_SER_ADD_BUFFER(FA_RIG_VER_BASE, rig->maskUpperBody, sizeof(uint8_t) * rig-> numBones);
	FUR_SER_ADD_BUFFER(FA_RIG_VER_BASE, rig->maskHands, sizeof(uint8_t) * rig-> numBones);
	FUR_SER_ADD_BUFFER(FA_RIG_VER_BASE, rig->maskFace, sizeof(uint8_t) * rig-> numBones);
	
	// inverse kinematics
	fa_rig_ik_setup_serialize(pSerializer, &rig->ikLeftLeg, pAllocCallbacks);
	fa_rig_ik_setup_serialize(pSerializer, &rig->ikRightLeg, pAllocCallbacks);
	
	// look at
	FUR_SER_ADD(FA_RIG_VER_BASE, rig->headLookAt.idxHead);
	FUR_SER_ADD(FA_RIG_VER_BASE, rig->headLookAt.idxNeck);
	FUR_SER_ADD(FA_RIG_VER_BASE, rig->headLookAt.idxSpine3);
	FUR_SER_ADD(FA_RIG_VER_BASE, rig->headLookAt.limitPitchDown);
	FUR_SER_ADD(FA_RIG_VER_BASE, rig->headLookAt.limitPitchUp);
	FUR_SER_ADD(FA_RIG_VER_BASE, rig->headLookAt.limitYaw);
}
