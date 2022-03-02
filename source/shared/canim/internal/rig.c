/* Copyright (c) 2016-2020 Furball Cat */

#include "rig.h"
#include "ccore/public.h"
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
