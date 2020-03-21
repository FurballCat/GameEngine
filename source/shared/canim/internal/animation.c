/* Copyright (c) 2016-2020 Furball Cat */

#include "animation.h"
#include "ccore/public.h"
#include "cmath/public.h"
#include <string.h>

#define MIN(x, y) x < y ? x : y

// -----

void fa_pose_set_identity(fa_pose_t* pose)
{
	fm_xform* xforms = pose->xforms;
	for(uint32_t i=0; i<pose->numXforms; ++i)
	{
		fm_xform_identity(&xforms[i]);
	}
	
	memset(pose->tracks, 0, sizeof(float) * pose->numTracks);
}

void fa_pose_set_reference(const fa_rig_t* rig, fa_pose_t* pose)
{
	FUR_ASSERT(rig->numBones == pose->numXforms);
	
	const fm_xform* refXForms = rig->refPose;
	fm_xform* xforms = pose->xforms;
	
	memcpy(xforms, refXForms, sizeof(fm_xform) * pose->numXforms);
	memset(pose->tracks, 0, sizeof(float) * pose->numTracks);
}

// -----

#define POSE_ALIGNMENT 16

struct fa_pose_stack_t
{
	uint8_t* buffer;
	uint16_t numPoses;
	uint16_t maxPoses;
	uint32_t bufferSize;
	
	uint16_t numBonesPerPose;
	uint16_t numTracksPerPose;
};

static inline uint32_t fa_pose_stack_get_transforms_size_in_bytes(const fa_pose_stack_t* stack)
{
	return stack->numBonesPerPose * sizeof(fm_xform);
}

static inline uint32_t fa_pose_stack_get_tracks_size_in_bytes(const fa_pose_stack_t* stack)
{
	return stack->numTracksPerPose * sizeof(float);
}

static inline uint32_t fa_pose_stack_get_pose_size_in_bytes(const fa_pose_stack_t* stack)
{
	uint32_t sizeWithoutPadding = fa_pose_stack_get_transforms_size_in_bytes(stack) + fa_pose_stack_get_tracks_size_in_bytes(stack);
	return sizeWithoutPadding + (sizeWithoutPadding % POSE_ALIGNMENT);
}

void fa_pose_stack_push(fa_pose_stack_t* stack, uint32_t count)
{
	FUR_ASSERT(stack->numPoses < stack->maxPoses);
	stack->numPoses += 1;
}

void fa_pose_stack_pop(fa_pose_stack_t* stack, uint32_t count)
{
	FUR_ASSERT(stack->numPoses > 0);
	stack->numPoses -= 1;
}

void fa_pose_stack_get(const fa_pose_stack_t* stack, uint32_t depth, fa_pose_t* pose)
{
	const uint32_t xformsSize = fa_pose_stack_get_transforms_size_in_bytes(stack);
	const uint32_t poseSize = fa_pose_stack_get_pose_size_in_bytes(stack);
	uint8_t* ptr = stack->buffer + (stack->numPoses - depth) * poseSize;
	
	pose->xforms = (fm_xform*)ptr;
	pose->tracks = (float*)(ptr + xformsSize);
	pose->numXforms = stack->numBonesPerPose;
	pose->numTracks = stack->numTracksPerPose;
}

// -----

static inline uint16_t fa_anim_clip_key_get_bone_index(const fa_anim_curve_t* curve)
{
	return curve->index & 0x3fff;
}

static inline uint16_t fa_anim_clip_key_get_channel(const fa_anim_curve_t* curve)
{
	return (curve->index & 0xc000) >> 14;
}

float fa_decompress_float_minus_one_plus_one(uint16_t value)
{
	return (((float)value) / 65535.0f) * 2.0f - 1.0f;
}

void fa_decompress_rotation_key(const fa_anim_curve_key_t* key, fm_quat* rot)
{
	const uint16_t* keyData = key->keyData;
	bool isLastComponentMinus = key->keyTime & 0x4000;
	
	rot->i = fa_decompress_float_minus_one_plus_one(keyData[0]);
	rot->j = fa_decompress_float_minus_one_plus_one(keyData[1]);
	rot->k = fa_decompress_float_minus_one_plus_one(keyData[2]);
	
	float r = 1.0f - rot->i * rot->i + rot->j * rot->j + rot->k * rot->k;
	r = sqrtf(r);
	if(isLastComponentMinus)
	{
		r = -r;
	}
	
	rot->r = r;
}

float fa_decompress_key_time(const uint16_t time)
{
	return ((float)time) / 30.0f;
}

void fa_anim_clip_sample(const fa_anim_clip_t* clip, float time, fa_pose_t* pose)
{
	const uint32_t numCurves = clip->numCurves;
	
	const uint16_t qTime = (uint32_t)(time * 30.0f);
	//const float fraction = time * 30.0f - qTime;
	
	for(uint32_t i_c=0; i_c<numCurves; ++i_c)
	{
		const fa_anim_curve_t* curve = &clip->curves[i_c];
		const uint16_t numKeys = curve->numKeys;
	
		uint16_t idx = 0;
		
		// todo: make it a binary search (or a binary-guess search, check weekly links)
		while(idx < numKeys && (curve->keys[idx].keyTime & 0x3FFF) < qTime)
		{
			++idx;
		}
		
		const uint16_t upperIdx = MIN(idx + 1, numKeys-1);
		
		fm_quat rot;
		
		if(idx == upperIdx)
		{
			fa_decompress_rotation_key(&curve->keys[idx], &rot);
		}
		else
		{
			fm_quat rot1;
			fa_decompress_rotation_key(&curve->keys[idx], &rot1);
			
			fm_quat rot2;
			fa_decompress_rotation_key(&curve->keys[upperIdx], &rot2);
			
			const float time1 = fa_decompress_key_time(curve->keys[idx].keyTime);
			const float time2 = fa_decompress_key_time(curve->keys[upperIdx].keyTime);
			
			float alpha = (time - time1) / (time2 - time1);
			fm_quat_lerp(&rot1, &rot2, alpha, &rot);
			fm_quat_norm(&rot);
		}
		
		uint16_t idxXform = curve->index;
		pose->xforms[idxXform].rot = rot;
	}
}

// -----

void fa_pose_copy(const fa_pose_t* src, fa_pose_t* dest)
{
	const uint32_t numXforms = MIN(src->numXforms, dest->numXforms);
	if(numXforms > 0)
	{
		memcpy(dest->xforms, src->xforms, sizeof(fm_xform) * numXforms);
	}
	
	const uint32_t numTracks = MIN(src->numTracks, dest->numTracks);
	if(numTracks > 0)
	{
		memcpy(dest->tracks, src->tracks, sizeof(float) * numTracks);
	}
}

void fa_pose_local_to_model(const fa_pose_t* localPose, const int16_t* parentIndices, fa_pose_t* modelPose)
{
	const fm_xform* localXforms = localPose->xforms;
	fm_xform* modelXforms = modelPose->xforms;
	
	uint32_t numBones = MIN(modelPose->numXforms, localPose->numXforms);
	
	// iterate non-root bones
	for(uint16_t i = 0; i < numBones; ++i)
	{
		const int16_t idxParent = parentIndices[i];
		if(idxParent >= 0)
		{
			//fm_xform_mul(&localXforms[i], &modelXforms[idxParent], &modelXforms[i]);
			fm_xform_mul(&modelXforms[idxParent], &localXforms[i], &modelXforms[i]);
		}
		else
		{
			modelXforms[i] = localXforms[i];
		}
	}
	
	// copy tracks
	uint32_t numTracks = MIN(modelPose->numTracks, localPose->numTracks);
	if(numTracks > 0)
	{
		memcpy(modelPose->tracks, localPose->tracks, sizeof(float) * numTracks);
	}
}
