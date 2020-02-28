/* Copyright (c) 2016-2020 Furball Cat */

#include "animation.h"
#include "ccore/public.h"
#include "cmath/public.h"

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
