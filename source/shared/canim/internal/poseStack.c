/* Copyright (c) 2016-2022 Furball Cat */

#include "poseStack.h"
#include "pose.h"
#include "ccore/public.h"
#include "cmath/public.h"
#include <string.h>

CANIM_API void fa_pose_stack_init(fa_pose_stack_t* pStack, const fa_pose_stack_desc_t* desc, void* buffer, u32 bufferSize)
{
	FUR_ASSERT(pStack);
	FUR_ASSERT(pStack->buffer == NULL);
	
	const u32 sizeXforms = desc->numBonesPerPose * sizeof(fm_xform);
	const u32 sizeWeightXforms = desc->numBonesPerPose * sizeof(u8);
	const u32 sizeTracks = desc->numTracksPerPose * sizeof(f32);
	const u32 sizeWeightTracks = desc->numTracksPerPose * sizeof(u8);
	
	u32 poseSize = sizeXforms + sizeTracks + sizeWeightXforms + sizeWeightTracks;
	poseSize += 16 - poseSize % 16;	// align pose size to 16 (add padding, so the next pose will be aligned to 16)
	u32 bufferSizeRequired = poseSize * desc->numMaxPoses;
	
	FUR_ASSERT(bufferSize >= bufferSizeRequired);
	
	pStack->buffer = buffer;
	pStack->bufferSize = bufferSizeRequired;
	pStack->numBones = desc->numBonesPerPose;
	pStack->numTracks = desc->numTracksPerPose;
	pStack->numMaxPoses = desc->numMaxPoses;
	pStack->numPoses = 0;
	pStack->poseSize = poseSize;
	pStack->offsetTracks = sizeXforms;
	pStack->offsetWeightXforms = sizeXforms + sizeTracks;
	pStack->offsetWeightTracks = sizeXforms + sizeTracks + sizeWeightXforms;
}

CANIM_API void fa_pose_stack_release(fa_pose_stack_t* pStack)
{
	FUR_ASSERT(pStack->buffer != NULL);
	
	memset(pStack, 0, sizeof(fa_pose_stack_t));
}

CANIM_API void fa_pose_stack_push(fa_pose_stack_t* pStack, u32 count)
{
	FUR_ASSERT(pStack->buffer != NULL);
	FUR_ASSERT(pStack->numPoses + count <= pStack->numMaxPoses);
	
	pStack->numPoses += count;
}

CANIM_API void fa_pose_stack_pop(fa_pose_stack_t* pStack, u32 count)
{
	FUR_ASSERT(pStack->buffer != NULL);
	FUR_ASSERT(pStack->numPoses >= count);
	
	pStack->numPoses -= count;
}

CANIM_API void fa_pose_stack_get(const fa_pose_stack_t* pStack, fa_pose_t* pPose, u32 depth)
{
	FUR_ASSERT(pStack->buffer != NULL);
	FUR_ASSERT(pStack->numPoses > depth);
	
	const u32 poseIndex = pStack->numPoses - 1 - depth;
	const u32 poseOffset = poseIndex * pStack->poseSize;
	u8* poseData = (const u8*)pStack->buffer + poseOffset;
	
	pPose->numXforms = pStack->numBones;
	pPose->numTracks = pStack->numTracks;
	pPose->flags = 0;	// todo: we should store flags somewhere
	
	if(pStack->numBones > 0)
	{
		pPose->xforms = (fm_xform*)poseData;
		pPose->weightsXforms = (u8*)(poseData + pStack->offsetWeightXforms);
	}
	else
	{
		pPose->xforms = NULL;
		pPose->weightsXforms = NULL;
	}
	
	if(pStack->numTracks > 0)
	{
		pPose->tracks = (f32*)(poseData + pStack->offsetTracks);
		pPose->weightsTracks = (u8*)(poseData + pStack->offsetWeightTracks);
	}
	else
	{
		pPose->tracks = NULL;
		pPose->weightsTracks = NULL;
	}
	
}
