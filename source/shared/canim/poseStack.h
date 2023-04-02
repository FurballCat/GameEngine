/* Copyright (c) 2016-2022 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include "ccore/types.h"

typedef struct FcPose FcPose;

typedef struct FcPoseStack
{
	void* buffer;
	u32 bufferSize;
	u32 poseSize;

	u32 numPoses;
	u32 numMaxPoses;

	u32 numBones;
	u32 numTracks;

	u32 offsetTracks;
	u32 offsetWeightXforms;
	u32 offsetWeightTracks;
} FcPoseStack;

typedef struct FcPoseStackDesc
{
	u32 numMaxPoses;

	u32 numBonesPerPose;
	u32 numTracksPerPose;
} FcPoseStackDesc;

CANIM_API void FcPoseStackInit(FcPoseStack* pStack, const FcPoseStackDesc* desc, void* buffer, u32 bufferSize);
CANIM_API void FcPoseStackRelease(FcPoseStack* pStack);

CANIM_API void FcPoseStackPush(FcPoseStack* pStack, u32 count);
CANIM_API void FcPoseStackPop(FcPoseStack* pStack, u32 count);
CANIM_API void FcPoseStackGet(const FcPoseStack* pStack, FcPose* pPose, u32 depth);

#ifdef __cplusplus
}
#endif // __cplusplus
