/* Copyright (c) 2016-2022 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include "ccore/types.h"

typedef struct fm_xform fm_xform;

typedef enum FcPoseFlags
{
	PF_ADDITIVE = 0x1,
} FcPoseFlags;
	
typedef struct FcPose
{
	fm_xform* xforms;
	f32* tracks;
	u8* weightsXforms;
	u8* weightsTracks;
	
	u16 numXforms;
	u16 numTracks;
	
	u32 flags;
} FcPose;
	
// -----

typedef struct FcRig FcRig;

CANIM_API void FcPoseSetIdentity(FcPose* pose, const u8* mask /* optional */);
CANIM_API void FcPoseSetReference(const FcRig* rig, FcPose* pose, const u8* mask /* optional */);
	
// -----

CANIM_API void FcPoseCopy(FcPose* dest, const FcPose* src);
CANIM_API void FcPoseLocalToModel(FcPose* modelPose, const FcPose* localPose, const int16_t* parentIndices);
	
CANIM_API void FcPoseBlendLinear(FcPose* out, const FcPose* a, const FcPose* b, f32 alpha);
CANIM_API void FcPoseApplyAdditive(FcPose* out, const FcPose* base, const FcPose* add, f32 weight);

// -----

#ifdef __cplusplus
}
#endif // __cplusplus
