/* Copyright (c) 2016-2022 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include "ccore/types.h"

typedef struct fm_xform fm_xform;
typedef u32 FcStringId;
typedef struct FcAllocator FcAllocator;
typedef struct FcSerializer FcSerializer;

typedef struct FcAnimCurveKey
{
	u16 keyTime;
	u16 keyData[3];
} FcAnimCurveKey;

typedef struct FcAnimCurve
{
	u16 index;
	u16 numRotKeys;
	u16 numPosKeys;
	FcAnimCurveKey* rotKeys;
	FcAnimCurveKey* posKeys;
} FcAnimCurve;
	
typedef struct FcAnimClip
{
	FcStringId name;
	f32 duration;
	u32 numCurves;
	u32 numDataKeys;
	FcAnimCurve* curves;
	FcAnimCurveKey* dataKeys;	// all keys in the animation
	
	f32 motionDelta[8];	// single loop of motion for this anim clip (pos[4] xyzw, rot[4] ijkr)
} FcAnimClip;

CANIM_API void fcAnimClipRelease(FcAnimClip* clip, FcAllocator* allocator);

CANIM_API void fcAnimCurveSample(const FcAnimCurve* curve, f32 time, bool asAdditive, fm_xform* xform);

typedef struct FcPose FcPose;

CANIM_API void fcAnimClipSample(const FcAnimClip* clip, f32 time, bool asAdditive, FcPose* pose, const u8* mask /* optional */);
CANIM_API void fcAnimClipSampleMotion(const FcAnimClip* clip, f32 timeBegin, f32 timeEnd, fm_xform* motion);

CANIM_API void fcAnimClipSerialize(FcSerializer* pSerializer, FcAnimClip* clip, FcAllocator* allocator);

#ifdef __cplusplus
}
#endif // __cplusplus
