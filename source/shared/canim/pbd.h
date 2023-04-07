/* Copyright (c) 2016-2022 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include "ccore/types.h"

typedef struct fm_vec4 fm_vec4;
typedef struct fm_mat4 fm_mat4;
typedef struct FcAllocator FcAllocator;

// Position Based Dynamics

// Dangles - single chain of particles with distance constraints, colliding against sphere
typedef struct FcPBDDangle
{
	fm_vec4* x0;
	fm_vec4* p;
	fm_vec4* v;
	f32* d;	// distance costraint, segments length, size=numParticles-1

	u32 numParaticles;
	f32 tAcc;
	f32 freq;
	f32 damping;
	
	// collision
	fm_vec4* spherePos;
	f32 sphereRadius;
} FcPBDDangle;

typedef struct FcPBDDangleCtx
{
	f32 dt;
} FcPBDDangleCtx;

typedef struct FcPBDDangleDesc
{
	u32 numParticles;
	f32 frequency;
	f32 dampingCoef;
} FcPBDDangleDesc;

CANIM_API void fcPBDDangleCreate(const FcPBDDangleDesc* desc, FcPBDDangle* dangle, FcAllocator* allocator);
CANIM_API void fcPBDDangleRelease(FcPBDDangle* dangle, FcAllocator* allocator);

CANIM_API void fcPBDDangleSimulate(const FcPBDDangleCtx* ctx, FcPBDDangle* dangle);
CANIM_API void fcPBDDangleToMatricesZUp(const FcPBDDangle* dangle, const fm_mat4* attachmentMatrix, fm_mat4* matrices);
CANIM_API void fcPBDDangleToMatricesYDown(const FcPBDDangle* dangle, const fm_mat4* attachmentMatrix, fm_mat4* matrices);

#ifdef __cplusplus
}
#endif // __cplusplus
