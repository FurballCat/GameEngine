/* Copyright (c) 2016-2022 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include <inttypes.h>

typedef struct fm_vec4 fm_vec4;
typedef struct fm_mat4 fm_mat4;
typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;

// Position Based Dynamics

// Dangles - single chain of particles with distance constraints, colliding against sphere
typedef struct fa_dangle
{
	fm_vec4* x0;
	fm_vec4* p;
	fm_vec4* v;
	float* d;	// distance costraint, segments length, size=numParticles-1

	uint32_t numParaticles;
	float tAcc;
	float freq;
	float damping;
	
	// collision
	fm_vec4* spherePos;
	float sphereRadius;
} fa_dangle;

typedef struct fa_dangle_sim_ctx
{
	float dt;
} fa_dangle_sim_ctx;

typedef struct fa_dangle_desc
{
	uint32_t numParticles;
	float frequency;
	float dampingCoef;
} fa_dangle_desc;

CANIM_API void fa_dangle_create(const fa_dangle_desc* desc, fa_dangle* dangle, fc_alloc_callbacks_t* pAllocCallbacks);
CANIM_API void fa_dangle_release(fa_dangle* dangle, fc_alloc_callbacks_t* pAllocCallbacks);

CANIM_API void fa_dangle_simulate(const fa_dangle_sim_ctx* ctx, fa_dangle* dangle);
CANIM_API void fa_dangle_to_matrices_z_up(const fa_dangle* dangle, const fm_mat4* attachmentMatrix, fm_mat4* matrices);
CANIM_API void fa_dangle_to_matrices_y_down(const fa_dangle* dangle, const fm_mat4* attachmentMatrix, fm_mat4* matrices);

#ifdef __cplusplus
}
#endif // __cplusplus
