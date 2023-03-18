/* Copyright (c) 2016-2022 Furball Cat */

#include "pbd.h"
#include "ccore/public.h"
#include "cmath/public.h"
#include <string.h>

void fa_dangle_simulate_single_step(fa_dangle* dangle, f32 dt)
{
	const u32 count = dangle->numParaticles;
	
	const fm_vec4 gravity = {0.0f, 0.0f, dt * -5.0f};
	const f32 damping_coef = dangle->damping;
	
	dangle->p[0] = dangle->x0[0];
	
	for(u32 i=1; i<count; ++i)	// start from 1, as 0 is attach point
	{
		// v = v + dt * g
		fm_vec4_add(&dangle->v[i], &gravity, &dangle->v[i]);
		
		// damping velocity
		fm_vec4_mulf(&dangle->v[i], damping_coef, &dangle->v[i]);
		
		// p = x0 + dt * v
		fm_vec4 vel = dangle->v[i];
		fm_vec4_mulf(&vel, dt, &vel);
		fm_vec4_add(&dangle->x0[i], &vel, &dangle->p[i]);
	}
	
	const u32 numIterations = 4;
	for(u32 it=0; it<numIterations; ++it)
	{
		for(u32 i=1; i<count; ++i)	// start from 1, as 0 is attach point
		{
			const fm_vec4 p0 = dangle->p[i-1];
			const fm_vec4 p1 = dangle->p[i];
			
			// distance constraint
			const f32 refDistance = dangle->d[i-1];
			fm_vec4 disp;
			fm_vec4_sub(&p1, &p0, &disp);
			
			const f32 distance = fm_vec4_mag(&disp);
			fm_vec4_normalize(&disp);
			
			const f32 constraintDist = refDistance - distance;
			fm_vec4_mulf(&disp, constraintDist, &disp);
			
			fm_vec4_add(&dangle->p[i], &disp, &dangle->p[i]);
			
			// sphere collision constraint
			if(dangle->spherePos)
			{
				const fm_vec4 spherePos = *dangle->spherePos;
				const f32 sphereRadius = dangle->sphereRadius;
				
				fm_vec4 sphereDir;
				fm_vec4_sub(&dangle->p[i], &spherePos, &sphereDir);
				f32 sphereDist = fm_vec4_mag(&sphereDir);
				if(sphereDist < sphereRadius)
				{
					fm_vec4_normalize(&sphereDir);
					fm_vec4_mulf(&sphereDir, sphereRadius - sphereDist, &sphereDir);
					fm_vec4_add(&dangle->p[i], &sphereDir, &dangle->p[i]);
				}
			}
		}
	}
	
	const f32 inv_dt = (dt > 0.00000001f) ? 1.0f / dt : 0.0f;
	for(u32 i=1; i<count; ++i)	// start from 1, as 0 is attach point
	{
		fm_vec4_sub(&dangle->p[i], &dangle->x0[i], &dangle->v[i]);
		fm_vec4_mulf(&dangle->v[i], inv_dt, &dangle->v[i]);
		dangle->x0[i] = dangle->p[i];
	}
}

void fa_dangle_create(const fa_dangle_desc* desc, fa_dangle* dangle, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_ASSERT(!dangle->x0 && !dangle->p && !dangle->v && !dangle->d);
	
	dangle->x0 = FUR_ALLOC_ARRAY_AND_ZERO(fm_vec4, desc->numParticles, 16, FC_MEMORY_SCOPE_PHYSICS, pAllocCallbacks);
	dangle->p = FUR_ALLOC_ARRAY_AND_ZERO(fm_vec4, desc->numParticles, 16, FC_MEMORY_SCOPE_PHYSICS, pAllocCallbacks);
	dangle->v = FUR_ALLOC_ARRAY_AND_ZERO(fm_vec4, desc->numParticles, 16, FC_MEMORY_SCOPE_PHYSICS, pAllocCallbacks);
	dangle->d = FUR_ALLOC_ARRAY_AND_ZERO(f32, desc->numParticles-1, 16, FC_MEMORY_SCOPE_PHYSICS, pAllocCallbacks);
	
	dangle->freq = desc->frequency;
	dangle->numParaticles = desc->numParticles;
	dangle->tAcc = 0.0f;
	dangle->damping = desc->dampingCoef;
}

void fa_dangle_release(fa_dangle* dangle, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_ASSERT(dangle->x0 && dangle->p && dangle->v && dangle->d);
	
	FUR_FREE(dangle->x0, pAllocCallbacks);
	FUR_FREE(dangle->p, pAllocCallbacks);
	FUR_FREE(dangle->v, pAllocCallbacks);
	FUR_FREE(dangle->d, pAllocCallbacks);
	
	dangle->x0 = NULL;
	dangle->p = NULL;
	dangle->v = NULL;
	dangle->d = NULL;
}

void fa_dangle_simulate(const fa_dangle_sim_ctx* ctx, fa_dangle* dangle)
{
	dangle->tAcc += ctx->dt;
	const f32 timeStep = 1.0f / dangle->freq;
	
	while(dangle->tAcc >= timeStep)
	{
		dangle->tAcc -= timeStep;
		fa_dangle_simulate_single_step(dangle, timeStep);
	}
}

void fa_dangle_to_matrices_z_up(const fa_dangle* dangle, const fm_mat4* attachmentMatrix, fm_mat4* matrices)
{
	const u32 count = dangle->numParaticles - 1;
	const fm_vec4* p = dangle->p;
	
	matrices[0] = *attachmentMatrix;
	matrices[0].w = p[0];
	matrices[0].w.w = 1.0f;
	
	fm_vec4 refDir = attachmentMatrix->x;
	
	for(u32 i=1; i<count; ++i)
	{
		fm_vec4 z;
		fm_vec4_sub(&p[i], &p[i+1], &z);
		fm_vec4_normalize(&z);
		
		fm_vec4 y;
		fm_vec4_cross(&z, &refDir, &y);
		fm_vec4_normalize(&y);
		
		fm_vec4 x;
		fm_vec4_cross(&y, &z, &x);
		fm_vec4_normalize(&x);
		
		matrices[i].x = x;
		matrices[i].y = y;
		matrices[i].z = z;
		matrices[i].w = p[i];
		matrices[i].w.w = 1.0f;
		
		refDir = matrices[i].x;
	}
	
	matrices[count] = matrices[count-1];
	matrices[count].w = p[count];
	matrices[count].w.w = 1.0f;
}

void fa_dangle_to_matrices_y_down(const fa_dangle* dangle, const fm_mat4* attachmentMatrix, fm_mat4* matrices)
{
	const u32 count = dangle->numParaticles - 1;
	const fm_vec4* p = dangle->p;
	
	fm_vec4 refDir = attachmentMatrix->x;
	
	for(u32 i=0; i<count; ++i)
	{
		fm_vec4 y;
		fm_vec4_sub(&p[i+1], &p[i], &y);
		fm_vec4_normalize(&y);
		
		fm_vec4 z;
		fm_vec4_cross(&refDir, &y, &z);
		fm_vec4_normalize(&z);
		
		fm_vec4 x;
		fm_vec4_cross(&y, &z, &x);
		fm_vec4_normalize(&x);
		
		matrices[i].x = x;
		matrices[i].y = y;
		matrices[i].z = z;
		matrices[i].w = p[i];
		matrices[i].w.w = 1.0f;
		
		refDir = matrices[i].x;
	}
	
	matrices[count] = matrices[count-1];
	matrices[count].w = p[count];
	matrices[count].w.w = 1.0f;
}
