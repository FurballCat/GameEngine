/* Copyright (c) Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus
	
#include <immintrin.h>

typedef struct fm_vec2
{
	float x, y;
} fm_vec2;

typedef struct fm_vec3
{
	float x, y, z;
} fm_vec3;

typedef struct fm_vec4
{
	float x, y, z, w;
} fm_vec4;

typedef struct fm_mat4
{
	fm_vec4 x, y, z, w;
} fm_mat4;

typedef struct fm_quat
{
	float i, j, k, r;
} fm_quat;

typedef struct fm_xform
{
	fm_vec4 pos;
	fm_quat rot;
} fm_xform;

typedef struct fm_euler_angles
{
	float yaw, pitch, roll;
} fm_euler_angles;

// axis-aligned bounding box
typedef struct fm_box
{
	fm_vec3 center;
	fm_vec3 extent;
} fm_box;

// frustum
typedef struct fm_frustum
{
	fm_vec4 leftPlane;
	fm_vec4 rightPlane;
	fm_vec4 topPlane;
	fm_vec4 bottomPlane;
	fm_vec4 nearPlane;
	fm_vec4 farPlane;
} fm_frustum;

// vector math version (intrin)
typedef __m128 xm_float4_v;

typedef struct xm_vec3
{
	xm_float4_v vec128;
} xm_vec3;

typedef struct xm_vec4
{
	xm_float4_v vec128;
} xm_vec4;

typedef struct xm_mat3
{
	xm_vec3 c0;
	xm_vec3 c1;
	xm_vec3 c2;
	xm_vec3 c3;
} xm_mat3;

typedef struct xm_mat4
{
	xm_vec4 c0;
	xm_vec4 c1;
	xm_vec4 c2;
	xm_vec4 c3;
} xm_mat4;
	
#ifdef __cplusplus
}
#endif // __cplusplus
