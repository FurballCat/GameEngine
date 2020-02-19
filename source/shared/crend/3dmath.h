/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

// requires <math.h>
/*
#ifndef __MATH_H__
#error "Missing math.h include in source file before this file."
#endif
*/

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include <immintrin.h>
	
#define FM_PI 3.14159265358979323846264338327950288
#define FM_DEG_TO_RAD(_x) _x * FM_PI / 180.0
	
typedef struct fm_vec4
{
	float x, y, z, w;
} fm_vec4;
	
typedef struct fm_mat4
{
	fm_vec4 x, y, z, w;
} fm_mat4_t;

typedef struct fm_quat
{
	float i, j, k, r;
} fm_quat;
	
typedef struct fm_xform
{
	fm_vec4 pos;
	fm_quat rot;
} fm_xform;
	
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
	
void xm_vec4_add(const xm_vec4 v1, const xm_vec4 v2, xm_vec4* result);
void xm_vec4_sub(const xm_vec4 v1, const xm_vec4 v2, xm_vec4* result);
void xm_vec4_mul(const xm_vec4 v1, const xm_vec4 v2, xm_vec4* result);
void xm_vec4_div(const xm_vec4 v1, const xm_vec4 v2, xm_vec4* result);
float xm_vec4_dot(const xm_vec4 v1, const xm_vec4 v2);
	
/***** VECTOR *****/

#define FM_VEC4_AXIS_X {1.0f, 0.0f, 0.0f, 0.0f}
#define FM_VEC4_AXIS_Y {0.0f, 1.0f, 0.0f, 0.0f}
#define FM_VEC4_AXIS_Z {0.0f, 0.0f, 1.0f, 0.0f}
	
// add b to a and save to v
void fm_vec4_add(const fm_vec4* a, const fm_vec4* b, fm_vec4* v);
	
// subtract b from a and save to v
void fm_vec4_sub(const fm_vec4* a, const fm_vec4* b, fm_vec4* v);

// magnitude of a vector
float fm_vec4_mag(const fm_vec4* v);

// dot product between a and b saved to v
float fm_vec4_dot(const fm_vec4* a, const fm_vec4* b);

// multiply vector v by scalar t and save to output
void fm_vec4_mulf(const fm_vec4* v, const float t, fm_vec4* output);
	
// cross product between a and b saved to v
void fm_vec4_cross(const fm_vec4* a, const fm_vec4* b, fm_vec4* v);
	
// normalize vector
void fm_vec4_normalize(fm_vec4* v);
	
// linear interpolation between a (0.0f) and b (1.0f), output to c
void fm_vec4_lerp(const fm_vec4* a, const fm_vec4* b, float alpha, fm_vec4* c);

/***** MATRIX *****/
	
// identity matrix
void fm_mat4_identity(fm_mat4_t* m);
	
// rotation matrix around axis x
void fm_mat4_rot_x(const float phi, fm_mat4_t* m);

// rotation matrix around axis y
void fm_mat4_rot_y(const float phi, fm_mat4_t* m);
	
// rotation matrix around axis Z
void fm_mat4_rot_z(const float phi, fm_mat4_t* m);
	
// look at matrix for graphics (view)
void fm_mat4_lookat(const fm_vec4* eye, const fm_vec4* at, const fm_vec4* up, fm_mat4_t* m);
	
// projection matrix, b - bottom, t - top, l - left, r - right, n - near, f - far
void fm_mat4_projection(const float b, const float t, const float l, const float r,
						const float n, const float f, fm_mat4_t* m);

// projection matrix based on fov and aspect ratio
void fm_mat4_projection_fov(const float fov, const float aspectRatio,
							const float near, const float far, fm_mat4_t* m);

// transpose matrix
void fm_mat4_transpose(fm_mat4_t* m);

// multiply matrix a by b and save to m
void fm_mat4_mul(const fm_mat4_t* a, const fm_mat4_t* b, fm_mat4_t* m);

// clamps value to range min..max
float fm_clamp(const float value, const float min, const float max);
	
/***** QUATERNION *****/
	
// quaternion identity
void fm_quat_identity(fm_quat* q);

// dot of ijk, but not r
float fm_quat_dot3(const fm_quat* a, const fm_quat* b);
	
// cross quaternions ijk
void fm_quat_cross3(const fm_quat* a, const fm_quat* b, fm_vec4* v);
	
// multiply a by b, output to c
void fm_quat_mul(const fm_quat* a, const fm_quat* b, fm_quat* c);
	
// rotate vector b by quaternion a, output vector to c
void fm_quat_rot(const fm_quat* a, const fm_vec4* b, fm_vec4* c);
	
// normalize quaternion
void fm_quat_norm(fm_quat* q);
	
// spherical interpolation between a (0.0f) and b (1.0f), output to c
void fm_quat_slerp(const fm_quat* a, const fm_quat* b, float alpha, fm_quat* c);
	
// linear interpolation between a (0.0f) and b (1.0f), output to c
void fm_quat_lerp(const fm_quat* a, const fm_quat* b, float alpha, fm_quat* c);

// convert quaternion to rotation matrix
void fm_quat_to_mat4(const fm_quat* q, fm_mat4_t* m);

// make quaternion from axis and angle
void fm_quat_rot_axis_angle(const fm_vec4* axis, const float angle, fm_quat* q);

/***** TRANSFORM *****/
	
// multiply transforms a and, output to c
void fm_xform_mul(const fm_xform* a, const fm_xform* b, fm_xform* c);
	
// linear interpolation between a (0.0f) and b (1.0f), output to c
void fm_xform_lerp(const fm_xform* a, const fm_xform* b, float alpha, fm_xform* c);
	
// spherical interpolation for rotation and linear for position between a (0.0f) and b (1.0f), output to c
void fm_xform_slerp(const fm_xform* a, const fm_xform* b, float alpha, fm_xform* c);
	
	
/***** SPLINES *****/

// Catmull-Rom spline - interpolation is done only between P1 and P2
void fm_spline_catmull_rom(const fm_vec4* p0, const fm_vec4* p1, const fm_vec4* p2, const fm_vec4* p3, const float t, fm_vec4* output );
	
#ifdef __cplusplus
}
#endif // __cplusplus
