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

#define FM_PI 3.14159265358979323846264338327950288
#define FM_DEG_TO_RAD(_x) _x * FM_PI / 180.0
	
typedef struct fm_vec4
{
	float x, y, z, w;
} fm_vec4;
	
typedef struct fm_mat4
{
	fm_vec4 x, y, z, w;
} fm_mat4;

/***** VECTOR *****/
	
// subtract b from a and save to v
void fm_vec4_sub(const fm_vec4* a, const fm_vec4* b, fm_vec4* v);

// magnitude of a vector
float fm_vec4_mag(const fm_vec4* v);

// dot product between a and b saved to v
float fm_vec4_dot(const fm_vec4* a, const fm_vec4* b);
	
// cross product between a and b saved to v
void fm_vec4_cross(const fm_vec4* a, const fm_vec4* b, fm_vec4* v);
	
// normalize vector
void fm_vec4_normalize(fm_vec4* v);

/***** MATRIX *****/
	
// identity matrix
void fm_mat4_identity(fm_mat4* m);
	
// rotation matrix around axis x
void fm_mat4_rot_x(const float phi, fm_mat4* m);

// rotation matrix around axis y
void fm_mat4_rot_y(const float phi, fm_mat4* m);
	
// rotation matrix around axis Z
void fm_mat4_rot_z(const float phi, fm_mat4* m);
	
// look at matrix for graphics (view)
void fm_mat4_lookat(const fm_vec4* eye, const fm_vec4* at, const fm_vec4* up, fm_mat4* m);
	
// projection matrix, b - bottom, t - top, l - left, r - right, n - near, f - far
void fm_mat4_projection(const float b, const float t, const float l, const float r,
						const float n, const float f, fm_mat4* m);

// projection matrix based on fov and aspect ratio
void fm_mat4_projection_fov(const float fov, const float aspectRatio,
							const float near, const float far, fm_mat4* m);

// transpose matrix
void fm_mat4_transpose(fm_mat4* m);

// multiply matrix a by b and save to m
void fm_mat4_mul(const fm_mat4* a, const fm_mat4* b, fm_mat4* m);
	
#ifdef __cplusplus
}
#endif // __cplusplus
