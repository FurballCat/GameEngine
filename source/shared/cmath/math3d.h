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
#include "ccore/types.h"
#include "cmath/mathtypes.h"

#define FM_PI 3.14159265358979323846264338327950288
#define FM_PI_2 1.57079632679489661923
#define FM_DEG_TO_RAD(_x) _x * FM_PI / 180.0
	
	static inline void xm_vec4_add(const xm_vec4 v1, const xm_vec4 v2, xm_vec4* result);
	static inline void xm_vec4_sub(const xm_vec4 v1, const xm_vec4 v2, xm_vec4* result);
	static inline void xm_vec4_mul(const xm_vec4 v1, const xm_vec4 v2, xm_vec4* result);
	static inline void xm_vec4_div(const xm_vec4 v1, const xm_vec4 v2, xm_vec4* result);
	static inline f32 xm_vec4_dot(const xm_vec4 v1, const xm_vec4 v2);
	
	/***** VECTOR *****/
	
#define FM_VEC4_AXIS_X {1.0f, 0.0f, 0.0f, 0.0f}
#define FM_VEC4_AXIS_Y {0.0f, 1.0f, 0.0f, 0.0f}
#define FM_VEC4_AXIS_Z {0.0f, 0.0f, 1.0f, 0.0f}
	
	// dot product between a and b saved to v
	static inline f32 fm_vec3_dot(const fm_vec3* a, const fm_vec3* b);
	
	// add b to a and save to v
	static inline void fm_vec3_add(const fm_vec3* a, const fm_vec3* b, fm_vec3* v);

	// subtract b from a and save to v
	static inline void fm_vec3_sub(const fm_vec3* a, const fm_vec3* b, fm_vec3* v);

	// set all vector components to zeros
	static inline void fm_vec4_zeros(fm_vec4* v);
	
	// add b to a and save to v
	static inline void fm_vec4_add(const fm_vec4* a, const fm_vec4* b, fm_vec4* v);
	
	// subtract b from a and save to v
	static inline void fm_vec4_sub(const fm_vec4* a, const fm_vec4* b, fm_vec4* v);
	
	// negate vector (v = -v)
	static inline void fm_vec4_neg(fm_vec4* v);

	// magnitude of a vector
	static inline f32 fm_vec4_mag(const fm_vec4* v);

	// magniture squared of a vector
	static inline f32 fm_vec4_mag2(const fm_vec4* v);

	// abs for vector elements
	static inline void fm_vec4_abs(fm_vec4* v);
	
	// dot product between a and b saved to v
	static inline f32 fm_vec4_dot(const fm_vec4* a, const fm_vec4* b);
	
	// multiply vector v by scalar t and save to output
	static inline void fm_vec4_mulf(const fm_vec4* v, const f32 t, fm_vec4* output);
	
	// cross product between a and b saved to v
	static inline void fm_vec4_cross(const fm_vec4* a, const fm_vec4* b, fm_vec4* v);
	
	// normalize vector
	static inline void fm_vec4_normalize(fm_vec4* v);
	
	// linear interpolation between a (0.0f) and b (1.0f), output to c
	static inline void fm_vec4_lerp(const fm_vec4* a, const fm_vec4* b, f32 alpha, fm_vec4* c);
	
	// shortest rotation between vectors 'from' and 'to'
	static inline void fm_vec4_rot_between(const fm_vec4* from, const fm_vec4* to, fm_quat* rot);

	// distance between a and b (always positive)
	static inline f32 fm_vec4_distance(const fm_vec4* a, const fm_vec4* b);

	/***** MATRIX *****/
	
	// identity matrix
	static inline void fm_mat4_identity(fm_mat4* m);
	
	// rotation matrix around axis x
	static inline void fm_mat4_rot_x(const f32 phi, fm_mat4* m);
	
	// rotation matrix around axis y
	static inline void fm_mat4_rot_y(const f32 phi, fm_mat4* m);
	
	// rotation matrix around axis Z
	static inline void fm_mat4_rot_z(const f32 phi, fm_mat4* m);
	
	// look at matrix for graphics (view)
	static inline void fm_mat4_lookat(const fm_vec4* eye, const fm_vec4* at, const fm_vec4* up, fm_mat4* m);
	
	// projection matrix, b - bottom, t - top, l - left, r - right, n - near, f - far
	static inline void fm_mat4_projection(const f32 b, const f32 t, const f32 l, const f32 r,
							const f32 n, const f32 f, fm_mat4* m);
	
	// projection matrix based on fov and aspect ratio
	static inline void fm_mat4_projection_fov(const f32 fov, const f32 aspectRatio,
								const f32 near, const f32 far, fm_mat4* m);
	
	// transpose matrix
	static inline void fm_mat4_transpose(fm_mat4* m);
	
	// multiply matrix a by b and save to m
	static inline void fm_mat4_mul(const fm_mat4* a, const fm_mat4* b, fm_mat4* m);
	
	// transform vector or position by matrix
	static inline void fm_mat4_transform(const fm_mat4* m, const fm_vec4* a, fm_vec4* b);
	
	// clamps value to range min..max
	static inline f32 fm_clamp(const f32 value, const f32 min, const f32 max);
	
	// snap to 0.0f when absolute value is lower than threashold
	static inline f32 fm_snap_near_zero(const f32 value, const f32 threshold);
	
	// return -1 if < 0, 1 if >= 0
	static inline f32 fm_sign(const f32 value);

	/***** QUATERNION *****/
	
	// quaternion identity
	static inline void fm_quat_identity(fm_quat* q);
	
	// dot of ijk, but not r
	static inline f32 fm_quat_dot3(const fm_quat* a, const fm_quat* b);
	
	// cross quaternions ijk
	static inline void fm_quat_cross3(const fm_quat* a, const fm_quat* b, fm_vec4* v);
	
	// multiply a by b, output to c
	static inline void fm_quat_mul(const fm_quat* a, const fm_quat* b, fm_quat* c);
	
	// rotate vector b by quaternion a, output vector to c
	static inline void fm_quat_rot(const fm_quat* q, const fm_vec4* v, fm_vec4* c);
	
	// normalize quaternion
	static inline void fm_quat_norm(fm_quat* q);
	
	// spherical interpolation between a (0.0f) and b (1.0f), output to c
	static inline void fm_quat_slerp(const fm_quat* a, const fm_quat* b, f32 alpha, fm_quat* c);
	
	// linear interpolation between a (0.0f) and b (1.0f), output to c
	static inline void fm_quat_lerp(const fm_quat* a, const fm_quat* b, f32 alpha, fm_quat* c);
	
	// convert quaternion to rotation matrix
	static inline void fm_quat_to_mat4(const fm_quat* q, fm_mat4* m);

	// convert quaternion to euler angles
	static inline void fm_quat_to_euler(const fm_quat* q, fm_euler_angles* e);
	
	// make quaternion from axis and angle
	static inline void fm_quat_rot_axis_angle(const fm_vec4* axis, const f32 angle, fm_quat* q);

	// convert quaternion to axis and angle
	static inline void fm_quat_to_axis_angle(const fm_quat* q, fm_vec4* axis, f32* angle);
	
	// make quaternion from axis and angle
	static inline void fm_quat_make_from_axis_angle(f32 x, f32 y, f32 z, const f32 angle, fm_quat* q);
	
	// make quaternion from euler angles (yaw - x, pitch - y, roll - z)
	static inline void fm_quat_make_from_euler_angles_xyz(const fm_euler_angles* angles, fm_quat* quat);
	
	// make quaternion from euler angles (yaw - z, pitch - x, roll - y)
	static inline void fm_quat_make_from_euler_angles_yzpxry(const fm_euler_angles* angles, fm_quat* quat);
	
	// make quaternion from euler angles (pitch - y, yaw - z, roll - x)
	static inline void fm_quat_make_from_euler_angles_pyyzrx(const fm_euler_angles* angles, fm_quat* quat);
	
	// conjugate of quaternion (inverse)
	static inline void fm_quat_conj(fm_quat* q);

	// x-axis rotated by quaternion q
	static inline fm_vec4 fm_quat_axis_x(fm_quat* q);

	// y-axis rotated by quaternion q
	static inline fm_vec4 fm_quat_axis_y(fm_quat* q);

	// z-axis rotated by quaternion q
	static inline fm_vec4 fm_quat_axis_z(fm_quat* q);

	/***** TRANSFORM *****/
	
	// set x to identity (pos 0,0,0,0 rot 0,0,0,1)
	static inline void fm_xform_identity(fm_xform* x);
	
	// multiply transforms a and, output to c
	static inline void fm_xform_mul(const fm_xform* a, const fm_xform* b, fm_xform* c);
	
	// linear interpolation between a (0.0f) and b (1.0f), output to c
	static inline void fm_xform_lerp(const fm_xform* a, const fm_xform* b, f32 alpha, fm_xform* c);
	
	// spherical interpolation for rotation and linear for position between a (0.0f) and b (1.0f), output to c
	static inline void fm_xform_slerp(const fm_xform* a, const fm_xform* b, f32 alpha, fm_xform* c);
	
	// convert transform to matrix
	static inline void fm_xform_to_mat4(const fm_xform* x, fm_mat4* m);
	
	// apply xform to vec4 a and store in v
	static inline void fm_xform_apply(const fm_xform* x, const fm_vec4* a, fm_vec4* v);

	// apply inverse of xform to vec4 a and store in v
	static inline void fm_xform_apply_inv(const fm_xform* x, const fm_vec4* a, fm_vec4* v);

	/***** SPLINES *****/
	
	// Catmull-Rom spline - interpolation is done only between P1 and P2
	static inline void fm_spline_catmull_rom(const fm_vec4* p0, const fm_vec4* p1, const fm_vec4* p2, const fm_vec4* p3,
											 const f32 t, fm_vec4* output );
	
	// uniform-s curve in range 0..1
	static inline f32 fm_curve_uniform_s(f32 alpha);

	/***** Bounding Boxes *****/

	// intersection test between two boxes
	static inline bool fm_intersection_box_box(const fm_box* a, const fm_box* b);

	// intersection test between a box and a point
	static inline bool fm_intersection_box_point(const fm_box* a, const fm_vec3* b);

	// append box b to box a, so box a will bound box b too
	static inline void fm_box_append(fm_box* a, const fm_box* b);

#ifdef __cplusplus
}
#endif // __cplusplus
