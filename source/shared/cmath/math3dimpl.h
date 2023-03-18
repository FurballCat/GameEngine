/* Copyright (c) 2016-2020 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include <math.h>
#include "cmath/mathtypes.h"

#define MAX(a, b) a > b ? a : b
#define MIN(a, b) a < b ? a : b

#define FM_SLERP_TOL 0.995f

static inline void xm_vec4_add(const xm_vec4 v1, const xm_vec4 v2, xm_vec4* result)
{
	result->vec128 = _mm_add_ps(v1.vec128, v2.vec128);
}

static inline void xm_vec4_sub(const xm_vec4 v1, const xm_vec4 v2, xm_vec4* result)
{
	result->vec128 = _mm_sub_ps(v1.vec128, v2.vec128);
}

static inline void xm_vec4_mul(const xm_vec4 v1, const xm_vec4 v2, xm_vec4* result)
{
	result->vec128 = _mm_mul_ps(v1.vec128, v2.vec128);
}

static inline void xm_vec4_div(const xm_vec4 v1, const xm_vec4 v2, xm_vec4* result)
{
	result->vec128 = _mm_div_ps(v1.vec128, v2.vec128);
}

static inline f32 xm_vec4_dot(const xm_vec4 v1, const xm_vec4 v2)
{
	//xm_float4_v result;
	//result = _mm_fmadd_ps(v1.vec128, v2.vec128, (xm_float4_v){0.0f, 0.0f, 0.0f, 0.0f});
	// todo: finish implementation
	
	return 0.0f;
}

//////////////////////////////////////////////////
// generic (C11 overloading style) math functions

#define fm_add(a, b) _Generic((a)) \
	fm_vec3: fm_vec3_add, \
	fm_vec4: fm_vec4_add, \
	fm_quat: fm_quat_add, \
	)(a, b)

#define fm_sub(a, b) _Generic((a)) \
	fm_vec3: fm_vec3_sub, \
	fm_vec4: fm_vec4_sub, \
	)(a, b)

#define fm_mul(a, b) _Generic((a) + (b)) \
	fm_vec3 + fm_vec3: fm_vec3_mul, \
	fm_vec3 + f32: fm_vec3_mulf, \
	fm_vec4 + fm_vec4: fm_vec4_mul, \
	fm_vec4 + f32: fm_vec4_mulf, \
	fm_quat + fm_quat: fm_quat_mul, \
	fm_quat + f32: fm_quat_mulf, \
	fm_xform + fm_xform: fm_xform_mul, \
	)(a, b)

#define fm_norm(a) _Generic((a)) \
	fm_vec3: fm_vec3_norm, \
	fm_vec4: fm_vec4_norm, \
	fm_quat: fm_quat_norm, \
	)(a)

#define fm_lerp(a, b, t) _Generic((a))	\
	fm_vec3: fm_vec3_lerp, \
	fm_vec4: fm_vec4_lerp, \
	fm_quat: fm_quat_lerp, \
	fm_xform: fm_xform_lerp, \
	)(a, b, t)

#define fm_slerp(a, b, t) _Generic((a))	\
	fm_quat: fm_quat_slerp, \
	fm_xform: fm_xform_slerp, \
	)(a, b, t)

//////////////////////////
// math functions by name

static inline f32 fm_vec3_dot(const fm_vec3 a, const fm_vec3 b)
{
	return a.x * b.x + a.y * b.y + a.z * b.z;
}

static inline fm_vec3 fm_vec3_add(const fm_vec3 a, const fm_vec3 b)
{
	const fm_vec3 res = { a.x + b.x, a.y + b.y, a.z + b.z };
	return res;
}

static inline fm_vec3 fm_vec3_sub(const fm_vec3 a, const fm_vec3 b)
{
	const fm_vec3 res = { a.x - b.x, a.y - b.y, a.z - b.z };
	return res;
}

static inline fm_vec3 fm_vec3_mul(const fm_vec3 a, const fm_vec3 b)
{
	const fm_vec3 res = { a.x * b.x, a.y * b.y, a.z * b.z };
	return res;
}

static inline fm_vec3 fm_vec3_mulf(const fm_vec3 a, const f32 s)
{
	const fm_vec3 res = { a.x * s, a.y * s, a.z * s };
	return res;
}

static inline f32 fm_vec3_mag(const fm_vec3 v)
{
	return sqrtf(v.x * v.x + v.y * v.y + v.z * v.z);
}
	
static inline fm_vec3 fm_vec3_norm(const fm_vec3 v)
{
	const f32 magnitude = fm_vec3_mag(v);
	const f32 magnitudeInv = 1.0f / magnitude;
	
	const fm_vec3 res = { v.x * magnitudeInv, v.y * magnitudeInv, v.z * magnitudeInv };
	return res;
}
	
static inline fm_vec3 fm_vec3_lerp(const fm_vec3 a, const fm_vec3 b, const f32 alpha)
{
	const f32 invAlpha = 1.0f - alpha;

	const fm_vec3 res = { a.x * alpha + b.x * invAlpha, a.y * alpha + b.y * invAlpha, a.z * alpha + b.z * invAlpha };
	return res;
}

static inline fm_vec4 fm_vec4_zeros()
{
	const fm_vec4 res = { 0.0f, 0.0f, 0.0f, 0.0f };
	return res;
}
	
static inline fm_vec4 fm_vec4_add(const fm_vec4 a, const fm_vec4 b)
{
	const fm_vec4 res = { a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w };
	return res;
}

static inline fm_vec4 fm_vec4_sub(const fm_vec4 a, const fm_vec4 b)
{
	const fm_vec4 res = { a.x - b.x, a.y - b.y, a.z - b.z, a.w - b.w };
	return res;
}

static inline fm_vec4 fm_vec4_mul(const fm_vec4 a, const fm_vec4 b)
{
	const fm_vec4 res = { a.x * b.x, a.y * b.y, a.z * b.z, a.w * b.w };
	return res;
}

static inline fm_vec4 fm_vec4_neg(const fm_vec4 v)
{
	const fm_vec4 res = { -v.x, -v.y, -v.z, -v.w };
	return res;
}

static inline f32 fm_vec4_mag(const fm_vec4 v)
{
	return sqrtf(v.x * v.x + v.y * v.y + v.z * v.z + v.w * v.w);
}

static inline f32 fm_vec4_mag2(const fm_vec4 v)
{
	return v.x * v.x + v.y * v.y + v.z * v.z + v.w * v.w;
}

static inline fm_vec4 fm_vec4_abs(const fm_vec4 v)
{
	const fm_vec4 res = { fabsf(v.x), fabsf(v.y), fabsf(v.z), fabsf(v.w) };
	return res;
}

static inline f32 fm_vec4_dot(const fm_vec4 a, const fm_vec4 b)
{
	return a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w;
}

static inline fm_vec4 fm_vec4_mulf(const fm_vec4 v, const f32 t)
{
	const fm_vec4 res = { v.x * t, v.y * t, v.z * t, v.w * t };
	return res;
}

static inline fm_vec4 fm_vec4_cross(const fm_vec4 a, const fm_vec4 b)
{
	const fm_vec4 res = { a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x, 0.0f };
	return res;
}

static inline fm_vec4 fm_vec4_norm(const fm_vec4 v)
{
	const f32 magnitude = fm_vec4_mag(v);
	const f32 magnitudeInv = 1.0f / magnitude;

	const fm_vec4 res = { v.x * magnitudeInv, v.y * magnitudeInv, v.z * magnitudeInv, v.w * magnitudeInv };
	return res;
}

static inline fm_vec4 fm_vec4_lerp(const fm_vec4 a, const fm_vec4 b, const f32 alpha)
{
	const f32 invAlpha = 1.0f - alpha;

	const fm_vec4 res = { a.x * alpha + b.x * invAlpha, a.y * alpha + b.y * invAlpha, a.z * alpha + b.z * invAlpha, a.w * alpha + b.w * invAlpha };
	return res;
}

static inline f32 fm_clamp(const f32 value, const f32 min, const f32 max)
{
	if (value < min)
		return min;

	if (value > max)
		return max;

	return value;
}

static inline f32 fm_snap_near_zero(const f32 value, const f32 threshold)
{
	if (fabsf(value) < threshold)
		return 0.0f;

	return value;
}

static inline f32 fm_sign(const f32 value)
{
	return value >= 0.0f ? 1.0f : -1.0f;
}

static inline fm_quat fm_quat_identity()
{
	const fm_quat res = { 0.0f, 0.0f, 0.0f, 1.0f };
	return res;
}

static inline f32 fm_quat_dot3(const fm_quat a, const fm_quat b)
{
	return a.i * b.i + a.j * b.j + a.k * b.k;
}

static inline f32 fm_quat_dot(const fm_quat a, const fm_quat b)
{
	return a.i * b.i + a.j * b.j + a.k * b.k + a.r * b.r;
}

static inline fm_vec4 fm_quat_cross3(const fm_quat a, const fm_quat b)
{
	const fm_vec4 res = { a.j * b.k - a.k * b.j, a.k * b.i - a.i * b.k, a.i * b.j - a.j * b.i, 0.0f };
	return res;
}

static inline fm_quat fm_quat_add(const fm_quat a, const fm_quat b)
{
	const fm_quat res = { a.i + b.i, a.j + b.j, a.k + b.k, a.r + b.r };
	return res;
}

static inline fm_quat fm_quat_mul(const fm_quat a, const fm_quat b)
{
	const f32 i = a.r * b.i + a.k * b.j - a.j * b.k + a.i * b.r;
	const f32 j = -a.k * b.i + a.r * b.j + a.i * b.k + a.j * b.r;
	const f32 k = a.j * b.i - a.i * b.j + a.r * b.k + a.k * b.r;
	const f32 r = -a.i * b.i - a.j * b.j - a.k * b.k + a.r * b.r;

	const fm_quat res = { i, j, k, r };
	return res;
}

static inline fm_quat fm_quat_mulf(const fm_quat a, const f32 t)
{
	const fm_quat res = { a.i * t, a.j * t, a.k * t, a.r * t };
	return res;
}

static inline fm_vec4 fm_quat_rot(const fm_quat q, const fm_vec4 v)
{
	const f32 dot_qv = v.x * q.i + v.y * q.j + v.z * q.k;
	const f32 mag_qv = q.i * q.i + q.j * q.j + q.k * q.k;

	const f32 s1 = 2.0f * dot_qv;
	const f32 s2 = q.r * q.r - mag_qv;
	const f32 s3 = 2.0f * q.r;

	const fm_vec4 qv = { q.i, q.j, q.k, 0.0f };
	const fm_vec4 cross_qv = fm_vec4_cross(v, qv);
	
	const fm_vec4 res = {
		s1 * qv.x + s2 * v.x + s3 * cross_qv.x,
		s1 * qv.y + s2 * v.y + s3 * cross_qv.y,
		s1 * qv.z + s2 * v.z + s3 * cross_qv.z,
		0.0f };

	return res;
}

static inline fm_quat fm_quat_norm(fm_quat q)
{
	const f32 n = sqrtf(q.i * q.i + q.j * q.j + q.k * q.k + q.r * q.r);
	const f32 n_inv = 1.0f / n;
	
	const fm_quat res = { q.i * n_inv, q.j * n_inv, q.k * n_inv, q.r * n_inv };
	return res;
}

static inline fm_quat fm_quat_neg(const fm_quat q)
{
	const fm_quat res = { -q.i, -q.j, -q.k, -q.r };
	return res;
}

static inline fm_quat fm_quat_make_from_axis_angle(const f32 x, const f32 y, const f32 z, const f32 angle)
{
	const f32 scale = sinf(angle / 2.0f) / sqrtf(x * x + y * y + z * z);

	const fm_quat res = { scale * x, scale * y, scale * z, cosf(angle / 2.0f) };
	return res;
}

static inline fm_quat fm_quat_make_from_euler_angles_yzpxry(const fm_euler_angles angles)
{
	const fm_quat p = fm_quat_make_from_axis_angle(1.0f, 0.0f, 0.0f, angles.pitch);
	const fm_quat r = fm_quat_make_from_axis_angle(0.0f, 1.0f, 0.0f, angles.roll);
	const fm_quat y = fm_quat_make_from_axis_angle(0.0f, 0.0f, 1.0f, angles.yaw);

	const fm_quat tmp = fm_quat_mul(p, y);
	return fm_quat_mul(tmp, r);
}

static inline fm_quat fm_quat_make_from_euler_angles_xyz(const fm_euler_angles angles)
{
	const fm_quat x = fm_quat_make_from_axis_angle(1.0f, 0.0f, 0.0f, angles.pitch);
	const fm_quat y = fm_quat_make_from_axis_angle(0.0f, 1.0f, 0.0f, angles.yaw);
	const fm_quat z = fm_quat_make_from_axis_angle(0.0f, 0.0f, 1.0f, angles.roll);

	const fm_quat tmp = fm_quat_mul(y, x);
	return fm_quat_mul(z, tmp);
}

static inline fm_quat fm_quat_make_from_euler_angles_pyyzrx(const fm_euler_angles angles)
{
	const fm_quat p = fm_quat_make_from_axis_angle(0.0f, 1.0f, 0.0f, angles.pitch);
	const fm_quat r = fm_quat_make_from_axis_angle(0.0f, 0.0f, 1.0f, angles.roll);
	const fm_quat y = fm_quat_make_from_axis_angle(1.0f, 0.0f, 0.0f, angles.yaw);

	const fm_quat tmp = fm_quat_mul(y, p);
	return fm_quat_mul(tmp, r);
}

static inline fm_quat fm_quat_make_from_euler_angles_pxryyz(const fm_euler_angles angles)
{
	const fm_quat p = fm_quat_make_from_axis_angle(0.0f, 1.0f, 0.0f, angles.pitch);
	const fm_quat r = fm_quat_make_from_axis_angle(1.0f, 0.0f, 0.0f, angles.roll);
	const fm_quat y = fm_quat_make_from_axis_angle(0.0f, 0.0f, 1.0f, angles.yaw);

	const fm_quat tmp = fm_quat_mul(r, p);
	return fm_quat_mul(tmp, y);
}

static inline fm_quat fm_vec4_rot_between(const fm_vec4 from, const fm_vec4 to)
{
	fm_quat rot = fm_quat_identity();

	const fm_vec4 fromNorm = fm_vec4_norm(from);
	const fm_vec4 toNorm = fm_vec4_norm(to);
	
	const f32 cosAlpha = fm_vec4_dot(fromNorm, toNorm);
	const f32 alpha = -acosf(cosAlpha);
	if(fabsf(alpha) > 0.0001f)
	{
		fm_vec4 axis = fm_vec4_cross(fromNorm, toNorm);
		if(fm_vec4_mag2(axis) > 0.0f)
		{
			axis = fm_vec4_norm(axis);

			rot = fm_quat_make_from_axis_angle(axis.x, axis.y, axis.z, alpha);
			rot = fm_quat_norm(rot);
		}
	}

	return rot;
}

static inline f32 fm_vec4_distance(const fm_vec4 a, const fm_vec4 b)
{
	const fm_vec4 diff = fm_vec4_sub(a, b);
	return fm_vec4_mag(diff);
}

static inline void fm_mat4_identity(fm_mat4* m)
{
	m->x.x = 1.0f;
	m->x.y = 0.0f;
	m->x.z = 0.0f;
	m->x.w = 0.0f;
	
	m->y.x = 0.0f;
	m->y.y = 1.0f;
	m->y.z = 0.0f;
	m->y.w = 0.0f;
	
	m->z.x = 0.0f;
	m->z.y = 0.0f;
	m->z.z = 1.0f;
	m->z.w = 0.0f;
	
	m->w.x = 0.0f;
	m->w.y = 0.0f;
	m->w.z = 0.0f;
	m->w.w = 1.0f;
}

static inline void fm_mat4_rot_x(const f32 phi, fm_mat4* m)
{
	m->x.x = 1; m->x.y = 0; m->x.z = 0; m->x.w = 0;
	m->y.x = 0; m->y.y = cosf(phi); m->y.z = -sinf(phi); m->y.w = 0;
	m->z.x = 0; m->z.y = sinf(phi); m->z.z = cosf(phi); m->z.w = 0;
	m->w.x = 0; m->w.y = 0; m->w.z = 0; m->w.w = 1;
}

static inline void fm_mat4_rot_y(const f32 phi, fm_mat4* m)
{
	m->x.x = cosf(phi); m->x.y = 0; m->x.z = sinf(phi); m->x.w = 0;
	m->y.x = 0; m->y.y = 1; m->y.z = 0; m->y.w = 0;
	m->z.x = -sinf(phi); m->z.y = 0; m->z.z = cosf(phi); m->z.w = 0;
	m->w.x = 0; m->w.y = 0; m->w.z = 0; m->w.w = 1;
}

static inline void fm_mat4_rot_z(const f32 phi, fm_mat4* m)
{
	m->x.x = cosf(phi); m->x.y = -sinf(phi); m->x.z = 0; m->x.w = 0;
	m->y.x = sinf(phi); m->y.y = cosf(phi); m->y.z = 0; m->y.w = 0;
	m->z.x = 0; m->z.y = 0; m->z.z = 1; m->z.w = 0;
	m->w.x = 0; m->w.y = 0; m->w.z = 0; m->w.w = 1;
}

static inline void fm_mat4_lookat_lh(const fm_vec4 eye, const fm_vec4 at, const fm_vec4 up, fm_mat4* m)
{
	const fm_vec4 axis_y = fm_vec4_norm(fm_vec4_sub(at, eye));
	const fm_vec4 axis_x = fm_vec4_norm(fm_vec4_cross(axis_y, up));
	const fm_vec4 axis_z = fm_vec4_cross(axis_x, axis_y);
	
	m->x.x = axis_x.x;
	m->x.y = axis_x.y;
	m->x.z = axis_x.z;
	m->x.w = -fm_vec4_dot(axis_x, eye);
	
	m->y.x = axis_y.x;
	m->y.y = axis_y.y;
	m->y.z = axis_y.z;
	m->y.w = -fm_vec4_dot(axis_y, eye);
	
	m->z.x = axis_z.x;
	m->z.y = axis_z.y;
	m->z.z = axis_z.z;
	m->z.w = -fm_vec4_dot(axis_z, eye);
	
	m->w.x = 0.0f;
	m->w.y = 0.0f;
	m->w.z = 0.0f;
	m->w.w = 1.0f;
}

static inline void fm_mat4_lookat_rh(const fm_vec4 eye, const fm_vec4 at, const fm_vec4 up, fm_mat4* m)
{
	const fm_vec4 axis_z = fm_vec4_norm(fm_vec4_sub(eye, at));
	const fm_vec4 axis_x = fm_vec4_norm(fm_vec4_cross(up, axis_z));
	const fm_vec4 axis_y = fm_vec4_cross(axis_z, axis_x);
	
	m->x.x = axis_x.x;
	m->x.y = axis_x.y;
	m->x.z = axis_x.z;
	m->x.w = fm_vec4_dot(axis_x, eye);
	
	m->y.x = axis_y.x;
	m->y.y = axis_y.y;
	m->y.z = axis_y.z;
	m->y.w = fm_vec4_dot(axis_y, eye);
	
	m->z.x = axis_z.x;
	m->z.y = axis_z.y;
	m->z.z = axis_z.z;
	m->z.w = fm_vec4_dot(axis_z, eye);
	
	m->w.x = 0.0f;
	m->w.y = 0.0f;
	m->w.z = 0.0f;
	m->w.w = 1.0f;
}
	
// projection matrix, b - bottom, t - top, l - left, r - right, n - near, f - far
static inline void fm_mat4_ortho_projection(const f32 b, const f32 t, const f32 l, const f32 r,
									  const f32 n, const f32 f, fm_mat4* m)
{
	// todo: check this math...
#if 1
	f32 sum_rl, sum_tb, sum_nf, inv_rl, inv_tb, inv_nf;
	sum_rl = (r + l);
	sum_tb = (t + b);
	sum_nf = (n + f);
	inv_rl = (1.0f / (r - l));
	inv_tb = (1.0f / (t - b));
	inv_nf = (1.0f / (n - f));
	
	m->x.x = inv_rl + inv_rl;
	m->x.y = 0.0f;
	m->x.z = 0.0f;
	m->x.w = -(r + l) / (r - l);
	
	m->y.x = 0.0f;
	m->y.y = inv_tb + inv_tb;
	m->y.z = 0.0f;
	m->y.w = (t + b) / (t - b);
	
	m->z.x = 0.0f;
	m->z.y = 0.0f;
	m->z.z = (1.0f / (n - f));
	m->z.w = 0.0f;
	
	m->w.x = 0.0f;
	m->w.y = 0.0f;
	m->w.z = -n / (n - f);
	m->w.w = 1.0f;
#else
	// calculate the width, height, and depth of the view frustum
	f32 width = r - l;
	f32 height = t - b;
	f32 depth = f - n;

	// calculate the translation factors
	f32 tx = -(r + l) / width;
	f32 ty = -(t + b) / height;
	f32 tz = -(f + n) / depth;

	m->x.x = 2.0f / width;
	m->x.y = 0.0f;
	m->x.z = 0.0f;
	m->x.w = 0.0f;

	m->y.x = 0.0f;
	m->y.y = 2.0f / height;
	m->y.z = 0.0f;
	m->y.w = 0.0f;

	m->z.x = 0.0f;
	m->z.y = 0.0f;
	m->z.z = -2.0f / depth;
	m->z.w = 0.0f;

	m->w.x = tx;
	m->w.y = ty;
	m->w.z = tz;
	m->w.w = 1.0f;
#endif
}
	
// projection matrix, b - bottom, t - top, l - left, r - right, n - near, f - far
static inline void fm_mat4_projection(const f32 b, const f32 t, const f32 l, const f32 r,
						const f32 n, const f32 f, fm_mat4* m)
{
	/*
	 return {{2 * n / (r - l), 0, (r + l) / (r - l), 0},
	 {0, 2 * n / (t - b), (t + b) / (t - b), 0},
	 {0, 0, -(f + n) / (f - n), -2 * f * n / (f - n)},
	 {0, 0, -1, 0}};
	 */
	
	m->x.x = 2 * n / (r - l);
	m->x.y = 0;
	m->x.z = (r + l) / (r - l);
	m->x.w = 0;
	
	m->y.x = 0;
	m->y.y = 2 * n / (t - b);
	m->y.z = (t + b) / (t - b);
	m->y.w = 0;
	
	m->z.x = 0;
	m->z.y = 0;
	m->z.z = -(f + n) / (f - n);
	m->z.w = -2 * f * n / (f - n);
	
	m->w.x = 0;
	m->w.y = 0;
	m->w.z = -1;
	m->w.w = 0;
}

// projection matrix based on fov and aspect ratio
static inline void fm_mat4_projection_fov(const f32 fov, const f32 aspectRatio,
							const f32 near, const f32 far, fm_mat4* m)
{
	const f32 scale = tanf(FM_DEG_TO_RAD(fov * 0.5f)) * near;
	const f32 top = scale;
	const f32 bottom = -top;
	const f32 right = aspectRatio * scale;
	const f32 left = -right;
	
	fm_mat4_projection(bottom, top, left, right, near, far, m);
}

static inline void fm_swapf(f32* a, f32* b)
{
	const f32 c = *a;
	*a = *b;
	*b = c;
}

static inline void fm_mat4_transpose(fm_mat4* m)
{
	fm_swapf(&m->x.y, &m->y.x);
	fm_swapf(&m->x.z, &m->z.x);
	fm_swapf(&m->x.w, &m->w.x);
	fm_swapf(&m->y.z, &m->z.y);
	fm_swapf(&m->y.w, &m->w.y);
	fm_swapf(&m->z.w, &m->w.z);
}

static inline void fm_mat4_mul(const fm_mat4* a, const fm_mat4* b, fm_mat4* m)
{
	const f32 m00 = a->x.x * b->x.x + a->x.y * b->y.x + a->x.z * b->z.x + a->x.w * b->w.x;
	const f32 m01 = a->x.x * b->x.y + a->x.y * b->y.y + a->x.z * b->z.y + a->x.w * b->w.y;
	const f32 m02 = a->x.x * b->x.z + a->x.y * b->y.z + a->x.z * b->z.z + a->x.w * b->w.z;
	const f32 m03 = a->x.x * b->x.w + a->x.y * b->y.w + a->x.z * b->z.w + a->x.w * b->w.w;
	
	const f32 m10 = a->y.x * b->x.x + a->y.y * b->y.x + a->y.z * b->z.x + a->y.w * b->w.x;
	const f32 m11 = a->y.x * b->x.y + a->y.y * b->y.y + a->y.z * b->z.y + a->y.w * b->w.y;
	const f32 m12 = a->y.x * b->x.z + a->y.y * b->y.z + a->y.z * b->z.z + a->y.w * b->w.z;
	const f32 m13 = a->y.x * b->x.w + a->y.y * b->y.w + a->y.z * b->z.w + a->y.w * b->w.w;
	
	const f32 m20 = a->z.x * b->x.x + a->z.y * b->y.x + a->z.z * b->z.x + a->z.w * b->w.x;
	const f32 m21 = a->z.x * b->x.y + a->z.y * b->y.y + a->z.z * b->z.y + a->z.w * b->w.y;
	const f32 m22 = a->z.x * b->x.z + a->z.y * b->y.z + a->z.z * b->z.z + a->z.w * b->w.z;
	const f32 m23 = a->z.x * b->x.w + a->z.y * b->y.w + a->z.z * b->z.w + a->z.w * b->w.w;
	
	const f32 m30 = a->w.x * b->x.x + a->w.y * b->y.x + a->w.z * b->z.x + a->w.w * b->w.x;
	const f32 m31 = a->w.x * b->x.y + a->w.y * b->y.y + a->w.z * b->z.y + a->w.w * b->w.y;
	const f32 m32 = a->w.x * b->x.z + a->w.y * b->y.z + a->w.z * b->z.z + a->w.w * b->w.z;
	const f32 m33 = a->w.x * b->x.w + a->w.y * b->y.w + a->w.z * b->z.w + a->w.w * b->w.w;
	
	m->x.x = m00;
	m->x.y = m01;
	m->x.z = m02;
	m->x.w = m03;
	
	m->y.x = m10;
	m->y.y = m11;
	m->y.z = m12;
	m->y.w = m13;
	
	m->z.x = m20;
	m->z.y = m21;
	m->z.z = m22;
	m->z.w = m23;
	
	m->w.x = m30;
	m->w.y = m31;
	m->w.z = m32;
	m->w.w = m33;
}
	
static inline fm_vec4 fm_mat4_transform(const fm_mat4* m, const fm_vec4 a)
{
	const fm_vec4 res = { fm_vec4_dot(a, m->x), fm_vec4_dot(a, m->y), fm_vec4_dot(a, m->z), fm_vec4_dot(a, m->w) };
	return res;
}
	
static inline fm_quat fm_quat_slerp(const fm_quat unitQuat0, const fm_quat unitQuat1, const f32 t)
{
	fm_quat start, tmpQ_0, tmpQ_1;
	f32 recipSinAngle, scale0, scale1, cosAngle, angle;
	cosAngle = fm_quat_dot( unitQuat0, unitQuat1 );
	if ( cosAngle < 0.0f )
	{
		cosAngle = -cosAngle;
		start = fm_quat_neg( unitQuat0 );
	}
	else
	{
		start = unitQuat0;
	}
	if ( cosAngle < FM_SLERP_TOL )
	{
		angle = acosf( cosAngle );
		recipSinAngle = ( 1.0f / sinf( angle ) );
		scale0 = ( sinf( ( ( 1.0f - t ) * angle ) ) * recipSinAngle );
		scale1 = ( sinf( ( t * angle ) ) * recipSinAngle );
	}
	else
	{
		scale0 = ( 1.0f - t );
		scale1 = t;
	}
	tmpQ_0 = fm_quat_mulf( start, scale0 );
	tmpQ_1 = fm_quat_mulf( unitQuat1, scale1 );
	return fm_quat_add( tmpQ_0, tmpQ_1 );
}

static inline fm_quat fm_quat_lerp(const fm_quat a, const fm_quat b, const f32 alpha)
{
	const f32 alpha_inv = 1.0f - alpha;
	const fm_quat res = {
		a.i * alpha_inv + b.i * alpha,
		a.j * alpha_inv + b.j * alpha,
		a.k * alpha_inv + b.k * alpha,
		a.r * alpha_inv + b.r * alpha };

	return res;
}

static inline void fm_quat_to_mat4(const fm_quat q, fm_mat4* m)
{
	m->x.x = 1.0f - 2.0f * q.j * q.j - 2.0f * q.k * q.k;
	m->x.y = 2.0f * q.i * q.j - 2.0f * q.k * q.r;
	m->x.z = 2.0f * q.i * q.k + 2.0f * q.j * q.r;
	m->x.w = 0.0f;
	
	m->y.x = 2.0f * q.i * q.j + 2.0f * q.k * q.r;
	m->y.y = 1.0f - 2.0f * q.i * q.i - 2.0f * q.k * q.k;
	m->y.z = 2.0f * q.j * q.k - 2.0f * q.i * q.r;
	m->y.w = 0.0f;
	
	m->z.x = 2.0f * q.i * q.k - 2.0f * q.j * q.r;
	m->z.y = 2.0f * q.j * q.k + 2.0f * q.i * q.r;
	m->z.z = 1.0f - 2.0f * q.i * q.i - 2.0f * q.j * q.j;
	m->z.w = 0.0f;
	
	m->w.x = 0.0f;
	m->w.y = 0.0f;
	m->w.z = 0.0f;
	m->w.w = 1.0f;
}

static inline fm_euler_angles fm_quat_to_euler(const fm_quat q)
{
	fm_euler_angles angles;

	// roll (x-axis rotation)
	f32 sinr_cosp = 2.0f * (q.r * q.i + q.j * q.k);
	f32 cosr_cosp = 1.0f - 2.0f * (q.i * q.i + q.j * q.j);
	angles.roll = atan2f(sinr_cosp, cosr_cosp);

	// pitch (y-axis rotation)
	f32 sinp = 2.0f * (q.r * q.j - q.k * q.i);
	if (fabs(sinp) >= 1.0f)
		angles.pitch = (f32)FM_PI / 2.0f * fm_sign(sinp); // use 90 degrees if out of range
	else
		angles.pitch = asinf(sinp);

	// yaw (z-axis rotation)
	f32 siny_cosp = 2.0f * (q.r * q.k + q.i * q.j);
	f32 cosy_cosp = 1.0f - 2.0f * (q.j * q.j + q.k * q.k);
	angles.yaw = atan2f(siny_cosp, cosy_cosp);

	return angles;
}

static inline fm_quat fm_quat_rot_axis_angle(const fm_vec4 axis, const f32 angle)
{
	const f32 scale = sinf(angle / 2) / fm_vec4_mag(axis);
	const fm_quat res = { scale * axis.x, scale * axis.y, scale * axis.z, cosf(angle / 2.0f) };
	return res;
}

static inline void fm_quat_to_axis_angle(const fm_quat q, fm_vec4* axis, f32* angle)
{
	*angle = acosf(fm_clamp(q.r, -1.0f, 1.0f)) * 2.0f;
	if(fabsf(*angle) > 0.0001f)
	{
		const f32 scale = sinf(*angle / 2.0f);
		axis->x = q.i / scale;
		axis->y = q.j / scale;
		axis->z = q.k / scale;
		axis->w = 0.0f;
	}
	else
	{
		axis->x = 0.0f;
		axis->y = 0.0f;
		axis->z = 0.0f;
		axis->w = 0.0f;
	}
}
	
static inline fm_quat fm_quat_conj(const fm_quat q)
{
	const fm_quat res = { -q.i, -q.j, -q.k, q.r };
	return res;
}

static inline fm_vec4 fm_quat_axis_x(const fm_quat q)
{
	const fm_vec4 axis_x = { 1.0f, 0.0f, 0.0f, 0.0f };
	const fm_vec4 res = fm_quat_rot(q, axis_x);
	return res;
}

static inline fm_vec4 fm_quat_axis_y(const fm_quat q)
{
	const fm_vec4 axis_y = { 0.0f, 1.0f, 0.0f, 0.0f };
	const fm_vec4 res = fm_quat_rot(q, axis_y);
	return res;
}

static inline fm_vec4 fm_quat_axis_z(const fm_quat q)
{
	const fm_vec4 axis_z = { 0.0f, 0.0f, 1.0f, 0.0f };
	const fm_vec4 res = fm_quat_rot(q, axis_z);
	return res;
}

static inline void fm_xform_identity(fm_xform* x)
{
	x->pos = fm_vec4_zeros();
	x->rot = fm_quat_identity();
}
	
static inline void fm_xform_mul(const fm_xform* a, const fm_xform* b, fm_xform* c)
{
	const fm_vec4 rotatedB = fm_quat_rot(a->rot, b->pos);

	c->pos = fm_vec4_add(a->pos, rotatedB);
	c->rot = fm_quat_mul(a->rot, b->rot);
}

static inline void fm_xform_lerp(const fm_xform* a, const fm_xform* b, const f32 alpha, fm_xform* c)
{
	c->pos = fm_vec4_lerp(b->pos, a->pos, alpha);
	c->rot = fm_quat_lerp(a->rot, b->rot, alpha);
}

static inline void fm_xform_slerp(const fm_xform* a, const fm_xform* b, f32 alpha, fm_xform* c)
{
	c->pos = fm_vec4_lerp(b->pos, a->pos, alpha);
	c->rot = fm_quat_slerp(a->rot, b->rot, alpha);
}

static inline void fm_xform_to_mat4(const fm_xform* x, fm_mat4* m)
{
	const fm_quat q = x->rot;
	
	m->x.x = 1.0f - 2.0f * q.j * q.j - 2.0f * q.k * q.k;
	m->x.y = 2.0f * q.i * q.j - 2.0f * q.k * q.r;
	m->x.z = 2.0f * q.i * q.k + 2.0f * q.j * q.r;
	m->x.w = 0.0f;
	
	m->y.x = 2.0f * q.i * q.j + 2.0f * q.k * q.r;
	m->y.y = 1.0f - 2.0f * q.i * q.i - 2.0f * q.k * q.k;
	m->y.z = 2.0f * q.j * q.k - 2.0f * q.i * q.r;
	m->y.w = 0.0f;
	
	m->z.x = 2.0f * q.i * q.k - 2.0f * q.j * q.r;
	m->z.y = 2.0f * q.j * q.k + 2.0f * q.i * q.r;
	m->z.z = 1.0f - 2.0f * q.i * q.i - 2.0f * q.j * q.j;
	m->z.w = 0.0f;
	
	m->w.x = x->pos.x;
	m->w.y = x->pos.y;
	m->w.z = x->pos.z;
	m->w.w = 1.0f;
}

static inline fm_vec4 fm_xform_apply(const fm_xform* x, const fm_vec4 a)
{
	fm_vec4 res = fm_vec4_add(x->pos, fm_quat_rot(x->rot, a));
	return res;
}

static inline fm_vec4 fm_xform_apply_inv(const fm_xform* x, const fm_vec4 a)
{
	const fm_quat invRot = fm_quat_conj(x->rot);
	return fm_quat_rot(invRot, fm_vec4_sub(a, x->pos));
}

#define FM_CATMULL_ROM_ALPHA 0.5f

static inline f32 fm_catmull_rom_get_t_value(const f32 t, const fm_vec4 p0, const  fm_vec4 p1)
{
	const f32 a = powf(p1.x - p0.x, 2.0f) + powf(p1.y - p0.y, 2.0f) + powf(p1.z - p0.z, 2.0f) + powf(p1.w - p0.w, 2.0f);
	const f32 b = powf(a, 0.5f);
	const f32 c = powf(b, FM_CATMULL_ROM_ALPHA);
	return c + t;
}

static inline fm_vec4 fm_spline_catmull_rom(const fm_vec4 p0, const fm_vec4 p1, const fm_vec4 p2, const fm_vec4 p3, const f32 t )
{
	const f32 t0 = 0.0f;
	const f32 t1 = fm_catmull_rom_get_t_value(t0, p0, p1);
	const f32 t2 = fm_catmull_rom_get_t_value(t1, p1, p2);
	const f32 t3 = fm_catmull_rom_get_t_value(t2, p2, p3);
	
	const fm_vec4 a1 = fm_vec4_add(fm_vec4_mulf(p0, (t1 - t) / (t1 - t0)), fm_vec4_mulf(p1, (t - t0) / (t1 - t0)));
	const fm_vec4 a2 = fm_vec4_add(fm_vec4_mulf(p0, (t2 - t) / (t2 - t1)), fm_vec4_mulf(p1, (t - t1) / (t2 - t1)));
	const fm_vec4 a3 = fm_vec4_add(fm_vec4_mulf(p0, (t3 - t) / (t3 - t2)), fm_vec4_mulf(p1, (t - t2) / (t3 - t2)));
	const fm_vec4 b1 = fm_vec4_add(fm_vec4_mulf(a1, (t2 - t) / (t2 - t0)), fm_vec4_mulf(a2, (t - t0) / (t2 - t0)));
	const fm_vec4 b2 = fm_vec4_add(fm_vec4_mulf(a2, (t3 - t) / (t3 - t1)), fm_vec4_mulf(a3, (t - t1) / (t3 - t1)));
	
	const fm_vec4 output = fm_vec4_add(fm_vec4_mulf(b1, (t2 - t) / (t2 - t1)), fm_vec4_mulf(b2, (t - t1) / (t2 - t1)));

	return output;
}
	
static inline f32 fm_curve_uniform_s(f32 alpha)
{
	f32 sqt = alpha * alpha;
	return sqt / (2.0f * (sqt - alpha) + 1.0f);
}

static inline bool fm_intersection_box_box(const fm_box* a, const fm_box* b)
{
	if(fabs(a->center.x - b->center.x) > (a->extent.x + b->extent.x))
		return false;
	
	if(fabs(a->center.y - b->center.y) > (a->extent.y + b->extent.y))
		return false;
	
	if(fabs(a->center.z - b->center.z) > (a->extent.z + b->extent.z))
		return false;
	
	return false;
}

static inline bool fm_intersection_box_point(const fm_box* a, const fm_vec3 b)
{
	if(fabs(a->center.x - b.x) > a->extent.x)
		return false;
	
	if(fabs(a->center.y - b.y) > a->extent.y)
		return false;
	
	if(fabs(a->center.z - b.z) > a->extent.z)
		return false;
	
	return false;
}

static inline void fm_box_append(fm_box* a, const fm_box* b)
{
	const fm_vec3 a_max = fm_vec3_add(a->center, a->extent);
	const fm_vec3 a_min = fm_vec3_sub(a->center, a->extent);
	
	const fm_vec3 b_max = fm_vec3_add(b->center, b->extent);
	const fm_vec3 b_min = fm_vec3_sub(b->center, b->extent);
	
	const fm_vec3 r_max = {
		MAX(a_max.x, b_max.x),
		MAX(a_max.y, b_max.y),
		MAX(a_max.z, b_max.z) };
	
	const fm_vec3 r_min = {
		MIN(a_min.x, b_min.x),
		MIN(a_min.y, b_min.y),
		MIN(a_min.z, b_min.z) };
	
	a->center.x = (r_max.x + r_min.x) / 2.0f;
	a->center.y = (r_max.y + r_min.y) / 2.0f;
	a->center.z = (r_max.z + r_min.z) / 2.0f;
	
	a->extent.x = (r_max.x - r_min.x) / 2.0f;
	a->extent.y = (r_max.y - r_min.y) / 2.0f;
	a->extent.z = (r_max.z - r_min.z) / 2.0f;
}

#ifdef __cplusplus
}
#endif // __cplusplus
