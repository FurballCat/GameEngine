/* Copyright (c) 2016-2020 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include <math.h>
#include "math3d.h"

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

static inline float xm_vec4_dot(const xm_vec4 v1, const xm_vec4 v2)
{
	//xm_float4_v result;
	//result = _mm_fmadd_ps(v1.vec128, v2.vec128, (xm_float4_v){0.0f, 0.0f, 0.0f, 0.0f});
	// todo: finish implementation
	
	return 0.0f;
}

///////////////////////

static inline float fm_vec3_dot(const fm_vec3* a, const fm_vec3* b)
{
	return a->x * b->x + a->y * b->y + a->z * b->z;
}

static inline void fm_vec3_add(const fm_vec3* a, const fm_vec3* b, fm_vec3* v)
{
	v->x = a->x + b->x;
	v->y = a->y + b->y;
	v->z = a->z + b->z;
}

static inline void fm_vec3_sub(const fm_vec3* a, const fm_vec3* b, fm_vec3* v)
{
	v->x = a->x - b->x;
	v->y = a->y - b->y;
	v->z = a->z - b->z;
}

static inline float fm_vec3_mag(const fm_vec3* v)
{
	return sqrtf(v->x * v->x + v->y * v->y + v->z * v->z);
}
	
static inline void fm_vec3_norm(fm_vec3* v)
{
	const float magnitude = fm_vec3_mag(v);
	const float magnitudeInv = 1.0f / magnitude;
	
	v->x *= magnitudeInv;
	v->y *= magnitudeInv;
	v->z *= magnitudeInv;
}
	
static inline void fm_vec4_zeros(fm_vec4* v)
{
	v->x = 0.0f;
	v->y = 0.0f;
	v->z = 0.0f;
	v->w = 0.0f;
}
	
static inline void fm_vec4_add(const fm_vec4* a, const fm_vec4* b, fm_vec4* v)
{
	v->x = a->x + b->x;
	v->y = a->y + b->y;
	v->z = a->z + b->z;
	v->w = a->w + b->w;
}

static inline void fm_vec4_sub(const fm_vec4* a, const fm_vec4* b, fm_vec4* v)
{
	v->x = a->x - b->x;
	v->y = a->y - b->y;
	v->z = a->z - b->z;
	v->w = a->w - b->w;
}

static inline void fm_vec4_neg(fm_vec4* v)
{
	v->x = -v->x;
	v->y = -v->y;
	v->z = -v->z;
	v->w = -v->w;
}

static inline float fm_vec4_mag(const fm_vec4* v)
{
	return sqrtf(v->x * v->x + v->y * v->y + v->z * v->z + v->w * v->w);
}

static inline float fm_vec4_mag2(const fm_vec4* v)
{
	return v->x * v->x + v->y * v->y + v->z * v->z + v->w * v->w;
}

static inline void fm_vec4_abs(fm_vec4* v)
{
	v->x = fabsf(v->x);
	v->y = fabsf(v->y);
	v->z = fabsf(v->z);
	v->w = fabsf(v->w);
}

static inline float fm_vec4_dot(const fm_vec4* a, const fm_vec4* b)
{
	return a->x * b->x + a->y * b->y + a->z * b->z + a->w * b->w;
}

static inline void fm_vec4_mulf(const fm_vec4* v, const float t, fm_vec4* output)
{
	output->x = v->x * t;
	output->y = v->y * t;
	output->z = v->z * t;
	output->w = v->w * t;
}

static inline void fm_vec4_cross(const fm_vec4* a, const fm_vec4* b, fm_vec4* v)
{
	v->x = a->y * b->z - a->z * b->y;
	v->y = a->z * b->x - a->x * b->z;
	v->z = a->x * b->y - a->y * b->x;
	v->w = 0.0f;
}

static inline void fm_vec4_normalize(fm_vec4* v)
{
	const float magnitude = fm_vec4_mag(v);
	const float magnitudeInv = 1.0f / magnitude;
	
	v->x *= magnitudeInv;
	v->y *= magnitudeInv;
	v->z *= magnitudeInv;
	v->w *= magnitudeInv;
}

static inline void fm_vec4_lerp(const fm_vec4* a, const fm_vec4* b, float alpha, fm_vec4* c)
{
	const float invAlpha = 1.0f - alpha;
	c->x = a->x * alpha + b->x * invAlpha;
	c->y = a->y * alpha + b->y * invAlpha;
	c->z = a->z * alpha + b->z * invAlpha;
	c->w = a->w * alpha + b->w * invAlpha;
}

static inline void fm_vec4_rot_between(const fm_vec4* from, const fm_vec4* to, fm_quat* rot)
{
	fm_vec4 fromNorm = *from;
	fm_vec4_normalize(&fromNorm);
	fm_vec4 toNorm = *to;
	fm_vec4_normalize(&toNorm);
	
	const float cosAlpha = fm_vec4_dot(&fromNorm, &toNorm);
	const float alpha = -acosf(cosAlpha);
	if(fabsf(alpha) > 0.0001f)
	{
		fm_vec4 axis;
		fm_vec4_cross(&fromNorm, &toNorm, &axis);
		if(fm_vec4_mag2(&axis) > 0.0f)
		{
			fm_vec4_normalize(&axis);

			fm_quat_make_from_axis_angle(axis.x, axis.y, axis.z, alpha, rot);
			fm_quat_norm(rot);
		}
		else
		{
			fm_quat_identity(rot);
		}
	}
	else
	{
		fm_quat_identity(rot);
	}
}

static inline float fm_vec4_distance(const fm_vec4* a, const fm_vec4* b)
{
	fm_vec4 diff = {0};
	fm_vec4_sub(a, b, &diff);
	return fm_vec4_mag(&diff);
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

static inline void fm_mat4_rot_x(const float phi, fm_mat4* m)
{
	m->x.x = 1; m->x.y = 0; m->x.z = 0; m->x.w = 0;
	m->y.x = 0; m->y.y = cosf(phi); m->y.z = -sinf(phi); m->y.w = 0;
	m->z.x = 0; m->z.y = sinf(phi); m->z.z = cosf(phi); m->z.w = 0;
	m->w.x = 0; m->w.y = 0; m->w.z = 0; m->w.w = 1;
}

static inline void fm_mat4_rot_y(const float phi, fm_mat4* m)
{
	m->x.x = cosf(phi); m->x.y = 0; m->x.z = sinf(phi); m->x.w = 0;
	m->y.x = 0; m->y.y = 1; m->y.z = 0; m->y.w = 0;
	m->z.x = -sinf(phi); m->z.y = 0; m->z.z = cosf(phi); m->z.w = 0;
	m->w.x = 0; m->w.y = 0; m->w.z = 0; m->w.w = 1;
}

static inline void fm_mat4_rot_z(const float phi, fm_mat4* m)
{
	m->x.x = cosf(phi); m->x.y = -sinf(phi); m->x.z = 0; m->x.w = 0;
	m->y.x = sinf(phi); m->y.y = cosf(phi); m->y.z = 0; m->y.w = 0;
	m->z.x = 0; m->z.y = 0; m->z.z = 1; m->z.w = 0;
	m->w.x = 0; m->w.y = 0; m->w.z = 0; m->w.w = 1;
}

static inline void fm_mat4_lookat_lh(const fm_vec4* eye, const fm_vec4* at, const fm_vec4* up, fm_mat4* m)
{
	fm_vec4 axis_y;
	fm_vec4_sub(at, eye, &axis_y);
	fm_vec4_normalize(&axis_y);
	
	fm_vec4 axis_x;
	fm_vec4_cross(&axis_y, up, &axis_x);
	fm_vec4_normalize(&axis_x);
	
	fm_vec4 axis_z;
	fm_vec4_cross(&axis_x, &axis_y, &axis_z);
	
	m->x.x = axis_x.x;
	m->x.y = axis_x.y;
	m->x.z = axis_x.z;
	m->x.w = -fm_vec4_dot(&axis_x, eye);
	
	m->y.x = axis_y.x;
	m->y.y = axis_y.y;
	m->y.z = axis_y.z;
	m->y.w = -fm_vec4_dot(&axis_y, eye);
	
	m->z.x = axis_z.x;
	m->z.y = axis_z.y;
	m->z.z = axis_z.z;
	m->z.w = -fm_vec4_dot(&axis_z, eye);
	
	m->w.x = 0.0f;
	m->w.y = 0.0f;
	m->w.z = 0.0f;
	m->w.w = 1.0f;
}

static inline void fm_mat4_lookat_rh(const fm_vec4* eye, const fm_vec4* at, const fm_vec4* up, fm_mat4* m)
{
	fm_vec4 axis_z;
	fm_vec4_sub(eye, at, &axis_z);
	fm_vec4_normalize(&axis_z);
	
	fm_vec4 axis_x;
	fm_vec4_cross(up, &axis_z, &axis_x);
	fm_vec4_normalize(&axis_x);
	
	fm_vec4 axis_y;
	fm_vec4_cross(&axis_z, &axis_x, &axis_y);
	
	m->x.x = axis_x.x;
	m->x.y = axis_x.y;
	m->x.z = axis_x.z;
	m->x.w = fm_vec4_dot(&axis_x, eye);
	
	m->y.x = axis_y.x;
	m->y.y = axis_y.y;
	m->y.z = axis_y.z;
	m->y.w = fm_vec4_dot(&axis_y, eye);
	
	m->z.x = axis_z.x;
	m->z.y = axis_z.y;
	m->z.z = axis_z.z;
	m->z.w = fm_vec4_dot(&axis_z, eye);
	
	m->w.x = 0.0f;
	m->w.y = 0.0f;
	m->w.z = 0.0f;
	m->w.w = 1.0f;
}
	
// projection matrix, b - bottom, t - top, l - left, r - right, n - near, f - far
static inline void fm_mat4_ortho_projection(const float b, const float t, const float l, const float r,
									  const float n, const float f, fm_mat4* m)
{
	// todo: check this math...
#if 1
	float sum_rl, sum_tb, sum_nf, inv_rl, inv_tb, inv_nf;
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
	float width = r - l;
	float height = t - b;
	float depth = f - n;

	// calculate the translation factors
	float tx = -(r + l) / width;
	float ty = -(t + b) / height;
	float tz = -(f + n) / depth;

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
static inline void fm_mat4_projection(const float b, const float t, const float l, const float r,
						const float n, const float f, fm_mat4* m)
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
static inline void fm_mat4_projection_fov(const float fov, const float aspectRatio,
							const float near, const float far, fm_mat4* m)
{
	const float scale = tanf(FM_DEG_TO_RAD(fov * 0.5f)) * near;
	const float top = scale;
	const float bottom = -top;
	const float right = aspectRatio * scale;
	const float left = -right;
	
	fm_mat4_projection(bottom, top, left, right, near, far, m);
}

static inline void fm_swapf(float* a, float* b)
{
	const float c = *a;
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
	const float m00 = a->x.x * b->x.x + a->x.y * b->y.x + a->x.z * b->z.x + a->x.w * b->w.x;
	const float m01 = a->x.x * b->x.y + a->x.y * b->y.y + a->x.z * b->z.y + a->x.w * b->w.y;
	const float m02 = a->x.x * b->x.z + a->x.y * b->y.z + a->x.z * b->z.z + a->x.w * b->w.z;
	const float m03 = a->x.x * b->x.w + a->x.y * b->y.w + a->x.z * b->z.w + a->x.w * b->w.w;
	
	const float m10 = a->y.x * b->x.x + a->y.y * b->y.x + a->y.z * b->z.x + a->y.w * b->w.x;
	const float m11 = a->y.x * b->x.y + a->y.y * b->y.y + a->y.z * b->z.y + a->y.w * b->w.y;
	const float m12 = a->y.x * b->x.z + a->y.y * b->y.z + a->y.z * b->z.z + a->y.w * b->w.z;
	const float m13 = a->y.x * b->x.w + a->y.y * b->y.w + a->y.z * b->z.w + a->y.w * b->w.w;
	
	const float m20 = a->z.x * b->x.x + a->z.y * b->y.x + a->z.z * b->z.x + a->z.w * b->w.x;
	const float m21 = a->z.x * b->x.y + a->z.y * b->y.y + a->z.z * b->z.y + a->z.w * b->w.y;
	const float m22 = a->z.x * b->x.z + a->z.y * b->y.z + a->z.z * b->z.z + a->z.w * b->w.z;
	const float m23 = a->z.x * b->x.w + a->z.y * b->y.w + a->z.z * b->z.w + a->z.w * b->w.w;
	
	const float m30 = a->w.x * b->x.x + a->w.y * b->y.x + a->w.z * b->z.x + a->w.w * b->w.x;
	const float m31 = a->w.x * b->x.y + a->w.y * b->y.y + a->w.z * b->z.y + a->w.w * b->w.y;
	const float m32 = a->w.x * b->x.z + a->w.y * b->y.z + a->w.z * b->z.z + a->w.w * b->w.z;
	const float m33 = a->w.x * b->x.w + a->w.y * b->y.w + a->w.z * b->z.w + a->w.w * b->w.w;
	
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
	
static inline void fm_mat4_transform(const fm_mat4* m, const fm_vec4* a, fm_vec4* b)
{
	b->x = fm_vec4_dot(a, &m->x);
	b->y = fm_vec4_dot(a, &m->y);
	b->z = fm_vec4_dot(a, &m->z);
	b->w = fm_vec4_dot(a, &m->w);
}

static inline float fm_clamp(const float value, const float min, const float max)
{
	if(value < min)
		return min;
	
	if(value > max)
		return max;
	
	return value;
}

static inline float fm_snap_near_zero(const float value, const float threshold)
{
	if(fabsf(value) < threshold)
		return 0.0f;
	
	return value;
}
	
static inline float fm_sign(const float value)
{
	return value >= 0.0f ? 1.0f : -1.0f;
}

static inline void fm_quat_identity(fm_quat* q)
{
	q->i = 0.0f;
	q->j = 0.0f;
	q->k = 0.0f;
	q->r = 1.0f;
}

static inline float fm_quat_dot3(const fm_quat* a, const fm_quat* b)
{
	return a->i * b->i + a->j * b->j + a->k * b->k;
}
	
static inline float fm_quat_dot(const fm_quat* a, const fm_quat* b)
{
	return a->i * b->i + a->j * b->j + a->k * b->k + a->r * b->r;
}

static inline void fm_quat_cross3(const fm_quat* a, const fm_quat* b, fm_vec4* v)
{
	v->x = a->j * b->k - a->k * b->j;
	v->y = a->k * b->i - a->i * b->k;
	v->z = a->i * b->j - a->j * b->i;
	v->w = 0.0f;
}

static inline void fm_quat_add(fm_quat* v, const fm_quat* a, const fm_quat* b)
{
	v->i = a->i + b->i;
	v->j = a->j + b->j;
	v->k = a->k + b->k;
	v->r = a->r + b->r;
}
	
static inline void fm_quat_mul(const fm_quat* a, const fm_quat* b, fm_quat* c)
{
	const float i = a->r * b->i + a->k * b->j - a->j * b->k + a->i * b->r;
	const float j = -a->k * b->i + a->r * b->j + a->i * b->k + a->j * b->r;
	const float k = a->j * b->i - a->i * b->j + a->r * b->k + a->k * b->r;
	const float r = -a->i * b->i - a->j * b->j - a->k * b->k + a->r * b->r;
	
	c->i = i;
	c->j = j;
	c->k = k;
	c->r = r;
}
	
static inline void fm_quat_mulf(fm_quat* c, const fm_quat* a, float t)
{
	c->i = a->i * t;
	c->j = a->j * t;
	c->k = a->k * t;
	c->r = a->r * t;
}

static inline void fm_quat_rot(const fm_quat* q, const fm_vec4* v, fm_vec4* c)
{
	const float dot_qv = v->x * q->i + v->y * q->j + v->z * q->k;
	const float mag_qv = q->i * q->i + q->j * q->j + q->k * q->k;
	
	const float s1 = 2.0f * dot_qv;
	const float s2 = q->r * q->r - mag_qv;
	const float s3 = 2.0f * q->r;
	
	const fm_vec4 qv = {q->i, q->j, q->k, 0.0f};
	fm_vec4 cross_qv;
	fm_vec4_cross(v, &qv, &cross_qv);
	
	c->x = s1 * qv.x + s2 * v->x + s3 * cross_qv.x;
	c->y = s1 * qv.y + s2 * v->y + s3 * cross_qv.y;
	c->z = s1 * qv.z + s2 * v->z + s3 * cross_qv.z;
	c->w = 0.0f;
}

static inline void fm_quat_norm(fm_quat* q)
{
	const float n = sqrtf(q->i * q->i + q->j * q->j + q->k * q->k + q->r * q->r);
	const float n_inv = 1.0f / n;
	
	q->i *= n_inv;
	q->j *= n_inv;
	q->k *= n_inv;
	q->r *= n_inv;
}

static inline void fm_quat_neg(fm_quat* q_out, const fm_quat* q_in)
{
	q_out->i = -q_in->i;
	q_out->j = -q_in->j;
	q_out->k = -q_in->k;
	q_out->r = -q_in->r;
}
	
static inline void fm_quat_slerp(const fm_quat* unitQuat0, const fm_quat* unitQuat1, float t, fm_quat* result)
{
	fm_quat start, tmpQ_0, tmpQ_1;
	float recipSinAngle, scale0, scale1, cosAngle, angle;
	cosAngle = fm_quat_dot( unitQuat0, unitQuat1 );
	if ( cosAngle < 0.0f )
	{
		cosAngle = -cosAngle;
		fm_quat_neg( &start, unitQuat0 );
	}
	else
	{
		start = *unitQuat0;
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
	fm_quat_mulf( &tmpQ_0, &start, scale0 );
	fm_quat_mulf( &tmpQ_1, unitQuat1, scale1 );
	fm_quat_add( result, &tmpQ_0, &tmpQ_1 );
}

static inline void fm_quat_lerp(const fm_quat* a, const fm_quat* b, float alpha, fm_quat* c)
{
	const float alpha_inv = 1.0f - alpha;
	c->i = a->i * alpha_inv + b->i * alpha;
	c->j = a->j * alpha_inv + b->j * alpha;
	c->k = a->k * alpha_inv + b->k * alpha;
	c->r = a->r * alpha_inv + b->r * alpha;
}

static inline void fm_quat_to_mat4(const fm_quat* q, fm_mat4* m)
{
	m->x.x = 1.0f - 2.0f * q->j * q->j - 2.0f * q->k * q->k;
	m->x.y = 2.0f * q->i * q->j - 2.0f * q->k * q->r;
	m->x.z = 2.0f * q->i * q->k + 2.0f * q->j * q->r;
	m->x.w = 0.0f;
	
	m->y.x = 2.0f * q->i * q->j + 2.0f * q->k * q->r;
	m->y.y = 1.0f - 2.0f * q->i * q->i - 2.0f * q->k * q->k;
	m->y.z = 2.0f * q->j * q->k - 2.0f * q->i * q->r;
	m->y.w = 0.0f;
	
	m->z.x = 2.0f * q->i * q->k - 2.0f * q->j * q->r;
	m->z.y = 2.0f * q->j * q->k + 2.0f * q->i * q->r;
	m->z.z = 1.0f - 2.0f * q->i * q->i - 2.0f * q->j * q->j;
	m->z.w = 0.0f;
	
	m->w.x = 0.0f;
	m->w.y = 0.0f;
	m->w.z = 0.0f;
	m->w.w = 1.0f;
}

static inline void fm_quat_to_euler(const fm_quat* q, fm_euler_angles* angles)
{
	// roll (x-axis rotation)
	float sinr_cosp = 2.0f * (q->r * q->i + q->j * q->k);
	float cosr_cosp = 1.0f - 2.0f * (q->i * q->i + q->j * q->j);
	angles->roll = atan2f(sinr_cosp, cosr_cosp);

	// pitch (y-axis rotation)
	float sinp = 2.0f * (q->r * q->j - q->k * q->i);
	if (fabs(sinp) >= 1.0f)
		angles->pitch = (float)FM_PI / 2.0f * fm_sign(sinp); // use 90 degrees if out of range
	else
		angles->pitch = asinf(sinp);

	// yaw (z-axis rotation)
	float siny_cosp = 2.0f * (q->r * q->k + q->i * q->j);
	float cosy_cosp = 1.0f - 2.0f * (q->j * q->j + q->k * q->k);
	angles->yaw = atan2f(siny_cosp, cosy_cosp);
}

static inline void fm_quat_rot_axis_angle(const fm_vec4* axis, const float angle, fm_quat* q)
{
	const float scale = sinf(angle / 2) / fm_vec4_mag(axis);
	q->i = scale * axis->x;
	q->j = scale * axis->y;
	q->k = scale * axis->z;
	q->r = cosf(angle / 2.0f);
}

static inline void fm_quat_to_axis_angle(const fm_quat* q, fm_vec4* axis, float* angle)
{
	*angle = acosf(fm_clamp(q->r, -1.0f, 1.0f)) * 2.0f;
	if(fabsf(*angle) > 0.0001f)
	{
		const float scale = sinf(*angle / 2.0f);
		axis->x = q->i / scale;
		axis->y = q->j / scale;
		axis->z = q->k / scale;
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
	
static inline void fm_quat_make_from_axis_angle(float x, float y, float z, const float angle, fm_quat* q)
{
	const float scale = sinf(angle / 2) / sqrt(x*x + y*y + z*z);
	q->i = scale * x;
	q->j = scale * y;
	q->k = scale * z;
	q->r = cosf(angle / 2.0f);
}
	
static inline void fm_quat_make_from_euler_angles_yzpxry(const fm_euler_angles* angles, fm_quat* quat)
{
	fm_quat p;
	fm_quat_make_from_axis_angle(1.0f, 0.0f, 0.0f, angles->pitch, &p);
	fm_quat r;
	fm_quat_make_from_axis_angle(0.0f, 1.0f, 0.0f, angles->roll, &r);
	fm_quat y;
	fm_quat_make_from_axis_angle(0.0f, 0.0f, 1.0f, angles->yaw, &y);
	
	fm_quat tmp;
	fm_quat_mul(&p, &y, &tmp);
	fm_quat_mul(&tmp, &r, quat);
}
	
static inline void fm_quat_make_from_euler_angles_xyz(const fm_euler_angles* angles, fm_quat* quat)
{
	fm_quat x;
	fm_quat_make_from_axis_angle(1.0f, 0.0f, 0.0f, angles->pitch, &x);
	fm_quat y;
	fm_quat_make_from_axis_angle(0.0f, 1.0f, 0.0f, angles->yaw, &y);
	fm_quat z;
	fm_quat_make_from_axis_angle(0.0f, 0.0f, 1.0f, angles->roll, &z);
	
	fm_quat tmp;
	fm_quat_mul(&y, &x, &tmp);
	fm_quat_mul(&z, &tmp, quat);
}
	
static inline void fm_quat_make_from_euler_angles_pyyzrx(const fm_euler_angles* angles, fm_quat* quat)
{
	fm_quat p;
	fm_quat_make_from_axis_angle(0.0f, 1.0f, 0.0f, angles->pitch, &p);
	fm_quat r;
	fm_quat_make_from_axis_angle(0.0f, 0.0f, 1.0f, angles->roll, &r);
	fm_quat y;
	fm_quat_make_from_axis_angle(1.0f, 0.0f, 0.0f, angles->yaw, &y);
	
	fm_quat tmp;
	fm_quat_mul(&y, &p, &tmp);
	fm_quat_mul(&tmp, &r, quat);
}
	
static inline void fm_quat_make_from_euler_angles_pxryyz(const fm_euler_angles* angles, fm_quat* quat)
{
	fm_quat p;
	fm_quat_make_from_axis_angle(0.0f, 1.0f, 0.0f, angles->pitch, &p);
	fm_quat r;
	fm_quat_make_from_axis_angle(1.0f, 0.0f, 0.0f, angles->roll, &r);
	fm_quat y;
	fm_quat_make_from_axis_angle(0.0f, 0.0f, 1.0f, angles->yaw, &y);
	
	fm_quat tmp;
	fm_quat_mul(&r, &p, &tmp);
	fm_quat_mul(&tmp, &y, quat);
}

static inline void fm_quat_conj(fm_quat* q)
{
	q->i = -q->i;
	q->j = -q->j;
	q->k = -q->k;
}

static inline fm_vec4 fm_quat_axis_x(fm_quat* q)
{
	fm_vec4 axis = {1.0f, 0.0f, 0.0f, 0.0f};
	fm_quat_rot(q, &axis, &axis);
	return axis;
}

static inline fm_vec4 fm_quat_axis_y(fm_quat* q)
{
	fm_vec4 axis = {0.0f, 1.0f, 0.0f, 0.0f};
	fm_quat_rot(q, &axis, &axis);
	return axis;
}

static inline fm_vec4 fm_quat_axis_z(fm_quat* q)
{
	fm_vec4 axis = {0.0f, 0.0f, 1.0f, 0.0f};
	fm_quat_rot(q, &axis, &axis);
	return axis;
}

static inline void fm_xform_identity(fm_xform* x)
{
	fm_vec4_zeros(&x->pos);
	fm_quat_identity(&x->rot);
}
	
static inline void fm_xform_mul(const fm_xform* a, const fm_xform* b, fm_xform* c)
{
	fm_vec4 rotatedB;
	fm_quat_rot(&a->rot, &b->pos, &rotatedB);
	fm_vec4_add(&a->pos, &rotatedB, &c->pos);
	fm_quat_mul(&a->rot, &b->rot, &c->rot);
}

static inline void fm_xform_lerp(const fm_xform* a, const fm_xform* b, float alpha, fm_xform* c)
{
	fm_vec4_lerp(&b->pos, &a->pos, alpha, &c->pos);
	fm_quat_lerp(&a->rot, &b->rot, alpha, &c->rot);
}

static inline void fm_xform_slerp(const fm_xform* a, const fm_xform* b, float alpha, fm_xform* c)
{
	fm_vec4_lerp(&b->pos, &a->pos, alpha, &c->pos);
	fm_quat_slerp(&a->rot, &b->rot, alpha, &c->rot);
}

static inline void fm_xform_to_mat4(const fm_xform* x, fm_mat4* m)
{
	const fm_quat* q = &x->rot;
	
	m->x.x = 1.0f - 2.0f * q->j * q->j - 2.0f * q->k * q->k;
	m->x.y = 2.0f * q->i * q->j - 2.0f * q->k * q->r;
	m->x.z = 2.0f * q->i * q->k + 2.0f * q->j * q->r;
	m->x.w = 0.0f;
	
	m->y.x = 2.0f * q->i * q->j + 2.0f * q->k * q->r;
	m->y.y = 1.0f - 2.0f * q->i * q->i - 2.0f * q->k * q->k;
	m->y.z = 2.0f * q->j * q->k - 2.0f * q->i * q->r;
	m->y.w = 0.0f;
	
	m->z.x = 2.0f * q->i * q->k - 2.0f * q->j * q->r;
	m->z.y = 2.0f * q->j * q->k + 2.0f * q->i * q->r;
	m->z.z = 1.0f - 2.0f * q->i * q->i - 2.0f * q->j * q->j;
	m->z.w = 0.0f;
	
	m->w.x = x->pos.x;
	m->w.y = x->pos.y;
	m->w.z = x->pos.z;
	m->w.w = 1.0f;
}

static inline void fm_xform_apply(const fm_xform* x, const fm_vec4* a, fm_vec4* v)
{
	fm_quat_rot(&x->rot, a, v);
	fm_vec4_add(&x->pos, v, v);
}

static inline void fm_xform_apply_inv(const fm_xform* x, const fm_vec4* a, fm_vec4* v)
{
	fm_quat invRot = x->rot;
	fm_quat_conj(&invRot);
	fm_vec4_sub(a, &x->pos, v);
	fm_quat_rot(&invRot, v, v);
}

#define FM_CATMULL_ROM_ALPHA 0.5f

static inline float fm_catmull_rom_get_t_value(const float t, const fm_vec4* p0, const  fm_vec4* p1)
{
	const float a = powf(p1->x - p0->x, 2.0f) + powf(p1->y - p0->y, 2.0f) + powf(p1->z - p0->z, 2.0f) + powf(p1->w - p0->w, 2.0f);
	const float b = powf(a, 0.5f);
	const float c = powf(b, FM_CATMULL_ROM_ALPHA);
	return c + t;
}

static inline void fm_spline_catmull_rom(const fm_vec4* p0, const fm_vec4* p1, const fm_vec4* p2, const fm_vec4* p3,
										 const float t, fm_vec4* output )
{
	const float t0 = 0.0f;
	const float t1 = fm_catmull_rom_get_t_value(t0, p0, p1);
	const float t2 = fm_catmull_rom_get_t_value(t1, p1, p2);
	const float t3 = fm_catmull_rom_get_t_value(t2, p2, p3);
	
	fm_vec4 tmp1;
	
	fm_vec4 a1;
	fm_vec4_mulf(p0, (t1-t)/(t1-t0), &a1);
	fm_vec4_mulf(p1, (t-t0)/(t1-t0), &tmp1);
	fm_vec4_add(&a1, &tmp1, &a1);
	
	fm_vec4 a2;
	fm_vec4_mulf(p0, (t2-t)/(t2-t1), &a2);
	fm_vec4_mulf(p1, (t-t1)/(t2-t1), &tmp1);
	fm_vec4_add(&a2, &tmp1, &a2);
	
	fm_vec4 a3;
	fm_vec4_mulf(p0, (t3-t)/(t3-t2), &a3);
	fm_vec4_mulf(p1, (t-t2)/(t3-t2), &tmp1);
	fm_vec4_add(&a3, &tmp1, &a3);
	
	fm_vec4 b1;
	fm_vec4_mulf(&a1, (t2-t)/(t2-t0), &b1);
	fm_vec4_mulf(&a2, (t-t0)/(t2-t0), &tmp1);
	fm_vec4_add(&b1, &tmp1, &b1);
	
	fm_vec4 b2;
	fm_vec4_mulf(&a2, (t3-t)/(t3-t1), &b2);
	fm_vec4_mulf(&a3, (t-t1)/(t3-t1), &tmp1);
	fm_vec4_add(&b2, &tmp1, &b2);
	
	fm_vec4_mulf(&b1, (t2-t)/(t2-t1), output);
	fm_vec4_mulf(&b2, (t-t1)/(t2-t1), &tmp1);
	fm_vec4_add(output, &tmp1, output);
}
	
static inline float fm_curve_uniform_s(float alpha)
{
	float sqt = alpha * alpha;
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

static inline bool fm_intersection_box_point(const fm_box* a, const fm_vec3* b)
{
	if(fabs(a->center.x - b->x) > a->extent.x)
		return false;
	
	if(fabs(a->center.y - b->y) > a->extent.y)
		return false;
	
	if(fabs(a->center.z - b->z) > a->extent.z)
		return false;
	
	return false;
}

static inline void fm_box_append(fm_box* a, const fm_box* b)
{
	fm_vec3 a_max;
	fm_vec3_add(&a->center, &a->extent, &a_max);
	
	fm_vec3 a_min;
	fm_vec3_sub(&a->center, &a->extent, &a_min);
	
	fm_vec3 b_max;
	fm_vec3_add(&b->center, &b->extent, &b_max);
	
	fm_vec3 b_min;
	fm_vec3_sub(&b->center, &b->extent, &b_min);
	
	fm_vec3 r_max;
	r_max.x = MAX(a_max.x, b_max.x);
	r_max.y = MAX(a_max.y, b_max.y);
	r_max.z = MAX(a_max.z, b_max.z);
	
	fm_vec3 r_min;
	r_min.x = MIN(a_min.x, b_min.x);
	r_min.y = MIN(a_min.y, b_min.y);
	r_min.z = MIN(a_min.z, b_min.z);
	
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
