/* Copyright (c) 2016-2020 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include <math.h>
#include "math3d.h"

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

static inline float fm_vec4_mag(const fm_vec4* v)
{
	return sqrtf(v->x * v->x + v->y * v->y + v->z * v->z + v->w * v->w);
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

static inline void fm_mat4_identity(fm_mat4_t* m)
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

static inline void fm_mat4_rot_x(const float phi, fm_mat4_t* m)
{
	m->x.x = 1; m->x.y = 0; m->x.z = 0; m->x.w = 0;
	m->y.x = 0; m->y.y = cos(phi); m->y.z = -sin(phi); m->y.w = 0;
	m->z.x = 0; m->z.y = sin(phi); m->z.z = cos(phi); m->z.w = 0;
	m->w.x = 0; m->w.y = 0; m->w.z = 0; m->w.w = 1;
}

static inline void fm_mat4_rot_y(const float phi, fm_mat4_t* m)
{
	m->x.x = cos(phi); m->x.y = 0; m->x.z = sin(phi); m->x.w = 0;
	m->y.x = 0; m->y.y = 1; m->y.z = 0; m->y.w = 0;
	m->z.x = -sin(phi); m->z.y = 0; m->z.z = cos(phi); m->z.w = 0;
	m->w.x = 0; m->w.y = 0; m->w.z = 0; m->w.w = 1;
}

static inline void fm_mat4_rot_z(const float phi, fm_mat4_t* m)
{
	m->x.x = cos(phi); m->x.y = -sin(phi); m->x.z = 0; m->x.w = 0;
	m->y.x = sin(phi); m->y.y = cos(phi); m->y.z = 0; m->y.w = 0;
	m->z.x = 0; m->z.y = 0; m->z.z = 1; m->z.w = 0;
	m->w.x = 0; m->w.y = 0; m->w.z = 0; m->w.w = 1;
}

static inline void fm_mat4_lookat(const fm_vec4* eye, const fm_vec4* at, const fm_vec4* up, fm_mat4_t* m)
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

// projection matrix, b - bottom, t - top, l - left, r - right, n - near, f - far
static inline void fm_mat4_projection(const float b, const float t, const float l, const float r,
						const float n, const float f, fm_mat4_t* m)
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
							const float near, const float far, fm_mat4_t* m)
{
	const float scale = tan(FM_DEG_TO_RAD(fov * 0.5f)) * near;
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

static inline void fm_mat4_transpose(fm_mat4_t* m)
{
	fm_swapf(&m->x.y, &m->y.x);
	fm_swapf(&m->x.z, &m->z.x);
	fm_swapf(&m->x.w, &m->w.x);
	fm_swapf(&m->y.z, &m->z.y);
	fm_swapf(&m->y.w, &m->w.y);
	fm_swapf(&m->z.w, &m->w.z);
}

static inline void fm_mat4_mul(const fm_mat4_t* a, const fm_mat4_t* b, fm_mat4_t* m)
{
	m->x.x = a->x.x * b->x.x + a->x.y * b->y.x + a->x.z * b->z.x + a->x.w * b->w.x;
	m->x.y = a->x.x * b->x.y + a->x.y * b->y.y + a->x.z * b->z.y + a->x.w * b->w.y;
	m->x.z = a->x.x * b->x.z + a->x.y * b->y.z + a->x.z * b->z.z + a->x.w * b->w.z;
	m->x.w = a->x.x * b->x.w + a->x.y * b->y.w + a->x.z * b->z.w + a->x.w * b->w.w;
	
	m->y.x = a->y.x * b->x.x + a->y.y * b->y.x + a->y.z * b->z.x + a->y.w * b->w.x;
	m->y.y = a->y.x * b->x.y + a->y.y * b->y.y + a->y.z * b->z.y + a->y.w * b->w.y;
	m->y.z = a->y.x * b->x.z + a->y.y * b->y.z + a->y.z * b->z.z + a->y.w * b->w.z;
	m->y.w = a->y.x * b->x.w + a->y.y * b->y.w + a->y.z * b->z.w + a->y.w * b->w.w;
	
	m->z.x = a->z.x * b->x.x + a->z.y * b->y.x + a->z.z * b->z.x + a->z.w * b->w.x;
	m->z.y = a->z.x * b->x.y + a->z.y * b->y.y + a->z.z * b->z.y + a->z.w * b->w.y;
	m->z.z = a->z.x * b->x.z + a->z.y * b->y.z + a->z.z * b->z.z + a->z.w * b->w.z;
	m->z.w = a->z.x * b->x.w + a->z.y * b->y.w + a->z.z * b->z.w + a->z.w * b->w.w;
	
	m->w.x = a->w.x * b->x.x + a->w.y * b->y.x + a->w.z * b->z.x + a->w.w * b->w.x;
	m->w.y = a->w.x * b->x.y + a->w.y * b->y.y + a->w.z * b->z.y + a->w.w * b->w.y;
	m->w.z = a->w.x * b->x.z + a->w.y * b->y.z + a->w.z * b->z.z + a->w.w * b->w.z;
	m->w.w = a->w.x * b->x.w + a->w.y * b->y.w + a->w.z * b->z.w + a->w.w * b->w.w;
}
	
static inline void fm_mat4_transform(const fm_mat4_t* m, const fm_vec4* a, fm_vec4* b)
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

static inline void fm_quat_cross3(const fm_quat* a, const fm_quat* b, fm_vec4* v)
{
	v->x = a->j * b->k - a->k * b->j;
	v->y = a->k * b->i - a->i * b->k;
	v->z = a->i * b->j - a->j * b->i;
	v->w = 0.0f;
}

static inline void fm_quat_mul(const fm_quat* a, const fm_quat* b, fm_quat* c)
{
	c->i = a->r * b->i + a->k * b->j - a->j * b->k + a->i * b->r;
	c->j = -a->k * b->i + a->r * b->j + a->i * b->k + a->j * b->r;
	c->k = a->j * b->i - a->i * b->j + a->r * b->k + a->k * b->r;
	c->r = -a->i * b->i - a->j * b->j - a->k * b->k + a->r * b->r;
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

static inline void fm_quat_slerp(const fm_quat* a, const fm_quat* b, float alpha, fm_quat* c)
{
	const float t = 1.0f - alpha;
	const float theta = acosf(a->i * b->i + a->j * b->j + a->k * b->k + a->r * b->r);
	const float sn = sinf(theta);
	const float w_a = sinf(t * theta) / sn;
	const float w_b = sinf(alpha * theta) / sn;
	
	c->i = w_a * a->i + w_b * b->i;
	c->j = w_a * a->j + w_b * b->j;
	c->k = w_a * a->k + w_b * b->k;
	c->r = w_a * a->r + w_b * b->r;
}

static inline void fm_quat_lerp(const fm_quat* a, const fm_quat* b, float alpha, fm_quat* c)
{
	const float alpha_inv = 1.0f - alpha;
	c->i = a->i * alpha_inv + b->i * alpha;
	c->j = a->j * alpha_inv + b->j * alpha;
	c->k = a->k * alpha_inv + b->k * alpha;
	c->r = a->r * alpha_inv + b->r * alpha;
}

static inline void fm_quat_to_mat4(const fm_quat* q, fm_mat4_t* m)
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

static inline void fm_quat_rot_axis_angle(const fm_vec4* axis, const float angle, fm_quat* q)
{
	const float scale = sinf(angle / 2) / fm_vec4_mag(axis);
	q->i = scale * axis->x;
	q->j = scale * axis->y;
	q->k = scale * axis->z;
	q->r = cosf(angle / 2.0f);
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
	fm_vec4_lerp(&a->pos, &b->pos, alpha, &c->pos);
	fm_quat_lerp(&a->rot, &b->rot, alpha, &c->rot);
}

static inline void fm_xform_slerp(const fm_xform* a, const fm_xform* b, float alpha, fm_xform* c)
{
	fm_vec4_lerp(&a->pos, &b->pos, alpha, &c->pos);
	fm_quat_slerp(&a->rot, &b->rot, alpha, &c->rot);
}

static inline void fm_xform_to_mat4(const fm_xform* x, fm_mat4_t* m)
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

#ifdef __cplusplus
}
#endif // __cplusplus