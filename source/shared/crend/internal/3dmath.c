/* Copyright (c) 2016-2019 Furball Cat */
#include "math.h"
#include "3dmath.h"

void fm_vec4_add(const fm_vec4* a, const fm_vec4* b, fm_vec4* v)
{
	v->x = a->x + b->x;
	v->y = a->y + b->y;
	v->z = a->z + b->z;
	v->w = a->w + b->w;
}

void fm_vec4_sub(const fm_vec4* a, const fm_vec4* b, fm_vec4* v)
{
	v->x = a->x - b->x;
	v->y = a->y - b->y;
	v->z = a->z - b->z;
	v->w = a->w - b->w;
}

float fm_vec4_mag(const fm_vec4* v)
{
	return sqrtf(v->x * v->x + v->y * v->y + v->z * v->z + v->w * v->w);
}

float fm_vec4_dot(const fm_vec4* a, const fm_vec4* b)
{
	return a->x * b->x + a->y * b->y + a->z * b->z + a->w * b->w;
}

void fm_vec4_cross(const fm_vec4* a, const fm_vec4* b, fm_vec4* v)
{
	v->x = a->y * b->z - a->z * b->y;
	v->y = a->z * b->x - a->x * b->z;
	v->z = a->x * b->y - a->y * b->x;
	v->w = 0.0f;
}

void fm_vec4_normalize(fm_vec4* v)
{
	const float magnitude = fm_vec4_mag(v);
	const float magnitudeInv = 1.0f / magnitude;
	
	v->x *= magnitudeInv;
	v->y *= magnitudeInv;
	v->z *= magnitudeInv;
	v->w *= magnitudeInv;
}

void fm_vec4_lerp(const fm_vec4* a, const fm_vec4* b, float alpha, fm_vec4* c)
{
	const float invAlpha = 1.0f - alpha;
	c->x = a->x * alpha + b->x * invAlpha;
	c->y = a->y * alpha + b->y * invAlpha;
	c->z = a->z * alpha + b->z * invAlpha;
	c->w = a->w * alpha + b->w * invAlpha;
}

void fm_mat4_identity(fm_mat4_t* m)
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

void fm_mat4_rot_x(const float phi, fm_mat4_t* m)
{
	m->x.x = 1; m->x.y = 0; m->x.z = 0; m->x.w = 0;
	m->y.x = 0; m->y.y = cos(phi); m->y.z = -sin(phi); m->y.w = 0;
	m->z.x = 0; m->z.y = sin(phi); m->z.z = cos(phi); m->z.w = 0;
	m->w.x = 0; m->w.y = 0; m->w.z = 0; m->w.w = 1;
}

void fm_mat4_rot_y(const float phi, fm_mat4_t* m)
{
	m->x.x = cos(phi); m->x.y = 0; m->x.z = sin(phi); m->x.w = 0;
	m->y.x = 0; m->y.y = 1; m->y.z = 0; m->y.w = 0;
	m->z.x = -sin(phi); m->z.y = 0; m->z.z = cos(phi); m->z.w = 0;
	m->w.x = 0; m->w.y = 0; m->w.z = 0; m->w.w = 1;
}

void fm_mat4_rot_z(const float phi, fm_mat4_t* m)
{
	m->x.x = cos(phi); m->x.y = -sin(phi); m->x.z = 0; m->x.w = 0;
	m->y.x = sin(phi); m->y.y = cos(phi); m->y.z = 0; m->y.w = 0;
	m->z.x = 0; m->z.y = 0; m->z.z = 1; m->z.w = 0;
	m->w.x = 0; m->w.y = 0; m->w.z = 0; m->w.w = 1;
}

void fm_mat4_lookat(const fm_vec4* eye, const fm_vec4* at, const fm_vec4* up, fm_mat4_t* m)
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
void fm_mat4_projection(const float b, const float t, const float l, const float r,
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
void fm_mat4_projection_fov(const float fov, const float aspectRatio,
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

void fm_mat4_transpose(fm_mat4_t* m)
{
	fm_swapf(&m->x.y, &m->y.x);
	fm_swapf(&m->x.z, &m->z.x);
	fm_swapf(&m->x.w, &m->w.x);
	fm_swapf(&m->y.z, &m->z.y);
	fm_swapf(&m->y.w, &m->w.y);
	fm_swapf(&m->z.w, &m->w.z);
}

void fm_mat4_mul(const fm_mat4_t* a, const fm_mat4_t* b, fm_mat4_t* m)
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

float fm_clamp(const float value, const float min, const float max)
{
	if(value < min)
		return min;
	
	if(value > max)
		return max;
	
	return value;
}

void fm_quat_mul(const fm_quat* a, const fm_quat* b, fm_quat* c)
{
	
}

void fm_quat_rot(const fm_quat* a, const fm_vec4* b, fm_vec4* c)
{
	
}

void fm_quat_norm(fm_quat* q)
{
	const float magnitude = sqrtf(q->i * q->i + q->j * q->j + q->k * q->k + q->r * q->r);
	const float magnitudeInv = 1.0f / magnitude;
	
	q->i *= magnitudeInv;
	q->j *= magnitudeInv;
	q->k *= magnitudeInv;
	q->r *= magnitudeInv;
}

void fm_quat_slerp(const fm_quat* a, const fm_quat* b, float alpha, fm_quat* c)
{
	
}

void fm_quat_lerp(const fm_quat* a, const fm_quat* b, float alpha, fm_quat* c)
{
	
}

void fm_xform_mul(const fm_xform* a, const fm_xform* b, fm_xform* c)
{
	fm_vec4 rotatedB;
	fm_quat_rot(&a->rot, &b->pos, &rotatedB);
	fm_vec4_add(&a->pos, &rotatedB, &c->pos);
	fm_quat_mul(&a->rot, &b->rot, &c->rot);
}

void fm_xform_lerp(const fm_xform* a, const fm_xform* b, float alpha, fm_xform* c)
{
	fm_vec4_lerp(&a->pos, &b->pos, alpha, &c->pos);
	fm_quat_lerp(&a->rot, &b->rot, alpha, &c->rot);
}

void fm_xform_slerp(const fm_xform* a, const fm_xform* b, float alpha, fm_xform* c)
{
	fm_vec4_lerp(&a->pos, &b->pos, alpha, &c->pos);
	fm_quat_slerp(&a->rot, &b->rot, alpha, &c->rot);
}
