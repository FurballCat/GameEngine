/* Copyright (c) 2016-2020 Furball Cat */

#include "camera.h"
#include "cmath/public.h"

typedef struct fg_camera_t
{
	fm_vec4 eye;
	fm_vec4 at;
	fm_vec4 up;
	
	fm_vec4 dirForward;
	fm_vec4 dirLeft;
} fg_camera_t;

fg_camera_t g_camera;

void fg_camera_create(fg_camera_t** camera, fc_alloc_callbacks_t* pAllocCallbacks)
{
	fm_vec4 at = {0.0f, 0.0f, 1.2f, 0.0f};
	fm_vec4 eye = {-3.0f, -3.0f, 1.7f, 0.0f};
	fm_vec4 up = {0.0f, 0.0f, 1.0f, 0.0f};
	
	*camera = &g_camera;
	g_camera.at = at;
	g_camera.eye = eye;
	g_camera.up = up;
	
	fm_vec4 dir_forward = {};
	fm_vec4 dir_left = {};
	
	dir_forward = eye;
	fm_vec4_normalize(&dir_forward);
	fm_vec4_cross(&dir_forward, &up, &dir_left);
	fm_vec4_cross(&up, &dir_left, &dir_forward);
	fm_vec4_normalize(&dir_left);
	
	g_camera.dirForward = dir_forward;
	g_camera.dirForward = dir_left;
}

void fg_camera_release(fg_camera_t* camera, fc_alloc_callbacks_t* pAllocCallbacks)
{
	// ...
}

void fg_camera_update_orbit(fg_camera_t* camera, const fg_camera_update_orbit_ctx* ctx)
{
	const fm_vec4 g_eye = {-3, -3, 1.7, 0};
	fm_vec4 camera_at = {0, 0, 1.2, 0};
	const fm_vec4 up = {0, 0, 1, 0};
	
	fm_vec4 eye = g_eye;
	
	fm_vec4 dir_forward = {};
	fm_vec4 dir_left = {};
	
	static float cameraRotation = 0.0f;
	const float rotationSpeed = 1.0f * ctx->dt;
	cameraRotation += rotationSpeed * ctx->rotationX;
	const float sinRot = sinf(cameraRotation);
	const float cosRot = cosf(cameraRotation);
	eye.x = g_eye.x * sinRot;
	eye.y = g_eye.y * cosRot;
	
	static float cameraZoom = 1.0f;
	const float zoomSpeed = 1.0f * ctx->dt;
	cameraZoom += zoomSpeed * (ctx->zoom);
	
	fm_vec4_sub(&eye, &camera_at, &eye);
	
	dir_forward = eye;
	fm_vec4_normalize(&dir_forward);
	fm_vec4_cross(&dir_forward, &up, &dir_left);
	fm_vec4_cross(&up, &dir_left, &dir_forward);
	fm_vec4_normalize(&dir_left);
	
	fm_vec4_mulf(&eye, cameraZoom, &eye);
	fm_vec4_add(&eye, &camera_at, &eye);
	
	camera->eye = eye;
	camera->at = camera_at;
	camera->up = up;
	camera->dirForward = dir_forward;
	camera->dirLeft = dir_left;
}

void fg_camera_adjust_by_player_movement(fg_camera_t* camera, fm_mat4* playerMatrix)
{
	fm_vec4_add(&camera->at, &playerMatrix->w, &camera->at);
	camera->at.w = 0.0f;
	fm_vec4_add(&camera->eye, &playerMatrix->w, &camera->eye);
	camera->eye.w = 0.0f;
}

void fg_camera_get_directions(fg_camera_t* camera, fm_vec4* dirForward, fm_vec4* dirLeft)
{
	*dirForward = camera->dirForward;
	*dirLeft = camera->dirLeft;
}

void fg_camera_view_matrix(fg_camera_t* camera, fm_mat4* matrix)
{
	fm_mat4_lookat_lh(&camera->eye, &camera->at, &camera->up, matrix);
}
