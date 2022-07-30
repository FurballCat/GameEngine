/* Copyright (c) Furball Cat */

#include "camera.h"
#include "cmath/public.h"

typedef struct fg_camera_t
{
	fm_xform locator;
	float fov;
} fg_camera_t;

fg_camera_t g_camera;

void fg_camera_create(fg_camera_t** camera, fc_alloc_callbacks_t* pAllocCallbacks)
{
	*camera = &g_camera;
	fm_xform_identity(&(*camera)->locator);
}

void fg_camera_release(fg_camera_t* camera, fc_alloc_callbacks_t* pAllocCallbacks)
{
	// ...
}

void fg_camera_update_orbit(fg_camera_t* camera, const fg_camera_update_orbit_ctx* ctx)
{
	const float poleLength = 3.0f;
	fm_vec4 camera_at = {0, 0, 1.2, 0};
	const fm_vec4 up = {0, 0, 1, 0};
	
	fm_vec4 eye = {};
	
	fm_vec4 dir_forward = {};
	fm_vec4 dir_left = {};
	
	static float cameraRotationYaw = 0.0f;
	const float rotationSpeed = 2.5f * ctx->dt;
	cameraRotationYaw += rotationSpeed * ctx->rotationYaw;
	
	static float cameraRotationPitch = 0.2f;
	const float rotationSpeedPitch = 2.5f * ctx->dt;
	cameraRotationPitch += rotationSpeedPitch * ctx->rotationPitch;
	
	cameraRotationPitch = fm_clamp(cameraRotationPitch, -1.0f, 1.0f);
	const float poleLengthFactor = 1.0f - (cameraRotationPitch < 0.0f ? 0.5f : 0.2f) * (cameraRotationPitch * cameraRotationPitch);
	
	const float sinRotPitch = sinf(cameraRotationPitch);
	const float cosRotPitch = cosf(cameraRotationPitch);
	
	const float sinRotYaw = sinf(cameraRotationYaw);
	const float cosRotYaw = cosf(cameraRotationYaw);
	eye.x = -poleLength * poleLengthFactor * cosRotPitch * sinRotYaw;
	eye.y = -poleLength * poleLengthFactor * cosRotPitch * cosRotYaw;
	eye.z = poleLength * poleLengthFactor * sinRotPitch;
	
	static float cameraZoom = 1.0f;
	const float zoomSpeed = 1.0f * ctx->dt;
	cameraZoom += zoomSpeed * (ctx->zoom);
	
	dir_forward = eye;
	fm_vec4_normalize(&dir_forward);
	fm_vec4_cross(&dir_forward, &up, &dir_left);
	fm_vec4_cross(&up, &dir_left, &dir_forward);
	fm_vec4_normalize(&dir_left);
	
	fm_vec4_mulf(&eye, cameraZoom, &eye);
	fm_vec4_add(&eye, &camera_at, &eye);
	
	fm_euler_angles angles = {};
	angles.yaw = -cameraRotationYaw;
	angles.pitch = -cameraRotationPitch;
	
	camera->locator.pos = eye;
	fm_quat_make_from_euler_angles_yzpxry(&angles, &camera->locator.rot);
	camera->fov = 70.0f;
}

void fg_camera_adjust_by_player_movement(fg_camera_t* camera, fm_mat4* playerMatrix)
{
	fm_vec4_add(&camera->locator.pos, &playerMatrix->w, &camera->locator.pos);
}

void fg_camera_get_directions(fg_camera_t* camera, fm_vec4* dirForward, fm_vec4* dirLeft)
{
	fm_quat rot = camera->locator.rot;
	fm_quat_conj(&rot);
	
	*dirForward = fm_quat_axis_y(&rot);
	fm_vec4_neg(dirForward);
	dirForward->z = 0.0f;
	fm_vec4_normalize(dirForward);
	
	*dirLeft = fm_quat_axis_x(&rot);
	fm_vec4_neg(dirLeft);
	dirLeft->z = 0.0f;
	fm_vec4_normalize(dirLeft);
}

void fg_camera_view_matrix(fg_camera_t* camera, fm_mat4* matrix)
{
	//fm_mat4_lookat_lh(&camera->eye, &camera->at, &camera->up, matrix);
	fm_xform locator = camera->locator;
	fm_quat_rot(&locator.rot, &locator.pos, &locator.pos);
	fm_vec4_neg(&locator.pos);
	
	fm_xform_to_mat4(&locator, matrix);
	fm_mat4_transpose(matrix);
}

void fg_camera_get_eye(fg_camera_t* camera, fm_vec4* eye)
{
	*eye = camera->locator.pos;
}
