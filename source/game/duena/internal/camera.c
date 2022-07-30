/* Copyright (c) Furball Cat */

#include "camera.h"
#include "cmath/public.h"
#include "ccore/public.h"

#define NUM_MAX_CAMERAS 16

typedef struct fg_camera_t
{
	fm_xform locator;
	float fov;
} fg_camera_t;

typedef struct fg_camera_ctx_t
{
	fg_camera_t* camera;
	fm_vec4 ownerPosition;
	
	float dt;
	
	float rotationYaw;
	float rotationPitch;
	float zoom;
} fg_camera_ctx_t;

typedef void (*fg_camera_func_t)(fg_camera_ctx_t* ctx, void* userData);

typedef struct fg_camera_slot_t
{
	fg_camera_t camera;
	void* userData;	// each camera has 128 bytes of memory to use
	fg_camera_func_t fnBegin;
	fg_camera_func_t fnUpdate;
} fg_camera_slot_t;

typedef struct fg_camera_system_t
{
	// stack of active cameras, each updated and then blended into final result
	fg_camera_slot_t cameraStack[NUM_MAX_CAMERAS];
	uint32_t numCameraStack;
	
	void* stackUserMemory;
	
	// player position, pivot point for cameras
	fm_vec4 posPlayer;
	
	// result of all cameras blended into single camera
	fg_camera_t finalCamera;
} fg_camera_system_t;

fg_camera_system_t g_cameraSystem;

fg_camera_system_t* fg_camera_system_create(fc_alloc_callbacks_t* pAllocCallbacks)
{
	g_cameraSystem.stackUserMemory = FUR_ALLOC_AND_ZERO(128 * NUM_MAX_CAMERAS, 16, FC_MEMORY_SCOPE_CAMERA, pAllocCallbacks);
	
	return &g_cameraSystem;
}

void fg_camera_system_release(fg_camera_system_t* sys, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(sys->stackUserMemory, pAllocCallbacks);
}

void fg_camera_system_update(fg_camera_system_t* sys, const fg_camera_system_update_ctx* ctx)
{
	fg_camera_ctx_t slotCtx = {};
	slotCtx.dt = ctx->dt;
	slotCtx.rotationYaw = ctx->rotationYaw;
	slotCtx.rotationPitch = ctx->rotationPitch;
	slotCtx.zoom = ctx->zoom;
	slotCtx.ownerPosition = sys->posPlayer;
	
	// update each camera on camera stack
	for(uint32_t i=0; i<sys->numCameraStack; ++i)
	{
		fg_camera_slot_t* slot = &sys->cameraStack[i];
		
		slotCtx.camera = &slot->camera;
		
		(*slot->fnUpdate)(&slotCtx, slot->userData);
		
		// todo: blend cameras instead of using the last one
		sys->finalCamera = slot->camera;
	}
}

void fg_camera_adjust_by_player_movement(fg_camera_system_t* sys, fm_mat4* playerMatrix)
{
	sys->posPlayer = playerMatrix->w;
}

void fg_camera_get_directions(fg_camera_system_t* sys, fm_vec4* dirForward, fm_vec4* dirLeft)
{
	const fg_camera_t* camera = &sys->finalCamera;
	
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

void fg_camera_view_matrix(fg_camera_system_t* sys, fm_mat4* matrix)
{
	const fg_camera_t* camera = &sys->finalCamera;
	
	//fm_mat4_lookat_lh(&camera->eye, &camera->at, &camera->up, matrix);
	fm_xform locator = camera->locator;
	fm_quat_rot(&locator.rot, &locator.pos, &locator.pos);
	fm_vec4_neg(&locator.pos);
	
	fm_xform_to_mat4(&locator, matrix);
	fm_mat4_transpose(matrix);
}

void fg_camera_get_eye(fg_camera_system_t* sys, fm_vec4* eye)
{
	const fg_camera_t* camera = &sys->finalCamera;
	*eye = camera->locator.pos;
}

float fg_camera_get_fov(fg_camera_system_t* sys)
{
	const fg_camera_t* camera = &sys->finalCamera;
	return camera->fov;
}

typedef struct fg_camera_follow_t
{
	// constants
	float poleLength;
	float height;
	
	// variables
	float yaw;
	float pitch;
	float zoom;
	
} fg_camera_follow_t;

void fg_camera_follow_update(fg_camera_ctx_t* ctx, void* userData)
{
	fg_camera_follow_t* data = (fg_camera_follow_t*)userData;
	
	const float poleLength = data->poleLength;
	fm_vec4 camera_at = {0, 0, data->height, 0};
	const fm_vec4 up = {0, 0, 1, 0};
	
	fm_vec4 eye = {};
	
	fm_vec4 dir_forward = {};
	fm_vec4 dir_left = {};
	
	const float rotationSpeed = 2.5f * ctx->dt;
	data->yaw += rotationSpeed * ctx->rotationYaw;
	
	const float rotationSpeedPitch = 2.5f * ctx->dt;
	data->pitch += rotationSpeedPitch * ctx->rotationPitch;
	
	data->pitch = fm_clamp(data->pitch, -1.0f, 1.0f);
	const float poleLengthFactor = 1.0f - (data->pitch < 0.0f ? 0.5f : 0.2f) * (data->pitch * data->pitch);
	
	const float sinRotPitch = sinf(data->pitch);
	const float cosRotPitch = cosf(data->pitch);
	
	const float sinRotYaw = sinf(data->yaw);
	const float cosRotYaw = cosf(data->yaw);
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
	angles.yaw = -data->yaw;
	angles.pitch = -data->pitch;
	
	fm_vec4_add(&eye, &ctx->ownerPosition, &eye);
	
	ctx->camera->locator.pos = eye;
	fm_quat_make_from_euler_angles_yzpxry(&angles, &ctx->camera->locator.rot);
	ctx->camera->fov = 70.0f;
}

void fg_camera_system_enable_camera_follow(fg_camera_system_t* sys, const fg_camera_params_follow_t* params)
{
	FUR_ASSERT(sys->numCameraStack < 16);
	
	const uint32_t idx = sys->numCameraStack;
	sys->numCameraStack++;
	
	sys->cameraStack[idx].userData = sys->stackUserMemory + 128 * idx;
	sys->cameraStack[idx].fnUpdate = fg_camera_follow_update;
	sys->cameraStack[idx].fnBegin = NULL;
	
	fg_camera_follow_t* data = (fg_camera_follow_t*)sys->cameraStack[idx].userData;
	data->height = params->height;
	data->poleLength = params->poleLength;
	data->zoom = params->zoom;
	data->yaw = 0.0f;
	data->pitch = 0.2f;
}
