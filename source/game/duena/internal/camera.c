/* Copyright (c) Furball Cat */

#include "camera.h"
#include "cmath/public.h"
#include "ccore/public.h"
#include "string.h"

#define NUM_MAX_CAMERAS 16
#define BYTES_PER_CAMERA_SLOT 128

typedef struct fg_camera_t
{
	fm_xform locator;
	f32 fov;
} fg_camera_t;

typedef struct fg_camera_ctx_t
{
	fg_camera_t* camera;
	const fg_camera_t* lastFrameFinalCamera;
	fm_vec4 ownerPosition;
	
	f32 dt;
	
	f32 rotationYaw;
	f32 rotationPitch;
	f32 zoom;
} fg_camera_ctx_t;

typedef void (*fg_camera_func_t)(fg_camera_ctx_t* ctx, void* userData);

typedef struct fg_camera_slot_t
{
	fg_camera_t camera;
	void* userData;	// each camera has 128 bytes of memory to use
	fg_camera_func_t fnBegin;
	fg_camera_func_t fnUpdate;
	
	f32 fadeInSec;
	f32 fadeInTime;
	
	bool started;
} fg_camera_slot_t;

typedef struct fg_camera_system_t
{
	// stack of active cameras, each updated and then blended into final result
	fg_camera_slot_t cameraStack[NUM_MAX_CAMERAS];
	u32 numCameraStack;
	
	void* stackUserMemory;
	
	// player position, pivot point for cameras
	fm_vec4 posPlayer;
	
	// result of all cameras blended into single camera
	fg_camera_t finalCamera;
} fg_camera_system_t;

fg_camera_system_t g_cameraSystem;

fg_camera_system_t* fg_camera_system_create(fc_alloc_callbacks_t* pAllocCallbacks)
{
	g_cameraSystem.stackUserMemory = FUR_ALLOC_AND_ZERO(BYTES_PER_CAMERA_SLOT * NUM_MAX_CAMERAS, 16, FC_MEMORY_SCOPE_CAMERA, pAllocCallbacks);
	
	return &g_cameraSystem;
}

void fg_camera_system_release(fg_camera_system_t* sys, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(sys->stackUserMemory, pAllocCallbacks);
}

void fg_camera_system_update(fg_camera_system_t* sys, const fg_camera_system_update_ctx* ctx)
{
	fg_camera_ctx_t slotCtx = {0};
	slotCtx.dt = ctx->dt;
	slotCtx.rotationYaw = ctx->rotationYaw;
	slotCtx.rotationPitch = ctx->rotationPitch;
	slotCtx.zoom = ctx->zoom;
	slotCtx.ownerPosition = sys->posPlayer;
	
	const fg_camera_t lastFrameFinalCamera = sys->finalCamera;
	slotCtx.lastFrameFinalCamera = &lastFrameFinalCamera;
	
	fm_xform_identity(&sys->finalCamera.locator);
	sys->finalCamera.fov = 70.0f;
	
	// update each camera on camera stack
	for(u32 i=0; i<sys->numCameraStack; ++i)
	{
		fg_camera_slot_t* slot = &sys->cameraStack[i];
		
		// begin camera if necessary
		slotCtx.camera = &slot->camera;
		
		if(!slot->started)
		{
			if(slot->fnBegin)	// begin is optional
			{
				(*slot->fnBegin)(&slotCtx, slot->userData);
			}
			
			slot->started = true;
		}
		
		// update fade in timer
		slot->fadeInTime = fm_clamp(slot->fadeInTime + ctx->dt, 0.0f, slot->fadeInSec);
		
		// update the camera
		(*slot->fnUpdate)(&slotCtx, slot->userData);
		
		// blend in the camera to final result
		const f32 alpha = fm_curve_uniform_s(slot->fadeInSec > 0.0f ? fm_clamp(slot->fadeInTime / slot->fadeInSec, 0.0f, 1.0f) : 1.0f);
		fm_xform_lerp(&sys->finalCamera.locator, &slot->camera.locator, alpha, &sys->finalCamera.locator);
		sys->finalCamera.fov = sys->finalCamera.fov * (1.0f - alpha) + slot->camera.fov * alpha;
	}
	
	// remove old cameras
	for(i32 i=sys->numCameraStack-1; i>=1; --i)
	{
		fg_camera_slot_t* slot = &sys->cameraStack[i];
		
		const f32 alpha = fm_curve_uniform_s(slot->fadeInSec > 0.0f ? fm_clamp(slot->fadeInTime / slot->fadeInSec, 0.0f, 1.0f) : 1.0f);
		if(alpha >= 1.0f)
		{
			u32 idx = 0;
			for(i32 j=i; j<sys->numCameraStack; ++j)
			{
				fg_camera_slot_t* srcSlot = &sys->cameraStack[j];
				fg_camera_slot_t* dstSlot = &sys->cameraStack[idx];
				
				// copy user data first, while the pointer is valid
				memcpy(dstSlot->userData, srcSlot->userData, BYTES_PER_CAMERA_SLOT);
				
				// remember the pointer
				void* userDataPtr = dstSlot->userData;
				
				// copy slot data (and stomp pointer)
				memcpy(dstSlot, srcSlot, sizeof(fg_camera_slot_t));
				
				// need to fix ptr after memcpy
				dstSlot->userData = userDataPtr;
			}
			
			sys->numCameraStack = sys->numCameraStack - i;
		}
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
	fm_vec4_norm(dirForward);
	
	*dirLeft = fm_quat_axis_x(&rot);
	fm_vec4_neg(dirLeft);
	dirLeft->z = 0.0f;
	fm_vec4_norm(dirLeft);
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

f32 fg_camera_get_fov(fg_camera_system_t* sys)
{
	const fg_camera_t* camera = &sys->finalCamera;
	return camera->fov;
}

typedef struct fg_camera_follow_t
{
	// constants
	f32 poleLength;
	f32 height;
	f32 fov;
	
	// variables
	f32 yaw;
	f32 pitch;
	f32 zoom;
	
} fg_camera_follow_t;

void fg_camera_follow_begin(fg_camera_ctx_t* ctx, void* userData)
{
	const fg_camera_t* lastCamera = ctx->lastFrameFinalCamera;

	// try to find the params that makes this camera the most similar to last frame
	fm_euler_angles angles = {0};
	fm_quat_to_euler(&lastCamera->locator.rot, &angles);
	
	fg_camera_follow_t* data = (fg_camera_follow_t*)userData;
	data->yaw = -angles.yaw;
	data->pitch = -angles.pitch;
}

void fg_camera_follow_update(fg_camera_ctx_t* ctx, void* userData)
{
	fg_camera_follow_t* data = (fg_camera_follow_t*)userData;
	
	const f32 poleLength = data->poleLength;
	fm_vec4 camera_at = {0, 0, data->height, 0};
	const fm_vec4 up = {0, 0, 1, 0};
	
	fm_vec4 eye = {0};
	
	fm_vec4 dir_forward = {0};
	fm_vec4 dir_left = {0};
	
	const f32 rotationSpeed = 2.5f * ctx->dt;
	data->yaw += rotationSpeed * ctx->rotationYaw;
	
	const f32 rotationSpeedPitch = 2.5f * ctx->dt;
	data->pitch += rotationSpeedPitch * ctx->rotationPitch;
	
	data->pitch = fm_clamp(data->pitch, -1.0f, 1.0f);
	const f32 poleLengthFactor = 1.0f - (data->pitch < 0.0f ? 0.5f : 0.2f) * (data->pitch * data->pitch);
	
	const f32 sinRotPitch = sinf(data->pitch);
	const f32 cosRotPitch = cosf(data->pitch);
	
	const f32 sinRotYaw = sinf(data->yaw);
	const f32 cosRotYaw = cosf(data->yaw);
	eye.x = -poleLength * poleLengthFactor * cosRotPitch * sinRotYaw;
	eye.y = -poleLength * poleLengthFactor * cosRotPitch * cosRotYaw;
	eye.z = poleLength * poleLengthFactor * sinRotPitch;
	
	static f32 cameraZoom = 1.0f;
	const f32 zoomSpeed = 1.0f * ctx->dt;
	cameraZoom += zoomSpeed * (ctx->zoom);
	
	dir_forward = eye;
	fm_vec4_norm(&dir_forward);
	fm_vec4_cross(&dir_forward, &up, &dir_left);
	fm_vec4_cross(&up, &dir_left, &dir_forward);
	fm_vec4_norm(&dir_left);
	
	fm_vec4_mulf(&eye, cameraZoom, &eye);
	fm_vec4_add(&eye, &camera_at, &eye);
	
	fm_euler_angles angles = {0};
	angles.yaw = -data->yaw;
	angles.pitch = -data->pitch;
	
	fm_vec4_add(&eye, &ctx->ownerPosition, &eye);
	
	ctx->camera->locator.pos = eye;
	fm_quat_make_from_euler_angles_yzpxry(&angles, &ctx->camera->locator.rot);
	ctx->camera->fov = data->fov;
}

void fg_camera_system_enable_camera_follow(fg_camera_system_t* sys, const fg_camera_params_follow_t* params, f32 fadeInSec)
{
	FUR_ASSERT(sys->numCameraStack < 16);
	
	const u32 idx = sys->numCameraStack;
	sys->numCameraStack++;
	
	fg_camera_slot_t* slot = &sys->cameraStack[idx];
	
	slot->fadeInTime = sys->numCameraStack == 1 ? fadeInSec : 0.0f;	// if it's the only camera, assume it's fully blended in
	slot->fadeInSec = fadeInSec;
	
	slot->userData = (u8*)sys->stackUserMemory + 128 * idx;
	slot->fnUpdate = fg_camera_follow_update;
	slot->fnBegin = fg_camera_follow_begin;
	slot->started = false;
	
	fg_camera_follow_t* data = (fg_camera_follow_t*)sys->cameraStack[idx].userData;
	data->height = params->height;
	data->poleLength = params->poleLength;
	data->zoom = params->zoom;
	data->yaw = 0.0f;
	data->pitch = 0.2f;
	data->fov = params->fov;
}
