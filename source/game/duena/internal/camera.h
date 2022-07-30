#pragma once

/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;
typedef struct fm_mat4 fm_mat4;
typedef struct fm_vec4 fm_vec4;
typedef struct fg_camera_system_t fg_camera_system_t;

typedef struct fg_camera_system_update_ctx
{
	float dt;
	float rotationYaw;
	float rotationPitch;
	float zoom;
} fg_camera_system_update_ctx;

// creation and update of camera system
fg_camera_system_t* fg_camera_system_create(fc_alloc_callbacks_t* pAllocCallbacks);
void fg_camera_system_release(fg_camera_system_t* sys, fc_alloc_callbacks_t* pAllocCallbacks);
void fg_camera_system_update(fg_camera_system_t* sys, const fg_camera_system_update_ctx* ctx);

// getting final camera params
void fg_camera_adjust_by_player_movement(fg_camera_system_t* sys, fm_mat4* playerMatrix);
void fg_camera_get_directions(fg_camera_system_t* sys, fm_vec4* dirForward, fm_vec4* dirLeft);
void fg_camera_view_matrix(fg_camera_system_t* sys, fm_mat4* matrix);
void fg_camera_get_eye(fg_camera_system_t* sys, fm_vec4* eye);
float fg_camera_get_fov(fg_camera_system_t* sys);

// different camera implementations
typedef struct fg_camera_params_follow_t
{
	float poleLength;
	float height;
	float zoom;
} fg_camera_params_follow_t;

void fg_camera_system_enable_camera_follow(fg_camera_system_t* sys, const fg_camera_params_follow_t* params, float fadeInSec);

#ifdef __cplusplus
}
#endif // __cplusplus
