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
typedef struct fg_camera_t fg_camera_t;

void fg_camera_create(fg_camera_t** camera, fc_alloc_callbacks_t* pAllocCallbacks);
void fg_camera_release(fg_camera_t* camera, fc_alloc_callbacks_t* pAllocCallbacks);

typedef struct fg_camera_update_orbit_ctx
{
	float dt;
	float rotationYaw;
	float rotationPitch;
	float zoom;
} fg_camera_update_orbit_ctx;

void fg_camera_update_orbit(fg_camera_t* camera, const fg_camera_update_orbit_ctx* ctx);
void fg_camera_adjust_by_player_movement(fg_camera_t* camera, fm_mat4* playerMatrix);
void fg_camera_get_directions(fg_camera_t* camera, fm_vec4* dirForward, fm_vec4* dirLeft);
void fg_camera_view_matrix(fg_camera_t* camera, fm_mat4* matrix);
void fg_camera_get_eye(fg_camera_t* camera, fm_vec4* eye);

#ifdef __cplusplus
}
#endif // __cplusplus
