#pragma once

/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "ccore/types.h"

typedef struct FcAllocator FcAllocator;
typedef struct fm_mat4 fm_mat4;
typedef struct fm_vec4 fm_vec4;
typedef struct FcCameraSystem FcCameraSystem;

typedef struct FcCameraSystemUpdateCtx
{
	f32 dt;
	f32 rotationYaw;
	f32 rotationPitch;
	f32 zoom;
} FcCameraSystemUpdateCtx;

// creation and update of camera system
FcResult fcCreateCameraSystem(const FcAllocator* allocator, FcCameraSystem** cameraSystem);
void fcDestroyCameraSystem(FcCameraSystem* sys, const FcAllocator* allocator);
void fcCameraSystemUpdate(FcCameraSystem* sys, const FcCameraSystemUpdateCtx* ctx);

// getting final camera params
void fcCameraSystemAdjustByPlayerMovement(FcCameraSystem* sys, fm_mat4* playerMatrix);
void fcCameraSystemGetDirections(FcCameraSystem* sys, fm_vec4* dirForward, fm_vec4* dirLeft);
void fcCameraSystemViewMatrix(FcCameraSystem* sys, fm_mat4* matrix);
void fcCameraSystemGetEye(FcCameraSystem* sys, fm_vec4* eye);
f32 fcCameraSystemGetFOV(FcCameraSystem* sys);

// different camera implementations
typedef struct FcCameraParamsFollow
{
	f32 poleLength;
	f32 height;
	f32 zoom;
	f32 fov;
} FcCameraParamsFollow;

void fcCameraEnableFollow(FcCameraSystem* sys, const FcCameraParamsFollow* params, f32 fadeInSec);

#ifdef __cplusplus
}
#endif // __cplusplus
