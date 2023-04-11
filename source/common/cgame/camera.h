/* Copyright (c) Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
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
CGAME_API FcResult fcCreateCameraSystem(const FcAllocator* allocator, FcCameraSystem** cameraSystem);
CGAME_API void fcDestroyCameraSystem(FcCameraSystem* sys, const FcAllocator* allocator);
CGAME_API void fcCameraSystemUpdate(FcCameraSystem* sys, const FcCameraSystemUpdateCtx* ctx);

// getting final camera params
CGAME_API void fcCameraSystemAdjustByPlayerMovement(FcCameraSystem* sys, fm_mat4* playerMatrix);
CGAME_API void fcCameraSystemGetDirections(FcCameraSystem* sys, fm_vec4* dirForward, fm_vec4* dirLeft);
CGAME_API void fcCameraSystemViewMatrix(FcCameraSystem* sys, fm_mat4* matrix);
CGAME_API void fcCameraSystemGetEye(FcCameraSystem* sys, fm_vec4* eye);
CGAME_API f32 fcCameraSystemGetFOV(FcCameraSystem* sys);

// different camera implementations
typedef struct FcCameraParamsFollow
{
	f32 poleLength;
	f32 height;
	f32 zoom;
	f32 fov;
} FcCameraParamsFollow;

CGAME_API void fcCameraEnableFollow(FcCameraSystem* sys, const FcCameraParamsFollow* params, f32 fadeInSec);

#ifdef __cplusplus
}
#endif // __cplusplus
