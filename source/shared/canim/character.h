/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include "enums.h"
#include "animCommands.h"	// todo: remove, because of FcPoseCache
#include "cmath/mathtypes.h"
#include <inttypes.h>
#include <stdbool.h>
	
typedef struct fm_xform fm_xform;
typedef struct FcAllocator FcAllocator;
typedef struct FcMemArenaAllocator FcMemArenaAllocator;
typedef u32 FcStringId;
typedef struct fm_vec4 fm_vec4;
typedef struct fm_mat4 fm_mat4;

typedef struct FcPose FcPose;
typedef struct FcRig FcRig;
typedef struct FcAnimClip FcAnimClip;
typedef struct FcPoseStack FcPoseStack;
typedef struct FcCommandBufferRecorder FcCommandBufferRecorder;
typedef struct FcCommandBufferContextDebug FcCommandBufferContextDebug;

// **************** CHARACTER **************** //
	
typedef enum FcAnimLayerType
{
	FA_CHAR_LAYER_FULL_BODY = 0,
	FA_CHAR_LAYER_PARTIAL,
	FA_CHAR_LAYER_COUNT
} FcAnimLayerType;

typedef struct FcAnimInfo
{
	// last world locator
	fm_vec3 worldPos;
	
	// desired movement
	fm_vec2 desiredMove;
	f32 animToLogicMotionRotationAlpha;	// 0.0f anim, 1.0f logic
	f32 animToLogicMotionTranslationAlpha;	// 0.0f anim, 1.0f logic
	
	// current state
	f32 currentYaw;
	
	// output motion
	fm_vec3 rootMotionDelta;
	f32 rootMotionDeltaYaw;
	
	// look-at (already in model space)
	fm_vec3 lookAtPoint;
	bool useLookAt;
} FcAnimInfo;

typedef struct FcAnimActionCtx
{
	f32 dt;
	f32 localTime;
	FcAnimInfo* animInfo;
	FcCommandBufferRecorder* cmdRecorder;
	FcCommandBufferContextDebug* debug;
	
	f32 rootMotionDeltaX;
	f32 rootMotionDeltaY;
	f32 rootMotionDeltaYaw;
} FcAnimActionCtx;

typedef struct FcAnimActionBeginEndCtx
{
	FcAnimInfo* animInfo;
} FcAnimActionBeginEndCtx;

// update is called every frame once action is active (if fade-in-sec is 0.0, then it will be active instantly, otherwise next frame)
typedef void (*FcAnimActionUpdateFn)(const FcAnimActionCtx* ctx, void* userData);

// provides animations to animation system used for the action
typedef const FcAnimClip** (*FcAnimActionGetAnimsFn)(const void* userData, u32* numAnims);

// called on begin and end of action when it's activated/deactivated, however, might not be called when action is cancelled (see below)
typedef void (*FcAnimActionBeginEndFn)(const FcAnimActionBeginEndCtx* ctx, void* userData);

// called in rare case when too many actions are scheduled or the last action is instant blend-in (fade-in-sec 0.0), we need to eat some of actions before activating
typedef void (*FcAnimActionCancelFn)(void* userData);
	
typedef enum FcCurveType
{
	FA_CURVE_UNIFORM_S = 0,	// default
	FA_CURVE_LINEAR
} FcCurveType;
	
typedef enum FcAnimIKMode
{
	FA_IK_MODE_NONE = 0, // default
	FA_IK_MODE_LEGS = 1,
} FcAnimIKMode;

typedef struct FcAnimActionArgs
{
	FcCurveType fadeInCurve;
	f32 fadeInSec;
	FcCurveType fadeOutCurve;
	f32 fadeOutSec;
	FcAnimIKMode ikMode;
	FcAnimLayerType layer;
	FcStringId layerName;
} FcAnimActionArgs;
	
typedef struct FcAnimAction
{
	void* userData;
	FcAnimActionBeginEndFn fnBegin;	// optional, called before the first Update
	FcAnimActionBeginEndFn fnEnd;	// optional, called after the last Update
	FcAnimActionCancelFn fnCancel;	// optional, called instead of begin/end when action was eaten by other actions, rare case, but happens
	FcAnimActionUpdateFn fnUpdate;	// required, called every frame
	FcAnimActionGetAnimsFn fnGetAnims;	// optional
	
	uint64_t globalStartTime; // todo: this shouldn't be an input, global start time should be set once action is started/scheduled
	bool isUsed;
	bool hasBegun;
	
	FcAnimActionArgs args;
} FcAnimAction;
	
typedef struct FcAnimActionQueue
{
	FcAnimAction actions[4];		// 0 and 1 are current and next actions, the rest are pending, begin/end are called only for 0 and 1
	
	bool cachePoseAfterCurrAction;
	bool cachePoseAfterNextAction;
} FcAnimActionQueue;

FcAnimAction* fcAnimActionQueueGetCurrent(FcAnimActionQueue* queue);
FcAnimAction* fcAnimActionQueueGetNext(FcAnimActionQueue* queue);
FcAnimAction* fcAnimActionQueueGetFreeSlot(FcAnimActionQueue* queue);

typedef struct FcAnimLayer
{
	FcAnimActionQueue actionQueue;
	
	FcPoseCache poseCache;
	bool transitionPoseCached;
	
	FcAnimMask maskID;	// mask for this layer, refers to rig masks
} FcAnimLayer;

// fa_action_scheduler_t
// - linked list of actions to schedule, you can put as many as you want, when ticked, it might cache pose of character
// - perhaps when character is invisible, then skip to the last action?

typedef struct FcAnimCharacter
{
	const FcRig* rig;
	
	// default layers (can be selected using FcAnimLayerType enum)
	FcAnimLayer layerFullBody;
	FcAnimLayer layerPartial;
	
	// named layers
	FcAnimLayer layerFace;
	FcAnimLayer layerHands;
	
	// resulting pose
	fm_xform* poseMS;
	f32* tracks;
	
	// assigned skinning matrices (might be NULL, as character is not owning memory for skin matrices)
	fm_mat4* skinMatrices;
	
	uint64_t globalTime;
	
	f32 lookAtHeadYaw;
	f32 lookAtHeadPitch;
	f32 lookAtWeight;
	
	FcAnimInfo animInfo;
} FcAnimCharacter;

// allocate and initialise necessary elements of character
void fcAnimCharacterInit(FcAnimCharacter* character, const FcRig* rig, FcAllocator* allocator);
void fcAnimCharacterRelease(FcAnimCharacter* character, FcAllocator* allocator);

void fcAnimActionQueueResolvePreAnimate(FcAnimCharacter* character, FcAnimActionQueue* queue);
void fcAnimActionQueueResolvePostAnimate(FcAnimCharacter* character, FcAnimActionQueue* queue);

typedef struct FcAnimCharacterUpdateCtx
{
	f32 dt;
	
	FcMemArenaAllocator* arenaAlloc;
	
	bool showDebug;
} FcAnimCharacterUpdateCtx;
	
CANIM_API void fcAnimCharacterUpdate(FcAnimCharacter* character, const FcAnimCharacterUpdateCtx* ctx);
	
// simple play animation action
typedef struct FcAnimActionAnimate
{
	const FcAnimClip* animation;
	bool forceLoop;
	bool forceNoLoop;
	f32 progress;
	
	bool reserved;
	
	bool useLoco;
	bool resetLoco;
	i32 loopsSoFar;
	f32 prevLocoPos[4];
	f32 prevLocoRot[4];
} FcAnimActionAnimate;
	
CANIM_API void fcAnimActionAnimateFnUpdate(const FcAnimActionCtx* ctx, void* userData);
CANIM_API const FcAnimClip** fcAnimActionAnimateFnGetAnims(const void* userData, u32* numAnims);
CANIM_API void fcAnimActionAnimateFnBegin(const FcAnimActionBeginEndCtx* ctx, void* userData);
CANIM_API void fcAnimActionAnimateFnEnd(const FcAnimActionBeginEndCtx* ctx, void* userData);
CANIM_API void fcAnimActionAnimateFnCancel(void* userData);

CANIM_API void fcAnimCharacterScheduleActionAnimate(FcAnimCharacter* character, FcAnimActionAnimate* action, const FcAnimActionArgs* args);
CANIM_API void fcAnimCharacterScheduleActionNone(FcAnimCharacter* character, const FcAnimActionArgs* args);

typedef struct FcAnimActionScheduleData
{
	FcAnimActionGetAnimsFn fnGetAnims;
	FcAnimActionUpdateFn fnUpdate;
	FcAnimActionBeginEndFn fnBegin;	// optional, called before the first Update
	FcAnimActionBeginEndFn fnEnd;	// optional, called after the last Update
	void* userData;
} FcAnimActionScheduleData;

CANIM_API void fcAnimCharacterScheduleAction(FcAnimCharacter* character, FcAnimActionScheduleData* data, const FcAnimActionArgs* args);

#ifdef __cplusplus
}
#endif // __cplusplus
