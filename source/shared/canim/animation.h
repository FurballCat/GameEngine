/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include "enums.h"
#include "pose.h"	// todo: remove
#include <inttypes.h>
#include <stdbool.h>
	
typedef struct fm_xform fm_xform;
typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;
typedef uint32_t fc_string_hash_t;
typedef struct fm_vec4 fm_vec4;
typedef struct fm_mat4 fm_mat4;

typedef struct fa_pose_t fa_pose_t;
typedef struct fa_rig_t fa_rig_t;
typedef struct fa_anim_clip_t fa_anim_clip_t;
typedef struct fa_pose_stack_t fa_pose_stack_t;

// -----

typedef struct fa_pose_cache_t
{
	fa_pose_t tempPose;
	float alpha;
} fa_pose_cache_t;
	
// **************** COMMANDS **************** //

typedef struct fa_cmd_context_debug_t
{
	uint32_t cmdDrawCursorVerticalPos;
} fa_cmd_context_debug_t;
	
typedef struct fa_cmd_context_t
{
	fa_pose_stack_t* poseStack;
	const fa_pose_cache_t* poseCache;
	const fa_rig_t* rig;
	const fa_anim_clip_t** animClips;
	uint32_t numAnimClips;
	
	const uint8_t* mask;	// optional, default is NULL
	
	fa_cmd_context_debug_t* debug;	// if NULL, don't use debug
} fa_cmd_context_t;

typedef struct fa_cmd_buffer_t
{
	void* data;
	uint32_t size;
} fa_cmd_buffer_t;
	
typedef struct fa_cmd_buffer_recorder_t
{
	void* currPointer;
	uint32_t sizeLeft;
	uint32_t sizeRecorded;
	
	uint32_t poseStackInitialSize;
	uint32_t poseStackSizeTracking;
} fa_cmd_buffer_recorder_t;

typedef enum fa_cmd_status_t
{
	FA_CMD_STATUS_OK = 0,
	FA_CMD_STATUS_STOP = 1,
} fa_cmd_status_t;
	
typedef fa_cmd_status_t (*fa_cmd_func_t)(fa_cmd_context_t* ctx, const void* cmdData);

CANIM_API void fa_cmd_buffer_evaluate(const fa_cmd_buffer_t* buffer, fa_cmd_context_t* ctx);
	
CANIM_API void fa_cmd_buffer_recorder_init(fa_cmd_buffer_recorder_t* recorder, void* outData, uint32_t maxSize);
CANIM_API void fa_cmd_begin(fa_cmd_buffer_recorder_t* recorder, uint32_t poseStackInitialSize);	// poseStackInitialSize = 0 by default
CANIM_API void fa_cmd_end(fa_cmd_buffer_recorder_t* recorder);

CANIM_API void fa_cmd_ref_pose(fa_cmd_buffer_recorder_t* recorder);
CANIM_API void fa_cmd_identity(fa_cmd_buffer_recorder_t* recorder);
CANIM_API void fa_cmd_anim_sample(fa_cmd_buffer_recorder_t* recorder, float time, uint16_t animClipId);
CANIM_API void fa_cmd_anim_sample_additive(fa_cmd_buffer_recorder_t* recorder, float time, uint16_t animClipId);
CANIM_API void fa_cmd_blend2(fa_cmd_buffer_recorder_t* recorder, float alpha);
CANIM_API void fa_cmd_blend_override(fa_cmd_buffer_recorder_t* recorder, float alpha, uint16_t maskId);
CANIM_API void fa_cmd_blend_additive(fa_cmd_buffer_recorder_t* recorder, float alpha);
CANIM_API void fa_cmd_use_cached_pose(fa_cmd_buffer_recorder_t* recorder, uint16_t poseId);
CANIM_API void fa_cmd_apply_mask(fa_cmd_buffer_recorder_t* recorder, uint16_t maskId);
	
// **************** CHARACTER **************** //
	
typedef enum fa_character_layer_t
{
	FA_CHAR_LAYER_FULL_BODY = 0,
	FA_CHAR_LAYER_PARTIAL,
	FA_CHAR_LAYER_COUNT
} fa_character_layer_t;

typedef struct fa_character_anim_info_t
{
	// last world locator
	float worldPos[3];
	
	// desired movement
	float desiredMoveX;
	float desiredMoveY;
	float animToLogicMotionRotationAlpha;	// 0.0f anim, 1.0f logic
	float animToLogicMotionTranslationAlpha;	// 0.0f anim, 1.0f logic
	
	// current state
	float currentYaw;
	
	// output motion
	float rootMotionDeltaX;
	float rootMotionDeltaY;
	float rootMotionDeltaZ;
	float rootMotionDeltaYaw;
	
	// look-at (already in model space)
	float lookAtPoint[3];
	bool useLookAt;
} fa_character_anim_info_t;
	
typedef struct fa_action_ctx_t
{
	float dt;
	float localTime;
	fa_character_anim_info_t* animInfo;
	fa_cmd_buffer_recorder_t* cmdRecorder;
	fa_cmd_context_debug_t* debug;
	
	float rootMotionDeltaX;
	float rootMotionDeltaY;
	float rootMotionDeltaYaw;
} fa_action_ctx_t;

typedef struct fa_action_begin_end_ctx_t
{
	fa_character_anim_info_t* animInfo;
} fa_action_begin_end_ctx_t;

// update is called every frame once action is active (if fade-in-sec is 0.0, then it will be active instantly, otherwise next frame)
typedef void (*fa_action_update_func_t)(const fa_action_ctx_t* ctx, void* userData);

// provides animations to animation system used for the action
typedef const fa_anim_clip_t** (*fa_action_get_anims_func_t)(const void* userData, uint32_t* numAnims);

// called on begin and end of action when it's activated/deactivated, however, might not be called when action is cancelled (see below)
typedef void (*fa_action_begin_end_func_t)(const fa_action_begin_end_ctx_t* ctx, void* userData);

// called in rare case when too many actions are scheduled or the last action is instant blend-in (fade-in-sec 0.0), we need to eat some of actions before activating
typedef void (*fa_action_cancel_func_t)(void* userData);
	
typedef enum fa_curve_type_t
{
	FA_CURVE_UNIFORM_S = 0,	// default
	FA_CURVE_LINEAR
} fa_curve_type_t;
	
typedef enum fa_ik_mode_t
{
	FA_IK_MODE_NONE = 0, // default
	FA_IK_MODE_LEGS = 1,
} fa_ik_mode_t;

typedef struct fa_action_args_t
{
	fa_curve_type_t fadeInCurve;
	float fadeInSec;
	fa_curve_type_t fadeOutCurve;
	float fadeOutSec;
	fa_ik_mode_t ikMode;
	fa_character_layer_t layer;
	fc_string_hash_t layerName;
} fa_action_args_t;
	
typedef struct fa_action_t
{
	void* userData;
	fa_action_begin_end_func_t fnBegin;	// optional, called before the first Update
	fa_action_begin_end_func_t fnEnd;	// optional, called after the last Update
	fa_action_cancel_func_t fnCancel;	// optional, called instead of begin/end when action was eaten by other actions, rare case, but happens
	fa_action_update_func_t fnUpdate;	// required, called every frame
	fa_action_get_anims_func_t fnGetAnims;	// optional
	
	uint64_t globalStartTime; // todo: this shouldn't be an input, global start time should be set once action is started/scheduled
	bool isUsed;
	bool hasBegun;
	
	fa_action_args_t args;
} fa_action_t;
	
typedef struct fa_action_queue_t
{
	fa_action_t actions[4];		// 0 and 1 are current and next actions, the rest are pending, begin/end are called only for 0 and 1
	
	bool cachePoseAfterCurrAction;
	bool cachePoseAfterNextAction;
} fa_action_queue_t;

fa_action_t* fa_action_queue_get_current(fa_action_queue_t* queue);
fa_action_t* fa_action_queue_get_next(fa_action_queue_t* queue);
fa_action_t* fa_action_queue_get_free_slot(fa_action_queue_t* queue);

typedef struct fa_layer_t
{
	fa_action_queue_t actionQueue;
	
	fa_pose_cache_t poseCache;
	bool transitionPoseCached;
	
	fa_mask_t maskID;	// mask for this layer, refers to rig masks
} fa_layer_t;

// fa_action_scheduler_t
// - linked list of actions to schedule, you can put as many as you want, when ticked, it might cache pose of character
// - perhaps when character is invisible, then skip to the last action?

typedef struct fa_character_t
{
	const fa_rig_t* rig;
	
	// default layers (can be selected using fa_character_layer_t enum)
	fa_layer_t layerFullBody;
	fa_layer_t layerPartial;
	
	// named layers
	fa_layer_t layerFace;
	fa_layer_t layerHands;
	
	// resulting pose
	fm_xform* poseMS;
	float* tracks;
	
	uint64_t globalTime;
	
	float lookAtHeadYaw;
	float lookAtHeadPitch;
	float lookAtWeight;
	
	fa_character_anim_info_t animInfo;
} fa_character_t;

// allocate and initialise necessary elements of character
void fa_character_init(fa_character_t* character, const fa_rig_t* rig, fc_alloc_callbacks_t* pAllocCallbacks);
void fa_character_release(fa_character_t* character, fc_alloc_callbacks_t* pAllocCallbacks);

void fa_action_queue_resolve_pre_animate(fa_character_t* character, fa_action_queue_t* queue);
void fa_action_queue_resolve_post_animate(fa_character_t* character, fa_action_queue_t* queue);

typedef struct fa_character_animate_ctx_t
{
	float dt;
	
	void* scratchpadBuffer;
	uint32_t scratchpadBufferSize;
	
	bool showDebug;
} fa_character_animate_ctx_t;
	
CANIM_API void fa_character_animate(fa_character_t* character, const fa_character_animate_ctx_t* ctx);
	
// simple play animation action
typedef struct fa_action_animate_t
{
	const fa_anim_clip_t* animation;
	bool forceLoop;
	bool forceNoLoop;
	float progress;
	
	bool reserved;
	
	bool useLoco;
	bool resetLoco;
	int32_t loopsSoFar;
	float prevLocoPos[4];
	float prevLocoRot[4];
} fa_action_animate_t;
	
CANIM_API void fa_action_animate_func(const fa_action_ctx_t* ctx, void* userData);
CANIM_API const fa_anim_clip_t** fa_action_animate_get_anims_func(const void* userData, uint32_t* numAnims);
CANIM_API void fa_action_animate_begin_func(const fa_action_begin_end_ctx_t* ctx, void* userData);
CANIM_API void fa_action_animate_end_func(const fa_action_begin_end_ctx_t* ctx, void* userData);
CANIM_API void fa_action_animate_cancel_func(void* userData);

CANIM_API void fa_character_schedule_action_simple(fa_character_t* character, fa_action_animate_t* action, const fa_action_args_t* args);
CANIM_API void fa_character_schedule_none_action(fa_character_t* character, const fa_action_args_t* args);

typedef struct fa_action_schedule_data_t
{
	fa_action_get_anims_func_t fnGetAnims;
	fa_action_update_func_t fnUpdate;
	fa_action_begin_end_func_t fnBegin;	// optional, called before the first Update
	fa_action_begin_end_func_t fnEnd;	// optional, called after the last Update
	void* userData;
} fa_action_schedule_data_t;
CANIM_API void fa_character_schedule_action(fa_character_t* character, fa_action_schedule_data_t* data, const fa_action_args_t* args);

// test play two animations action
typedef struct fa_action_animate_test_t
{
	fa_anim_clip_t* anims[2];
	float alpha;
	float timeToNextAnim;
	
	bool equipWeapon;
} fa_action_animate_test_t;
	
CANIM_API void fa_action_animate_test_func(const fa_action_ctx_t* ctx, void* userData);
CANIM_API const fa_anim_clip_t** fa_action_animate_test_get_anims_func(const void* userData, uint32_t* numAnims);

CANIM_API void fa_character_schedule_action_test_simple(fa_character_t* character, fa_action_animate_test_t* action, const fa_action_args_t* args);

// player locomotion action
typedef enum fa_action_player_loco_anims_t
{
	FA_ACTION_PLAYER_LOCO_ANIM_IDLE = 0,
	FA_ACTION_PLAYER_LOCO_ANIM_RUN,
	FA_ACTION_PLAYER_LOCO_ANIM_RUN_TO_IDLE_SHARP,
	FA_ACTION_PLAYER_LOCO_ANIM_IDLE_TO_RUN_0,
	
	FA_ACTION_PLAYER_LOCO_ANIM_COUNT
} fa_action_player_loco_anims_t;

typedef struct fa_action_player_loco_t
{
	fa_anim_clip_t* anims[FA_ACTION_PLAYER_LOCO_ANIM_COUNT];
	
	// will of player movement direction
	float moveX;
	float moveY;
	
	// anim state
	float idleLocalTime;
	float runLocalTime;
	float idleToRunState;
	float blendState;	// idle 0..1 run
	
	uint32_t stateCurr;
	uint32_t stateNext;
	
	float yawOrientation;
	bool isStopping;
	
	bool resetLoco;
	int32_t loopsSoFar;
	float locoRot[4];
	float locoPos[4];
} fa_action_player_loco_t;

CANIM_API void fa_action_player_loco_begin_func(const fa_action_begin_end_ctx_t* ctx, void* userData);
CANIM_API void fa_action_player_loco_end_func(const fa_action_begin_end_ctx_t* ctx, void* userData);
CANIM_API void fa_action_player_loco_update(const fa_action_ctx_t* ctx, void* userData);
CANIM_API const fa_anim_clip_t** fa_action_player_loco_get_anims_func(const void* userData, uint32_t* numAnims);

typedef struct fa_action_player_jump_t
{
	fa_anim_clip_t* anims[2];	// 0: jump-in-place, 1: jump
	float progress;
	int32_t jumpType;
} fa_action_player_jump_t;

CANIM_API void fa_action_player_jump_update(const fa_action_ctx_t* ctx, void* userData);
CANIM_API const fa_anim_clip_t** fa_action_player_jump_get_anims_func(const void* userData, uint32_t* numAnims);

typedef struct fa_action_player_loco_start_t
{
	fa_anim_clip_t* anims[1];	// 0: idle-to-run-0
	
	float finishFromEnd;
	bool isFinished;
	bool ignoreYaw;
	
	bool resetLoco;
	float prevLocoRot[4];
	float prevLocoPos[4];
} fa_action_player_loco_start_t;

CANIM_API void fa_action_player_loco_start_begin_func(const fa_action_begin_end_ctx_t* ctx, void* userData);
CANIM_API void fa_action_player_loco_start_end_func(const fa_action_begin_end_ctx_t* ctx, void* userData);
CANIM_API void fa_action_player_loco_start_update(const fa_action_ctx_t* ctx, void* userData);
CANIM_API const fa_anim_clip_t** fa_action_player_loco_start_get_anims_func(const void* userData, uint32_t* numAnims);

// dangles
typedef struct fa_dangle
{
	fm_vec4* x0;
	fm_vec4* p;
	fm_vec4* v;
	float* d;	// distance costraint, segments length, size=numParticles-1

	uint32_t numParaticles;
	float tAcc;
	float freq;
	float damping;
	
	// collision
	fm_vec4* spherePos;
	float sphereRadius;
} fa_dangle;

typedef struct fa_dangle_sim_ctx
{
	float dt;
} fa_dangle_sim_ctx;

typedef struct fa_dangle_desc
{
	uint32_t numParticles;
	float frequency;
	float dampingCoef;
} fa_dangle_desc;

CANIM_API void fa_dangle_create(const fa_dangle_desc* desc, fa_dangle* dangle, fc_alloc_callbacks_t* pAllocCallbacks);
CANIM_API void fa_dangle_release(fa_dangle* dangle, fc_alloc_callbacks_t* pAllocCallbacks);

CANIM_API void fa_dangle_simulate(const fa_dangle_sim_ctx* ctx, fa_dangle* dangle);
CANIM_API void fa_dangle_to_matrices_z_up(const fa_dangle* dangle, const fm_mat4* attachmentMatrix, fm_mat4* matrices);
CANIM_API void fa_dangle_to_matrices_y_down(const fa_dangle* dangle, const fm_mat4* attachmentMatrix, fm_mat4* matrices);

#ifdef __cplusplus
}
#endif // __cplusplus
