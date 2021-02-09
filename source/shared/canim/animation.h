/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include <inttypes.h>
#include <stdbool.h>
	
typedef struct fm_xform fm_xform;
typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;
typedef uint32_t fc_string_hash_t;
	
typedef struct fa_rig_t
{
	fc_string_hash_t* boneNameHashes;
	int16_t* parents;
	fm_xform* refPose;
	uint32_t numBones;
	
	int16_t idxLocoJoint;	// locomotion (root motion) joint index
} fa_rig_t;

CANIM_API void fa_rig_release(fa_rig_t* rig, fc_alloc_callbacks_t* pAllocCallbacks);
	
typedef struct fa_anim_curve_key_t
{
	uint16_t keyTime;
	uint16_t keyData[3];
} fa_anim_curve_key_t;

typedef struct fa_anim_curve_t
{
	uint16_t index;
	uint16_t numKeys;
	fa_anim_curve_key_t* keys;
} fa_anim_curve_t;
	
typedef struct fa_anim_clip_t
{
	fc_string_hash_t name;
	float duration;
	uint32_t numCurves;
	uint32_t numDataKeys;
	fa_anim_curve_t* curves;
	fa_anim_curve_key_t* dataKeys;	// all keys in the animation
} fa_anim_clip_t;
	
CANIM_API void fa_anim_clip_release(fa_anim_clip_t* clip, fc_alloc_callbacks_t* pAllocCallbacks);

typedef enum fa_pose_flags_t
{
	PF_ADDITIVE = 0x1,
} fa_pose_flags_t;
	
typedef struct fa_pose_t
{
	fm_xform* xforms;
	float* tracks;
	uint8_t* weightsXforms;
	uint8_t* weightsTracks;
	
	uint16_t numXforms;
	uint16_t numTracks;
	
	uint32_t flags;
} fa_pose_t;
	
// -----

CANIM_API void fa_pose_set_identity(fa_pose_t* pose);
CANIM_API void fa_pose_set_reference(const fa_rig_t* rig, fa_pose_t* pose);
	
// -----
	
CANIM_API void fa_anim_clip_sample(const fa_anim_clip_t* clip, float time, fa_pose_t* pose);

CANIM_API void fa_pose_copy(fa_pose_t* dest, const fa_pose_t* src);
CANIM_API void fa_pose_local_to_model(fa_pose_t* modelPose, const fa_pose_t* localPose, const int16_t* parentIndices);
	
CANIM_API void fa_pose_blend_linear(fa_pose_t* out, const fa_pose_t* a, const fa_pose_t* b, float alpha);

// -----
	
typedef struct fa_pose_stack_t
{
	void* buffer;
	uint32_t bufferSize;
	uint32_t poseSize;
	
	uint32_t numPoses;
	uint32_t numMaxPoses;
	
	uint32_t numBones;
	uint32_t numTracks;
	
	uint32_t offsetTracks;
	uint32_t offsetWeightXforms;
	uint32_t offsetWeightTracks;
} fa_pose_stack_t;
	
typedef struct fa_pose_stack_desc_t
{
	uint32_t numMaxPoses;
	
	uint32_t numBonesPerPose;
	uint32_t numTracksPerPose;
} fa_pose_stack_desc_t;

CANIM_API void fa_pose_stack_init(fa_pose_stack_t* pStack, const fa_pose_stack_desc_t* desc, void* buffer, uint32_t bufferSize);
CANIM_API void fa_pose_stack_release(fa_pose_stack_t* pStack);
	
CANIM_API void fa_pose_stack_push(fa_pose_stack_t* pStack, uint32_t count);
CANIM_API void fa_pose_stack_pop(fa_pose_stack_t* pStack, uint32_t count);
CANIM_API void fa_pose_stack_get(const fa_pose_stack_t* pStack, fa_pose_t* pPose, uint32_t depth);
	
// **************** COMMANDS **************** //

typedef struct fa_cmd_context_t
{
	fa_pose_stack_t* poseStack;
	const fa_rig_t* rig;
	const fa_anim_clip_t** animClips;
	uint32_t numAnimClips;
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
CANIM_API void fa_cmd_blend2(fa_cmd_buffer_recorder_t* recorder, float alpha);
CANIM_API void fa_cmd_blend_override(fa_cmd_buffer_recorder_t* recorder, float alpha, uint16_t maskId);
CANIM_API void fa_cmd_blend_additive(fa_cmd_buffer_recorder_t* recorder, float alpha);
	
// **************** CHARACTER **************** //
	
typedef enum fa_character_layer_t
{
	FA_CHAR_LAYER_BODY = 0,
	FA_CHAR_LAYER_COUNT
} fa_character_layer_t;
	
typedef struct fa_action_ctx_t
{
	float dt;
	float localTime;
	fa_character_layer_t layer;
	fa_cmd_buffer_recorder_t* cmdRecorder;
} fa_action_ctx_t;
	
typedef void (*fa_action_func_t)(const fa_action_ctx_t* ctx, void* userData);
typedef const fa_anim_clip_t** (*fa_action_get_anims_func_t)(const void* userData, uint32_t* numAnims);
	
typedef enum fa_curve_type_t
{
	FA_CURVE_UNIFORM_S = 0,	// default
	FA_CURVE_LINEAR
} fa_curve_type_t;
	
typedef struct fa_action_t
{
	void* userData;
	fa_action_func_t func;	// if this is NULL, then action is NULL
	fa_action_get_anims_func_t getAnimsFunc;
	
	uint64_t globalStartTime; // todo: this shouldn't be an input, global start time should be set once action is started/scheduled
	float fadeInSec;
	fa_curve_type_t fadeInCurve;
} fa_action_t;
	
typedef struct fa_layer_t
{
	//const uint8_t* mask;
	// optional cached pose - caching result of currAction, so we can move nextAction to currAction, then use nextAction for 3rd action
	fa_action_t currAction;
	fa_action_t nextAction;	// if 3rd action scheduled, then if fade-in is 0 - jump to 3rd, if fade-in is >0, then we have 1 frame of old stuff so we cache the pose
} fa_layer_t;

// fa_action_scheduler_t
// - linked list of actions to schedule, you can put as many as you want, when ticked, it might cache pose of character
// - perhaps when character is invisible, then skip to the last action?
	
typedef struct fa_character_t
{
	const fa_rig_t* rig;
	fa_layer_t layers[FA_CHAR_LAYER_COUNT];
	
	// resulting pose
	fm_xform* poseMS;
	float* tracks;
} fa_character_t;

typedef struct fa_character_animate_ctx_t
{
	float dt;
	uint64_t globalTime;
	
	void* scratchpadBuffer;
	uint32_t scratchpadBufferSize;
} fa_character_animate_ctx_t;
	
CANIM_API void fa_character_animate(fa_character_t* character, const fa_character_animate_ctx_t* ctx);

// simple play animation action
typedef struct fa_action_animate_t
{
	fa_anim_clip_t* animation;
	bool forceLoop;
	bool forceNoLoop;
} fa_action_animate_t;
	
CANIM_API void fa_action_animate_func(const fa_action_ctx_t* ctx, void* userData);
CANIM_API const fa_anim_clip_t** fa_action_animate_get_anims_func(const void* userData, uint32_t* numAnims);
	
#ifdef __cplusplus
}
#endif // __cplusplus
