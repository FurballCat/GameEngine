/* Copyright (c) 2016-2022 Furball Cat */

#include "character.h"
#include "rig.h"
#include "animClip.h"
#include "poseStack.h"
#include "animCommands.h"
#include "ccore/public.h"
#include "cmath/public.h"
#include <string.h>
#include <stdio.h>

#define MIN(x, y) x < y ? x : y

void fa_action_reset(fa_action_t* action)
{
	memset(action, 0, sizeof(fa_action_t));
}

void fa_character_leg_ik(fa_character_t* character, const fa_ik_setup_t* ikSetup, fa_pose_t* poseLS, fa_pose_t* poseMS, const fm_vec4* targetArg, float weightIK)
{
	fm_vec4 targetFixed = *targetArg;
	targetFixed.z += 0.1f;
	
	fm_xform chainLS[4] = {
		poseLS->xforms[ikSetup->idxBeginParent],
		poseLS->xforms[ikSetup->idxBegin],
		poseLS->xforms[ikSetup->idxMid],
		poseLS->xforms[ikSetup->idxEnd],
	};
	fm_xform chainMS[4] = {
		poseMS->xforms[ikSetup->idxBeginParent],
		poseMS->xforms[ikSetup->idxBegin],
		poseMS->xforms[ikSetup->idxMid],
		poseMS->xforms[ikSetup->idxEnd],
	};
	
	fm_quat originalFootMS = chainMS[3].rot;
	
	const float angleMin = ikSetup->minAngle;
	const float angleMax = ikSetup->maxAngle;
	fm_axis_t hingeAxis = ikSetup->hingeAxisMid;
	
	fm_vec4 endEffector = chainMS[3].pos;
	fm_vec4 target;
	fm_vec4_lerp(&targetFixed, &endEffector, weightIK, &target);
	
	static uint32_t num_iterations = 20;
	for(uint32_t it=0; it<num_iterations; ++it)
	{
		// loop bones in IK setup
		for(uint32_t i=2; i>=1; --i)
		{
			const uint32_t ip = i-1;
			
			endEffector = chainMS[3].pos;
			
			fm_vec4 e_i;
			fm_vec4_sub(&endEffector, &chainMS[i].pos, &e_i);
			fm_vec4 t_i;
			fm_vec4_sub(&target, &chainMS[i].pos, &t_i);
			
			fm_vec4_normalize(&e_i);
			fm_vec4_normalize(&t_i);
			const float angle = -acosf(fm_vec4_dot(&e_i, &t_i));
			const bool canRot = fabsf(angle) > 0.0001f;
			if(canRot)
			{
				fm_vec4 axis;
				fm_vec4_cross(&e_i, &t_i, &axis);
				if(fm_vec4_mag2(&axis) > 0.0f)
				{
					fm_vec4_normalize(&axis);
					
					fm_quat rot;
					fm_quat_rot_axis_angle(&axis, angle, &rot);
					
					fm_quat invMS = chainMS[ip].rot;
					fm_quat_conj(&invMS);
					
					// take axis for hinge
					fm_vec4 jointAxis;
					fm_axis_to_vec4(hingeAxis, &jointAxis);
					fm_quat_rot(&chainMS[i].rot, &jointAxis, &jointAxis);
					
					// rotate
					fm_quat_mul(&rot, &chainMS[i].rot, &chainMS[i].rot);
					
					// hinge constraint
					if(i == 2)
					{
						fm_vec4 jointAxisNew;
						fm_axis_to_vec4(hingeAxis, &jointAxisNew);
						fm_quat_rot(&chainMS[i].rot, &jointAxisNew, &jointAxisNew);
						fm_quat backRot;
						fm_vec4_rot_between(&jointAxisNew, &jointAxis, &backRot);
						fm_quat_mul(&backRot, &chainMS[i].rot, &chainMS[i].rot);
					}
					
					// write back to LS
					fm_quat_mul(&invMS, &chainMS[i].rot, &chainLS[i].rot);
					fm_quat_norm(&chainLS[i].rot);
					
					// constrain angle
					if(i == 2)
					{
						fm_vec4 rotAxis;
						float rotAngle;
						fm_quat_to_axis_angle(&chainLS[i].rot, &rotAxis, &rotAngle);
						
						if(fabsf(rotAngle) > 0.00001f)
						{
							if(rotAngle > FM_PI)
							{
								rotAngle -= 2 * FM_PI;
							}
							
							fm_vec4 origRotAxis;
							fm_axis_to_vec4(hingeAxis, &origRotAxis);
							if(fm_vec4_dot(&origRotAxis, &rotAxis) > 0.0f)
								rotAngle = fm_clamp(rotAngle, angleMin, angleMax);
							else
								rotAngle = fm_clamp(rotAngle, -angleMax, -angleMin);
							
							fm_quat_rot_axis_angle(&rotAxis, rotAngle, &chainLS[i].rot);
							fm_xform_mul(&chainMS[ip], &chainLS[i], &chainMS[i]);
						}
					}
					
					// update children
					for(uint32_t g=i; g<3; ++g)
					{
						fm_xform_mul(&chainMS[g], &chainLS[g+1], &chainMS[g+1]);
					}
				}
			}
			
		}
	}
	
	// recreate original foot MS orientation
	fm_quat invCurrKneeMS = chainMS[2].rot;
	fm_quat_conj(&invCurrKneeMS);
	fm_quat footCorrection;
	fm_quat_mul(&invCurrKneeMS, &originalFootMS, &footCorrection);
	chainLS[3].rot = footCorrection;
	
	// write results to poseLS
	poseLS->xforms[ikSetup->idxBegin] = chainLS[1];
	poseLS->xforms[ikSetup->idxMid] = chainLS[2];
	poseLS->xforms[ikSetup->idxEnd] = chainLS[3];
}

float fa_action_get_local_time(const fa_action_t* action, const fa_character_t* character)
{
	float localTime = -1.0f;
	
	if(character->globalTime >= action->globalStartTime)
	{
		localTime = (float)((character->globalTime - action->globalStartTime) / 1000000.0);
	}
	FUR_ASSERT(localTime != -1.0f);
	
	return localTime;
}

float fa_action_get_alpha(fa_character_t* character, const fa_action_t* action)
{
	if(!action->isUsed)
		return 0.0f;
	
	float alpha = 1.0f;
	
	const fa_action_args_t* args = &action->args;
	const float localTime = fa_action_get_local_time(action, character);
	
	if(args->fadeInSec > 0.0f)
	{
		alpha = fm_clamp(localTime / args->fadeInSec, 0.0f, 1.0f);
		if(args->fadeInCurve == FA_CURVE_UNIFORM_S)
		{
			alpha = fm_curve_uniform_s(alpha);
		}
	}
	
	return alpha;
}

typedef struct fa_cross_layer_context_t
{
	fa_pose_stack_t* poseStack;
	
	void* scratchMemory;
	uint32_t scratchMemorySize;
	
	fa_cmd_context_debug_t* debug;
	
	float dt;
	
	float outWeightLegsIK;
	
	float rootMotionDeltaX;
	float rootMotionDeltaY;
	float rootMotionDeltaYaw;
	
	fm_vec4 lookAtMS;
	float lookAtWeight;
} fa_cross_layer_context_t;

void fa_character_layer_cache_pose(fa_layer_t* layer, fa_cross_layer_context_t* ctx, float alpha)
{
	fa_pose_t outPose;
	fa_pose_stack_get(ctx->poseStack, &outPose, 0);
	
	fa_pose_copy(&layer->poseCache.tempPose, &outPose);
	layer->transitionPoseCached = true;
	layer->poseCache.alpha = alpha;
	
	if(ctx->debug)
	{
		const float color[4] = FUR_COLOR_RED;
		fc_dbg_text(-450.0f, 1.0f, "caching_pose", color);
	}
}

void fa_character_action_animate(fa_character_t* character, fa_layer_t* layer, fa_action_t* action, fa_cross_layer_context_t* ctx)
{
	FUR_ASSERT(action->fnUpdate != NULL);
	
	fa_cmd_buffer_t animCmdBuffer = { ctx->scratchMemory, ctx->scratchMemorySize };
	fa_cmd_buffer_recorder_t recorder = {};
	fa_cmd_buffer_recorder_init(&recorder, animCmdBuffer.data, animCmdBuffer.size);
	
	// record commands
	FUR_ASSERT(action->fnUpdate != NULL);
	FUR_ASSERT(action->fnGetAnims != NULL);
	
	const float localTime = fa_action_get_local_time(action, character);
	
	fa_action_ctx_t actionCtx = {};
	actionCtx.dt = ctx->dt;
	actionCtx.cmdRecorder = &recorder;
	actionCtx.animInfo = &character->animInfo;
	actionCtx.localTime = localTime;
	actionCtx.debug = ctx->debug;
	
	// record commands
	{
		fa_cmd_begin(&recorder, ctx->poseStack->numPoses);
		action->fnUpdate(&actionCtx, action->userData);
		fa_cmd_end(&recorder);
	}
	
	// evaluate commands
	fa_cmd_context_t animCtx = {};
	animCtx.animClips = action->fnGetAnims(action->userData, &animCtx.numAnimClips);
	animCtx.rig = character->rig;
	animCtx.poseStack = ctx->poseStack;
	animCtx.poseCache = &layer->poseCache;
	animCtx.debug = ctx->debug;
	animCtx.mask = fa_rig_get_mask(character->rig, layer->maskID);
	
	fa_cmd_buffer_evaluate(&animCmdBuffer, &animCtx);
	
	ctx->rootMotionDeltaX = actionCtx.rootMotionDeltaX;
	ctx->rootMotionDeltaY = actionCtx.rootMotionDeltaY;
	ctx->rootMotionDeltaYaw = actionCtx.rootMotionDeltaYaw;
}

void fa_action_safely_cancel(fa_action_t* action)
{
	if(action->isUsed && action->fnCancel)
	{
		action->fnCancel(action->userData);
	}
}

fa_action_t* fa_action_queue_get_current(fa_action_queue_t* queue)
{
	if(queue->actions[0].isUsed)
	{
		return &queue->actions[0];
	}
	
	return NULL;
}

fa_action_t* fa_action_queue_get_next(fa_action_queue_t* queue)
{
	if(queue->actions[1].isUsed)
	{
		return &queue->actions[1];
	}
	
	return NULL;
}

fa_action_t* fa_action_queue_get_free_slot(fa_action_queue_t* queue)
{
	// find free slot
	for(uint32_t i=0; i<4; ++i)
	{
		if(queue->actions[i].isUsed == false)
		{
			return &queue->actions[i];
		}
	}
	
	// if not found, make some slot free
	fa_action_safely_cancel(&queue->actions[2]); // cancel action [2]
	queue->actions[2] = queue->actions[3];	// move action [3] to [2]
	fa_action_reset(&queue->actions[3]);	// clear unused slot
	
	return &queue->actions[3];	// return free, unused slot
}

void fa_action_safely_begin(fa_action_begin_end_ctx_t* ctx, fa_action_t* action)
{
	if(action->isUsed && action->fnBegin && !action->hasBegun)
	{
		action->fnBegin(ctx, action->userData);
		action->hasBegun = true;
	}
}

void fa_action_safely_end(fa_action_begin_end_ctx_t* ctx, fa_action_t* action)
{
	if(action->isUsed && action->fnEnd && action->hasBegun)
	{
		action->fnEnd(ctx, action->userData);
		action->hasBegun = false;
	}
}

void fa_character_init(fa_character_t* character, const fa_rig_t* rig, fc_alloc_callbacks_t* pAllocCallbacks)
{
	character->rig = rig;
	
	const uint16_t numBones = rig->numBones;
	
	character->poseMS = FUR_ALLOC_ARRAY_AND_ZERO(fm_xform, numBones, 16, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
	character->layerFullBody.poseCache.tempPose.xforms = FUR_ALLOC_ARRAY_AND_ZERO(fm_xform, numBones, 16, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
	character->layerFullBody.poseCache.tempPose.weightsXforms = FUR_ALLOC_ARRAY_AND_ZERO(uint8_t, numBones, 16, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
	character->layerFullBody.poseCache.tempPose.numXforms = numBones;
	
	character->layerPartial.poseCache.tempPose.xforms = FUR_ALLOC_ARRAY_AND_ZERO(fm_xform, numBones, 16, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
	character->layerPartial.poseCache.tempPose.weightsXforms = FUR_ALLOC_ARRAY_AND_ZERO(uint8_t, numBones, 16, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
	character->layerPartial.poseCache.tempPose.numXforms = numBones;
	character->layerPartial.maskID = FA_MASK_UPPER_BODY;
	
	character->layerFace.poseCache.tempPose.xforms = FUR_ALLOC_ARRAY_AND_ZERO(fm_xform, numBones, 16, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
	character->layerFace.poseCache.tempPose.weightsXforms = FUR_ALLOC_ARRAY_AND_ZERO(uint8_t, numBones, 16, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
	character->layerFace.poseCache.tempPose.numXforms = numBones;
	character->layerFace.maskID = FA_MASK_FACE;
	
	character->layerHands.poseCache.tempPose.xforms = FUR_ALLOC_ARRAY_AND_ZERO(fm_xform, numBones, 16, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
	character->layerHands.poseCache.tempPose.weightsXforms = FUR_ALLOC_ARRAY_AND_ZERO(uint8_t, numBones, 16, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
	character->layerHands.poseCache.tempPose.numXforms = numBones;
	character->layerHands.maskID = FA_MASK_HANDS;
}

void fa_character_release(fa_character_t* character, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(character->poseMS, pAllocCallbacks);
	
	FUR_FREE(character->layerFullBody.poseCache.tempPose.xforms, pAllocCallbacks);
	FUR_FREE(character->layerFullBody.poseCache.tempPose.weightsXforms, pAllocCallbacks);
	
	FUR_FREE(character->layerPartial.poseCache.tempPose.xforms, pAllocCallbacks);
	FUR_FREE(character->layerPartial.poseCache.tempPose.weightsXforms, pAllocCallbacks);
	
	FUR_FREE(character->layerFace.poseCache.tempPose.xforms, pAllocCallbacks);
	FUR_FREE(character->layerFace.poseCache.tempPose.weightsXforms, pAllocCallbacks);
	
	FUR_FREE(character->layerHands.poseCache.tempPose.xforms, pAllocCallbacks);
	FUR_FREE(character->layerHands.poseCache.tempPose.weightsXforms, pAllocCallbacks);
}

void fa_action_queue_resolve_pre_animate(fa_character_t* character, fa_action_queue_t* queue)
{
	fa_action_begin_end_ctx_t ctx = {};
	ctx.animInfo = &character->animInfo;
	
	// check if any of the pending actions should be instantly activated
	if(fa_action_get_alpha(character, &queue->actions[3]) >= 1.0)
	{
		// end old actions
		fa_action_safely_end(&ctx, &queue->actions[0]);
		fa_action_safely_end(&ctx, &queue->actions[1]);
		
		// rare case when we need to cancel action [2], as it's eaten up by action [3]
		fa_action_safely_cancel(&queue->actions[2]);
		
		// move pending actions
		queue->actions[0] = queue->actions[3];
		
		// clear unused slots
		fa_action_reset(&queue->actions[1]);
		fa_action_reset(&queue->actions[2]);
		fa_action_reset(&queue->actions[3]);
	}
	else if(fa_action_get_alpha(character, &queue->actions[2]) >= 1.0)
	{
		// end old actions
		fa_action_safely_end(&ctx, &queue->actions[0]);
		fa_action_safely_end(&ctx, &queue->actions[1]);
		
		// move pending actions
		queue->actions[0] = queue->actions[2];
		queue->actions[1] = queue->actions[3];
		
		// clear unused slots
		fa_action_reset(&queue->actions[2]);
		fa_action_reset(&queue->actions[3]);
	}
	else if(queue->actions[2].isUsed && queue->actions[3].isUsed)
	{
		queue->cachePoseAfterNextAction = true;
	}
	else if(queue->actions[2].isUsed)
	{
		queue->cachePoseAfterCurrAction = true;
	}
}

void fa_action_queue_resolve_post_animate(fa_character_t* character, fa_action_queue_t* queue)
{
	fa_action_begin_end_ctx_t ctx = {};
	ctx.animInfo = &character->animInfo;
	
	if(queue->cachePoseAfterNextAction) // case of cache for 2 actions
	{
		// end old actions
		fa_action_safely_end(&ctx, &queue->actions[0]);
		fa_action_safely_end(&ctx, &queue->actions[1]);
		
		// move pending actions
		queue->actions[0] = queue->actions[2];
		queue->actions[1] = queue->actions[3];
		
		// clear unused slots
		fa_action_reset(&queue->actions[2]);
		fa_action_reset(&queue->actions[3]);
		
		queue->cachePoseAfterNextAction = false;
	}
	else if(queue->cachePoseAfterCurrAction)	// case of cache for 1 action
	{
		// end old actions
		fa_action_safely_end(&ctx, &queue->actions[0]);
		
		// move pending actions
		queue->actions[0] = queue->actions[1];
		queue->actions[1] = queue->actions[2];
		
		// clear unused slots
		fa_action_reset(&queue->actions[2]);
		fa_action_reset(&queue->actions[3]);
		
		queue->cachePoseAfterCurrAction = false;
	}
	else if(fa_action_get_alpha(character, &queue->actions[1]) >= 1.0)	// normal case, just next action
	{
		// end old actions
		fa_action_safely_end(&ctx, &queue->actions[0]);
		
		// move actions
		queue->actions[0] = queue->actions[1];
		queue->actions[1] = queue->actions[2];
		queue->actions[2] = queue->actions[3];
		
		// clear unused slots
		fa_action_reset(&queue->actions[1]);
		fa_action_reset(&queue->actions[2]);
		fa_action_reset(&queue->actions[3]);
	}
}

void fa_character_layer_animate(fa_character_t* character, fa_cross_layer_context_t* ctx, fa_layer_t* layer)
{
	// resolve scheduled actions
	fa_action_queue_resolve_pre_animate(character, &layer->actionQueue);
	
	fa_action_t* currAction = &layer->actionQueue.actions[0];
	fa_action_t* nextAction = &layer->actionQueue.actions[1];
	
	const float nextAlpha = fa_action_get_alpha(character, nextAction);
	const float currAlpha = fa_action_get_alpha(character, currAction) * (1.0f - nextAlpha);
	const float cachedPoseAlpha = layer->poseCache.alpha * (1.0f - currAlpha);
	
	// copy cached pose if required
	if(layer->transitionPoseCached && cachedPoseAlpha > 0.0f)
	{
		fa_pose_t outPose;
		fa_pose_stack_get(ctx->poseStack, &outPose, 0);
		fa_pose_blend_linear(&outPose, &layer->poseCache.tempPose, &outPose, cachedPoseAlpha);		// blend to include pose weights/mask
	}
	else
	{
		layer->transitionPoseCached = false;
	}
	
	// todo: refactor that
	fa_action_begin_end_ctx_t beginEndCtx = {};
	beginEndCtx.animInfo = &character->animInfo;
	fa_action_safely_begin(&beginEndCtx, currAction);
	fa_action_safely_begin(&beginEndCtx, nextAction);
	
	// animate current action
	if(currAction->fnUpdate != NULL)
	{
		fa_action_t* action = currAction;
		fa_character_action_animate(character, layer, action, ctx);
		
		if(layer->actionQueue.cachePoseAfterCurrAction)
		{
			fa_character_layer_cache_pose(layer, ctx, currAlpha);
		}
		
		// blend in result
		{
			fa_pose_t actionPose;
			fa_pose_t outPose;
			fa_pose_stack_get(ctx->poseStack, &actionPose, 0);
			fa_pose_stack_get(ctx->poseStack, &outPose, 1);
			fa_pose_blend_linear(&outPose, &actionPose, &outPose, currAlpha);
			fa_pose_stack_pop(ctx->poseStack, 1);
		}
	}
	
	// animate next action
	if(nextAction->fnUpdate != NULL)
	{
		fa_action_t* action = nextAction;
		fa_character_action_animate(character, layer, action, ctx);
		
		if(layer->actionQueue.cachePoseAfterNextAction)
		{
			fa_character_layer_cache_pose(layer, ctx, nextAlpha);
		}
		
		// blend in result
		{
			fa_pose_t actionPose;
			fa_pose_t outPose;
			fa_pose_stack_get(ctx->poseStack, &actionPose, 0);
			fa_pose_stack_get(ctx->poseStack, &outPose, 1);
			fa_pose_blend_linear(&outPose, &actionPose, &outPose, nextAlpha);
			fa_pose_stack_pop(ctx->poseStack, 1);
		}
	}
	
	// only body layer can affect legs IK, it's secured outside this function
	{
		const float currIK = currAction->args.ikMode == FA_IK_MODE_LEGS ? currAlpha : 0.0f;
		const float nextIK = nextAction->args.ikMode == FA_IK_MODE_LEGS ? 1.0f : 0.0f;
		
		const float weightIK = currIK * (1.0f - nextAlpha) + nextIK * nextAlpha;
		ctx->outWeightLegsIK = weightIK;
	}
	
	fa_action_queue_resolve_post_animate(character, &layer->actionQueue);
}

void fa_character_ik(fa_character_t* character, fa_cross_layer_context_t* layerCtx)
{
	// inverse kinematics
	{
		const float weightIK = layerCtx->outWeightLegsIK;
		
		if(weightIK > 0.0f)
		{
			fa_pose_stack_push(layerCtx->poseStack, 1);
			
			fa_pose_t poseMS;
			fa_pose_stack_get(layerCtx->poseStack, &poseMS, 0);
			fa_pose_t poseLS;
			fa_pose_stack_get(layerCtx->poseStack, &poseLS, 1);
			
			fa_pose_local_to_model(&poseMS, &poseLS, character->rig->parents);
			
			fm_vec4 leftTarget = poseMS.xforms[character->rig->ikLeftLeg.idxEnd].pos;
			leftTarget.z = -0.2f;
			fm_vec4 rightTarget = poseMS.xforms[character->rig->ikRightLeg.idxEnd].pos;
			rightTarget.z = 0.0f;
			
			// pelvis height correction
			{
				fm_vec4 leftHipPos = poseMS.xforms[character->rig->ikLeftLeg.idxBeginParent].pos;
				fm_vec4 rightHipPos = poseMS.xforms[character->rig->ikRightLeg.idxBeginParent].pos;
				fm_vec4 leftTargetDir;
				fm_vec4_sub(&leftTarget, &leftHipPos, &leftTargetDir);
				fm_vec4 rightTargetDir;
				fm_vec4_sub(&rightTarget, &rightHipPos, &rightTargetDir);
				
				const float leftDistance = fm_vec4_mag(&leftTargetDir);
				const float rightDistance = fm_vec4_mag(&rightTargetDir);
				fm_vec4 leftLegVec = poseLS.xforms[character->rig->ikLeftLeg.idxMid].pos;
				fm_vec4_add(&leftLegVec, &poseLS.xforms[character->rig->ikLeftLeg.idxEnd].pos, &leftLegVec);
				fm_vec4 rightLegVec = poseLS.xforms[character->rig->ikRightLeg.idxMid].pos;
				fm_vec4_add(&rightLegVec, &poseLS.xforms[character->rig->ikRightLeg.idxEnd].pos, &rightLegVec);
				
				const float footCorrectionDistance = 0.15f;
				
				const float leftLegLength = fm_vec4_mag(&leftLegVec) + footCorrectionDistance;
				const float rightLegLength = fm_vec4_mag(&rightLegVec) + footCorrectionDistance;
				
				float pelvisCorrectionHeight = 0.0f;
				if(leftLegLength < leftDistance)
					pelvisCorrectionHeight = leftDistance - leftLegLength;
				if(rightLegLength < (rightDistance - pelvisCorrectionHeight))
					pelvisCorrectionHeight = rightDistance - rightLegLength;
				
				poseLS.xforms[character->rig->ikLeftLeg.idxBeginParent].pos.y -= pelvisCorrectionHeight * weightIK;
				fa_pose_local_to_model(&poseMS, &poseLS, character->rig->parents);
			}
			
			fa_character_leg_ik(character, &character->rig->ikLeftLeg, &poseLS, &poseMS, &leftTarget, weightIK);
			fa_character_leg_ik(character, &character->rig->ikRightLeg, &poseLS, &poseMS, &rightTarget, weightIK);
			
			fa_pose_stack_pop(layerCtx->poseStack, 1);
		}
	}
}

void fa_character_look_at(fa_character_t* character, fa_pose_t* poseLS, fa_pose_t* poseMS,
						  float weight, fm_vec4* lookAtPointMS)
{
	// no need to apply look-at if the weight is 0.0
	if(weight <= 0.0f)
		return;
	
	// get look at setup and look at point in model space
	const fa_look_at_setup_t* setup = &character->rig->headLookAt;
	fm_vec4 lookAtMS = *lookAtPointMS;
	
	// transform look at from model space to head space
	fm_xform locator = poseMS->xforms[setup->idxHead];
	fm_vec4 lookAtLocatorSpace = {};
	fm_xform_apply_inv(&locator, &lookAtMS, &lookAtLocatorSpace);
	
	// assume +X axis is forward direction (depends on the rig)
	fm_vec4 forward = {1.0f, 0.0f, 0.0f, 0.0f};
	
	// constrain look-at to some sane ranges
	{
		// remainder: head space here means (+z right, -z left, +y up, -y down, +x forward, -x backward)
		fm_vec4 dir = lookAtLocatorSpace;
		
		float yaw = -acosf(dir.z / sqrtf(dir.x * dir.x + dir.z * dir.z)) + M_PI_2;
		float pitch = -acosf(dir.y / sqrtf(dir.x * dir.x + dir.y * dir.y)) + M_PI_2;
		
		// fixed yaw to either side when direction is facing backwards
		if(dir.x < 0.0f)
			yaw = fm_sign(yaw) * setup->limitYaw;
		
		// keep within yaw limits
		if(yaw < -setup->limitYaw)
			yaw = -setup->limitYaw;
		else if(yaw > setup->limitYaw)
			yaw = setup->limitYaw;
		
		// keep within pitch limiets - notice different limit for down and up
		if(pitch < -setup->limitPitchDown)
			pitch = -setup->limitPitchDown;
		else if(pitch > setup->limitPitchUp)
			pitch = setup->limitPitchUp;
		
		// smoothly approach new yaw (low-pass filter)
		character->lookAtHeadYaw = character->lookAtHeadYaw * 0.95f + yaw * 0.05f;
		character->lookAtHeadPitch = character->lookAtHeadPitch * 0.95f + pitch * 0.05f;
		
		// prepare new look-at position
		fm_quat yawRot = {};
		fm_quat_make_from_axis_angle(0.0f, 1.0f, 0.0f, character->lookAtHeadYaw, &yawRot);
		
		fm_quat pitchRot = {};
		fm_quat_make_from_axis_angle(0.0f, 0.0f, -1.0f, character->lookAtHeadPitch, &pitchRot);
		
		fm_quat_rot(&yawRot, &forward, &dir);
		fm_quat_rot(&pitchRot, &dir, &dir);
		const float mag = fm_vec4_mag(&lookAtLocatorSpace);
		fm_vec4_mulf(&dir, mag, &lookAtLocatorSpace);
	}
	
	// calculate final look-at rotation correction with weight
	fm_quat headCorrection = {};
	fm_vec4_rot_between(&forward, &lookAtLocatorSpace, &headCorrection);
	
	// apply weight to look-at
	fm_quat identity = {};
	fm_quat_identity(&identity);
	fm_quat_slerp(&identity, &headCorrection, weight, &headCorrection);
	
	// apply look-at rotation correction to local space
	fm_quat_mul(&headCorrection, &poseLS->xforms[setup->idxHead].rot, &poseLS->xforms[setup->idxHead].rot);
}

void fa_character_animate(fa_character_t* character, const fa_character_animate_ctx_t* ctx)
{
	// pre-process inputs in anim info
	fm_vec4 lookAtMS = {};
	
	{
		fa_character_anim_info_t* info = &character->animInfo;
		
		// get world locator of the character
		const fm_vec4 posWS = {info->worldPos.x, info->worldPos.y, info->worldPos.z, 1.0f};
		fm_quat rotWS = {};
		fm_quat_make_from_axis_angle(0.0f, 0.0f, 1.0f, info->currentYaw, &rotWS);
		
		// move look-at WS to MS
		const fm_vec4 lookAtWS = {info->lookAtPoint.x, info->lookAtPoint.y, info->lookAtPoint.z, 1.0f};
		fm_vec4_sub(&lookAtWS, &posWS, &lookAtMS);
		
		fm_quat invRotWS = rotWS;
		fm_quat_conj(&invRotWS);
		fm_quat_rot(&invRotWS, &lookAtMS, &lookAtMS);
	}
	
	// allocate pose stack and command buffer memory
	const uint32_t poseStackSize = 128 * 1024;
	void* animPoseStackMemory = fc_mem_arena_alloc(ctx->arenaAlloc, poseStackSize, 8);
	
	const uint32_t animCmdBufferSize = 32 * 1024;
	void* animCmdBufferMemory = fc_mem_arena_alloc(ctx->arenaAlloc, animCmdBufferSize, 0);
	
	// init pose stack - pose stack is shared across multiple command buffers
	fa_pose_stack_t poseStack = {};
	
	{
		fa_pose_stack_desc_t desc = {};
		
		desc.numBonesPerPose = character->rig->numBones;
		desc.numTracksPerPose = 0;
		desc.numMaxPoses = 4;
		
		fa_pose_stack_init(&poseStack, &desc, animPoseStackMemory, poseStackSize);
	}
	
	fa_cmd_context_debug_t debug = {};
	
	// update layers
	fa_cross_layer_context_t layerCtx = {};
	layerCtx.dt = ctx->dt;
	layerCtx.poseStack = &poseStack;
	layerCtx.scratchMemory = animCmdBufferMemory;
	layerCtx.scratchMemorySize = animCmdBufferSize;
	layerCtx.debug = ctx->showDebug ? &debug : NULL;
	
	fa_pose_t poseLS;
	
	// reset pose to ref pose
	fa_pose_stack_push(&poseStack, 1);
	fa_pose_stack_get(&poseStack, &poseLS, 0);
	{
		FUR_ASSERT(poseLS.numXforms == character->rig->numBones);
		
		for(uint32_t i=0; i<poseLS.numXforms; ++i)
		{
			poseLS.xforms[i] = character->rig->refPose[i];
			poseLS.weightsXforms[i] = 255;
		}
		
		for(uint32_t i=0; i<poseLS.numTracks; ++i)
		{
			poseLS.tracks[i] = 0.0f;
			poseLS.weightsTracks[i] = 255;
		}
		
		fm_quat_identity(&poseLS.xforms[character->rig->idxLocoJoint].rot);
		fm_vec4_zeros(&poseLS.xforms[character->rig->idxLocoJoint].pos);
	}
	
	// body
	FUR_PROFILE("body-layer")
	{
		fa_character_layer_animate(character, &layerCtx, &character->layerFullBody);
	}
	
	// store weight IK
	const float weightLegsIK = layerCtx.outWeightLegsIK;
	
	// partial layer, can be applied anywhere, but does not interrupt full body
	FUR_PROFILE("partial-layer")
	{
		fa_character_layer_animate(character, &layerCtx, &character->layerPartial);
	}
	
	// hands
	FUR_PROFILE("hands-layer")
	{
		fa_character_layer_animate(character, &layerCtx, &character->layerHands);
	}
	
	// face
	FUR_PROFILE("face-layer")
	{
		fa_character_layer_animate(character, &layerCtx, &character->layerFace);
	}
	
	// inverse kinematics
	FUR_PROFILE("ik")
	{
		layerCtx.outWeightLegsIK = weightLegsIK;
		fa_character_ik(character, &layerCtx);
	}
	
	// look-at
	FUR_PROFILE("look-at")
	{
		static float time = 0.0f;
		time += ctx->dt;
		
		float lookAtWeightTarget = 0.0f;
		
		if(character->animInfo.useLookAt)
		{
			layerCtx.lookAtMS = lookAtMS;
			
			lookAtWeightTarget = 0.8f;
		}
		
		character->lookAtWeight = character->lookAtWeight * 0.95f + lookAtWeightTarget * 0.05f;
		layerCtx.lookAtWeight = character->lookAtWeight;
		
		if(character->lookAtWeight <= 0.0f)
		{
			character->lookAtHeadYaw = 0.0f;
			character->lookAtHeadPitch = 0.0f;
		}
		
		// calculate and push temporary MS pose
		fa_pose_stack_push(&poseStack, 1);
		fa_pose_t poseMS;
		fa_pose_stack_get(&poseStack, &poseMS, 0);
		fa_pose_t poseLS;
		fa_pose_stack_get(&poseStack, &poseLS, 1);
		fa_pose_local_to_model(&poseMS, &poseLS, character->rig->parents);
		
		fa_character_look_at(character, &poseLS, &poseMS, layerCtx.lookAtWeight, &layerCtx.lookAtMS);
		
		// pop temporary MS pose
		fa_pose_stack_pop(&poseStack, 1);
	}
	
	// ragdoll

	// convert to model space
	FUR_PROFILE("ls-to-ms")
	{
		const int16_t* parentIndices = character->rig->parents;
		fa_pose_t poseMS = {};
		poseMS.xforms = character->poseMS;
		poseMS.numXforms = character->rig->numBones;
		fa_pose_local_to_model(&poseMS, &poseLS, parentIndices);
	}
	
	// apply root motion
	{
		fa_pose_t poseMS = {};
		poseMS.xforms = character->poseMS;
		poseMS.numXforms = character->rig->numBones;
		
		const int16_t idxLocoJoint = character->rig->idxLocoJoint;
		if(idxLocoJoint != -1)
		{
			FUR_ASSERT(idxLocoJoint < poseMS.numXforms);
			
			fm_xform animMotionDelta = poseMS.xforms[idxLocoJoint];
			
			fm_vec4_sub(&animMotionDelta.pos, &poseMS.xforms[0].pos, &animMotionDelta.pos);
			
			fm_quat rootQuatConj = poseMS.xforms[0].rot;
			fm_quat_conj(&rootQuatConj);
			
			fm_quat_mul(&animMotionDelta.rot, &rootQuatConj, &animMotionDelta.rot);
			fm_quat_norm(&animMotionDelta.rot);
			
			// we know it's gonna be motion 2D (for now) so axis should be vertical in pose space
			fm_vec4 axis = {};
			float animYawDelta = 0.0f;
			fm_quat_to_axis_angle(&animMotionDelta.rot, &axis, &animYawDelta);
			
			animYawDelta *= -fm_sign(axis.y);	// the axis flips depending on direction of rotation (to left, to right)
			
			fm_quat rotWS = {};
			fm_quat_make_from_axis_angle(0.0f, 0.0f, 1.0f, character->animInfo.currentYaw, &rotWS);
			fm_quat_rot(&rotWS, &animMotionDelta.pos, &animMotionDelta.pos);
			
			// motion from logic
			const fm_vec4 logicMotionDelta = {character->animInfo.desiredMove.x, character->animInfo.desiredMove.y, 0.0f};
			fm_vec4 logicMotionDir = logicMotionDelta;
			
			float logicCurrentYaw = character->animInfo.currentYaw;
			float logicYawDelta = 0.0f;
			
			if(fm_vec4_mag2(&logicMotionDir) > 0.001f)
			{
				fm_vec4_normalize(&logicMotionDir);
				logicCurrentYaw = -fm_sign(logicMotionDir.y) * acosf(logicMotionDir.x);
				logicYawDelta = logicCurrentYaw - character->animInfo.currentYaw;
			}
			
			const float animToLogicMotionDirectionAlpha = character->animInfo.animToLogicMotionRotationAlpha;
			const float finalYawDelta = animYawDelta * (1.0f - animToLogicMotionDirectionAlpha) + logicYawDelta * animToLogicMotionDirectionAlpha;
			
			character->animInfo.rootMotionDeltaYaw = finalYawDelta;
			character->animInfo.currentYaw += finalYawDelta;
			
			const float animToLogicMotionSpeedAlpha = character->animInfo.animToLogicMotionTranslationAlpha;
			
			fm_vec4 finalMotionDelta = {};
			fm_vec4_lerp(&logicMotionDelta, &animMotionDelta.pos, animToLogicMotionSpeedAlpha, &finalMotionDelta);
			
			character->animInfo.rootMotionDelta.x = finalMotionDelta.x;
			character->animInfo.rootMotionDelta.y = finalMotionDelta.y;
			character->animInfo.rootMotionDelta.z = finalMotionDelta.z;
		}
	}
}

void fa_action_animate_func(const fa_action_ctx_t* ctx, void* userData)
{
	fa_action_animate_t* data = (fa_action_animate_t*)userData;
	
	const float animDuration = data->animation->duration;
	const float time = fmodf(ctx->localTime, animDuration);
	
	// loops for motion
	const int32_t loopsSinceBeginning = (int32_t)(ctx->localTime / animDuration);
	const int32_t loopsThisFrame = loopsSinceBeginning - data->loopsSoFar;
	data->loopsSoFar = loopsSinceBeginning;
	
	if(data->useLoco)
	{
		fa_cmd_anim_sample_with_locomotion(ctx->cmdRecorder, time, 0, data->resetLoco, loopsThisFrame, data->prevLocoPos, data->prevLocoRot);
	}
	else
	{
		fa_cmd_anim_sample(ctx->cmdRecorder, time, 0);
	}
	
	if(data->resetLoco)
	{
		data->resetLoco = false;
		data->loopsSoFar = 0;
	}
	
	data->progress = time / animDuration;
}

const fa_anim_clip_t** fa_action_animate_get_anims_func(const void* userData, uint32_t* numAnims)
{
	const fa_action_animate_t* data = (const fa_action_animate_t*)userData;
	*numAnims = 1;
	return (const fa_anim_clip_t**)&data->animation;	// todo: check it, is this return correct?
}

void fa_action_animate_begin_func(const fa_action_begin_end_ctx_t* ctx, void* userData)
{
	fa_action_animate_t* data = (fa_action_animate_t*)userData;
	
	// at the beginning, we want to reset locomotion data
	data->resetLoco = true;
	data->loopsSoFar = 0;
	data->progress = 0.0f;
}

void fa_action_animate_end_func(const fa_action_begin_end_ctx_t* ctx, void* userData)
{
	fa_action_animate_t* data = (fa_action_animate_t*)userData;
	data->reserved = false;
}

void fa_action_animate_cancel_func(void* userData)
{
	fa_action_animate_t* data = (fa_action_animate_t*)userData;
	data->reserved = false;
}

fa_layer_t* fa_character_layer_select(fa_character_t* character, const fa_action_args_t* args)
{
	if(args->layerName == 0)
	{
		switch(args->layer)
		{
			case FA_CHAR_LAYER_FULL_BODY:
				return &character->layerFullBody;
			case FA_CHAR_LAYER_PARTIAL:
				return &character->layerPartial;
			default:
				return NULL;
		}
	}
	
	if(args->layerName == SID("full-body"))
		return &character->layerFullBody;
	else if(args->layerName == SID("partial"))
		return &character->layerPartial;
	else if(args->layerName == SID("face"))
		return &character->layerFace;
	else if(args->layerName == SID("hands"))
		return &character->layerHands;
	
	return NULL;
}

void fa_character_schedule_action_simple(fa_character_t* character, fa_action_animate_t* action, const fa_action_args_t* args)
{
	fa_layer_t* layer = fa_character_layer_select(character, args);
	FUR_ASSERT(layer);
	
	fa_action_t* actionSlot = fa_action_queue_get_free_slot(&layer->actionQueue);
	actionSlot->userData = action;
	actionSlot->fnUpdate = fa_action_animate_func;
	actionSlot->fnGetAnims = fa_action_animate_get_anims_func;
	actionSlot->fnBegin = fa_action_animate_begin_func;
	actionSlot->fnEnd = fa_action_animate_end_func;
	actionSlot->fnCancel = fa_action_animate_cancel_func;
	actionSlot->globalStartTime = character->globalTime;
	actionSlot->isUsed = true;
	actionSlot->args = *args;
}

CANIM_API void fa_character_schedule_none_action(fa_character_t* character, const fa_action_args_t* args)
{
	fa_layer_t* layer = fa_character_layer_select(character, args);
	FUR_ASSERT(layer);
	
	fa_action_t* actionSlot = fa_action_queue_get_free_slot(&layer->actionQueue);
	actionSlot->userData = NULL;
	actionSlot->fnUpdate = NULL;
	actionSlot->fnGetAnims = NULL;
	actionSlot->globalStartTime = character->globalTime;
	actionSlot->isUsed = true;
	actionSlot->args = *args;
}

CANIM_API void fa_character_schedule_action(fa_character_t* character, fa_action_schedule_data_t* data, const fa_action_args_t* args)
{
	fa_layer_t* layer = fa_character_layer_select(character, args);
	FUR_ASSERT(layer);
	
	fa_action_t* actionSlot = fa_action_queue_get_free_slot(&layer->actionQueue);
	actionSlot->userData = data->userData;
	actionSlot->fnUpdate = data->fnUpdate;
	actionSlot->fnBegin = data->fnBegin;
	actionSlot->fnEnd = data->fnEnd;
	actionSlot->fnGetAnims = data->fnGetAnims;
	actionSlot->globalStartTime = character->globalTime;
	actionSlot->isUsed = true;
	actionSlot->args = *args;
}

// -----

void fa_action_animate_test_func(const fa_action_ctx_t* ctx, void* userData)
{
	fa_action_animate_test_t* data = (fa_action_animate_test_t*)userData;
	
	const float d_0 = data->anims[0]->duration;
	const float t_0 = fmodf(ctx->localTime, d_0);
	
	const float d_1 = data->anims[1]->duration;
	const float t_1 = fmodf(ctx->localTime, d_1);
	
#if 0
	fa_cmd_anim_sample(ctx->cmdRecorder, t_0, 0);
	fa_cmd_anim_sample(ctx->cmdRecorder, t_1, 1);
	fa_cmd_apply_mask(ctx->cmdRecorder, FA_MASK_UPPER_BODY);
	fa_cmd_blend2(ctx->cmdRecorder, 1.0f);
#elif 1
	const float alpha = fm_clamp(ctx->localTime - data->timeToNextAnim, 0.0f, 0.5f) * 2.0f;
	fa_cmd_anim_sample(ctx->cmdRecorder, t_0, 0);
	fa_cmd_anim_sample(ctx->cmdRecorder, t_1, 1);
	fa_cmd_apply_mask(ctx->cmdRecorder, FA_MASK_UPPER_BODY);
	fa_cmd_blend2(ctx->cmdRecorder, alpha);
	
	if(alpha >= 0.1f)
	{
		data->equipWeapon = true;
	}
#elif 0
	if(ctx->localTime < 2.0f)
	{
		fa_cmd_identity(ctx->cmdRecorder);
	}
	else
	{
		const float alpha = fm_clamp(ctx->localTime - 2.0f, 0.0f, 1.0f);
		fa_cmd_identity(ctx->cmdRecorder);
		fa_cmd_anim_sample(ctx->cmdRecorder, 0.0f, 0);
		fa_cmd_blend2(ctx->cmdRecorder, alpha);
	}
#else
	fa_cmd_anim_sample(ctx->cmdRecorder, t_0, 0);
	fa_cmd_anim_sample_additive(ctx->cmdRecorder, t_1, 1);
	fa_cmd_apply_additive(ctx->cmdRecorder, 1.0f);
#endif
}

const fa_anim_clip_t** fa_action_animate_test_get_anims_func(const void* userData, uint32_t* numAnims)
{
	const fa_action_animate_test_t* data = (const fa_action_animate_test_t*)userData;
	*numAnims = 2;
	return (const fa_anim_clip_t**)&data->anims;	// todo: check it, is this return correct?
}

void fa_character_schedule_action_test_simple(fa_character_t* character, fa_action_animate_test_t* action, const fa_action_args_t* args)
{
	fa_layer_t* layer = fa_character_layer_select(character, args);
	FUR_ASSERT(layer);
	
	fa_action_t* actionSlot = fa_action_queue_get_free_slot(&layer->actionQueue);
	actionSlot->userData = action;
	actionSlot->fnUpdate = fa_action_animate_test_func;
	actionSlot->fnGetAnims = fa_action_animate_test_get_anims_func;
	actionSlot->globalStartTime = character->globalTime;
	actionSlot->args = *args;
}

// -----

void fa_action_player_loco_begin_func(const fa_action_begin_end_ctx_t* ctx, void* userData)
{
	fa_action_player_loco_t* data = (fa_action_player_loco_t*)userData;
	
	data->resetLoco = true;
}

void fa_action_player_loco_end_func(const fa_action_begin_end_ctx_t* ctx, void* userData)
{
	
}

void fa_action_player_loco_update(const fa_action_ctx_t* ctx, void* userData)
{
	fa_action_player_loco_t* data = (fa_action_player_loco_t*)userData;
	
	// loops for motion
	const float animDuration = data->anims[FA_ACTION_PLAYER_LOCO_ANIM_RUN]->duration;
	const float animTime = fmodf(ctx->localTime, animDuration);
	const int32_t loopsSinceBeginning = (int32_t)(ctx->localTime / animDuration);
	const int32_t loopsThisFrame = loopsSinceBeginning - data->loopsSoFar;
	data->loopsSoFar = loopsSinceBeginning;
	
	fa_cmd_anim_sample_with_locomotion(ctx->cmdRecorder, animTime, FA_ACTION_PLAYER_LOCO_ANIM_RUN, data->resetLoco, loopsThisFrame, data->locoPos, data->locoRot);
	
	if(data->resetLoco)
	{
		data->resetLoco = false;
		data->loopsSoFar = 0;
	}
}

const fa_anim_clip_t** fa_action_player_loco_get_anims_func(const void* userData, uint32_t* numAnims)
{
	const fa_action_player_loco_t* data = (const fa_action_player_loco_t*)userData;
	*numAnims = FA_ACTION_PLAYER_LOCO_ANIM_COUNT;
	return (const fa_anim_clip_t**)&data->anims;	// todo: check it, is this return correct?
}

// -----

void fa_action_player_jump_update(const fa_action_ctx_t* ctx, void* userData)
{
	fa_action_player_jump_t* data = (fa_action_player_jump_t*)userData;
	
	// update player motion
	fa_character_anim_info_t* animInfo = ctx->animInfo;
	
	const float t = ctx->localTime;
	const bool doMove = fabs(animInfo->desiredMove.x) > 0.05f || fabs(animInfo->desiredMove.x) > 0.05f;
	
	if(data->jumpType == 0)
	{
		data->jumpType = doMove ? 2 : 1;
	}
	
	if(data->jumpType == 1) // jump in place
	{
		const float d = data->anims[0]->duration;
		const float t_anim = fm_clamp(t, 0.0f, d);
		
		data->progress = t / d;
		
		fa_cmd_anim_sample(ctx->cmdRecorder, t_anim, 0);
	}
	else // jump in run
	{
		const float d = data->anims[1]->duration;
		const float t_anim = fm_clamp(t, 0.0f, d);
		
		data->progress = t / d;
		
		fa_cmd_anim_sample(ctx->cmdRecorder, t_anim, 1);
	}
}

const fa_anim_clip_t** fa_action_player_jump_get_anims_func(const void* userData, uint32_t* numAnims)
{
	const fa_action_player_jump_t* data = (const fa_action_player_jump_t*)userData;
	*numAnims = 2;
	return (const fa_anim_clip_t**)&data->anims;	// todo: check it, is this return correct?
}

// -----


void fa_action_player_loco_start_begin_func(const fa_action_begin_end_ctx_t* ctx, void* userData)
{
	fa_action_player_loco_start_t* data = (fa_action_player_loco_start_t*)userData;
	data->resetLoco = true;
}

void fa_action_player_loco_start_end_func(const fa_action_begin_end_ctx_t* ctx, void* userData)
{
	
}

void fa_action_player_loco_start_update(const fa_action_ctx_t* ctx, void* userData)
{
	fa_action_player_loco_start_t* data = (fa_action_player_loco_start_t*)userData;
	const float t = ctx->localTime;
	const float d = data->anims[0]->duration;
	
	// animate start
	const float t_anim = fm_clamp(t, 0.0f, d);
	
	data->isFinished = t > (d - data->finishFromEnd);
	
	fa_cmd_anim_sample_with_locomotion(ctx->cmdRecorder, t_anim, 0, data->resetLoco, 0, data->prevLocoPos, data->prevLocoRot);
	
	if(data->resetLoco)
	{
		data->resetLoco = false;
	}
}

const fa_anim_clip_t** fa_action_player_loco_start_get_anims_func(const void* userData, uint32_t* numAnims)
{
	const fa_action_player_loco_start_t* data = (const fa_action_player_loco_start_t*)userData;
	*numAnims = 1;
	return (const fa_anim_clip_t**)&data->anims;	// todo: check it, is this return correct?
}
