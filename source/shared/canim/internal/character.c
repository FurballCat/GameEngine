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

void fcAnimActionReset(FcAnimAction* action)
{
	memset(action, 0, sizeof(FcAnimAction));
}

void fcAnimCharacterLegIK(FcAnimCharacter* character, const FcAnimIKSetup* ikSetup, FcPose* poseLS, FcPose* poseMS, const fm_vec4* targetArg, f32 weightIK)
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
	
	const f32 angleMin = ikSetup->minAngle;
	const f32 angleMax = ikSetup->maxAngle;
	fm_axis_t hingeAxis = ikSetup->hingeAxisMid;
	
	fm_vec4 endEffector = chainMS[3].pos;
	fm_vec4 target;
	fm_vec4_lerp(&endEffector, &targetFixed, weightIK, &target);
	
	static u32 num_iterations = 20;
	for(u32 it=0; it<num_iterations; ++it)
	{
		// loop bones in IK setup
		for(u32 i=2; i>=1; --i)
		{
			const u32 ip = i-1;
			
			endEffector = chainMS[3].pos;
			
			fm_vec4 e_i;
			fm_vec4_sub(&endEffector, &chainMS[i].pos, &e_i);
			fm_vec4 t_i;
			fm_vec4_sub(&target, &chainMS[i].pos, &t_i);
			
			fm_vec4_norm(&e_i);
			fm_vec4_norm(&t_i);
			const f32 angle = -acosf(fm_vec4_dot(&e_i, &t_i));
			const bool canRot = fabsf(angle) > 0.0001f;
			if(canRot)
			{
				fm_vec4 axis;
				fm_vec4_cross(&e_i, &t_i, &axis);
				if(fm_vec4_mag2(&axis) > 0.0f)
				{
					fm_vec4_norm(&axis);
					
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
						f32 rotAngle;
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
					for(u32 g=i; g<3; ++g)
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

f32 fcAnimActionGetLocalTime(const FcAnimAction* action, const FcAnimCharacter* character)
{
	f32 localTime = -1.0f;
	
	if(character->globalTime >= action->globalStartTime)
	{
		localTime = (f32)((character->globalTime - action->globalStartTime) / 1000000.0);
	}
	FUR_ASSERT(localTime != -1.0f);
	
	return localTime;
}

f32 fcAnimActionGetAlpha(FcAnimCharacter* character, const FcAnimAction* action)
{
	if(!action->isUsed)
		return 0.0f;
	
	f32 alpha = 1.0f;
	
	const FcAnimActionArgs* args = &action->args;
	const f32 localTime = fcAnimActionGetLocalTime(action, character);
	
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
	FcPoseStack* poseStack;
	
	void* scratchMemory;
	u32 scratchMemorySize;
	
	FcCommandBufferContextDebug* debug;
	
	f32 dt;
	
	f32 outWeightLegsIK;
	
	f32 rootMotionDeltaX;
	f32 rootMotionDeltaY;
	f32 rootMotionDeltaYaw;
	
	fm_vec4 lookAtMS;
	f32 lookAtWeight;
} fa_cross_layer_context_t;

void fcAnimCharacterLayerCachePose(FcAnimLayer* layer, fa_cross_layer_context_t* ctx, f32 alpha)
{
	FcPose outPose;
	FcPoseStackGet(ctx->poseStack, &outPose, 0);
	
	FcPoseCopy(&layer->poseCache.tempPose, &outPose);
	layer->transitionPoseCached = true;
	layer->poseCache.alpha = alpha;
	
	if(ctx->debug)
	{
		const f32 color[4] = FUR_COLOR_RED;
		fcDebugText(-450.0f, 1.0f, "caching_pose", color, 0.5f);
	}
}

void fcAnimCharacterActionAnimate(FcAnimCharacter* character, FcAnimLayer* layer, FcAnimAction* action, fa_cross_layer_context_t* ctx)
{
	FUR_ASSERT(action->fnUpdate != NULL);
	
	FcAnimCommandBuffer animCmdBuffer = { ctx->scratchMemory, ctx->scratchMemorySize };
	FcCommandBufferRecorder recorder = {0};
	fcAnimCmdBufferRecorderInit(&recorder, animCmdBuffer.data, animCmdBuffer.size);
	
	// record commands
	FUR_ASSERT(action->fnUpdate != NULL);
	FUR_ASSERT(action->fnGetAnims != NULL);
	
	const f32 localTime = fcAnimActionGetLocalTime(action, character);
	
	FcAnimActionCtx actionCtx = {0};
	actionCtx.dt = ctx->dt;
	actionCtx.cmdRecorder = &recorder;
	actionCtx.animInfo = &character->animInfo;
	actionCtx.localTime = localTime;
	actionCtx.debug = ctx->debug;
	
	// record commands
	{
		fcAnimCmdBegin(&recorder, ctx->poseStack->numPoses);
		action->fnUpdate(&actionCtx, action->userData);
		fcAnimCmdEnd(&recorder);
	}
	
	// evaluate commands
	FcAnimCommandCtx animCtx = {0};
	animCtx.animClips = action->fnGetAnims(action->userData, &animCtx.numAnimClips);
	animCtx.rig = character->rig;
	animCtx.poseStack = ctx->poseStack;
	animCtx.poseCache = &layer->poseCache;
	animCtx.debug = ctx->debug;
	animCtx.mask = fcRigGetMask(character->rig, layer->maskID);
	
	fcAnimCmdBufferEvaluate(&animCmdBuffer, &animCtx);
	
	ctx->rootMotionDeltaX = actionCtx.rootMotionDeltaX;
	ctx->rootMotionDeltaY = actionCtx.rootMotionDeltaY;
	ctx->rootMotionDeltaYaw = actionCtx.rootMotionDeltaYaw;
}

void fcAnimActionSafelyCancel(FcAnimAction* action)
{
	if(action->isUsed && action->fnCancel)
	{
		action->fnCancel(action->userData);
	}
}

FcAnimAction* fcAnimActionQueueGetCurrent(FcAnimActionQueue* queue)
{
	if(queue->actions[0].isUsed)
	{
		return &queue->actions[0];
	}
	
	return NULL;
}

FcAnimAction* fcAnimActionQueueGetNext(FcAnimActionQueue* queue)
{
	if(queue->actions[1].isUsed)
	{
		return &queue->actions[1];
	}
	
	return NULL;
}

FcAnimAction* fcAnimActionQueueGetFreeSlot(FcAnimActionQueue* queue)
{
	// find free slot
	for(u32 i=0; i<4; ++i)
	{
		if(queue->actions[i].isUsed == false)
		{
			return &queue->actions[i];
		}
	}
	
	// if not found, make some slot free
	fcAnimActionSafelyCancel(&queue->actions[2]); // cancel action [2]
	queue->actions[2] = queue->actions[3];	// move action [3] to [2]
	fcAnimActionReset(&queue->actions[3]);	// clear unused slot
	
	return &queue->actions[3];	// return free, unused slot
}

void fcAnimActionSafelyBegin(FcAnimActionBeginEndCtx* ctx, FcAnimAction* action)
{
	if(action->isUsed && action->fnBegin && !action->hasBegun)
	{
		action->fnBegin(ctx, action->userData);
		action->hasBegun = true;
	}
}

void fcAnimActionSafelyEnd(FcAnimActionBeginEndCtx* ctx, FcAnimAction* action)
{
	if(action->isUsed && action->fnEnd && action->hasBegun)
	{
		action->fnEnd(ctx, action->userData);
		action->hasBegun = false;
	}
}

void fcAnimCharacterInit(FcAnimCharacter* character, const FcRig* rig, FcAllocator* allocator)
{
	character->rig = rig;
	
	const u16 numBones = rig->numBones;
	
	character->poseMS = FUR_ALLOC_ARRAY_AND_ZERO(fm_xform, numBones, 16, FC_MEMORY_SCOPE_ANIMATION, allocator);
	character->layerFullBody.poseCache.tempPose.xforms = FUR_ALLOC_ARRAY_AND_ZERO(fm_xform, numBones, 16, FC_MEMORY_SCOPE_ANIMATION, allocator);
	character->layerFullBody.poseCache.tempPose.weightsXforms = FUR_ALLOC_ARRAY_AND_ZERO(u8, numBones, 16, FC_MEMORY_SCOPE_ANIMATION, allocator);
	character->layerFullBody.poseCache.tempPose.numXforms = numBones;
	
	character->layerPartial.poseCache.tempPose.xforms = FUR_ALLOC_ARRAY_AND_ZERO(fm_xform, numBones, 16, FC_MEMORY_SCOPE_ANIMATION, allocator);
	character->layerPartial.poseCache.tempPose.weightsXforms = FUR_ALLOC_ARRAY_AND_ZERO(u8, numBones, 16, FC_MEMORY_SCOPE_ANIMATION, allocator);
	character->layerPartial.poseCache.tempPose.numXforms = numBones;
	character->layerPartial.maskID = FA_MASK_UPPER_BODY;
	
	character->layerFace.poseCache.tempPose.xforms = FUR_ALLOC_ARRAY_AND_ZERO(fm_xform, numBones, 16, FC_MEMORY_SCOPE_ANIMATION, allocator);
	character->layerFace.poseCache.tempPose.weightsXforms = FUR_ALLOC_ARRAY_AND_ZERO(u8, numBones, 16, FC_MEMORY_SCOPE_ANIMATION, allocator);
	character->layerFace.poseCache.tempPose.numXforms = numBones;
	character->layerFace.maskID = FA_MASK_FACE;
	
	character->layerHands.poseCache.tempPose.xforms = FUR_ALLOC_ARRAY_AND_ZERO(fm_xform, numBones, 16, FC_MEMORY_SCOPE_ANIMATION, allocator);
	character->layerHands.poseCache.tempPose.weightsXforms = FUR_ALLOC_ARRAY_AND_ZERO(u8, numBones, 16, FC_MEMORY_SCOPE_ANIMATION, allocator);
	character->layerHands.poseCache.tempPose.numXforms = numBones;
	character->layerHands.maskID = FA_MASK_HANDS;
}

void fcAnimCharacterRelease(FcAnimCharacter* character, FcAllocator* allocator)
{
	FUR_FREE(character->poseMS, allocator);
	
	FUR_FREE(character->layerFullBody.poseCache.tempPose.xforms, allocator);
	FUR_FREE(character->layerFullBody.poseCache.tempPose.weightsXforms, allocator);
	
	FUR_FREE(character->layerPartial.poseCache.tempPose.xforms, allocator);
	FUR_FREE(character->layerPartial.poseCache.tempPose.weightsXforms, allocator);
	
	FUR_FREE(character->layerFace.poseCache.tempPose.xforms, allocator);
	FUR_FREE(character->layerFace.poseCache.tempPose.weightsXforms, allocator);
	
	FUR_FREE(character->layerHands.poseCache.tempPose.xforms, allocator);
	FUR_FREE(character->layerHands.poseCache.tempPose.weightsXforms, allocator);
}

void fcAnimActionQueueResolvePreAnimate(FcAnimCharacter* character, FcAnimActionQueue* queue)
{
	FcAnimActionBeginEndCtx ctx = {0};
	ctx.animInfo = &character->animInfo;
	
	// check if any of the pending actions should be instantly activated
	if(fcAnimActionGetAlpha(character, &queue->actions[3]) >= 1.0)
	{
		// end old actions
		fcAnimActionSafelyEnd(&ctx, &queue->actions[0]);
		fcAnimActionSafelyEnd(&ctx, &queue->actions[1]);
		
		// rare case when we need to cancel action [2], as it's eaten up by action [3]
		fcAnimActionSafelyCancel(&queue->actions[2]);
		
		// move pending actions
		queue->actions[0] = queue->actions[3];
		
		// clear unused slots
		fcAnimActionReset(&queue->actions[1]);
		fcAnimActionReset(&queue->actions[2]);
		fcAnimActionReset(&queue->actions[3]);
	}
	else if(fcAnimActionGetAlpha(character, &queue->actions[2]) >= 1.0)
	{
		// end old actions
		fcAnimActionSafelyEnd(&ctx, &queue->actions[0]);
		fcAnimActionSafelyEnd(&ctx, &queue->actions[1]);
		
		// move pending actions
		queue->actions[0] = queue->actions[2];
		queue->actions[1] = queue->actions[3];
		
		// clear unused slots
		fcAnimActionReset(&queue->actions[2]);
		fcAnimActionReset(&queue->actions[3]);
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

void fcAnimActionQueueResolvePostAnimate(FcAnimCharacter* character, FcAnimActionQueue* queue)
{
	FcAnimActionBeginEndCtx ctx = {0};
	ctx.animInfo = &character->animInfo;
	
	if(queue->cachePoseAfterNextAction) // case of cache for 2 actions
	{
		// end old actions
		fcAnimActionSafelyEnd(&ctx, &queue->actions[0]);
		fcAnimActionSafelyEnd(&ctx, &queue->actions[1]);
		
		// move pending actions
		queue->actions[0] = queue->actions[2];
		queue->actions[1] = queue->actions[3];
		
		// clear unused slots
		fcAnimActionReset(&queue->actions[2]);
		fcAnimActionReset(&queue->actions[3]);
		
		queue->cachePoseAfterNextAction = false;
	}
	else if(queue->cachePoseAfterCurrAction)	// case of cache for 1 action
	{
		// end old actions
		fcAnimActionSafelyEnd(&ctx, &queue->actions[0]);
		
		// move pending actions
		queue->actions[0] = queue->actions[1];
		queue->actions[1] = queue->actions[2];
		
		// clear unused slots
		fcAnimActionReset(&queue->actions[2]);
		fcAnimActionReset(&queue->actions[3]);
		
		queue->cachePoseAfterCurrAction = false;
	}
	else if(fcAnimActionGetAlpha(character, &queue->actions[1]) >= 1.0)	// normal case, just next action
	{
		// end old actions
		fcAnimActionSafelyEnd(&ctx, &queue->actions[0]);
		
		// move actions
		queue->actions[0] = queue->actions[1];
		queue->actions[1] = queue->actions[2];
		queue->actions[2] = queue->actions[3];
		
		// clear unused slots
		fcAnimActionReset(&queue->actions[1]);
		fcAnimActionReset(&queue->actions[2]);
		fcAnimActionReset(&queue->actions[3]);
	}
}

void fcAnimCharacterUpdateLayer(FcAnimCharacter* character, fa_cross_layer_context_t* ctx, FcAnimLayer* layer)
{
	// resolve scheduled actions
	fcAnimActionQueueResolvePreAnimate(character, &layer->actionQueue);
	
	FcAnimAction* currAction = &layer->actionQueue.actions[0];
	FcAnimAction* nextAction = &layer->actionQueue.actions[1];
	
	const f32 nextAlpha = fcAnimActionGetAlpha(character, nextAction);
	const f32 currAlpha = fcAnimActionGetAlpha(character, currAction) * (1.0f - nextAlpha);
	const f32 cachedPoseAlpha = layer->poseCache.alpha * (1.0f - currAlpha);
	
	// copy cached pose if required
	if(layer->transitionPoseCached && cachedPoseAlpha > 0.0f)
	{
		FcPose outPose;
		FcPoseStackGet(ctx->poseStack, &outPose, 0);
		FcPoseBlendLinear(&outPose, &layer->poseCache.tempPose, &outPose, cachedPoseAlpha);		// blend to include pose weights/mask
	}
	else
	{
		layer->transitionPoseCached = false;
	}
	
	// todo: refactor that
	FcAnimActionBeginEndCtx beginEndCtx = {0};
	beginEndCtx.animInfo = &character->animInfo;
	fcAnimActionSafelyBegin(&beginEndCtx, currAction);
	fcAnimActionSafelyBegin(&beginEndCtx, nextAction);
	
	// animate current action
	if(currAction->fnUpdate != NULL)
	{
		FcAnimAction* action = currAction;
		fcAnimCharacterActionAnimate(character, layer, action, ctx);
		
		if(layer->actionQueue.cachePoseAfterCurrAction)
		{
			fcAnimCharacterLayerCachePose(layer, ctx, currAlpha);
		}
		
		// blend in result
		{
			FcPose actionPose;
			FcPose outPose;
			FcPoseStackGet(ctx->poseStack, &actionPose, 0);
			FcPoseStackGet(ctx->poseStack, &outPose, 1);
			FcPoseBlendLinear(&outPose, &actionPose, &outPose, currAlpha);
			FcPoseStackPop(ctx->poseStack, 1);
		}
	}
	
	// animate next action
	if(nextAction->fnUpdate != NULL)
	{
		FcAnimAction* action = nextAction;
		fcAnimCharacterActionAnimate(character, layer, action, ctx);
		
		if(layer->actionQueue.cachePoseAfterNextAction)
		{
			fcAnimCharacterLayerCachePose(layer, ctx, nextAlpha);
		}
		
		// blend in result
		{
			FcPose actionPose;
			FcPose outPose;
			FcPoseStackGet(ctx->poseStack, &actionPose, 0);
			FcPoseStackGet(ctx->poseStack, &outPose, 1);
			FcPoseBlendLinear(&outPose, &actionPose, &outPose, nextAlpha);
			FcPoseStackPop(ctx->poseStack, 1);
		}
	}
	
	// only body layer can affect legs IK, it's secured outside this function
	{
		const f32 currIK = currAction->args.ikMode == FA_IK_MODE_LEGS ? currAlpha : 0.0f;
		const f32 nextIK = nextAction->args.ikMode == FA_IK_MODE_LEGS ? 1.0f : 0.0f;
		
		const f32 weightIK = currIK * (1.0f - nextAlpha) + nextIK * nextAlpha;
		ctx->outWeightLegsIK = weightIK;
	}
	
	fcAnimActionQueueResolvePostAnimate(character, &layer->actionQueue);
}

void fcAnimCharacterIK(FcAnimCharacter* character, fa_cross_layer_context_t* layerCtx)
{
	// inverse kinematics
	{
		const f32 weightIK = layerCtx->outWeightLegsIK;
		
		if(weightIK > 0.0f)
		{
			FcPoseStackPush(layerCtx->poseStack, 1);
			
			FcPose poseMS;
			FcPoseStackGet(layerCtx->poseStack, &poseMS, 0);
			FcPose poseLS;
			FcPoseStackGet(layerCtx->poseStack, &poseLS, 1);
			
			FcPoseLocalToModel(&poseMS, &poseLS, character->rig->parents);
			
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
				
				const f32 leftDistance = fm_vec4_mag(&leftTargetDir);
				const f32 rightDistance = fm_vec4_mag(&rightTargetDir);
				fm_vec4 leftLegVec = poseLS.xforms[character->rig->ikLeftLeg.idxMid].pos;
				fm_vec4_add(&leftLegVec, &poseLS.xforms[character->rig->ikLeftLeg.idxEnd].pos, &leftLegVec);
				fm_vec4 rightLegVec = poseLS.xforms[character->rig->ikRightLeg.idxMid].pos;
				fm_vec4_add(&rightLegVec, &poseLS.xforms[character->rig->ikRightLeg.idxEnd].pos, &rightLegVec);
				
				const f32 footCorrectionDistance = 0.15f;
				
				const f32 leftLegLength = fm_vec4_mag(&leftLegVec) + footCorrectionDistance;
				const f32 rightLegLength = fm_vec4_mag(&rightLegVec) + footCorrectionDistance;
				
				f32 pelvisCorrectionHeight = 0.0f;
				if(leftLegLength < leftDistance)
					pelvisCorrectionHeight = leftDistance - leftLegLength;
				if(rightLegLength < (rightDistance - pelvisCorrectionHeight))
					pelvisCorrectionHeight = rightDistance - rightLegLength;
				
				poseLS.xforms[character->rig->ikLeftLeg.idxBeginParent].pos.y -= pelvisCorrectionHeight * weightIK;
				FcPoseLocalToModel(&poseMS, &poseLS, character->rig->parents);
			}
			
			fcAnimCharacterLegIK(character, &character->rig->ikLeftLeg, &poseLS, &poseMS, &leftTarget, weightIK);
			fcAnimCharacterLegIK(character, &character->rig->ikRightLeg, &poseLS, &poseMS, &rightTarget, weightIK);
			
			FcPoseStackPop(layerCtx->poseStack, 1);
		}
	}
}

void fcAnimCharacterLookAt(FcAnimCharacter* character, FcPose* poseLS, FcPose* poseMS,
						  f32 weight, fm_vec4* lookAtPointMS)
{
	// no need to apply look-at if the weight is 0.0
	if(weight <= 0.0f)
		return;
	
	// get look at setup and look at point in model space
	const FcAnimLookAtSetup* setup = &character->rig->headLookAt;
	fm_vec4 lookAtMS = *lookAtPointMS;
	
	// transform look at from model space to head space
	fm_xform locator = poseMS->xforms[setup->idxHead];
	fm_vec4 lookAtLocatorSpace = {0};
	fm_xform_apply_inv(&locator, &lookAtMS, &lookAtLocatorSpace);
	
	// assume +X axis is forward direction (depends on the rig)
	fm_vec4 forward = {1.0f, 0.0f, 0.0f, 0.0f};
	
	// constrain look-at to some sane ranges
	{
		// remainder: head space here means (+z right, -z left, +y up, -y down, +x forward, -x backward)
		fm_vec4 dir = lookAtLocatorSpace;
		
		f32 yaw = -acosf(dir.z / sqrtf(dir.x * dir.x + dir.z * dir.z)) + FM_PI_2;
		f32 pitch = -acosf(dir.y / sqrtf(dir.x * dir.x + dir.y * dir.y)) + FM_PI_2;
		
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
		fm_quat yawRot = {0};
		fm_quat_make_from_axis_angle(0.0f, 1.0f, 0.0f, character->lookAtHeadYaw, &yawRot);
		
		fm_quat pitchRot = {0};
		fm_quat_make_from_axis_angle(0.0f, 0.0f, -1.0f, character->lookAtHeadPitch, &pitchRot);
		
		fm_quat_rot(&yawRot, &forward, &dir);
		fm_quat_rot(&pitchRot, &dir, &dir);
		const f32 mag = fm_vec4_mag(&lookAtLocatorSpace);
		fm_vec4_mulf(&dir, mag, &lookAtLocatorSpace);
	}
	
	// calculate final look-at rotation correction with weight
	fm_quat headCorrection = {0};
	fm_vec4_rot_between(&forward, &lookAtLocatorSpace, &headCorrection);
	
	// apply weight to look-at
	fm_quat identity = {0};
	fm_quat_identity(&identity);
	fm_quat_slerp(&identity, &headCorrection, weight, &headCorrection);
	
	// apply look-at rotation correction to local space
	fm_quat_mul(&headCorrection, &poseLS->xforms[setup->idxHead].rot, &poseLS->xforms[setup->idxHead].rot);
}

void fcAnimCharacterUpdate(FcAnimCharacter* character, const FcAnimCharacterUpdateCtx* ctx)
{
	// pre-process inputs in anim info
	fm_vec4 lookAtMS = {0};
	
	{
		FcAnimInfo* info = &character->animInfo;
		
		// get world locator of the character
		const fm_vec4 posWS = {info->worldPos.x, info->worldPos.y, info->worldPos.z, 1.0f};
		fm_quat rotWS = {0};
		fm_quat_make_from_axis_angle(0.0f, 0.0f, 1.0f, info->currentYaw, &rotWS);
		
		// move look-at WS to MS
		const fm_vec4 lookAtWS = {info->lookAtPoint.x, info->lookAtPoint.y, info->lookAtPoint.z, 1.0f};
		fm_vec4_sub(&lookAtWS, &posWS, &lookAtMS);
		
		fm_quat invRotWS = rotWS;
		fm_quat_conj(&invRotWS);
		fm_quat_rot(&invRotWS, &lookAtMS, &lookAtMS);
	}
	
	// allocate pose stack and command buffer memory
	const u32 poseStackSize = 128 * 1024;
	void* animPoseStackMemory = fcMemArenaAlloc(ctx->arenaAlloc, poseStackSize, 8);
	
	const u32 animCmdBufferSize = 32 * 1024;
	void* animCmdBufferMemory = fcMemArenaAlloc(ctx->arenaAlloc, animCmdBufferSize, 0);
	
	// init pose stack - pose stack is shared across multiple command buffers
	FcPoseStack poseStack = {0};
	
	{
		FcPoseStackDesc desc = {0};
		
		desc.numBonesPerPose = character->rig->numBones;
		desc.numTracksPerPose = 0;
		desc.numMaxPoses = 4;
		
		FcPoseStackInit(&poseStack, &desc, animPoseStackMemory, poseStackSize);
	}
	
	FcCommandBufferContextDebug debug = {0};
	
	// update layers
	fa_cross_layer_context_t layerCtx = {0};
	layerCtx.dt = ctx->dt;
	layerCtx.poseStack = &poseStack;
	layerCtx.scratchMemory = animCmdBufferMemory;
	layerCtx.scratchMemorySize = animCmdBufferSize;
	layerCtx.debug = ctx->showDebug ? &debug : NULL;
	
	FcPose poseLS;
	
	// reset pose to ref pose
	FcPoseStackPush(&poseStack, 1);
	FcPoseStackGet(&poseStack, &poseLS, 0);
	{
		FUR_ASSERT(poseLS.numXforms == character->rig->numBones);
		
		for(u32 i=0; i<poseLS.numXforms; ++i)
		{
			poseLS.xforms[i] = character->rig->refPose[i];
			poseLS.weightsXforms[i] = 255;
		}
		
		for(u32 i=0; i<poseLS.numTracks; ++i)
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
		fcAnimCharacterUpdateLayer(character, &layerCtx, &character->layerFullBody);
	}
	
	// store weight IK
	const f32 weightLegsIK = layerCtx.outWeightLegsIK;
	
	// partial layer, can be applied anywhere, but does not interrupt full body
	FUR_PROFILE("partial-layer")
	{
		fcAnimCharacterUpdateLayer(character, &layerCtx, &character->layerPartial);
	}
	
	// hands
	FUR_PROFILE("hands-layer")
	{
		fcAnimCharacterUpdateLayer(character, &layerCtx, &character->layerHands);
	}
	
	// face
	FUR_PROFILE("face-layer")
	{
		fcAnimCharacterUpdateLayer(character, &layerCtx, &character->layerFace);
	}
	
	// inverse kinematics
	FUR_PROFILE("ik")
	{
		layerCtx.outWeightLegsIK = weightLegsIK;
		fcAnimCharacterIK(character, &layerCtx);
	}
	
	// look-at
	FUR_PROFILE("look-at")
	{
		static f32 time = 0.0f;
		time += ctx->dt;
		
		f32 lookAtWeightTarget = 0.0f;
		
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
		FcPoseStackPush(&poseStack, 1);
		FcPose poseMS;
		FcPoseStackGet(&poseStack, &poseMS, 0);
		FcPose poseLS;
		FcPoseStackGet(&poseStack, &poseLS, 1);
		FcPoseLocalToModel(&poseMS, &poseLS, character->rig->parents);
		
		fcAnimCharacterLookAt(character, &poseLS, &poseMS, layerCtx.lookAtWeight, &layerCtx.lookAtMS);
		
		// pop temporary MS pose
		FcPoseStackPop(&poseStack, 1);
	}
	
	// ragdoll

	// convert to model space
	FUR_PROFILE("ls-to-ms")
	{
		const int16_t* parentIndices = character->rig->parents;
		FcPose poseMS = {0};
		poseMS.xforms = character->poseMS;
		poseMS.numXforms = character->rig->numBones;
		FcPoseLocalToModel(&poseMS, &poseLS, parentIndices);
	}
	
	// apply root motion
	{
		FcPose poseMS = {0};
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
			fm_vec4 axis = {0};
			f32 animYawDelta = 0.0f;
			fm_quat_to_axis_angle(&animMotionDelta.rot, &axis, &animYawDelta);
			
			animYawDelta *= -fm_sign(axis.y);	// the axis flips depending on direction of rotation (to left, to right)
			
			fm_quat rotWS = {0};
			fm_quat_make_from_axis_angle(0.0f, 0.0f, 1.0f, character->animInfo.currentYaw, &rotWS);
			fm_quat_rot(&rotWS, &animMotionDelta.pos, &animMotionDelta.pos);
			
			// motion from logic
			const fm_vec4 logicMotionDelta = {character->animInfo.desiredMove.x, character->animInfo.desiredMove.y, 0.0f};
			fm_vec4 logicMotionDir = logicMotionDelta;
			
			f32 logicCurrentYaw = character->animInfo.currentYaw;
			f32 logicYawDelta = 0.0f;
			
			if(fm_vec4_mag2(&logicMotionDir) > 0.001f)
			{
				fm_vec4_norm(&logicMotionDir);
				logicCurrentYaw = -fm_sign(logicMotionDir.y) * acosf(logicMotionDir.x);
				logicYawDelta = logicCurrentYaw - character->animInfo.currentYaw;
			}
			
			const f32 animToLogicMotionDirectionAlpha = character->animInfo.animToLogicMotionRotationAlpha;
			const f32 finalYawDelta = animYawDelta * (1.0f - animToLogicMotionDirectionAlpha) + logicYawDelta * animToLogicMotionDirectionAlpha;
			
			character->animInfo.rootMotionDeltaYaw = finalYawDelta;
			character->animInfo.currentYaw += finalYawDelta;
			
			const f32 animToLogicMotionSpeedAlpha = character->animInfo.animToLogicMotionTranslationAlpha;
			
			fm_vec4 finalMotionDelta = {0};
			fm_vec4_lerp(&animMotionDelta.pos, &logicMotionDelta, animToLogicMotionSpeedAlpha, &finalMotionDelta);
			
			character->animInfo.rootMotionDelta.x = finalMotionDelta.x;
			character->animInfo.rootMotionDelta.y = finalMotionDelta.y;
			character->animInfo.rootMotionDelta.z = finalMotionDelta.z;
		}
	}
}

void fcAnimActionAnimateFnUpdate(const FcAnimActionCtx* ctx, void* userData)
{
	FcAnimActionAnimate* data = (FcAnimActionAnimate*)userData;
	
	const f32 animDuration = data->animation->duration;
	const f32 time = fmodf(ctx->localTime, animDuration);
	
	// loops for motion
	const i32 loopsSinceBeginning = (i32)(ctx->localTime / animDuration);
	const i32 loopsThisFrame = loopsSinceBeginning - data->loopsSoFar;
	data->loopsSoFar = loopsSinceBeginning;
	
	if(data->useLoco)
	{
		fcAnimCmdSampleWithLocomotion(ctx->cmdRecorder, time, 0, data->resetLoco, loopsThisFrame, data->prevLocoPos, data->prevLocoRot);
	}
	else
	{
		fcAnimCmdSample(ctx->cmdRecorder, time, 0);
	}
	
	if(data->resetLoco)
	{
		data->resetLoco = false;
		data->loopsSoFar = 0;
	}
	
	data->progress = time / animDuration;
}

const FcAnimClip** fcAnimActionAnimateFnGetAnims(const void* userData, u32* numAnims)
{
	const FcAnimActionAnimate* data = (const FcAnimActionAnimate*)userData;
	*numAnims = 1;
	return (const FcAnimClip**)&data->animation;	// todo: check it, is this return correct?
}

void fcAnimActionAnimateFnBegin(const FcAnimActionBeginEndCtx* ctx, void* userData)
{
	FcAnimActionAnimate* data = (FcAnimActionAnimate*)userData;
	
	// at the beginning, we want to reset locomotion data
	data->resetLoco = true;
	data->loopsSoFar = 0;
	data->progress = 0.0f;
}

void fcAnimActionAnimateFnEnd(const FcAnimActionBeginEndCtx* ctx, void* userData)
{
	FcAnimActionAnimate* data = (FcAnimActionAnimate*)userData;
	data->reserved = false;
}

void fcAnimActionAnimateFnCancel(void* userData)
{
	FcAnimActionAnimate* data = (FcAnimActionAnimate*)userData;
	data->reserved = false;
}

FcAnimLayer* fcAnimCharacterLayerSelect(FcAnimCharacter* character, const FcAnimActionArgs* args)
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

void fcAnimCharacterScheduleActionAnimate(FcAnimCharacter* character, FcAnimActionAnimate* action, const FcAnimActionArgs* args)
{
	FcAnimLayer* layer = fcAnimCharacterLayerSelect(character, args);
	FUR_ASSERT(layer);
	
	FcAnimAction* actionSlot = fcAnimActionQueueGetFreeSlot(&layer->actionQueue);
	actionSlot->userData = action;
	actionSlot->fnUpdate = fcAnimActionAnimateFnUpdate;
	actionSlot->fnGetAnims = fcAnimActionAnimateFnGetAnims;
	actionSlot->fnBegin = fcAnimActionAnimateFnBegin;
	actionSlot->fnEnd = fcAnimActionAnimateFnEnd;
	actionSlot->fnCancel = fcAnimActionAnimateFnCancel;
	actionSlot->globalStartTime = character->globalTime;
	actionSlot->isUsed = true;
	actionSlot->args = *args;
}

CANIM_API void fcAnimCharacterScheduleActionNone(FcAnimCharacter* character, const FcAnimActionArgs* args)
{
	FcAnimLayer* layer = fcAnimCharacterLayerSelect(character, args);
	FUR_ASSERT(layer);
	
	FcAnimAction* actionSlot = fcAnimActionQueueGetFreeSlot(&layer->actionQueue);
	actionSlot->userData = NULL;
	actionSlot->fnUpdate = NULL;
	actionSlot->fnGetAnims = NULL;
	actionSlot->globalStartTime = character->globalTime;
	actionSlot->isUsed = true;
	actionSlot->args = *args;
}

CANIM_API void fcAnimCharacterScheduleAction(FcAnimCharacter* character, FcAnimActionScheduleData* data, const FcAnimActionArgs* args)
{
	FcAnimLayer* layer = fcAnimCharacterLayerSelect(character, args);
	FUR_ASSERT(layer);
	
	FcAnimAction* actionSlot = fcAnimActionQueueGetFreeSlot(&layer->actionQueue);
	actionSlot->userData = data->userData;
	actionSlot->fnUpdate = data->fnUpdate;
	actionSlot->fnBegin = data->fnBegin;
	actionSlot->fnEnd = data->fnEnd;
	actionSlot->fnGetAnims = data->fnGetAnims;
	actionSlot->globalStartTime = character->globalTime;
	actionSlot->isUsed = true;
	actionSlot->args = *args;
}
