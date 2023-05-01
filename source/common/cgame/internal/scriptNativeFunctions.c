/* Copyright (c) Furball Cat */

#include "cgame/script.h"
#include "ccore/public.h"
#include "cmath/math3dimpl.h"
#include "cgame/world.h"
#include "cgame/camera.h"
#include "canim/public.h"

typedef enum FcScriptAnimateArg
{
	FS_ANIMATE_ARG_FADE_IN_CURVE = 0,
	FS_ANIMATE_ARG_FADE_IN_SEC,
	FS_ANIMATE_ARG_FADE_OUT_CURVE,
	FS_ANIMATE_ARG_FADE_OUT_SEC,
	FS_ANIMATE_ARG_IK_MODE,
	FS_ANIMATE_ARG_LAYER,
	FS_ANIMATE_ARG_LAYER_NAME,
} FcScriptAnimateArg;

FcVariant fcScriptNative_Animate(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs >= 2);
	const FcStringId objectName = args[0].asStringHash;
	const FcStringId animName = args[1].asStringHash;

	// find game object
	FcGameObject* gameObj = fcScriptLookUpGameObject(ctx, objectName);
	FUR_ASSERT(gameObj);

	if (!gameObj->fn->animate)
	{
		FcVariant result = {0};
		return result;
	}

	const FcAnimClip* animClip = fcResourceRegisterFindAnimClip(&ctx->world->resources, animName);
	FUR_ASSERT(animClip);

	FcAnimActionArgs animArgs = {0};

	// get variadic arguments - iterate every 2 arguments, as we need (animate-arg ENUM) VALUE, this is two variadic arguments
	for (u32 i = 2; i + 1 < numArgs; i += 2)
	{
		const FcScriptAnimateArg arg_enum = (FcScriptAnimateArg)args[i].asInt32;
		switch (arg_enum)
		{
		case FS_ANIMATE_ARG_FADE_IN_SEC:
			animArgs.fadeInSec = args[i + 1].asFloat;
			break;
		case FS_ANIMATE_ARG_FADE_IN_CURVE:
			animArgs.fadeInCurve = (FcCurveType)args[i + 1].asInt32;
			break;
		case FS_ANIMATE_ARG_FADE_OUT_SEC:
			animArgs.fadeOutSec = args[i + 1].asFloat;
			break;
		case FS_ANIMATE_ARG_FADE_OUT_CURVE:
			animArgs.fadeOutCurve = (FcCurveType)args[i + 1].asInt32;
			break;
		case FS_ANIMATE_ARG_IK_MODE:
			animArgs.ikMode = (FcAnimIKMode)args[i + 1].asInt32;
			break;
		case FS_ANIMATE_ARG_LAYER:
			animArgs.layer = (FcAnimLayerType)args[i + 1].asInt32;
			break;
		case FS_ANIMATE_ARG_LAYER_NAME:
			animArgs.layerName = args[i + 1].asStringHash;
			break;
		default:
			break;
		}
	}

	bool useLocomotion = false;

	if (animArgs.layerName == SID("full-body") || animArgs.layerName == 0)
	{
		useLocomotion = true;
	}

	if (gameObj->fn->animate)
	{
		FcGameObjectAnimateCtx animCtx = { 0 };
		animCtx.animArgs = &animArgs;
		animCtx.animClip = animClip;
		animCtx.forceLoop = true;
		animCtx.useLocomotion = true;

		gameObj->fn->animate(gameObj, &animCtx);
	}

	ctx->waitSeconds = FM_MAX(ctx->waitSeconds - animArgs.fadeOutSec, 0.0f);

	FcVariant result = {0};
	return result;
};

FcVariant fcScriptNative_WaitAnimate(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs >= 2);
	const FcStringId animName = args[1].asStringHash;

	const FcAnimClip* animClip = fcResourceRegisterFindAnimClip(&ctx->world->resources, animName);
	FUR_ASSERT(animClip);

	ctx->waitSeconds = animClip->duration;

	return fcScriptNative_Animate(ctx, numArgs, args);
}

FcVariant fcScriptNative_EquipItem(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs >= 2);
	const FcStringId objectName = args[0].asStringHash;
	//const FcStringId itemName = args[1].asStringHash;

	// find game object
	FcGameObject* gameObj = fcScriptLookUpGameObject(ctx, objectName);
	FUR_ASSERT(gameObj);

	FcVariant value;
	value.asBool = true;
	gameObj->fn->setVar(gameObj, SID("equip-item"), value);

	FcVariant result = {0};
	return result;
}

FcVariant fcScriptNative_WaitSeconds(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs == 1);
	const f32 timeInSeconds = args[0].asFloat;

	ctx->waitSeconds = timeInSeconds;

	FcVariant result = {0};
	return result;
}

FcVariant fcScriptNative_GetVariable(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs == 2);
	const FcStringId objectName = args[0].asStringHash;
	const FcStringId varName = args[1].asStringHash;

	// find game object
	FcGameObject* gameObj = fcScriptLookUpGameObject(ctx, objectName);
	FUR_ASSERT(gameObj);

	FcVariant result = {0};

	if (gameObj->fn->getVar)
	{
		return gameObj->fn->getVar(gameObj, varName);
	}

	return result;
}

FcVariant fcScriptNative_SetVariable(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs >= 3);
	const FcStringId objectName = args[0].asStringHash;
	const FcStringId varName = args[1].asStringHash;
	const FcVariant varValue = args[2];

	// find game object
	FcGameObject* gameObj = fcScriptLookUpGameObject(ctx, objectName);
	FUR_ASSERT(gameObj);

	if (gameObj->fn->setVar)
	{
		gameObj->fn->setVar(gameObj, varName, varValue);
	}

	FcVariant result = {0};
	return result;
}

FcVariant fcScriptNative_Go(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs == 1);
	const FcStringId goToState = args[0].asStringHash;

	ctx->nextState = goToState;

	FcVariant result = {0};
	return result;
}

FcVariant fcScriptNative_GoWhen(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs == 2);
	const FcStringId goToState = args[0].asStringHash;
	const bool condition = args[1].asBool;

	if (condition)
	{
		ctx->nextState = goToState;
	}

	return args[1];
}

FcVariant fcScriptNative_CmpGt(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs == 2);
	const i32 a = args[0].asInt32;
	const i32 b = args[1].asInt32;

	FcVariant result;
	result.asBool = a > b;
	return result;
}

FcVariant fcScriptNative_CmpEq(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs == 2);
	const i32 a = args[0].asInt32;
	const i32 b = args[1].asInt32;

	FcVariant result;
	result.asBool = (a == b);
	return result;
}

FcVariant fcScriptNative_CameraEnable(FcScriptCtx* ctx, u32 numArgs, const FcVariant* args)
{
	FUR_ASSERT(numArgs == 3);
	const FcStringId objectName = args[0].asStringHash;
	const FcStringId cameraType = args[1].asStringHash;
	const f32 fadeInSec = args[2].asFloat;

	if (cameraType == SID("follow"))
	{
		FcCameraParamsFollow params = {0};
		params.height = 1.2f;
		params.zoom = 1.5f;
		params.poleLength = 1.5f;
		params.fov = 70.0f;
		fcCameraEnableFollow(ctx->world->systems.camera, &params, fadeInSec);
	}
	else if (cameraType == SID("follow-vista"))
	{
		FcCameraParamsFollow params = {0};
		params.height = 1.5f;
		params.zoom = 1.5f;
		params.poleLength = 1.0f;
		params.fov = 60.0f;
		fcCameraEnableFollow(ctx->world->systems.camera, &params, fadeInSec);
	}

	FcVariant result = {0};
	return result;
}

