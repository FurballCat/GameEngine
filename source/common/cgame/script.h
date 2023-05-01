/* Copyright (c) Furball Cat */

#pragma once

#include "api.h"
#include "ccore/types.h"

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

typedef struct FcGameObject FcGameObject;
typedef struct FcWorld FcWorld;
typedef struct FcBinaryBuffer FcBinaryBuffer;

// this is the context you will get on script function call
typedef struct FcScriptCtx
{
	FcStringId state;
	FcStringId stateEventToCall;

	FcStringId nextState;
	FcVariant lastResult;
	f32 waitSeconds;
	u32 numSkipOps;

	FcGameObject* self;
	FcWorld* world;
} FcScriptCtx;

// this is the instance of the script, keep it with your game object
typedef struct FcScriptLambda
{
	f32 waitSeconds;
	u32 numSkipOps;
	bool isActive;	// todo: remove?
	FcStringId lambdaName;	// or state name
	FcStringId eventName;
	FcStringId state;
	FcGameObject* selfGameObject;		// object associated with the script (NULL if none)
	const FcBinaryBuffer* scriptBlob;	// load .bin script binary into this buffer
} FcScriptLambda;

// call this to execute the script
CGAME_API void fcScriptUpdateLambda(FcScriptLambda* lambda, FcWorld* world, f32 dt);

// helper script functions
CGAME_API FcGameObject* fcScriptLookUpGameObject(FcScriptCtx* ctx, FcStringId name);

// call once at the start and end of the application
CGAME_API FcResult fcScriptInit(const FcAllocator* allocator);
CGAME_API void fcScriptRelease(const FcAllocator* allocator);

#ifdef __cplusplus
}
#endif // __cplusplus
