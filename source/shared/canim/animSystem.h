/* Copyright (c) Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include "ccore/types.h"

typedef struct FcAllocator FcAllocator;
typedef struct FcAnimSystem FcAnimSystem;
typedef struct FcAnimCharacter FcAnimCharacter;
typedef struct FcRig FcRig;
typedef struct FcMemArenaAllocator FcMemArenaAllocator;

// call init and release once at the beginning and end of engine creation
CANIM_API FcAnimSystem* fcAnimSystemInit(FcAllocator* pAllocCallbacks);
CANIM_API void fcAnimSystemRelease(FcAnimSystem* sys, FcAllocator* pAllocCallbacks);

// create single character
typedef struct FcAnimCharacterDesc
{
	const FcRig* rig;
	f64 globalTime;
} FcAnimCharacterDesc;

CANIM_API FcAnimCharacter* fcAnimCharacterCreate(const FcAnimCharacterDesc* desc, FcAllocator* pAllocCallbacks);
CANIM_API void fcAnimSystemAnimCharacterRelease(FcAnimCharacter* character, FcAllocator* pAllocCallbacks);

CANIM_API void fcAnimSystemAddCharacter(FcAnimSystem* sys, FcAnimCharacter* character);
CANIM_API void fcAnimSystemRemoveCharacter(FcAnimSystem* sys, FcAnimCharacter* character);

typedef struct FcAnimSystemUpdateCtx
{
	f32 dt;
	f64 globalTime;
	
	FcMemArenaAllocator* arenaAlloc;
} FcAnimSystemUpdateCtx;

CANIM_API void fcAnimSystemUpdate(FcAnimSystem* sys, const FcAnimSystemUpdateCtx* ctx);

#ifdef __cplusplus
}
#endif // __cplusplus
