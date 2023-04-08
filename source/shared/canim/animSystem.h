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

// call create and destroy once at the beginning and end of engine creation
CANIM_API FcResult fcCreateAnimSystem(const FcAllocator* allocator, FcAnimSystem** animSystem);
CANIM_API void fcDestroyAnimSystem(FcAnimSystem* sys, const FcAllocator* allocator);

// create single character
typedef struct FcAnimCharacterDesc
{
	const FcRig* rig;
	f64 globalTime;
} FcAnimCharacterDesc;

CANIM_API FcAnimCharacter* fcCreateAnimCharacter(const FcAnimCharacterDesc* desc, const FcAllocator* allocator);
CANIM_API void fcDestroyAnimCharacter(FcAnimCharacter* character, const FcAllocator* allocator);

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
