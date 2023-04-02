/* Copyright (c) Furball Cat */

#include "animSystem.h"
#include "ccore/public.h"
#include "canim/public.h"
#include "cmath/public.h"

#define FUR_MAX_ANIM_CHARACTERS 32

typedef struct FcAnimSystem
{
	FcAnimCharacter* characters[FUR_MAX_ANIM_CHARACTERS];
	i32 numCharacters;
	
} FcAnimSystem;

FcAnimSystem* fcAnimSystemInit(FcAllocator* pAllocCallbacks)
{
	FcAnimSystem* sys = FUR_ALLOC_AND_ZERO(sizeof(FcAnimSystem), 0, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
	return sys;
}

void fcAnimSystemRelease(FcAnimSystem* sys, FcAllocator* pAllocCallbacks)
{
	FUR_FREE(sys, pAllocCallbacks);
}

FcAnimCharacter* fcAnimCharacterCreate(const FcAnimCharacterDesc* desc, FcAllocator* pAllocCallbacks)
{
	FcAnimCharacter* character = FUR_ALLOC_AND_ZERO(sizeof(FcAnimCharacter), 0, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
	
	fcAnimCharacterInit(character, desc->rig, pAllocCallbacks);
	character->globalTime = desc->globalTime * 1000000;
	
	return character;
}

void fcAnimSystemAnimCharacterRelease(FcAnimCharacter* character, FcAllocator* pAllocCallbacks)
{
	fcAnimCharacterRelease(character, pAllocCallbacks);
	
	FUR_FREE(character, pAllocCallbacks);
}

void fcAnimSystemAddCharacter(FcAnimSystem* sys, FcAnimCharacter* character)
{
	FUR_ASSERT(sys->numCharacters < FUR_MAX_ANIM_CHARACTERS);
	
	// acquire index for character
	const i32 idx = sys->numCharacters;
	sys->numCharacters++;
	
	// add character
	sys->characters[idx] = character;
}

void fcAnimSystemRemoveCharacter(FcAnimSystem* sys, FcAnimCharacter* character)
{
	for(i32 i=0; i<sys->numCharacters; ++i)
	{
		// if not found, continue searching
		if(sys->characters[i] != character)
			continue;
		
		// if found, zero-out
		sys->characters[i] = NULL;
		
		// swap with last added character;
		if(i+1 < sys->numCharacters)
		{
			sys->characters[i] = sys->characters[sys->numCharacters];
		}
		
		// decrease number of characters and finish the search
		sys->numCharacters--;
		
		break;
	}
}

void fcAnimSystemUpdate(FcAnimSystem* sys, const FcAnimSystemUpdateCtx* ctx)
{
	// for each character
	for(i32 i=0; i<sys->numCharacters; ++i)
	{
		FcAnimCharacter* character = sys->characters[i];
		
		// sub arena allocator
		FcMemArenaAllocator arenaAlloc = fcMemArenaSub(*(ctx->arenaAlloc));	// note: when using jobs, remember to split the memory
		
		// animation states update
		FcAnimCharacterUpdateCtx animateCtx = {0};
		animateCtx.dt = ctx->dt;
		animateCtx.arenaAlloc = &arenaAlloc;
		animateCtx.showDebug = false;
		
		fcAnimCharacterUpdate(character, &animateCtx);
		
		// skinning
		if(character->skinMatrices)
		{
			const fm_xform* poseMS = character->poseMS;
			const u32 numSkinMatrices = character->rig->numBones;
			for(u32 i=0; i<numSkinMatrices; ++i)
			{
				fm_xform_to_mat4(&poseMS[i], &character->skinMatrices[i]);
			}
		}
	}
}
