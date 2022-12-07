/* Copyright (c) Furball Cat */

#include "animSystem.h"
#include "ccore/public.h"
#include "canim/public.h"
#include "cmath/public.h"

#define FUR_MAX_ANIM_CHARACTERS 32

typedef struct fa_anim_sys_t
{
	fa_character_t* characters[FUR_MAX_ANIM_CHARACTERS];
	int32_t numCharacters;
	
} fa_anim_sys_t;

fa_anim_sys_t* fa_anim_sys_init(fc_alloc_callbacks_t* pAllocCallbacks)
{
	fa_anim_sys_t* sys = FUR_ALLOC_AND_ZERO(sizeof(fa_anim_sys_t), 0, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
	return sys;
}

void fa_anim_sys_release(fa_anim_sys_t* sys, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(sys, pAllocCallbacks);
}

fa_character_t* fa_anim_sys_create_character(const fa_character_desc_t* desc, fc_alloc_callbacks_t* pAllocCallbacks)
{
	fa_character_t* character = FUR_ALLOC_AND_ZERO(sizeof(fa_character_t), 0, FC_MEMORY_SCOPE_ANIMATION, pAllocCallbacks);
	
	fa_character_init(character, desc->rig, pAllocCallbacks);
	character->globalTime = desc->globalTime * 1000000;
	
	return character;
}

void fa_anim_sys_release_character(fa_character_t* character, fc_alloc_callbacks_t* pAllocCallbacks)
{
	fa_character_release(character, pAllocCallbacks);
	
	FUR_FREE(character, pAllocCallbacks);
}

void fa_anim_sys_add_character(fa_anim_sys_t* sys, fa_character_t* character)
{
	FUR_ASSERT(sys->numCharacters < FUR_MAX_ANIM_CHARACTERS);
	
	// acquire index for character
	const int32_t idx = sys->numCharacters;
	sys->numCharacters++;
	
	// add character
	sys->characters[idx] = character;
}

void fa_anim_sys_remove_character(fa_anim_sys_t* sys, fa_character_t* character)
{
	for(int32_t i=0; i<sys->numCharacters; ++i)
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

void fa_anim_sys_update(fa_anim_sys_t* sys, const fa_anim_sys_update_ctx_t* ctx)
{
	// for each character
	for(int32_t i=0; i<sys->numCharacters; ++i)
	{
		fa_character_t* character = sys->characters[i];
		
		// animation states update
		fa_character_animate_ctx_t animateCtx = {};
		animateCtx.dt = ctx->dt;
		animateCtx.arenaAlloc = ctx->arenaAlloc;	// note: when using jobs, remember to split the memory
		animateCtx.showDebug = false;
		
		fa_character_animate(character, &animateCtx);
		
		// skinning
		if(character->skinMatrices)
		{
			const fm_xform* poseMS = character->poseMS;
			const uint32_t numSkinMatrices = character->rig->numBones;
			for(uint32_t i=0; i<numSkinMatrices; ++i)
			{
				fm_xform_to_mat4(&poseMS[i], &character->skinMatrices[i]);
			}
		}
	}
}
