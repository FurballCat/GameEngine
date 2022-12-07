/* Copyright (c) Furball Cat */

#include "animSystem.h"
#include "ccore/public.h"
#include "canim/public.h"

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
