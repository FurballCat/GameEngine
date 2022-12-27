/* Copyright (c) Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include <inttypes.h>
#include <stdbool.h>

typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;
typedef struct fa_anim_sys_t fa_anim_sys_t;
typedef struct fa_character_t fa_character_t;
typedef struct fa_rig_t fa_rig_t;
typedef struct fc_mem_arena_alloc_t fc_mem_arena_alloc_t;

// call init and release once at the beginning and end of engine creation
CANIM_API fa_anim_sys_t* fa_anim_sys_init(fc_alloc_callbacks_t* pAllocCallbacks);
CANIM_API void fa_anim_sys_release(fa_anim_sys_t* sys, fc_alloc_callbacks_t* pAllocCallbacks);

// create single character
typedef struct fa_character_desc_t
{
	const fa_rig_t* rig;
	double globalTime;
} fa_character_desc_t;

CANIM_API fa_character_t* fa_anim_sys_create_character(const fa_character_desc_t* desc, fc_alloc_callbacks_t* pAllocCallbacks);
CANIM_API void fa_anim_sys_release_character(fa_character_t* character, fc_alloc_callbacks_t* pAllocCallbacks);

CANIM_API void fa_anim_sys_add_character(fa_anim_sys_t* sys, fa_character_t* character);
CANIM_API void fa_anim_sys_remove_character(fa_anim_sys_t* sys, fa_character_t* character);

typedef struct fa_anim_sys_update_ctx_t
{
	float dt;
	double globalTime;
	
	fc_mem_arena_alloc_t* arenaAlloc;
} fa_anim_sys_update_ctx_t;

CANIM_API void fa_anim_sys_update(fa_anim_sys_t* sys, const fa_anim_sys_update_ctx_t* ctx);

#ifdef __cplusplus
}
#endif // __cplusplus
