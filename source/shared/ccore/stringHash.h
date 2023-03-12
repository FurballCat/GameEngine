/* Copyright (c) 2016-2020 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus
	
#include "ccore/types.h"
#include "api.h"

typedef u32 fc_string_hash_t;

CCORE_API fc_string_hash_t fc_make_string_hash(const char* name);

// debug
typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;
CCORE_API fc_string_hash_t fc_make_string_hash_and_register(const char* name);
CCORE_API const char* fc_string_hash_as_cstr_debug(fc_string_hash_t hash);
CCORE_API void fc_string_hash_register_init(fc_alloc_callbacks_t* pAllocCallbacks);
CCORE_API void fc_string_hash_register_release(fc_alloc_callbacks_t* pAllocCallbacks);
	
#define SID(_name) fc_make_string_hash(_name)
#define SID_REG(_name) fc_make_string_hash_and_register(_name)
	
#ifdef __cplusplus
}
#endif // __cplusplus
