/* Copyright (c) 2016-2020 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus
	
#include <inttypes.h>
#include "api.h"

typedef uint32_t fc_string_hash_t;

CCORE_API fc_string_hash_t fc_make_string_hash(const char* name);
	
#define FUR_NAME(_name) fc_make_string_hash(_name)

#ifdef __cplusplus
}
#endif // __cplusplus
