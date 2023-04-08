/* Copyright (c) 2016-2020 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus
	
#include "ccore/types.h"
#include "api.h"

typedef u32 FcStringId;

CCORE_API FcStringId fcMakeStringId(const char* name);

// debug
CCORE_API FcStringId fcMakeStringIdAndRegister(const char* name);
CCORE_API const char* fcStringIdAsDebugCstr(FcStringId hash);

typedef struct FcAllocator FcAllocator;
CCORE_API void fcStringIdRegisterInit(const FcAllocator* allocator);
CCORE_API void fcStringIdRegisterRelease(const FcAllocator* allocator);
	
#define SID(_name) fcMakeStringId(_name)
#define SID_REG(_name) fcMakeStringIdAndRegister(_name)
	
#ifdef __cplusplus
}
#endif // __cplusplus
