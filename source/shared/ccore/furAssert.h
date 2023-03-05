/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus
	
#include <assert.h>
	
#ifndef FUR_ASSERT
#if PLATFORM_OSX
	#define FUR_ASSERT(x) assert(x)
#elif PLATFORM_WINDOWS
	#define FUR_ASSERT(x) do { if(!(x)) { __debugbreak(); } } while ((void)0,0)
#endif
#endif
	
#ifdef __cplusplus
}
#endif // __cplusplus
