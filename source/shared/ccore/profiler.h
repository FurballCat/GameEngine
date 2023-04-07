/* Copyright (c) 2016-2021 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include <inttypes.h>
#include <stdbool.h>
#include "api.h"
#include "platform.h"

#define FUR_USE_PROFILER 1
#define FUR_USE_LOG_PROFILER 1

// put FUR_PROFILE("scope-name") before curly bracket '{' for automatic scope
#if FUR_USE_PROFILER
#define FUR_PROFILE(scopeName) for(FcProfilerScope* __profiler_scope = fcProfilerScopeBegin(scopeName); __profiler_scope; fcProfilerScopeEnd(__profiler_scope), __profiler_scope = NULL)
#else
#define FUR_PROFILE(scopeName)
#endif

typedef struct FcAllocator FcAllocator;

CCORE_API void fcProfilerInit(FcAllocator* allocator);
CCORE_API void fcProfilerRelease(FcAllocator* allocator);

typedef struct FcProfilerScope
{
	const char* name;
	struct FcProfilerScope* parent;	// kept only because of fibers
	FcTimeval startTime;
	FcTimeval stopTime;
	u16 depth;	// depth in callstack
	u16 threadID;	// the thread that closed the scope
} FcProfilerScope;

CCORE_API FcProfilerScope* fcProfilerScopeBegin(const char* name);
CCORE_API void fcProfilerScopeEnd(FcProfilerScope* scope);

CCORE_API void fcProfilerStartFrame(void);
CCORE_API void fcProfilerEndFrame(void);
CCORE_API void fcProfilerToggleDraw(void);
CCORE_API void fcProfilerTogglePause(void);
CCORE_API bool fcProfilerIsDrawOn(void);
CCORE_API void fcProfilerZoomAndPanDelta(f32 zoomDelta, f32 panDelta);

// put FUR_LOG_PROFILE("scope-name") before curly bracket '{' for automatic scope, use log profiler for engine init or one time functions
#if FUR_USE_LOG_PROFILER
#define FUR_LOG_PROFILE(scopeName) for(uint64_t __start_time = fcLogProfilerBegin(); __start_time != 0; fcLogProfilerEnd(scopeName, __start_time), __start_time = 0)
#else
#define FUR_LOG_PROFILE(scopeName)
#endif

CCORE_API uint64_t fcLogProfilerBegin(void);
CCORE_API void fcLogProfilerEnd(const char* scopeName, uint64_t startTime);

// for fibers, to not loose callstack when switching fiber
i32 fcProfilerStoreScopestack(FcProfilerScope* stack[32]);
void fcProfilerLoadScopestack(FcProfilerScope* stack[32], i32 numDepth);

// to detect locks and thread contention
void fcProfilerEnterContention(void);
void fcProfilerExitContention(const char* name);

#ifdef __cplusplus
}
#endif // __cplusplus
