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
#define FUR_PROFILE(scopeName) for(fc_profiler_scope_t* __profiler_scope = fc_profiler_scope_begin(scopeName); __profiler_scope; fc_profiler_scope_end(__profiler_scope), __profiler_scope = NULL)
#else
#define FUR_PROFILE(scopeName)
#endif

typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;

CCORE_API void fc_profiler_init(fc_alloc_callbacks_t* pAllocCallbacks);
CCORE_API void fc_profiler_release(fc_alloc_callbacks_t* pAllocCallbacks);

typedef struct fc_profiler_scope_t
{
	const char* name;
	struct fc_profiler_scope_t* parent;	// kept only because of fibers
	fc_timeval_t startTime;
	fc_timeval_t stopTime;
	u16 depth;	// depth in callstack
	u16 threadID;	// the thread that closed the scope
} fc_profiler_scope_t;

CCORE_API fc_profiler_scope_t* fc_profiler_scope_begin(const char* name);
CCORE_API void fc_profiler_scope_end(fc_profiler_scope_t* scope);

CCORE_API void fc_profiler_start_frame(void);
CCORE_API void fc_profiler_end_frame(void);
CCORE_API void fc_profiler_toggle_draw(void);
CCORE_API void fc_profiler_toggle_pause(void);
CCORE_API bool fc_profiler_is_draw_on(void);
CCORE_API void fc_profiler_zoom_and_pan_delta(f32 zoomDelta, f32 panDelta);

// put FUR_LOG_PROFILE("scope-name") before curly bracket '{' for automatic scope, use log profiler for engine init or one time functions
#if FUR_USE_LOG_PROFILER
#define FUR_LOG_PROFILE(scopeName) for(uint64_t __start_time = fc_log_profiler_begin(); __start_time != 0; fc_log_profiler_end(scopeName, __start_time), __start_time = 0)
#else
#define FUR_LOG_PROFILE(scopeName)
#endif

CCORE_API uint64_t fc_log_profiler_begin(void);
CCORE_API void fc_log_profiler_end(const char* scopeName, uint64_t startTime);

// for fibers, to not loose callstack when switching fiber
i32 fc_profiler_store_scopestack(fc_profiler_scope_t* stack[32]);
void fc_profiler_load_scopestack(fc_profiler_scope_t* stack[32], i32 numDepth);

// to detect locks and thread contention
void fc_profiler_enter_contention(void);
void fc_profiler_exit_contention(const char* name);

#ifdef __cplusplus
}
#endif // __cplusplus
