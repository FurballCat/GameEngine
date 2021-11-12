/* Copyright (c) 2016-2021 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include <inttypes.h>
#include <stdbool.h>
#include "api.h"

// put before curly bracket '{' for automatic scope
#define FUR_PROFILE(scopeName) for(fc_profiler_scope_t* __profiler_scope = fc_profiler_scope_begin(scopeName); __profiler_scope; fc_profiler_scope_end(__profiler_scope), __profiler_scope = NULL)

typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;

CCORE_API void fc_profiler_init(fc_alloc_callbacks_t* pAllocCallbacks);
CCORE_API void fc_profiler_release(fc_alloc_callbacks_t* pAllocCallbacks);

typedef struct fc_profiler_scope_t
{
	const char* name;
	uint32_t startTime;	// in microseconds (s > ms > us)
	uint32_t stopTime;
	uint32_t depth;	// depth in callstack
} fc_profiler_scope_t;

CCORE_API fc_profiler_scope_t* fc_profiler_scope_begin(const char* name);
CCORE_API void fc_profiler_scope_end(fc_profiler_scope_t* scope);

CCORE_API void fc_profiler_start_frame(void);
CCORE_API void fc_profiler_end_frame(void);
CCORE_API void fc_profiler_toggle_draw(void);
CCORE_API void fc_profiler_toggle_pause(void);
CCORE_API bool fc_profiler_is_draw_on(void);
CCORE_API void fc_profiler_zoom_and_pan_delta(float zoomDelta, float panDelta);

#ifdef __cplusplus
}
#endif // __cplusplus