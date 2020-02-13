/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus
	
#include <inttypes.h>
#include "api.h"
	
typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;
	
CCORE_API void fc_dbg_init(fc_alloc_callbacks_t* pAllocCallbacks);
CCORE_API void fc_dbg_release(fc_alloc_callbacks_t* pAllocCallbacks);
	
#define FUR_COLOR_WHITE 	{1.0f, 1.0f, 1.0f, 1.0f}
#define FUR_COLOR_BLACK 	{0.0f, 0.0f, 0.0f, 1.0f}
#define FUR_COLOR_RED  		{1.0f, 0.0f, 0.0f, 1.0f}
#define FUR_COLOR_GREEN 	{0.0f, 1.0f, 0.0f, 1.0f}
#define FUR_COLOR_BLUE 		{0.0f, 0.0f, 1.0f, 1.0f}
#define FUR_COLOR_CYAN 		{0.0f, 1.0f, 1.0f, 1.0f}
#define FUR_COLOR_MAGENTA 	{1.0f, 0.0f, 1.0f, 1.0f}
#define FUR_COLOR_YELLOW 	{1.0f, 1.0f, 0.0f, 1.0f}
	
CCORE_API void fc_dbg_line(const float start[3], const float end[3], const float color[4]);
CCORE_API uint32_t fc_dbg_line_buffer_size(void);
	
// call these functions in given order when retrieving data for current frame
CCORE_API void fc_dbg_line_lock(void);
	
CCORE_API const float* fc_dbg_line_get_data(void);
CCORE_API uint32_t fc_dbg_line_current_num_lines(void);
CCORE_API uint32_t fc_dbg_line_current_lines_size(void);
CCORE_API uint32_t fc_dbg_line_num_total_vertices(void);
	
CCORE_API void fc_dbg_line_clear(void);
	
CCORE_API void fc_dbg_line_unlock(void);
	
#ifdef __cplusplus
}
#endif // __cplusplus
