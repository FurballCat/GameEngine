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
#define FUR_COLOR_DARK_GREY	{0.2f, 0.2f, 0.2f, 1.0f}
	
// call these to draw debug fragments
CCORE_API void fc_dbg_line(const float start[3], const float end[3], const float color[4]);
CCORE_API void fc_dbg_triangle(const float pointA[3], const float pointB[3], const float pointC[3], const float color[4]);

CCORE_API void fc_dbg_plane(const float center[3], const float halfLength, const float color[4]);
	
// use sprintf( txt, "blah blah %u", value ) and pass txt to the function (txt is char txt[32] or any other reasonable size)
CCORE_API void fc_dbg_text(float x, float y, const char* txt, const float color[4]);
	
// sizes of each debug fragment buffers
CCORE_API uint32_t fc_dbg_line_buffer_size(void);
CCORE_API uint32_t fc_dbg_triangle_buffer_size(void);
	
// call these functions in given order when retrieving data for current frame
CCORE_API void fc_dbg_buffers_lock(void);
	
CCORE_API const float* fc_dbg_line_get_data(void);
CCORE_API uint32_t fc_dbg_line_current_num_lines(void);
CCORE_API uint32_t fc_dbg_line_current_lines_size(void);
CCORE_API uint32_t fc_dbg_line_num_total_vertices(void);
	
CCORE_API const float* fc_dbg_triangles_get_data(void);
CCORE_API uint32_t fc_dbg_triangles_current_num_triangles(void);
CCORE_API uint32_t fc_dbg_triangles_current_triangles_size(void);
CCORE_API uint32_t fc_dbg_triangles_num_total_vertices(void);

typedef struct fc_dbg_buffers_desc_t
{
	const float* linesData;
	uint32_t linesDataSize;
	
	const float* trianglesData;
	uint32_t trianglesDataSize;
	
	const float* textLocationData;
	const uint32_t* textRangeData;
	uint32_t textLinesCount;
	
	const char* textCharactersData;
	uint32_t textCharacterDataSize;
} fc_dbg_buffer_desc_t;
	
CCORE_API void fc_dbg_get_buffers(fc_dbg_buffer_desc_t* outDesc);
	
CCORE_API void fc_dbg_buffers_clear(void);
	
CCORE_API void fc_dbg_buffers_unlock(void);
	
#ifdef __cplusplus
}
#endif // __cplusplus
