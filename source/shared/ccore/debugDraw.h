/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus
	
#include "ccore/types.h"
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
CCORE_API void fc_dbg_line(const f32 start[3], const f32 end[3], const f32 color[4]);
CCORE_API void fc_dbg_triangle(const f32 pointA[3], const f32 pointB[3], const f32 pointC[3], const f32 color[4]);
CCORE_API void fc_dbg_box_wire(const f32 center[3], const f32 extent[3], const f32 color[4]);
CCORE_API void fc_dbg_plane(const f32 center[3], const f32 halfLength, const f32 color[4]);
CCORE_API void fc_dbg_circle(const f32 center[3], const f32 radius, const f32 up[3], const f32 color[4]);
CCORE_API void fc_dbg_sphere_wire(const f32 center[3], const f32 radius, const f32 color[4]);
	
// use sprintf( txt, "blah blah %u", value ) and pass txt to the function (txt is char txt[32] or any other reasonable size)
CCORE_API void fc_dbg_text(f32 x, f32 y, const char* txt, const f32 color[4], f32 scale);

typedef struct fc_dbg_screen_info_t
{
	f32 width;
	f32 height;
} fc_dbg_screen_info_t;

// set/get info about screen corners and size
CCORE_API void fc_dbg_get_screen_info(fc_dbg_screen_info_t* info);
CCORE_API void fc_dbg_set_screen_info(const fc_dbg_screen_info_t* info);	// this should be called on screen resize

typedef enum fc_dbg_screen_anchors_t
{
	FC_DBG_ANCHOR_LEFT_UP_CORNER = 0,
	FC_DBG_ANCHOR_RIGHT_UP_CORNER,
	FC_DBG_ANCHOR_LEFT_BOTTOM_CORNER,
	FC_DBG_ANCHOR_RIGHT_BOTTOM_CORNER,
	FC_DBG_ANCHOR_CENTER,
} fc_dbg_screen_anchors_t;

// call this to set init x and y for 2D drawing aligned to anchors
CCORE_API void fc_dbg_apply_anchor(f32* x, f32* y, fc_dbg_screen_anchors_t anchor);
static inline f32 fc_dbg_get_text_line_height(f32 scale) { return 28.0f * scale; }

// draw flat 2D rectancle (drawn under text), use it for simple debug UI rectancles
CCORE_API void fc_dbg_rect(f32 x, f32 y, f32 width, f32 height, const f32 color[4]);
	
// sizes of each debug fragment buffers
CCORE_API u32 fc_dbg_line_buffer_size(void);
CCORE_API u32 fc_dbg_triangle_buffer_size(void);
CCORE_API u32 fc_dbg_text_characters_capacity(void);
CCORE_API u32 fc_dbg_text_num_total_characters(void);
	
// call these functions in given order when retrieving data for current frame
CCORE_API void fc_dbg_buffers_lock(void);
	
CCORE_API const f32* fc_dbg_line_get_data(void);
CCORE_API u32 fc_dbg_line_current_num_lines(void);
CCORE_API u32 fc_dbg_line_current_lines_size(void);
CCORE_API u32 fc_dbg_line_num_total_vertices(void);
	
CCORE_API const f32* fc_dbg_triangles_get_data(void);
CCORE_API u32 fc_dbg_triangles_current_num_triangles(void);
CCORE_API u32 fc_dbg_triangles_current_triangles_size(void);
CCORE_API u32 fc_dbg_triangles_num_total_vertices(void);

CCORE_API u32 fc_dbg_rects_current_num_rects(void);
CCORE_API u32 fc_dbg_rects_buffer_size(void);
CCORE_API u32 fc_dbg_rects_current_num_vertices(void);
CCORE_API u32 fc_dbg_rects_current_data_size(void);
CCORE_API u32 fc_dbg_rects_num_total_vertices(void);
CCORE_API u32 fc_dbg_rect_num_floats_per_vertex(void);

typedef struct fc_dbg_buffers_desc_t
{
	// 3D lines
	const f32* linesData;
	u32 linesDataSize;
	
	// 3D triangles
	const f32* trianglesData;
	u32 trianglesDataSize;
	
	// 2D text
	const f32* textLocationData;
	const f32* textScaleData;
	const u32* textRangeData;
	u32 textLinesCount;
	
	const char* textCharactersData;
	u32 textCharacterDataSize;
	
	// 2D rects
	const f32* rectsData;
	u32 rectsDataSize;
} fc_dbg_buffer_desc_t;
	
CCORE_API void fc_dbg_get_buffers(fc_dbg_buffer_desc_t* outDesc);
	
CCORE_API void fc_dbg_buffers_clear(void);
	
CCORE_API void fc_dbg_buffers_unlock(void);
	
#ifdef __cplusplus
}
#endif // __cplusplus
