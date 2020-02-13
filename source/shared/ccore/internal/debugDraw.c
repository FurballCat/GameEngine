/* Copyright (c) 2016-2019 Furball Cat */

#include <stdlib.h>
#include <string.h>

#include "debugDraw.h"
#include "memory.h"
#include "furAssert.h"

#define FC_DEBUG_FRAGMENTS_LINES_CAPACITY 4096
#define FC_DEBUG_VERTEX_NUM_FLOATS 7

typedef struct fc_debug_fragments_t
{
	float* linesData;
	uint32_t numLines;	// todo: this can be atomic, so we will avoid locking the global
	
} fc_debug_fragments_t;

fc_debug_fragments_t g_debugFragments;

void fc_dbg_init(fc_alloc_callbacks_t* pAllocCallbacks)
{
	memset(&g_debugFragments, 0, sizeof(fc_debug_fragments_t));
	
	g_debugFragments.linesData = FUR_ALLOC(fc_dbg_line_buffer_size(), 8, FC_MEMORY_SCOPE_DEBUG, pAllocCallbacks);
	g_debugFragments.numLines = 0;
}

void fc_dbg_release(fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(g_debugFragments.linesData, pAllocCallbacks);
	
	memset(&g_debugFragments, 0, sizeof(fc_debug_fragments_t));
}

void fc_dbg_line(const float start[3], const float end[3], const float color[4])
{
	FUR_ASSERT(g_debugFragments.numLines < FC_DEBUG_FRAGMENTS_LINES_CAPACITY);
	
	uint32_t idx = g_debugFragments.numLines * 2 * FC_DEBUG_VERTEX_NUM_FLOATS;
	
	g_debugFragments.linesData[idx++] = start[0];
	g_debugFragments.linesData[idx++] = start[1];
	g_debugFragments.linesData[idx++] = start[2];
	
	g_debugFragments.linesData[idx++] = color[0];
	g_debugFragments.linesData[idx++] = color[1];
	g_debugFragments.linesData[idx++] = color[2];
	g_debugFragments.linesData[idx++] = color[3];
	
	g_debugFragments.linesData[idx++] = end[0];
	g_debugFragments.linesData[idx++] = end[1];
	g_debugFragments.linesData[idx++] = end[2];
	
	g_debugFragments.linesData[idx++] = color[0];
	g_debugFragments.linesData[idx++] = color[1];
	g_debugFragments.linesData[idx++] = color[2];
	g_debugFragments.linesData[idx++] = color[3];
	
	g_debugFragments.numLines += 1;
	
	FUR_ASSERT(idx == g_debugFragments.numLines * 2 * FC_DEBUG_VERTEX_NUM_FLOATS);
}

uint32_t fc_dbg_line_buffer_size_by_num_lines(uint32_t numLines)
{
	return sizeof(float) * FC_DEBUG_VERTEX_NUM_FLOATS * 2 * numLines;
}

uint32_t fc_dbg_line_num_total_vertices(void)
{
	return 2 * FC_DEBUG_FRAGMENTS_LINES_CAPACITY;
}

uint32_t fc_dbg_line_buffer_size(void)
{
	return fc_dbg_line_buffer_size_by_num_lines(FC_DEBUG_FRAGMENTS_LINES_CAPACITY);
}

void fc_dbg_line_lock(void)
{
	// todo: implement when multithreading comes in
}

const float* fc_dbg_line_get_data(void)
{
	return g_debugFragments.linesData;
}

uint32_t fc_dbg_line_current_num_lines(void)
{
	return g_debugFragments.numLines;
}

uint32_t fc_dbg_line_current_lines_size(void)
{
	return fc_dbg_line_buffer_size_by_num_lines(fc_dbg_line_current_num_lines());
}

void fc_dbg_line_clear()
{
	g_debugFragments.numLines = 0;
}

void fc_dbg_line_unlock(void)
{
	// todo: implement when multithreading comes in
}
