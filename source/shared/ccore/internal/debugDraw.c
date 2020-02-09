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
	
	g_debugFragments.linesData = FUR_ALLOC(sizeof(float) * 2 * FC_DEBUG_VERTEX_NUM_FLOATS * FC_DEBUG_FRAGMENTS_LINES_CAPACITY, 8, FC_MEMORY_SCOPE_DEBUG, pAllocCallbacks);
	g_debugFragments.numLines = 0;
}

void fc_dbg_release(fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(g_debugFragments.linesData, pAllocCallbacks);
	
	memset(&g_debugFragments, 0, sizeof(fc_debug_fragments_t));
}

void fc_dbg_line(float start[3], float end[3], float color[4])
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
