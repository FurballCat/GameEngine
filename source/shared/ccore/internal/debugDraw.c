/* Copyright (c) 2016-2019 Furball Cat */

#include <stdlib.h>
#include <string.h>

#include "debugDraw.h"
#include "memory.h"
#include "furAssert.h"

#define FC_DEBUG_FRAGMENTS_LINES_CAPACITY 4096
#define FC_DEBUG_FRAGMENTS_TRIANGLES_CAPACITY 4096
#define FC_DEBUG_VERTEX_NUM_FLOATS 7

#define FC_DEBUG_LINE_SIZE sizeof(float) * FC_DEBUG_VERTEX_NUM_FLOATS * 2
#define FC_DEBUG_TRIANGLE_SIZE sizeof(float) * FC_DEBUG_VERTEX_NUM_FLOATS * 3

typedef struct fc_debug_fragments_t
{
	float* linesData;
	uint32_t numLines;	// todo: this can be atomic, so we will avoid locking the global
	
	float* trianglesData;
	uint32_t numTriangles;
	
} fc_debug_fragments_t;

fc_debug_fragments_t g_debugFragments;

void fc_dbg_init(fc_alloc_callbacks_t* pAllocCallbacks)
{
	memset(&g_debugFragments, 0, sizeof(fc_debug_fragments_t));
	
	g_debugFragments.linesData = FUR_ALLOC(fc_dbg_line_buffer_size(), 8, FC_MEMORY_SCOPE_DEBUG, pAllocCallbacks);
	g_debugFragments.numLines = 0;
	
	g_debugFragments.trianglesData = FUR_ALLOC(fc_dbg_triangle_buffer_size(), 8, FC_MEMORY_SCOPE_DEBUG, pAllocCallbacks);
	g_debugFragments.numTriangles = 0;
}

void fc_dbg_release(fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(g_debugFragments.linesData, pAllocCallbacks);
	FUR_FREE(g_debugFragments.trianglesData, pAllocCallbacks);
	
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

uint32_t fc_dbg_line_num_total_vertices(void)
{
	return 2 * FC_DEBUG_FRAGMENTS_LINES_CAPACITY;
}

uint32_t fc_dbg_line_buffer_size(void)
{
	return FC_DEBUG_LINE_SIZE * FC_DEBUG_FRAGMENTS_LINES_CAPACITY;
}

void fc_dbg_buffers_lock(void)
{
	// todo: implement when multithreading comes in
}

void fc_dbg_get_buffers(fc_dbg_buffer_desc_t* outDesc)
{
	outDesc->linesData = g_debugFragments.linesData;
	outDesc->linesDataSize = fc_dbg_line_current_lines_size();
	
	outDesc->trianglesData = g_debugFragments.trianglesData;
	outDesc->trianglesDataSize = fc_dbg_triangles_current_triangles_size();
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
	return FC_DEBUG_LINE_SIZE * fc_dbg_line_current_num_lines();
}

const float* fc_dbg_triangles_get_data(void)
{
	return g_debugFragments.trianglesData;
}

uint32_t fc_dbg_triangles_current_num_triangles(void)
{
	return g_debugFragments.numTriangles;
}

uint32_t fc_dbg_triangles_current_triangles_size(void)
{
	return FC_DEBUG_TRIANGLE_SIZE * fc_dbg_triangles_current_num_triangles();
}

uint32_t fc_dbg_triangles_num_total_vertices(void)
{
	return 3 * FC_DEBUG_FRAGMENTS_TRIANGLES_CAPACITY;
}

void fc_dbg_buffers_clear()
{
	g_debugFragments.numLines = 0;
	g_debugFragments.numTriangles = 0;
}

void fc_dbg_buffers_unlock(void)
{
	// todo: implement when multithreading comes in
}

void fc_dbg_triangle(const float pointA[3], const float pointB[3], const float pointC[3], const float color[4])
{
	FUR_ASSERT(g_debugFragments.numTriangles < FC_DEBUG_FRAGMENTS_TRIANGLES_CAPACITY);
	
	uint32_t idx = g_debugFragments.numTriangles * 3 * FC_DEBUG_VERTEX_NUM_FLOATS;
	
	g_debugFragments.trianglesData[idx++] = pointA[0];
	g_debugFragments.trianglesData[idx++] = pointA[1];
	g_debugFragments.trianglesData[idx++] = pointA[2];
	
	g_debugFragments.trianglesData[idx++] = color[0];
	g_debugFragments.trianglesData[idx++] = color[1];
	g_debugFragments.trianglesData[idx++] = color[2];
	g_debugFragments.trianglesData[idx++] = color[3];
	
	g_debugFragments.trianglesData[idx++] = pointB[0];
	g_debugFragments.trianglesData[idx++] = pointB[1];
	g_debugFragments.trianglesData[idx++] = pointB[2];
	
	g_debugFragments.trianglesData[idx++] = color[0];
	g_debugFragments.trianglesData[idx++] = color[1];
	g_debugFragments.trianglesData[idx++] = color[2];
	g_debugFragments.trianglesData[idx++] = color[3];
	
	g_debugFragments.trianglesData[idx++] = pointC[0];
	g_debugFragments.trianglesData[idx++] = pointC[1];
	g_debugFragments.trianglesData[idx++] = pointC[2];
	
	g_debugFragments.trianglesData[idx++] = color[0];
	g_debugFragments.trianglesData[idx++] = color[1];
	g_debugFragments.trianglesData[idx++] = color[2];
	g_debugFragments.trianglesData[idx++] = color[3];
	
	g_debugFragments.numTriangles += 1;
	
	FUR_ASSERT(idx == g_debugFragments.numTriangles * 3 * FC_DEBUG_VERTEX_NUM_FLOATS);
}

uint32_t fc_dbg_triangle_buffer_size(void)
{
	return FC_DEBUG_TRIANGLE_SIZE * FC_DEBUG_FRAGMENTS_TRIANGLES_CAPACITY;
}

void fc_dbg_plane(const float center[3], const float halfLength, const float color[4])
{
	const float planeA[3] = {-halfLength + center[0], -halfLength + center[1], center[2]};
	const float planeB[3] = {-halfLength + center[0], halfLength + center[1], center[2]};
	const float planeC[3] = {halfLength + center[0], halfLength + center[1], center[2]};
	const float planeD[3] = {halfLength + center[0], -halfLength + center[1], center[2]};
	
	fc_dbg_triangle(planeC, planeB, planeA, color);
	fc_dbg_triangle(planeA, planeD, planeC, color);
}
