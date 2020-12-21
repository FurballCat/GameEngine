/* Copyright (c) 2016-2019 Furball Cat */

#include <stdlib.h>
#include <string.h>

#include "debugDraw.h"
#include "memory.h"
#include "furAssert.h"

#define FC_DEBUG_FRAGMENTS_LINES_CAPACITY 4096
#define FC_DEBUG_FRAGMENTS_TRIANGLES_CAPACITY 4096
#define FC_DEBUG_VERTEX_NUM_FLOATS 7

// 512 lines of 512 characters or 1024 lines of 256 characters or 4096 lines of 64 characters
#define FC_DEBUG_FRAGMENTS_TEXT_CHARACTERS_CAPACITY 262144
#define FC_DEBUG_FRAGMENTS_TEXT_LINES_CAPACITY 4096
#define FC_DEBUG_TEXT_LOCATION_DATA_NUM_FLOATS 5
#define FC_DEBUG_TEXT_LOCATION_DATA_NUM_RANGE 2

#define FC_DEBUG_LINE_SIZE sizeof(float) * FC_DEBUG_VERTEX_NUM_FLOATS * 2
#define FC_DEBUG_TRIANGLE_SIZE sizeof(float) * FC_DEBUG_VERTEX_NUM_FLOATS * 3

typedef struct fc_debug_fragments_t
{
	float* linesData;
	uint32_t numLines;	// todo: this can be atomic, so we will avoid locking the global
	
	float* trianglesData;
	uint32_t numTriangles;
	
	float* textLocationData;	// x, y, r, g, b
	uint32_t* textRangeData;	// offset, length
	uint32_t numTextLines;
	char* textCharactersData;
	uint32_t numTextCharacters;
	
} fc_debug_fragments_t;

fc_debug_fragments_t g_debugFragments;

void fc_dbg_init(fc_alloc_callbacks_t* pAllocCallbacks)
{
	memset(&g_debugFragments, 0, sizeof(fc_debug_fragments_t));
	
	// lines alloc
	g_debugFragments.linesData = FUR_ALLOC(fc_dbg_line_buffer_size(), 8, FC_MEMORY_SCOPE_DEBUG, pAllocCallbacks);
	g_debugFragments.numLines = 0;
	
	// triangles alloc
	g_debugFragments.trianglesData = FUR_ALLOC(fc_dbg_triangle_buffer_size(), 8, FC_MEMORY_SCOPE_DEBUG, pAllocCallbacks);
	g_debugFragments.numTriangles = 0;
	
	// text alloc
	{
		const uint32_t sizeData = sizeof(float) * FC_DEBUG_FRAGMENTS_TEXT_LINES_CAPACITY * FC_DEBUG_TEXT_LOCATION_DATA_NUM_FLOATS;
		g_debugFragments.textLocationData = FUR_ALLOC(sizeData, 8, FC_MEMORY_SCOPE_DEBUG, pAllocCallbacks);
	}
	
	{
		const uint32_t sizeData = sizeof(uint32_t) * FC_DEBUG_FRAGMENTS_TEXT_LINES_CAPACITY * FC_DEBUG_TEXT_LOCATION_DATA_NUM_RANGE;
		g_debugFragments.textRangeData = FUR_ALLOC(sizeData, 8, FC_MEMORY_SCOPE_DEBUG, pAllocCallbacks);
	}
	
	{
		const uint32_t sizeData = sizeof(char) * FC_DEBUG_FRAGMENTS_TEXT_CHARACTERS_CAPACITY;
		g_debugFragments.textCharactersData = FUR_ALLOC(sizeData, 8, FC_MEMORY_SCOPE_DEBUG, pAllocCallbacks);
	}
	
	g_debugFragments.numTextCharacters = 0;
	g_debugFragments.numTextLines = 0;
}

void fc_dbg_release(fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(g_debugFragments.linesData, pAllocCallbacks);
	FUR_FREE(g_debugFragments.trianglesData, pAllocCallbacks);
	FUR_FREE(g_debugFragments.textLocationData, pAllocCallbacks);
	FUR_FREE(g_debugFragments.textRangeData, pAllocCallbacks);
	FUR_FREE(g_debugFragments.textCharactersData, pAllocCallbacks);
	
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
	
	outDesc->textLocationData = g_debugFragments.textLocationData;
	outDesc->textCharactersData = g_debugFragments.textCharactersData;
	outDesc->textRangeData = g_debugFragments.textRangeData;
	outDesc->textLinesCount = g_debugFragments.numTextLines;
	outDesc->textCharacterDataSize = g_debugFragments.numTextCharacters;
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
	g_debugFragments.numTextCharacters = 0;
	g_debugFragments.numTextLines = 0;
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

void fc_dbg_text(float x, float y, const char* txt, const float color[4])
{
	const uint32_t length = (uint32_t)strlen(txt);
	
	FUR_ASSERT(g_debugFragments.numTextLines < FC_DEBUG_FRAGMENTS_TEXT_LINES_CAPACITY);
	FUR_ASSERT(g_debugFragments.numTextCharacters + length < FC_DEBUG_FRAGMENTS_TEXT_CHARACTERS_CAPACITY);
	
	const uint32_t offsetFloat = g_debugFragments.numTextLines * FC_DEBUG_TEXT_LOCATION_DATA_NUM_FLOATS;
	const uint32_t offsetRange = g_debugFragments.numTextLines * FC_DEBUG_TEXT_LOCATION_DATA_NUM_RANGE;
	g_debugFragments.numTextLines += 1;

	float* dataLocation = g_debugFragments.textLocationData + offsetFloat;
	dataLocation[0] = x;
	dataLocation[1] = y;
	dataLocation[2] = color[0];
	dataLocation[3] = color[1];
	dataLocation[4] = color[2];
	
	const uint32_t offsetCharacters = g_debugFragments.numTextCharacters;
	g_debugFragments.numTextCharacters += length;
	
	char* dataCharacters = g_debugFragments.textCharactersData + offsetCharacters;
	memcpy(dataCharacters, txt, length);
	
	uint32_t* dataRange = g_debugFragments.textRangeData + offsetRange;
	dataRange[0] = offsetCharacters;
	dataRange[1] = length;
}
