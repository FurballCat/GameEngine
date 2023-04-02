/* Copyright (c) 2016-2019 Furball Cat */

#include <stdlib.h>
#include <string.h>

#include "debugDraw.h"
#include "memory.h"
#include "furAssert.h"
#include <corecrt_math_defines.h>

#define FC_DEBUG_FRAGMENTS_LINES_CAPACITY 4096
#define FC_DEBUG_FRAGMENTS_TRIANGLES_CAPACITY 4096
#define FC_DEBUG_FRAGMENTS_RECTS_CAPACITY 4096
#define FC_DEBUG_VERTEX_NUM_FLOATS 7

// 512 lines of 512 characters or 1024 lines of 256 characters or 4096 lines of 64 characters
#define FC_DEBUG_FRAGMENTS_TEXT_CHARACTERS_CAPACITY 262144
#define FC_DEBUG_FRAGMENTS_TEXT_LINES_CAPACITY 4096
#define FC_DEBUG_TEXT_LOCATION_DATA_NUM_FLOATS 5
#define FC_DEBUG_TEXT_LOCATION_DATA_NUM_RANGE 2

#define FC_DEBUG_LINE_SIZE sizeof(f32) * FC_DEBUG_VERTEX_NUM_FLOATS * 2
#define FC_DEBUG_TRIANGLE_SIZE sizeof(f32) * FC_DEBUG_VERTEX_NUM_FLOATS * 3

// rects vertices are xy rgba - 6 floats, 6 vertices per rect because 2x triangle 3x vertex
#define FC_DEBUG_RECT_NUM_VERTICES 6
#define FC_DEBUG_RECTS_VERTEX_NUM_FLOATS 6
#define FC_DEBUG_RECT_SIZE sizeof(f32) * FC_DEBUG_RECTS_VERTEX_NUM_FLOATS * FC_DEBUG_RECT_NUM_VERTICES

typedef struct FcDebugFragments
{
	// debug 3D lines
	f32* linesData;
	u32 numLines;	// todo: this can be atomic, so we will avoid locking the global
	
	// debug 3D triangles
	f32* trianglesData;
	u32 numTriangles;
	
	// debug text
	f32* textLocationData;	// 5x f32: x, y, r, g, b
	u32* textRangeData;	// offset, length
	u32 numTextLines;
	char* textCharactersData;
	f32* textScaleData;	// 1 f32 per debug text
	u32 numTextCharacters;
	
	// debug 2D rectangles
	f32* rectData;	// 4x vertex of 7 floats (xyz rgba)
	u32 numRects;

	// screen info
	FcDebugScreenInfo screenInfo;
	
} FcDebugFragments;

FcDebugFragments g_debugFragments;

void fcDebugInit(FcAllocator* pAllocCallbacks)
{
	memset(&g_debugFragments, 0, sizeof(FcDebugFragments));
	
	// lines alloc
	g_debugFragments.linesData = FUR_ALLOC(fcDebugLineBufferSize(), 8, FC_MEMORY_SCOPE_DEBUG, pAllocCallbacks);
	g_debugFragments.numLines = 0;
	
	// triangles alloc
	g_debugFragments.trianglesData = FUR_ALLOC(fcDebugTriangleBufferSize(), 8, FC_MEMORY_SCOPE_DEBUG, pAllocCallbacks);
	g_debugFragments.numTriangles = 0;
	
	// text alloc
	{
		const u32 sizeData = sizeof(f32) * FC_DEBUG_FRAGMENTS_TEXT_LINES_CAPACITY * FC_DEBUG_TEXT_LOCATION_DATA_NUM_FLOATS;
		g_debugFragments.textLocationData = FUR_ALLOC(sizeData, 8, FC_MEMORY_SCOPE_DEBUG, pAllocCallbacks);
	}
	
	{
		const u32 sizeData = sizeof(u32) * FC_DEBUG_FRAGMENTS_TEXT_LINES_CAPACITY * FC_DEBUG_TEXT_LOCATION_DATA_NUM_RANGE;
		g_debugFragments.textRangeData = FUR_ALLOC(sizeData, 8, FC_MEMORY_SCOPE_DEBUG, pAllocCallbacks);
	}
	
	{
		const u32 sizeData = sizeof(char) * FC_DEBUG_FRAGMENTS_TEXT_CHARACTERS_CAPACITY;
		g_debugFragments.textCharactersData = FUR_ALLOC(sizeData, 8, FC_MEMORY_SCOPE_DEBUG, pAllocCallbacks);
	}

	{
		const u32 sizeData = sizeof(f32) * FC_DEBUG_FRAGMENTS_TEXT_LINES_CAPACITY;
		g_debugFragments.textScaleData = FUR_ALLOC(sizeData, 8, FC_MEMORY_SCOPE_DEBUG, pAllocCallbacks);
	}
	
	// 2D rect alloc
	{
		const u32 sizeData = FC_DEBUG_RECT_SIZE * FC_DEBUG_FRAGMENTS_RECTS_CAPACITY;
		g_debugFragments.rectData = FUR_ALLOC(sizeData, 8, FC_MEMORY_SCOPE_DEBUG, pAllocCallbacks);
		g_debugFragments.numRects = 0;
	}
	
	g_debugFragments.numTextCharacters = 0;
	g_debugFragments.numTextLines = 0;
}

void fcDebugRelease(FcAllocator* pAllocCallbacks)
{
	FUR_FREE(g_debugFragments.linesData, pAllocCallbacks);
	FUR_FREE(g_debugFragments.trianglesData, pAllocCallbacks);
	FUR_FREE(g_debugFragments.textLocationData, pAllocCallbacks);
	FUR_FREE(g_debugFragments.textRangeData, pAllocCallbacks);
	FUR_FREE(g_debugFragments.textCharactersData, pAllocCallbacks);
	FUR_FREE(g_debugFragments.textScaleData, pAllocCallbacks);
	FUR_FREE(g_debugFragments.rectData, pAllocCallbacks);
	
	memset(&g_debugFragments, 0, sizeof(FcDebugFragments));
}

void fcDebugLine(const f32 start[3], const f32 end[3], const f32 color[4])
{
	FUR_ASSERT(g_debugFragments.numLines < FC_DEBUG_FRAGMENTS_LINES_CAPACITY);
	
	u32 idx = g_debugFragments.numLines * 2 * FC_DEBUG_VERTEX_NUM_FLOATS;
	
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

u32 fcDebugLineNumTotalVertices(void)
{
	return 2 * FC_DEBUG_FRAGMENTS_LINES_CAPACITY;
}

u32 fcDebugLineBufferSize(void)
{
	return FC_DEBUG_LINE_SIZE * FC_DEBUG_FRAGMENTS_LINES_CAPACITY;
}

void fcDebugBuffersLock(void)
{
	// todo: implement when multithreading comes in
}

void fcDebugBuffersGet(FcDebugBuffersDesc* outDesc)
{
	outDesc->linesData = g_debugFragments.linesData;
	outDesc->linesDataSize = fcDebugLineCurrentLinesSize();
	
	outDesc->trianglesData = g_debugFragments.trianglesData;
	outDesc->trianglesDataSize = fcDebugTrianglesCurrentTrianglesSize();
	
	outDesc->textLocationData = g_debugFragments.textLocationData;
	outDesc->textCharactersData = g_debugFragments.textCharactersData;
	outDesc->textScaleData = g_debugFragments.textScaleData;
	outDesc->textRangeData = g_debugFragments.textRangeData;
	outDesc->textLinesCount = g_debugFragments.numTextLines;
	outDesc->textCharacterDataSize = g_debugFragments.numTextCharacters;
	
	outDesc->rectsData = g_debugFragments.rectData;
	outDesc->rectsDataSize = fcDebugRectsCurrentDataSize();
}

const f32* fcDebugLineGetData(void)
{
	return g_debugFragments.linesData;
}

u32 fcDebugLineCurrentNumlines(void)
{
	return g_debugFragments.numLines;
}

u32 fcDebugLineCurrentLinesSize(void)
{
	return FC_DEBUG_LINE_SIZE * fcDebugLineCurrentNumlines();
}

const f32* fcDebugTrianglesGetData(void)
{
	return g_debugFragments.trianglesData;
}

u32 fcDebugTrianglesCurrentNumTriangles(void)
{
	return g_debugFragments.numTriangles;
}

u32 fcDebugTrianglesCurrentTrianglesSize(void)
{
	return FC_DEBUG_TRIANGLE_SIZE * fcDebugTrianglesCurrentNumTriangles();
}

u32 fcDebugTrianglesNumTotalVertices(void)
{
	return 3 * FC_DEBUG_FRAGMENTS_TRIANGLES_CAPACITY;
}

u32 fcDebugRectsCurrentNumRects(void)
{
	return g_debugFragments.numRects;
}

u32 fcDebugRectsBufferSize(void)
{
	return fcDebugRectsNumTotalVertices() * FC_DEBUG_RECTS_VERTEX_NUM_FLOATS * sizeof(f32);
}

u32 fcDebugRectsCurrentNumVertices(void)
{
	return fcDebugRectsCurrentNumRects() * FC_DEBUG_RECT_NUM_VERTICES;
}

u32 fcDebugRectsCurrentDataSize(void)
{
	return fcDebugRectsCurrentNumVertices() * FC_DEBUG_RECTS_VERTEX_NUM_FLOATS * sizeof(f32);
}

u32 fcDebugRectsNumTotalVertices(void)
{
	return FC_DEBUG_RECT_NUM_VERTICES * FC_DEBUG_FRAGMENTS_RECTS_CAPACITY;
}

u32 fcDebugRectsNumFloatsPerVertex(void)
{
	return FC_DEBUG_RECTS_VERTEX_NUM_FLOATS;
}

void fcDebugBuffersClear()
{
	g_debugFragments.numLines = 0;
	g_debugFragments.numTriangles = 0;
	g_debugFragments.numTextCharacters = 0;
	g_debugFragments.numTextLines = 0;
	g_debugFragments.numRects = 0;
}

void fcDebugBuffersUnlock(void)
{
	// todo: implement when multithreading comes in
}

void fcDebugTriangle(const f32 pointA[3], const f32 pointB[3], const f32 pointC[3], const f32 color[4])
{
	FUR_ASSERT(g_debugFragments.numTriangles < FC_DEBUG_FRAGMENTS_TRIANGLES_CAPACITY);
	
	u32 idx = g_debugFragments.numTriangles * 3 * FC_DEBUG_VERTEX_NUM_FLOATS;
	
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

u32 fcDebugTriangleBufferSize(void)
{
	return FC_DEBUG_TRIANGLE_SIZE * FC_DEBUG_FRAGMENTS_TRIANGLES_CAPACITY;
}

void fcDebugBoxWire(const f32 center[3], const f32 extent[3], const f32 color[4])
{
	// convert to min/max bounding box
	f32 a[3];
	f32 b[3];
	
	for(u32 i=0; i<3; ++i)
	{
		a[i] = center[i] - extent[i];
		b[i] = center[i] + extent[i];
	}
	
	// four x-axis aligned lines
	{
		f32 start[3] = {a[0], a[1], a[2]};
		f32 end[3] = {b[0], a[1], a[2]};
		fcDebugLine(start, end, color);
	}
	
	{
		f32 start[3] = {a[0], b[1], a[2]};
		f32 end[3] = {b[0], b[1], a[2]};
		fcDebugLine(start, end, color);
	}
	
	{
		f32 start[3] = {a[0], a[1], b[2]};
		f32 end[3] = {b[0], a[1], b[2]};
		fcDebugLine(start, end, color);
	}
	
	{
		f32 start[3] = {a[0], b[1], b[2]};
		f32 end[3] = {b[0], b[1], b[2]};
		fcDebugLine(start, end, color);
	}
	
	// four y-axis aligned lines
	{
		f32 start[3] = {a[0], a[1], a[2]};
		f32 end[3] = {a[0], b[1], a[2]};
		fcDebugLine(start, end, color);
	}
	
	{
		f32 start[3] = {b[0], a[1], a[2]};
		f32 end[3] = {b[0], b[1], a[2]};
		fcDebugLine(start, end, color);
	}
	
	{
		f32 start[3] = {a[0], a[1], b[2]};
		f32 end[3] = {a[0], b[1], b[2]};
		fcDebugLine(start, end, color);
	}
	
	{
		f32 start[3] = {b[0], a[1], b[2]};
		f32 end[3] = {b[0], b[1], b[2]};
		fcDebugLine(start, end, color);
	}
	
	// four z-axis aligned lines
	{
		f32 start[3] = {a[0], a[1], a[2]};
		f32 end[3] = {a[0], a[1], b[2]};
		fcDebugLine(start, end, color);
	}
	
	{
		f32 start[3] = {b[0], a[1], a[2]};
		f32 end[3] = {b[0], a[1], b[2]};
		fcDebugLine(start, end, color);
	}
	
	{
		f32 start[3] = {a[0], b[1], a[2]};
		f32 end[3] = {a[0], b[1], b[2]};
		fcDebugLine(start, end, color);
	}
	
	{
		f32 start[3] = {b[0], b[1], a[2]};
		f32 end[3] = {b[0], b[1], b[2]};
		fcDebugLine(start, end, color);
	}
}

void fcDebugPlane(const f32 center[3], const f32 halfLength, const f32 color[4])
{
	const f32 planeA[3] = {-halfLength + center[0], -halfLength + center[1], center[2]};
	const f32 planeB[3] = {-halfLength + center[0], halfLength + center[1], center[2]};
	const f32 planeC[3] = {halfLength + center[0], halfLength + center[1], center[2]};
	const f32 planeD[3] = {halfLength + center[0], -halfLength + center[1], center[2]};
	
	fcDebugTriangle(planeC, planeB, planeA, color);
	fcDebugTriangle(planeA, planeD, planeC, color);
}

void fcDebugHelperCross(const f32 a[3], const f32 b[3], f32 c[3])
{
	c[0] = a[1] * b[2] - a[2] * b[1];
	c[1] = a[0] * b[2] - a[2] * b[0];
	c[2] = a[0] * b[1] - a[1] * b[0];
}

void fcDebugHelperPerpendicular(f32 v1[3], f32 v2[3])
{
	// Find the index of the smallest element in v1
	int i = (abs(v1[0]) < abs(v1[1])) ? 0 : 1;
	i = (abs(v1[i]) < abs(v1[2])) ? i : 2;

	// Compute the perpendicular vector by setting the smallest element to zero
	v2[0] = v2[1] = v2[2] = 0.0f;
	v2[(i + 1) % 3] = v1[(i + 2) % 3];
	v2[(i + 2) % 3] = -v1[(i + 1) % 3];
}

void fcDebugCircle(const f32 center[3], const f32 radius, const f32 up[3], const f32 color[4])
{
	f32 right[3] = { 0 };
	fcDebugHelperPerpendicular(up, right);

	f32 left[3] = { 0 };
	fcDebugHelperCross(up, right, left);

	f32 v_start[3] = {
		center[0] + radius * (cosf(0.0f) * right[0] + sinf(0.0f) * left[0]),
		center[1] + radius * (cosf(0.0f) * right[1] + sinf(0.0f) * left[1]),
		center[2] + radius * (cosf(0.0f) * right[2] + sinf(0.0f) * left[2])
	};

	const u32 segments = radius * 100.0f;
	const f32 segment_angle = 2.0f * M_PI / segments;

	for (i32 i = 1; i <= segments; i++)
	{
		const f32 v_end[3] = {
			center[0] + radius * (cosf(segment_angle * i) * right[0] + sinf(segment_angle * i) * left[0]),
			center[1] + radius * (cosf(segment_angle * i) * right[1] + sinf(segment_angle * i) * left[1]),
			center[2] + radius * (cosf(segment_angle * i) * right[2] + sinf(segment_angle * i) * left[2])
		};

		fcDebugLine(v_start, v_end, color);

		v_start[0] = v_end[0];
		v_start[1] = v_end[1];
		v_start[2] = v_end[2];
	}
}

void fcDebugSphereWire(const f32 center[3], const f32 radius, const f32 color[4])
{
	f32 mat[3][3] = {
		{ 1.0f, 0.0f, 0.0f },
		{ 0.0f, 1.0f, 0.0f },
		{ 0.0f, 0.0f, 1.0f }
	};

	for (i32 k = 0; k < 3; ++k)
	{
		f32 v_start[3] = {
		center[0] + radius * (cosf(0.0f) * mat[k%3][0] + sinf(0.0f) * mat[(k+1)%3][0]),
		center[1] + radius * (cosf(0.0f) * mat[k%3][1] + sinf(0.0f) * mat[(k+1)%3][1]),
		center[2] + radius * (cosf(0.0f) * mat[k%3][2] + sinf(0.0f) * mat[(k+1)%3][2])
		};

		const u32 segments = radius * 100.0f;
		const f32 segment_angle = 2.0f * M_PI / segments;

		for (i32 i = 1; i <= segments; i++)
		{
			const f32 v_end[3] = {
				center[0] + radius * (cosf(segment_angle * i) * mat[k%3][0] + sinf(segment_angle * i) * mat[(k+1)%3][0]),
				center[1] + radius * (cosf(segment_angle * i) * mat[k%3][1] + sinf(segment_angle * i) * mat[(k+1)%3][1]),
				center[2] + radius * (cosf(segment_angle * i) * mat[k%3][2] + sinf(segment_angle * i) * mat[(k+1)%3][2])
			};

			fcDebugLine(v_start, v_end, color);

			v_start[0] = v_end[0];
			v_start[1] = v_end[1];
			v_start[2] = v_end[2];
		}
	}
}

void fcDebugText(f32 x, f32 y, const char* txt, const f32 color[4], f32 scale)
{
	const u32 length = (u32)strlen(txt);
	const u32 idx = g_debugFragments.numTextLines;

	FUR_ASSERT(idx < FC_DEBUG_FRAGMENTS_TEXT_LINES_CAPACITY);
	FUR_ASSERT(g_debugFragments.numTextCharacters + length + 1 < FC_DEBUG_FRAGMENTS_TEXT_CHARACTERS_CAPACITY);
	
	const u32 offsetFloat = idx * FC_DEBUG_TEXT_LOCATION_DATA_NUM_FLOATS;
	const u32 offsetRange = idx * FC_DEBUG_TEXT_LOCATION_DATA_NUM_RANGE;
	g_debugFragments.numTextLines += 1;

	f32* dataLocation = g_debugFragments.textLocationData + offsetFloat;
	dataLocation[0] = x;
	dataLocation[1] = -y;
	dataLocation[2] = color[0];
	dataLocation[3] = color[1];
	dataLocation[4] = color[2];
	
	const u32 offsetCharacters = g_debugFragments.numTextCharacters;
	g_debugFragments.numTextCharacters += length + 1;
	
	char* dataCharacters = g_debugFragments.textCharactersData + offsetCharacters;
	memcpy(dataCharacters, txt, length);
	dataCharacters[length] = '\0';
	
	u32* dataRange = g_debugFragments.textRangeData + offsetRange;
	dataRange[0] = offsetCharacters;
	dataRange[1] = length;

	f32* dataScale = g_debugFragments.textScaleData + idx;
	*dataScale = scale;
}

void fcDebugGetScreenInfo(FcDebugScreenInfo* info)
{
	*info = g_debugFragments.screenInfo;
}

void fcDebugSetScreenInfo(const FcDebugScreenInfo* info)
{
	g_debugFragments.screenInfo = *info;
}

void fcDebugApplyAnchor(f32* x, f32* y, FcDebugScreenAnchors anchor)
{
	FcDebugScreenInfo s = g_debugFragments.screenInfo;

	switch (anchor)
	{
	case FC_DBG_ANCHOR_LEFT_UP_CORNER:
		// += 0.0f
		break;
	case FC_DBG_ANCHOR_RIGHT_UP_CORNER:
		*x += s.width;
		//*y += 0.0f;
		break;
	case FC_DBG_ANCHOR_LEFT_BOTTOM_CORNER:
		//*x += 0.0f;
		*y += s.height;
		break;
	case FC_DBG_ANCHOR_RIGHT_BOTTOM_CORNER:
		*x += s.width;
		*y += s.height;
		break;
	case FC_DBG_ANCHOR_CENTER:
		*x += s.width / 2.0f;
		*y += s.height / 2.0f;
		break;
	default:
		break;
	}
}

void fcDebugRect(f32 x, f32 y, f32 width, f32 height, const f32 color[4])
{
	FUR_ASSERT(g_debugFragments.numRects < FC_DEBUG_FRAGMENTS_RECTS_CAPACITY);
	
	const u32 idx = g_debugFragments.numRects * FC_DEBUG_RECT_NUM_VERTICES * FC_DEBUG_RECTS_VERTEX_NUM_FLOATS;
	g_debugFragments.numRects += 1;
	
	f32* vertices = g_debugFragments.rectData + idx;
	const u32 vertexStride = FC_DEBUG_RECTS_VERTEX_NUM_FLOATS;
	
	//  / Z
	// o----X
	// |
	// |
	// Y
	
	// top-right CCW triangle
	// O---
	//  \  |
	//   \ |
	//    \|
	//
	{
		f32* pos = vertices;
		f32* vcolor = vertices + 2;
		
		pos[0] = x;
		pos[1] = -y;
		vcolor[0] = color[0];
		vcolor[1] = color[1];
		vcolor[2] = color[2];
		vcolor[3] = color[3];
	}
	
	vertices += vertexStride;
	
	//  ---
	//  \  |
	//   \ |
	//    \|
	//     O
	{
		f32* pos = vertices;
		f32* vcolor = vertices + 2;
		
		pos[0] = x + width;
		pos[1] = -y - height;
		vcolor[0] = color[0];
		vcolor[1] = color[1];
		vcolor[2] = color[2];
		vcolor[3] = color[3];
	}
	
	vertices += vertexStride;
	
	//  ---O
	//  \  |
	//   \ |
	//    \|
	//
	{
		f32* pos = vertices;
		f32* vcolor = vertices + 2;
		
		pos[0] = x + width;
		pos[1] = -y;
		vcolor[0] = color[0];
		vcolor[1] = color[1];
		vcolor[2] = color[2];
		vcolor[3] = color[3];
	}
	
	vertices += vertexStride;
	
	// bottom-left CCW triangle
	// O
	// |\
	// | \
	// |  \
	//  ---
	{
		// same as vertex 0
		f32* pos = vertices;
		f32* vcolor = vertices + 2;
		
		pos[0] = x;
		pos[1] = -y;
		vcolor[0] = color[0];
		vcolor[1] = color[1];
		vcolor[2] = color[2];
		vcolor[3] = color[3];
	}
	
	vertices += vertexStride;
	
	//
	// |\
	// | \
	// |  \
	// O---
	{
		f32* pos = vertices;
		f32* vcolor = vertices + 2;
		
		pos[0] = x;
		pos[1] = -y - height;
		vcolor[0] = color[0];
		vcolor[1] = color[1];
		vcolor[2] = color[2];
		vcolor[3] = color[3];
	}
	
	vertices += vertexStride;
	
	//
	// |\
	// | \
	// |  \
	//  ---O
	{
		// same as vertex 1
		f32* pos = vertices;
		f32* vcolor = vertices + 2;
		
		pos[0] = x + width;
		pos[1] = -y - height;
		vcolor[0] = color[0];
		vcolor[1] = color[1];
		vcolor[2] = color[2];
		vcolor[3] = color[3];
	}
	
	vertices += vertexStride;
}

u32 fcDebugTextCharactersCapacity(void)
{
	return FC_DEBUG_FRAGMENTS_TEXT_CHARACTERS_CAPACITY;
}

u32 fcDebugTextNumTotalCharacter(void)
{
	return FC_DEBUG_FRAGMENTS_TEXT_CHARACTERS_CAPACITY;
}
