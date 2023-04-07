/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus
	
#include "ccore/types.h"
#include "api.h"
	
typedef struct FcAllocator FcAllocator;
	
CCORE_API void fcDebugInit(FcAllocator* allocator);
CCORE_API void fcDebugRelease(FcAllocator* allocator);
	
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
CCORE_API void fcDebugLine(const f32 start[3], const f32 end[3], const f32 color[4]);
CCORE_API void fcDebugTriangle(const f32 pointA[3], const f32 pointB[3], const f32 pointC[3], const f32 color[4]);
CCORE_API void fcDebugBoxWire(const f32 center[3], const f32 extent[3], const f32 color[4]);
CCORE_API void fcDebugPlane(const f32 center[3], const f32 halfLength, const f32 color[4]);
CCORE_API void fcDebugCircle(const f32 center[3], const f32 radius, const f32 up[3], const f32 color[4]);
CCORE_API void fcDebugSphereWire(const f32 center[3], const f32 radius, const f32 color[4]);
	
// use sprintf( txt, "blah blah %u", value ) and pass txt to the function (txt is char txt[32] or any other reasonable size)
CCORE_API void fcDebugText(f32 x, f32 y, const char* txt, const f32 color[4], f32 scale);

typedef struct FcDebugScreenInfo
{
	f32 width;
	f32 height;
} FcDebugScreenInfo;

// set/get info about screen corners and size
CCORE_API void fcDebugGetScreenInfo(FcDebugScreenInfo* info);
CCORE_API void fcDebugSetScreenInfo(const FcDebugScreenInfo* info);	// this should be called on screen resize

typedef enum FcDebugScreenAnchors
{
	FC_DBG_ANCHOR_LEFT_UP_CORNER = 0,
	FC_DBG_ANCHOR_RIGHT_UP_CORNER,
	FC_DBG_ANCHOR_LEFT_BOTTOM_CORNER,
	FC_DBG_ANCHOR_RIGHT_BOTTOM_CORNER,
	FC_DBG_ANCHOR_CENTER,
} FcDebugScreenAnchors;

// call this to set init x and y for 2D drawing aligned to anchors
CCORE_API void fcDebugApplyAnchor(f32* x, f32* y, FcDebugScreenAnchors anchor);
static inline f32 fcDebugGetTextLineHeight(f32 scale) { return 28.0f * scale; }

// draw flat 2D rectancle (drawn under text), use it for simple debug UI rectancles
CCORE_API void fcDebugRect(f32 x, f32 y, f32 width, f32 height, const f32 color[4]);
	
// sizes of each debug fragment buffers
CCORE_API u32 fcDebugLineBufferSize(void);
CCORE_API u32 fcDebugTriangleBufferSize(void);
CCORE_API u32 fcDebugTextCharactersCapacity(void);
CCORE_API u32 fcDebugTextNumTotalCharacter(void);
	
// call these functions in given order when retrieving data for current frame
CCORE_API void fcDebugBuffersLock(void);
	
CCORE_API const f32* fcDebugLineGetData(void);
CCORE_API u32 fcDebugLineCurrentNumlines(void);
CCORE_API u32 fcDebugLineCurrentLinesSize(void);
CCORE_API u32 fcDebugLineNumTotalVertices(void);
	
CCORE_API const f32* fcDebugTrianglesGetData(void);
CCORE_API u32 fcDebugTrianglesCurrentNumTriangles(void);
CCORE_API u32 fcDebugTrianglesCurrentTrianglesSize(void);
CCORE_API u32 fcDebugTrianglesNumTotalVertices(void);

CCORE_API u32 fcDebugRectsCurrentNumRects(void);
CCORE_API u32 fcDebugRectsBufferSize(void);
CCORE_API u32 fcDebugRectsCurrentNumVertices(void);
CCORE_API u32 fcDebugRectsCurrentDataSize(void);
CCORE_API u32 fcDebugRectsNumTotalVertices(void);
CCORE_API u32 fcDebugRectsNumFloatsPerVertex(void);

typedef struct FcDebugBuffersDesc
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
} FcDebugBuffersDesc;
	
CCORE_API void fcDebugBuffersGet(FcDebugBuffersDesc* outDesc);
	
CCORE_API void fcDebugBuffersClear(void);
	
CCORE_API void fcDebugBuffersUnlock(void);
	
#ifdef __cplusplus
}
#endif // __cplusplus
