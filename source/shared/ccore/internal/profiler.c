/* Copyright (c) 2016-2020 Furball Cat */

#include <sys/time.h>
#include <stdio.h>

#include "profiler.h"
#include "memory.h"
#include "furAssert.h"
#include "debugDraw.h"

#define FC_PROFILER_MAX_SCOPES 4096
#define FC_PROFILER_MAX_FRAMES 5
#define FC_PROFILER_FRAMES_DRAWN 4

typedef struct fc_profiler_t
{
	uint32_t currentNumScopes;
	uint32_t currentDepth;
	fc_profiler_scope_t* scopes[FC_PROFILER_MAX_FRAMES];	// one frame is for pause mode
	uint32_t frameNumScopes[FC_PROFILER_MAX_FRAMES];
	uint32_t frameStartTimes[FC_PROFILER_MAX_FRAMES];
	uint8_t currentFrame;
	uint8_t pausedOnFrame;
	bool debugDraw;
	float zoom;
	float pan;
} fc_profiler_t;

fc_profiler_t g_profiler;

void fc_profiler_init(fc_alloc_callbacks_t* pAllocCallbacks)
{
	g_profiler.zoom = 50.0f;
	
	for(uint32_t i=0; i<FC_PROFILER_MAX_FRAMES; ++i)
	{
		g_profiler.scopes[i] = FUR_ALLOC_ARRAY_AND_ZERO(fc_profiler_scope_t, FC_PROFILER_MAX_SCOPES, 0, FC_MEMORY_SCOPE_PROFILER, pAllocCallbacks);
	}
}

void fc_profiler_release(fc_alloc_callbacks_t* pAllocCallbacks)
{
	for(uint32_t i=0; i<FC_PROFILER_MAX_FRAMES; ++i)
	{
		FUR_FREE(g_profiler.scopes[i], pAllocCallbacks);
	}
}

fc_profiler_scope_t* fc_profiler_scope_begin(const char* name)
{
	FUR_ASSERT(g_profiler.currentNumScopes < FC_PROFILER_MAX_SCOPES);
	
	const uint32_t idx = g_profiler.currentNumScopes;
	g_profiler.currentNumScopes += 1;
	const uint32_t depth = g_profiler.currentDepth;
	g_profiler.currentDepth += 1;
	
	struct timeval time = {};
	gettimeofday(&time, NULL);
	
	fc_profiler_scope_t scope = {};
	scope.name = name;
	scope.depth = depth;
	scope.startTime = time.tv_usec;
	fc_profiler_scope_t* pScope = &g_profiler.scopes[g_profiler.currentFrame][idx];
	*pScope = scope;
	
	return pScope;
}

void fc_profiler_scope_end(fc_profiler_scope_t* scope)
{
	g_profiler.currentDepth -= 1;
	
	struct timeval time = {};
	gettimeofday(&time, NULL);
	
	scope->stopTime = time.tv_usec;
}

void fc_profiler_start_frame(void)
{
	// tick profiler
	{
		FUR_ASSERT(g_profiler.currentDepth == 0); // make sure all scopes were closed

		struct timeval frameStartTime = {};
		gettimeofday(&frameStartTime, NULL);

		g_profiler.frameStartTimes[g_profiler.currentFrame] = frameStartTime.tv_usec;
		g_profiler.currentNumScopes = 0;
	}
	
	// draw last frame
	if(g_profiler.debugDraw)
	{
		float x = -1000.0f + g_profiler.pan * g_profiler.zoom;
		float y = 600.0f;
		
		float color[4] = {0.0f, 0.0f, 0.4f, 0.6f};
		const float white[4] = FUR_COLOR_WHITE;
		const float blue[4] = {0.0f, 0.8f, 0.8f, 0.8f};
		const float green[4] = {0.0f, 0.8f, 0.0f, 0.8f};
		const float yellow[4] = {0.8f, 0.8f, 0.0f, 0.8f};
		
		fc_dbg_rect(x, y, 1, 1200.0f, blue);
		fc_dbg_rect(x + 16.6f * g_profiler.zoom, y, 1, 1200.0f, green);
		fc_dbg_rect(x + 33.3f * g_profiler.zoom, y, 1, 1200.0f, yellow);
		
		// start drawing from the oldest frame (which is current + 1, as we rotate frames)
		uint32_t frameIdx = (g_profiler.pausedOnFrame + 1) % FC_PROFILER_FRAMES_DRAWN;
		const uint32_t firstFrameStartTime = g_profiler.frameStartTimes[frameIdx];
		
		for(uint32_t f=0; f<FC_PROFILER_FRAMES_DRAWN-1; ++f, frameIdx = (frameIdx + 1) % FC_PROFILER_FRAMES_DRAWN)
		{
			fc_profiler_scope_t* scopes = g_profiler.scopes[frameIdx];
			const uint32_t numScopes = g_profiler.frameNumScopes[frameIdx];
			
			for(uint32_t i=0; i<numScopes; ++i)
			{
				fc_profiler_scope_t scope = scopes[i];
				
				color[0] = ((uint8_t)scope.name[0]) / 255.0f;
				color[1] = ((uint8_t)scope.name[1]) / 255.0f;
				color[2] = ((uint8_t)scope.name[2]) / 255.0f;
				
				const float startTime_ms = (scope.startTime - firstFrameStartTime) / 1000.0f;
				const float stopTime_ms = (scope.stopTime - firstFrameStartTime) / 1000.0f;
				const float x_offset = startTime_ms * g_profiler.zoom;
				const float width = stopTime_ms * g_profiler.zoom - x_offset;
				
				// draw scope rectangle
				if(width > 4.0f)
				{
					fc_dbg_rect(x + x_offset, y - 30.0 * scope.depth, width, 28.0, color);
				}
				
				// draw scope name
				if(width > 150)
				{
					char txt[256];
					sprintf(txt, "%s (%1.3fms)", scope.name, stopTime_ms - startTime_ms);
					fc_dbg_text(x + x_offset, y - 30.0 * scope.depth, txt, white);
				}
			}
		}
	}
}

void fc_profiler_end_frame(void)
{
	g_profiler.frameNumScopes[g_profiler.currentFrame] = g_profiler.currentNumScopes;
	
	// if not profiler pause, then rotate frames
	if(g_profiler.currentFrame != FC_PROFILER_FRAMES_DRAWN)
	{
		g_profiler.currentFrame = (g_profiler.currentFrame + 1) % FC_PROFILER_FRAMES_DRAWN;
		g_profiler.pausedOnFrame = g_profiler.currentFrame;
	}
}

void fc_profiler_toggle_draw(void)
{
	g_profiler.debugDraw = !g_profiler.debugDraw;
}

void fc_profiler_toggle_pause(void)
{
	const uint32_t pausingModeFrame = FC_PROFILER_MAX_FRAMES - 1;
	if(g_profiler.currentFrame == pausingModeFrame)
	{
		g_profiler.currentFrame = g_profiler.pausedOnFrame;
	}
	else
	{
		g_profiler.pausedOnFrame = g_profiler.currentFrame;
		g_profiler.currentFrame = pausingModeFrame;
	}
}

bool fc_profiler_is_draw_on(void)
{
	return g_profiler.debugDraw;
}

void fc_profiler_zoom_and_pan_delta(float zoomDelta, float panDelta)
{
	g_profiler.zoom += zoomDelta * (g_profiler.zoom / 50.0f);
	g_profiler.pan += panDelta / (g_profiler.zoom / 10.0f);
}
