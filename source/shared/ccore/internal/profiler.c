/* Copyright (c) 2016-2020 Furball Cat */

#include <stdio.h>

#include "profiler.h"
#include "memory.h"
#include "furAssert.h"
#include "debugDraw.h"
#include "jobs.h"

#define FC_PROFILER_MAX_SCOPES 8192
#define FC_PROFILER_MAX_FRAMES 5
#define FC_PROFILER_FRAMES_DRAWN 4
#define FC_PROFILER_MAX_CONTENTION_SCOPES 8192
#define FC_PROFILER_INIT_ZOOM 35.0f

typedef struct FcContentionScope
{
	const char* name;
	FcTimeval startTime;
	FcTimeval endTime;
} FcContentionScope;

typedef struct FcProfilerThreadInfo
{
	u32 currentNumScopes;
	u32 currentDepth;
	FcProfilerScope* current;
	FcProfilerScope* scopes[FC_PROFILER_MAX_FRAMES];	// one frame is for pause mode
	u32 frameNumScopes[FC_PROFILER_MAX_FRAMES];
	
	u32 frameNumContentionScopes[FC_PROFILER_MAX_FRAMES];
	FcContentionScope* contentionScopes[FC_PROFILER_MAX_FRAMES];
	u32 currentNumContentionScopes;
	
	FcTimeval tempContentionStartTime;	// used on enter contention
	
} FcProfilerThreadInfo;

typedef struct FcProfiler
{
	FcProfilerThreadInfo* threads;
	i32 numThreads;	// includign main thread, use thread index to access threads array
	
	FcTimeval frameStartTimes[FC_PROFILER_MAX_FRAMES];
	
	u8 currentFrame;
	u8 pausedOnFrame;
	bool debugDraw;
	f32 zoom;
	f32 pan;
} FcProfiler;

FcProfiler g_profiler;

void fcProfilerInit(FcAllocator* pAllocCallbacks)
{
	g_profiler.zoom = FC_PROFILER_INIT_ZOOM;
	
	g_profiler.numThreads = fcJobSystemNumMaxThreads();
	
	g_profiler.threads = FUR_ALLOC_ARRAY_AND_ZERO(FcProfilerThreadInfo, g_profiler.numThreads, 0, FC_MEMORY_SCOPE_PROFILER, pAllocCallbacks);
	
	for(i32 t=0; t<g_profiler.numThreads; ++t)
	{
		for(u32 i=0; i<FC_PROFILER_MAX_FRAMES; ++i)
		{
			g_profiler.threads[t].scopes[i] = FUR_ALLOC_ARRAY_AND_ZERO(FcProfilerScope, FC_PROFILER_MAX_SCOPES, 0, FC_MEMORY_SCOPE_PROFILER, pAllocCallbacks);
			g_profiler.threads[t].contentionScopes[i] = FUR_ALLOC_ARRAY_AND_ZERO(FcContentionScope, FC_PROFILER_MAX_CONTENTION_SCOPES, 0, FC_MEMORY_SCOPE_PROFILER, pAllocCallbacks);
		}
	}
}

void fcProfilerRelease(FcAllocator* pAllocCallbacks)
{
	for(i32 t=0; t<g_profiler.numThreads; ++t)
	{
		for(u32 i=0; i<FC_PROFILER_MAX_FRAMES; ++i)
		{
			FUR_FREE(g_profiler.threads[t].scopes[i], pAllocCallbacks);
			FUR_FREE(g_profiler.threads[t].contentionScopes[i], pAllocCallbacks);
		}
	}
	
	FUR_FREE(g_profiler.threads, pAllocCallbacks);
}

FcProfilerScope* fcProfilerScopeBegin(const char* name)
{
	const i32 threadIndex = fcJobSystemGetThisThreadIndex();
	FcProfilerThreadInfo* thread = &g_profiler.threads[threadIndex];
	
	FUR_ASSERT(thread->currentNumScopes < FC_PROFILER_MAX_SCOPES);
	
	const u32 idx = thread->currentNumScopes;
	thread->currentNumScopes += 1;
	const u32 depth = thread->currentDepth;
	thread->currentDepth += 1;
	
	FcProfilerScope scope = {0};
	scope.name = name;
	scope.depth = depth;
	fcTimevalNow(&scope.startTime);
	FcProfilerScope* pScope = &thread->scopes[g_profiler.currentFrame][idx];
	*pScope = scope;
	
	// keep track of scope stack because of fibers (so we can store and load stack)
	FcProfilerScope* parent = thread->current;
	thread->current = pScope;
	pScope->parent = parent;
	
	return pScope;
}

void fcProfilerScopeEnd(FcProfilerScope* scope)
{
	// note that scope might not belong to current thread in case of fiber switch
	
	const i32 threadIndex = fcJobSystemGetThisThreadIndex();
	FcProfilerThreadInfo* thread = &g_profiler.threads[threadIndex];
	
	thread->currentDepth -= 1;
	
	fcTimevalNow(&scope->stopTime);
	scope->threadID = (int16_t)threadIndex;
	
	thread->current = scope->parent;
}

void fcProfilerPause(void)
{
	const u32 pausingModeFrame = FC_PROFILER_MAX_FRAMES - 1;
	if(g_profiler.currentFrame != pausingModeFrame)
	{
		g_profiler.pausedOnFrame = g_profiler.currentFrame;
		g_profiler.currentFrame = pausingModeFrame;
	}
}

void fcProfilerStartFrame(void)
{
	FUR_ASSERT(fcJobSystemIsMainThread());
	
	// tick profiler
	{
		FcTimeval frameStartTime = {0};
		fcTimevalNow(&frameStartTime);

		g_profiler.frameStartTimes[g_profiler.currentFrame] = frameStartTime;
		
		for(i32 t=0; t<g_profiler.numThreads; ++t)
		{
			FUR_ASSERT(g_profiler.threads[t].currentDepth == 0); // make sure all scopes were closed
			
			g_profiler.threads[t].currentNumScopes = 0;
			g_profiler.threads[t].currentNumContentionScopes = 0;
		}
	}
	
	// draw last frame
	if(g_profiler.debugDraw)
	{
		const f32 text_scale = 0.5f;
		const f32 margin = 20.0f;
		const f32 line_height = fcDebugGetTextLineHeight(text_scale);
		const f32 core_rect_height = 7 * line_height;

		FcDebugScreenInfo screen = { 0 };
		fcDebugGetScreenInfo(&screen);

		const f32 offset_to_bottom = screen.height - 2.0f * margin;
		
		f32 x = 120.0f + margin + g_profiler.pan * g_profiler.zoom - screen.width * 0.15f * (g_profiler.zoom - FC_PROFILER_INIT_ZOOM) / FC_PROFILER_INIT_ZOOM;
		f32 y = margin;

		fcDebugApplyAnchor(&x, &y, FC_DBG_ANCHOR_LEFT_UP_CORNER);
		
		f32 color[4] = {0.0f, 0.0f, 0.4f, 1.0f};
		const f32 white[4] = FUR_COLOR_WHITE;
		const f32 blue[4] = {0.0f, 0.0f, 0.4f, 0.6f};
		const f32 green[4] = {0.0f, 0.4f, 0.0f, 0.6f};
		const f32 red[4] = {0.4f, 0.0f, 0.0f, 0.6f};
		const f32 yellow[4] = {0.8f, 0.8f, 0.0f, 0.8f};
		const f32 grey[4] = {0.6f, 0.6f, 0.6f, 1.0f};
		const f32 dark_grey[4] = { 0.2f, 0.2f, 0.2f, 0.8f };

		fcDebugRect(0.0f, 0.0f, screen.width, screen.height, dark_grey);
		
		// lines for ms
		{
			char txt[8];
			
			for(i32 i=0; i<60; ++i)
			{
				const bool isRound = (i % 5 == 0);
				
				// with big zoom-out, display only round numbers (every 5 ms)
				if(!isRound && g_profiler.zoom < 80.0f)
					continue;
				
				i32 x_offset = i * g_profiler.zoom;
				fcDebugRect(x + x_offset, y, isRound ? 2 : 1, offset_to_bottom, white);
				
				sprintf(txt, "%i ms", i);
				fcDebugText(x + x_offset + 4, y + offset_to_bottom - line_height, txt, white, text_scale);
			}
		}
		
		// areas of FPS
		fcDebugRect(x, y, 16.6666f * g_profiler.zoom, offset_to_bottom, green);	// 60 FPS
		fcDebugRect(x + 16.6666f * g_profiler.zoom, y, 16.6666f * g_profiler.zoom, offset_to_bottom, blue);	// 30 FPS
		fcDebugRect(x + 33.3333f * g_profiler.zoom, y, screen.width - x, offset_to_bottom, red);	// less than 30 FPS
		
		// start drawing from the oldest frame (which is current + 1, as we rotate frames)
		u32 frameIdx = (g_profiler.pausedOnFrame + 1) % FC_PROFILER_FRAMES_DRAWN;
		const FcTimeval firstFrameStartTime = g_profiler.frameStartTimes[frameIdx];
		const f64 firstFrameStartTime_ms = firstFrameStartTime.sec * 1000.0 + firstFrameStartTime.usec / 1000.0;
		
		char coreTxt[16] = {0};
		
		for(i32 t=0; t<g_profiler.numThreads; ++t)
		{
			const FcProfilerThreadInfo* thread = &g_profiler.threads[t];
			
			f32 y = margin + t * core_rect_height;	// place thread lines in their own horizontal panels
			
			// line for each core/thread
			fcDebugRect(x - 120.0f, y, screen.width + 1000.0f - x, 1.0f, grey);
			sprintf(coreTxt, "Core %i", t);
			fcDebugText(x - 120.0f, y + 2.0f, coreTxt, white, 0.7f);
			
			for(u32 f=0; f<FC_PROFILER_FRAMES_DRAWN-1; ++f, frameIdx = (frameIdx + 1) % FC_PROFILER_FRAMES_DRAWN)
			{
				// draw instrumentation scopes
				{
					FcProfilerScope* scopes = thread->scopes[frameIdx];
					const u32 numScopes = thread->frameNumScopes[frameIdx];
					
					for(u32 i=0; i<numScopes; ++i)
					{
						FcProfilerScope scope = scopes[i];
						
						// scope could be closed by another thread in case of fiber switch
						y = margin + scope.threadID * core_rect_height;
						
						color[0] = ((u8)scope.name[0]) / 255.0f;
						color[1] = ((u8)scope.name[1]) / 255.0f;
						color[2] = ((u8)scope.name[2]) / 255.0f;
						
						const f32 startTime_ms = scope.startTime.sec * 1000.0 + scope.startTime.usec / 1000.0 - firstFrameStartTime_ms;
						const f32 stopTime_ms = scope.stopTime.sec * 1000.0 + scope.stopTime.usec / 1000.0 - firstFrameStartTime_ms;
						const f32 x_offset = startTime_ms * g_profiler.zoom;
						const f32 width = stopTime_ms * g_profiler.zoom - x_offset;
						
						// draw scope rectangle
						if(width > 4.0f)
						{
							fcDebugRect(x + x_offset, y + line_height * scope.depth, width, line_height, color);
						}
						
						// draw scope name
						if(width > 150)
						{
							char txt[256];
							const f32 elapsedTime_ms = stopTime_ms - startTime_ms;
							sprintf(txt, "%s (%1.3fms)", scope.name, elapsedTime_ms);
							fcDebugText(x + x_offset, y + line_height * scope.depth, txt, white, text_scale);
							
							/*if(elapsedTime_ms > 20.0f)
							{
								fcProfilerPause();
							}*/
						}
					}
				}
				
				// draw contention scopes
				{
					FcContentionScope* scopes = thread->contentionScopes[frameIdx];
					const u32 numScopes = thread->frameNumContentionScopes[frameIdx];
					
					for(u32 i=0; i<numScopes; ++i)
					{
						FcContentionScope scope = scopes[i];
						
						// scope could be closed by another thread in case of fiber switch
						y = margin + t * core_rect_height;
						
						const f32 startTime_ms = scope.startTime.sec * 1000.0 + scope.startTime.usec / 1000.0 - firstFrameStartTime_ms;
						const f32 stopTime_ms = scope.endTime.sec * 1000.0 + scope.endTime.usec / 1000.0 - firstFrameStartTime_ms;
						const f32 x_offset = startTime_ms * g_profiler.zoom;
						const f32 width = stopTime_ms * g_profiler.zoom - x_offset;
						
						if (x_offset < 0.0f)
							continue;

						// draw scope rectangle
						if(width > 4.0f)
						{
							fcDebugRect(x + x_offset, y - 10, width, 5.0, yellow);
						}
						
						// draw scope name
						if(width > 150)
						{
							char txt[256];
							const f32 elapsedTime_ms = stopTime_ms - startTime_ms;
							sprintf(txt, "%s (%1.3fms)", scope.name, elapsedTime_ms);
							fcDebugText(x + x_offset, y - line_height, txt, white, text_scale);
							
							/*if(elapsedTime_ms > 20.0f)
							{
								fcProfilerPause();
							}*/
						}
					}
				}
			}
		}
	}
}

void fcProfilerEndFrame(void)
{
	FUR_ASSERT(fcJobSystemIsMainThread());
	
	for(i32 t=0; t<g_profiler.numThreads; ++t)
	{
		g_profiler.threads[t].frameNumScopes[g_profiler.currentFrame] = g_profiler.threads[t].currentNumScopes;
		g_profiler.threads[t].frameNumContentionScopes[g_profiler.currentFrame] = g_profiler.threads[t].currentNumContentionScopes;
	}
	
	// if not profiler pause, then rotate frames
	if(g_profiler.currentFrame != FC_PROFILER_FRAMES_DRAWN)
	{
		g_profiler.currentFrame = (g_profiler.currentFrame + 1) % FC_PROFILER_FRAMES_DRAWN;
		g_profiler.pausedOnFrame = g_profiler.currentFrame;
	}
}

void fcProfilerToggleDraw(void)
{
	g_profiler.debugDraw = !g_profiler.debugDraw;
}

void fcProfilerTogglePause(void)
{
	const u32 pausingModeFrame = FC_PROFILER_MAX_FRAMES - 1;
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

bool fcProfilerIsDrawOn(void)
{
	return g_profiler.debugDraw;
}

void fcProfilerZoomAndPanDelta(f32 zoomDelta, f32 panDelta)
{
	g_profiler.zoom += zoomDelta * (g_profiler.zoom / 50.0f);
	g_profiler.pan += panDelta / (g_profiler.zoom / 10.0f);
}

uint64_t fcLogProfilerBegin(void)
{
	FcTimeval time = {0};
	fcTimevalNow(&time);
	
	return (uint64_t)time.sec * 1000000 + (uint64_t)time.usec;
}

void fcLogProfilerEnd(const char* scopeName, uint64_t startTime)
{
	FcTimeval time = {0};
	fcTimevalNow(&time);
	const uint64_t endTime = (uint64_t)time.sec * 1000000 + (uint64_t)time.usec;
	
	const f32 scopeTime = (f32)(endTime - startTime) / 1000.0f;
	
	printf("%s: %1.3fms\n", scopeName, scopeTime);
}

i32 fcProfilerStoreScopestack(FcProfilerScope* stack[32])
{
	const i32 threadIndex = fcJobSystemGetThisThreadIndex();
	FcProfilerThreadInfo* thread = &g_profiler.threads[threadIndex];
	const int8_t currFrame = g_profiler.currentFrame;
	FcProfilerScope* scopes = thread->scopes[currFrame];
	
	i32 numStack = 0;
	
	// create a copy of current callstack, and keep old scopes for the next callstack
	FcProfilerScope* scope = thread->current;
	FcProfilerScope* child = NULL;
	while(scope)
	{
		// finish this scope
		fcProfilerScopeEnd(scope);
		
		// copy it to new scope
		FUR_ASSERT(thread->currentNumScopes < FC_PROFILER_MAX_SCOPES);
		const u32 idx = thread->currentNumScopes;
		thread->currentNumScopes += 1;
		scopes[idx] = *scope;
		
		// fix pointer to parent for new child
		if(child)
			child->parent = &scopes[idx];
		
		// save scope
		stack[numStack] = scope;
		
		// continue
		child = &scopes[idx];
		scope = scope->parent;
		numStack += 1;
	}
	
	FUR_ASSERT(thread->current == NULL);
	FUR_ASSERT(thread->currentDepth == 0);
	
	return numStack;
}

void fcProfilerLoadScopestack(FcProfilerScope* stack[32], i32 numDepth)
{
	FUR_ASSERT(numDepth <= 32);
	
	// we will assume all scopes start at the same time after jumping back to fiber
	FcTimeval time = {0};
	fcTimevalNow(&time);
	
	// just fake the time for old scopes (as these were stored in original stack on fiber)
	for(i32 i=0; i<numDepth; ++i)
	{
		FcProfilerScope* scope = stack[i];
		scope->startTime.sec = time.sec;
		scope->startTime.usec = time.usec;
	}
	
	// set proper depth for the profiler
	const i32 threadIndex = fcJobSystemGetThisThreadIndex();
	FcProfilerThreadInfo* thread = &g_profiler.threads[threadIndex];
	thread->currentDepth = numDepth;
	thread->current = stack[0];
}

void fcProfilerEnterContention(void)
{
	const i32 threadIndex = fcJobSystemGetThisThreadIndex();
	FcProfilerThreadInfo* thread = &g_profiler.threads[threadIndex];
	fcTimevalNow(&thread->tempContentionStartTime);
}

void fcProfilerExitContention(const char* name)
{
	const i32 threadIndex = fcJobSystemGetThisThreadIndex();
	FcProfilerThreadInfo* thread = &g_profiler.threads[threadIndex];
	
	FcTimeval time = { 0 };
	fcTimevalNow(&time);
	
	// skip short contention times, as it might be no contention at all
	if(time.sec != thread->tempContentionStartTime.sec || time.usec - thread->tempContentionStartTime.usec > 30)
	{
		FcContentionScope* scopes = thread->contentionScopes[g_profiler.currentFrame];
		const i32 idx = thread->currentNumContentionScopes;
		
		scopes[idx].name = name;
		scopes[idx].startTime = thread->tempContentionStartTime;
		scopes[idx].endTime = time;
		
		thread->currentNumContentionScopes++;
	}
}
