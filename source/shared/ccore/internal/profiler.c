/* Copyright (c) 2016-2020 Furball Cat */

#include <sys/time.h>
#include <stdio.h>

#include "profiler.h"
#include "memory.h"
#include "furAssert.h"
#include "debugDraw.h"
#include "jobs.h"

#define FC_PROFILER_MAX_SCOPES 8192
#define FC_PROFILER_MAX_FRAMES 5
#define FC_PROFILER_FRAMES_DRAWN 4

typedef struct fc_profiler_thread_info_t
{
	uint32_t currentNumScopes;
	uint32_t currentDepth;
	fc_profiler_scope_t* scopes[FC_PROFILER_MAX_FRAMES];	// one frame is for pause mode
	uint32_t frameNumScopes[FC_PROFILER_MAX_FRAMES];
	
	fc_profiler_scope_t* current;
} fc_profiler_thread_info_t;

typedef struct fc_profiler_t
{
	fc_profiler_thread_info_t* threads;
	int32_t numThreads;	// includign main thread, use thread index to access threads array
	
	struct timeval frameStartTimes[FC_PROFILER_MAX_FRAMES];
	
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
	
	g_profiler.numThreads = fc_job_system_num_max_threads();
	
	g_profiler.threads = FUR_ALLOC_ARRAY_AND_ZERO(fc_profiler_thread_info_t, g_profiler.numThreads, 0, FC_MEMORY_SCOPE_PROFILER, pAllocCallbacks);
	
	for(int32_t t=0; t<g_profiler.numThreads; ++t)
	{
		for(uint32_t i=0; i<FC_PROFILER_MAX_FRAMES; ++i)
		{
			g_profiler.threads[t].scopes[i] = FUR_ALLOC_ARRAY_AND_ZERO(fc_profiler_scope_t, FC_PROFILER_MAX_SCOPES, 0, FC_MEMORY_SCOPE_PROFILER, pAllocCallbacks);
		}
	}
}

void fc_profiler_release(fc_alloc_callbacks_t* pAllocCallbacks)
{
	for(int32_t t=0; t<g_profiler.numThreads; ++t)
	{
		for(uint32_t i=0; i<FC_PROFILER_MAX_FRAMES; ++i)
		{
			FUR_FREE(g_profiler.threads[t].scopes[i], pAllocCallbacks);
		}
	}
	
	FUR_FREE(g_profiler.threads, pAllocCallbacks);
}

fc_profiler_scope_t* fc_profiler_scope_begin(const char* name)
{
	const int32_t threadIndex = fc_job_system_get_this_thread_index();
	fc_profiler_thread_info_t* thread = &g_profiler.threads[threadIndex];
	
	FUR_ASSERT(thread->currentNumScopes < FC_PROFILER_MAX_SCOPES);
	
	const uint32_t idx = thread->currentNumScopes;
	thread->currentNumScopes += 1;
	const uint32_t depth = thread->currentDepth;
	thread->currentDepth += 1;
	
	struct timeval time = {};
	gettimeofday(&time, NULL);
	
	fc_profiler_scope_t scope = {};
	scope.name = name;
	scope.depth = depth;
	scope.startTime.sec = time.tv_sec;
	scope.startTime.usec = time.tv_usec;
	fc_profiler_scope_t* pScope = &thread->scopes[g_profiler.currentFrame][idx];
	*pScope = scope;
	
	// keep track of scope stack because of fibers (so we can store and load stack)
	fc_profiler_scope_t* parent = thread->current;
	thread->current = pScope;
	pScope->parent = parent;
	
	return pScope;
}

void fc_profiler_scope_end(fc_profiler_scope_t* scope)
{
	// note that scope might not belong to current thread in case of fiber switch
	
	const int32_t threadIndex = fc_job_system_get_this_thread_index();
	fc_profiler_thread_info_t* thread = &g_profiler.threads[threadIndex];
	
	thread->currentDepth -= 1;
	
	struct timeval time = {};
	gettimeofday(&time, NULL);
	
	scope->stopTime.sec = time.tv_sec;
	scope->stopTime.usec = time.tv_usec;
	scope->threadID = (int16_t)threadIndex;
	
	thread->current = scope->parent;
}

void fc_profiler_pause(void)
{
	const uint32_t pausingModeFrame = FC_PROFILER_MAX_FRAMES - 1;
	if(g_profiler.currentFrame != pausingModeFrame)
	{
		g_profiler.pausedOnFrame = g_profiler.currentFrame;
		g_profiler.currentFrame = pausingModeFrame;
	}
}

void fc_profiler_start_frame(void)
{
	FUR_ASSERT(fc_job_system_is_main_thread());
	
	// tick profiler
	{
		struct timeval frameStartTime = {};
		gettimeofday(&frameStartTime, NULL);
		
		g_profiler.frameStartTimes[g_profiler.currentFrame] = frameStartTime;
		
		for(int32_t t=0; t<g_profiler.numThreads; ++t)
		{
			FUR_ASSERT(g_profiler.threads[t].currentDepth == 0); // make sure all scopes were closed
			
			g_profiler.threads[t].currentNumScopes = 0;
		}
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
		const float grey[4] = {0.6f, 0.6f, 0.6f, 0.8f};
		
		fc_dbg_rect(x, y, 1, 1200.0f, blue);
		fc_dbg_rect(x + 16.6f * g_profiler.zoom, y, 1, 1200.0f, green);
		fc_dbg_rect(x + 33.3f * g_profiler.zoom, y, 1, 1200.0f, yellow);
		
		// start drawing from the oldest frame (which is current + 1, as we rotate frames)
		uint32_t frameIdx = (g_profiler.pausedOnFrame + 1) % FC_PROFILER_FRAMES_DRAWN;
		const struct timeval firstFrameStartTime = g_profiler.frameStartTimes[frameIdx];
		const double firstFrameStartTime_ms = firstFrameStartTime.tv_sec * 1000.0 + firstFrameStartTime.tv_usec / 1000.0;
		
		char coreTxt[16] = {};
		
		for(int32_t t=0; t<g_profiler.numThreads; ++t)
		{
			const fc_profiler_thread_info_t* thread = &g_profiler.threads[t];
			
			float y = 600.0f - t * 200.0f;	// place thread lines in their own horizontal panels
			
			// line for each core/thread
			fc_dbg_rect(x - 120.0f, y, 8000.0f, 1.0f, grey);
			sprintf(coreTxt, "Core %i", t);
			fc_dbg_text(x - 120.0f, y, coreTxt, white);
			
			for(uint32_t f=0; f<FC_PROFILER_FRAMES_DRAWN-1; ++f, frameIdx = (frameIdx + 1) % FC_PROFILER_FRAMES_DRAWN)
			{
				fc_profiler_scope_t* scopes = thread->scopes[frameIdx];
				const uint32_t numScopes = thread->frameNumScopes[frameIdx];
				
				for(uint32_t i=0; i<numScopes; ++i)
				{
					fc_profiler_scope_t scope = scopes[i];
					
					// scope could be closed by another thread in case of fiber switch
					y = 600.0f - scope.threadID * 200.0f;
					
					color[0] = ((uint8_t)scope.name[0]) / 255.0f;
					color[1] = ((uint8_t)scope.name[1]) / 255.0f;
					color[2] = ((uint8_t)scope.name[2]) / 255.0f;
					
					const float startTime_ms = scope.startTime.sec * 1000.0 + scope.startTime.usec / 1000.0 - firstFrameStartTime_ms;
					const float stopTime_ms = scope.stopTime.sec * 1000.0 + scope.stopTime.usec / 1000.0 - firstFrameStartTime_ms;
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
						const float elapsedTime_ms = stopTime_ms - startTime_ms;
						sprintf(txt, "%s (%1.3fms)", scope.name, elapsedTime_ms);
						fc_dbg_text(x + x_offset, y - 30.0 * scope.depth, txt, white);
						
						/*if(elapsedTime_ms > 20.0f)
						{
							fc_profiler_pause();
						}*/
					}
				}
			}
		}
	}
}

void fc_profiler_end_frame(void)
{
	FUR_ASSERT(fc_job_system_is_main_thread());
	
	for(int32_t t=0; t<g_profiler.numThreads; ++t)
	{
		g_profiler.threads[t].frameNumScopes[g_profiler.currentFrame] = g_profiler.threads[t].currentNumScopes;
	}
	
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

uint64_t fc_log_profiler_begin(void)
{
	struct timeval time = {};
	gettimeofday(&time, NULL);
	
	return (uint64_t)time.tv_sec * 1000000 + (uint64_t)time.tv_usec;
}

void fc_log_profiler_end(const char* scopeName, uint64_t startTime)
{
	struct timeval time = {};
	gettimeofday(&time, NULL);
	const uint64_t endTime = (uint64_t)time.tv_sec * 1000000 + (uint64_t)time.tv_usec;
	
	const float scopeTime = (float)(endTime - startTime) / 1000.0f;
	
	printf("%s: %1.3fms\n", scopeName, scopeTime);
}

int32_t fc_profiler_store_scopestack(fc_profiler_scope_t* stack[32])
{
	const int32_t threadIndex = fc_job_system_get_this_thread_index();
	fc_profiler_thread_info_t* thread = &g_profiler.threads[threadIndex];
	const int8_t currFrame = g_profiler.currentFrame;
	fc_profiler_scope_t* scopes = thread->scopes[currFrame];
	
	int32_t numStack = 0;
	
	// create a copy of current callstack, and keep old scopes for the next callstack
	fc_profiler_scope_t* scope = thread->current;
	fc_profiler_scope_t* child = NULL;
	while(scope)
	{
		// finish this scope
		fc_profiler_scope_end(scope);
		
		// copy it to new scope
		FUR_ASSERT(thread->currentNumScopes < FC_PROFILER_MAX_SCOPES);
		const uint32_t idx = thread->currentNumScopes;
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

void fc_profiler_load_scopestack(fc_profiler_scope_t* stack[32], int32_t numDepth)
{
	FUR_ASSERT(numDepth <= 32);
	
	// we will assume all scopes start at the same time after jumping back to fiber
	struct timeval time = {};
	gettimeofday(&time, NULL);
	
	// just fake the time for old scopes (as these were stored in original stack on fiber)
	for(int32_t i=0; i<numDepth; ++i)
	{
		fc_profiler_scope_t* scope = stack[i];
		scope->startTime.sec = time.tv_sec;
		scope->startTime.usec = time.tv_usec;
	}
	
	// set proper depth for the profiler
	const int32_t threadIndex = fc_job_system_get_this_thread_index();
	fc_profiler_thread_info_t* thread = &g_profiler.threads[threadIndex];
	thread->currentDepth = numDepth;
	thread->current = stack[0];
}
