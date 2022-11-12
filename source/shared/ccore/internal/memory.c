/* Copyright (c) 2016-2019 Furball Cat */

#include "memory.h"
#include "furAssert.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct fc_memory_map_entry_t
{
	fc_memory_scope_t name;
	fc_memory_scope_t parent;
	size_t capacity;
} fc_memory_map_entry_t;

#define FUR_SIZE_MB(x) x * 1024 * 1024

fc_memory_map_entry_t g_memoryMap[] = {
	{FC_MEMORY_SCOPE_SYSTEM, FC_MEMORY_SCOPE_SYSTEM, FUR_SIZE_MB(1024)},
		{FC_MEMORY_SCOPE_GLOBAL, FC_MEMORY_SCOPE_SYSTEM, FUR_SIZE_MB(256)},
			{FC_MEMORY_SCOPE_ARENA, FC_MEMORY_SCOPE_GLOBAL, FUR_SIZE_MB(2)},
			{FC_MEMORY_SCOPE_PHYSICS, FC_MEMORY_SCOPE_GLOBAL, FUR_SIZE_MB(16)},
			{FC_MEMORY_SCOPE_ANIMATION, FC_MEMORY_SCOPE_GLOBAL, FUR_SIZE_MB(60)},
			{FC_MEMORY_SCOPE_JOBS, FC_MEMORY_SCOPE_GLOBAL, FUR_SIZE_MB(2)},
		
			{FC_MEMORY_SCOPE_GAME, FC_MEMORY_SCOPE_GLOBAL, FUR_SIZE_MB(128)},
				{FC_MEMORY_SCOPE_INPUT, FC_MEMORY_SCOPE_GAME, FUR_SIZE_MB(4)},
				{FC_MEMORY_SCOPE_CAMERA, FC_MEMORY_SCOPE_GAME, FUR_SIZE_MB(1)},
				{FC_MEMORY_SCOPE_SCRIPT, FC_MEMORY_SCOPE_GAME, FUR_SIZE_MB(4)},
				{FC_MEMORY_SCOPE_PROFILER, FC_MEMORY_SCOPE_DEBUG, FUR_SIZE_MB(4)},
				{FC_MEMORY_SCOPE_RENDER, FC_MEMORY_SCOPE_GLOBAL, FUR_SIZE_MB(56)},
		
		{FC_MEMORY_SCOPE_DEBUG, FC_MEMORY_SCOPE_SYSTEM, FUR_SIZE_MB(128)}
};

bool g_mapSorted = false;

int fc_mem_entry_compare( const void* a, const void* b)
{
	const fc_memory_map_entry_t* entryA = (const fc_memory_map_entry_t*)a;
	const fc_memory_map_entry_t* entryB = (const fc_memory_map_entry_t*)b;
	
	if(entryA->name == entryB->name)
		return 0;
	else if(entryA->name < entryB->name)
		return -1;
	else
		return 1;
}

fc_memory_map_entry_t fc_mem_map_find(fc_memory_scope_t scope)
{
	// todo: rethink, not the best way to init
	if(!g_mapSorted)
	{
		g_mapSorted = true;
		qsort(g_memoryMap, FUR_ARRAY_SIZE(g_memoryMap), sizeof(fc_memory_map_entry_t), fc_mem_entry_compare);
	}
	
	return g_memoryMap[scope];
}

bool fc_mem_map_belongs_to(fc_memory_scope_t scope, fc_memory_scope_t ancestor)
{
	fc_memory_scope_t parent = scope;
	
	while(parent != ancestor && parent != FC_MEMORY_SCOPE_SYSTEM)
	{
		parent = fc_mem_map_find(parent).parent;
	}
	
	return parent == ancestor;
}

typedef struct fc_mem_debug_info_t
{
	//uint32_t markerBegin;
	struct fc_mem_debug_info_t* next;
	struct fc_mem_debug_info_t* prev;
	const char* line;
	size_t size;
	enum fc_memory_scope_t scope;
	uint16_t offsetToOriginalPtr;
	//uint32_t markerEnd;
	
} fc_mem_debug_info_t;

fc_mem_debug_info_t g_rootDebugMemInfo;

void* fc_alloc(struct fc_alloc_callbacks_t* pAllocCallbacks, size_t size, size_t alignment,
							  enum fc_memory_scope_t scope, const char* info)
{
	if(size == 0)
		return NULL;
	
#if FUR_MEMORY_DEBUG == 1
	size_t originalSize = size;
	size += sizeof(fc_mem_debug_info_t);
	size += alignment-1;
#endif
	
	void* ptr = NULL;
	
	if(pAllocCallbacks)
		ptr = pAllocCallbacks->pfnAllocate(pAllocCallbacks->pUserData, size, alignment, scope);
	else
		ptr = malloc(size);
	
	uint16_t offset_forward = 0;
	if(alignment != 0)
	{
		void* ptr_candidate = ((uint8_t*)ptr) + (sizeof(fc_mem_debug_info_t) + alignment-1);
		offset_forward = (size_t)ptr_candidate % alignment;
	}
	
	void* debug_ptr = ((uint8_t*)ptr) + offset_forward;
	void* alignedPtr = ((uint8_t*)ptr) + (sizeof(fc_mem_debug_info_t) + offset_forward);
	
#if FUR_MEMORY_DEBUG == 1
	// put info in front of allocated memory
	fc_mem_debug_info_t* debugPtr = (fc_mem_debug_info_t*)debug_ptr;
	//debugPtr->markerBegin = 'memb';
	//debugPtr->markerEnd = 'meme';
	debugPtr->next = g_rootDebugMemInfo.next;
	debugPtr->prev = &g_rootDebugMemInfo;
	debugPtr->line = info;
	debugPtr->size = originalSize;
	debugPtr->scope = scope;
	debugPtr->offsetToOriginalPtr = offset_forward;
	
	if(g_rootDebugMemInfo.next && g_rootDebugMemInfo.next->prev)
	{
		g_rootDebugMemInfo.next->prev = debugPtr;
	}
	g_rootDebugMemInfo.next = debugPtr;
#endif
	
	return alignedPtr;
}

void* fc_alloc_and_zero(struct fc_alloc_callbacks_t* pAllocCallbacks, size_t size, size_t alignment,
								  enum fc_memory_scope_t scope, const char* info)
{
	if(size == 0)
		return NULL;
	
	void* ptr = fc_alloc(pAllocCallbacks, size, alignment, scope, info);
	memset(ptr, 0, size);
	return ptr;
}

void fc_dealloc(struct fc_alloc_callbacks_t* pAllocCallbacks, void* pMemory, const char* info)
{
	if(pMemory == NULL)
		return;
	
#if FUR_MEMORY_DEBUG == 1
	// move ptr back, to include info part
	fc_mem_debug_info_t* debugPtr = (fc_mem_debug_info_t*)(((uint8_t*)pMemory) - sizeof(fc_mem_debug_info_t));
	void* originalPtr = ((uint8_t*)debugPtr) - debugPtr->offsetToOriginalPtr;
	
	FUR_ASSERT((uint64_t)debugPtr->next != 0xfefefefefefefefe);	// either double-free or memory stomp (someone else freed this memory before you)
	
	if(debugPtr->next)
		debugPtr->next->prev = debugPtr->prev;
	debugPtr->prev->next = debugPtr->next;
	
	size_t fullSize = debugPtr->size + sizeof(fc_mem_debug_info_t);
	
	// debug pattern for dealloc
	memset(originalPtr, 0xFE, fullSize);
	
#endif
	
	if(pAllocCallbacks)
		pAllocCallbacks->pfnFree(pAllocCallbacks->pUserData, pMemory);
	else
		free(originalPtr);
}

bool fc_validate_memory(void)
{
	if(g_rootDebugMemInfo.next != NULL)
	{
		fc_mem_debug_info_t* debugInfo = g_rootDebugMemInfo.next;
		
		while(debugInfo != NULL)
		{
			printf("Memory leak: size=%lu, line=%s\n", debugInfo->size, debugInfo->line);
			debugInfo = debugInfo->next;
		}
		
		return false;
	}
	
	printf("No memory leaks.\n");
	
	return true;
}

fc_mem_stats_t fc_memory_stats(void)
{
	fc_mem_stats_t stats = {};
	stats.numBytesCapacity = fc_mem_map_find(FC_MEMORY_SCOPE_SYSTEM).capacity;
	
	fc_mem_debug_info_t* ptr = &g_rootDebugMemInfo;
	while(ptr != NULL)
	{
		if(fc_mem_map_belongs_to(ptr->scope, FC_MEMORY_SCOPE_SYSTEM))
		{
			stats.numBytesUsed += ptr->size;
			stats.numAllocs += 1;
		}
		
		ptr = ptr->next;
	}
	
	return stats;
}

const char* fc_memory_get_scope_debug_name(enum fc_memory_scope_t scope)
{
	switch(scope)
	{
		case FC_MEMORY_SCOPE_SYSTEM:
			return "system";
		case FC_MEMORY_SCOPE_GLOBAL:
			return "global";
		case FC_MEMORY_SCOPE_INPUT:
			return "input";
		case FC_MEMORY_SCOPE_PHYSICS:
			return "physics";
		case FC_MEMORY_SCOPE_ANIMATION:
			return "animation";
		case FC_MEMORY_SCOPE_CAMERA:
			return "camera";
		case FC_MEMORY_SCOPE_SCRIPT:
			return "script";
		case FC_MEMORY_SCOPE_DEBUG:
			return "debug";
		case FC_MEMORY_SCOPE_PROFILER:
			return "profiler";
		case FC_MEMORY_SCOPE_RENDER:
			return "render";
		case FC_MEMORY_SCOPE_GAME:
			return "game";
		case FC_MEMORY_SCOPE_ARENA:
			return "arena";
		case FC_MEMORY_SCOPE_JOBS:
			return "jobs";
		default:
			FUR_ASSERT(false);	// unknown name of the memory scope, please implement
	}
	
	return "unknown";
}

fc_mem_stats_t fc_memory_stats_for_scope(enum fc_memory_scope_t scope)
{
	fc_mem_stats_t stats = {};
	stats.numBytesCapacity = fc_mem_map_find(scope).capacity;
	
	fc_mem_debug_info_t* ptr = &g_rootDebugMemInfo;
	while(ptr != NULL)
	{
		if(fc_mem_map_belongs_to(ptr->scope, scope))
		{
			stats.numBytesUsed += ptr->size;
			stats.numAllocs += 1;
		}
		
		ptr = ptr->next;
	}
	
	return stats;
}

fc_mem_arena_alloc_t fc_mem_arena_make(void* buffer, uint32_t capacity)
{
	fc_mem_arena_alloc_t res = {};
	res.size = 0;
	res.capacity = capacity;
	res.buffer = buffer;
	return res;
}

fc_mem_arena_alloc_t fc_mem_arena_sub(fc_mem_arena_alloc_t alloc)
{
	fc_mem_arena_alloc_t res = {};
	res.size = 0;
	res.capacity = alloc.capacity - alloc.size;
	res.buffer = alloc.buffer + alloc.size;
	return res;
}

void* fc_mem_arena_alloc(fc_mem_arena_alloc_t* pAlloc, uint32_t size, uint32_t alignment)
{
	FUR_ASSERT(pAlloc->size + size <= pAlloc->capacity);
	
	// todo: add alignment
	size_t offset = pAlloc->size;
	pAlloc->size += size;
	return pAlloc->buffer + offset;
}

void* fc_mem_arena_alloc_and_zero(fc_mem_arena_alloc_t* pAlloc, uint32_t size, uint32_t alignment)
{
	void* mem = fc_mem_arena_alloc(pAlloc, size, alignment);
	memset(mem, 0, size);
	return mem;
}
