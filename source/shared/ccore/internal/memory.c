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

fc_memory_map_entry_t g_memoryMap[] = {
	{FC_MEMORY_SCOPE_GLOBAL}
};

typedef struct fc_mem_debug_info_t
{
	struct fc_mem_debug_info_t* next;
	struct fc_mem_debug_info_t* prev;
	const char* line;
	enum fc_memory_scope_t scope;
	size_t size;
	
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
#endif
	
	void* ptr = NULL;
	
	if(pAllocCallbacks)
		ptr = pAllocCallbacks->pfnAllocate(pAllocCallbacks->pUserData, size, alignment, scope);
	else
		ptr = malloc(size);
	
#if FUR_MEMORY_DEBUG == 1
	// put info in front of allocated memory
	fc_mem_debug_info_t* debugPtr = (fc_mem_debug_info_t*)ptr;
	debugPtr->next = g_rootDebugMemInfo.next;
	debugPtr->prev = &g_rootDebugMemInfo;
	debugPtr->line = info;
	debugPtr->size = originalSize;
	debugPtr->scope = scope;
	
	if(g_rootDebugMemInfo.next && g_rootDebugMemInfo.next->prev)
	{
		g_rootDebugMemInfo.next->prev = debugPtr;
	}
	g_rootDebugMemInfo.next = debugPtr;
	
	// move pointer by 8 bytes, so it skips the info part
	// we will move back 8 bytes when deallocating
	ptr = ((uint8_t*)ptr) + sizeof(fc_mem_debug_info_t);
#endif
	
	return ptr;
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
	pMemory = ((uint8_t*)pMemory) - sizeof(fc_mem_debug_info_t);
	
	fc_mem_debug_info_t* debugPtr = (fc_mem_debug_info_t*)pMemory;
	
	FUR_ASSERT((uint64_t)debugPtr->next != 0xfefefefefefefefe);	// either double-free or memory stomp (someone else freed this memory before you)
	
	if(debugPtr->next)
		debugPtr->next->prev = debugPtr->prev;
	debugPtr->prev->next = debugPtr->next;
	
	size_t fullSize = debugPtr->size + sizeof(fc_mem_debug_info_t);
	
	// debug pattern for dealloc
	memset(pMemory, 0xFE, fullSize);
	
#endif
	
	if(pAllocCallbacks)
		pAllocCallbacks->pfnFree(pAllocCallbacks->pUserData, pMemory);
	else
		free(pMemory);
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
	
	fc_mem_debug_info_t* ptr = &g_rootDebugMemInfo;
	while(ptr != NULL)
	{
		stats.numBytes += ptr->size;
		stats.numAllocs += 1;
		
		ptr = ptr->next;
	}
	
	return stats;
}

const char* fc_memory_get_scope_debug_name(enum fc_memory_scope_t scope)
{
	switch(scope)
	{
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
			return "game";
		default:
			FUR_ASSERT(false);	// unknown name of the memory scope, please implement
	}
	
	return "unknown";
}

fc_mem_stats_t fc_memory_stats_for_scope(enum fc_memory_scope_t scope)
{
	fc_mem_stats_t stats = {};
	
	fc_mem_debug_info_t* ptr = &g_rootDebugMemInfo;
	while(ptr != NULL)
	{
		if(ptr->scope == scope)
		{
			stats.numBytes += ptr->size;
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
