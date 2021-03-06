/* Copyright (c) 2016-2019 Furball Cat */

#include "memory.h"
#include "furAssert.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct fc_mem_debug_info_t
{
	struct fc_mem_debug_info_t* next;
	struct fc_mem_debug_info_t* prev;
	const char* line;
	size_t size;
	
} fc_mem_debug_info_t;

fc_mem_debug_info_t g_rootDebugMemInfo;

void* fc_alloc(struct fc_alloc_callbacks_t* pAllocCallbacks, size_t size, size_t alignment,
							  enum fc_memory_scope_t scope, const char* info)
{
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
	void* ptr = fc_alloc(pAllocCallbacks, size, alignment, scope, info);
	memset(ptr, 0, size);
	return ptr;
}

void fc_dealloc(struct fc_alloc_callbacks_t* pAllocCallbacks, void* pMemory, const char* info)
{
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
