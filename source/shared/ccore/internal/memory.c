/* Copyright (c) 2016-2019 Furball Cat */

#include "memory.h"
#include <stdlib.h>

void* fc_alloc(struct fc_alloc_callbacks_t* pAllocCallbacks, size_t size, size_t alignment,
							  enum fc_memory_scope_t scope, const char* info)
{
#if FUR_MEMORY_DEBUG == 0
	if(pAllocCallbacks)
		return pAllocCallbacks->pfnAllocate(pAllocCallbacks->pUserData, size, alignment, scope);
#endif
	
	return malloc(size);
}

void fc_dealloc(struct fc_alloc_callbacks_t* pAllocCallbacks, void* pMemory, const char* info)
{
#if FUR_MEMORY_DEBUG == 0
	if(pAllocCallbacks)
		return pAllocCallbacks->pfnFree(pAllocCallbacks->pUserData, pMemory);
#endif
	
	free(pMemory);
}
