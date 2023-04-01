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
	u64 capacity;
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
	//u32 markerBegin;
	struct fc_mem_debug_info_t* next;
	struct fc_mem_debug_info_t* prev;
	const char* line;
	u64 size;
	enum fc_memory_scope_t scope;
	u16 offsetToOriginalPtr;
	//u32 markerEnd;
	
} fc_mem_debug_info_t;

fc_mem_debug_info_t g_rootDebugMemInfo;

void* fc_alloc(struct fc_alloc_callbacks_t* pAllocCallbacks, u64 size, u64 alignment,
							  enum fc_memory_scope_t scope, const char* info)
{
	if(size == 0)
		return NULL;
	
	if(pAllocCallbacks)
		return pAllocCallbacks->pfnAllocate(pAllocCallbacks->pUserData, size, alignment, scope);
	
#if FUR_MEMORY_DEBUG == 1
	u64 originalSize = size;
	size += sizeof(fc_mem_debug_info_t);
	size += alignment-1;
#endif
	
	void* ptr = NULL;
	
	ptr = malloc(size);
	
	u16 offset_forward = 0;
	if(alignment != 0)
	{
		void* ptr_candidate = ((u8*)ptr) + (sizeof(fc_mem_debug_info_t) + alignment-1);
		offset_forward = (u64)ptr_candidate % alignment;
	}
	
	void* debug_ptr = ((u8*)ptr) + offset_forward;
	void* alignedPtr = ((u8*)ptr) + (sizeof(fc_mem_debug_info_t) + offset_forward);
	
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

void* fc_alloc_and_zero(struct fc_alloc_callbacks_t* pAllocCallbacks, u64 size, u64 alignment,
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
	
	if(pAllocCallbacks)
	{
		pAllocCallbacks->pfnFree(pAllocCallbacks->pUserData, pMemory);
		return;
	}
	
#if FUR_MEMORY_DEBUG == 1
	// move ptr back, to include info part
	fc_mem_debug_info_t* debugPtr = (fc_mem_debug_info_t*)(((u8*)pMemory) - sizeof(fc_mem_debug_info_t));
	void* originalPtr = ((u8*)debugPtr) - debugPtr->offsetToOriginalPtr;
	
	FUR_ASSERT((uint64_t)debugPtr->next != 0xfefefefefefefefe);	// either f64-free or memory stomp (someone else freed this memory before you)
	
	if(debugPtr->next)
		debugPtr->next->prev = debugPtr->prev;
	debugPtr->prev->next = debugPtr->next;
	
	u64 fullSize = debugPtr->size + sizeof(fc_mem_debug_info_t);
	
	// debug pattern for dealloc
	memset(originalPtr, 0xFE, fullSize);
	
#endif
	
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
	fc_mem_stats_t stats = {0};
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
	fc_mem_stats_t stats = {0};
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

fc_mem_arena_alloc_t fc_mem_arena_make(void* buffer, u32 capacity)
{
	fc_mem_arena_alloc_t res = {0};
	res.size = 0;
	res.capacity = capacity;
	res.buffer = buffer;
	return res;
}

fc_mem_arena_alloc_t fc_mem_arena_sub(fc_mem_arena_alloc_t alloc)
{
	fc_mem_arena_alloc_t res = {0};
	res.size = 0;
	res.capacity = alloc.capacity - alloc.size;
	res.buffer = (u8*)alloc.buffer + alloc.size;
	return res;
}

void* fc_mem_arena_alloc(fc_mem_arena_alloc_t* pAlloc, u32 size, u32 alignment)
{
	FUR_ASSERT(pAlloc->size + size <= pAlloc->capacity);
	
	// todo: add alignment
	u64 offset = pAlloc->size;
	pAlloc->size += size;
	return (u8*)pAlloc->buffer + offset;
}

void* fc_mem_arena_alloc_and_zero(fc_mem_arena_alloc_t* pAlloc, u32 size, u32 alignment)
{
	void* mem = fc_mem_arena_alloc(pAlloc, size, alignment);
	memset(mem, 0, size);
	return mem;
}

// relocatable heap alloc functions
void* fc_mem_rel_heap_fn_alloc(void* pUserData, u64 size, u64 alignment, enum fc_memory_scope_t scope)
{
	fc_mem_rel_heap_alloc_t* alloc = pUserData;
	
	// todo: implement alignment
	FUR_ASSERT(alloc->size + size < alloc->capacity);
	
	void* ptr = alloc->freePtr;
	
	alloc->freePtr = (u8*)alloc->freePtr + size;
	alloc->size += size;
	
	return ptr;
}

void* fc_mem_rel_heap_fn_realloc(void* pUserData, void* pOriginalMemory, u64 size, u64 alignment, enum fc_memory_scope_t scope)
{
	FUR_ASSERT(false);	// not implemented
	return NULL;
}

void fc_mem_rel_heap_fn_free(void* pUserData, void* pMemory)
{
	// empty
}

void fc_mem_rel_heap_fn_internal_alloc_notify(void* pUserData, u64 size, enum fc_memory_type_t type, enum fc_memory_scope_t scope)
{
	// empty
}

void fc_mem_rel_heap_fn_internal_free_notify(void* pUserData, u64 size)
{
	// empty
}

fc_alloc_callbacks_t fc_mem_rel_heap_get_callbacks(fc_mem_rel_heap_alloc_t* pAlloc)
{
	fc_alloc_callbacks_t res = {0};
	
	res.pUserData = pAlloc;
	res.pfnAllocate = fc_mem_rel_heap_fn_alloc;
	res.pfnReallocate = fc_mem_rel_heap_fn_realloc;
	res.pfnFree = fc_mem_rel_heap_fn_free;
	res.pfnInternalAllocate = fc_mem_rel_heap_fn_internal_alloc_notify;
	res.pfnInternalFree = fc_mem_rel_heap_fn_internal_free_notify;
	
	return res;
}

void fc_relocate_pointer(void** ptr, i32 delta, void* lowerBound, void* upperBound)
{
	if(lowerBound <= *ptr && *ptr < upperBound)
	{
		*ptr = (u8*) *ptr + delta;
	}
}

void* fc_array_add(fc_array_t* arr)
{
	FUR_ASSERT(arr);
	FUR_ASSERT(arr->data);
	FUR_ASSERT(arr->num < arr->capacity);

	const u32 idx = arr->num;
	arr->num++;

	return ((u8*)arr->data + idx * arr->stride);
}

static inline void* fc_array_at_unsafe(fc_array_t* arr, u32 idx)
{
	return ((u8*)arr->data + idx * arr->stride);
}

void* fc_array_at(fc_array_t* arr, u32 idx)
{
	FUR_ASSERT(arr);
	FUR_ASSERT(arr->data);
	FUR_ASSERT(arr->num > 0);
	FUR_ASSERT(idx < arr->num);

	return fc_array_at_unsafe(arr, idx);
}

void fc_array_remove_swap(fc_array_t* arr, u32 idx)
{
	FUR_ASSERT(arr);
	FUR_ASSERT(arr->data);
	FUR_ASSERT(arr->num > 0);
	FUR_ASSERT(idx < arr->num);

	if (arr->num > 1)
	{
		void* lastElem = fc_array_at_unsafe(arr, arr->num - 1);
		void* removedElem = fc_array_at_unsafe(arr, idx);

		memcpy(removedElem, lastElem, arr->stride);
	}

	arr->num--;
}

static inline void* fc_map_key_at_unsafe(fc_map_t* map, u32 idx)
{
	return ((u8*)map->keys + (idx * map->keyStride));
}

static inline void* fc_map_elem_storage_ptr_at_unsafe(fc_map_t* map, u32 idx)
{
	return ((u8*)map->elems + (idx * map->elemStride));
}

void fc_map_insert(fc_map_t* map, const void* key, void* elem)
{
	FUR_ASSERT(map);
	FUR_ASSERT(map->keys && map->elems);

	void* elemFound = fc_map_find(map, key);
	if (elemFound)
	{
		memcpy(elemFound, elem, map->elemStride);
	}
	else
	{
		FUR_ASSERT(map->num < map->capacity);
		const u32 idx = map->num;
		
		void* keyStorage = fc_map_key_at_unsafe(map, idx);
		memcpy(keyStorage, key, map->keyStride);

		void* elemStorage = fc_map_elem_storage_ptr_at_unsafe(map, idx);
		memcpy(elemStorage, elem, map->elemStride);

		map->num++;
	}
}

void* fc_map_find(fc_map_t* map, const void* key)
{
	FUR_ASSERT(map);
	FUR_ASSERT(map->keys);

	void* result = NULL;

	for (u32 i = 0; i < map->num; ++i)
	{
		if (memcmp(key, fc_map_key_at_unsafe(map, i), map->keyStride) == 0)
		{
			result = *(void**)fc_map_elem_storage_ptr_at_unsafe(map, i);
			break;
		}
	}

	return result;
}

bool fc_map_remove_swap(fc_map_t* map, const void* key)
{
	FUR_ASSERT(map);
	FUR_ASSERT(map->keys);

	u32 idx = 0;
	bool found = false;

	for (u32 i = 0; i < map->num; ++i)
	{
		if (memcmp(key, fc_map_key_at_unsafe(map, i), map->keyStride) == 0)
		{
			idx = i;
			found = true;
		}
	}

	if (!found)
		return false;

	if (map->num > 1)
	{
		void* foundKey = fc_map_key_at_unsafe(map, idx);
		void* lastKey = fc_map_key_at_unsafe(map, map->num - 1);
		memcpy(foundKey, lastKey, map->keyStride);

		void* foundElem = fc_map_elem_storage_ptr_at_unsafe(map, idx);
		void* lastElem = fc_map_elem_storage_ptr_at_unsafe(map, map->num - 1);
		memcpy(foundElem, lastElem, map->elemStride);
	}

	map->num--;

	return true;
}
