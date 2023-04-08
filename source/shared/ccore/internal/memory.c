/* Copyright (c) 2016-2019 Furball Cat */

#include "memory.h"
#include "furAssert.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct FcMemoryMapEntry
{
	FcMemoryScope name;
	FcMemoryScope parent;
	u64 capacity;
} FcMemoryMapEntry;

#define FUR_SIZE_MB(x) x * 1024 * 1024

FcMemoryMapEntry g_memoryMap[] = {
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

int fcMemDebugInfo( const void* a, const void* b)
{
	const FcMemoryMapEntry* entryA = (const FcMemoryMapEntry*)a;
	const FcMemoryMapEntry* entryB = (const FcMemoryMapEntry*)b;
	
	if(entryA->name == entryB->name)
		return 0;
	else if(entryA->name < entryB->name)
		return -1;
	else
		return 1;
}

FcMemoryMapEntry fcMemoryMapFind(FcMemoryScope scope)
{
	// todo: rethink, not the best way to init
	if(!g_mapSorted)
	{
		g_mapSorted = true;
		qsort(g_memoryMap, FUR_ARRAY_SIZE(g_memoryMap), sizeof(FcMemoryMapEntry), fcMemDebugInfo);
	}
	
	return g_memoryMap[scope];
}

bool fcMemoryMapBelongsTo(FcMemoryScope scope, FcMemoryScope ancestor)
{
	FcMemoryScope parent = scope;
	
	while(parent != ancestor && parent != FC_MEMORY_SCOPE_SYSTEM)
	{
		parent = fcMemoryMapFind(parent).parent;
	}
	
	return parent == ancestor;
}

typedef struct FcMemDebugInfo
{
	//u32 markerBegin;
	struct FcMemDebugInfo* next;
	struct FcMemDebugInfo* prev;
	const char* line;
	u64 size;
	enum FcMemoryScope scope;
	u16 offsetToOriginalPtr;
	//u32 markerEnd;
	
} FcMemDebugInfo;

FcMemDebugInfo g_rootDebugMemInfo;

void* fcAlloc(struct FcAllocator* allocator, u64 size, u64 alignment,
							  enum FcMemoryScope scope, const char* info)
{
	if(size == 0)
		return NULL;
	
	if(allocator)
		return allocator->pfnAllocate(allocator->pUserData, size, alignment, scope);
	
#if FUR_MEMORY_DEBUG == 1
	u64 originalSize = size;
	size += sizeof(FcMemDebugInfo);
	size += alignment-1;
#endif
	
	void* ptr = NULL;
	
	ptr = malloc(size);
	
	u16 offset_forward = 0;
	if(alignment != 0)
	{
		void* ptr_candidate = ((u8*)ptr) + (sizeof(FcMemDebugInfo) + alignment-1);
		offset_forward = (u64)ptr_candidate % alignment;
	}
	
	void* debug_ptr = ((u8*)ptr) + offset_forward;
	void* alignedPtr = ((u8*)ptr) + (sizeof(FcMemDebugInfo) + offset_forward);
	
#if FUR_MEMORY_DEBUG == 1
	// put info in front of allocated memory
	FcMemDebugInfo* debugPtr = (FcMemDebugInfo*)debug_ptr;
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

void* fcAllocAndZero(const FcAllocator* allocator, u64 size, u64 alignment,
								  FcMemoryScope scope, const char* info)
{
	if(size == 0)
		return NULL;
	
	void* ptr = fcAlloc(allocator, size, alignment, scope, info);
	memset(ptr, 0, size);
	return ptr;
}

void fcFree(const FcAllocator* allocator, void* pMemory, const char* info)
{
	if(pMemory == NULL)
		return;
	
	if(allocator)
	{
		allocator->pfnFree(allocator->pUserData, pMemory);
		return;
	}
	
#if FUR_MEMORY_DEBUG == 1
	// move ptr back, to include info part
	FcMemDebugInfo* debugPtr = (FcMemDebugInfo*)(((u8*)pMemory) - sizeof(FcMemDebugInfo));
	void* originalPtr = ((u8*)debugPtr) - debugPtr->offsetToOriginalPtr;
	
	FUR_ASSERT((uint64_t)debugPtr->next != 0xfefefefefefefefe);	// either f64-free or memory stomp (someone else freed this memory before you)
	
	if(debugPtr->next)
		debugPtr->next->prev = debugPtr->prev;
	debugPtr->prev->next = debugPtr->next;
	
	u64 fullSize = debugPtr->size + sizeof(FcMemDebugInfo);
	
	// debug pattern for dealloc
	memset(originalPtr, 0xFE, fullSize);
	
#endif
	
	free(originalPtr);
}

bool fcValidateMemory(void)
{
	if(g_rootDebugMemInfo.next != NULL)
	{
		FcMemDebugInfo* debugInfo = g_rootDebugMemInfo.next;
		
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

FcMemStats fcMemoryStats(void)
{
	FcMemStats stats = {0};
	stats.numBytesCapacity = fcMemoryMapFind(FC_MEMORY_SCOPE_SYSTEM).capacity;
	
	FcMemDebugInfo* ptr = &g_rootDebugMemInfo;
	while(ptr != NULL)
	{
		if(fcMemoryMapBelongsTo(ptr->scope, FC_MEMORY_SCOPE_SYSTEM))
		{
			stats.numBytesUsed += ptr->size;
			stats.numAllocs += 1;
		}
		
		ptr = ptr->next;
	}
	
	return stats;
}

const char* fcMemoryGetScopeDebugName(FcMemoryScope scope)
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

FcMemStats fcMemoryStatsForScope(FcMemoryScope scope)
{
	FcMemStats stats = {0};
	stats.numBytesCapacity = fcMemoryMapFind(scope).capacity;
	
	FcMemDebugInfo* ptr = &g_rootDebugMemInfo;
	while(ptr != NULL)
	{
		if(fcMemoryMapBelongsTo(ptr->scope, scope))
		{
			stats.numBytesUsed += ptr->size;
			stats.numAllocs += 1;
		}
		
		ptr = ptr->next;
	}
	
	return stats;
}

FcMemArenaAllocator fcMemArenaMake(void* buffer, u32 capacity)
{
	FcMemArenaAllocator res = {0};
	res.size = 0;
	res.capacity = capacity;
	res.buffer = buffer;
	return res;
}

FcMemArenaAllocator fcMemArenaSub(FcMemArenaAllocator alloc)
{
	FcMemArenaAllocator res = {0};
	res.size = 0;
	res.capacity = alloc.capacity - alloc.size;
	res.buffer = (u8*)alloc.buffer + alloc.size;
	return res;
}

void* fcMemArenaAlloc(FcMemArenaAllocator* pAlloc, u32 size, u32 alignment)
{
	FUR_ASSERT(pAlloc->size + size <= pAlloc->capacity);
	
	// todo: add alignment
	u64 offset = pAlloc->size;
	pAlloc->size += size;
	return (u8*)pAlloc->buffer + offset;
}

void* fcMemArenaAllocAndZero(FcMemArenaAllocator* pAlloc, u32 size, u32 alignment)
{
	void* mem = fcMemArenaAlloc(pAlloc, size, alignment);
	memset(mem, 0, size);
	return mem;
}

// relocatable heap alloc functions
void* fcMemRelHeapFnAlloc(void* pUserData, u64 size, u64 alignment, enum FcMemoryScope scope)
{
	FcMemRelHeapPool* alloc = pUserData;
	
	// todo: implement alignment
	FUR_ASSERT(alloc->size + size < alloc->capacity);
	
	void* ptr = alloc->freePtr;
	
	alloc->freePtr = (u8*)alloc->freePtr + size;
	alloc->size += size;
	
	return ptr;
}

void* fcMemRelHeapFnRealloc(void* pUserData, void* pOriginalMemory, u64 size, u64 alignment, enum FcMemoryScope scope)
{
	FUR_ASSERT(false);	// not implemented
	return NULL;
}

void fcMemRelHeapFnFree(void* pUserData, void* pMemory)
{
	// empty
}

void fcMemRelHeapFnInternalAllocNotify(void* pUserData, u64 size, enum FcMemoryType type, enum FcMemoryScope scope)
{
	// empty
}

void fcMemRelHeapFnInternalFreeNotify(void* pUserData, u64 size)
{
	// empty
}

FcAllocator fcMemRelHeapGetAllocator(FcMemRelHeapPool* pAlloc)
{
	FcAllocator res = {0};
	
	res.pUserData = pAlloc;
	res.pfnAllocate = fcMemRelHeapFnAlloc;
	res.pfnReallocate = fcMemRelHeapFnRealloc;
	res.pfnFree = fcMemRelHeapFnFree;
	res.pfnInternalAllocate = fcMemRelHeapFnInternalAllocNotify;
	res.pfnInternalFree = fcMemRelHeapFnInternalFreeNotify;
	
	return res;
}

void fcRelocatePointer(void** ptr, i32 delta, void* lowerBound, void* upperBound)
{
	if(lowerBound <= *ptr && *ptr < upperBound)
	{
		*ptr = (u8*) *ptr + delta;
	}
}

void* fcArrayAdd(FcArray* arr)
{
	FUR_ASSERT(arr);
	FUR_ASSERT(arr->data);
	FUR_ASSERT(arr->num < arr->capacity);

	const u32 idx = arr->num;
	arr->num++;

	return ((u8*)arr->data + idx * arr->stride);
}

static inline void* fcArrayAtUnsafe(FcArray* arr, u32 idx)
{
	return ((u8*)arr->data + idx * arr->stride);
}

void* fcArrayAt(FcArray* arr, u32 idx)
{
	FUR_ASSERT(arr);
	FUR_ASSERT(arr->data);
	FUR_ASSERT(arr->num > 0);
	FUR_ASSERT(idx < arr->num);

	return fcArrayAtUnsafe(arr, idx);
}

void fcArrayRemoveSwap(FcArray* arr, u32 idx)
{
	FUR_ASSERT(arr);
	FUR_ASSERT(arr->data);
	FUR_ASSERT(arr->num > 0);
	FUR_ASSERT(idx < arr->num);

	if (arr->num > 1)
	{
		void* lastElem = fcArrayAtUnsafe(arr, arr->num - 1);
		void* removedElem = fcArrayAtUnsafe(arr, idx);

		memcpy(removedElem, lastElem, arr->stride);
	}

	arr->num--;
}

static inline void* fcMapKeyAtUnsafe(FcMap* map, u32 idx)
{
	return ((u8*)map->keys + (idx * map->keyStride));
}

static inline void* fcMapElemStoragePtrAtUnsafe(FcMap* map, u32 idx)
{
	return ((u8*)map->elems + (idx * map->elemStride));
}

void fcMapInsert(FcMap* map, const void* key, void* elem)
{
	FUR_ASSERT(map);
	FUR_ASSERT(map->keys && map->elems);

	void* elemFound = fcMapFind(map, key);
	if (elemFound)
	{
		memcpy(elemFound, elem, map->elemStride);
	}
	else
	{
		FUR_ASSERT(map->num < map->capacity);
		const u32 idx = map->num;
		
		void* keyStorage = fcMapKeyAtUnsafe(map, idx);
		memcpy(keyStorage, key, map->keyStride);

		void* elemStorage = fcMapElemStoragePtrAtUnsafe(map, idx);
		memcpy(elemStorage, elem, map->elemStride);

		map->num++;
	}
}

void* fcMapFind(FcMap* map, const void* key)
{
	FUR_ASSERT(map);
	FUR_ASSERT(map->keys);

	void* result = NULL;

	for (u32 i = 0; i < map->num; ++i)
	{
		if (memcmp(key, fcMapKeyAtUnsafe(map, i), map->keyStride) == 0)
		{
			result = *(void**)fcMapElemStoragePtrAtUnsafe(map, i);
			break;
		}
	}

	return result;
}

bool fcMapRemoveSwap(FcMap* map, const void* key)
{
	FUR_ASSERT(map);
	FUR_ASSERT(map->keys);

	u32 idx = 0;
	bool found = false;

	for (u32 i = 0; i < map->num; ++i)
	{
		if (memcmp(key, fcMapKeyAtUnsafe(map, i), map->keyStride) == 0)
		{
			idx = i;
			found = true;
		}
	}

	if (!found)
		return false;

	if (map->num > 1)
	{
		void* foundKey = fcMapKeyAtUnsafe(map, idx);
		void* lastKey = fcMapKeyAtUnsafe(map, map->num - 1);
		memcpy(foundKey, lastKey, map->keyStride);

		void* foundElem = fcMapElemStoragePtrAtUnsafe(map, idx);
		void* lastElem = fcMapElemStoragePtrAtUnsafe(map, map->num - 1);
		memcpy(foundElem, lastElem, map->elemStride);
	}

	map->num--;

	return true;
}
