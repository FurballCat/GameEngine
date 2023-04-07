/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus
	
#include "ccore/types.h"
#include "api.h"
	
#define FUR_MEMORY_DEBUG 1

// Use FUR_ALLOC and FUR_DEALLOC macros for memory management
// Pass allocation callbacks to every function that allocates anything

// Memory interface
typedef enum FcMemoryType
{
	FC_MEMORY_TYPE_DEFAULT = 0
} FcMemoryType;

typedef enum FcMemoryScope
{
	FC_MEMORY_SCOPE_SYSTEM = 0,
	FC_MEMORY_SCOPE_GLOBAL,
	FC_MEMORY_SCOPE_JOBS,
	FC_MEMORY_SCOPE_INPUT,
	FC_MEMORY_SCOPE_PHYSICS,
	FC_MEMORY_SCOPE_ANIMATION,
	FC_MEMORY_SCOPE_CAMERA,
	FC_MEMORY_SCOPE_SCRIPT,
	FC_MEMORY_SCOPE_DEBUG,
	FC_MEMORY_SCOPE_PROFILER,
	FC_MEMORY_SCOPE_RENDER,
	FC_MEMORY_SCOPE_GAME,
	FC_MEMORY_SCOPE_ARENA,
	FC_MEMORY_SCOPE_CORE,
	
	// note: remember to add name to the fcMemoryGetScopeDebugName
	
	FC_MEMORY_SCOPE_COUNT
} FcMemoryScope;

typedef void* (*FcMemAllocFn)(	void* 						pUserData,
									u64						size,
									u64						alignment,
									enum FcMemoryScope		scope);

typedef void* (*FcMemReallocFn)(void*						pUserData,
									 void*						pOriginalMemory,
									 u64						size,
									 u64						alignment,
									 enum FcMemoryScope		scope);

typedef void (*FcMemFreeFn)(void*	pUserData,
								 void*	pMemory);

typedef void (*FcMemInternalAllocNotifyFn)(void*						pUserData,
												  u64					size,
												  enum FcMemoryType		type,
												  enum FcMemoryScope	scope);

typedef void (*FcMemInternalFreeNotifyFn)(void*	pUserData,
												 u64	size);

// Pass this around in every function that allocates any memory
typedef struct FcAllocator
{
	void* 						pUserData;
	FcMemAllocFn 				pfnAllocate;
	FcMemReallocFn				pfnReallocate;
	FcMemFreeFn					pfnFree;
	FcMemInternalAllocNotifyFn 	pfnInternalAllocate;
	FcMemInternalFreeNotifyFn 	pfnInternalFree;
} FcAllocator;

#define S1(x) #x
#define S2(x) S1(x)

#if FUR_MEMORY_DEBUG == 0
	#define FUR_ALLOC(_size, _alignment, _scope, _pAllocCallbacks)	\
		fcAlloc(_pAllocCallbacks, _size, _alignment, _scope, "")
	#define FUR_ALLOC_AND_ZERO(_size, _alignment, _scope, _pAllocCallbacks)	\
		fcAllocAndZero(_pAllocCallbacks, _size, _alignment, _scope, "")
	#define FUR_FREE(_pMemory, _pAllocCallbacks)	\
		fcFree(_pAllocCallbacks, _pMemory, "")
#else
	#define FUR_ALLOC(_size, _alignment, _scope, _pAllocCallbacks)	\
		fcAlloc(_pAllocCallbacks, _size, _alignment, _scope, __FILE__ ":" S2(__LINE__))
	#define FUR_ALLOC_AND_ZERO(_size, _alignment, _scope, _pAllocCallbacks)	\
		fcAllocAndZero(_pAllocCallbacks, _size, _alignment, _scope, __FILE__ ":" S2(__LINE__))
	#define FUR_FREE(_pMemory, _pAllocCallbacks)	\
		fcFree(_pAllocCallbacks, _pMemory, __FILE__ ":" S2(__LINE__))
#endif
	
#define FUR_ALLOC_ARRAY(_type, _count, _alignment, _scope, _pAllocCallbacks)	\
	(_type*)FUR_ALLOC(sizeof(_type) * _count, _alignment, _scope, _pAllocCallbacks)
#define FUR_ALLOC_ARRAY_AND_ZERO(_type, _count, _alignment, _scope, _pAllocCallbacks)	\
	(_type*)FUR_ALLOC_AND_ZERO(sizeof(_type) * _count, _alignment, _scope, _pAllocCallbacks)

CCORE_API void* fcAlloc(struct FcAllocator* allocator, u64 size, u64 alignment,
							 enum FcMemoryScope scope, const char* info);
	
CCORE_API void* fcAllocAndZero(struct FcAllocator* allocator, u64 size, u64 alignment,
							 enum FcMemoryScope scope, const char* info);

CCORE_API void fcFree(struct FcAllocator* allocator, void* pMemory, const char* info);
	
CCORE_API bool fcValidateMemory(void);

typedef struct FcMemStats
{
	u32 numAllocs;
	u64 numBytesUsed;
	u64 numBytesCapacity;
} FcMemStats;

CCORE_API FcMemStats fcMemoryStats(void);
CCORE_API const char* fcMemoryGetScopeDebugName(enum FcMemoryScope scope);
CCORE_API FcMemStats fcMemoryStatsForScope(enum FcMemoryScope scope);

// arena allocator - acts like stack allocator with limited memory, size is reset once at the end of the scope
// it's an alloc and forget type of memory
typedef struct FcMemArenaAllocator
{
	void* buffer;
	u32 capacity;
	u32 size;
} FcMemArenaAllocator;

// makes arena allocator out of buffer with given capacity, size will be 0
CCORE_API FcMemArenaAllocator fcMemArenaMake(void* buffer, u32 capacity);

// makes sub arena allocator out of buffer = alloc.buffer + alloc.size, capacity = alloc.capacity - alloc.size, size = 0
CCORE_API FcMemArenaAllocator fcMemArenaSub(FcMemArenaAllocator alloc);

// call alloc to allocate memory on arena allocator (not real allocation, just stack allocation on preallocated memory)
CCORE_API void* fcMemArenaAlloc(FcMemArenaAllocator* pAlloc, u32 size, u32 alignment);
CCORE_API void* fcMemArenaAllocAndZero(FcMemArenaAllocator* pAlloc, u32 size, u32 alignment);

// relocatable heap allocator - used for level allocations (including game objects)
// it basically acts like a stack allocator, but when released in the middle, can relocate memory
typedef struct FcMemRelHeapPool
{
	void* buffer;
	void* freePtr;
	u32 capacity;
	u32 size;
} FcMemRelHeapPool;

CCORE_API FcAllocator fcMemRelHeapGetAllocator(FcMemRelHeapPool* pAlloc);

CCORE_API void fcRelocatePointer(void** ptr, i32 delta, void* lowerBound, void* upperBound);

#define FUR_ARRAY_SIZE(_arr) sizeof(_arr) / sizeof(_arr[0])

// static array generic type, to define one use FUR_DEFINE_ARRAY_TYPE( your_array_type_name, element_type );
typedef struct FcArray
{
	void* data;
	u32 capacity;
	u32 num;
	u32 stride;
} FcArray;

CCORE_API void* fcArrayAdd(FcArray* arr);
CCORE_API void* fcArrayAt(FcArray* arr, u32 idx);
CCORE_API void fcArrayRemoveSwap(FcArray* arr, u32 idx);

#define fcArrayAlloc(_arrayPtr, _type, _capacity, _alignment, _scope, _pAllocCallbacks)	\
	do { \
		FUR_ASSERT(!(_arrayPtr)->data);	\
		(_arrayPtr)->data = FUR_ALLOC(sizeof(_type) * _capacity, _alignment, _scope, _pAllocCallbacks);	\
		(_arrayPtr)->capacity = _capacity;	\
		(_arrayPtr)->num = 0;	\
		(_arrayPtr)->stride = sizeof(_type);	\
	} while(false)

#define fcArrayAllocAndZero(_arrayPtr, _type, _capacity, _alignment, _scope, _pAllocCallbacks)	\
	do { \
		FUR_ASSERT(!(_arrayPtr)->data);	\
		(_arrayPtr)->data = FUR_ALLOC_AND_ZERO(sizeof(_type) * _capacity, _alignment, _scope, _pAllocCallbacks); \
		(_arrayPtr)->capacity = _capacity;	\
		(_arrayPtr)->num = 0;	\
		(_arrayPtr)->stride = sizeof(_type);	\
	} while(false)

#define fcArrayFree(_arrayPtr, _pAllocCallbacks)	\
	do { \
		FUR_FREE((_arrayPtr)->data, _pAllocCallbacks); \
		(_arrayPtr)->data = NULL;	\
		(_arrayPtr)->num = 0;	\
		(_arrayPtr)->capacity = 0;	\
		(_arrayPtr)->stride = 0;	\
	} while(false)

// static map generic type, to define one use FUR_DEFINE_MAP_TYPE( your_array_type_name, element_type );
typedef struct FcMap
{
	void* keys;
	void* elems;
	u32 capacity;
	u32 num;
	u16 keyStride;
	u16 elemStride;
} FcMap;

CCORE_API void fcMapInsert(FcMap* map, const void* key, void* elem);
CCORE_API void* fcMapFind(FcMap* map, const void* key);
CCORE_API bool fcMapRemoveSwap(FcMap* map, const void* key);

#define fcMapAlloc(_mapPtr, _keyType, _elemType, _capacity, _alignment, _scope, _pAllocCallbacks)	\
	do { \
		FUR_ASSERT(!(_mapPtr)->keys && !(_mapPtr)->elems);	\
		(_mapPtr)->keys = FUR_ALLOC(sizeof(_keyType) * _capacity, _alignment, _scope, _pAllocCallbacks);	\
		(_mapPtr)->elems = FUR_ALLOC(sizeof(_elemType) * _capacity, _alignment, _scope, _pAllocCallbacks);	\
		(_mapPtr)->capacity = _capacity;	\
		(_mapPtr)->num = 0;	\
		(_mapPtr)->keyStride = sizeof(_keyType);	\
		(_mapPtr)->elemStride = sizeof(_elemType);	\
	} while(false)

#define fcMapAllocAndZero(_mapPtr, _keyType, _elemType, _capacity, _alignment, _scope, _pAllocCallbacks)	\
	do { \
		FUR_ASSERT(!(_mapPtr)->keys && !(_mapPtr)->elems);	\
		(_mapPtr)->keys = FUR_ALLOC_AND_ZERO(sizeof(_keyType) * _capacity, _alignment, _scope, _pAllocCallbacks);	\
		(_mapPtr)->elems = FUR_ALLOC_AND_ZERO(sizeof(_elemType) * _capacity, _alignment, _scope, _pAllocCallbacks);	\
		(_mapPtr)->capacity = _capacity;	\
		(_mapPtr)->num = 0;	\
		(_mapPtr)->keyStride = sizeof(_keyType);	\
		(_mapPtr)->elemStride = sizeof(_elemType);	\
	} while(false)

#define fcMapFree(_mapPtr, _pAllocCallbacks)	\
	do { \
		FUR_FREE((_mapPtr)->keys, _pAllocCallbacks); \
		FUR_FREE((_mapPtr)->elems, _pAllocCallbacks); \
		(_mapPtr)->keys = NULL;	\
		(_mapPtr)->num = 0;	\
		(_mapPtr)->capacity = 0;	\
		(_mapPtr)->keyStride = 0;	\
		(_mapPtr)->elemStride = 0;	\
	} while(false)

#ifdef __cplusplus
}
#endif // __cplusplus
