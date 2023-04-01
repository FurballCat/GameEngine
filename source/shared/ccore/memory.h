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
typedef enum fc_memory_type_t
{
	FC_MEMORY_TYPE_DEFAULT = 0
} fc_memory_type_t;

typedef enum fc_memory_scope_t
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
	
	// note: remember to add name to the fc_memory_get_scope_debug_name
	
	FC_MEMORY_SCOPE_COUNT
} fc_memory_scope_t;

typedef void* (*fc_mem_alloc_fn_t)(	void* 						pUserData,
									u64						size,
									u64						alignment,
									enum fc_memory_scope_t		scope);

typedef void* (*fc_mem_realloc_fn_t)(void*						pUserData,
									 void*						pOriginalMemory,
									 u64						size,
									 u64						alignment,
									 enum fc_memory_scope_t		scope);

typedef void (*fc_mem_free_fn_t)(void*	pUserData,
								 void*	pMemory);

typedef void (*fc_mem_internal_alloc_notify_fn_t)(void*						pUserData,
												  u64					size,
												  enum fc_memory_type_t		type,
												  enum fc_memory_scope_t	scope);

typedef void (*fc_mem_internal_free_notify_fn_t)(void*	pUserData,
												 u64	size);

// Pass this around in every function that allocates any memory
typedef struct fc_alloc_callbacks_t
{
	void* 								pUserData;
	fc_mem_alloc_fn_t 					pfnAllocate;
	fc_mem_realloc_fn_t					pfnReallocate;
	fc_mem_free_fn_t					pfnFree;
	fc_mem_internal_alloc_notify_fn_t 	pfnInternalAllocate;
	fc_mem_internal_free_notify_fn_t 	pfnInternalFree;
} fc_alloc_callbacks_t;

#define S1(x) #x
#define S2(x) S1(x)

#if FUR_MEMORY_DEBUG == 0
	#define FUR_ALLOC(_size, _alignment, _scope, _pAllocCallbacks)	\
		fc_alloc(_pAllocCallbacks, _size, _alignment, _scope, "")
	#define FUR_ALLOC_AND_ZERO(_size, _alignment, _scope, _pAllocCallbacks)	\
		fc_alloc_and_zero(_pAllocCallbacks, _size, _alignment, _scope, "")
	#define FUR_FREE(_pMemory, _pAllocCallbacks)	\
		fc_dealloc(_pAllocCallbacks, _pMemory, "")
#else
	#define FUR_ALLOC(_size, _alignment, _scope, _pAllocCallbacks)	\
		fc_alloc(_pAllocCallbacks, _size, _alignment, _scope, __FILE__ ":" S2(__LINE__))
	#define FUR_ALLOC_AND_ZERO(_size, _alignment, _scope, _pAllocCallbacks)	\
		fc_alloc_and_zero(_pAllocCallbacks, _size, _alignment, _scope, __FILE__ ":" S2(__LINE__))
	#define FUR_FREE(_pMemory, _pAllocCallbacks)	\
		fc_dealloc(_pAllocCallbacks, _pMemory, __FILE__ ":" S2(__LINE__))
#endif
	
#define FUR_ALLOC_ARRAY(_type, _count, _alignment, _scope, _pAllocCallbacks)	\
	(_type*)FUR_ALLOC(sizeof(_type) * _count, _alignment, _scope, _pAllocCallbacks)
#define FUR_ALLOC_ARRAY_AND_ZERO(_type, _count, _alignment, _scope, _pAllocCallbacks)	\
	(_type*)FUR_ALLOC_AND_ZERO(sizeof(_type) * _count, _alignment, _scope, _pAllocCallbacks)

CCORE_API void* fc_alloc(struct fc_alloc_callbacks_t* pAllocCallbacks, u64 size, u64 alignment,
							 enum fc_memory_scope_t scope, const char* info);
	
CCORE_API void* fc_alloc_and_zero(struct fc_alloc_callbacks_t* pAllocCallbacks, u64 size, u64 alignment,
							 enum fc_memory_scope_t scope, const char* info);

CCORE_API void fc_dealloc(struct fc_alloc_callbacks_t* pAllocCallbacks, void* pMemory, const char* info);
	
CCORE_API bool fc_validate_memory(void);

typedef struct fc_mem_stats_t
{
	u32 numAllocs;
	u64 numBytesUsed;
	u64 numBytesCapacity;
} fc_mem_stats_t;

CCORE_API fc_mem_stats_t fc_memory_stats(void);
CCORE_API const char* fc_memory_get_scope_debug_name(enum fc_memory_scope_t scope);
CCORE_API fc_mem_stats_t fc_memory_stats_for_scope(enum fc_memory_scope_t scope);

// arena allocator - acts like stack allocator with limited memory, size is reset once at the end of the scope
// it's an alloc and forget type of memory
typedef struct fc_mem_arena_alloc_t
{
	void* buffer;
	u32 capacity;
	u32 size;
} fc_mem_arena_alloc_t;

// makes arena allocator out of buffer with given capacity, size will be 0
CCORE_API fc_mem_arena_alloc_t fc_mem_arena_make(void* buffer, u32 capacity);

// makes sub arena allocator out of buffer = alloc.buffer + alloc.size, capacity = alloc.capacity - alloc.size, size = 0
CCORE_API fc_mem_arena_alloc_t fc_mem_arena_sub(fc_mem_arena_alloc_t alloc);

// call alloc to allocate memory on arena allocator (not real allocation, just stack allocation on preallocated memory)
CCORE_API void* fc_mem_arena_alloc(fc_mem_arena_alloc_t* pAlloc, u32 size, u32 alignment);
CCORE_API void* fc_mem_arena_alloc_and_zero(fc_mem_arena_alloc_t* pAlloc, u32 size, u32 alignment);

// relocatable heap allocator - used for level allocations (including game objects)
// it basically acts like a stack allocator, but when released in the middle, can relocate memory
typedef struct fc_mem_rel_heap_alloc_t
{
	void* buffer;
	void* freePtr;
	u32 capacity;
	u32 size;
} fc_mem_rel_heap_alloc_t;

CCORE_API fc_alloc_callbacks_t fc_mem_rel_heap_get_callbacks(fc_mem_rel_heap_alloc_t* pAlloc);

CCORE_API void fc_relocate_pointer(void** ptr, i32 delta, void* lowerBound, void* upperBound);

#define FUR_ARRAY_SIZE(_arr) sizeof(_arr) / sizeof(_arr[0])

// static array generic type, to define one use FUR_DEFINE_ARRAY_TYPE( your_array_type_name, element_type );
typedef struct fc_array_t
{
	void* data;
	u32 capacity;
	u32 num;
	u32 stride;
} fc_array_t;

CCORE_API void* fc_array_add(fc_array_t* arr);
CCORE_API void* fc_array_at(fc_array_t* arr, u32 idx);
CCORE_API void fc_array_remove_swap(fc_array_t* arr, u32 idx);

#define fc_array_alloc(_arrayPtr, _type, _capacity, _alignment, _scope, _pAllocCallbacks)	\
	do { \
		FUR_ASSERT(!(_arrayPtr)->data);	\
		(_arrayPtr)->data = FUR_ALLOC(sizeof(_type) * _capacity, _alignment, _scope, _pAllocCallbacks);	\
		(_arrayPtr)->capacity = _capacity;	\
		(_arrayPtr)->num = 0;	\
		(_arrayPtr)->stride = sizeof(_type);	\
	} while(false)

#define fc_array_alloc_and_zero(_arrayPtr, _type, _capacity, _alignment, _scope, _pAllocCallbacks)	\
	do { \
		FUR_ASSERT(!(_arrayPtr)->data);	\
		(_arrayPtr)->data = FUR_ALLOC_AND_ZERO(sizeof(_type) * _capacity, _alignment, _scope, _pAllocCallbacks); \
		(_arrayPtr)->capacity = _capacity;	\
		(_arrayPtr)->num = 0;	\
		(_arrayPtr)->stride = sizeof(_type);	\
	} while(false)

#define fc_array_free(_arrayPtr, _pAllocCallbacks)	\
	do { \
		FUR_FREE((_arrayPtr)->data, _pAllocCallbacks); \
		(_arrayPtr)->data = NULL;	\
		(_arrayPtr)->num = 0;	\
		(_arrayPtr)->capacity = 0;	\
		(_arrayPtr)->stride = 0;	\
	} while(false)

// static map generic type, to define one use FUR_DEFINE_MAP_TYPE( your_array_type_name, element_type );
typedef struct fc_map_t
{
	void* keys;
	void* elems;
	u32 capacity;
	u32 num;
	u16 keyStride;
	u16 elemStride;
} fc_map_t;

CCORE_API void fc_map_insert(fc_map_t* map, const void* key, void* elem);
CCORE_API void* fc_map_find(fc_map_t* map, const void* key);
CCORE_API bool fc_map_remove_swap(fc_map_t* map, const void* key);

#define fc_map_alloc(_mapPtr, _keyType, _elemType, _capacity, _alignment, _scope, _pAllocCallbacks)	\
	do { \
		FUR_ASSERT(!(_mapPtr)->keys && !(_mapPtr)->elems);	\
		(_mapPtr)->keys = FUR_ALLOC(sizeof(_keyType) * _capacity, _alignment, _scope, _pAllocCallbacks);	\
		(_mapPtr)->elems = FUR_ALLOC(sizeof(_elemType) * _capacity, _alignment, _scope, _pAllocCallbacks);	\
		(_mapPtr)->capacity = _capacity;	\
		(_mapPtr)->num = 0;	\
		(_mapPtr)->keyStride = sizeof(_keyType);	\
		(_mapPtr)->elemStride = sizeof(_elemType);	\
	} while(false)

#define fc_map_alloc_and_zero(_mapPtr, _keyType, _elemType, _capacity, _alignment, _scope, _pAllocCallbacks)	\
	do { \
		FUR_ASSERT(!(_mapPtr)->keys && !(_mapPtr)->elems);	\
		(_mapPtr)->keys = FUR_ALLOC_AND_ZERO(sizeof(_keyType) * _capacity, _alignment, _scope, _pAllocCallbacks);	\
		(_mapPtr)->elems = FUR_ALLOC_AND_ZERO(sizeof(_elemType) * _capacity, _alignment, _scope, _pAllocCallbacks);	\
		(_mapPtr)->capacity = _capacity;	\
		(_mapPtr)->num = 0;	\
		(_mapPtr)->keyStride = sizeof(_keyType);	\
		(_mapPtr)->elemStride = sizeof(_elemType);	\
	} while(false)

#define fc_map_free(_mapPtr, _pAllocCallbacks)	\
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
