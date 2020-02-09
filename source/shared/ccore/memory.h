/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus
	
#include <inttypes.h>
#include "api.h"
	
#define FUR_MEMORY_DEBUG 0

// Use FUR_ALLOC and FUR_DEALLOC macros for memory management
// Pass allocation callbacks to every function that allocates anything

// Memory interface
typedef enum fc_memory_type_t
{
	FR_MEMORY_TYPE_DEFAULT = 0
} fc_memory_type_t;

typedef enum fc_memory_scope_t
{
	FR_MEMORY_SCOPE_DEFAULT = 0
} fc_memory_scope_t;

typedef void* (*fc_mem_alloc_fn_t)(	void* 						pUserData,
									size_t						size,
									size_t						alignment,
									enum fc_memory_scope_t		scope);

typedef void* (*fc_mem_realloc_fn_t)(void*						pUserData,
									 void*						pOriginalMemory,
									 size_t						size,
									 size_t						alignment,
									 enum fc_memory_scope_t		scope);

typedef void (*fc_mem_free_fn_t)(void*	pUserData,
								 void*	pMemory);

typedef void (*fc_mem_internal_alloc_notify_fn_t)(void*						pUserData,
												  size_t					size,
												  enum fc_memory_type_t		type,
												  enum fc_memory_scope_t	scope);

typedef void (*fc_mem_internal_free_notify_fn_t)(void*	pUserData,
												 size_t	size);

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

#if FUR_MEMORY_DEBUG == 0
	#define FUR_ALLOC(_size, _alignment, _scope, _pAllocCallbacks)	\
		fc_alloc(_pAllocCallbacks, _size, _alignment, _scope, "")
	#define FUR_FREE(_pMemory, _pAllocCallbacks)	\
		fc_dealloc(_pAllocCallbacks, _pMemory, "")
#else
	#define FUR_ALLOC(_size, _alignment, _scope, _pAllocCallbacks)	\
		fc_alloc(_pAllocCallbacks, _size, _alignment, _scope, __FILE__ ":" S2(__LINE__))
	#define FUR_FREE(_pMemory, _pAllocCallbacks)	\
		fc_dealloc(_pAllocCallbacks, _pMemory, __FILE__ ":" S2(__LINE__))
#endif

CCORE_API void* fc_alloc(struct fc_alloc_callbacks_t* pAllocCallbacks, size_t size, size_t alignment,
							 enum fc_memory_scope_t scope, const char* info);

CCORE_API void fc_dealloc(struct fc_alloc_callbacks_t* pAllocCallbacks, void* pMemory, const char* info);

#ifdef __cplusplus
}
#endif // __cplusplus
