/* Copyright (c) Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include <stdbool.h>
#include <inttypes.h>

#if PLATFORM_OSX
#include <stdatomic.h>	// for atomic_int

// todo: fix this on OSX
typedef pthread_rwlock_t fc_rwlock_t;

typedef struct fc_timeval_t
{
	uint64_t sec;	// seconds
	uint32_t usec;	// microseconds
} fc_timeval_t;

#define FUR_THREAD_LOCAL thread_local

#elif PLATFORM_WINDOWS

// equivalent of SRWLOCK in windows
typedef struct fc_rwlock_t
{
	void* ptr;
} fc_rwlock_t;

typedef struct fc_timeval_t
{
	uint64_t sec;	// seconds
	uint32_t usec;	// microseconds
} fc_timeval_t;

#define FUR_THREAD_LOCAL __declspec( thread )

#endif

/* Usage
	FUR_SCOPED_WRITE_LOCK(rwlock)
	{
		// locked section code ...
	}
 */
#define FUR_SCOPED_WRITE_LOCK(_lock, _name) for(int32_t _lock_check = fc_rwlock_write_lock(&_lock, _name); _lock_check == 1; _lock_check = fc_rwlock_write_unlock(&_lock))
#define FUR_SCOPED_READ_LOCK(_lock, _name) for(int32_t _lock_check = fc_rwlock_read_lock(&_lock, _name); _lock_check == 1; _lock_check = fc_rwlock_read_unlock(&_lock))

CCORE_API void fc_rwlock_init(fc_rwlock_t* lock);

CCORE_API int32_t fc_rwlock_read_unlock(fc_rwlock_t* lock);
CCORE_API int32_t fc_rwlock_write_unlock(fc_rwlock_t* lock);

CCORE_API int32_t fc_rwlock_read_lock(fc_rwlock_t* lock, const char* name);
CCORE_API int32_t fc_rwlock_write_lock(fc_rwlock_t* lock, const char* name);

// time
CCORE_API void fc_timeval_now(fc_timeval_t* tv);

// atomic
typedef int fc_atomic_int;

// store int inside atomic_int
void fc_atomic_store(fc_atomic_int* dst, int src);

// returns int from atomic_int
int fc_atomic_load(fc_atomic_int* src);

// fetches int from atomic_int and subtracts subValue
int fc_atomic_fetch_sub(fc_atomic_int* dst, int subValue);

// thread
typedef struct fc_thread_t
{
	uint64_t id;
} fc_thread_t;

CCORE_API fc_thread_t fc_thread_self();
CCORE_API int32_t fc_thread_create_suspended(fc_thread_t* outThread, void (*func)(void*), void* userData);
CCORE_API void fc_thread_resume(fc_thread_t thread);
CCORE_API void fc_thread_join(fc_thread_t thread);
CCORE_API void fc_thread_set_affinity(fc_thread_t thread, int32_t coreID);

#ifdef __cplusplus
}
#endif // __cplusplus
