/* Copyright (c) Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "api.h"
#include "types.h"

#if PLATFORM_OSX
#include <stdatomic.h>	// for atomic_int
#include <pthread.h>

// todo: fix this on OSX
typedef pthread_rwlock_t fc_rwlock_t;

typedef struct fc_timeval_t
{
	u64 sec;	// seconds
	u32 usec;	// microseconds
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
	u64 sec;	// seconds
	u32 usec;	// microseconds
} fc_timeval_t;

#define FUR_THREAD_LOCAL __declspec( thread )

#endif

/* Usage
	FUR_SCOPED_WRITE_LOCK(rwlock)
	{
		// locked section code ...
	}
 */
#define FUR_SCOPED_WRITE_LOCK(_lock, _name) for(i32 _lock_check = fc_rwlock_write_lock(&_lock, _name); _lock_check == 1; _lock_check = fc_rwlock_write_unlock(&_lock))
#define FUR_SCOPED_READ_LOCK(_lock, _name) for(i32 _lock_check = fc_rwlock_read_lock(&_lock, _name); _lock_check == 1; _lock_check = fc_rwlock_read_unlock(&_lock))

CCORE_API void fc_rwlock_init(fc_rwlock_t* lock);

CCORE_API i32 fc_rwlock_read_unlock(fc_rwlock_t* lock);
CCORE_API i32 fc_rwlock_write_unlock(fc_rwlock_t* lock);

CCORE_API i32 fc_rwlock_read_lock(fc_rwlock_t* lock, const char* name);
CCORE_API i32 fc_rwlock_write_lock(fc_rwlock_t* lock, const char* name);

// time
CCORE_API void fc_timeval_now(fc_timeval_t* tv);

// atomic
#if PLATFORM_OSX
typedef atomic_int fc_atomic_int;
#elif PLATFORM_WINDOWS
typedef int fc_atomic_int;
#endif

// store int inside atomic_int
void fc_atomic_store(fc_atomic_int* dst, int src);

// returns int from atomic_int
int fc_atomic_load(fc_atomic_int* src);

// fetches int from atomic_int and subtracts subValue
int fc_atomic_fetch_sub(fc_atomic_int* dst, int subValue);

// thread
typedef struct fc_thread_t
{
	u64 id;
} fc_thread_t;

CCORE_API fc_thread_t fc_thread_self();
CCORE_API i32 fc_thread_create_suspended(fc_thread_t* outThread, void (*func)(void*), void* userData);
CCORE_API void fc_thread_resume(fc_thread_t thread);
CCORE_API void fc_thread_join(fc_thread_t thread);
CCORE_API void fc_thread_set_affinity(fc_thread_t thread, i32 coreID);

#ifdef __cplusplus
}
#endif // __cplusplus
