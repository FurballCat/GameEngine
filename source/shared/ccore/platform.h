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
typedef pthread_rwlock_t FcLockRW;

typedef struct FcTimeval
{
	u64 sec;	// seconds
	u32 usec;	// microseconds
} FcTimeval;

#define FUR_THREAD_LOCAL thread_local

#elif PLATFORM_WINDOWS

// equivalent of SRWLOCK in windows
typedef struct FcLockRW
{
	void* ptr;
} FcLockRW;

typedef struct FcTimeval
{
	u64 sec;	// seconds
	u32 usec;	// microseconds
} FcTimeval;

#define FUR_THREAD_LOCAL __declspec( thread )

#endif

/* Usage
	FUR_SCOPED_WRITE_LOCK(rwlock)
	{
		// locked section code ...
	}
 */
#define FUR_SCOPED_WRITE_LOCK(_lock, _name) for(i32 _lock_check = fcLockRWWriteLock(&_lock, _name); _lock_check == 1; _lock_check = fcLockRWWriteUnlock(&_lock))
#define FUR_SCOPED_READ_LOCK(_lock, _name) for(i32 _lock_check = fcLockRWReadLock(&_lock, _name); _lock_check == 1; _lock_check = fcLockRWReadUnlock(&_lock))

CCORE_API void fcLockRWInit(FcLockRW* lock);

CCORE_API i32 fcLockRWReadUnlock(FcLockRW* lock);
CCORE_API i32 fcLockRWWriteUnlock(FcLockRW* lock);

CCORE_API i32 fcLockRWReadLock(FcLockRW* lock, const char* name);
CCORE_API i32 fcLockRWWriteLock(FcLockRW* lock, const char* name);

// time
CCORE_API void fcTimevalNow(FcTimeval* tv);
CCORE_API f32 fcTimevalDiffToDeltaSeconds(FcTimeval* prev, FcTimeval* now);

// atomic
#if PLATFORM_OSX
typedef atomic_int FcAtomicInt;
#elif PLATFORM_WINDOWS
typedef int FcAtomicInt;
#endif

// store int inside atomic_int
void fcAtomicIntStore(FcAtomicInt* dst, int src);

// returns int from atomic_int
int fcAtomicIntLoad(FcAtomicInt* src);

// fetches int from atomic_int and subtracts subValue
int fcAtomicIntFetchSub(FcAtomicInt* dst, int subValue);

// thread
typedef struct FcThread
{
	u64 id;
} FcThread;

CCORE_API FcThread fcThreadSelf();
CCORE_API i32 fcThreadCreateSuspended(FcThread* outThread, void (*func)(void*), void* userData);
CCORE_API void fcThreadResume(FcThread thread);
CCORE_API void fcThreadJoin(FcThread thread);
CCORE_API void fcThreadSetAffinity(FcThread thread, i32 coreID);

#ifdef __cplusplus
}
#endif // __cplusplus
