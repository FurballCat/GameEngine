/* Copyright (c) Furball Cat */

#include "platform.h"
#include "profiler.h"

#if PLATFORM_OSX
#include <sys/time.h>
#include <mach/thread_policy.h>
#include <mach/thread_act.h>

void fc_rwlock_init(fc_rwlock_t* lock)
{
	pthread_rwlock_init(lock, NULL);
}

i32 fc_rwlock_read_unlock(fc_rwlock_t* lock)
{
	pthread_rwlock_unlock(lock);
	return 0;
}

i32 fc_rwlock_unlock(fc_rwlock_t* lock)
{
	pthread_rwlock_unlock(lock);
	return 0;
}

i32 fc_rwlock_read_lock(fc_rwlock_t* lock, const char* name)
{
#if FUR_USE_PROFILER
	fc_profiler_enter_contention();
#endif

	pthread_rwlock_rdlock(lock);

#if FUR_USE_PROFILER
	fc_profiler_exit_contention(name);
#endif

	return 1;
}

i32 fc_rwlock_write_lock(fc_rwlock_t* lock, const char* name)
{
#if FUR_USE_PROFILER
	fc_profiler_enter_contention();
#endif

	pthread_rwlock_wrlock(lock);

#if FUR_USE_PROFILER
	fc_profiler_exit_contention(name);
#endif

	return 1;
}

void fc_timeval_now(fc_timeval_t* tv)
{
	struct timeval time;
	gettimeofday(&time, NULL);
	tv->sec = time.tv_sec;
	tv->usec = time.tv_usec;
}

fc_thread_t fc_thread_self()
{
	fc_thread_t thread;
	thread.id = (void*)pthread_self();
	return thread;
}

i32 fc_thread_create_suspended(fc_thread_t* outThread, void (*func)(void*), void* userData)
{
	return pthread_create_suspended_np(outThread, NULL, func, userData);
}

void fc_thread_resume(fc_thread_t thread)
{
	thread_resume((pthread_t)thread.id);
}

void fc_thread_join(fc_thread_t thread)
{
	pthread_join((pthread_t)thread.id, NULL);
}

void fc_thread_set_affinity(fc_thread_t thread, i32 coreID)
{
	thread_affinity_policy_data_t policyData = { coreID };
	mach_port_t mach_thread = pthread_mach_thread_np((pthread_t)thread.id);
	thread_policy_set(mach_thread, THREAD_AFFINITY_POLICY, (thread_policy_t)&policyData, THREAD_AFFINITY_POLICY_COUNT);
}

void fc_atomic_store(fc_atomic_int* dst, int src)
{
	atomic_store(dst, src);
}

int fc_atomic_load(fc_atomic_int* src)
{
	return atomic_load(src);
}

int fc_atomic_fetch_sub(fc_atomic_int* dst, int subValue)
{
	return atomic_fetch_sub(dst, subValue);
}

#elif PLATFORM_WINDOWS
#include <windows.h>

void fc_rwlock_init(fc_rwlock_t* lock)
{
	InitializeSRWLock((SRWLOCK*)lock);
}

i32 fc_rwlock_read_unlock(fc_rwlock_t* lock)
{
	ReleaseSRWLockShared((SRWLOCK*)lock);
	return 0;
}

i32 fc_rwlock_write_unlock(fc_rwlock_t* lock)
{
	ReleaseSRWLockExclusive((SRWLOCK*)lock);
	return 0;
}

i32 fc_rwlock_read_lock(fc_rwlock_t* lock, const char* name)
{
#if FUR_USE_PROFILER
	fc_profiler_enter_contention();
#endif

	AcquireSRWLockShared((SRWLOCK*)lock);

#if FUR_USE_PROFILER
	fc_profiler_exit_contention(name);
#endif

	return 1;
}

i32 fc_rwlock_write_lock(fc_rwlock_t* lock, const char* name)
{
#if FUR_USE_PROFILER
	fc_profiler_enter_contention();
#endif

	AcquireSRWLockExclusive((SRWLOCK*)lock);

#if FUR_USE_PROFILER
	fc_profiler_exit_contention(name);
#endif

	return 1;
}

void fc_timeval_now(fc_timeval_t* tv)
{
	// todo: do it once at the start
	LARGE_INTEGER frequency;
	QueryPerformanceFrequency(&frequency);

	LARGE_INTEGER time;
	QueryPerformanceCounter(&time);

	// todo: not the fastest way...
	const f64 elapsedTime = (f64)time.QuadPart / (f64)frequency.QuadPart;

	tv->sec = (uint64_t)elapsedTime;
	tv->usec = (u32)((elapsedTime - tv->sec) * 1000000);
}

void fc_atomic_store(fc_atomic_int* dst, int src)
{
	InterlockedExchange(dst, src);
}

int fc_atomic_load(fc_atomic_int* src)
{
	return InterlockedExchangeAdd(src, 0);
}

int fc_atomic_fetch_sub(fc_atomic_int* dst, int subValue)
{
	return InterlockedExchangeAdd(dst, -subValue);
}

fc_thread_t fc_thread_self()
{
	fc_thread_t thread;
	thread.id = (void*)GetCurrentThread();
	return thread;
}

i32 fc_thread_create_suspended(fc_thread_t* outThread, void (*func)(void*), void* userData)
{
	HANDLE thread = CreateThread(NULL, 0, func, userData, CREATE_SUSPENDED, NULL);
	if (!thread)
		return 1;

	outThread->id = (void*)thread;
	return 0;
}

void fc_thread_resume(fc_thread_t thread)
{
	ResumeThread((HANDLE)thread.id);
}

void fc_thread_join(fc_thread_t thread)
{
	WaitForSingleObject((HANDLE)thread.id, INFINITE);
}

void fc_thread_set_affinity(fc_thread_t thread, i32 coreID)
{
	DWORD_PTR mask = (i32)(1 << coreID);
	SetThreadAffinityMask((HANDLE)thread.id, mask);
}

#endif
