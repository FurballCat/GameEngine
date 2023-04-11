/* Copyright (c) Furball Cat */

#include "platform.h"
#include "profiler.h"

#if PLATFORM_OSX
#include <sys/time.h>
#include <mach/thread_policy.h>
#include <mach/thread_act.h>

void fcLockRWInit(FcLockRW* lock)
{
	pthread_rwlock_init(lock, NULL);
}

i32 fcLockRWReadUnlock(FcLockRW* lock)
{
	pthread_rwlock_unlock(lock);
	return 0;
}

i32 fcLockRWUnlock(FcLockRW* lock)
{
	pthread_rwlock_unlock(lock);
	return 0;
}

i32 fcLockRWReadLock(FcLockRW* lock, const char* name)
{
#if FUR_USE_PROFILER
	fcProfilerEnterContention();
#endif

	pthread_rwlock_rdlock(lock);

#if FUR_USE_PROFILER
	fcProfilerExitContention(name);
#endif

	return 1;
}

i32 fcLockRWWriteLock(FcLockRW* lock, const char* name)
{
#if FUR_USE_PROFILER
	fcProfilerEnterContention();
#endif

	pthread_rwlock_wrlock(lock);

#if FUR_USE_PROFILER
	fcProfilerExitContention(name);
#endif

	return 1;
}

void fcTimevalNow(FcTimeval* tv)
{
	struct timeval time;
	gettimeofday(&time, NULL);
	tv->sec = time.tv_sec;
	tv->usec = time.tv_usec;
}

FcThread fcThreadSelf()
{
	FcThread thread;
	thread.id = (void*)pthread_self();
	return thread;
}

i32 fcThreadCreateSuspended(FcThread* outThread, void (*func)(void*), void* userData)
{
	return pthread_create_suspended_np(outThread, NULL, func, userData);
}

void fcThreadResume(FcThread thread)
{
	thread_resume((pthread_t)thread.id);
}

void fcThreadJoin(FcThread thread)
{
	pthread_join((pthread_t)thread.id, NULL);
}

void fcThreadSetAffinity(FcThread thread, i32 coreID)
{
	thread_affinity_policy_data_t policyData = { coreID };
	mach_port_t mach_thread = pthread_mach_thread_np((pthread_t)thread.id);
	thread_policy_set(mach_thread, THREAD_AFFINITY_POLICY, (thread_policy_t)&policyData, THREAD_AFFINITY_POLICY_COUNT);
}

void fcAtomicIntStore(FcAtomicInt* dst, int src)
{
	atomic_store(dst, src);
}

int fcAtomicIntLoad(FcAtomicInt* src)
{
	return atomic_load(src);
}

int fcAtomicIntFetchSub(FcAtomicInt* dst, int subValue)
{
	return atomic_fetch_sub(dst, subValue);
}

#elif PLATFORM_WINDOWS
#include <windows.h>

void fcLockRWInit(FcLockRW* lock)
{
	InitializeSRWLock((SRWLOCK*)lock);
}

i32 fcLockRWReadUnlock(FcLockRW* lock)
{
	ReleaseSRWLockShared((SRWLOCK*)lock);
	return 0;
}

i32 fcLockRWWriteUnlock(FcLockRW* lock)
{
	ReleaseSRWLockExclusive((SRWLOCK*)lock);
	return 0;
}

i32 fcLockRWReadLock(FcLockRW* lock, const char* name)
{
#if FUR_USE_PROFILER
	fcProfilerEnterContention();
#endif

	AcquireSRWLockShared((SRWLOCK*)lock);

#if FUR_USE_PROFILER
	fcProfilerExitContention(name);
#endif

	return 1;
}

i32 fcLockRWWriteLock(FcLockRW* lock, const char* name)
{
#if FUR_USE_PROFILER
	fcProfilerEnterContention();
#endif

	AcquireSRWLockExclusive((SRWLOCK*)lock);

#if FUR_USE_PROFILER
	fcProfilerExitContention(name);
#endif

	return 1;
}

void fcTimevalNow(FcTimeval* tv)
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

void fcAtomicIntStore(FcAtomicInt* dst, int src)
{
	InterlockedExchange(dst, src);
}

int fcAtomicIntLoad(FcAtomicInt* src)
{
	return InterlockedExchangeAdd(src, 0);
}

int fcAtomicIntFetchSub(FcAtomicInt* dst, int subValue)
{
	return InterlockedExchangeAdd(dst, -subValue);
}

FcThread fcThreadSelf()
{
	FcThread thread;
	thread.id = (void*)GetCurrentThread();
	return thread;
}

i32 fcThreadCreateSuspended(FcThread* outThread, void (*func)(void*), void* userData)
{
	HANDLE thread = CreateThread(NULL, 0, func, userData, CREATE_SUSPENDED, NULL);
	if (!thread)
		return 1;

	outThread->id = (void*)thread;
	return 0;
}

void fcThreadResume(FcThread thread)
{
	ResumeThread((HANDLE)thread.id);
}

void fcThreadJoin(FcThread thread)
{
	WaitForSingleObject((HANDLE)thread.id, INFINITE);
}

void fcThreadSetAffinity(FcThread thread, i32 coreID)
{
	DWORD_PTR mask = (i32)(1 << coreID);
	SetThreadAffinityMask((HANDLE)thread.id, mask);
}

#endif

f32 fcTimevalDiffToDeltaSeconds(FcTimeval* prev, FcTimeval* now)
{
	const f64 usec_to_sec = 1.0 / 1000000.0;
	const f64 startTime = prev->sec + prev->usec * usec_to_sec;
	const f64 stopTime = now->sec + now->usec * usec_to_sec;
	const f32 elapsedTime = stopTime - startTime;

	return elapsedTime;
}
