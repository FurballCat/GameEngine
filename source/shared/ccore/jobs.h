/* Copyright (c) Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "ccore/types.h"
#include "platform.h"
#include "api.h"

#define FUR_JOB_ENTRY_POINT(_funcName) void _funcName(void* _userData)
#define FUR_JOB_USER_DATA(_type) (_type*)_userData

/* Example usage:
 
 // Job function definition
 FUR_JOB_ENTRY_POINT(fc_test_job_func)
 {
	my_data_t* userData = FUR_JOB_USER_DATA(my_data_t);
	...
 }
 
 ...
 
 my_data_t myData = (...);	// it can be on the stack, that's the cool part about fibers
 
 FcJobDecl jobDecl = {};
 jobDecl.func = fcTestJobFunc;
 jobDecl.data = &myData;
 
 FcJobCounter* counter = NULL;
 fcRunJobs(&jobDecl, 1, &counter);	// at this point the job is scheduled
 
 fcWaitForCounterAndFree(counter);	// this waits until the job is done
 */

typedef struct FcAllocator FcAllocator;

CCORE_API void fcJobSystemInit(FcAllocator* allocator);
CCORE_API void fcJobSystemRelease(FcAllocator* allocator);

typedef void (*FcJobFunc)(void* userData);

typedef struct FcJobDecl
{
	FcJobFunc func;
	void* userData;
} FcJobDecl;

typedef struct FcJobCounter
{
	FcAtomicInt value;
	i32 index;
} FcJobCounter;

CCORE_API void fcRunJobs(const FcJobDecl* jobs, i32 numJobs, FcJobCounter** outCounter);
CCORE_API void fcWaitForCounterAndFree(FcJobCounter* counter);

// call this on main once at the init, this job will be your main thread loop
void fcJobSystemSetupMainThreadJob(const FcJobDecl* job);

// call this on main, once called, main thread is now a regular worker thread
CCORE_API void fcJobSystemEnterWorkerThreadMode(void);

// call at the end of main, to exit all worker threads, including main-worker thread
CCORE_API void fcJobSystemExitAllJobs(void);

// this will return index between 0 and num_max_threads, where 0 is the main thread
CCORE_API i32 fcJobSystemGetThisThreadIndex(void);

// returns true if this is the main thread, otherwise false
CCORE_API bool fcJobSystemIsMainThread(void);

// this will return number of worker threads + 1 (main thread)
CCORE_API i32 fcJobSystemNumMaxThreads(void);

#ifdef __cplusplus
}
#endif // __cplusplus
