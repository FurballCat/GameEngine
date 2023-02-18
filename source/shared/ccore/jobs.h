/* Copyright (c) Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include <inttypes.h>
#include <stdbool.h>
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
 
 fc_job_decl_t jobDecl = {};
 jobDecl.func = fc_test_job_func;
 jobDecl.data = &myData;
 
 fc_job_counter_t* counter = NULL;
 fc_run_jobs(&jobDecl, 1, &counter);	// at this point the job is scheduled
 
 fc_wait_for_counter_and_free(counter);	// this waits until the job is done
 */

typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;

CCORE_API void fc_job_system_init(fc_alloc_callbacks_t* pAllocCallbacks);
CCORE_API void fc_job_system_release(fc_alloc_callbacks_t* pAllocCallbacks);

typedef void (*fc_job_fn_t)(void* userData);

typedef struct fc_job_decl_t
{
	fc_job_fn_t func;
	void* userData;
} fc_job_decl_t;

typedef struct fc_job_counter_t
{
	fc_atomic_int value;
	int32_t index;
} fc_job_counter_t;

CCORE_API void fc_run_jobs(const fc_job_decl_t* jobs, int32_t numJobs, fc_job_counter_t** outCounter);
CCORE_API void fc_wait_for_counter_and_free(fc_job_counter_t* counter);

// call this on main once at the init, this job will be your main thread loop
void fc_job_system_setup_main_thread_job(const fc_job_decl_t* job);

// call this on main, once called, main thread is now a regular worker thread
CCORE_API void fc_job_system_enter_worker_thread_mode(void);

// call at the end of main, to exit all worker threads, including main-worker thread
CCORE_API void fc_job_system_exit_all_jobs(void);

// this will return index between 0 and num_max_threads, where 0 is the main thread
CCORE_API int32_t fc_job_system_get_this_thread_index(void);

// returns true if this is the main thread, otherwise false
CCORE_API bool fc_job_system_is_main_thread(void);

// this will return number of worker threads + 1 (main thread)
CCORE_API int32_t fc_job_system_num_max_threads(void);

#ifdef __cplusplus
}
#endif // __cplusplus
