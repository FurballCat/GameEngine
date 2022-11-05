/* Copyright (c) Furball Cat */

#include "jobs.h"
#include "memory.h"
#include "furAssert.h"
#include <stdatomic.h>

#define FUR_NUM_THREADS 5
#define FUR_MAX_JOB_COUNTERS 4096
#define FUR_MAX_JOBS 4096
#define FUR_NUM_SMALL_FIBERS 128
#define FUR_SMALL_FIBER_STACK_MEMORY 64 * 1024

typedef struct fc_free_list_lockless
{
	atomic_int tail;
	int32_t* indices;
	int32_t capcity;
} fc_free_list_lockless;

void fc_free_list_alloc(fc_free_list_lockless* list, int32_t capacity, fc_alloc_callbacks_t* pAllocCallbacks)
{
	list->capcity = capacity;
	atomic_store(&list->tail, capacity-1);
	list->indices = FUR_ALLOC_ARRAY(int32_t, capacity, 0, FC_MEMORY_SCOPE_JOBS, pAllocCallbacks);
	
	for(int32_t i=0; i<capacity; ++i)
	{
		list->indices[i] = capacity - 1 - i;
	}
}

void fc_free_list_free(fc_free_list_lockless* list, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(list->indices, pAllocCallbacks);
}

int32_t fc_free_list_acquire(fc_free_list_lockless* list)
{
	int32_t slotIndex = atomic_fetch_sub(&list->tail, 1);
	FUR_ASSERT(slotIndex >= 0);
	return list->indices[slotIndex];
}

void fc_free_list_release(fc_free_list_lockless* list, int32_t index)
{
	int32_t slotIndex = atomic_fetch_add(&list->tail, 1);
	FUR_ASSERT(slotIndex < list->capcity);
	list->indices[slotIndex] = index;
}

typedef struct fc_scheduled_job_t
{
	// user data + function
	fc_job_decl_t decl;
	
	// counter that will be decremented after this job finishes
	fc_job_counter_t* counter;
	
	// counter for which this job should wait to be 0 before launching
	fc_job_counter_t* counterToWaitFor;
	
	// this is a special job, one to rule them all
	bool isMainThreadJob;
} fc_scheduled_job_t;

typedef struct fc_job_result_t
{
	fc_job_counter_t* counterToWaitFor;
	bool isFinished;	// if false, then it means it's suspended
} fc_job_result_t;

typedef struct fc_job_system_t
{
	pthread_t mainThreadID;
	pthread_t threadIDs[FUR_NUM_THREADS];
	int32_t threadIndices[FUR_NUM_THREADS];
	
	// counters active and non-active
	fc_free_list_lockless countersFreeList;
	fc_job_counter_t* counters;
	bool* isCounterUsed;	// todo: this could be a free list
	
	// jobs without fiber assigned
	fc_free_list_lockless pendingJobsFreeList;
	fc_scheduled_job_t* pendingJobs;
	
	// this is locked between threads
	fc_rwlock_t pendingJobsIndicesLock;
	int32_t* pendingJobsIndices;
	int32_t numPendingJobsIndices;
	
	// active and suspended fiber jobs
	fc_free_list_lockless fibersFreeList;
	fcontext_t fibers[FUR_NUM_SMALL_FIBERS];
	fc_scheduled_job_t fiberJobs[FUR_NUM_SMALL_FIBERS];
	
	// suspended fibers wait list
	fc_rwlock_t suspendedFiberIndicesLock;
	int32_t suspendedFiberIndices[FUR_NUM_SMALL_FIBERS];
	int32_t numSuspendedFiberIndices;
	
	// fibers stack memory (long block for all fibers)
	void* fiberStackMemory;
	
	volatile bool isRunning;
	
} fc_job_system_t;

fc_job_counter_t* fc_job_system_make_counter(fc_job_system_t* sys, int32_t value)
{
	const int32_t index = fc_free_list_acquire(&sys->countersFreeList);
	
	fc_job_counter_t* counter = &sys->counters[index];
	counter->index = index;
	atomic_store(&counter->value, value);
	
	return counter;
}

void fc_job_system_release_counter(fc_job_system_t* sys, fc_job_counter_t* counter)
{
	const int32_t index = counter->index;
	FUR_ASSERT(atomic_load(&counter->value) == 0);
	
	fc_free_list_release(&sys->countersFreeList, index);
}

fc_job_system_t g_jobSystem;

void fc_fiber_func(transfer_t t)
{
	// calling the main job function
	fc_scheduled_job_t* job = (fc_scheduled_job_t*)t.data;
	fc_job_context_t ctx = {t, job->decl.userData};
	job->decl.func(&ctx);
	
	// fiber return value
	fc_job_result_t result = {NULL, true};
	
	// note: fiber function has to always exit with jump to caller (in this case fc_worker_thread_evaluate)
	// as beside return instruction we also have to change the context (stack pointer register) in CPU,
	// otherwise we would return to some instruction, but with wrong stack.
	// In case of no jump at the end of function, fcontext assembly code will call exit and halt the program execution.
	jump_fcontext(ctx.transfer.fctx, &result);
}

void fc_worker_thread_evaluate(int32_t threadIndex)
{
	const bool isMainThread = (threadIndex == 0);
	
	// check if there is any fiber to resume
	int32_t fiberIndexToRun = -1;
	
	FUR_SCOPED_WRITE_LOCK(g_jobSystem.suspendedFiberIndicesLock)
	{
		for(int32_t i=0; i<g_jobSystem.numSuspendedFiberIndices; ++i)
		{
			const int32_t fiberIndex = g_jobSystem.suspendedFiberIndices[i];
			
			// skip main thread job on pure worker threads, only main thread can execute main thread job
			if(g_jobSystem.fiberJobs[fiberIndex].isMainThreadJob != isMainThread)
				continue;
			
			// we know that once counter reaches 0, it will stay 0
			if(atomic_load(&g_jobSystem.fiberJobs[fiberIndex].counterToWaitFor->value) == 0)
			{
				// replace remove this index from suspended list
				if(i+1 < g_jobSystem.numSuspendedFiberIndices)
				{
					g_jobSystem.suspendedFiberIndices[i] = g_jobSystem.suspendedFiberIndices[g_jobSystem.numSuspendedFiberIndices-1];
				}
				g_jobSystem.numSuspendedFiberIndices -= 1;
				
				// return fiber index to resume
				fiberIndexToRun = fiberIndex;
				break;
			}
		}
	}
	
	// acquire new job to run if there's no fiber to resume
	if(fiberIndexToRun == -1)
	{
		int32_t jobIndexToInit = -1;
		
		FUR_SCOPED_WRITE_LOCK(g_jobSystem.pendingJobsIndicesLock)
		{
			if(g_jobSystem.numPendingJobsIndices > 0)
			{
				jobIndexToInit = g_jobSystem.numPendingJobsIndices - 1;
				g_jobSystem.numPendingJobsIndices -= 1;
			}
		}
		
		if(jobIndexToInit != -1)
		{
			// reserve fiber
			const int32_t fiberIndex = fc_free_list_acquire(&g_jobSystem.fibersFreeList);
			
			// copy job info and release the slot
			g_jobSystem.fiberJobs[fiberIndex] = g_jobSystem.pendingJobs[jobIndexToInit];
			fc_free_list_release(&g_jobSystem.pendingJobsFreeList, jobIndexToInit);
			
			// initialise fiber
			void* memStack = g_jobSystem.fiberStackMemory + fiberIndex * FUR_SMALL_FIBER_STACK_MEMORY;
			g_jobSystem.fibers[fiberIndex] = make_fcontext(memStack, FUR_SMALL_FIBER_STACK_MEMORY, fc_fiber_func);
			
			fiberIndexToRun = fiberIndex;
		}
	}
	
	// run fiber
	if(fiberIndexToRun != -1)
	{
		fcontext_t fiber = g_jobSystem.fibers[fiberIndexToRun];
		
		// jump to fiber
		transfer_t fiberTransfer = jump_fcontext(fiber, &g_jobSystem.fiberJobs[fiberIndexToRun]);
		fc_job_result_t* result = (fc_job_result_t*)fiberTransfer.data;
		
		// suspend if not finished
		if(result->isFinished == false)
		{
			FUR_ASSERT(result->counterToWaitFor);
			
			g_jobSystem.fibers[fiberIndexToRun] = fiberTransfer.fctx;
			g_jobSystem.fiberJobs[fiberIndexToRun].counterToWaitFor = result->counterToWaitFor;
			
			// put fiber on wait list
			FUR_SCOPED_WRITE_LOCK(g_jobSystem.suspendedFiberIndicesLock)
			{
				const int32_t slotIndex = g_jobSystem.numSuspendedFiberIndices;
				FUR_ASSERT(slotIndex < FUR_NUM_SMALL_FIBERS);
				
				g_jobSystem.suspendedFiberIndices[slotIndex] = fiberIndexToRun;
				g_jobSystem.numSuspendedFiberIndices += 1;
			}
		}
		else // if finished, decrement the counter
		{
			FUR_ASSERT(g_jobSystem.fiberJobs[fiberIndexToRun].counter);
			
			// decrement counter
			atomic_fetch_sub(&g_jobSystem.fiberJobs[fiberIndexToRun].counter->value, 1);
			
			// release fiber
			fc_free_list_release(&g_jobSystem.fibersFreeList, fiberIndexToRun);
		}
	}
	
	// if there's nothing to resome and nothing pending, then put on sleep
	if(fiberIndexToRun == -1)
	{
		// todo: put on sleep
	}
}

void* fc_thread_func(void* vargp)
{
	const int32_t threadIndex = *((const int32_t*)vargp);
	
	while(g_jobSystem.isRunning)
	{
		fc_worker_thread_evaluate(threadIndex);
	}
	
	return NULL;
}

int32_t fc_job_system_get_this_thread_index(void)
{
	pthread_t id = pthread_self();
	
	if(g_jobSystem.mainThreadID == id)
		return 0;
	
	for(int32_t i=0; i<FUR_NUM_THREADS; ++i)
	{
		if(g_jobSystem.threadIDs[i] == id)
			return i;
	}
	
	FUR_ASSERT(false);	// wait... who dis?
	return -1;
}

int32_t fc_job_system_num_max_threads(void)
{
	return FUR_NUM_THREADS + 1;
}

void fc_job_system_init(fc_alloc_callbacks_t* pAllocCallbacks)
{
	// allocate memory for job system
	g_jobSystem.counters = FUR_ALLOC_ARRAY_AND_ZERO(fc_job_counter_t, FUR_MAX_JOB_COUNTERS, 0, FC_MEMORY_SCOPE_JOBS, pAllocCallbacks);
	g_jobSystem.isCounterUsed = FUR_ALLOC_ARRAY_AND_ZERO(bool, FUR_MAX_JOB_COUNTERS, 0, FC_MEMORY_SCOPE_JOBS, pAllocCallbacks);
	g_jobSystem.pendingJobs = FUR_ALLOC_ARRAY_AND_ZERO(fc_scheduled_job_t, FUR_MAX_JOBS, 0, FC_MEMORY_SCOPE_JOBS, pAllocCallbacks);
	
	fc_free_list_alloc(&g_jobSystem.countersFreeList, FUR_MAX_JOB_COUNTERS, pAllocCallbacks);
	fc_free_list_alloc(&g_jobSystem.pendingJobsFreeList, FUR_MAX_JOBS, pAllocCallbacks);
	fc_free_list_alloc(&g_jobSystem.fibersFreeList, FUR_NUM_SMALL_FIBERS, pAllocCallbacks);
	
	g_jobSystem.fiberStackMemory = FUR_ALLOC_AND_ZERO(FUR_NUM_SMALL_FIBERS * FUR_SMALL_FIBER_STACK_MEMORY, 0, FC_MEMORY_SCOPE_JOBS, pAllocCallbacks);
	
	// when this flag will become false, all worker threads will exit as soon as possible
	g_jobSystem.isRunning = true;
	
	g_jobSystem.mainThreadID = pthread_self();
	
	// start worker threads
	for(int32_t i=0; i<FUR_NUM_THREADS; ++i)
	{
		g_jobSystem.threadIndices[i] = i+1;
		pthread_create(&g_jobSystem.threadIDs[i], NULL, fc_thread_func, &g_jobSystem.threadIndices[i]);
	}
}

void fc_job_system_release(fc_alloc_callbacks_t* pAllocCallbacks)
{
	g_jobSystem.isRunning = false;
	
	for(int32_t i=0; i<FUR_NUM_THREADS; ++i)
	{
		pthread_join(g_jobSystem.threadIDs[i], NULL);
	}
	
	FUR_FREE(g_jobSystem.fiberStackMemory, pAllocCallbacks);
	
	fc_free_list_free(&g_jobSystem.countersFreeList, pAllocCallbacks);
	fc_free_list_free(&g_jobSystem.pendingJobsFreeList, pAllocCallbacks);
	fc_free_list_free(&g_jobSystem.fibersFreeList, pAllocCallbacks);
	
	FUR_FREE(g_jobSystem.pendingJobs, pAllocCallbacks);
	FUR_FREE(g_jobSystem.counters, pAllocCallbacks);
	FUR_FREE(g_jobSystem.isCounterUsed, pAllocCallbacks);
}

void fc_run_jobs_internal(const fc_job_decl_t* jobs, int32_t numJobs, fc_job_counter_t** outCounter, bool isMainThread)
{
	fc_job_counter_t* counter = fc_job_system_make_counter(&g_jobSystem, numJobs);
	*outCounter = counter;
	
	FUR_ASSERT(numJobs < 64);	// if you reach 64, then consider increasing the jobsBatchIndices size
	int32_t jobsBatchIndices[64];
	
	for(int32_t i=0; i<numJobs; ++i)
	{
		const int32_t jobIndex = fc_free_list_acquire(&g_jobSystem.pendingJobsFreeList);
		fc_scheduled_job_t* scheduledJob = &g_jobSystem.pendingJobs[jobIndex];
		
		scheduledJob->decl = jobs[i];
		scheduledJob->counter = counter;
		scheduledJob->counterToWaitFor = NULL;
		scheduledJob->isMainThreadJob = isMainThread;
		
		jobsBatchIndices[i] = jobIndex;
	}
	
	// add all jobs indices to pending list
	FUR_SCOPED_WRITE_LOCK(g_jobSystem.pendingJobsIndicesLock)
	{
		const int32_t startIndex = g_jobSystem.numPendingJobsIndices;
		FUR_ASSERT(startIndex + numJobs <= FUR_MAX_JOBS);
		
		for(int32_t i=0; i<numJobs; ++i)
		{
			g_jobSystem.pendingJobsIndices[startIndex + i] = jobsBatchIndices[i];
		}
	}
}

void fc_job_system_setup_main_thread_job(const fc_job_decl_t* job)
{
	fc_job_counter_t* counter = NULL;
	fc_run_jobs_internal(job, 1, &counter, true);
}

void fc_job_system_enter_worker_thread_mode()
{
	int32_t mainThreadIndex = 0;
	fc_thread_func(&mainThreadIndex);
}

void fc_run_jobs(const fc_job_decl_t* jobs, int32_t numJobs, fc_job_counter_t** outCounter)
{
	fc_run_jobs_internal(jobs, numJobs, outCounter, false);
}

void fc_wait_for_counter_and_free(fc_job_context_t* ctx, fc_job_counter_t* counter)
{
	// if counnter is not 0, then suspend the fiber
	if(atomic_load(&counter->value) != 0)
	{
		fc_job_result_t result = {};
		result.isFinished = false;
		result.counterToWaitFor = counter;
		
		ctx->transfer = jump_fcontext(ctx->transfer.fctx, &result);
	}
	
	// otherwise release the counter and continue
	fc_job_system_release_counter(&g_jobSystem, counter);
}
