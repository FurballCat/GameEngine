/* Copyright (c) Furball Cat */

#include "jobs.h"
#include "memory.h"
#include "furAssert.h"
#include "profiler.h"
#include <immintrin.h>	// for _mm_pause()

#define FUR_NUM_THREADS 5
#define FUR_MAX_JOB_COUNTERS 4096
#define FUR_MAX_JOBS 4096
#define FUR_NUM_SMALL_FIBERS 128
#define FUR_SMALL_FIBER_STACK_MEMORY 128 * 1024

#define thread_local _Thread_local

// fibers implementation
typedef void* fcontext_t;

typedef struct transfer_t
{
	fcontext_t fctx;
	void* data;
} transfer_t;

// these fiber function implementations are in ASM
transfer_t jump_fcontext(fcontext_t const to, void * vp);
fcontext_t make_fcontext(void * sp, u64 size, void (* fn)(transfer_t) );
transfer_t ontop_fcontext(fcontext_t const to, void * vp, transfer_t (*fn)(transfer_t));	// based on an idea of Giovanni Derreta

// free list - locless implementation
typedef struct FcFreeList
{
	FcLockRW lock;
	i32 tail;
	i32* indices;
	i32 capcity;
} FcFreeList;

void fcFreeListAlloc(FcFreeList* list, i32 capacity, const FcAllocator* allocator)
{
	list->capcity = capacity;
	list->tail = capacity-1;
	list->indices = FUR_ALLOC_ARRAY(i32, capacity, 0, FC_MEMORY_SCOPE_JOBS, allocator);
	fcLockRWInit(&list->lock);
	
	for(i32 i=0; i<capacity; ++i)
	{
		list->indices[i] = capacity - 1 - i;
	}
}

void fcFreeListFree(FcFreeList* list, const FcAllocator* allocator)
{
	FUR_FREE(list->indices, allocator);
}

i32 fcFreeListAcquire(FcFreeList* list)
{
	i32 index = -1;
	
	FUR_SCOPED_WRITE_LOCK(list->lock, "free-list")
	{
		const i32 slotIndex = list->tail;
		list->tail--;
		FUR_ASSERT(slotIndex >= 0);
		index = list->indices[slotIndex];
	}
	
	return index;
}

void fcFreeListRelease(FcFreeList* list, i32 index)
{
	FUR_SCOPED_WRITE_LOCK(list->lock, "free-list")
	{
		list->tail++;
		i32 slotIndex = list->tail;
		FUR_ASSERT(slotIndex < list->capcity);
		
		list->indices[slotIndex] = index;
	}
}

typedef struct FcScheduledJob
{
	// user data + function
	FcJobDecl decl;
	
	// counter that will be decremented after this job finishes
	FcJobCounter* counter;
	
	// counter for which this job should wait to be 0 before launching
	FcJobCounter* counterToWaitFor;
	
	// this is a special job, one to rule them all
	bool isMainThreadJob;
} FcScheduledJob;

typedef struct FcJobResult
{
	FcJobCounter* counterToWaitFor;
	bool isFinished;	// if false, then it means it's suspended
} FcJobResult;

typedef struct FcJobSystem
{
	FcThread mainThreadID;
	FcThread threadIDs[FUR_NUM_THREADS];
	i32 threadIndices[FUR_NUM_THREADS];
	
	// counters active and non-active
	FcFreeList countersFreeList;
	FcJobCounter* counters;
	
	// jobs without fiber assigned
	FcFreeList pendingJobsFreeList;
	FcScheduledJob* pendingJobs;
	
	// this is locked between threads
	FcLockRW pendingJobsIndicesLock;
	i32* pendingJobsIndices;
	volatile i32 numPendingJobsIndices;
	
	// active and suspended fiber jobs
	FcFreeList fibersFreeList;
	fcontext_t fibers[FUR_NUM_SMALL_FIBERS];
	FcScheduledJob fiberJobs[FUR_NUM_SMALL_FIBERS];
	
	// suspended fibers wait list
	FcLockRW suspendedFiberIndicesLock;
	i32 suspendedFiberIndices[FUR_NUM_SMALL_FIBERS];
	volatile i32 numSuspendedFiberIndices;
	
	// fibers stack memory (long block for all fibers)
	void* fiberStackMemory;
	
	volatile bool isRunning;
	
} FcJobSystem;

FcJobCounter* fcJobSystemMakeCounter(FcJobSystem* sys, i32 value)
{
	const i32 index = fcFreeListAcquire(&sys->countersFreeList);
	
	FcJobCounter* counter = &sys->counters[index];
	counter->index = index;
	fcAtomicIntStore(&counter->value, value);
	
	return counter;
}

void fcJobSystemReleaseCounter(FcJobSystem* sys, FcJobCounter* counter)
{
	const i32 index = counter->index;
	FUR_ASSERT(fcAtomicIntLoad(&counter->value) == 0);
	
	fcFreeListRelease(&sys->countersFreeList, index);
}

FcJobSystem g_jobSystem;

// a way to go back to worker thread, set only when inside specific job's fiber
FUR_THREAD_LOCAL fcontext_t g_threadLocalGoToWorker;
FUR_THREAD_LOCAL i32 g_threadID;

void fcFiberFunc(transfer_t t)
{
	g_threadLocalGoToWorker = t.fctx;
	
	// calling the main job function
	FcScheduledJob* job = (FcScheduledJob*)t.data;
	job->decl.func(job->decl.userData);
	
	// fiber return value
	FcJobResult result = {NULL, true};
	
	// note: fiber function has to always exit with jump to caller (in this case fcWorkerThreadEvaluate)
	// as beside return instruction we also have to change the context (stack pointer register) in CPU,
	// otherwise we would return to some instruction, but with wrong stack.
	// In case of no jump at the end of function, fcontext assembly code will call exit and halt the program execution.
	jump_fcontext(g_threadLocalGoToWorker, &result);
}

void fcWorkerThreadEvaluate(i32 threadIndex)
{
	const bool isMainThread = (threadIndex == 0);
	
	// check if there is any fiber to resume
	i32 fiberIndexToRun = -1;
	
	FUR_SCOPED_WRITE_LOCK(g_jobSystem.suspendedFiberIndicesLock, "fiber-suspended-fibers-eval")
	{
		for(i32 i=0; i<g_jobSystem.numSuspendedFiberIndices; ++i)
		{
			const i32 fiberIndex = g_jobSystem.suspendedFiberIndices[i];
			
			// skip main thread job on pure worker threads, only main thread can execute main thread job
			if(g_jobSystem.fiberJobs[fiberIndex].isMainThreadJob != isMainThread)
				continue;
			
			// we know that once counter reaches 0, it will stay 0
			if(fcAtomicIntLoad(&g_jobSystem.fiberJobs[fiberIndex].counterToWaitFor->value) == 0)
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
		i32 jobIndexToInit = -1;
		
		FUR_SCOPED_WRITE_LOCK(g_jobSystem.pendingJobsIndicesLock, "fiber-pending-jobs-eval")
		{
			//printf("Thread %i found no fiber to acquire, looking for job\n", threadIndex);
			
			if(g_jobSystem.numPendingJobsIndices > 0)
			{
				const i32 candidateIndex = g_jobSystem.pendingJobsIndices[g_jobSystem.numPendingJobsIndices - 1];
				
				// this condition will skip only at the beginning, as there will be one pending job for main thread
				if(g_jobSystem.pendingJobs[candidateIndex].isMainThreadJob != isMainThread)
				{
					//printf("Thread %i sees main job %i, skipping\n", threadIndex, jobIndexToInit);
					continue;	// func fact: I can use continue here because scoped lock is a for loop
				}
				
				jobIndexToInit = candidateIndex;
				
				g_jobSystem.numPendingJobsIndices -= 1;
				
				//printf("Thread %i acquires job %i\n", threadIndex, jobIndexToInit);
			}
		}
		
		if(jobIndexToInit != -1)
		{
			i32 fiberIndex = -1;
			
			// reserve fiber
			fiberIndex = fcFreeListAcquire(&g_jobSystem.fibersFreeList);
			
			// copy job info and release the slot
			g_jobSystem.fiberJobs[fiberIndex] = g_jobSystem.pendingJobs[jobIndexToInit];
			fcFreeListRelease(&g_jobSystem.pendingJobsFreeList, jobIndexToInit);
			
			// initialise fiber (memory is fiberIndex+1 because stack starts from the other end)
			void* memStack = (u8*)(g_jobSystem.fiberStackMemory) + (fiberIndex+1) * FUR_SMALL_FIBER_STACK_MEMORY;
			g_jobSystem.fibers[fiberIndex] = make_fcontext(memStack, FUR_SMALL_FIBER_STACK_MEMORY, fcFiberFunc);
			
			fiberIndexToRun = fiberIndex;
		}
	}
	
	// run fiber
	if(fiberIndexToRun != -1)
	{
		fcontext_t fiber = g_jobSystem.fibers[fiberIndexToRun];
		
		// jump to fiber (either beginning or in the middle)
		transfer_t fiberTransfer = jump_fcontext(fiber, &g_jobSystem.fiberJobs[fiberIndexToRun]);
		FcJobResult* result = (FcJobResult*)fiberTransfer.data;
		
		// sanity check, outside fiber this variable should be NULL
		//FUR_ASSERT(g_threadLocalGoToWorker == NULL);
		
		// suspend if not finished
		if(result->isFinished == false)
		{
			FUR_ASSERT(result->counterToWaitFor);
			
			g_jobSystem.fibers[fiberIndexToRun] = fiberTransfer.fctx;
			g_jobSystem.fiberJobs[fiberIndexToRun].counterToWaitFor = result->counterToWaitFor;
			
			// put fiber on wait list
			FUR_SCOPED_WRITE_LOCK(g_jobSystem.suspendedFiberIndicesLock, "fiber-suspended-fibers")
			{
				const i32 slotIndex = g_jobSystem.numSuspendedFiberIndices;
				FUR_ASSERT(slotIndex < FUR_NUM_SMALL_FIBERS);
				
				g_jobSystem.suspendedFiberIndices[slotIndex] = fiberIndexToRun;
				g_jobSystem.numSuspendedFiberIndices += 1;
			}
		}
		else // if finished, decrement the counter
		{
			FUR_ASSERT(g_jobSystem.fiberJobs[fiberIndexToRun].counter);
			
			// decrement counter
			fcAtomicIntFetchSub(&g_jobSystem.fiberJobs[fiberIndexToRun].counter->value, 1);
			
			// release fiber
			fcFreeListRelease(&g_jobSystem.fibersFreeList, fiberIndexToRun);
		}
	}
	
	// if there's nothing to resome and nothing pending, then put on sleep
	if(fiberIndexToRun == -1)
	{
		// hint CPU that we're spinning
		_mm_pause();
	}
}

void* fcThreadFunc(void* vargp)
{
	g_threadID = *((const i32*)vargp);
	
	while(g_jobSystem.isRunning)
	{
		fcWorkerThreadEvaluate(g_threadID);
	}
	
	return NULL;
}

i32 fcJobSystemGetThisThreadIndex(void)
{
	return g_threadID;
}

bool fcJobSystemIsMainThread(void)
{
	return fcJobSystemGetThisThreadIndex() == 0;
}

i32 fcJobSystemNumMaxThreads(void)
{
	return FUR_NUM_THREADS + 1;
}

void fcJobSystemInit(const FcAllocator* allocator)
{
	// allocate memory for job system
	g_jobSystem.counters = FUR_ALLOC_ARRAY_AND_ZERO(FcJobCounter, FUR_MAX_JOB_COUNTERS, 0, FC_MEMORY_SCOPE_JOBS, allocator);
	g_jobSystem.pendingJobs = FUR_ALLOC_ARRAY_AND_ZERO(FcScheduledJob, FUR_MAX_JOBS, 0, FC_MEMORY_SCOPE_JOBS, allocator);
	
	fcFreeListAlloc(&g_jobSystem.countersFreeList, FUR_MAX_JOB_COUNTERS, allocator);
	fcFreeListAlloc(&g_jobSystem.pendingJobsFreeList, FUR_MAX_JOBS, allocator);
	fcFreeListAlloc(&g_jobSystem.fibersFreeList, FUR_NUM_SMALL_FIBERS, allocator);
	
	g_jobSystem.pendingJobsIndices = FUR_ALLOC_ARRAY_AND_ZERO(i32, FUR_MAX_JOBS, 0, FC_MEMORY_SCOPE_JOBS, allocator);
	
	g_jobSystem.fiberStackMemory = FUR_ALLOC_AND_ZERO(FUR_NUM_SMALL_FIBERS * FUR_SMALL_FIBER_STACK_MEMORY, 16, FC_MEMORY_SCOPE_JOBS, allocator);
	
	// when this flag will become false, all worker threads will exit as soon as possible
	g_jobSystem.isRunning = true;
	
	fcLockRWInit(&g_jobSystem.pendingJobsIndicesLock);
	fcLockRWInit(&g_jobSystem.suspendedFiberIndicesLock);
	
	// mac note: I'm not sure mac is actually taking my affinity requests into account
	
	// set main thread affinity
	{
		g_jobSystem.mainThreadID = fcThreadSelf();
		fcThreadSetAffinity(g_jobSystem.mainThreadID, 0);
	}
	
	// start worker threads
	for(i32 i=0; i<FUR_NUM_THREADS; ++i)
	{
		g_jobSystem.threadIndices[i] = i+1;
		i32 res = fcThreadCreateSuspended(&g_jobSystem.threadIDs[i], fcThreadFunc, &g_jobSystem.threadIndices[i]);
		FUR_ASSERT(res == 0);
		
		fcThreadSetAffinity(g_jobSystem.threadIDs[i], i+1);
		fcThreadResume(g_jobSystem.threadIDs[i]);
	}
}

void fcJobSystemRelease(const FcAllocator* allocator)
{
	g_jobSystem.isRunning = false;
	
	for(i32 i=0; i<FUR_NUM_THREADS; ++i)
	{
		fcThreadJoin(g_jobSystem.threadIDs[i]);
	}
	
	FUR_FREE(g_jobSystem.fiberStackMemory, allocator);
	
	FUR_FREE(g_jobSystem.pendingJobsIndices, allocator);
	
	fcFreeListFree(&g_jobSystem.countersFreeList, allocator);
	fcFreeListFree(&g_jobSystem.pendingJobsFreeList, allocator);
	fcFreeListFree(&g_jobSystem.fibersFreeList, allocator);
	
	FUR_FREE(g_jobSystem.pendingJobs, allocator);
	FUR_FREE(g_jobSystem.counters, allocator);
}

void fcRunJobsInternal(const FcJobDecl* jobs, i32 numJobs, FcJobCounter** outCounter, bool isMainThread)
{
	FcJobCounter* counter = fcJobSystemMakeCounter(&g_jobSystem, numJobs);
	*outCounter = counter;
	
	FUR_ASSERT(numJobs < 64);	// if you reach 64, then consider increasing the jobsBatchIndices size
	i32 jobsBatchIndices[64];
	
	for(i32 i=0; i<numJobs; ++i)
	{
		const i32 jobIndex = fcFreeListAcquire(&g_jobSystem.pendingJobsFreeList);
		FcScheduledJob* scheduledJob = &g_jobSystem.pendingJobs[jobIndex];
		
		scheduledJob->decl = jobs[i];
		scheduledJob->counter = counter;
		scheduledJob->counterToWaitFor = NULL;
		scheduledJob->isMainThreadJob = isMainThread;
		
		jobsBatchIndices[i] = jobIndex;
	}
	
	// add all jobs indices to pending list
	FUR_SCOPED_WRITE_LOCK(g_jobSystem.pendingJobsIndicesLock, "fiber-pending-jobs")
	{
		const i32 startIndex = g_jobSystem.numPendingJobsIndices;
		FUR_ASSERT(startIndex + numJobs <= FUR_MAX_JOBS);
		
		for(i32 i=0; i<numJobs; ++i)
		{
			g_jobSystem.pendingJobsIndices[startIndex + i] = jobsBatchIndices[i];
		}
		
		g_jobSystem.numPendingJobsIndices += numJobs;
	}
}

void fcJobSystemSetupMainThreadJob(const FcJobDecl* job)
{
	FcJobCounter* counter = NULL;
	fcRunJobsInternal(job, 1, &counter, true);
}

void fcJobSystemEnterWorkerThreadMode(void)
{
	i32 mainThreadIndex = 0;
	fcThreadFunc(&mainThreadIndex);
}

void fcJobSystemExitAllJobs(void)
{
	g_jobSystem.isRunning = false;
}

void fcRunJobs(const FcJobDecl* jobs, i32 numJobs, FcJobCounter** outCounter)
{
	fcRunJobsInternal(jobs, numJobs, outCounter, false);
}

void fcWaitForCounterAndFree(FcJobCounter* counter)
{
	// if counnter is not 0, then suspend the fiber
	if(fcAtomicIntLoad(&counter->value) != 0)
	{
#if FUR_USE_PROFILER
		// store profiler state before jump
		FcProfilerScope* stack[32];
		const i32 numStack = fcProfilerStoreScopestack(stack);
#endif
		
		// return from fiber into worker thread function
		FcJobResult result = {0};
		result.isFinished = false;
		result.counterToWaitFor = counter;
		
		fcontext_t fctx = g_threadLocalGoToWorker;
		g_threadLocalGoToWorker = NULL;
		transfer_t transfer = jump_fcontext(fctx, &result);
		g_threadLocalGoToWorker = transfer.fctx;
		
#if FUR_USE_PROFILER
		// load profiler state after jump
		fcProfilerLoadScopestack(stack, numStack);
#endif
	}
	
	// otherwise release the counter and continue
	fcJobSystemReleaseCounter(&g_jobSystem, counter);
}
