#include "pch.h"
#include "fibers.h"

using namespace jobs;

enum class JobState
{
    Finished,
    Suspended,
};

struct JobResult
{
    Counter* m_counterToWaitFor;
    JobState m_state;
};

struct ScheduledJob
{
    JobDesc m_desc;
    Counter* m_counter = nullptr;
	Counter* m_counterToWaitFor = nullptr;
	bool m_isMainGameLoop = false;
};

namespace
{
    constexpr uint32 c_numAvailableSmallFibers = 128;
    constexpr uint32 c_numAvailableBigFibers = 32;
    constexpr uint32 c_smallFiberStackMemorySize = 64_kb;
    constexpr uint32 c_bigfibersStackMemorySize = 512_kb;
    constexpr uint32 c_allFibersStackMemorySize = c_numAvailableSmallFibers * c_smallFiberStackMemorySize + c_numAvailableBigFibers * c_bigfibersStackMemorySize;
    
    SpinLock g_pendingJobsLock;
    DynArray<ScheduledJob> g_pendingJobs;
    
    SpinLock g_waitListLock;
    DynArray<uint32> g_waitList;
    fcontext_t g_fiberStates[c_numAvailableSmallFibers];
    ScheduledJob g_fiberJobs[c_numAvailableSmallFibers];
    
    uint8* g_fibersStackMemory;
    volatile bool g_jobSystemRunning = false;
    
    SpinLock g_fiberListLock;
    uint32 g_freeSmallFibersListTail = c_numAvailableSmallFibers-1;
    uint32 g_freeSmallFibersList[c_numAvailableSmallFibers];
    
    constexpr uint32 c_numDedicatedWorkerThreads = 6;
	constexpr uint32 c_numAllAvailableThreads = c_numDedicatedWorkerThreads + 1;	// +1 as app thread can also be worker thread
    
    std::thread g_threads[c_numDedicatedWorkerThreads];
}

namespace utils
{
    inline uint8* GetSmallStackAddress(uint32 fiberIndex)
    {
        return g_fibersStackMemory + fiberIndex * c_smallFiberStackMemorySize;
    }
    
    inline uint32 AcquireFiberIndex()
    {
        ScopedLock<SpinLock> lock(g_fiberListLock);
        ASSERT(g_freeSmallFibersListTail > 0);
        g_freeSmallFibersListTail--;
        return g_freeSmallFibersList[g_freeSmallFibersListTail];
    }
    
    inline void ReleaseFiberIndex(uint32 fiberIndex)
    {
        ScopedLock<SpinLock> lock(g_fiberListLock);
        ASSERT(g_freeSmallFibersListTail < c_numAvailableSmallFibers - 1);
        g_freeSmallFibersList[g_freeSmallFibersListTail] = fiberIndex;
        g_freeSmallFibersListTail++;
    }
}

JOBS_API void jobs::RunJobs(const JobDesc* jobs, uint32 numJobs, Counter** outCounter)
{
    Counter* counter = nullptr;
    if(outCounter)
    {
        counter = new Counter(numJobs); // todo: should be allocated by pool allocator
        *outCounter = counter;
    }
    
    ScopedLock<SpinLock> lock(g_pendingJobsLock);
    for(uint32 i=0; i<numJobs; ++i)
    {
        ScheduledJob job;
        job.m_desc = jobs[i];
        job.m_counter = counter;
        
        g_pendingJobs.push_back(job);
    }
}

JOBS_API void jobs::RunMainGameLoopJob(const JobDesc& job)
{
	ScopedLock<SpinLock> lock(g_pendingJobsLock);
	
	ScheduledJob scheduledJob;
	scheduledJob.m_desc = job;
	scheduledJob.m_isMainGameLoop = true;
		
	g_pendingJobs.push_back(scheduledJob);
}

void FiberFunc(transfer_t t)
{
    // calling the main job function
    ScheduledJob* job = reinterpret_cast<ScheduledJob*>(t.data);
    jobs::Context context = {t, job->m_desc.m_userData};
    job->m_desc.m_jobFunc(context);
    
    // fiber return value
    JobResult result = {nullptr, JobState::Finished};
    
    // note: fiber function has to always exit with jump to caller (in this case FiberMain)
    // as beside return instruction we also have to change the context (stack pointer register) in CPU,
    // otherwise we would return to some instruction, but with wrong stack.
    // In case of no jump at the end of function, fcontext assembly code will call exit and halt the program execution.
    jump_fcontext( context.m_transfer.fctx, &result);
}

JOBS_API void jobs::WorkerThreadOneIteration(uint32 threadIndex)
{
	uint32 fiberIndex = 0;
	bool fiberReady = false;
	
	// check if there is any fiber to resume
	{
		ScopedLock<SpinLock> lock(g_waitListLock);
		for(auto it = g_waitList.begin(); it != g_waitList.end(); it++)
		{
			uint32 id = *it;
			
			// skip main game loop on app thread to avoid endless loop
			// todo: refactor that, we can track this fiber seperately, it will live forever
			if(threadIndex == jobs::GetAppThreadIndex() && g_fiberJobs[id].m_isMainGameLoop)
				continue;
			
			FUR_ASSERT(g_fiberJobs[id].m_counterToWaitFor);
			if(*g_fiberJobs[id].m_counterToWaitFor == 0)
			{
				// then take it and check it's resume condition (counter == 0)
				fiberIndex = *it;
				g_waitList.erase(it);
				fiberReady = true;
				break;
			}
		}
	}
	
	// if no fiber ready to resume, then get next pending job and acquire fiber for it
	if(!fiberReady)
	{
		ScheduledJob job;
		
		ScopedLock<SpinLock> lock(g_pendingJobsLock);
		if(!g_pendingJobs.empty())
		{
			job = g_pendingJobs.back();
			g_pendingJobs.pop_back();
			
			// acquire fiber and it's stack memory
			fiberIndex = utils::AcquireFiberIndex();
			uint8* stack = utils::GetSmallStackAddress(fiberIndex);
			
			// preparing fiber context on the given stack
			fcontext_t fiber = make_fcontext( stack, c_smallFiberStackMemorySize, FiberFunc);
			
			g_fiberJobs[fiberIndex] = job;
			g_fiberStates[fiberIndex] = fiber;
			
			fiberReady = true;
		}
	}
	
	// start or resume fiber
	if(fiberReady)
	{
		fcontext_t fiber = g_fiberStates[fiberIndex];
		
		// jump to fiber
		transfer_t fiberTransfer = jump_fcontext(fiber, &g_fiberJobs[fiberIndex]);
		
		const JobResult* result = reinterpret_cast<const JobResult*>(fiberTransfer.data);
		if(result->m_state == JobState::Suspended)
		{
			g_fiberStates[fiberIndex] = fiberTransfer.fctx;
			g_fiberJobs[fiberIndex].m_counterToWaitFor = result->m_counterToWaitFor;
			ScopedLock<SpinLock> lock(g_waitListLock);
			g_waitList.push_back(fiberIndex);
		}
		else
		{
			ScheduledJob& job = g_fiberJobs[fiberIndex];
			if(job.m_counter)
			{
				(*job.m_counter)--;
			}
			utils::ReleaseFiberIndex(fiberIndex);
		}
	}
}

void WorkerThread(uint32 threadIndex)
{
    while(g_jobSystemRunning)
    {
		WorkerThreadOneIteration(threadIndex);
	}
}

JOBS_API void jobs::InitializeJobSystem()
{
    // allocate fibers' stack memory
    ASSERT(g_fibersStackMemory == nullptr);
    g_fibersStackMemory = mem::Allocate<mem::Tag::Jobs>(c_allFibersStackMemorySize);
	
    // initialize free fibers list (it's technically a stack, but to avoid confusion it's named a 'list')
    for(uint32 i=0; i<c_numAvailableSmallFibers; ++i)
    {
        g_freeSmallFibersList[i] = i;
    }
	
    g_freeSmallFibersListTail = c_numAvailableSmallFibers-1;
	
    // set finishing flag, if this will change to false in future, all worker threads will end their loop
    g_jobSystemRunning = true;
    
    // todo: assign threads to cores explicitly
    for(uint32 i=0; i<c_numDedicatedWorkerThreads; ++i)
    {
        g_threads[i] = std::thread(WorkerThread, i+1);	// +1 because app thread index is 0, so worker threads start from 1
    }
}

JOBS_API void jobs::DeinitializeJobSystem()
{
    ASSERT(g_fibersStackMemory != nullptr);
    mem::Free<mem::Tag::Jobs>(g_fibersStackMemory);
    g_fibersStackMemory = nullptr;
    
    g_jobSystemRunning = false;
    
    for(uint32 i=0; i<c_numDedicatedWorkerThreads; ++i)
    {
        g_threads[i].join();
    }
}

JOBS_API void jobs::WaitForCounterAndFree(jobs::Context& context, Counter* counter)
{
    if(*counter > 0)
    {
        JobResult result = {counter, JobState::Suspended};
        context.m_transfer = jump_fcontext(context.m_transfer.fctx, &result);
    }
	
	FUR_ASSERT(*counter == 0);
	delete counter;
}

JOBS_API uint32 jobs::GetNumThreads()
{
	return c_numAllAvailableThreads;
}

JOBS_API uint32 jobs::GetAppThreadIndex()
{
	return 0;
}










