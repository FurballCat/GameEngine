#pragma once
#include "fcontext.h"

/* How it works:
 Schedule jobs with RunJobs function. This will push given jobs to the queue. There are 3 queues: low, medium and high priority.
 
 Thread function:
    1. Check if there is any fiber to finish, if so, check it's resume condition. If it's met, then resume fiber.
        Finish or suspend the fiber and start over again.
    2. Check if there is any job to do, if so then take it from the queue. If no, then start over again.
    3. Check if there is any free fiber, if so then take it and launch the job. If no, then assert.
    4. Finish or suspend the fiber and start over again.
 */

namespace jobs
{
    // counters (can be allocated by pool allocator)
    typedef std::atomic<uint32> Counter;
    
    struct JOBS_API Context
    {
        transfer_t m_transfer;
        void* m_userData;
    };
    
    // job description, takes fiber function and user data pointer as parameters
    // you can schedule this job by calling RunJobs(&job, 1)
    // you can create jobs descriptions on stack
    struct JOBS_API JobDesc
    {
        bool IsValid() const { return m_jobFunc != nullptr; }
        
        void (*m_jobFunc)(Context&);
        void* m_userData;
    };
    
    // jobs - job descriptors, numJobs - size od jobs array, optional counter if you want to wait for jobs to finish
	JOBS_API void RunJobs(const JobDesc* jobs, uint32 numJobs, Counter** outCounter = nullptr);
	
	// there's only one main game loop job and it cannot be executed on app thread
	JOBS_API void RunMainGameLoopJob(const JobDesc& job);
	
	JOBS_API void WaitForCounterAndFree(Context& context, Counter* counter);
    
    JOBS_API void InitializeJobSystem();
	JOBS_API void DeinitializeJobSystem();
	
	JOBS_API uint32 GetNumThreads();
	JOBS_API uint32 GetAppThreadIndex();
	
	JOBS_API void WorkerThreadOneIteration(uint32 threadIndex);
}

#define FIBER_DECLARE_FUNC(_funcName) void _funcName(transfer_t _t);
#define FIBER_DEFINE_FUNC(_funcName) void _funcName(transfer_t _t)
#define FIBER_WAIT_FOR() jump_fcontext(_t.fctx, _t.data);

#define FIBER_USER_DATA(_type) reinterpret_cast<_type*>(_t.data);
