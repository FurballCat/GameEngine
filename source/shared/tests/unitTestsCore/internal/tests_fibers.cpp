#include "pch.h"
#include "unitTests/unitTestsFramework.h"
#include "core/api.h"
#include "core/types.h"
#include "core/threads.h"
#include "jobs/public.h"
#include "memory/memory.h"

using namespace test;
using namespace mem;

void FiberSimple(transfer_t t)
{
    uint32* userData = reinterpret_cast<uint32*>(t.data);
    
    (*userData)++;
    
    // Note: fiber function has to always exit with jump to caller (in this case FiberMain)
    // as beside return instruction we also have to change the context (stack pointer register) in CPU,
    // otherwise we would return to some instruction, but with wrong stack.
    // In case of no jump at the end of function, fcontext assembly code will call exit and halt the program execution.
    jump_fcontext( t.fctx, t.data);
}

UNITTEST(FiberCore, SimpleFiberCall)
{
    uint32 userData = 0;
    
    // Creating a stack
    uint32 stackSize = 64_kb;
    uint8* stack = mem::Allocate<mem::Tag::Game>(stackSize);
    
    // Preparing fiber context on the given stack
    fcontext_t ctx = make_fcontext( stack, stackSize, FiberSimple);
    
    // Jumping to ctx context (FiberA with the current state of the context, which is just the beggining of the function)
    jump_fcontext(ctx, &userData);
    
    // Free allocated stack
    mem::Free<mem::Tag::Game>(stack);
    
    Assert::AreEqual(userData, 1);
}

void FiberTwoStages(transfer_t t)
{
    uint32* userData = reinterpret_cast<uint32*>(t.data);
    
    (*userData)++;
    
    jump_fcontext( t.fctx, t.data);
    
    (*userData) += 4;
    
    jump_fcontext( t.fctx, t.data);
}

UNITTEST(FiberCore, TwoStagesFiber)
{
    uint32 userData = 0;
    
    // Creating a stack
    uint32 stackSize = 64_kb;
    uint8* stack = mem::Allocate<mem::Tag::Game>(stackSize);
    
    // Preparing fiber context on the given stack
    fcontext_t ctx = make_fcontext( stack, stackSize, FiberTwoStages);
    
    // Start fiber
    transfer_t continue_t = jump_fcontext(ctx, &userData);
    
    Assert::AreEqual(userData, 1);
    
    // Continue fiber
    jump_fcontext(continue_t.fctx, &userData);
    
    Assert::AreEqual(userData, 5);
    
    // Free allocated stack
    mem::Free<mem::Tag::Game>(stack);
}

void FiberWithResult(transfer_t t)
{
    uint32 result = 42;
    
    jump_fcontext( t.fctx, &result);
}

UNITTEST(FiberCore, ResultFromFiber)
{
    // Creating a stack
    uint32 stackSize = 64_kb;
    uint8* stack = mem::Allocate<mem::Tag::Game>(stackSize);
    
    // Preparing fiber context on the given stack
    fcontext_t ctx = make_fcontext( stack, stackSize, FiberWithResult);
    
    // Start fiber
    transfer_t t = jump_fcontext(ctx, nullptr);
    
    // Get and check result value
    uint32& result = *(reinterpret_cast<uint32*>(t.data));
    Assert::AreEqual(result, 42);
    
    // Free allocated stack
    mem::Free<mem::Tag::Game>(stack);
}

void SimpleJob(jobs::Context& context)
{
    uint32* data = reinterpret_cast<uint32*>(context.m_userData);
    (*data)++;
}

UNITTEST(FiberLib, SimpleJob)
{
    jobs::InitializeJobSystem();
    
    uint32 userData = 0;
    jobs::JobDesc job = {SimpleJob, &userData};
    
    Assert::AreEqual(userData, 0);
    
    jobs::Counter* counter = nullptr;
    
    jobs::RunJobs(&job, 1, &counter);
    
    Assert::IsTrue(counter != nullptr);
    
    while(*counter > 0)
    {
        std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }
    
    delete counter; // we are not on fiber, so we have to both wait and delete counter ourselfs
    
    Assert::AreEqual(userData, 1);
    
    jobs::DeinitializeJobSystem();
}

UNITTEST(FiberLib, MultipleJobsWithCounter)
{
    jobs::InitializeJobSystem();
    
    uint32 userData[2] = {0};
    jobs::JobDesc job[2] = {{SimpleJob, &userData[0]}, {SimpleJob, &userData[1]}};
    
    Assert::AreEqual(userData[0], 0);
    Assert::AreEqual(userData[1], 0);
    
    jobs::Counter* counter = nullptr;
    
    jobs::RunJobs(job, 2, &counter);

    Assert::IsTrue(counter != nullptr);
    
    while(*counter > 0)
    {
        std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }
    
    delete counter; // we are not on fiber, so we have to both wait and delete counter ourselfs
    
    Assert::AreEqual(userData[0], 1);
    Assert::AreEqual(userData[1], 1);
    
    jobs::DeinitializeJobSystem();
}

void SuspendedJob(jobs::Context& context)
{
    uint32* userData = reinterpret_cast<uint32*>(context.m_userData);
    
    jobs::JobDesc job[2] = {{SimpleJob, &userData[0]}, {SimpleJob, &userData[1]}};
    
    jobs::Counter* counter = nullptr;
    jobs::RunJobs(job, 2, &counter);
    jobs::WaitForCounterAndFree(context, counter);
}

UNITTEST(FiberLib, SuspendedJobTest)
{
    jobs::InitializeJobSystem();
    
    uint32 userData[2] = {0};
    jobs::JobDesc job = {SuspendedJob, &userData};
    
    Assert::AreEqual(userData[0], 0);
    Assert::AreEqual(userData[1], 0);
    
    jobs::Counter* counter = nullptr;
    jobs::RunJobs(&job, 1, &counter);
    
    Assert::IsTrue(counter != nullptr);
    
    while(*counter > 0)
    {
        std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }
    
    delete counter; // we are not on fiber, so we have to both wait and delete counter ourselfs
    
    Assert::AreEqual(userData[0], 1);
    Assert::AreEqual(userData[1], 1);
    
    jobs::DeinitializeJobSystem();
}

void ManyJobsSpawner(jobs::Context& context)
{
	uint32* userData = reinterpret_cast<uint32*>(context.m_userData);
	
	jobs::JobDesc job[100];
	
	for(uint32 i=0; i<100; ++i)
	{
		job[i].m_jobFunc = SimpleJob;
		job[i].m_userData = &userData[i];
	}
	
	jobs::Counter* counter = nullptr;
	jobs::RunJobs(job, 100, &counter);
	jobs::WaitForCounterAndFree(context, counter);
}

UNITTEST(FiberLib, ManyJobsTest)
{
	jobs::InitializeJobSystem();
	
	uint32 userData[100] = {0};
	jobs::JobDesc job = {ManyJobsSpawner, &userData};
	
	for(uint32 i=0; i<100; ++i)
	{
		Assert::AreEqual(userData[i], 0);
	}
	
	jobs::Counter* counter = nullptr;
	jobs::RunJobs(&job, 1, &counter);
	
	Assert::IsTrue(counter != nullptr);
	
	while(*counter > 0)
	{
		std::this_thread::sleep_for(std::chrono::milliseconds(1));
	}
	
	delete counter; // we are not on fiber, so we have to both wait and delete counter ourselfs
	
	for(uint32 i=0; i<100; ++i)
	{
		Assert::AreEqual(userData[i], 1);
	}
	
	jobs::DeinitializeJobSystem();
}































