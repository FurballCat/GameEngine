#include "pch.h"
#include "profiler.h"

using namespace profiler;

void Profiler::BeginFiber(const char* name, ProfilerScopeColor color, uint64 frameID)
{
    
}

void Profiler::EndFiber()
{
    
}

void Profiler::BeginScope(const char* name)
{
    //auto start = ProfilerClock::now();
    //uint8 index = GetCurrentThreadEntriesIndex();
    //uint16& lastFreeEntryIndex = m_lastEntry[index];
    //ASSERT(lastFreeEntryIndex < c_maximumCallstackDepth);
    
    //auto& entry = m_entries[index][lastFreeEntryIndex];
    //entry.m_name = name;
    //entry.m_start = start;
    
    //++lastFreeEntryIndex;
}

void Profiler::EndScope()
{
    //auto end = ProfilerClock::now();
    //uint8 index = GetCurrentThreadEntriesIndex();
    //uint16& lastFreeEntryIndex = m_lastEntry[index];
    //uint16 lastEntryIndex = --lastFreeEntryIndex;
    
    //auto& entry = m_entries[index][lastEntryIndex];
    
    // TODO: store name, start and end in buffer
}

uint8 Profiler::GetCurrentThreadEntriesIndex()
{
    ThreadID currentID = std::this_thread::get_id();
    
    for(uint32 i=0; i<c_maximumThreadCount; ++i)
    {
        // not entirely thread safe, but probably this condition shouldn't be met even if another thread is writing to this array
        if(m_threadMap[i] == currentID)
        {
            return i;
        }
    }
    
    ThreadID emptyThreadID;
    ScopedLock<SpinLock> lock(m_lock);
    for(uint32 i=0; i<c_maximumThreadCount; ++i)
    {
        if(m_threadMap[i] == emptyThreadID)
        {
            m_threadMap[i] = currentID;
            return i;
        }
    }
    
    ASSERT(false);  // can't find current thread entries
    return 0;
}
