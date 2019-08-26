#pragma once
#include <chrono>
#include "api.h"
#include "core/threads.h"

namespace profiler
{
    using ProfilerUnit = std::chrono::nanoseconds;
    using ProfilerClock = std::chrono::high_resolution_clock;
    using ProfilerTimePoint = std::chrono::time_point<ProfilerClock, ProfilerUnit>;
    
    constexpr uint8 c_maximumThreadCount = 16;
    constexpr uint16 c_maximumCallstackDepth = 1024;
    
    using ThreadID = std::thread::id;
    
    struct ProfilerPendingScopeEntry
    {
        const char* m_name;
        ProfilerTimePoint m_start;
    };
    
    enum class ProfilerScopeColor
    {
        Red,
        Green,
        Blue,
        Cyan,
        Magenta,
        Yellow
    };
    
    class PROFILER_API Profiler
    {
    public:
        void BeginFiber(const char* name, ProfilerScopeColor color, uint64 frameID);
        void EndFiber();
        
        void BeginScope(const char* name);
        void EndScope();
        
        static Profiler& GetInstance()
        {
            static Profiler profiler;
            return profiler;
        }
        
    private:
        uint8 GetCurrentThreadEntriesIndex();
        
        ThreadID m_threadMap[c_maximumThreadCount];
        uint16 m_lastEntry[c_maximumThreadCount];
        ProfilerPendingScopeEntry m_entries[c_maximumThreadCount][c_maximumCallstackDepth];
        
        SpinLock m_lock;
    };
    
    class PROFILER_API ProfilerCategoryScope
    {
    public:
        ProfilerCategoryScope(const char* name, ProfilerScopeColor color, uint64 frameID)
        {
            Profiler::GetInstance().BeginFiber(name, color, frameID);
        }
        
        ~ProfilerCategoryScope()
        {
            Profiler::GetInstance().EndFiber();
        }
    };
    
    class PROFILER_API ProfilerScope
    {
    public:
        ProfilerScope(const char* name)
        {
            Profiler::GetInstance().BeginScope(name);
        }
        
        ~ProfilerScope()
        {
            Profiler::GetInstance().EndScope();
        }
    };
}

#define PROFILER_SCOPE(name) profiler::ProfilerScope _profiler_scope(#name);
