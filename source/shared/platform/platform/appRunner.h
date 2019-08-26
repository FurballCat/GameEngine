#pragma once

namespace platform
{
    class IApplication;
    
    // Application launcher code specific to given platform (for example on Mac you have a menu bar not related to any window, but to whole application)
    // On Windows there's window procedure (WndProc) function etc.
    class IAppRunner
    {
    public:
        virtual ~IAppRunner() {}
        
        // Returns application return code
        virtual int32 Run() = 0;
    };
    
    // Implemented in specific platform code
    PLATFORM_API SharedPtr<IAppRunner> CreateAppRunner(SharedPtr<IApplication> application);
}
