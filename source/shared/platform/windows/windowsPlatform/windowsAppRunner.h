#pragma once
#include "platform/appRunner.h"

namespace platform
{
    class IApplication;
    
    // Application launcher code specific to given platform (for example on Windows you have a menu bar not related to any window, but to whole application)
    // On Windows there's window procedure (WndProc) function etc.
    class WindowsAppRunner : public IAppRunner
    {
    public:
		WindowsAppRunner(SharedPtr<IApplication> app);
        
        virtual int32 Run() override;
    
    private:
        SharedPtr<IApplication> m_app;
    };
}
