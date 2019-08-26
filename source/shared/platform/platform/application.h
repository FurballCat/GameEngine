#pragma once

namespace platform
{
    // Implement this interface and pass to IAppRunner to have your application running
    class IApplication
    {
    public:
        virtual ~IApplication() {}
        
        // Initialize application by creating systems, windows, setting up input etc.
        // Load only most required content here like progress icon and animation
        // Load menu and game content on separate thread and show progress animation while in Tick
        virtual bool Initialize() = 0;
        
        // Tick main loop iteration
        // Return false to close application (note that OnExternalCloseRequest will not be called then)
        virtual bool Tick() = 0;
        
        // Suspend whole application (i.e. for energy saving purposes)
        virtual void Suspend() = 0;
        
        // Resume application after suspend
        virtual void Resume() = 0;
        
        // Called when someone (but not the application itself) requests exit, ask for progress save on that event etc.
        // When returned, application will no longer call Tick
        virtual void OnExternalCloseRequest() = 0;
        
        // Shutdown all systems and windows
        virtual void Shutdown() = 0;
    };
}
