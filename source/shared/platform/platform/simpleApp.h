#pragma once

namespace gpu
{
    class Drawable;
    class Device;
    class RenderPassDescriptor;
}

namespace input
{
    class InputCollector;
}

struct TimeInfo;

namespace platform
{
    // Implement this interface and pass to RunSimpleApp through desc to have your application running
	struct ViewportInfo
	{
		gpu::Drawable* m_drawable;
		gpu::RenderPassDescriptor* m_passDescriptor;
	};
	
    class ISimpleApp
    {
    public:
        virtual ~ISimpleApp() {}
        
        // Initialize application by creating systems, windows, setting up input etc.
        // Load only most required content here like progress icon and animation
        // Load menu and game content on separate thread and show progress animation while in Tick
        virtual bool Initialize() = 0;
        
        // Device should be created during initialize phase
        virtual gpu::Device* GetDevice() = 0;
        
        // Tick main loop iteration
        // Return false to close application (note that OnExternalCloseRequest will not be called then)
        virtual bool DrawViewport(ViewportInfo& viewport, const TimeInfo& timeInfo) = 0;
        
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
    
    struct SimpleAppDesc
    {
        uint32 windowWidth;
        uint32 windowHeight;
        ISimpleApp* app;
        input::InputCollector* m_inputCollector;
    };
    
    // Implemented in specific platform code
    PLATFORM_API int32 RunSimpleApp(const SimpleAppDesc& desc);
}
