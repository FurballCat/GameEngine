#pragma once

namespace gpu
{
    class IGraphicsDevice;
    class IWindow;
    
    class IGraphicsView
    {
    public:
        virtual ~IGraphicsView() {}
    };
    
    // Implemented in specific platform code
    PLATFORM_API SharedPtr<IGraphicsView> CreateGraphicsView(SharedPtr<IWindow> window, SharedPtr<IGraphicsDevice> graphicsDevice);
}
