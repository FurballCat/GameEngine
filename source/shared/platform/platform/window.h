#pragma once

namespace platform
{
    class IWindow
    {
    public:
        virtual ~IWindow() {}
        
        virtual uint32 GetWidth() const = 0;
        virtual uint32 GetHeight() const = 0;
        
        virtual void Run() = 0;
    };
    
    struct WindowDesc
    {
        uint32 width;
        uint32 height;
    };
    
    // Implemented in specific platform code
    PLATFORM_API SharedPtr<IWindow> CreateWindow(const WindowDesc& desc);
}
