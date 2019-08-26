#include "pch.h"
#include "macGraphicsView.h"
#include "macWindow.h"
#include <cocoa/cocoa.h>

using namespace platform;

@implementation MetalView
@end

// Implement platform specific global interface
namespace gpu
{
    SharedPtr<IGraphicsView> CreateGraphicsView(SharedPtr<platform::IWindow> window, SharedPtr<IGraphicsDevice> graphicsDevice)
    {
        //MacWindow* macWindow = PlatformCast(window.get());
        
        return SharedPtr<IGraphicsView>(new MacGraphicsView());
    }
}
