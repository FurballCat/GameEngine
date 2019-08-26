#include "pch.h"
#include "macSimpleApp.h"
#include <cocoa/cocoa.h>
#include <MetalKit/MTKView.h>
#include "profiler/profiler.h"
#include "gpu/public.h"
#include "macInputKeyboard.h"
#include "input/inputCollector.h"
#include "macHIDInput.h"

using namespace platform;

@interface MacWindowViewController : NSViewController<MTKViewDelegate> {
@public void (*m_render)(const MacSimpleApp&);
@public MacSimpleApp* m_window;
}

@end

@implementation MacWindowViewController
-(void)mtkView:(nonnull MTKView *)view drawableSizeWillChange:(CGSize)size
{
    
}

-(void)drawInMTKView:(nonnull MTKView *)view
{
    (*m_render)(*m_window);
}
@end

@interface MyMTKView : MTKView {
@public MacSimpleApp* m_window;
}
@end

@implementation MyMTKView
- (void)keyDown:(NSEvent *)event
{
    m_window->KeyDown(event.keyCode);
}

- (void)keyUp:(NSEvent *)event
{
    m_window->KeyUp(event.keyCode);
}

- (BOOL)acceptsFirstResponder
{
    return YES;
}
@end

// Implement platform specific global interface
namespace platform
{
    void Render(const MacSimpleApp& win)
    {
		MacSimpleApp& macApp = const_cast<MacSimpleApp&>(win);
		TimeInfo timeInfo = macApp.GetNextTimeInfo();
		
		macApp.TickInternals(timeInfo);
		
        ISimpleApp* app = const_cast<ISimpleApp*>(win.GetApp());
		
        gpu::Drawable drawable = win.GetDrawable();
        gpu::RenderPassDescriptor descriptor = win.GetRenderPassDescriptor();
		
		ViewportInfo viewport = {&drawable, &descriptor};
		app->DrawViewport(viewport, timeInfo);
    }

    int32 RunSimpleApp(const SimpleAppDesc& desc)
    {
        if (desc.app == nullptr)
            return 1;

        if (desc.app->Initialize() == false)
            return 2;
        
        MacSimpleApp app(desc.app);
        desc.m_inputCollector->AddDevice(app.GetKeyboard());
		desc.m_inputCollector->AddDevice(app.GetGamepad());
        app.Run(&Render, desc.windowWidth, desc.windowHeight, desc.app->GetDevice());
        
        desc.app->Shutdown();
        
        return 0;
    }
}

MacSimpleApp::MacSimpleApp(ISimpleApp* app)
    : m_app(app)
{
    m_keyboard = SharedPtr<input::MacInputKeyboard>(new input::MacInputKeyboard());
	m_gamepad = SharedPtr<input::HIDInput>(new input::HIDInput());
}

void MacSimpleApp::Run(void (*renderFunc)(const MacSimpleApp&), uint32 width, uint32 height, gpu::Device* device)
{
    m_width = width;
    m_height = height;
	
	m_timer = Timer::Tick();	// start global game engine timer
	
    NSRect frame = NSMakeRect(0, 0, m_width, m_height);
    NSWindow* window = [[NSWindow alloc] initWithContentRect:frame
#if MTLPP_IS_AVAILABLE_MAC(10_12)
                                                   styleMask:(NSWindowStyleMaskTitled | NSWindowStyleMaskClosable | NSWindowStyleMaskResizable)
#else
                                                   styleMask:(NSTitledWindowMask | NSClosableWindowMask | NSResizableWindowMask)
#endif
                                                     backing:NSBackingStoreBuffered
                                                       defer:NO];
    window.title = [[NSProcessInfo processInfo] processName];
    MacWindowViewController* viewController = [MacWindowViewController new];
    viewController->m_render = &platform::Render;
    viewController->m_window = this;
    
    MyMTKView* view = [[MyMTKView alloc] initWithFrame:frame];
    view.device = (__bridge id<MTLDevice>)device->GetPlatformPtr().GetPtr();
    view.delegate = viewController;
    view.autoresizingMask = NSViewWidthSizable | NSViewHeightSizable;
    view->m_window = this;
    
    [window.contentView addSubview:view];
    [window center];
    [window orderFrontRegardless];
    
    m_view = ns::Handle{ (__bridge void*)view };
	
    [NSApplication sharedApplication];
    [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
    
    NSMenu* menubar = [NSMenu new];
    NSMenuItem* appMenuItem = [NSMenuItem new];
    NSMenu* appMenu = [NSMenu new];
    NSMenuItem* quitMenuItem = [[NSMenuItem alloc] initWithTitle:@"Quit" action:@selector(stop:) keyEquivalent:@"q"];
    [menubar addItem:appMenuItem];
    [appMenu addItem:quitMenuItem];
    [appMenuItem setSubmenu:appMenu];
    [NSApp setMainMenu:menubar];
    
    [NSApp activateIgnoringOtherApps:YES];
    [NSApp run];
}

uint32 MacSimpleApp::GetWidth() const
{
    return static_cast<uint32>(((__bridge MTKView*)m_view.GetPtr()).frame.size.width);
}

uint32 MacSimpleApp::GetHeight() const
{
    return static_cast<uint32>(((__bridge MTKView*)m_view.GetPtr()).frame.size.height);
}

gpu::Drawable MacSimpleApp::GetDrawable() const
{
    return gpu::Drawable(ns::Handle{ (__bridge void*)((__bridge MTKView*)m_view.GetPtr()).currentDrawable });
}

gpu::RenderPassDescriptor MacSimpleApp::GetRenderPassDescriptor() const
{
    return gpu::RenderPassDescriptor(ns::Handle{ (__bridge void*)((__bridge MTKView*)m_view.GetPtr()).currentRenderPassDescriptor });
}

void MacSimpleApp::KeyDown(uint32 keyId)
{
    m_keyboard->KeyDown(keyId);
}

void MacSimpleApp::KeyUp(uint32 keyId)
{
    m_keyboard->KeyUp(keyId);
}

TimeInfo MacSimpleApp::GetNextTimeInfo()
{
	float dt = (float)m_timer.Tock();
	double t = m_timer.AbsoluteTime();
	return {t, dt};
}

void MacSimpleApp::TickInternals(const TimeInfo& timeInfo)
{
	m_gamepad->SendControllerEvents(timeInfo);
}






















