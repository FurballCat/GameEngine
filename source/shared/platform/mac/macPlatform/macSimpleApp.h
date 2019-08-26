#pragma once
#include "platform/simpleApp.h"
#include "gpu/internal/metal/mtlpp/mtlpp.h"
#include "core/timer.h"

namespace gpu
{
    class Drawable;
    class RenderPassDescriptor;
    class Device;
}

namespace input
{
    class MacInputKeyboard;
	class HIDInput;
}

namespace platform
{
    class ISimpleApp;
    
    class MacSimpleApp
    {
    public:
        MacSimpleApp(ISimpleApp* app);
        
        void Run(void (*renderFunc)(const MacSimpleApp&), uint32 width, uint32 height, gpu::Device* device);
        
        uint32 GetWidth() const;
        uint32 GetHeight() const;
        
        gpu::Drawable GetDrawable() const;
        gpu::RenderPassDescriptor GetRenderPassDescriptor() const;
        
        const ISimpleApp* GetApp() const { return m_app; }
        
        void KeyDown(uint32 keyId);
        void KeyUp(uint32 keyId);
        
        SharedPtr<input::MacInputKeyboard>& GetKeyboard() { return m_keyboard; }
		SharedPtr<input::HIDInput>& GetGamepad() { return m_gamepad; }
		
		void TickInternals(const TimeInfo& timeInfo);
		
		TimeInfo GetNextTimeInfo();
		
    private:
        uint32 m_width;
        uint32 m_height;
        
        class MtlView : public ns::Object
        {
        public:
            MtlView() { }
            MtlView(const ns::Handle& handle) : ns::Object(handle) { }
        };
		
        MtlView m_view;
        ISimpleApp* m_app;
        SharedPtr<input::MacInputKeyboard> m_keyboard;
		SharedPtr<input::HIDInput> m_gamepad;
		
		Timer m_timer;
    };
}
