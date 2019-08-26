#pragma once
#include "input/inputDevice.h"

namespace input
{
    class MacInputKeyboard : public InputDevice
    {
    public:
        // Platform specific methods
        void KeyDown(uint32 keyCode);
        void KeyUp(uint32 keyCode);
        
        // InputDevice interface implementation
        virtual Name GetName() const override;
        virtual void AcquireEvents(DynArray<InputEvent>& events) override;
        virtual void Reset() override;
        virtual void SendCommand(InputDeviceCommand& command) override;
        virtual DynArray<InputDeviceCommandDesc> ListAvailableCommands() const override;
        virtual Name GetEventName(InputEventID id) const override;
        virtual DynArray<InputEventDesc> ListAvailableEvents() const override;
        virtual bool IsConnected() const override;
        virtual void Suspend() override;
        virtual void Resume() override;
        
    private:
        DynArray<InputEvent> m_pendingEvents;

    };
}
