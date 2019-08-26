#include "pch.h"
#include "macInputKeyboard.h"

using namespace input;

void MacInputKeyboard::KeyDown(uint32 keyCode)
{
    m_pendingEvents.push_back({keyCode, 1.0f, 0, 0});
}

void MacInputKeyboard::KeyUp(uint32 keyCode)
{
    m_pendingEvents.push_back({keyCode, 0.0f, 0, 0});
}

Name MacInputKeyboard::GetName() const
{
    return Name("Keyboard");
}

void MacInputKeyboard::AcquireEvents(DynArray<InputEvent>& events)
{
    events.insert(events.end(), m_pendingEvents.begin(), m_pendingEvents.end());
    m_pendingEvents.clear();
}

void MacInputKeyboard::Reset()
{
    DynArray<InputEvent> pending = std::move(m_pendingEvents);
    
    for(const auto& evt : pending)
    {
        if(evt.m_value > 0.0f)
        {
            m_pendingEvents.push_back(evt);
            m_pendingEvents.back().m_value = 0.0f;
        }
    }
}

void MacInputKeyboard::SendCommand(InputDeviceCommand& command)
{
    
}

DynArray<InputDeviceCommandDesc> MacInputKeyboard::ListAvailableCommands() const
{
    return DynArray<InputDeviceCommandDesc>();
}

Name MacInputKeyboard::GetEventName(InputEventID id) const
{
    if(id == 123) return Name("Left");
    if(id == 124) return Name("Right");
    
    return Name("None");
}

DynArray<InputEventDesc> MacInputKeyboard::ListAvailableEvents() const
{
    DynArray<InputEventDesc> events;
    
    events.push_back({123, Name("Left")});
    events.push_back({124, Name("Right")});
    
    return events;
}

bool MacInputKeyboard::IsConnected() const
{
    return true;
}

void MacInputKeyboard::Suspend()
{
    
}

void MacInputKeyboard::Resume()
{
    
}
