#include "pch.h"
#include "inputCollector.h"
#include "inputDevice.h"

using namespace input;

void input::InputCollector::AddDevice(SharedPtr<InputDevice> device)
{
	m_devices.push_back(device);
}

void input::InputCollector::CollectInput(DynArray<InputEvent>& events)
{
	for (uint8 i = 0; i < m_devices.size(); ++i)
	{
		DynArray<InputEvent> deviceEvents;
		m_devices[i]->AcquireEvents(deviceEvents);

		for (auto& evt : deviceEvents)
		{
			evt.m_deviceIndex = i;
		}

		events.insert(events.end(), deviceEvents.begin(), deviceEvents.end());
	}
}

Name input::InputCollector::GetEventName(InputEvent& evt)
{
	if (evt.m_deviceIndex < m_devices.size())
	{
		return m_devices[evt.m_deviceIndex]->GetEventName(evt.m_id);
	}

	return Name("unknown");
}

Name input::InputCollector::GetDeviceName(InputEvent& evt)
{
	if (evt.m_deviceIndex < m_devices.size())
	{
		return m_devices[evt.m_deviceIndex]->GetName();
	}

	return Name("unknown");
}
