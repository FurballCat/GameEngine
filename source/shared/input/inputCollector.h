#pragma once
#include "../input/api.h"

namespace input
{
	struct InputEvent;
	class InputDevice;

	class INPUT_API InputCollector
	{
	public:
		void AddDevice(SharedPtr<InputDevice> device);
		void CollectInput(DynArray<InputEvent>& events);

		Name GetEventName(InputEvent& evt);
		Name GetDeviceName(InputEvent& evt);

	private:
		DynArray<SharedPtr<InputDevice>> m_devices;
	};
}
