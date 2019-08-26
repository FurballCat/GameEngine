#pragma once

namespace input
{
	// Input event, like key down, key up, mouse movements, stick axis value etc.
	typedef uint8 InputUserID;
	typedef uint32 InputEventID;
	typedef uint8 InputDeviceIndex;
	struct InputEvent
	{
		InputEventID m_id;
		float m_value;
		InputUserID m_user;
		InputDeviceIndex m_deviceIndex;		// So InputEventIDs can overlap from different devices
	};

	// Input device command - like force set feedback, set back light etc.
	typedef uint32 InputDeviceCommandID;
	struct InputDeviceCommand
	{
		InputDeviceCommandID m_id;
		float m_value;
		InputUserID m_user;
	};

	// Description of the device command
	struct InputDeviceCommandDesc
	{
		InputDeviceCommandID m_id;
		Name m_name;
	};

	// Description of the input event
	struct InputEventDesc
	{
		InputEventID m_id;
		Name m_name;
	};

	// An input device, can be keyboard, mouse, pad, joystick, whatever else, even multiple pads
	class InputDevice
	{
	public:
		virtual ~InputDevice() {}

		// Get device name (xpad, keyboard, mouse, etc.)
		virtual Name GetName() const = 0;

		// Get events collected so far
		virtual void AcquireEvents(DynArray<InputEvent>& events) = 0;

		// Reset state and events (this might generate release events)
		virtual void Reset() = 0;

		// Custom device commands like force feedback etc.
		virtual void SendCommand(InputDeviceCommand& command) = 0;

		// List available device commands
		virtual DynArray<InputDeviceCommandDesc> ListAvailableCommands() const = 0;

		// Get translation from abstract ID to name (like KEY_A, KEY_B, DEVICE_LOST, LEFT_STICK_X_AXIS etc.)
		virtual Name GetEventName(InputEventID id) const = 0;

		// List all types of events that can occur
		virtual DynArray<InputEventDesc> ListAvailableEvents() const = 0;

		// Is this device connected
		virtual bool IsConnected() const = 0;

		// Suspend the input device, so no more events will be sent (this might trigger Reset)
		virtual void Suspend() = 0;

		// Resume input device, so events will be sent again
		virtual void Resume() = 0;
	};
}
