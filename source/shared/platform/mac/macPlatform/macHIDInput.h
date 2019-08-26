#pragma once
#include <IOKit/hid/IOHIDLib.h>
#include "input/inputDevice.h"

struct TimeInfo;

namespace input
{
	class HIDInput : public InputDevice
	{
	public:
		HIDInput();
		
		void SendControllerEvents(const TimeInfo& timeInfo);
		
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
		static void HIDDeviceMatchingCallback(void* context, IOReturn result, void* sender, IOHIDDeviceRef deviceRef);
		static void HIDDeviceRemovalCallback(void* context, IOReturn result, void* sender, IOHIDDeviceRef deviceRef);
		
		void OnNewHIDController(IOReturn result, IOHIDDeviceRef deviceRef);
		
		void OnControllerButtonPressed(InputEventID button, InputUserID player);
		void OnControllerButtonReleased(InputEventID button, InputUserID player);
		void OnControllerAnalog(InputEventID analog, InputUserID player, float value);
		
		static const int32 MAX_CONTROLLER_BUTTON_MAPPINGS = 24;
		
		struct HIDElementInfo
		{
			IOHIDElementRef m_elementRef;
			IOHIDElementType m_type;
			uint16 m_usagePage;
			uint16 m_usage;
			int32 m_minValue;
			int32 m_maxValue;
			
			
		};
		
		struct DeviceInfo
		{
			IOHIDDeviceRef m_deviceRef;
			DynArray<HIDElementInfo> m_elements;
			
			int8 m_buttonMappings[MAX_CONTROLLER_BUTTON_MAPPINGS];
			uint16 m_leftAnalogXMapping;
			uint16 m_leftAnalogYMapping;
			uint16 m_leftTriggerAnalogMapping;
			uint16 m_rightAnalogXMapping;
			uint16 m_rightAnalogYMapping;
			uint16 m_rightTriggerAnalogMapping;
			
			void SetupMappings();
		};
		
		struct ControllerState
		{
			DeviceInfo m_device;
			bool m_buttons[MAX_CONTROLLER_BUTTON_MAPPINGS];
			int32 m_leftAnalogX;
			int32 m_leftAnalogY;
			int32 m_rightAnalogX;
			int32 m_rightAnalogY;
			int32 m_leftTriggerAnalog;
			int32 m_rightTriggerAnalog;
			bool m_buttonStates[MAX_CONTROLLER_BUTTON_MAPPINGS];
			float m_nextRepeatTime[MAX_CONTROLLER_BUTTON_MAPPINGS];
			
			uint8 m_controllerId;
		};
		
		static const int32 MAX_CONTROLLERS = 16;
		
		ControllerState m_controllers[MAX_CONTROLLERS];
		
		uint32 m_buttons[MAX_CONTROLLER_BUTTON_MAPPINGS];
		
		float m_initialButtonRepeatDelay;
		float m_buttonRepeatDelay;
		
		bool m_isGamepadAttached;
		
		IOHIDManagerRef m_hidManager;
		
		DynArray<InputEvent> m_pendingEvents;
	};
}
