#include "pch.h"
#include "macHIDInput.h"
#include "core/timer.h"
#include "macInputGamepad.h"

using namespace input;

static int32 GetDevicePropertyAsInt32(IOHIDDeviceRef deviceRef, CFStringRef prop)
{
	CFTypeRef ref = IOHIDDeviceGetProperty(deviceRef, prop);
	if (ref && CFGetTypeID(ref) == CFNumberGetTypeID())
	{
		int32 value = 0;
		CFNumberGetValue((CFNumberRef)ref, kCFNumberSInt32Type, &value);
		return value;
	}
	return 0;
}

static float GetRangePercantage(int32 minValue, int32 maxValue, int32 value)
{
	float range = (float)abs(maxValue - minValue);
	float valueDistanceFromMin = (float)abs(value - minValue);
	return valueDistanceFromMin / range;
}

HIDInput::HIDInput()
{
	for (int32 i=0; i < MAX_CONTROLLERS; ++i)
	{
		ControllerState& controllerState = m_controllers[i];
		std::memset(&controllerState, 0, sizeof(controllerState));
		
		controllerState.m_controllerId = i;
	}
	
	m_buttons[0] = input::Gamepad_faceButtonBottom;
	m_buttons[1] = input::Gamepad_faceButtonRight;
	m_buttons[2] = input::Gamepad_faceButtonLeft;
	m_buttons[3] = input::Gamepad_faceButtonTop;
	m_buttons[4] = input::Gamepad_leftShoulder;
	m_buttons[5] = input::Gamepad_rightShoulder;
	m_buttons[6] = input::Gamepad_specialRight;
	m_buttons[7] = input::Gamepad_specialLeft;
	m_buttons[8] = input::Gamepad_leftThumb;
	m_buttons[9] = input::Gamepad_rightThumb;
	m_buttons[10] = input::Gamepad_leftTriggerThreshold;
	m_buttons[11] = input::Gamepad_rightTriggerThreshold;
	m_buttons[12] = input::Gamepad_dpadUp;
	m_buttons[13] = input::Gamepad_dpadDown;
	m_buttons[14] = input::Gamepad_dpadLeft;
	m_buttons[15] = input::Gamepad_dpadRight;
	m_buttons[16] = input::Gamepad_leftStickUp;
	m_buttons[17] = input::Gamepad_leftStickDown;
	m_buttons[18] = input::Gamepad_leftStickLeft;
	m_buttons[19] = input::Gamepad_leftStickRight;
	m_buttons[20] = input::Gamepad_rightStickUp;
	m_buttons[21] = input::Gamepad_rightStickDown;
	m_buttons[22] = input::Gamepad_rightStickLeft;
	m_buttons[23] = input::Gamepad_rightStickRight;
	
	m_hidManager = IOHIDManagerCreate(kCFAllocatorDefault, 0L);
	if(!m_hidManager)
		return;
	
	IOReturn result = IOHIDManagerOpen(m_hidManager, kIOHIDOptionsTypeNone);
	if(result != kIOReturnSuccess)
	{
		CFRelease(m_hidManager);
		m_hidManager = NULL;
		return;
	}
	
	CFMutableArrayRef matchingArray = CFArrayCreateMutable(kCFAllocatorDefault, 0, &kCFTypeArrayCallBacks);
	if (!matchingArray)
	{
		CFRelease(m_hidManager);
		m_hidManager = NULL;
		return;
	}
	
	uint32 usagePage = kHIDPage_GenericDesktop;
	uint32 usage = kHIDUsage_GD_GamePad;
	
	// Create a dictionary to add usage page/usages to
	CFMutableDictionaryRef matchingGamepads = CFDictionaryCreateMutable(kCFAllocatorDefault, 0, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
	if (matchingGamepads)
	{
		// Add key for device type to refine the matching dictionary.
		CFNumberRef PageCFNumberRef = CFNumberCreate(kCFAllocatorDefault, kCFNumberIntType, &usagePage);
		if (PageCFNumberRef)
		{
			CFDictionarySetValue(matchingGamepads, CFSTR(kIOHIDDeviceUsagePageKey), PageCFNumberRef);
			CFRelease(PageCFNumberRef);
			
			// Note: the usage is only valid if the usage page is also defined
			CFNumberRef UsageCFNumberRef = CFNumberCreate(kCFAllocatorDefault, kCFNumberIntType, &usage);
			if (UsageCFNumberRef)
			{
				CFDictionarySetValue(matchingGamepads, CFSTR(kIOHIDDeviceUsageKey), UsageCFNumberRef);
				CFRelease(UsageCFNumberRef);
			}
		}
	}
	else
	{
		CFRelease(m_hidManager);
		m_hidManager = NULL;
		return;
	}
	
	CFArrayAppendValue(matchingArray, matchingGamepads);
	CFRelease(matchingGamepads);
	
	IOHIDManagerSetDeviceMatchingMultiple(m_hidManager, matchingArray);
	CFRelease(matchingArray);
	
	// Setup HID Manager's add/remove devices callbacks
	IOHIDManagerRegisterDeviceMatchingCallback(m_hidManager, HIDDeviceMatchingCallback, this);
	IOHIDManagerRegisterDeviceRemovalCallback(m_hidManager, HIDDeviceRemovalCallback, this);
	
	// Add HID Manager to run loop
	IOHIDManagerScheduleWithRunLoop(m_hidManager, CFRunLoopGetCurrent(), kCFRunLoopDefaultMode);
}

void HIDInput::HIDDeviceMatchingCallback(void* context, IOReturn result, void* sender, IOHIDDeviceRef deviceRef)
{
	HIDInput* hidInput = (HIDInput*)context;
	hidInput->OnNewHIDController(result, deviceRef);
}

void HIDInput::HIDDeviceRemovalCallback(void* context, IOReturn result, void* sender, IOHIDDeviceRef deviceRef)
{
	HIDInput* hidInput = (HIDInput*)context;
	
	for (int32 i = 0; i < MAX_CONTROLLERS; ++i)
	{
		if(hidInput->m_controllers[i].m_device.m_deviceRef == deviceRef)
		{
			// todo: this is a good place to send info that controller was disconnected
			
			hidInput->m_controllers[i].m_device.m_deviceRef = NULL;
			break;
		}
	}
	
	hidInput->m_isGamepadAttached = false;
	for (int32 i = 0; i < MAX_CONTROLLERS; ++i)
	{
		if (hidInput->m_controllers[i].m_device.m_deviceRef)
		{
			hidInput->m_isGamepadAttached = true;
			break;
		}
	}
}

void HIDInput::OnNewHIDController(IOReturn result, IOHIDDeviceRef deviceRef)
{
	int32 index = -1;
	
	// find free slot for new controller
	for(int32 i=0; i<MAX_CONTROLLERS; ++i)
	{
		if(m_controllers[i].m_device.m_deviceRef == NULL)
		{
			index = i;
			break;
		}
	}
	
	if(index != -1)
	{
		if(IOHIDDeviceOpen(deviceRef, kIOHIDOptionsTypeNone) == kIOReturnSuccess)
		{
			CFArrayRef elementsArray = IOHIDDeviceCopyMatchingElements(deviceRef, NULL, 0);
			if(elementsArray)
			{
				DeviceInfo& deviceInfo = m_controllers[index].m_device;
				deviceInfo.m_deviceRef = deviceRef;
				deviceInfo.m_elements.clear();
				deviceInfo.SetupMappings();
				
				CFIndex elementsCount = CFArrayGetCount(elementsArray);
				for (CFIndex elementIndex = 0; elementIndex < elementsCount; ++elementIndex)
				{
					HIDElementInfo element;
					element.m_elementRef = (IOHIDElementRef)CFArrayGetValueAtIndex(elementsArray, elementIndex);
					element.m_type = IOHIDElementGetType(element.m_elementRef);
					element.m_usagePage = IOHIDElementGetUsagePage(element.m_elementRef);
					element.m_usage = IOHIDElementGetUsage(element.m_elementRef);
					element.m_minValue = (int32)IOHIDElementGetLogicalMin(element.m_elementRef);
					element.m_maxValue = (int32)IOHIDElementGetLogicalMax(element.m_elementRef);
					
					if ((element.m_type == kIOHIDElementTypeInput_Button && element.m_usagePage == kHIDPage_Button && element.m_usage < MAX_CONTROLLER_BUTTON_MAPPINGS)
						|| ((element.m_type == kIOHIDElementTypeInput_Misc || element.m_type == kIOHIDElementTypeInput_Axis) && element.m_usagePage == kHIDPage_GenericDesktop))
					{
						deviceInfo.m_elements.push_back(element);
					}
				}
				
				CFRelease(elementsArray);
				
				// at this stage we can inform the game that controller is attached
			}
			else
			{
				IOHIDDeviceClose(deviceRef, kIOHIDOptionsTypeNone);
			}
		}
	}
}

void HIDInput::DeviceInfo::SetupMappings()
{
	const int32 vendorID = GetDevicePropertyAsInt32(m_deviceRef, CFSTR(kIOHIDVendorIDKey));
	const int32 productID = GetDevicePropertyAsInt32(m_deviceRef, CFSTR(kIOHIDProductIDKey));
	
	if (vendorID == 0x54c && productID == 0x5c4)
	{
		m_buttonMappings[1]	= 2;	// Square		->	X
		m_buttonMappings[2]	= 0;	// Cross		->	A
		m_buttonMappings[3]	= 1;	// Circle		->	B
		m_buttonMappings[4]	= 3;	// Triangle		->	Y
		m_buttonMappings[5]	= 4;	// L1			->	Left Shoulder
		m_buttonMappings[6]	= 5;	// R1			->	Right Shoulder
		m_buttonMappings[7]	= 10;	// L2			->	Left Trigger
		m_buttonMappings[8]	= 11;	// R2			->	Right Trigger
		m_buttonMappings[9]	= 7;	// Share		->	Back
		m_buttonMappings[10]	= 6;	// Options		->	Start
		m_buttonMappings[11]	= 8;	// L3			->	Left Thumbstick
		m_buttonMappings[12]	= 9;	// R3			->	Right Thumbstick
		
		m_leftAnalogXMapping = kHIDUsage_GD_X;
		m_leftAnalogYMapping = kHIDUsage_GD_Y;
		m_leftTriggerAnalogMapping = kHIDUsage_GD_Rx;
		m_rightAnalogXMapping = kHIDUsage_GD_Z;
		m_rightAnalogYMapping = kHIDUsage_GD_Rz;
		m_rightTriggerAnalogMapping = kHIDUsage_GD_Ry;
	}
}

void HIDInput::SendControllerEvents(const TimeInfo& timeInfo)
{
	for (int32 controllerIndex = 0; controllerIndex < MAX_CONTROLLERS; ++controllerIndex)
	{
		ControllerState& controllerState = m_controllers[controllerIndex];
		
		if( controllerState.m_device.m_deviceRef )
		{
			bool currentButtonStates[MAX_CONTROLLER_BUTTON_MAPPINGS] = {0};
			
			for (int index = 0; index < controllerState.m_device.m_elements.size(); ++index)
			{
				HIDElementInfo& element = controllerState.m_device.m_elements[index];
				
				IOHIDValueRef valueRef;
				IOReturn result = IOHIDDeviceGetValue(controllerState.m_device.m_deviceRef, element.m_elementRef, &valueRef);
				if (result == kIOReturnSuccess && IOHIDValueGetLength(valueRef) <= sizeof(int32))
				{
					const int32 newValue = (int32)IOHIDValueGetIntegerValue(valueRef);
					if (element.m_usagePage == kHIDPage_Button)
					{
						const int8 mappedIndex = controllerState.m_device.m_buttonMappings[element.m_usage];
						if (mappedIndex != -1)
						{
							currentButtonStates[mappedIndex] = newValue > 0;
						}
					}
					else
					{
						const bool isTrigger = element.m_usage == controllerState.m_device.m_leftTriggerAnalogMapping || element.m_usage == controllerState.m_device.m_rightTriggerAnalogMapping;
						const float percentage = GetRangePercantage(element.m_minValue, element.m_maxValue, newValue);
						const float floatValue = isTrigger ? percentage : percentage * 2.0f - 1.0f;
						
						if (element.m_usage == controllerState.m_device.m_leftAnalogXMapping && controllerState.m_leftAnalogX != newValue)
						{
							OnControllerAnalog(input::Gamepad_leftAnalogX, controllerState.m_controllerId, floatValue);
							currentButtonStates[18] = floatValue < -0.2f;	// LeftStickLeft
							currentButtonStates[19] = floatValue > 0.2f;	// LeftStickRight
							controllerState.m_leftAnalogX = newValue;
						}
						else if (element.m_usage == controllerState.m_device.m_leftAnalogYMapping && controllerState.m_leftAnalogY != newValue)
						{
							OnControllerAnalog(input::Gamepad_leftAnalogY, controllerState.m_controllerId, floatValue);
							currentButtonStates[16] = floatValue < -0.2f;	// LeftStickUp
							currentButtonStates[17] = floatValue > 0.2f;	// LeftStickDown
							controllerState.m_leftAnalogY = newValue;
						}
						else if (element.m_usage == controllerState.m_device.m_rightAnalogXMapping && controllerState.m_rightAnalogX != newValue)
						{
							OnControllerAnalog(input::Gamepad_rightAnalogX, controllerState.m_controllerId, floatValue);
							currentButtonStates[22] = floatValue < -0.2f;	// RightStickLeft
							currentButtonStates[23] = floatValue > 0.2f;	// RightStickDown
							controllerState.m_rightAnalogX = newValue;
						}
						else if (element.m_usage == controllerState.m_device.m_rightAnalogYMapping && controllerState.m_rightAnalogY != newValue)
						{
							OnControllerAnalog(input::Gamepad_rightAnalogY, controllerState.m_controllerId, floatValue);
							currentButtonStates[20] = floatValue < -0.2f;	// RightStickUp
							currentButtonStates[21] = floatValue > 0.2f;	// RightStickRight
							controllerState.m_rightAnalogY = newValue;
						}
						else if (element.m_usage == controllerState.m_device.m_leftTriggerAnalogMapping)
						{
							if (controllerState.m_leftTriggerAnalog != newValue)
							{
								OnControllerAnalog(input::Gamepad_leftTrigger, controllerState.m_controllerId, floatValue);
								controllerState.m_leftTriggerAnalog = newValue;
							}
							currentButtonStates[10] = floatValue > 0.01f;
						}
						else if (element.m_usage == controllerState.m_device.m_rightTriggerAnalogMapping)
						{
							if (controllerState.m_rightTriggerAnalog != newValue)
							{
								OnControllerAnalog(input::Gamepad_rightTrigger, controllerState.m_controllerId, floatValue);
								controllerState.m_rightTriggerAnalog = newValue;
							}
							currentButtonStates[11] = floatValue > 0.01f;
						}
						else if (element.m_usage == kHIDUsage_GD_Hatswitch)
						{
							switch (newValue)
							{
								case 0: currentButtonStates[12] = true;									break; // Up
								case 1: currentButtonStates[12] = true;	currentButtonStates[15] = true;	break; // Up + right
								case 2: currentButtonStates[15] = true;									break; // Right
								case 3: currentButtonStates[15] = true;	currentButtonStates[13] = true;	break; // Right + down
								case 4: currentButtonStates[13] = true;									break; // Down
								case 5: currentButtonStates[13] = true;	currentButtonStates[14] = true;	break; // Down + left
								case 6: currentButtonStates[14] = true;									break; // Left
								case 7: currentButtonStates[14] = true;	currentButtonStates[12] = true;	break; // Left + up
							}
						}
					}
				}
			}
			
			const double currentTime = timeInfo.t;
			
			// For each button check against the previous state and send the correct message if any
			for (int32 buttonIndex = 0; buttonIndex < MAX_CONTROLLER_BUTTON_MAPPINGS; ++buttonIndex)
			{
				if (currentButtonStates[buttonIndex] != controllerState.m_buttonStates[buttonIndex])
				{
					if (currentButtonStates[buttonIndex])
					{
						OnControllerButtonPressed(m_buttons[buttonIndex], controllerState.m_controllerId);
					}
					else
					{
						OnControllerButtonReleased(m_buttons[buttonIndex], controllerState.m_controllerId);
					}
					
					if (currentButtonStates[buttonIndex] != 0)
					{
						// this button was pressed - set the button's m_nextRepeatTime to the InitialButtonRepeatDelay
						controllerState.m_nextRepeatTime[buttonIndex] = currentTime + m_initialButtonRepeatDelay;
					}
				}
				else if (currentButtonStates[buttonIndex] != 0 && controllerState.m_nextRepeatTime[buttonIndex] <= currentTime)
				{
					OnControllerButtonPressed(m_buttons[buttonIndex], controllerState.m_controllerId);
					
					// set the button's m_nextRepeatTime to the ButtonRepeatDelay
					controllerState.m_nextRepeatTime[buttonIndex] = currentTime + m_buttonRepeatDelay;
				}
				
				// Update the state for next time
				controllerState.m_buttonStates[buttonIndex] = currentButtonStates[buttonIndex];
			}	
		}
	}
}

void HIDInput::OnControllerButtonPressed(InputEventID button, InputUserID player)
{
	m_pendingEvents.push_back(InputEvent{button, 1.0f, player, 0});
}

void HIDInput::OnControllerButtonReleased(InputEventID button, InputUserID player)
{
	m_pendingEvents.push_back(InputEvent{button, 0.0f, player, 0});
}

void HIDInput::OnControllerAnalog(InputEventID analog, InputUserID player, float value)
{
	m_pendingEvents.push_back(InputEvent{analog, value, player, 0});
}

Name HIDInput::GetName() const
{
	return Name("Gamepad");
}

void HIDInput::AcquireEvents(DynArray<InputEvent>& events)
{
	events.insert(events.end(), m_pendingEvents.begin(), m_pendingEvents.end());
	m_pendingEvents.clear();
}

void HIDInput::Reset()
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

void HIDInput::SendCommand(InputDeviceCommand& command)
{
	
}

DynArray<InputDeviceCommandDesc> HIDInput::ListAvailableCommands() const
{
	return DynArray<InputDeviceCommandDesc>();
}

Name HIDInput::GetEventName(InputEventID id) const
{
	return Name("None");
}

DynArray<InputEventDesc> HIDInput::ListAvailableEvents() const
{
	DynArray<InputEventDesc> events;
	return events;
}

bool HIDInput::IsConnected() const
{
	return m_isGamepadAttached;
}

void HIDInput::Suspend()
{
	
}

void HIDInput::Resume()
{
	
}


























