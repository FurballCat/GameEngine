/* Copyright (c) 2016-2020 Furball Cat */

#include "macHIDInput.h"

#include <assert.h>

#ifndef FUR_ASSERT
#define FUR_ASSERT(x) assert(x)
#endif

int32_t fi_get_device_property_as_int32(IOHIDDeviceRef deviceRef, CFStringRef prop)
{
	CFTypeRef ref = IOHIDDeviceGetProperty(deviceRef, prop);
	if (ref && CFGetTypeID(ref) == CFNumberGetTypeID())
	{
		int32_t value = 0;
		CFNumberGetValue((CFNumberRef)ref, kCFNumberSInt32Type, &value);
		return value;
	}
	return 0;
}

float fi_get_range_percantage(int32_t minValue, int32_t maxValue, int32_t value)
{
	float range = (float)abs(maxValue - minValue);
	float valueDistanceFromMin = (float)abs(value - minValue);
	return valueDistanceFromMin / range;
}

void fi_device_info_setup_mappings(fi_device_info_t* pInfo)
{
	const int32_t vendorID = fi_get_device_property_as_int32(pInfo->m_deviceRef, CFSTR(kIOHIDVendorIDKey));
	const int32_t productID = fi_get_device_property_as_int32(pInfo->m_deviceRef, CFSTR(kIOHIDProductIDKey));
	
	if (vendorID == 0x54c && productID == 0x5c4)
	{
		pInfo->m_buttonMappings[1]	= 2;	// Square		->	X
		pInfo->m_buttonMappings[2]	= 0;	// Cross		->	A
		pInfo->m_buttonMappings[3]	= 1;	// Circle		->	B
		pInfo->m_buttonMappings[4]	= 3;	// Triangle		->	Y
		pInfo->m_buttonMappings[5]	= 4;	// L1			->	Left Shoulder
		pInfo->m_buttonMappings[6]	= 5;	// R1			->	Right Shoulder
		pInfo->m_buttonMappings[7]	= 10;	// L2			->	Left Trigger
		pInfo->m_buttonMappings[8]	= 11;	// R2			->	Right Trigger
		pInfo->m_buttonMappings[9]	= 7;	// Share		->	Back
		pInfo->m_buttonMappings[10]	= 6;	// Options		->	Start
		pInfo->m_buttonMappings[11]	= 8;	// L3			->	Left Thumbstick
		pInfo->m_buttonMappings[12]	= 9;	// R3			->	Right Thumbstick
		
		pInfo->m_leftAnalogXMapping = kHIDUsage_GD_X;
		pInfo->m_leftAnalogYMapping = kHIDUsage_GD_Y;
		pInfo->m_leftTriggerAnalogMapping = kHIDUsage_GD_Rx;
		pInfo->m_rightAnalogXMapping = kHIDUsage_GD_Z;
		pInfo->m_rightAnalogYMapping = kHIDUsage_GD_Rz;
		pInfo->m_rightTriggerAnalogMapping = kHIDUsage_GD_Ry;
	}
}

void fi_hid_input_on_new_hid_controller(fi_hid_input_t* pInput, IOReturn result, IOHIDDeviceRef deviceRef)
{
	int32_t index = -1;
	
	// find free slot for new controller
	for(int32_t i=0; i<MAX_CONTROLLERS; ++i)
	{
		if(pInput->m_controllers[i].m_device.m_deviceRef == NULL)
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
				fi_device_info_t* deviceInfo = &pInput->m_controllers[index].m_device;
				deviceInfo->m_deviceRef = deviceRef;
				memset(deviceInfo->m_elements, 0, sizeof(fi_hid_element_info_t) * MAX_DEVICE_ELEMENTS);
				fi_device_info_setup_mappings(deviceInfo);
				
				CFIndex elementsCount = CFArrayGetCount(elementsArray);
				for (CFIndex elementIndex = 0; elementIndex < elementsCount; ++elementIndex)
				{
					fi_hid_element_info_t element;
					element.m_elementRef = (IOHIDElementRef)CFArrayGetValueAtIndex(elementsArray, elementIndex);
					element.m_type = IOHIDElementGetType(element.m_elementRef);
					element.m_usagePage = IOHIDElementGetUsagePage(element.m_elementRef);
					element.m_usage = IOHIDElementGetUsage(element.m_elementRef);
					element.m_minValue = (int32_t)IOHIDElementGetLogicalMin(element.m_elementRef);
					element.m_maxValue = (int32_t)IOHIDElementGetLogicalMax(element.m_elementRef);
					
					if ((element.m_type == kIOHIDElementTypeInput_Button && element.m_usagePage == kHIDPage_Button && element.m_usage < MAX_CONTROLLER_BUTTON_MAPPINGS)
						|| ((element.m_type == kIOHIDElementTypeInput_Misc || element.m_type == kIOHIDElementTypeInput_Axis) && element.m_usagePage == kHIDPage_GenericDesktop))
					{
						const uint32_t idx = deviceInfo->m_numElements;
						if(idx < MAX_DEVICE_ELEMENTS)
						{
							deviceInfo->m_elements[idx] = element;
							deviceInfo->m_numElements += 1;
						}
						else
						{
							FUR_ASSERT(0);
						}
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

void fi_hid_input_device_matching_callback(void* context, IOReturn result, void* sender, IOHIDDeviceRef deviceRef)
{
	fi_hid_input_t* hidInput = (fi_hid_input_t*)context;
	fi_hid_input_on_new_hid_controller(hidInput, result, deviceRef);
}

void fi_hid_input_device_removal_callback(void* context, IOReturn result, void* sender, IOHIDDeviceRef deviceRef)
{
	fi_hid_input_t* hidInput = (fi_hid_input_t*)context;
	
	for (int32_t i = 0; i < MAX_CONTROLLERS; ++i)
	{
		if(hidInput->m_controllers[i].m_device.m_deviceRef == deviceRef)
		{
			// todo: this is a good place to send info that controller was disconnected
			
			hidInput->m_controllers[i].m_device.m_deviceRef = NULL;
			break;
		}
	}
	
	hidInput->m_isGamepadAttached = false;
	for (int32_t i = 0; i < MAX_CONTROLLERS; ++i)
	{
		if (hidInput->m_controllers[i].m_device.m_deviceRef)
		{
			hidInput->m_isGamepadAttached = true;
			break;
		}
	}
}

void fi_hid_input_init(fi_hid_input_t* pInput)
{
	for (uint8_t i=0; i < MAX_CONTROLLERS; ++i)
	{
		fi_controller_state_t* controllerState = &pInput->m_controllers[i];
		memset(controllerState, 0, sizeof(fi_controller_state_t));
		
		controllerState->m_controllerId = i;
	}
	
	pInput->m_buttons[0] = Gamepad_faceButtonBottom;
	pInput->m_buttons[1] = Gamepad_faceButtonRight;
	pInput->m_buttons[2] = Gamepad_faceButtonLeft;
	pInput->m_buttons[3] = Gamepad_faceButtonTop;
	pInput->m_buttons[4] = Gamepad_leftShoulder;
	pInput->m_buttons[5] = Gamepad_rightShoulder;
	pInput->m_buttons[6] = Gamepad_specialRight;
	pInput->m_buttons[7] = Gamepad_specialLeft;
	pInput->m_buttons[8] = Gamepad_leftThumb;
	pInput->m_buttons[9] = Gamepad_rightThumb;
	pInput->m_buttons[10] = Gamepad_leftTriggerThreshold;
	pInput->m_buttons[11] = Gamepad_rightTriggerThreshold;
	pInput->m_buttons[12] = Gamepad_dpadUp;
	pInput->m_buttons[13] = Gamepad_dpadDown;
	pInput->m_buttons[14] = Gamepad_dpadLeft;
	pInput->m_buttons[15] = Gamepad_dpadRight;
	pInput->m_buttons[16] = Gamepad_leftStickUp;
	pInput->m_buttons[17] = Gamepad_leftStickDown;
	pInput->m_buttons[18] = Gamepad_leftStickLeft;
	pInput->m_buttons[19] = Gamepad_leftStickRight;
	pInput->m_buttons[20] = Gamepad_rightStickUp;
	pInput->m_buttons[21] = Gamepad_rightStickDown;
	pInput->m_buttons[22] = Gamepad_rightStickLeft;
	pInput->m_buttons[23] = Gamepad_rightStickRight;
	
	pInput->m_hidManager = IOHIDManagerCreate(kCFAllocatorDefault, 0L);
	if(!pInput->m_hidManager)
		return;
	
	IOReturn result = IOHIDManagerOpen(pInput->m_hidManager, kIOHIDOptionsTypeNone);
	if(result != kIOReturnSuccess)
	{
		CFRelease(pInput->m_hidManager);
		pInput->m_hidManager = NULL;
		return;
	}
	
	CFMutableArrayRef matchingArray = CFArrayCreateMutable(kCFAllocatorDefault, 0, &kCFTypeArrayCallBacks);
	if (!matchingArray)
	{
		CFRelease(pInput->m_hidManager);
		pInput->m_hidManager = NULL;
		return;
	}
	
	uint32_t usagePage = kHIDPage_GenericDesktop;
	uint32_t usage = kHIDUsage_GD_GamePad;
	
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
		CFRelease(pInput->m_hidManager);
		pInput->m_hidManager = NULL;
		return;
	}
	
	CFArrayAppendValue(matchingArray, matchingGamepads);
	CFRelease(matchingGamepads);
	
	IOHIDManagerSetDeviceMatchingMultiple(pInput->m_hidManager, matchingArray);
	CFRelease(matchingArray);
	
	// Setup HID Manager's add/remove devices callbacks
	IOHIDManagerRegisterDeviceMatchingCallback(pInput->m_hidManager, fi_hid_input_device_matching_callback, pInput);
	IOHIDManagerRegisterDeviceRemovalCallback(pInput->m_hidManager, fi_hid_input_device_removal_callback, pInput);
	
	// Add HID Manager to run loop
	IOHIDManagerScheduleWithRunLoop(pInput->m_hidManager, CFRunLoopGetCurrent(), kCFRunLoopDefaultMode);
}

void fi_hid_input_on_controller_button_pressed(fi_hid_input_t* pInput, uint16_t buttonID, uint8_t playerID)
{
	const uint32_t idx = pInput->m_numPendingEvents;
	if(idx < MAX_PENDING_EVENTS)
	{
		pInput->m_numPendingEvents += 1;
		fi_input_event_t* evt = &pInput->m_pendingEvents[idx];
		evt->value = 1.0f;
		evt->eventID = buttonID;
		evt->playerID = playerID;
		// evt->deviceID =
	}
}

void fi_hid_input_on_controller_button_released(fi_hid_input_t* pInput, uint16_t buttonID, uint8_t playerID)
{
	const uint32_t idx = pInput->m_numPendingEvents;
	if(idx < MAX_PENDING_EVENTS)
	{
		pInput->m_numPendingEvents += 1;
		fi_input_event_t* evt = &pInput->m_pendingEvents[idx];
		evt->value = 0.0f;
		evt->eventID = buttonID;
		evt->playerID = playerID;
		// evt->deviceID =
	}
}

void fi_hid_input_on_controller_analog(fi_hid_input_t* pInput, uint16_t analogID, uint8_t playerID, float value)
{
	const uint32_t idx = pInput->m_numPendingEvents;
	if(idx < MAX_PENDING_EVENTS)
	{
		pInput->m_numPendingEvents += 1;
		fi_input_event_t* evt = &pInput->m_pendingEvents[idx];
		evt->value = value;
		evt->eventID = analogID;
		evt->playerID = playerID;
		// evt->deviceID =
	}
}

void fi_hid_input_update(fi_hid_input_t* pInput, double currentTime)
{
	// clear events array for new events this frame
	pInput->m_numPendingEvents = 0;
	
	for (int32_t controllerIndex = 0; controllerIndex < MAX_CONTROLLERS; ++controllerIndex)
	{
		fi_controller_state_t* controllerState = &pInput->m_controllers[controllerIndex];
		
		if( controllerState->m_device.m_deviceRef )
		{
			bool currentButtonStates[MAX_CONTROLLER_BUTTON_MAPPINGS] = {0};
			
			for (int32_t index = 0; index < controllerState->m_device.m_numElements; ++index)
			{
				fi_hid_element_info_t* element = &controllerState->m_device.m_elements[index];
				
				IOHIDValueRef valueRef;
				IOReturn result = IOHIDDeviceGetValue(controllerState->m_device.m_deviceRef, element->m_elementRef, &valueRef);
				if (result == kIOReturnSuccess && IOHIDValueGetLength(valueRef) <= sizeof(int32_t))
				{
					const int32_t newValue = (int32_t)IOHIDValueGetIntegerValue(valueRef);
					if (element->m_usagePage == kHIDPage_Button)
					{
						const int8_t mappedIndex = controllerState->m_device.m_buttonMappings[element->m_usage];
						if (mappedIndex != -1)
						{
							currentButtonStates[mappedIndex] = newValue > 0;
						}
					}
					else
					{
						const bool isTrigger = element->m_usage == controllerState->m_device.m_leftTriggerAnalogMapping || element->m_usage == controllerState->m_device.m_rightTriggerAnalogMapping;
						const float percentage = fi_get_range_percantage(element->m_minValue, element->m_maxValue, newValue);
						const float floatValue = isTrigger ? percentage : percentage * 2.0f - 1.0f;
						
						if (element->m_usage == controllerState->m_device.m_leftAnalogXMapping && controllerState->m_leftAnalogX != newValue)
						{
							fi_hid_input_on_controller_analog(pInput, Gamepad_leftAnalogX, controllerState->m_controllerId, floatValue);
							currentButtonStates[18] = floatValue < -0.2f;	// LeftStickLeft
							currentButtonStates[19] = floatValue > 0.2f;	// LeftStickRight
							controllerState->m_leftAnalogX = newValue;
						}
						else if (element->m_usage == controllerState->m_device.m_leftAnalogYMapping && controllerState->m_leftAnalogY != newValue)
						{
							fi_hid_input_on_controller_analog(pInput, Gamepad_leftAnalogY, controllerState->m_controllerId, floatValue);
							currentButtonStates[16] = floatValue < -0.2f;	// LeftStickUp
							currentButtonStates[17] = floatValue > 0.2f;	// LeftStickDown
							controllerState->m_leftAnalogY = newValue;
						}
						else if (element->m_usage == controllerState->m_device.m_rightAnalogXMapping && controllerState->m_rightAnalogX != newValue)
						{
							fi_hid_input_on_controller_analog(pInput, Gamepad_rightAnalogX, controllerState->m_controllerId, floatValue);
							currentButtonStates[22] = floatValue < -0.2f;	// RightStickLeft
							currentButtonStates[23] = floatValue > 0.2f;	// RightStickDown
							controllerState->m_rightAnalogX = newValue;
						}
						else if (element->m_usage == controllerState->m_device.m_rightAnalogYMapping && controllerState->m_rightAnalogY != newValue)
						{
							fi_hid_input_on_controller_analog(pInput, Gamepad_rightAnalogY, controllerState->m_controllerId, floatValue);
							currentButtonStates[20] = floatValue < -0.2f;	// RightStickUp
							currentButtonStates[21] = floatValue > 0.2f;	// RightStickRight
							controllerState->m_rightAnalogY = newValue;
						}
						else if (element->m_usage == controllerState->m_device.m_leftTriggerAnalogMapping)
						{
							if (controllerState->m_leftTriggerAnalog != newValue)
							{
								fi_hid_input_on_controller_analog(pInput, Gamepad_leftTrigger, controllerState->m_controllerId, floatValue);
								controllerState->m_leftTriggerAnalog = newValue;
							}
							currentButtonStates[10] = floatValue > 0.01f;
						}
						else if (element->m_usage == controllerState->m_device.m_rightTriggerAnalogMapping)
						{
							if (controllerState->m_rightTriggerAnalog != newValue)
							{
								fi_hid_input_on_controller_analog(pInput, Gamepad_rightTrigger, controllerState->m_controllerId, floatValue);
								controllerState->m_rightTriggerAnalog = newValue;
							}
							currentButtonStates[11] = floatValue > 0.01f;
						}
						else if (element->m_usage == kHIDUsage_GD_Hatswitch)
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
			
			// For each button check against the previous state and send the correct message if any
			for (int32_t buttonIndex = 0; buttonIndex < MAX_CONTROLLER_BUTTON_MAPPINGS; ++buttonIndex)
			{
				if (currentButtonStates[buttonIndex] != controllerState->m_buttonStates[buttonIndex])
				{
					if (currentButtonStates[buttonIndex])
					{
						fi_hid_input_on_controller_button_pressed(pInput, pInput->m_buttons[buttonIndex], controllerState->m_controllerId);
					}
					else
					{
						fi_hid_input_on_controller_button_released(pInput, pInput->m_buttons[buttonIndex], controllerState->m_controllerId);
					}
					
					if (currentButtonStates[buttonIndex] != 0)
					{
						// this button was pressed - set the button's m_nextRepeatTime to the InitialButtonRepeatDelay
						controllerState->m_nextRepeatTime[buttonIndex] = currentTime + pInput->m_initialButtonRepeatDelay;
					}
				}
				else if (currentButtonStates[buttonIndex] != 0 && controllerState->m_nextRepeatTime[buttonIndex] <= currentTime)
				{
					fi_hid_input_on_controller_button_pressed(pInput, pInput->m_buttons[buttonIndex], controllerState->m_controllerId);
					
					// set the button's m_nextRepeatTime to the ButtonRepeatDelay
					controllerState->m_nextRepeatTime[buttonIndex] = currentTime + pInput->m_buttonRepeatDelay;
				}
				
				// Update the state for next time
				controllerState->m_buttonStates[buttonIndex] = currentButtonStates[buttonIndex];
			}	
		}
	}
}

uint32_t fi_hid_input_get_events(const fi_hid_input_t* pInput, fi_input_event_t* pEvents, uint32_t capacity, uint32_t startIndex)
{
	if(pInput->m_numPendingEvents <= startIndex)
		return 0;
	
	const uint32_t numPendingEvents = pInput->m_numPendingEvents - startIndex;
	
	uint32_t numEventsCollected = 0;
	for(; numEventsCollected < numPendingEvents && numEventsCollected < capacity; ++numEventsCollected)
	{
		pEvents[numEventsCollected] = pInput->m_pendingEvents[numEventsCollected];
	}
	
	return numEventsCollected;
}
